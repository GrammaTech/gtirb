(defpackage :gtirb/validate
  (:use :gt/full :gtirb :graph :command-line-arguments)
  (:import-from :gtirb.proto)
  (:shadowing-import-from :gt/full :size :copy)
  (:shadowing-import-from :gtirb :symbol)
  (:export :validate))
(in-package :gtirb/validate)
(in-readtable :curry-compose-reader-macros)
(defmethod size ((gtirb-node gtirb-node)) (gtirb:size gtirb-node))
(defmethod copy ((graph graph) &key &allow-other-keys) (graph:copy graph))


;;;; Interface
(defvar *requirements* nil
  "A-list of default requirements keyed by object type.")

(defvar *failed-checks*)

(defgeneric validate (object &key requirements)
  (:documentation "Validate that OBJECT satisfies the requirements in .")
  (:method
      ((object t) &key (requirements *requirements*) &aux (*failed-checks* nil))
    (values (every {check object} (cdr (assoc (type-of object) requirements)))
            *failed-checks*)))

(defclass check ()
  ((action :reader action :initarg :action
           :type (or symbol function) :documentation "Action to run the check.")
   (object :reader object :initarg :object
           :type symbol :documentation "Type of object the check applies to.")
   (name :reader name :initarg :name
         :type string :documentation "Name of the check."))
  (:documentation "Check objects hold validation checks for gtirb instances."))

(defmethod initialize-instance :after
    ((check check) &key (requirements *requirements*) &allow-other-keys)
  (unless (assoc (object check) requirements)
    (push (list (object check)) requirements))
  (pushnew check (cdr (assoc (object check) requirements)) :key #'name))

(defgeneric check (object requirement)
  (:documentation "Check that OBJECT satisfies REQUIREMENT.")
  (:method ((obj t) (requirement symbol)) (funcall requirement obj))
  (:method ((obj t) (requirement function)) (funcall requirement obj))
  (:method ((obj t) (requirement check)) (funcall (action requirement) obj)))

(defmethod check :around ((object t) (requirement function))
  (or (call-next-method) (push requirement *failed-checks*)))
(defmethod check :around ((object t) (requirement check))
  (or (call-next-method) (push (name requirement) *failed-checks*)))


;;;; GTIRB Checks
(defmacro define-check-generic (name (object) &body methods)
  `(progn (defgeneric ,name (,object) ,@methods)
          (make-instance 'check
            :name ',name
            :action ',name
            :object ',(second (first (second (first methods)))))))

(define-check-generic size-matches-contents (object)
  (:method ((obj gtirb)) (every #'size-matches-contents (modules obj)))
  (:method ((obj module)) (every #'size-matches-contents (sections obj)))
  (:method ((obj section)) (every #'size-matches-contents (byte-intervals obj)))
  (:method ((obj byte-interval)) (= (size obj) (length (contents obj)))))

(flet ((nothing-overlaps- (things &aux (min 0))
         (and (every (lambda (pair)
                       (destructuring-bind (address . size) pair
                         (prog1 (>= address min)
                           (setf min (max min (+ address size))))))
                     (sort (mapcar «cons #'address #'size» things) #'<
                           :key #'car))
              (every #'nothing-overlaps things))))
  (define-check-generic nothing-overlaps (object)
    (:method ((obj gtirb)) (every #'nothing-overlaps (modules obj)))
    (:method ((obj module)) (nothing-overlaps- (sections obj)))
    (:method ((obj section)) (nothing-overlaps- (byte-intervals obj)))
    (:method ((obj byte-interval))
      (nothing-overlaps- (remove-if-not {typep _ 'code-block} (blocks obj))))
    (:method ((obj code-block)) t)))

(define-check-generic all-referents-exist (object)
  (:method ((obj gtirb))
    (and (every #'all-referents-exist (modules obj))
         (every {get-uuid _ obj} (nodes (cfg obj)))))
  (:method ((obj module)) (every #'all-referents-exist (symbols obj)))
  (:method ((obj symbol))
    (if (gtirb.proto:has-referent-uuid (gtirb::proto obj))
        (payload obj)
        t)))

(define-check-generic symbolic-expression-size-well-formed (object)
  (:method ((obj gtirb))
    (every [{every «member #'second
                           [#'hash-table-keys #'symbolic-expressions
                                              {get-uuid _ obj} #'car]»}
            #'hash-table-keys #'aux-data-data #'cdr
            (lambda (el) (assoc "symbolicExpressionSizes" el :test #'string=))
            #'aux-data]
           (modules obj))))


;;;; Command-line interface
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +validate-args+
    '((("help" #\h #\?) :type boolean :optional t
       :documentation "display help output"))))

(define-command validate-file (gtirb-file &spec +validate-args+)
  "Validate GTIRB-FILE." ""
  (flet ((exit (code)
           (if *lisp-interaction*
               (return-from validate-file (zerop code))
               (quit code))))
    (when help (show-help-for-validate-file) (quit))
    (if (validate (read-gtirb gtirb-file))
        (exit 0)
        (exit 2))))
