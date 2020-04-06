(defpackage :gtirb/validate
  (:use :gt/full :gtirb :graph :command-line-arguments)
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

(defgeneric validate (object)
  (:documentation "Validate that OBJECT satisfies the requirements in .")
  (:method ((object t) &aux (*failed-checks* nil))
    (values (every {check object} (cdr (assoc (type-of object) *requirements*)))
            *failed-checks*)))

(defclass check ()
  ((action :reader action :initarg :action
           :type (or symbol function) :documentation "Action to run the check.")
   (object :reader object :initarg :object
           :type symbol :documentation "Type of object the check applies to.")
   (name :reader name :initarg :name
         :type string :documentation "Name of the check.")))

(defmethod initialize-instance :after ((check check) &key &allow-other-keys)
  (unless (assoc (object check) *requirements*)
    (push (list (object check)) *requirements*))
  (pushnew check (cdr (assoc (object check) *requirements*)) :key #'name))

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
(defgeneric size-matches-contents (object)
  (:method ((obj gtirb)) (every #'size-matches-contents (modules obj)))
  (:method ((obj module)) (every #'size-matches-contents (sections obj)))
  (:method ((obj section)) (every #'size-matches-contents (byte-intervals obj)))
  (:method ((obj byte-interval)) (= (size obj) (length (contents obj)))))

(make-instance 'check
  :name 'size-matches-contents
  :action 'size-matches-contents
  :object 'gtirb)


;;;; Command-line interface
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +validate-args+
    '((("help" #\h #\?) :type boolean :optional t
       :documentation "display help output"))))

(define-command validate-file (gtirb-file &spec +validate-args+)
  "Validate GTIRB-FILE." ""
  (flet ((exit (code)
           `(if *lisp-interaction*
                (return-from validate code)
                (quit code))))
    (when help (show-help-for-validate) (quit))
    (if (validate (read-gtirb gtirb-file))
        (exit 1)
        (exit 2))))
