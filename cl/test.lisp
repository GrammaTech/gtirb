(defpackage :gtirb/test
  (:use :common-lisp :stefil :gtirb :gtirb/dot
        :alexandria
        :named-readtables :curry-compose-reader-macros)
  (:import-from :md5 :md5sum-file)
  (:import-from :uiop :nest :run-program :with-temporary-file)
  (:shadowing-import-from :gtirb :symbol)
  (:export :test))
(in-package :gtirb/test)
(in-readtable :curry-compose-reader-macros)

(defvar *proto-path* nil "Path to protobuf.")


;;;; Fixtures.
(defixture hello
  (:setup (progn
            (let ((gtirb-path (with-temporary-file (:pathname p :keep t) p)))
              (with-temporary-file (:pathname bin-path)
                (run-program (format nil "echo 'main(){puts(\"hello world\");}'~
                                           |gcc -x c - -o ~a"
                                     bin-path) :force-shell t)
                (run-program (format nil "ddisasm --ir ~a ~a" gtirb-path bin-path))
                (delete-file bin-path))
              (setf *proto-path* gtirb-path))))
  (:teardown (progn
               (delete-file *proto-path*)
               (setf *proto-path* nil))))


;;;; Main test suite.
(defsuite test)
(in-suite test)

(deftest simple-read ()
  (with-fixture hello
    (is (eql 'proto:ir
             (class-name (class-of (gtirb::read-proto *proto-path*)))))))

(deftest simple-write ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (gtirb::write-proto (gtirb::read-proto *proto-path*) path)
      (is (probe-file path)))))

(deftest idempotent-read-write ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (gtirb::write-proto (gtirb::read-proto *proto-path*) path)
      (is (equalp (md5sum-file *proto-path*)
                  (md5sum-file path))))))

(defun noisy-set-equality (left right)
  (is (= (length left) (length right)))
  (is (not (or (emptyp left) (emptyp right))))
  (is (equal (type-of (first left))
             (type-of (first right))))
  (is (set-equal left right :test #'is-equal-p)))

(defgeneric is-equal-p (left right)
  (:documentation "Return t if LEFT and RIGHT are equal.")
  (:method ((left t) (right t))
    (equalp left right))
  (:method ((left cons) (right cons))
    (and (is-equal-p (car left) (car right))
         (is-equal-p (cdr left) (cdr right))))
  (:method ((left hash-table) (right hash-table))
    (set-equal (hash-table-alist left) (hash-table-alist right)
               :test #'is-equal-p))
  (:method ((left proto:block) (right proto:block))
    (and (equalp (proto:uuid left)
                 (proto:uuid right))
         (= (proto:address left) (proto:address right))
         (= (proto:size left) (proto:size right))))
  (:method ((left section) (right section))
    (and (string= (name left) (name right))
         (= (address left) (address right))
         (= (size left) (size right))))
  (:method ((left symbol) (right symbol))
    (and (string= (name left) (name right))
         (= (value left) (value right))
         (= (referent-uuid left) (referent-uuid right))
         (eql (storage-kind left) (storage-kind right))))
  (:method ((left proto:data-object) (right proto:data-object))
    (and (equalp (proto:uuid left)
                 (proto:uuid right))
         (= (proto:address left) (proto:address right))
         (= (proto:size left) (proto:size right))))
  (:method ((left aux-data) (right aux-data))
    (and (tree-equal (aux-data-type left) (aux-data-type right))
         (is-equal-p (aux-data-data left) (aux-data-data right))))
  (:method ((left edge-label) (right edge-label))
    (and (eql (edge-type left) (edge-type right))
         (eql (conditional left) (conditional right))
         (eql (direct left) (direct right))))
  (:method ((left graph:digraph) (right graph:digraph))
    (and (set-equal (graph:nodes left) (graph:nodes right)
                    :test #'is-equal-p)
         (set-equal (graph:edges-w-values left)
                    (graph:edges-w-values right)
                    :test #'is-equal-p)))
  (:method ((left image-byte-map) (right image-byte-map))
    (and (= (addr-min left) (addr-min right))
         (= (addr-max left) (addr-max right))
         (= (base-address left) (base-address right))
         (= (entry-point-address left) (entry-point-address right))
         (is-equal-p (regions left) (regions right)))))

(deftest idempotent-read-write-w-class ()
  (nest
   (with-fixture hello)
   (with-temporary-file (:pathname path))
   (let ((hello1 (read-gtirb *proto-path*)))
     (write-gtirb hello1 path)
     (let ((hello2 (read-gtirb path)))
       ;; Test image-byte-map equality.
       (is (apply #'is-equal-p
                  (mapcar [#'image-byte-map #'first #'modules]
                          (list hello1 hello2))))
       ;; Test block equality.
       (is (apply #'noisy-set-equality
                  (mapcar [#'hash-table-values #'blocks #'first #'modules]
                          (list hello1 hello2))))
       ;; Test section equality.
       (is (apply #'noisy-set-equality
                  (mapcar [#'sections #'first #'modules]
                          (list hello1 hello2))))
       ;; Test symbol equality.
       (is (apply #'is-equal-p
                  (mapcar [#'symbols #'first #'modules]
                          (list hello1 hello2))))
       ;; Test aux-data equality.
       (is (apply #'noisy-set-equality
                  (mapcar [{mapcar #'cdr} #'aux-data #'first #'modules]
                          (list hello1 hello2))))
       ;; Test CFG equality.
       (is (apply #'is-equal-p
                  (mapcar [#'cfg #'first #'modules]
                          (list hello1 hello2))))))))

(deftest idempotent-aux-data-type ()
  (with-fixture hello
    (let ((it (read-gtirb *proto-path*)))
      (is (tree-equal
           (mapcar [#'pb:string-value #'proto:type-name #'gtirb::proto #'cdr]
                   (aux-data (first (modules it))))
           (mapcar [#'gtirb::aux-data-type-print #'aux-data-type #'cdr]
                   (aux-data (first (modules it))))
           :test #'string=)))))

(deftest idempotent-aux-data-decode-encode ()
  (let ((type1 '(:tuple :uuid :uint64-t :int64-t :uint64-t))
        (type2 '(:sequence :uuid))
        (data '(1 2 3 4)))
    (is (equalp
         (gtirb::aux-data-decode type1 (gtirb::aux-data-encode type1 data))
         data))
    (is (equalp
         (gtirb::aux-data-decode type2 (gtirb::aux-data-encode type2 data))
         data)))
  (with-fixture hello
    (let ((hello (read-gtirb *proto-path*)))
      (mapc (lambda (pair)
              (destructuring-bind (name . aux-data) pair
                (let ((orig (proto:data (gtirb::proto aux-data)))
                      (new (gtirb::aux-data-encode
                            (aux-data-type aux-data) (aux-data-data aux-data))))
                  (is (equalp orig new)
                      "~s with type ~a encodes/decodes is not idempotent~%~s"
                      name (aux-data-type aux-data)
                      (list orig new)))))
            (aux-data (first (modules hello)))))))

(deftest update-proto-to-disk-and-back ()
  (nest
   (with-fixture hello)
   (with-temporary-file (:pathname path))
   (let ((test-string "this is a test")
         (hello (read-gtirb *proto-path*))
         (aux (make-instance 'aux-data)))
     (setf (aux-data-type aux) :string)
     (setf (aux-data-data aux) test-string)
     (push (cons "test" aux) (aux-data (first (modules hello))))
     (write-gtirb hello path)
     (let* ((next (read-gtirb path))
            (proto (gtirb::proto (first (modules next)))))
       ;; Test for non-empty protobuf elements.
       (is (not (zerop (length (proto:regions
                                (proto:byte-map
                                 (proto:image-byte-map proto)))))))
       ;; Test for the aux-data table created earlier in the test.
       (is (string= (aux-data-data
                     (cdr (assoc "test" (aux-data (first (modules next)))
                                 :test #'string=)))
                    test-string))))))


;;;; Dot test suite
(deftest write-dot-to-file ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (let ((hello (read-gtirb *proto-path*)))
        (to-dot-file (first (modules hello)) path)))))
