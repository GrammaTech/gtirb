(defpackage :gtirb/test
  (:use :common-lisp :stefil :gtirb :gtirb/dot
        :named-readtables :curry-compose-reader-macros)
  (:import-from :md5 :md5sum-file)
  (:import-from :uiop :run-program :with-temporary-file)
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

(deftest idempotent-read-write-w-class ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (write-gtirb (read-gtirb *proto-path*) path)
      (is (equalp (md5sum-file *proto-path*)
                  (md5sum-file path))))))

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
                            (aux-data-type aux-data) (data aux-data))))
                  (is (equalp orig new)
                      "~s with type ~a encodes/decodes is not idempotent~%~s"
                      name (aux-data-type aux-data)
                      (list orig new)))))
            (aux-data (first (modules hello)))))))

(deftest update-proto-to-disk-and-back ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (let ((test-string "this is a test")
            (hello (read-gtirb *proto-path*))
            (aux (make-instance 'aux-data)))
        (setf (aux-data-type aux) :string)
        (setf (data aux) test-string)
        (push (cons "test" aux) (aux-data (first (modules hello))))
        (write-gtirb hello path)
        ;; TODO: Round-trip serialization isn't working.
        ;;       Probably a bug in update-proto.
        (let ((next (read-gtirb path)))
          (let ((proto (gtirb::proto (first (modules next)))))
            ;; Test for non-empty protobuf elements.
            (is (not (zerop (length (proto:regions (proto:byte-map (proto:image-byte-map proto))))))))
          (is (string= (data (cdr (assoc "test" (aux-data (first (modules next)))
                                         :test #'string=)))
                       test-string)))))))


;;;; Dot test suite
(deftest write-dot-to-file ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (let ((hello (read-gtirb *proto-path*)))
        (to-dot-file (first (modules hello)) path)))))
