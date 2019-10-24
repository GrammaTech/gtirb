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
             (class-name (class-of (read-gtirb-proto *proto-path*)))))))

(deftest simple-write ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (write-gtirb-proto (read-gtirb-proto *proto-path*) path)
      (is (probe-file path)))))

(deftest idempotent-read-write ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (write-gtirb-proto (read-gtirb-proto *proto-path*) path)
      (is (equalp (md5sum-file *proto-path*)
                  (md5sum-file path))))))

(deftest idempotent-aux-data-type ()
  (with-fixture hello
    (let ((it (make-instance 'gtirb :proto (read-gtirb-proto *proto-path*))))
      (is (tree-equal
           (mapcar [#'pb:string-value #'proto:type-name #'gtirb::proto #'cdr]
                   (aux-data (first (modules it))))
           (mapcar [#'gtirb::aux-data-type-print #'aux-data-type #'cdr]
                   (aux-data (first (modules it))))
           :test #'string=)))))

(deftest write-dot-to-file ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (let ((hello (make-instance 'gtirb
                     :proto (read-gtirb-proto *proto-path*))))
        (to-dot-file (first (modules hello)) path)))))
