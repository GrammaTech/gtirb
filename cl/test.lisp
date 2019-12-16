(defpackage :gtirb/test
  (:use :common-lisp
        :alexandria
        :stefil
        :gtirb
        :gtirb/dot
        :gtirb/update
        :named-readtables :curry-compose-reader-macros)
  (:import-from :md5 :md5sum-file)
  (:import-from :uiop :nest :run-program :with-temporary-file)
  (:shadowing-import-from :gtirb :symbol)
  (:export :test))
(in-package :gtirb/test)
(in-readtable :curry-compose-reader-macros)

(defvar *proto-path* nil "Path to protobuf.")


;;;; Fixtures.
(defixture hello-v0
  (:setup
   (progn
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

(defixture hello
  (:setup
   (progn
     (let ((gtirb-path (with-temporary-file (:pathname p :keep t) p)))
       (with-temporary-file (:pathname gtirb-v0-path)
         (with-temporary-file (:pathname gtirb-path-temp)
           (with-temporary-file (:pathname bin-path)
             (run-program (format nil "echo 'main(){puts(\"hello world\");}'~
                                           |gcc -x c - -o ~a"
                                  bin-path) :force-shell t)
             (run-program
              (format nil "ddisasm --ir ~a ~a" gtirb-v0-path bin-path))
             (delete-file bin-path))
           ;; Convert GTIRB-V0 to current GTIRB.
           (write-proto (upgrade (read-proto 'proto-v0:ir gtirb-v0-path))
                        gtirb-path-temp)
           ;; FIXME: There is a bug in update.lisp in which somehow
           ;;        extra information is being packaged into the
           ;;        serialized protobuf.  This is purged by this
           ;;        extra through protobuf read/write, but it would
           ;;        be good to find out what it is and remove it.
           (write-proto (upgrade (read-proto 'proto:ir gtirb-path-temp))
                        gtirb-path)))
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
  (nest
   (with-fixture hello)
   (with-temporary-file (:pathname path))
   (let ((hello1 (read-gtirb *proto-path*)))
     (write-gtirb hello1 path)
     (is (is-equal-p hello1 (read-gtirb path))))))

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
       (is (not (zerop (length (proto:byte-intervals (aref (proto:sections proto) 0))))))
       ;; Test for the aux-data table created earlier in the test.
       (is (string= (aux-data-data
                     (cdr (assoc "test" (aux-data (first (modules next)))
                                 :test #'string=)))
                    test-string))))))

(deftest back-pointers-work ()
  (with-fixture hello
    (let ((hello (read-gtirb *proto-path*)))
      ;; Modules point to IR.
      (is (every [{eql hello} #'gtirb] (modules hello)))
      ;; Sections point to modules.
      (mapc (lambda (module)
              (is (every [{eql module} #'module] (sections module)))
              ;; Byte-intervals point to sections.
              (mapc (lambda (section)
                      (is (every [{eql section} #'section]
                                 (byte-intervals section)))
                      ;; Blocks point to byte-intervals.
                      (mapc (lambda (byte-interval)
                              (is (every [{eql byte-interval} #'byte-interval]
                                         (blocks byte-interval))))
                            (byte-intervals section)))
                    (sections module)))
            (modules hello)))))

(deftest access-block-bytes ()
  (with-fixture hello
    (is (every [#'not #'null #'bytes]
               (nest (mappend #'blocks)
                     (mappend #'byte-intervals)
                     (mappend #'sections)
                     (modules (read-gtirb *proto-path*)))))))

(deftest find-every-block-in-the-module ()
  (nest (with-fixture hello)
        (let ((it (read-gtirb *proto-path*))))
        (is)
        (every {get-uuid _ it})
        (mapcar [#'proto:uuid #'gtirb/gtirb::proto])
        (mappend #'blocks)
        (mappend #'byte-intervals)
        (mappend #'sections)
        (modules it)))


;;;; Dot test suite
(deftest write-dot-to-file ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (let ((hello (read-gtirb *proto-path*)))
        (to-dot-file (first (modules hello)) path)))))


;;;; Update test suite
(defun first-aux-data (ir)
  (proto:value (aref (proto:aux-data (aref (proto:modules ir) 0)) 0)))

(deftest read-and-upgrade ()
  (with-fixture hello-v0
    (let* ((old (read-proto 'proto-v0:ir *proto-path*))
           (new (upgrade old)))
      (is (eql 'proto:ir (class-name (class-of new))))
      (is (eql 'proto:byte-interval
               (class-name (class-of
                            (aref (proto:byte-intervals
                                   (aref (proto:sections
                                          (aref (proto:modules new)
                                                0)) 0)) 0)))))
      ;; Test for non-empty AuxData.
      (is (not (emptyp (proto:data (first-aux-data new)))))
      new)))

(deftest simple-update ()
  (with-fixture hello-v0
    (with-temporary-file (:pathname path)
      (update *proto-path* path)
      (let ((new (read-proto 'proto:ir path)))
        (is (eql 'proto:ir (class-name (class-of new))))
        (is (not (emptyp (proto:data (first-aux-data new)))))))))
