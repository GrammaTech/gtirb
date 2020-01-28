(defpackage :gtirb/test
  (:use :common-lisp
        :alexandria
        :stefil
        :gtirb
        :gtirb/dot
        :gtirb/update
        :gtirb/utility
        :gtirb/ranged
        :graph
        :named-readtables :curry-compose-reader-macros)
  (:import-from :md5 :md5sum-file :md5sum-sequence)
  (:import-from :uiop :nest :run-program :with-temporary-file :quit)
  (:shadowing-import-from :gtirb :symbol)
  (:export :test :batch-test))
(in-package :gtirb/test)
(in-readtable :curry-compose-reader-macros)

(defvar *proto-path* nil "Path to protobuf.")

(defun batch-test (&optional args)
  "Run tests in 'batch' mode printing results to STDERR then quit.
The ERRNO used when exiting lisp indicates success or failure."
  (declare (ignorable args))
  (let* ((stefil::*test-progress-print-right-margin* (expt 2 20))
         (failures (coerce (stefil::failure-descriptions-of
                            (without-debugging (test)))
                           'list)))
    (if failures
        (format *error-output* "FAILURES~%~{  ~a~~%~}"
                (mapc [#'stefil::name-of
                       #'stefil::test-of
                       #'car #'stefil::test-context-backtrace-of]
                      failures))
        (format *error-output* "SUCCESS~%"))
    (quit (if failures 2 0))))


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
             (class-name (class-of (read-proto 'proto:ir *proto-path*)))))))

(deftest simple-write ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (gtirb::write-proto (read-proto 'proto:ir *proto-path*) path)
      (is (probe-file path)))))

(deftest idempotent-read-write ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (gtirb::write-proto (read-proto 'proto:ir *proto-path*) path)
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
        (mapcar #'uuid)
        (mappend #'blocks)
        (mappend #'byte-intervals)
        (mappend #'sections)
        (modules it)))

(deftest get-blocks-and-bytes-from-cfg-nodes ()
  (nest (with-fixture hello)
        (let ((it (read-gtirb *proto-path*))))
        (is)
        (every {get-uuid _ it})
        (nodes)
        (cfg it)))

(deftest set-block-bytes-to-the-same-size ()
  (with-fixture hello
    (let* ((it (read-gtirb *proto-path*))
           (original-byte-intervals (nest (mappend #'byte-intervals)
                                          (mappend #'sections)
                                          (modules it)))
           (original-byte-interval-md5sum
            (nest (md5sum-sequence)
                  (force-byte-array)
                  (apply #'concatenate 'vector)
                  (mapcar #'contents original-byte-intervals))))
      (let ((target (first (mappend #'blocks original-byte-intervals))))
        (setf (bytes target)
              (make-array (length (bytes target)) :initial-element 9))
        (is (= (length original-byte-intervals) ; No new byte intervals.
               (length (nest (mappend #'byte-intervals)
                             (mappend #'sections)
                             (modules it)))))
        (is (not (equalp original-byte-interval-md5sum ; New contents.
                         (nest (md5sum-sequence)
                               (force-byte-array)
                               (apply #'concatenate 'vector)
                               (mapcar #'contents)
                               (mappend #'byte-intervals)
                               (mappend #'sections)
                               (modules it)))))))))

(deftest set-block-bytes-to-the-different-size ()
  (with-fixture hello
    (let* ((it (read-gtirb *proto-path*))
           (original-byte-intervals (nest (mappend #'byte-intervals)
                                          (mappend #'sections)
                                          (modules it))))
      (let ((target (first (mappend #'blocks original-byte-intervals))))
        (setf (bytes target)
              (make-array (* 2 (length (bytes target))) :initial-element 9))
        (is (= (1+ (length original-byte-intervals)) ; One new byte intervals.
               (length (nest (mappend #'byte-intervals)
                             (mappend #'sections)
                             (modules it)))))))))

(deftest address-range-block-lookup ()
  (with-fixture hello
    (let* ((it (read-gtirb *proto-path*))
           (a-block (first (nest (mappend #'blocks)
                                 (mappend #'byte-intervals)
                                 (mappend #'sections)
                                 (modules it)))))
      (is (= 2 (length (address-range a-block))))
      (is (member a-block (get-address it (first (address-range a-block))))))))

(deftest payload-can-be-read-and-set ()
  (with-fixture hello
    (let* ((it (read-gtirb *proto-path*))
           (symbols (mappend #'symbols (modules it)))
           (value-symbol
            (find-if [#'proto:has-value #'gtirb::proto] symbols))
           (referent-symbol
            (find-if [#'proto:has-referent-uuid #'gtirb::proto] symbols)))
      ;; Reading gives the right type of payload.
      (is (subtypep (type-of (payload value-symbol)) 'number))
      (is (subtypep (type-of (payload referent-symbol)) 'gtirb::proto-backed))
      ;; Setting a payload has the right effect.
      (setf (payload value-symbol) referent-symbol) ; Value to referent.
      (is (subtypep (type-of (payload value-symbol)) 'gtirb::proto-backed))
      (is (not (proto:has-value (gtirb::proto value-symbol))))
      (setf (payload referent-symbol) 42) ; Referent to value.
      (is (subtypep (type-of (payload referent-symbol)) 'number))
      (is (not (proto:has-referent-uuid (gtirb::proto referent-symbol)))))))

(deftest can-create-without-parents ()
  (is (typep (make-instance 'module) 'module))
  (is (typep (make-instance 'section) 'section))
  (is (typep (make-instance 'byte-interval) 'byte-interval)))


;;;; Dot test suite
(deftest write-dot-to-file ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (to-dot-file (read-gtirb *proto-path*) path))))


;;;; Update test suite
;;;
;;; NOTE: This is a place where property based testing would be very
;;;       useful.  Consider this before starting any further work on
;;;       the updater.  Probably would make sense to use check-it:
;;;       https://github.com/DalekBaldwin/check-it
;;;
;;;       More generally, having check-it generators for GTIRB would
;;;       be useful not just for testing this for for every other
;;;       GTIRB-based library or tool.
;;;
(defun first-aux-data (ir)
  (proto:value (aref (proto:aux-data (aref (proto:modules ir) 0)) 0)))

(deftest read-and-upgrade ()
  (nest
   (with-fixture hello-v0)
   (let* ((old (read-proto 'proto-v0:ir *proto-path*))
          (new (upgrade old))))
   ;; Same Symbol values.
   (flet ((same-symbol-fields (old-field new-field)
            (every #'equalp
                   (map 'list old-field
                        (proto-v0:symbols (aref (proto-v0:modules old) 0)))
                   (map 'list new-field
                        (proto:symbols (aref (proto:modules new) 0))))))
     (is (eql 'proto:ir (class-name (class-of new))))
     (is (eql 'proto:byte-interval
              (class-name (class-of
                           (aref (proto:byte-intervals
                                  (aref (proto:sections
                                         (aref (proto:modules new)
                                               0)) 0)) 0)))))
     ;; Test for non-empty AuxData.
     (is (not (emptyp (proto:data (first-aux-data new)))))
     ;; Test for equal Symbol fields.
     (is (same-symbol-fields #'proto-v0:value #'proto:value))
     (is (same-symbol-fields #'proto-v0:referent-uuid #'proto:referent-uuid))
     ;; Return the new one in case you want it at the REPL.
     new)))

(deftest simple-update ()
  (with-fixture hello-v0
    (with-temporary-file (:pathname path)
      (update *proto-path* path)
      (let ((new (read-proto 'proto:ir path)))
        (is (eql 'proto:ir (class-name (class-of new))))
        (is (not (emptyp (proto:data (first-aux-data new)))))))))

(deftest simple-update-populates-data-block-uuids ()
  (with-fixture hello-v0
    (with-temporary-file (:pathname path)
      (update *proto-path* path)
      (nest
       (let ((new (read-gtirb path))))
       (is)
       (every [#'not #'emptyp])
       (mapcar [#'proto:uuid #'gtirb/gtirb::proto])
       (mappend #'blocks)
       (mappend #'byte-intervals)
       (mappend #'sections)
       (modules new)))))

(deftest simple-update-populates-byte-interval-addresses ()
  (with-fixture hello-v0
    (with-temporary-file (:pathname path)
      (update *proto-path* path)
      (nest
       (let ((new (read-gtirb path))))
       (is)
       (every «and #'addressp [#'numberp #'address]»)
       (mappend #'byte-intervals)
       (mappend #'sections)
       (modules new))
      (nest
       (let ((new (read-gtirb path))))
       (is)
       (every [#'proto:has-address #'gtirb/gtirb::proto])
       (mappend #'byte-intervals)
       (mappend #'sections)
       (modules new)))))


;;;; Ranged collection test suite.
(deftest inserted-is-found ()
  (let ((it (make-instance 'ranged)))
    (ranged-insert it :example 5 10)
    (is (set-equal (ranged-find it 0 6) '(:example)))
    (is (set-equal (ranged-find it 9 20) '(:example)))
    (is (set-equal (ranged-find it 0 200) '(:example)))
    (is (null (ranged-find it 4)))
    (is (null (ranged-find it 10)))
    (gtirb/ranged::in-range it)))

(deftest deleted-is-lost ()
  (let ((it (make-instance 'ranged)))
    (ranged-insert it :example 5 10)
    (ranged-delete it :example 5 10)
    (is (null (ranged-find it 0 6)))
    (is (null (ranged-find it 9 20)))
    (is (null (ranged-find it 0 200)))
    (is (null (ranged-find it 4)))
    (is (null (ranged-find it 10)))
    (gtirb/ranged::in-range it)))
