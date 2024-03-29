(defpackage :gtirb/test
  (:use :common-lisp
        :alexandria
        :flexi-streams
        :stefil
        :gtirb
        :gtirb/dot
        :gtirb/utility
        :gtirb/ranged
        :gtirb/version
        :graph
        :named-readtables :curry-compose-reader-macros)
  (:import-from :trivial-package-local-nicknames :add-package-local-nickname)
  (:import-from :gtirb.proto)
  (:import-from :gtirb/utility :check-magic-header :write-magic-header)
  (:import-from :md5 :md5sum-file :md5sum-sequence)
  (:import-from :uiop :nest :run-program :with-temporary-file :quit)
  (:import-from :asdf/system :system-relative-pathname)
  (:shadowing-import-from :gtirb :symbol)
  (:export :test :batch-test))
(in-package :gtirb/test)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-package-local-nickname :proto :gtirb.proto))

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

(defvar *gtirb-dir* (system-relative-pathname "gtirb" "../"))


;;;; Fixtures.
(defixture hello
  (:setup
   (progn
     #+live-w-ddisasm
     (with-temporary-file (:pathname bin-path)
       (setf *proto-path* (with-temporary-file (:pathname p :keep t) p))
       (run-program (format nil "echo 'main(){puts(\"hello world\");}'~
                                     |gcc -x c - -o ~a"
                            bin-path) :force-shell t)
       (run-program
        (format nil "ddisasm --ir ~a ~a" *proto-path* bin-path)))
     #-live-w-ddisasm
     (setf *proto-path*
           (merge-pathnames "python/tests/hello.gtirb" *gtirb-dir*))))
  (:teardown
   (progn
     #+live-w-ddisasm (delete-file *proto-path*)
     (setf *proto-path* nil))))


;;;; Main test suite.
(defsuite test)
(in-suite test)

(deftest we-have-versions ()
  (is (stringp gtirb-version))
  (is (integerp protobuf-version)))

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
      ;; Protobuf provides multiple options for serializing repeated
      ;; enums, so this check can fail even with valid serialization.
      #+inhibited
      (is (equalp (md5sum-file *proto-path*)
                  (md5sum-file path))))))

(deftest read-gtirb-from-streams-and-files ()
  (with-fixture hello
    (is (typep (read-gtirb *proto-path*) 'gtirb))
    (is (typep (read-gtirb (namestring *proto-path*)) 'gtirb))
    (is (typep (with-open-file (input *proto-path*) (read-gtirb input))
               'gtirb))))

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
           (mapcar [{format nil "~/gtirb::aux-data-type-print/"} #'aux-data-type #'cdr]
                   (aux-data (first (modules it))))
           :test #'string=)))))

(deftest idempotent-aux-data-decode-encode ()
  (let ((type1 '(:tuple :uuid :uint64-t :int64-t :uint64-t))
        (type2 '(:sequence :uuid))
        (type3 '(:sequence (:variant :uuid :string)))
        (data '(1 2 3 4))
        (data2 '((0 . 1) (1 . "foo") (0 . 3) (1 . "bar"))))
    (is (equalp
         (gtirb::aux-data-decode type1 (gtirb::aux-data-encode type1 data))
         data))
    (is (equalp
         (gtirb::aux-data-decode type2 (gtirb::aux-data-encode type2 data))
         data))
    (is (equalp
         (gtirb::aux-data-decode type3 (gtirb::aux-data-encode type3 data2))
         data2)))
  (with-fixture hello
    (let ((hello (read-gtirb *proto-path*)))
      (mapc (lambda (pair)
              (destructuring-bind (name . aux-data) pair
                (let* ((orig (proto:data (gtirb::proto aux-data)))
                       (type (aux-data-type aux-data))
                       (new (gtirb::aux-data-encode
                             type (aux-data-data aux-data))))
                  (is (equalp (gtirb::aux-data-decode type new)
                              (gtirb::aux-data-decode type orig))
                      "~s with type ~a encodes/decodes is not idempotent~%~s"
                      name (aux-data-type aux-data)
                      (list (gtirb::aux-data-decode type orig)
                            (gtirb::aux-data-decode type new))))))
            (aux-data (first (modules hello)))))))

(deftest test-check-magic-header ()
  (is (signals gtirb-magic-error
        (check-magic-header #())))
  (is (signals gtirb-magic-error
        (check-magic-header #(0 0 0 0 0 0 0 0))))
  (is (signals gtirb-magic-error
        (check-magic-header #(71 84 73 82 66 0 0 0))))
  (is (null (check-magic-header (vector 71 84 73 82 66 0 0 protobuf-version)))))

(deftest test-write-magic-header ()
  (with-output-to-sequence (out)
    (write-magic-header out)
    (is (equalp (get-output-stream-sequence out)
                (vector 71 84 73 82 66 0 0 protobuf-version)))))

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

(deftest create-module-with-a-name ()
  (is (string= "foo"
               (nest
                (pb:string-value)
                (proto:name)
                (gtirb::proto)
                (make-instance 'module :name "foo" :allow-other-keys t)))))

;; FIXME: create-module-without-a-name should verify that the appropriate
;; warning is emitted, but for some reason the warning doesn't percolate up
;; to the point where the signals testing macro is able to catch it.
(deftest create-module-without-a-name ()
  (is (emptyp (nest
               (pb:string-value)
               (proto:name)
               (gtirb::proto)
               (make-instance 'module)))))

(deftest entry-points-on-modules ()
  (with-fixture hello
    (let* ((hello (read-gtirb *proto-path*))
           (module (first (modules hello)))
           (code-block (nest (first)
                             (remove-if-not {typep _ 'code-block})
                             (blocks hello))))
      ;; Entry-point is read as a code block.
      (is (typep (entry-point module) 'code-block))
      ;; Newly saved entry-point has the UUID of the code block.
      (setf (entry-point module) code-block)
      (is (= (uuid code-block)
             (uuid-to-integer (proto:entry-point (gtirb::proto module))))))))

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
               (blocks (read-gtirb *proto-path*))))))

(deftest find-every-block-in-the-module ()
  (nest (with-fixture hello)
        (let ((it (read-gtirb *proto-path*))))
        (is)
        (every {get-uuid _ it})
        (mapcar #'uuid)
        (blocks it)))

(deftest get-blocks-and-bytes-from-cfg-nodes ()
  (nest (with-fixture hello)
        (let ((it (read-gtirb *proto-path*))))
        (is)
        (every {get-uuid _ it})
        (nodes)
        (cfg it)))

(deftest shift-subseq-adds-and-removes-as-expected ()
  (let ((it #(1 2 3 4 5)))
    (setf (gtirb::shift-subseq it 2 3) #(9 9 9 9))
    (is (equalp it #(1 2 9 9 9 9 4 5))))
  (let ((it #(1 2 3 4 5)))
    (setf (gtirb::shift-subseq it 2 3) #())
    (is (equalp it #(1 2 4 5)))))

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
                                          (modules it)))
           (target (first (mappend #'blocks original-byte-intervals)))
           (original-bi-size (size (byte-interval target)))
           (original-bi-bytes (bytes (byte-interval target)))
           (original-size (length (bytes target)))
           (rest-block-bytes
            (cdr (mapcar #'bytes (blocks (byte-interval target))))))
      (setf (bytes target) (make-array (* 2 original-size) :initial-element 9))
      ;; First block bytes are updated.
      (is (equalp (bytes (first (mappend #'blocks original-byte-intervals)))
                  (make-array (* 2 original-size) :initial-element 9)))
      ;; Block size is updated.
      (is (equalp (size target) (* 2 original-size)))
      ;; Byte-interval size is updated.
      (is (equalp (size (byte-interval target))
                  (+ original-bi-size original-size)))
      ;; Remainder of the byte-interval's bytes are the same.
      (is (equalp (subseq (bytes (byte-interval target)) (* 2 original-size))
                  (subseq original-bi-bytes original-size)))
      ;; Remaining blocks bytes are the same.
      (is (every #'equalp
                 rest-block-bytes
                 (cdr (mapcar #'bytes (blocks (byte-interval target)))))))))

(deftest address-range-block-lookup ()
  (with-fixture hello
    (let* ((it (read-gtirb *proto-path*))
           (a-block (first (blocks it))))
      (is (= 2 (length (address-range a-block))))
      (is (member a-block (nest (mappend #'blocks)
                                (on-address it)
                                (first)
                                (address-range a-block))))
      (is (member a-block (nest (mappend #'blocks)
                                (at-address it)
                                (first)
                                (address-range a-block))))
      (is (not (member a-block (nest (mappend #'blocks)
                                     (at-address it)
                                     (second)
                                     (address-range a-block))))))))

(deftest symbolic-expressions-pushed-back ()
  (with-fixture hello
    ;; Collect a byte-interval with offset symbolic expressions.
    (let* ((bi (nest (find-if [{some [#'not #'zerop #'car]}
                               #'hash-table-alist #'symbolic-expressions])
                     (cdr) (mappend #'byte-intervals)
                     (sections) (first) (modules)
                     (read-gtirb *proto-path*)))
           (original-length (length (bytes bi)))
           (original-offsets (hash-table-keys (symbolic-expressions bi)))
           (largest-offset (extremum original-offsets #'>))
           (smallest-offset (extremum original-offsets #'<)))
      ;; Add bytes after the last symbol.
      (setf (bytes bi largest-offset largest-offset)
            (make-array 2 :initial-element 9))
      ;; Ensure bytes have been altered.
      (is (> (length (bytes bi)) original-length))
      ;; Ensure symbolic expressions before last have not moved.
      (is (set-equal (remove (+ largest-offset 2)
                             (hash-table-keys (symbolic-expressions bi)))
                     (remove largest-offset original-offsets)))
      ;; Remove those added bytes
      (setf (bytes bi largest-offset (+ largest-offset 2)) #())
      ;; Add bytes at the beginning.
      (setf (bytes bi 0 0) (make-array 1 :initial-element 9))
      ;; Ensure symbolic expressions have actually been pushed back.
      (is (set-equal (mapcar #'1+ original-offsets)
                     (hash-table-keys (symbolic-expressions bi))))
      ;; Drop bytes at the beginning.
      (setf (bytes bi 0 (1+ smallest-offset)) #())
      ;; Ensure this symbolic expression now starts at the beginning.
      (is (zerop (extremum (hash-table-keys (symbolic-expressions bi))
                           #'<))))))

(defvar *listing-comments*)
(defun get-listing-comments (it)
  (when-let ((table (assoc "comments" (gtirb::aux-data-w-offsets (ir it))
                           :test #'equalp)))
    (hash-table-alist (aux-data-data (cdr table)))))
(defmethod listing :around (object &key stream comments)
  (declare (ignorable stream comments))
  (let ((*listing-comments* (if (boundp '*listing-comments*)
                                *listing-comments*
                                (get-listing-comments object))))
    (call-next-method)))
(defgeneric listing (OBJECT &key stream comments)
  (:documentation "Print a listing of OBJECT.")
  (:method ((self gtirb) &key (stream t) comments)
    (format stream "~&IR~%")
    (mapc {listing _ :stream stream :comments comments} (modules self)))
  (:method ((self module) &key (stream t) comments)
    (format stream "~&Module: ~S~%" (name self))
    (mapc {listing _ :stream stream :comments comments} (sections self)))
  (:method ((self section) &key (stream t) comments)
    (format stream "~&Section: ~S" (name self))
    (mapc {listing _ :stream stream :comments comments} (byte-intervals self)))
  (:method ((self byte-interval) &key (stream t) comments)
    (format stream "~:[ (no address)~; at ~a~]~%" (addressp self) (address self))
    (mapc {listing _ :stream stream :comments comments} (blocks self)))
  (:method ((self gtirb-byte-block) &key (stream t) comments)
    (let ((bytes (bytes self))
          (comments (when comments
                      (remove-if-not [{= (uuid self)} #'caar] *listing-comments*))))
      (dotimes (step (size self))
        (format stream (if (zerop step) "~&>" " "))
        (if-let ((comment (and comments (find-if [{= step} #'cadar] comments))))
          (format stream "~4X ; ~A~%" (aref bytes step) (cdr comment))
          (format stream "~4X" (aref bytes step)))
        (when (= 7 (mod step 8)) (format stream "~&"))))))

(defun block-comments (data-block it)
  (sort (nest (remove-if-not [{= (uuid data-block)} #'caar])
              (hash-table-alist)
              (aux-data-data) (cdr)
              (assoc "comments" (gtirb::aux-data-w-offsets (ir it))
                     :test #'equalp))
        #'< :key [#'second #'car]))

(deftest offsets-pushed-back ()
  (nest (let ((gtirb::*update-aux-data-offsets* t)))
        (with-fixture hello)
        (let* ((it (read-gtirb *proto-path*))
               (text (find-if [{string= ".text"} #'name]
                              (sections (first (modules it)))))
               (commented-block (find-if {block-comments _ it} (blocks text)))
               (comment (first (block-comments commented-block it)))
               (starting-offset (cadar comment)))
          (is (gtirb::get-aux-data-w-offsets it)
              "We're finding aux-data tables with offsets. ~
             Should be at least comments and cfiDirectives.")
          (is (gtirb::aux-data-w-offsets it)
              "The `aux-data-w-offsets' was populated.")
          #+nil (listing commented-block :comments t)
          (setf (bytes commented-block 0 0) #(0 0 0 0))
          #+nil (listing commented-block :comments t)
          (is (= (+ 4 starting-offset)
                 (cadar (first (block-comments commented-block it))))))))

(deftest block-symbolic-expressions ()
  (with-fixture hello
    (is (nest (mappend [#'hash-table-values #'symbolic-expressions])
              (mappend #'blocks) (mappend #'byte-intervals)
              (sections) (first) (modules)
              (read-gtirb *proto-path*)))))

(deftest symbolic-expressions-maintained ()
  (nest
   (with-fixture hello)
   (mapc (lambda (db)
           (let ((o-offset (offset db))
                 (o-bi-size (size (byte-interval db)))
                 (o-db-size (size db))
                 (o-bi-se-size (hash-table-size
                                (symbolic-expressions (byte-interval db))))
                 (o-se-size (hash-table-size
                             (symbolic-expressions db))))
             (setf (bytes db 0 0) #(#x90 #x90 #x90 #x90))
             (is (= o-offset (offset db)))
             (is (= (+ 4 o-bi-size) (size (byte-interval db))))
             (is (= (+ 4 o-db-size) (size db)))
             (is (= o-bi-se-size
                    (hash-table-size
                     (symbolic-expressions (byte-interval db)))))
             (is (= o-se-size
                    (hash-table-size
                     (symbolic-expressions db)))))))
   (blocks (read-gtirb *proto-path*))))

(deftest payload-can-be-read-and-set ()
  (with-fixture hello
    (let* ((it (read-gtirb *proto-path*))
           (symbols (mappend #'symbols (modules it)))
           (referent-symbol
            (find-if [#'proto:has-referent-uuid #'gtirb::proto] symbols)))
      ;; Reading gives the right type of payload.
      (is (subtypep (type-of (payload referent-symbol)) 'gtirb::proto-backed))
      ;; Setting a payload has the right effect.
      (setf (payload referent-symbol) 42) ; Referent to value.
      (is (subtypep (type-of (payload referent-symbol)) 'number))
      (is (not (proto:has-referent-uuid (gtirb::proto referent-symbol)))))))

(deftest can-create-without-parents ()
  (is (typep (make-instance 'module) 'module))
  (is (typep (make-instance 'section) 'section))
  (is (typep (make-instance 'byte-interval) 'byte-interval)))

(deftest direct-ir-access ()
  (with-fixture hello
    (let* ((it (read-gtirb *proto-path*)))
      (is (typep (ir (setf it (first (modules it)))) 'gtirb))
      (is (typep (ir (setf it (first (sections it)))) 'gtirb))
      (is (typep (ir (setf it (first (byte-intervals it)))) 'gtirb))
      (is (typep (ir (setf it (first (blocks it)))) 'gtirb)))))

(deftest every-symbolic-expression-has-symbols ()
  (flet ((every-symbolic-expression-has-symbols-proto (path)
           (every (lambda (se)
                    (cond
                      ((proto:has-addr-const se)
                       (proto:symbol-uuid (proto:addr-const se)))
                      ((proto:has-addr-addr se)
                       (proto:symbol1-uuid (proto:addr-addr se)))))
                  (nest
                   (mapcar #'proto:value)
                   (mappend [{coerce _ 'list} #'proto:symbolic-expressions])
                   (mappend [{coerce _ 'list} #'proto:byte-intervals])
                   (coerce (proto:sections
                            (aref (proto:modules
                                   (read-proto 'proto:ir path)) 0))
                           'list))))
         (every-symbolic-expression-has-symbols-gtirb (path)
           (nest (every #'symbols)
                 (mappend [#'hash-table-values #'symbolic-expressions])
                 (mappend #'byte-intervals)
                 (mappend #'sections)
                 (modules (read-gtirb path)))))
    (with-fixture hello
      ;; First confirm for the protobuf
      (is (every-symbolic-expression-has-symbols-proto *proto-path*))
      ;; Second confirm for the GTIRB representation.
      (is (every-symbolic-expression-has-symbols-gtirb *proto-path*))
      ;; Then re-confirm both for a rewritten protobuf file.
      (uiop:with-temporary-file (:pathname temporary-file)
        (write-gtirb (read-gtirb *proto-path*) temporary-file)
        (is (every-symbolic-expression-has-symbols-proto temporary-file))
        (is (every-symbolic-expression-has-symbols-gtirb temporary-file))))))

#+ignore-expected-failure
(deftest every-block-is-found-at-its-address ()
  (with-fixture hello
    (let ((it (read-gtirb *proto-path*)))
      (nest (is)
            (every «member #'identity [{at-address it} #'address]»)
            (mappend #'blocks)
            (mappend #'byte-intervals)
            (mappend #'sections)
            (modules it)))))

(deftest truncating-size-on-byte-interval-errors-and-truncates ()
  (with-fixture hello
    (let ((byte-intervals (nest (mappend #'byte-intervals)
                                (mappend #'sections)
                                (modules)
                                (read-gtirb *proto-path*))))
      ;; Truncation signals an error.
      (signals ir
        (setf (size (first byte-intervals))
              (1- (length (contents (first byte-intervals))))))
      ;; Truncate restart works.
      (let* ((bi (second byte-intervals))
             (original-length (length (contents bi))))
        (handler-bind
            ((ir
              (lambda (e)
                (if (find-restart 'truncate-contents)
                    (invoke-restart 'truncate-contents)
                    (error e)))))
          (setf (size bi) (1- original-length)))
        (is (= original-length
               (+ 1 (size bi))
               (+ 1 (length (contents bi))))))
      ;; Ignore restart works.
      (let* ((bi (third byte-intervals))
             (original-length (length (contents bi))))
        (handler-bind
            ((ir
              (lambda (e)
                (if (find-restart 'ignore)
                    (invoke-restart 'ignore)
                    (error e)))))
          (setf (size bi) (1- original-length)))
        (is (= original-length (length (contents bi))))
        (is (> original-length (size bi)))))))


;;;; Dot test suite
(deftest write-dot-to-file ()
  (with-fixture hello
    (with-temporary-file (:pathname path)
      (to-dot-file (read-gtirb *proto-path*) path))))
