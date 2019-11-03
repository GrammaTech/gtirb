(in-package :gtirb)

(uiop/stream:with-temporary-file (:pathname path :keep nil)
  (let ((it (make-instance 'gtirb))
        (test-string "Something."))
    (push (cons "test" (make-instance 'aux-data)) (aux-data it))
    (setf (aux-data-type (cdar (aux-data it))) :string
          (aux-data-data (cdar (aux-data it))) test-string)
    (write-gtirb it path)
    (assert (string= (aux-data-data (cdar (aux-data (read-gtirb path))))
                     test-string)
            (path) "AuxData in GTIRB at ~s holds ~s." path test-string)))
