;; -*- lisp -*-
;;
;; Usage: write-documentation PKG ABSTRACT OUTPUT-HTML
;;   Write the automatically generated documentation for PKG to
;;   OUTPUT-HTML.  The ABSTRACT should be a file in Markdown format to
;;   place at the top of the generated documentation.
;;
(load "~/.sbclrc" :if-does-not-exist nil)
(ql:quickload :alexandria)
(ql:quickload :simpler-documentation-template)
(ql:quickload :markdown.cl)
(unless (= (length sb-ext:*posix-argv*) 5)
  (format t "Usage: write-documentation PKG ABSTRACT OUTPUT-HTML"))
(let ((package (alexandria:make-keyword
                (string-upcase (second sb-ext:*posix-argv*))))
      (abstract-path (third sb-ext:*posix-argv*))
      (output-path (fourth sb-ext:*posix-argv*)))
  (ql:quickload package)
  #+debug (format t "Writing: ~S~%" (list package abstract-path output-path))
  (simpler-documentation-template:create-template
   package
   :maybe-skip-methods-p t
   :target output-path
   :abstract-html (markdown.cl:parse
                   (with-open-file (in abstract-path)
                     (let* ((file-bytes (file-length in))
                            (seq (make-string file-bytes))
                            (file-chars (read-sequence seq in)))
                       (subseq seq 0 file-chars))))))
