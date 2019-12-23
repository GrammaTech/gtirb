(defpackage :gtirb/ranged
  (:use :common-lisp)
  (:import-from :uiop :nest)
  (:import-from :cl-containers
                :binary-search-tree
                :insert-item
                :delete-item
                :size
                :element
                :find-successor-node
                :predecessor
                :successor)
  (:export :ranged
           :ranged-insert
           :ranged-delete
           :ranged-find))
(in-package :gtirb/ranged)
(declaim (optimize (speed 3) ;; (safety 0) (debug 0)
                   ))

(defclass ranged ()
  ((tree :initform (make-instance 'binary-search-tree :key #'car :test #'=)
         :type binary-search-tree
         :documentation "Internal store."))
  (:documentation "A collection supporting retrieval of objects by integer ranges."))

(defmethod print-object ((obj ranged) (stream stream))
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (tree) obj (format stream "~a" (size tree)))))

(declaim (inline key))
(declaim (ftype (function (cl-containers::bst-node) integer) key))
(defun key (node) (car (element node)))

(defun in-range (ranged start end)
  (check-type ranged ranged "A ranged collection.")
  (check-type start integer "An integer index into the range.")
  (check-type end integer "An integer index into the range.")
  (with-slots (tree) ranged
    (let ((start-item (find-successor-node tree (list start))))
      ;; TODO: Handle case of no successor.
      ;; TODO: Empty ranges on either end. (Require a min and max?)
      (unless (= (key start-item) start)
        (setf start-item (predecessor tree start-item)))
      (let ((last (successor tree start-item))
            (ranges (list start-item)))
        (loop
           :while (<= (key last) end)
           :do (progn (push last ranges) (setf last (successor tree last)))
           :finally (return ranges))))))

(defun ranged-insert (ranged item start &optional (end start))
  "Insert ITEM into ranged collection RANGED between START and END.
Return the RANGED collection after inserting ITEM."
  (check-type ranged ranged "A ranged collection.")
  (check-type start integer "An integer index into the range.")
  (check-type end integer "An integer index into the range.")
  (nest
   (with-slots (tree) ranged)
   (flet ((replace-node (node &aux (element (element node)))
            (delete-item tree node)
            (insert-item tree (cons (car element) (cons item (cdr element)))))))
   (let ((ranges (in-range ranged start end)))
     ;; Handle the first range, might need to be split.
     (let ((first (pop ranges)))
       (if (= start (key first))
           ;; No need to split, simply replace.
           (replace-node first)
           ;; Add a new node (without deleting the original) which has
           ;; the effect of splitting the range of the original.
           (insert-item tree (cons start (cons item (cdr (element first)))))))
     ;; Handle the middle ranges.
     (loop :while (> (length ranges) 1) :do (replace-node (pop ranges)))
     ;; Handle the last range, might need to be split.
     (let ((last (pop ranges)))
       (unless (= end (key last))
         ;; Split the original range into two.
         (insert-item tree (cons (1+ end) (cdr (element last)))))
       ;; Add the new item to the last range.
       (replace-node last))))
  ranged)

(defun ranged-drop (ranged item start &optional (end start))
  "Delete ITEM from ranged collection RANGED between START and END.
Return the RANGED collection after deleting ITEM."
  (check-type ranged ranged "A ranged collection.")
  (check-type start integer "An integer index into the range.")
  (check-type end integer "An integer index into the range."))

(defun ranged-find (ranged start &optional (end start))
  "Find all items in RANGED between START and END."
  (check-type ranged ranged "A ranged collection.")
  (check-type start integer "An integer index into the range.")
  (check-type end integer "An integer index into the range."))
