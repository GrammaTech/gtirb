(defpackage :gtirb/ranged
  (:use :common-lisp)
  (:import-from :interval)
  (:export :make-ranged
           :ranged-insert
           :ranged-delete
           :ranged-find
           :ranged-find-at))
(in-package :gtirb/ranged)
#-debug (declaim (optimize (speed 3) (safety 0) (debug 0)))
#+debug (declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun make-ranged ()
  (interval:make-tree))

(defstruct (uuid-interval (:include interval:interval))
  (uuid 0 :type integer))

(defun uuid-interval= (i1 i2)
  (= (uuid-interval-uuid i1) (uuid-interval-uuid i2)))

(defun ranged-insert (tree uuid start end)
  (interval:insert tree (make-uuid-interval :uuid uuid
                                            :start start
                                            :end end)))

(defun ranged-delete (tree uuid start end)
  (interval:delete tree (make-uuid-interval :uuid uuid
                                            :start start
                                            :end end)))

(defun ranged-find-at (tree address)
  (mapcar #'uuid-interval-uuid
          (remove-if-not (lambda (i)
                           (= address (interval:interval-start i)))
                         (interval:find-all tree address))))

(defun ranged-find (tree start &optional (end start))
  (mapcar #'uuid-interval-uuid (interval:find-all tree (cons start end))))
