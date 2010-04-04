;; Copyright (c) 2009, Nicholas "Indy" Ray. All rights reserved.
;; See the LICENSE file for usage, modification, and distribution terms.
(require 'cl-who)
(require 'cl-markdown)

(defparameter *file-config* "config.ter")
(defparameter *html-extension* "html")

(defparameter *template-file* nil)

(defun make-local-path (file)
  (merge-pathnames (pathname file) (current-directory)))

;; from who.lisp
(defun apply-to-tree (function test tree)
  (declare (optimize speed space))
  (declare (type function function test))
  "Apply FUNCTION recursively to all elements of the tree TREE \(not
only leaves) which pass TEST."
  (cond
    ((funcall test tree)
      (funcall function tree))
    ((consp tree)
      (cons
       (apply-to-tree function test (car tree))
       (apply-to-tree function test (cdr tree))))
    (t tree)))

(defun run-terrace ()
  (if (not (probe-file (make-local-path *file-config*)))
      (progn
        (format *standard-output* "Missing config file: ~A." *file-config*)
        (quit)))

  (load (make-local-path *file-config*))

  (let ((*template* (let ((in (open (make-local-path *template-file*)))
                          (html nil))
                      (loop for line = (read in nil)
                            while line do
                            (setf html (cons line html)))
                      (close in)
                      html)))
    (mapcar
     (lambda (file)
       (let ((in (open file))
             (out (open
                   (make-pathname :type *html-extension* :defaults file)
                   :direction :output :if-exists :supersede))
             (markup ""))
         (loop for line = (read-line in nil)
               while line do
               (setf markup (concatenate 'string markup line)))
         (close in)
         (eval (cl-who:tree-to-commands
                (apply-to-tree
                 (lambda (x)
                   `(cl-who:str
                     (let ((x
                            ,(with-output-to-string
                               (s)
                               (cl-markdown:markdown markup :stream s))))
                       x)))
                 (lambda (x) (eq x 'content))
                 *template*)
                out))
         (close out)))
     (directory (make-local-path "*.pt")))))

(ccl::save-application "terrace" :toplevel-function (lambda () (run-terrace) (ccl::quit)) :prepend-kernel t)