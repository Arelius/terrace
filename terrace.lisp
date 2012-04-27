;; Copyright (c) 2009, Nicholas "Indy" Ray. All rights reserved.
;; See the LICENSE file for usage, modification, and distribution terms.
(require 'cl-who)
(require 'cl-markdown)
(require 'css-lite)

(defparameter *file-config* "config.ter")
(defparameter *html-extension* "html")

(defparameter *template-file* nil)

(defun make-local-path (file)
  (merge-pathnames (pathname file) (current-directory)))

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
         (eval (cl-who::tree-to-commands
                (cl-who::apply-to-tree
                 (lambda (x)
                   (cond
                    ((eq (car x) 'content)
                     `(cl-who:str
                       ,(with-output-to-string
                          (s)
                          (cl-markdown:markdown markup :stream s))))
                    ((eq (car x) 'css)
                     `(cl-who:str
                       (css-lite:css-string ,@(cdr x))))
                    ((eq (car x) 'inline-css)
                     `(css-lite:inline-css ,@(cdr x)))))
                 (lambda (x) (and
                         (consp x)
                         (or
                          (eq (car x) 'content)
                          (eq (car x) 'css)
                          (eq (car x) 'inline-css))))
                 *template*)
                out))
         (close out)))
     (directory (make-local-path "*.pt")))))

(ccl::save-application "terrace" :toplevel-function (lambda () (run-terrace) (ccl::quit)) :prepend-kernel t)