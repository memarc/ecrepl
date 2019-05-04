
(in-package #:ecrepl)

(defvar *all-headers* (list (format nil "/* ecrepl header file */~%")))
(defvar *all-sources* (list (format nil "/* ecrepl source file */~%")))
(defvar *all-c-forms* (list (format nil "/* ecrepl c-form list */~%")))

(defun cexec (commands)
  nil)

(defmacro ensure-list (x)
  `(if (listp ,x)
       ,x
       (list ,x)))

(defun ceval (headers sources c-forms)
  (setf headers (ensure-list headers)
        sources (ensure-list sources)
        c-forms (ensure-list c-forms))
  (setf *all-headers* (nconc *all-headers* headers)
        *all-sources* (nconc *all-sources* sources)
        *all-c-forms* (nconc *all-c-forms* c-forms))
  (unless (or sources c-forms)
    (return-from ceval nil))
  (when *debug*
    (format *debug-io* "compiling program~%---~%~a~%---~%"
            (concatenate 'string
                         (apply #'concatenate 'string *all-headers*)
                         (format nil "~%/* end of header file */~%")
                         (apply #'concatenate 'string sources)
                         (format nil "~%~%void <gensym> () { ~{~a~} }~%" c-forms))))
  (let* ((*compile-verbose* nil)
         (function (compile nil `(lambda ()
                                   (ffi:clines ,@*all-headers* ,@sources)
                                   (ffi:c-inline nil nil :void ,(format nil "~{~a~^~%~}" c-forms))))))
    (prog1 function
      (funcall function))))
