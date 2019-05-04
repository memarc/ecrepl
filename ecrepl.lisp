;;; (c) 2019 Daniel Kochmański
;;; (l) BSD-2-Clause

#-ecl (error "Moderately evil laugh.")

(defpackage #:ecrepl
  (:use #:cl)
  (:export #:cread #:cload #:close #:clist #:crepl))
(in-package #:ecrepl)

(defvar *debug* nil)
(flet ((reload (path)
         (load (compile-file path))))
  (reload "module-manager")
  (reload "c11-defgeneric")
  (reload "printx")
  (reload "syntax-reading"))

;;; ensure synchronous output with printf. in threaded environment we
;;; use fdopen, so we need to manually curb stdout.
#-threads
(ext:set-buffering-mode ext:+process-standard-output+ :none)
#+threads
(funcall (compile nil (lambda () (ffi:c-inline nil nil :void "setvbuf(stdout, NULL, _IONBF, 0);"))))

(defun read-eval-print ()
  (fresh-line)
  (format t "~&[ecrepl]% ")
  ;; CREAD is a naive, ad-hoc and incomplete C11 syntax parser. How
  ;; cool would it be to have a full grown parser for the language?
  (multiple-value-bind (headers sources c-forms commands)
      (cread)
    (ceval headers sources c-forms)
    ;; Not necessarily print (executes all commands, corresponding
    ;; print commands should be added by CREAD when applicable).
    (cexec commands)))

(defun crepl ()
  (format t "Press C-d to exit.~%")
  (loop
     (handler-case (read-eval-print)
       (ext:interactive-interrupt ()
         (format t "Press C-d to exit.~%"))
       (end-of-file ()
         (return-from crepl)))))
