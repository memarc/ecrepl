(in-package #:ecrepl)

;;; example function
(define-c11-generic-function generic-test (arg1 arg2)
  (:default (arg1 arg2) :void
     (format nil "printf(\"~a\\n\");" "Dispatch error: method not found."))
  (:method ((arg1 :int) (arg2 :int)) :void
     #1=(format nil "printf(\"~a\", ~a, ~a, ~a, ~a);"
                "[%s x %s] %d %d\\n"
                (format nil "~s" ^type-arg1)
                (format nil "~s" ^type-arg2)
                arg1 arg2))
  (:method ((arg1 :int) (arg2 :char)) :void   #1#)
  (:method ((arg1 :float) (arg2 :char)) :void #1#))

;;; for complex
(ceval "#include <complex.h>" nil nil)

;;; define generic function printx to print results
(macrolet ((frob ()
             `(define-c11-generic-function printx (arg)
                ,@(mapcar (lambda (elt)
                            `(:method ((arg ,(first elt))) :void
                                      (format nil ,@(cdr elt))))
                          `((:bool "printf(\"(~a) %s\", ~a ? ~s : ~s);" ^type-arg arg "true" "false")
                            (:char "printf(\"(~a) %c\", ~a);" ^type-arg arg)
                            (:signed-char "printf(\"(~a) %d\", ~a);" ^type-arg arg)
                            (:unsigned-char "printf(\"(~a) %u\", ~a);" ^type-arg arg)
                            (:short "printf(\"(~a) %d\", ~a);" ^type-arg arg)
                            (:unsigned-short "printf(\"(~a) %u\", ~a);" ^type-arg arg)
                            (:int "printf(\"(~a) %d\", ~a);" ^type-arg arg)
                            (:unsigned-int "printf(\"(~a) %u\", ~a);" ^type-arg arg)
                            (:long "printf(\"(~a) %ld\", ~a);" ^type-arg arg)
                            (:unsigned-long "printf(\"(~a) %lu\", ~a);" ^type-arg arg)
                            (:long-long "printf(\"(~a) %lld\", ~a);" ^type-arg arg)
                            (:unsigned-long-long "printf(\"(~a) %llu\", ~a);" ^type-arg arg)
                            (:float "printf(\"(~a) %f\", ~a);" ^type-arg arg)
                            (:double "printf(\"(~a) %f\", ~a);" ^type-arg arg)
                            (:long-double "printf(\"(~a) %Lf\", ~a);" ^type-arg arg)
                            (:complex-float "printf(\"(~a) %f%+fi\", crealf(~a), cimagf(~a));" ^type-arg arg arg)
                            (:complex-double "printf(\"(~a) %f%+fi\", creal(~a), cimag(~a));" ^type-arg arg arg)
                            (:complex-long "printf(\"(~a) %Lf%+Lfi\", creall(~a), cimagl(~a));" ^type-arg arg arg)
                            (:char* "printf(\"(~a) %s\", ~a);" ^type-arg arg)
                            (:cl_object "printf(\"(~a) %p\", ~a);" ^type-arg arg))))))
  (frob))
