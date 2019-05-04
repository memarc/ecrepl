(in-package #:ecrepl)

;; http://www.robertgamble.net/2012/01/c11-generic-selections.html
;; https://en.cppreference.com/w/c/language/generic
;; https://stackoverflow.com/questions/9804371/syntax-and-sample-usage-of-generic-in-c11#17290414

;;; random note: clang and gcc expand _Generic to something like
;;; if (__builtin_type_match(x, y)) do_1;
;;; else if (__builtin_type_match(x, y)) do_2;
;;;
;;; that means that if we put function calls in expansion they are all
;;; compiled in a code and due to static typing they can't have
;;; signatures which won't match all possible _Generic
;;; combinations. That's why we use vaargs instead of exact function
;;; definitions.

(defparameter *known-c-types*
  '((:bool           . "_Bool")
    (:complex-float  . "_Complex float")
    (:complex-double . "_Complex double")
    (:complex-long   . "_Complex long double")))

(defun lisp-to-rep-type (key)
  (let ((result (cdr (assoc key *known-c-types*))))
    (or result
        (substitute #\space #\- (string-downcase key)))))

(defparameter *promoted-c-types*
  '((:bool               . "int")
    (:char               . "int")
    (:signed-char        . "int")
    (:unsigned-char      . "int")
    (:short              . "int")
    (:short-int          . "int")
    (:signed-short       . "int")
    (:signed-short-int   . "int")
    (:unsigned-short     . "int")
    (:unsigned-short-int . "int")
    (:float              . "double")))

(defun lisp-to-promoted (key)
  (let ((result (cdr (assoc key *promoted-c-types*))))
    (or result
        (substitute #\space #\- (string-downcase key)))))

;;; Pretty dumb type tagging. There is no semantic understanding what
;;; is a type, keyword is simply taken and assigned first available
;;; number. There may be two equivalent types with different coding
;;; and we put burden of ensuring that not happening on the programmer
;;; (i.e :LONG-LONG and :LONG-LONG-INT).
(defparameter *registered-types* (make-hash-table))
(defun lisp-to-rep-type-id (key)
  (or (gethash key *registered-types*)
      (setf (gethash key *registered-types*)
            (hash-table-count *registered-types*))))

(defun gf-name-mangler (name &rest arg-types)
  ;; naive, not fault tolerant. we use short type coding instead of
  ;; full names to save some characters (in C identifier has max 31).
  (concatenate 'string
               "ecl_gf"
               (apply #'concatenate 'string
                      (mapcar (lambda (type)
                                ;; short type coding 1-9A-Z
                                (format nil "_~35r" (lisp-to-rep-type-id type)))
                              arg-types))
               "_"
               (substitute #\_ #\- (string-downcase name))))

;;; builds an alist tree of all elements
;;; i.e ([a b c] [a c d] [a b d] [d c a]) will generate
;;; (a->[b->[c d] c->[d]] d->[c a])
(defun combinations (gf-name args)
  (labels ((combinations-1 (args assoc-lst arg-types)
             (let* ((arg (second (pop args)))
                    (arg* (lisp-to-rep-type arg))
                    (asc (assoc arg* assoc-lst :test #'string=)))
               (unless asc
                 (setf asc (cons arg* nil))
                 (push asc assoc-lst))
               (setf (cdr asc)
                     (if args
                         (combinations-1 args (cdr asc) (cons arg arg-types))
                         (apply #'gf-name-mangler gf-name (reverse (cons arg arg-types)))))
               assoc-lst)))
    (loop
       with assoc-lst = nil
       for list in args
       do (setf assoc-lst (combinations-1 list assoc-lst nil))
       finally
         (return assoc-lst))))

;;; c output routines
(defvar *c-str*)
(defvar *h-str*)
(defvar *indent* 0)

(defmacro with-compiler ((&optional print) &body body)
  `(let ((*c-str* (make-string-output-stream))
         (*h-str* (make-string-output-stream)))
     ,@body
     (let ((r1 (get-output-stream-string *h-str*))
           (r2 (get-output-stream-string *c-str*)))
       ,(when print
          `(format *debug-io* "/* h-file */~%~a~%~%/* c-file */~%~a~%" r1 r2))
       (ceval r1 r2 nil)
       (values r1 r2))))

(defun wc (fmt &rest args)
  (dotimes (v *indent*)
    (princ #\space *h-str*))
  (apply #'format *c-str* fmt args)
  (terpri *c-str*))

(defun wh (fmt &rest args)
  (dotimes (v *indent*)
    (princ #\space *h-str*))
  (apply #'format *h-str* fmt args)
  (terpri *h-str*))

(defun format-function (type name args &rest body)
  (wh "extern ~a ~a (~{~a~^, ~});" type name args)
  (wc "~a ~a (~{~a~^, ~}) {" type name args)
  (mapc (lambda (form)
          (format *c-str* "~4t~a~%" form))
        body)
  (wc "}~%"))


(defparameter *indent* 0)
(defparameter *nesting* 0)
(defun format-generic-inner (all-args all-types default &optional args (lastp t))
  (let ((arg (pop all-args)))
    (when arg
      (wh "_Generic((~a),~100t\\" arg)
      (let ((*indent* (+ 4 *indent*))
            (*nesting* (1+ *nesting*)))
        (loop
           for i on all-types
           for arg-type-tree-assoc = (car i)
           for arg-type-tree-type = (car arg-type-tree-assoc)
           for arg-type-tree-tree = (cdr arg-type-tree-assoc)
           ;;for args* = (list* (format nil "(~a) ~a" arg-type-tree-type arg) args)
           for args* = (list* arg args)
           do
             (cond ((listp arg-type-tree-tree)
                    (wh "~a: ~100t\\" arg-type-tree-type)
                    (format-generic-inner all-args
                                          arg-type-tree-tree
                                          default
                                          args*
                                          nil))
                   (t
                    (wh "~a: ~a (~a),~100t\\"
                        arg-type-tree-type
                        arg-type-tree-tree
                        (format nil "~{~a~^, ~}" (list* "NULL" (reverse args*))))))
           finally
             (let ((app (list* "NULL" (reverse args*))))
               (if lastp
                   (wh "default: ~a (~a)) ~100t\\" default (format nil "~{~a~^, ~}" app))
                   (wh "default: ~a (~a)),~100t\\" default (format nil "~{~a~^, ~}" app)))))))))

(defun format-generic (gf-name args arg-combinations)
  (wh "#define ~a(~{~a~^, ~})~100t\\" gf-name args)
  (let ((*indent* (+ *indent* 4)))
    (format-generic-inner args
                          arg-combinations
                          (gf-name-mangler gf-name :void* :rest)))
  (wh ""))


(defmacro define-no-applicable-method (gf-name)
  `(format-function (lisp-to-rep-type :void*)
                    (gf-name-mangler ,gf-name :void* :rest)
                    '("const void *fmt" "...")
                    (format nil "printf(\"~a: no applicable method.\\n\");" ,gf-name)
                    "return NULL;"))

(defmacro define-c11-method (gf-name args result-type &body body)
  (let* ((arg-names (mapcar #'first args))
         (arg-types (mapcar #'second args))
         (gf-name (string-downcase gf-name ))
         (method-name (apply #'gf-name-mangler gf-name arg-types)))
    `(format-function
      ;; the prototype
      (lisp-to-rep-type ,result-type) ,method-name '("const void *fmt" "...")
      ;; initialize va-args
      "va_list ecl_gf_vaargs;"
      "va_start(ecl_gf_vaargs, fmt);"
      ,@(mapcar (lambda (name type &aux
                                     (safe (lisp-to-promoted type))
                                     (type (lisp-to-rep-type type))
                                     (name (string-downcase name)))
                  (format nil "~a ~a = va_arg(ecl_gf_vaargs, ~a);" type name safe))
                arg-names
                arg-types)
      "va_end(ecl_gf_vaargs);"
      (let (,@(mapcar (lambda (arg)
                        (list arg (string-downcase arg)))
                      arg-names)
            ,@(mapcar (lambda (name type)
                        (list (intern (concatenate 'string "^TYPE-" (string name)))
                              (lisp-to-rep-type type)))
                      arg-names arg-types))
        
        ,@body))))

;;; todo:
;;;  - add optional type combinations with volatile, *, [] etc.
;;;  - add partial specializations (vaargs, first argument being fmt)
(defmacro define-c11-generic-function (gf-name args &rest methods)
  `(with-compiler (t)
     ;; 1. define functions (-> object file)
     ,@(loop
          for method in methods
          when (eq :method (first method))
          collect
            `(define-c11-method ,gf-name ,(second method) ,(third method) ,@(nthcdr 3 method)))
     (define-no-applicable-method ,(substitute #\_ #\- (string-downcase gf-name )))
     ;; 2. collect (A x B x C x ... x Z) combinations for _Generic
     ,(let ((combinations (combinations
                           gf-name
                           (mapcar (lambda (method)
                                     (second method))
                                   (remove-if-not
                                    (lambda (elt)
                                      (eq :method (first elt)))
                                    methods)))))
        `(format-generic ,(substitute #\_ #\- (string-downcase gf-name ))
                         '(,@(mapcar #'string-downcase args))
                         ',combinations))))



