(in-package #:ecrepl)

;;; This partial form reader is by no means a conforming C parser. It
;;; is a naive ad-hoc mechanism and works until it breaks. It should
;;; be replaced by such parser though.
;;;
;;; Fun weekend project: write a complete C18 -> AST parser and
;;; integrate it with ecrepl.

(define-condition syntax-problem (simple-error) ())

(defun read-line* ()
  (loop
     with text = ""
     with ch = #\\
     while (eq ch #\\)
     do (let* ((line (read-line))
               (len (length line)))
          (setf ch (elt line (1- len))
                text (concatenate 'string text line (string #\newline))))
     finally (return text)))

(defun cread-prep-if (str directive)
  (unless (member directive '(ifdef ifndef if))
    (error 'syntax-problem
           :format-control "Preprocessor directive ~a does not belong to the IF-GROUP."
           :format-arguments (list directive)))
  (loop
     for line = (read-line*)
     for text = (concatenate 'string str line) then (concatenate 'string text line)
     if (char= (char line 0) #\#) do
       (case (read-from-string line nil :eof :start 1)
         ((ifdef ifndef if)
          (setf text (cread-prep-if text directive)))
         ((elif else)
          #|nothing to do|#)
         (endif
          (return-from cread-prep-if (values text :prep :if-group))))))

(defun cread-prep ()
  (let* ((text (read-line*))
         (directive (read-from-string text nil :eof :start 1)))
    (case directive
      (include   (values text :prep :include))
      (warning   (values text :prep :warning))
      (define    (values text :prep :define))
      (pragma    (values text :prep :pragma))
      (undef     (values text :prep :undef))
      (error     (values text :prep :error))
      (line      (values text :prep :line))
      (:eof      (values text :prep :newline))
      (otherwise (cread-prep-if text directive)))))

(defun read-comment (buffer)
  (vector-push-extend (read-char) buffer)
  (loop for ch = (read-char)
     do
       (vector-push-extend ch buffer)
     when (and (char= ch #\*)
               (char= (peek-char) #\/))
     do
       (vector-push-extend (read-char) buffer)
       (return-from read-comment)))

(defun read-delimited-expr (pred &key
                                   (inclusive t)
                                   (buffer (make-array 20
                                                       :element-type 'character
                                                       :fill-pointer 0
                                                       :adjustable t)))
  (cond ((characterp pred)
         (setf pred (let ((xx pred)) (lambda (c) (char= c xx)))))
        ((listp pred)
         (setf pred (let ((xx pred)) (lambda (c) (member c xx))))))
  (loop
     for ch = (peek-char)
     when (funcall pred ch) do
       (when inclusive
         (vector-push-extend (read-char) buffer))
       (return-from read-delimited-expr buffer)
     do
       (vector-push-extend (read-char) buffer)
       (case ch
         (#\{ (read-delimited-expr #\} :buffer buffer))
         (#\" (read-delimited-expr #\" :buffer buffer))
         (#\/ (case (peek-char)
                (#\* (read-comment buffer))
                (#\/ (map nil (lambda (c)
                                (vector-push-extend c buffer))
                          (read-line*)))))
         (#\( (read-delimited-expr #\) :buffer buffer)))))

(defun read-number ()
  (prog1 (read-delimited-expr #\; :inclusive nil)
    (read-char)))

(defun read-token ()
  (read-delimited-expr
   (let ((spaces-in? nil)
         (characters '(#\; #\, #\( #\) #\{ #\} #\[ #\])))
     (lambda (ch)
       (when (and (not spaces-in?) (not (member ch '(#\space #\newline))))
         (setf spaces-in? t
               characters (list* #\space #\newline characters)))
       (member ch characters)))
   :inclusive nil))

(defun read-extension ()
  (error "not implemented yet"))

(defun cread-expr ()
  (let ((ch (peek-char t)))
    (cond ((member ch '(#\space #\newline #\\))
           (read-char))
          ((char= ch #\")
           (values (concatenate 'string
                                (string (read-char))
                                (read-delimited-expr #\"))
                   :rval :string))
          ((char= ch #\')
           (values (concatenate 'string
                                (string (read-char))
                                (read-delimited-expr #\'))
                   :rval :char))
          ((char= ch #\{)
           (values (concatenate 'string
                                (string (read-char))
                                (read-delimited-expr #\}))
                   :code :block))
          ((char= ch #\()
           (values (concatenate 'string
                                (string (read-char))
                                (read-delimited-expr #\)))
                   :rval :parens))
          ((char= ch #\@)
           (values (read-extension) :extension))
          ((digit-char-p ch)
           (values (read-number) :rval :number))
          ((alphanumericp ch)
           (let* ((first-token (read-token))
                  (last-char (elt first-token (1- (length first-token)))))
             (cond
               ;; token extern gives it all
               ((string= "extern" first-token)
                (values (concatenate 'string first-token (read-delimited-expr #\;)) :decl :extern))
               ;; either "foo;", "foo[];" or other rval
               ((char= #\; last-char)
                (values first-token :rval :simple))
               ((member first-token '("for" "while" "switch") :test #'string=)
                (values (concatenate 'string
                                     first-token
                                     (read-delimited-expr #\()
                                     (read-delimited-expr #\))
                                     (cread-expr))
                        :code))
               ((string= "do" first-token)
                (values (concatenate 'string
                                     first-token
                                     (cread-expr)
                                     (read-token)
                                     (read-delimited-expr #\()
                                     (read-delimited-expr #\))
                                     (read-delimited-expr #\;))
                        :code))
               ((string= "if" first-token)
                (values (concatenate 'string
                                     first-token
                                     (read-delimited-expr #\()
                                     (read-delimited-expr #\))
                                     (cread-expr)
                                     #+ (or) (cread-maybe-else-branch))
                        :code))
               ((string= "switch" first-token)
                (values (concatenate 'string
                                     first-token
                                     (read-delimited-expr #\()
                                     (read-delimited-expr #\))
                                     (cread-expr))
                        :code))
               ((string= "return" first-token)
                (values (concatenate 'string
                                     first-token
                                     (read-delimited-expr #\;))
                        :code))
               ((string= "_Generic" first-token)
                (values (concatenate 'string
                                     first-token
                                     (read-delimited-expr #\()
                                     (read-delimited-expr #\)))
                        :code))
               ;; token static gives it all (faulty, may be a function)
               ((string= "static" first-token)
                (let ((rest (read-delimited-expr '(#\; #\{))))
                  (if (char= #\; (elt rest (1- (length rest))))
                      (values (concatenate 'string first-token rest) :defn :static)
                      (values (concatenate 'string
                                           first-token
                                           rest
                                           (read-delimited-expr #\}))
                              :defn :static))))
               (t
                (let* ((second-token (read-token))
                       (rest (read-delimited-expr '(#\; #\{)))
                       (last-char (elt rest (1- (length rest)))))
                  (cond ((zerop (length (string-trim '(#\space #\newline) second-token)))
                         (values (concatenate 'string
                                              first-token
                                              second-token
                                              rest)
                                 :rval
                                 (if (char= #\( (elt rest 0)) :call :atom)))
                        ((char= #\; last-char)
                         (values (concatenate 'string
                                              first-token
                                              second-token
                                              rest)
                                 :defn :var))
                        ((char= #\{ last-char)
                         (values (concatenate 'string
                                              first-token
                                              second-token
                                              rest
                                              (read-delimited-expr #\}))
                                 :defn :fn))))))))
          (t
           (values (prog1 (read-token) (read-char))
                   :rval
                   (case ch
                     (#\& :address)
                     (#\* :derefernce)
                     (otherwise :unknown)))))))

(defun cread-block ()
  (values (concatenate 'string
                       (string (read-char))
                       (read-delimited-expr #\}))
          :code :block))

;;; Returns the form text and its category. Text is a string and
;;; category is of type:
;;;
;;;   (member :prep :decl :defn :code :rval :extension :ecreplcmd)
;;;
;;; Function may signal a condition SYNTAX-PROBLEM if it finds
;;; something uncanny.
(defun cread-form ()
  (loop                                 ; skip noise
     for ch = (peek-char)
     while (member ch '(#\space #\newline #\\))
     do (read-char))
  (case (peek-char nil)
    (#\; (return-from cread-form (progn
                                   (read-char)
                                   (values "" :ecreplcmd))))
    (#\# (return-from cread-form (cread-prep)))
    (#\{ (return-from cread-form (cread-block)))
    (otherwise (return-from cread-form (cread-expr)))))

(defun extract-declarations (text extra)
  (ecase extra
    (:static nil)
    (:var
     (with-input-from-string (*standard-input* text)
       (format nil "extern ~a;"
               (read-delimited-expr '(#\= #\;) :inclusive nil))))
    (:fn
     (with-input-from-string (*standard-input* text)
       (format nil "extern ~a;"
               (read-delimited-expr #\{ :inclusive nil))))))

(defun check-header (text)
  (let ((*standard-output* (make-broadcast-stream))
        (*compile-verbose* nil))
    (compile nil `(lambda ()
                    (ffi:clines ,@*all-headers* ,text)))))

(defun check-source (decl text)
  (let ((*standard-output* (make-broadcast-stream))
        (*compile-verbose* nil))
    (compile nil `(lambda ()
                    (ffi:clines ,@*all-headers* ,decl ,text)))))

(defun check-c-form (code)
  (let ((*standard-output* (make-broadcast-stream))
        (*compile-verbose* nil))
    (compile nil `(lambda ()
                    (ffi:clines ,@*all-headers*)
                    (ffi:c-inline nil nil :void ,code)))))

;;; These functions will expand into funnier calls later.
(defun wrap-fun (text)
  (concatenate 'string text ";"))

(defun wrap-var (text)
  (format nil "printx(~a);" text))

;;; Returns four values: header texts, source texts, free expression
;;; texts (which should be embedded in "main" function) and commands.
;;; All values are lists (possibly empty).
(defun cread ()
  (ext:collect (include-in-headers
                include-in-sources
                include-in-c-forms
                include-as-command)
    (multiple-value-bind (text category extra) (cread-form)
      (when *debug*
        (format *debug-io*
                "~&~s~%categorized as ~s (~s)." text category extra))
      (ecase category
        ((:prep :decl)
         (if (check-header text)
             (progn (include-in-headers text)
                    (include-as-command '(:print nil)))
             (format t "Header ~s is invalid. Ignoring.")))
        (:defn
         (let ((decl (extract-declarations text extra)))
           (if (check-source decl text)
               (progn 
                 (include-in-headers decl)
                 (include-in-sources text)
                 (include-as-command '(:print t)))
               (format t "Definition ~s is invalid. ~% Inferred header: ~s." text decl))))
        (:code
         (if (check-c-form (wrap-fun text))
             (progn
               (include-in-c-forms (wrap-fun text))
               (include-as-command '(:print t)))
             (format t "Code block ~s is invalid." text)))
        (:rval
         (cond ((check-c-form (wrap-var text))
                (include-in-c-forms (wrap-var text))
                (include-as-command '(:print last)))
               ((check-c-form (wrap-fun text))
                (include-in-c-forms (wrap-fun text))
                (include-as-command '(:print last)))
               (T (format t "rval ~s is invalid." text))))
        (:extension
         (multiple-value-bind (decl text name) (expand text extra)
           (when (check-sources decl text)
             (include-in-headers decl)
             (include-in-sources text)
             (include-as-command `(:print ,name)))))
        (:ecreplcmd
         (include-as-command
          (read-from-string (format nil "(~a)" text))))))
    (when *debug*
      (format *debug-io*
              "~&headers:~%~s~%sources:~%~s~%c-forms:~%~s~%commands:~%~s~%---~%"
              (include-in-headers)
              (include-in-sources)
              (include-in-c-forms)
              (include-as-command)))
    (values (include-in-headers)
            (include-in-sources)
            (include-in-c-forms)
            (include-as-command))))
