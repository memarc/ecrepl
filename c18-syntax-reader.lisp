;;; (c) 2019 Daniel Kochmański
;;; (l) BSD-2-Clause

;;(in-package #:ecrepl)

(defclass <abstract-token> ()
  ((start :initarg :start)
   (end   :initarg :end)))

(defclass <token>               (<abstract-token>) ())
(defclass <preprocessing-token> (<abstract-token>) ())

(defclass <keyword>        (<token>) ())
(defclass <identifier>     (<token>) ())
(defclass <constant>       (<token>) ())
(defclass <string-literal> (<token>) ())
(defclass <punctator>      (<token>) ())

(defclass <header-name>        (<preprocessing-token>) ())
(defclass <identifier>         (<preprocessing-token>) ())
(defclass <pp-number>          (<preprocessing-token>) ())
(defclass <character-constant> (<preprocessing-token>) ())
(defclass <string-literal>     (<preprocessing-token>) ())
(defclass <punctator>          (<preprocessing-token>) ())
(defclass <other>              (<preprocessing-token>) ())


;;; Packrat parser for C18 (notes)

;;; Esrap *does* support left recursion. I took old source code when
;;; it didn't so I thought it can't – that lead to a situation where
;;; I've manually transformed left-recurrent definitions. All such
;;; rules start with *.


;;; A.1 Lexical grammar

(esrap:defrule whitespace
    (or #\space
        #\tab
        #\newline
        #.(code-char 11)                ; vertical tab
        #.(code-char 12)                ; form feed
        comment))

(esrap:defrule comment
    (or (and "//" (* (not #\newline)) #\newline)
        (and "/*" (* (not "*/")) "*/")))

;;; A.1.1 Lexical elements
(esrap:defrule token
    (or keyword identifier constant string-literal punctator))

(esrap:defrule preprocessing-token
    (or header-name
        identifier
        pp-number
        character-constant
        string-literal
        punctator
        (not whitespace)))


;;; A.1.2 Keywords

(defparameter *keywords*
  '(|auto|          |extern|        |short|         |while|
    |break|         |float|         |signed|        |_Alignas|
    |case|          |for|           |sizeof|        |_Alignof|
    |char|          |goto|          |static|        |_Atomic|
    |const|         |if|            |struct|        |_Bool|
    |continue|      |inline|        |switch|        |_Complex|
    |default|       |int|           |typedef|       |_Generic|
    |double|        |long|          |union|         |_Imaginary|
    |do|            |register|      |unsigned|      |_Noreturn|
    |else|          |restrict|      |void|          |_Static_assert|
    |enum|          |return|        |volatile|      |_Thread_local|))

(defmacro deftokens (name symbols)
  `(progn
     (esrap:defrule ,name (or ,@(symbol-value symbols)))
     ,@(mapcar (lambda (k)
                 `(esrap:defrule ,k
                      (and (* whitespace)
                           ,(string k)
                           (* whitespace))
                    (:destructure
                     (wch1 keyword wch2)
                     (declare (ignore wch1 wch2))
                     keyword)))
               (symbol-value symbols))))

(deftokens keyword *keywords*)


;;; A.1.3 Identifiers
(esrap:defrule identifier
    (and identifier-nondigit
         (* (or identifier-nondigit digit)))
  (:text t))

(esrap:defrule identifier-nondigit
    (or nondigit universal-character-name))

(esrap:defrule nondigit
    (or #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
        #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
        #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
        #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
        #\_))

(esrap:defrule digit
    (digit-char-p character))


;;; A.1.4 Universal character names
(esrap:defrule universal-character-name
    (or (and #\\ #\u hex-quad)
        (and #\\ #\U hex-quad hex-quad)))

(esrap:defrule hex-quad
    (and hexadecimal-digit
         hexadecimal-digit
         hexadecimal-digit
         hexadecimal-digit))


;;; A.1.5 Constants
(esrap:defrule constant
    (or integer-constant
        floating-constant
        enumeration-constant
        character-constant))

(esrap:defrule integer-constant
    (or (and decimal-constant     (esrap:? integer-suffix))
        (and octal-constant       (esrap:? integer-suffix))
        (and hexadecimal-constant (esrap:? integer-suffix))))

(esrap:defrule decimal-constant
    (and nonzero-digit (* digit)))

(esrap:defrule octal-constant
    (and #\0 (* octal-digit)))

(esrap:defrule hexadecimal-constant
    (and hexadecimal-prefix (+ hexadecimal-digit)))

(esrap:defrule hexadecimal-prefix
    (or "0x" "0X"))

(esrap:defrule nonzero-digit
    (or #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(esrap:defrule octal-digit
    (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))

(esrap:defrule hexadecimal-digit
    (or digit
        #\a #\b #\c #\d #\e #\f
        #\A #\B #\C #\D #\E #\F))

(esrap:defrule integer-suffix
    (or (and unsigned-suffix  (esrap:? long-suffix))
        (and unsigned-suffix  (esrap:? long-long-suffix))
        (and long-suffix      (esrap:? unsigned-suffix))
        (and long-long-suffix (esrap:? unsigned-suffix))))

(esrap:defrule unsigned-suffix
    (or #\u #\U))

(esrap:defrule long-suffix
    (or #\l #\L))

(esrap:defrule long-long-suffix
    (or "ll" "LL"))

(esrap:defrule floating-constant
    (or decimal-floating-constant
        hexadecimal-floating-constant))

(esrap:defrule decimal-floating-constant
    (or (and fractional-constant
             (esrap:? exponent-part)
             (esrap:? floating-suffix))
        (and digit-sequence
             exponent-part
             (esrap:? floating-suffix))))

(esrap:defrule hexadecimal-floating-constant
    (or (and hexadecimal-prefix
             hexadecimal-fractional-constant
             binary-exponent-part
             (esrap:? floating-suffix))
        (and hexadecimal-prefix
             hexadecimal-digit-sequence
             binary-exponent-part
             (esrap:? floating-suffix))))

(esrap:defrule fractional-constant
    (or (and (esrap:? digit-sequence) |.| digit-sequence)
        (and digit-sequence |.|)))

(esrap:defrule exponent-part
    (or (and #\e (esrap:? sign) digit-sequence)
        (and #\E (esrap:? sign) digit-sequence)))

(esrap:defrule sign
    (or |+| |-|))

(esrap:defrule digit-sequence
    (+ digit))

(esrap:defrule hexadecimal-fractional-constant
    (or (and (esrap:? hexadecimal-digit-sequence) |.| hexadecimal-digit-sequence)
        (and hexadecimal-digit-sequence |.|)))

(esrap:defrule binary-exponent-part
    (or (and #\p (esrap:? sign) digit-sequence)
        (and #\P (esrap:? sign) digit-sequence)))

(esrap:defrule hexadecimal-digit-sequence
    (+ hexadecimal-digit))

(esrap:defrule floating-suffix
    (or #\f #\l #\F #\L))

(esrap:defrule enumeration-constant
    identifier)

(esrap:defrule character-constant
    (or (and #\' c-char-sequence #\')
        (and #\L #\' c-char-sequence #\')
        (and #\u #\' c-char-sequence #\')
        (and #\U #\' c-char-sequence #\')))

(esrap:defrule c-char-sequence
    (+ c-char))

(esrap:defrule c-char
    (or (not (or #\' #\\ #\newline))
        escape-sequence))

(esrap:defrule escape-sequence
    (or simple-escape-sequence
        octal-escape-sequence
        hexadecimal-escape-sequence
        universal-character-name))

(esrap:defrule simple-escape-sequence
    (or "\\'" "\\\"" "\\?" "\\\\"
        "\\a" "\\b" "\\f" "\\n" "\\r" "\\t" "\\v"))

(esrap:defrule octal-escape-sequence
    (or (and "\\" octal-digit)
        (and "\\" octal-digit octal-digit)
        (and "\\" octal-digit octal-digit octal-digit)))

(esrap:defrule hexadecimal-escape-sequence
    (and "\\x" (+ hexadecimal-digit)))


;;; A.1.6 String literals
(esrap:defrule string-literal
    (and (esrap:? encoding-prefix) #\" (* s-char-sequence) #\"))

(esrap:defrule encoding-prefix
    (or "u8" "u" "U" "L"))

(esrap:defrule s-char-sequence
    (+ s-char))

(esrap:defrule s-char
    (or (not (or #\" #\\ #\newline))
        escape-sequence))


;;; A.1.7 Punctators
(defparameter *punctators*
  '(|[| |]| |(| |)| |{| |}| |.| |->|
    |++| |--| |&| |*| |+| |-| |~| |!|
    |/| |%| |<<| |>>| |<| |>| |<=| |>=| |==| |!=| |^| |\|| |&&| |\|\||
    |?| |:| |;| |...|
    |=| |*=| |/=| |%=| |+=| |-=| |<<=| |>>=| |&=| |^=| |\|=|
    |,| |#| |##|
    |<:| |:>| |<%| |%>| |%:| |%:%:|))

(deftokens punctator *punctators*)


;;; A.1.8 Header names
(esrap:defrule header-name
    (or (and < h-char-sequence >)
        (and #\" q-char-sequence #\")))

(esrap:defrule h-char-sequence
    (+ h-char))

(esrap:defrule h-char
    (not (or #\newline #\>)))

(esrap:defrule q-char-sequence
    (+ q-char))

(esrap:defrule q-char
    (not (or #\newline #\")))


;;; A.1.9 Preprocessing numbers
(esrap:defrule pp-number
    (or (and digit pp-number*)
        (and |.| digit pp-number*)))

(esrap:defrule pp-number*
    (or (and (or digit
                 identifier-nondigit
                 (and "e" sign)
                 (and "E" sign)
                 (and "p" sign)
                 (and "P" sign)
                 ".")
             pp-number*)
        (esrap:& (esrap:? character))))


;;; A.2


;;; A.2.1 Expression
(esrap:defrule primary-expression
    (or identifier
        constant
        string-literal
        (and |(| expression |)|)
        generic-selection))

(esrap:defrule generic-selection
    (and |_Generic| |(| assignment-expression |,| generic-assoc-list |)|))

(esrap:defrule generic-assoc-list
    (+ generic-association))

(esrap:defrule generic-association
    (or (and type-name |:| assignment-expression)
        (and |default| |:| assignment-expression)))

(esrap:defrule postfix-expression
    (or (and primary-expression postfix-expression*)
        (and |(| type-name |)| { initializer-list (* |,|) } postfix-expression*)))

(esrap:defrule postfix-expression*
    (or (and [ expression ] postfix-expression*)
        (and |(| (* argument-expression-list) |)| postfix-expression*)
        (and |.| identifier postfix-expression*)
        (and |->| identifier postfix-expression*)
        (and |++| postfix-expression*)
        (and |-| postfix-expression*)
        (esrap:& (esrap:? character))))

(esrap:defrule argument-expression-list
    (and assignment-expression (* (and |,| argument-expression))))

(esrap:defrule unary-expression
    (or postfix-expression
        (and |++| unary-expression)
        (and |-| unary-expression)
        (and unary-operator cast-expression)
        (and |sizeof| unary-expression)
        (and |sizeof| |(| type-name |)|)
        (and |_Alignof| |(| type-name |)|)))

(defvar *unary-tokens* '(|&| |*| |+| |-| |~| |!|))
(deftokens unary-operators *unary-tokens*)

(esrap:defrule cast-expression
    (or unary-expression
        (and |(| type-name |)| cast-expression)))

(esrap:defrule multiplicative-expression
    (and cast-expression multiplicative-expression*))

(esrap:defrule multiplicative-expression*
    (or (and (or (and |*| cast-expression)
                 (and |/| cast-expression)
                 (and |%| cast-expression))
             multiplicative-expression*)
        (esrap:& (esrap:? character))))

(esrap:defrule additive-expression
    (and multiplicative-expression additive-expression*))

(esrap:defrule additive-expression*
    (or (and (or (and |+| multiplicative-expression)
                 (and |-| multiplicative-expression))
             additive-expression*)
        (esrap:& (esrap:? character))))

(esrap:defrule shift-expression
    (and additive-expression shift-expression*))

(esrap:defrule shift-expression*
    (or (and (or (and |<<| additive-expression)
                 (and |>>| additive-expression))
             shift-expression*)
        (esrap:& (esrap:? character))))

(esrap:defrule relational-expression
    (and shift-expression relational-expression*))

(esrap:defrule relational-expression*
    (or (and (or (and |<| shift-expression)
                 (and |>| shift-expression)
                 (and |<=| shift-expression)
                 (and |>=| shift-expression))
             relational-expression*)
        (esrap:& (esrap:? character))))

(esrap:defrule equality-expression
    (and relational-expression equality-expression*))

(esrap:defrule equality-expression*
    (or (and (or (and |==| relational-expression)
                 (and |!=| relational-expression))
             equality-expression*)
        (esrap:& (esrap:? character))))

(esrap:defrule and-expression
    (and equality-expression and-expression*))

(esrap:defrule and-expression*
    (or (and |&| equality-expression and-expression*)
        (esrap:& (esrap:? character))))

(esrap:defrule xor-expression
    (and and-expression xor-expression*))

(esrap:defrule xor-expression*
    (or (and |^| and-expression xor-expression*)
        (esrap:& (esrap:? character))))

(esrap:defrule or-expression
    (and xor-expression or-expression*))

(esrap:defrule or-expression*
    (or (and |\|| xor-expression or-expression*)
        (esrap:& (esrap:? character))))

(esrap:defrule logical-and-expression
    (and or-expression logical-and-expression*))

(esrap:defrule logical-and-expression*
    (or (and |&&| or-expression logical-and-expression*)
        (esrap:& (esrap:? character))))

(esrap:defrule logical-or-expression
    (and logical-and-expression logical-or-expression*))

(esrap:defrule logical-or-expression*
    (or (and |\|\|| logical-and-expression logical-or-expression*)
        (esrap:& (esrap:? character))))

(esrap:defrule conditional-expression
    (or logical-or-expression
        (and logical-or-expression |?| expression |:| conditional-expression)))

(esrap:defrule assignment-expression
    (or conditional-expression
        (and unary-expression assignment-operator assignment-expression)))

(defvar *assignments* '(|=| |*=| |/=| |%=| |+=| |-=| |<<=| |>>=| |&=| |^=| |\|=|))
(deftokens assignment-operator *assignments*)

(esrap:defrule expression
    (and assignment-expression expression*))

(esrap:defrule expression*
    (or (and |,| assignment-expression expression*)
        (esrap:& (esrap:? character))))

(esrap:defrule constant-expression
    conditional-expression)


;;; A.2.2 Declarations
(esrap:defrule declaration
    (or (and declaration-specifiers (esrap:? init-declarator-list) |;|)
        static_assert-declaration))

(esrap:defrule declaration-specifiers
    (and (or storage-class-specifier
             type-specifier
             type-qualifier
             function-specifier
             alignment-specifier)
         (esrap:? declaration-specifiers)))

(esrap:defrule init-declarator-list
    (and init-declarator init-declarator-list*))

(esrap:defrule init-declarator-list*
    (and |,| init-declarator init-declarator-list*))

(esrap:defrule init-declarator
    (or declarator
        (and declarator |=| initializer)))

(esrap:defrule storage-class-specifier
    (or |typedef|
        |extern|
        |static|
        |_Thread_local|
        |auto|
        |register|))

(esrap:defrule type-specifier
    (or |void|
        |char|
        |short|
        |int|
        |long|
        |float|
        |double|
        |signed|
        |unsigned|
        |_Bool|
        |_Complex|
        atomic-type-specifier
        struct-or-union-specifier
        enum-specifier
        typedef-name))

(esrap:defrule struct-or-union-specifier
    (or (and struct-or-union (esrap:? identifier) { struct-declaration-list })
        (and struct-or-union identifier)))

(esrap:defrule struct-or-union
    (or |struct| |union|))

(esrap:defrule struct-declaration-list
    (+ struct-declaration))

(esrap:defrule struct-declaration
    (or (and specifier-qualifier-list (esrap:? struct-declarator-list) |;|)
        static_assert-declaration))

(esrap:defrule specifier-qualifier-list
    (and (or type-specifier
             type-qualifier
             alignment-specifier)
         (esrap:? specifier-qualifier-list)))

(esrap:defrule struct-declarator-list
    (+ struct-declarator))

(esrap:defrule struct-declarator
 (or declarator
     (and (esrap:? declarator) |:| constant-expression)))

(esrap:defrule enum-specifier
    (or (and |enum| (esrap:? identifier) { enumerator-list })
        (and |enum| (esrap:? identifier) { enumerator-list |,| })
        (and |enum| identifier)))

(esrap:defrule enumerator-list
    (+ enumerator))

(esrap:defrule enumeratior
    (or enumeration-constant
        (and enumeration-constant |=| constant-expression)))

(esrap:defrule atomic-type-specifier
    (and |_Atomic| |(| type-name |)|))

(esrap:defrule type-qualifier
    (or |const| |restrict| |volatile| |_Atomic|))

(esrap:defrule function-specifier
    (or |inline| |_Noreturn|))

(esrap:defrule alignment-specifier
    (or (and |_Alignas| |(| type-name |)|)
        (and |_Alignas| |(| constant-expression |)|)))

(esrap:defrule declarator
    (and (esrap:? pointer) (or direct-declarator ;indirect LR
                               direct-declarator*)))

(esrap:defrule direct-declarator
    (or (and identifier direct-declarator*)
        (and |(| declarator |)| direct-declarator*)))

(esrap:defrule direct-declarator*
    (or (and (or (and [ (esrap:? type-qualifier-list) (esrap:? assignment-expression) ])
                 (and [ |static| (esrap:? type-qualifier-list) assignment-expression ])
                 (and [ type-qualifier-list |static| assignment-expression ])
                 (and [ (esrap:? type-qualifier-list) |*|)
                 (and |(| parameter-type-list |)|)
                 (and |(| (esrap:? identifier-list) |)| ))
             direct-declarator*)
        (esrap:& (esrap:? character))))

(esrap:defrule pointer
    (or (and |*| (esrap:? type-qualifier-list))
        (and |*| (esrap:? type-qualifier-list) pointer)))

(esrap:defrule type-qualifier-list
    (+ type-qualifier))

(esrap:defrule parameter-type-list
    (or parameter-list
        (and parameter-list |,| |...|)))

(esrap:defrule parameter-list
    (and parameter-declaration parameter-list*))

(esrap:defrule parameter-list*
    (or (and |,| parameter-declaration parameter-list*)
        (esrap:& (esrap:? character))))

(esrap:defrule parameter-declaration
    (or (and declaration-specifiers declarator)
        (and declaration-specifiers (esrap:? abstract-declarator))))

(esrap:defrule identifier-list
    (and identifier identifier-list*))

(esrap:defrule identifier-list*
    (or (and |,| identifier identifier-list*)
        (esrap:& (esrap:? character))))

(esrap:defrule type-name
    (and specifier-qualifier-list (esrap:? abstract-declarator)))

(esrap:defrule abstract-declarator
    (or pointer
        (and (esrap:? pointer) direct-abstract-declarator)))

(esrap:defrule direct-abstract-declarator
    (or (and |(| abstract-declarator |)|)
        (and (esrap:? direct-abstract-declarator)
             (or (and [ (esrap:? type-qualifier-list) (esrap:? assignment-expression) ])
                 (and [ |static| (esrap:? type-qualifier-list) assignment-expression ])
                 (and [ type-qualifier-list |static| assignment-expression ])
                 (and [ |*| ])
                 (and |(| (esrap:? parameter-type-list) |)|)))))

(esrap:defrule typedef-name
    identifier)

(esrap:defrule initializer
    (or assignment-expression
        (and { initializer-list })
        (and { initializer-list |,| })))

(esrap:defrule initializer-list
    (and (esrap:? designation) initializer initializer-list*))

(esrap:defrule initializer-list*
    (or (and |,| (esrap:? designation) initializer initializer-list*)
        (esrap:& (esrap:? character))))

(esrap:defrule designation
    (and designator-list |=|))

(esrap:defrule designator-list
    (+ designator))

(esrap:defrule designator
    (or (and [ constant-expression ])
        (and |.| identifier)))

(esrap:defrule static_assert-declaration
    (and |_Static_assert| |(| constatn-expression |,| string-listeral |)| |;|))


;;; A.2.3 Statements
(esrap:defrule statement
    (or labeled-statement
        compound-statement
        expression-statement
        selection-statement
        iteration-statement
        jump-statement))

(esrap:defrule labeled-statement
    (or (and identifier |:| statement)
        (and |case| constant-expression |:| statement)
        (and |default| |:| statement)))

(esrap:defrule compound-statement
    (and { (esrap:? block-item-list) }))

(esrap:defrule block-item-list
    (+ block-item))

(esrap:defrule block-item
    (or declaration
        statement))

(esrap:defrule expression-statement
    (and (esrap:? expression) |;|))

(esrap:defrule selection-statement
    (or (and |if| |(| expression |)| statement)
        (and |if| |(| expression |)| statement |else| statement)
        (and |switch| |(| expression |)| statement)))

(esrap:defrule iteration-statement
    (or (and |while| |(| expression |)| statement)
        (and |do| statement |while| |(| expression |)| |;|)
        (and |for| |(| (esrap:? expression) |;| (esrap:? expression) |;| (esrap:? expression) |)| statement)
        (and |for| |(| declaration    |;| (esrap:? expression) |;| (esrap:? expression) |)| statement)))

(esrap:defrule jump-statement
    (or (and |goto| identifier |;|)
        (and |continue| |;|)
        (and |break| |;|)
        (and |return| (esrap:? expression) |;|)))


;;; A.2.4 External definitions
(esrap:defrule translation-unit
    (or external-declaration
        (and translation-unit external-declaration)))

(esrap:defrule translation-unit*
    (or (and external-declaration translation-unit*)
        (esrap:& (esrap:? character))))

(esrap:defrule external-declaration
    (or function-definition
        declaration))

(esrap:defrule function-definition
    (and declaration-specifiers declarator (esrap:? declaration-list) compound-statement))

(esrap:defrule declaration-list
    (+ declaration))


;;; A.3
(esrap:defrule preprocessing-file
    (esrap:? group))

(esrap:defrule group
    (+ group-part))

(esrap:defrule group-part
    (or if-section
        control-line
        text-line
        (and |#| non-directive)))

(esrap:defrule if-section
    (and if-group (esrap:? elif-groups) (esrap:? else-group) endif-line))

(esrap:defrule if-group
    (or (and |#| |if| constant-expression new-line (esrap:? group))
        (and |#| |ifdef| identifier new-line (esrap:? group))
        (and |#| |ifndef| identifier new-line (esrap:? group))))

(esrap:defrule elif-groups
    (+ elif-group))

(esrap:defrule elif-group
    (and |#| |elif| constant-expression new-line (esrap:? group)))

(esrap:defrule elif-group
    (and |#| |else| new-line (esrap:? group)))

(esrap:defrule endif
    (and |#| |endif| new-line))

(esrap:defrule control-line
    (or (and |#| include pp-tokens new-line)
        (and |#| define identifier replacement-list new-line)
        (and |#| define lparen (esrap:? identifier-list) |)| replacement-list new-line)
        (and |#| define lparen |...| |)| replacement-list new-line)
        (and |#| define lparen identifier-list |,| |...| |)| replacement-list new-line)
        (and |#| undef identifier new-line)
        (and |#| line pp-tokens new-line)
        (and |#| error (esrap:? pp-tokens) new-line)
        (and |#| pragma (esrap:? pp-tokens) new-line)
        (and |#| new-line)))

(esrap:defrule text-line
    (and (esrap:? pp-tokens) (not #\\) new-line))

(esrap:defrule non-directive
    (and pp-tokens new-line))

;;; not right?
(esrap:defrule lparen
    |(|)

(esrap:defrule replacement-list
    (esrap:? pp-tokens))

(esrap:defrule pp-tokens
    (+ (and (* whitespace) preprocessing-token (* whitespace))))

(esrap:defrule new-line
    #\newline)

(esrap:defrule on-off-switch
    (or |ON| |OFF| |DEFAULT|))
