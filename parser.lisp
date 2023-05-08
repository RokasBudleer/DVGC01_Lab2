;;=====================================================================
;; LISP READER & LEXER - new version 161202
;;=====================================================================

;;=====================================================================
;; Help functions
;;=====================================================================
;; ctos         convert a character to a string
;; str_con      concatenate 2 strings str, c
;; whitespace   is c whitespace?
;;=====================================================================

(defun ctos (c)        (make-string 1 :initial-element c))
(defun str-con (str c) (concatenate 'string str (ctos c)))
(defun whitespace (c)  (member c '(#\Space #\Tab #\Newline)))

;;=====================================================================
;; get-wspace   remove whitespace
;;=====================================================================

(defun get-wspace (ip)
   (setf c (read-char ip nil 'EOF))
   (cond   
           ((whitespace c)  (get-wspace ip))
           (t                             c)
   )
)

;;=====================================================================
;; Read an Identifier         Compare this with C's do-while construct
;;=====================================================================

(defun get-name (ip lexeme c)
   (setf lexeme (str-con lexeme c))
   (setf c      (read-char ip nil 'EOF))
   (cond        
                ((alphanumericp c)  (get-name ip lexeme c))
                (t                  (list        c lexeme))
   )
)

;;=====================================================================
;; Read a Number              Compare this with C's do-while construct
;;=====================================================================

(defun get-number (ip lexeme c)
   (setf lexeme (str-con lexeme c))
   (setf c      (read-char ip nil 'EOF))
   (cond
         ((not (null (digit-char-p c)))  (get-number ip lexeme c))
         (t                              (list          c lexeme))
   )
  )

;;=====================================================================
;; Read a single character or ":="
;;=====================================================================

(defun get-symbol (ip lexeme c)
   (setf lexeme (str-con lexeme c))
   (setf c1 c)
   (setf c (read-char ip nil 'EOF))
   (cond
         ((and (char= c1 #\:) (char= c #\=))  (get-symbol ip lexeme c))
         (t                                   (list          c lexeme))
   )
)

;;=====================================================================
;; Read a Lexeme                       lexeme is an "accumulator"
;;                                     Compare this with the C version
;;=====================================================================

(defun get-lex (state)
   (setf lexeme "")
   (setf ip (pstate-stream   state))
   (setf c  (pstate-nextchar state))
   (if (whitespace c) (setf c (get-wspace ip)))
   (cond
         ((eq c 'EOF)                     (list 'EOF ""))
         ((alpha-char-p c)                (get-name   ip lexeme c))
         ((not (null (digit-char-p c)))   (get-number ip lexeme c))
         (t                               (get-symbol ip lexeme c))
   )
)

;;=====================================================================
; map-lexeme(lexeme) returns a list: (token, lexeme)
;;=====================================================================

(defun map-lexeme (lexeme)
(format t "~%Symbol: ~S " lexeme)
   (list (cond
    ;; keywordtab ===================================
        ((string=  lexeme "program")   'PROGRAM    )
        ((string=  lexeme "input"  )   'INPUT      )
        ((string=  lexeme "output" )   'OUTPUT     )
        ((string=  lexeme "var"    )   'VAR        )
        ((string=  lexeme "begin"  )   'BEGIN      )
        ((string=  lexeme "end"    )   'END        )
        ((string=  lexeme "boolean")   'BOOLEAN    )
        ((string=  lexeme "integer")   'INTEGER    )
        ((string=  lexeme "real"   )   'REAL       )
    ;; tokentab =====================================
        ((string=  lexeme ":=")        'ASSIGN     )
        ((string=  lexeme ")")         'RP         )
        ((string=  lexeme "(")         'LP         )
        ((string=  lexeme "*")         'ASTERIX    )
        ((string=  lexeme "+")         'PLUS       )
        ((string=  lexeme ",")         'COMMA      )
        ((string=  lexeme "-")         'MINUS      )
        ((string=  lexeme ".")         'PUNCT      )
        ((string=  lexeme "/")         'FWDSLASH   )
        ((string=  lexeme ":")         'COLON      )
        ((string=  lexeme ";")         'SEMICOLON  )
        ((string=  lexeme "=")         'EQUAL      )
        ((string=  lexeme "" )	        'EOF        )
        ((is-id    lexeme    )         'ID         )
        ((is-number lexeme   )         'NUM        )
        (t                             'UNKNOWN )
         )
    lexeme)
)

;;=====================================================================
; ID is [A-Z,a-z][A-Z,a-z,0-9]*          number is [0-9][0-9]*
;;=====================================================================

(defun is-id (str)
    (and (alpha-char-p (char str 0)) (every #'alphanumericp str))
)


(defun is-number (str)
    (every #'digit-char-p str)
)

;;=====================================================================
; THIS IS THE PARSER PART
;;=====================================================================

;;=====================================================================
; Create a stucture - parse state descriptor
;;=====================================================================
; lookahead is the list (token, lexeme)
; stream    is the input filestream
; nextchar  is the char following the last lexeme read
; status    is the parse status (OK, NOTOK)
; symtab    is the symbol table
;;=====================================================================

(defstruct pstate
    (lookahead)
    (stream)
    (nextchar)
    (status)
    (symtab)
)

;;=====================================================================
; Constructor for the structure pstate - initialise
; stream to the input file stream (ip)
;;=====================================================================

(defun create-parser-state (ip)
   (make-pstate
      :stream        ip
      :lookahead     ()
      :nextchar      #\Space
      :status        'OK
      :symtab        ()
    )
)

;;=====================================================================
; SYMBOL TABLE MANIPULATION
;;=====================================================================

;;=====================================================================
; token  - returns the token  from (token lexeme)(reader)
; lexeme - returns the lexeme from (token lexeme)(reader)
;;=====================================================================

(defun token  (state)
    (first    (pstate-lookahead state))
)
(defun lexeme (state)
    (second   (pstate-lookahead state))
)

;;=====================================================================
; symbol table manipulation: add + lookup + display
;;=====================================================================

(defun symtab-add (state id)
    (if (null (pstate-symtab state))
        (setf (pstate-symtab state) (list id))
        (setf (pstate-symtab state) (append (pstate-symtab state) (list id)))
    )
)

(defun symtab-member (state id)
    (member id (pstate-symtab state) :test #'equal)
)
;; &rest - allows variable length parameter lists
(defun symtab-display (state)
   (format t "~%------------------------------------------------------")
   (format t "~%Symbol Table is: ~S " (pstate-symtab state))
   (format t "~%------------------------------------------------------")
)

;;=====================================================================
; Error functions: Syntax & Semantic
;;=====================================================================

(defun synerr1 (state symbol)
    (format t "~%*** Syntax error:   Expected ~8S found ~8S "
           symbol (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun synerr2 (state)
    (format t "~%*** Syntax error:   Expected TYPE     found ~S "
           (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun synerr3 (state)
    (format t "~%*** Syntax error:   Expected OPERAND  found ~S "
           (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun semerr1 (state)
    (format t "~%*** Semantic error: ~S already declared."
                (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun semerr2 (state)
    (format t "~%*** Semantic error: ~S not declared."
          (lexeme state))
    (setf (pstate-status state) 'NOTOK)
)

(defun semerr3 (state)
    (format t "~%*** Semantic error: found ~8S expected EOF."
          (lexeme state))
    (setf (pstate-status state) 'NOTOK)
    ;; *** TO BE DONE - completed! ***
)

;;=====================================================================
; The return value from get-token is always a list. (token lexeme)
;;=====================================================================

(defun get-token (state)
  (let    ((result (get-lex state)))
    (setf (pstate-nextchar  state) (first result))
    (setf (pstate-lookahead state) (map-lexeme (second result)))
  )
 )

;;=====================================================================
; match compares lookahead with symbol (the expected token)
; if symbol == lookahead token ==> get next token else Syntax error
;;=====================================================================

(defun match (state symbol)
   (if (eq symbol (token state))
       (get-token  state)
       (synerr1    state symbol)
       )
)

;;=====================================================================
; THE GRAMMAR RULES
;;=====================================================================

;;=====================================================================
; <stat-part>     --> begin <stat-list> end .
; <stat-list>     --> <stat> | <stat> ; <stat-list>
; <stat>          --> <assign-stat>
; <assign-stat>   --> id := <expr>
; <expr>          --> <term>     | <term> + <expr>
; <term>          --> <factor>   | <factor> * <term>
; <factor>        --> ( <expr> ) | <operand>
; <operand>       --> id | number
;;=====================================================================

(defun operand (state)
    ;;(format t "~% *** In operand")
    (cond 
        ((EQ (token state) 'ID) 
            (progn 
                (if (not(symtab-member state (lexeme state)))
                    (semerr2 state)
                )
                (match state 'ID)
            )
        )
        ((EQ (token state) 'NUM)
            (match state 'NUM)
        )
        (t (synerr3 state))
    )   
)

(defun factor (state)
    ;;(format t "~% *** In factor")   
    (if (EQ (lexeme state) 'LP)
        (progn
            (match state 'LP)
            (expr state)
            (match state 'RP))
        (operand state)
    )
)

(defun term (state)
    ;;(format t "~% *** In term")
    (factor state)
    (if (EQ (token state) 'ASTERIX)
        (progn
            (match state 'ASTERIX)
            (term state))
    )
)

(defun expr (state)
    ;;(format t "~% *** In expr")
    (term state)
    (if (EQ (token state) 'PLUS)
        (progn
            (match state 'PLUS)
            (expr state))
    )
)

(defun assign_stat (state)
    ;;(format t "~% *** In assign_stat")
    (if (EQ (lexeme state) 'ID)
        (if (not(symtab-member state (lexeme state)))
            (semerr2 state)
        )
    )
    (match state 'ID)
    (match state 'ASSIGN)
    (expr state)
)
(defun stat (state)
    ;;(format t "~% *** In stat")
    (assign_stat state)
)

(defun stat-list (state)
    ;;(format t "~% *** In stat-list")
    (stat state)
    (if (EQ (token state) 'SEMICOLON)
        (progn 
            (match state 'SEMICOLON)
            (stat-list state)
        )
    )
)

(defun stat-part (state)
     ;;(format t "~% *** In stat-part")
     (match state 'BEGIN)
     (stat-list state)
     (match state 'END)
     (match state 'PUNCT)
)

;;=====================================================================
; <var-part>     --> var <var-dec-list>
; <var-dec-list> --> <var-dec> | <var-dec><var-dec-list>
; <var-dec>      --> <id-list> : <type> ;
; <id-list>      --> id | id , <id-list>
; <type>         --> integer | real | boolean
;;=====================================================================

(defun type (state)
    ;;(format t "~% *** In type")
    (cond 
        ((EQ (token state) 'INTEGER)    (match state 'INTEGER))
        ((EQ (token state) 'REAL)       (match state 'REAL))
        ((EQ (token state) 'BOOLEAN)    (match state 'BOOLEAN))
        (t                              (synerr2 state))  
    )
)

(defun id-list (state)
    ;;(format t "~% *** In id-list")
    (if (EQ (token state) 'ID)
        (progn 
            (if (symtab-member state (lexeme state))
                (semerr1 state)
                (symtab-add state (lexeme state))
            )
        )
    )
    (match state 'ID)
    (if (EQ (token state) 'COMMA)
        (progn
            (match state 'COMMA)
            (id-list state)
        )
    )
)

(defun var-dec (state)
    ;;(format t "~% *** In var-dec")
    (id-list state)
    (match state 'COLON)
    (type state)
    (match state 'SEMICOLON)
)

(defun var-dec-list (state)
    ;;(format t "~% *** In var-dec-list")
    (var-dec state)
    (if (EQ(token state) 'ID)
        (var-dec-list state))
)

(defun var-part (state)
    ;;(format t "~% *** In var-part")
    (match state 'VAR)
    (var-dec-list state)
)

;;=====================================================================
; <program-header>
;;=====================================================================

(defun program-header (state)
    ;;(format t "~% *** In program-header")
    (match state 'PROGRAM)
    
    ;;(if (EQ(token state) 'ID) 
    ;;    (symtab-add state (lexeme state))
    ;;    (symtab-add state "???"))
    
    (match state 'ID)
    (match state 'LP) 
    (match state 'INPUT) 
    (match state 'COMMA) 
    (match state 'OUTPUT) 
    (match state 'RP) 
    (match state 'SEMICOLON)
)

;;=====================================================================
; <program> --> <program-header><var-part><stat-part>
;;=====================================================================
(defun program (state)
   (program-header state)
   (var-part       state)
   (stat-part      state)
)

;;=====================================================================
; THE PARSER - parse a file
;;=====================================================================

(defun check-end (state)
    (if (not(EQ (token state)   'EOF))
        (semerr3 state)
    )
)

;;=====================================================================
; Test parser for file name input
;;=====================================================================

(defun parse (filename)
   (format t "~%------------------------------------------------------")
   (format t "~%--- Parsing program: ~S " filename)
   (format t "~%------------------------------------------------------")
   (with-open-file (ip (open filename) :direction :input)
      (setf state (create-parser-state ip))
      (setf (pstate-nextchar state) (read-char ip nil 'EOF))
      (get-token      state)
      (program        state)
      (check-end      state)
      (symtab-display state)
      )
   (if (eq (pstate-status state) 'OK)
      (format t "~%Parse Successful. ")
      (format t "~%Parse Fail. ")
      )
   (format t "~%------------------------------------------------------")
)

;;=====================================================================
; THE PARSER - parse all the test files
;;=====================================================================

(defun parse-all ()
    (mapcar (lambda (concatenate (string) "/")))
    (mapcar #'parse '("testfiles/sem1.pas" "testfiles/sem2.pas"))
)

;;=====================================================================
; THE PARSER - test all files
;;=====================================================================

;; (parse-all)
;; (mapcar #'parser '("testa.pas" "testb.pas" ... "testz.pas"))

;;=====================================================================
; THE PARSER - test a single file
;;=====================================================================

;;(parse-all)
(parse "testfiles/s.pas")

;;=====================================================================
; THE PARSER - end of code
;;=====================================================================
