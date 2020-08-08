#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)



;utils
(define neg-list
  (lambda(x)
    (cond
      [(number? x) (- x)]
      [(boolean? x) (not x)]
      [(list? x) (cond [(null? x) (list)]
                       [else (cons (neg-list (car x)) (neg-list (cdr x)))])])
    ))

(define find-element
  (lambda (mylist index)
    (cond
      [(null? mylist) '()]
      [(eq? index 0) (car mylist)]
      [else (find-element (cdr mylist) (- index 1))])))


(define find-memval-in-list
  (lambda (mylist member)
    (cond 
      [(null? member) mylist]
      [else (find-memval-in-list (find-element mylist (car member)) (cdr member))])))


(define lt-list-number?
  (lambda (mylist num)
        (cond
           [(number? mylist) (< mylist num)]
           [(and (eq? (length mylist) 1) (number? (car mylist))) (< (car mylist) num)]
           [else (and (lt-list-number? (car mylist) num) (lt-list-number? (cdr mylist) num))]
           )))

(define gt-list-number?
  (lambda (mylist num)
        (cond
           [(number? mylist) (> mylist num)]
           [(and (eq? (length mylist) 1) (number? (car mylist))) (> (car mylist) num)]
           [else (and (gt-list-number? (car mylist) num) (gt-list-number? (cdr mylist) num))]
           )))

(define lt-list-string?
  (lambda (mylist mystring)
        (cond
           [(string? mylist) (string<? mylist mystring)]
           [(and (eq? (length mylist) 1) (string? (car mylist))) (string<? (car mylist) mystring)]
           [else (and (lt-list-string? (car mylist) mystring) (lt-list-string? (cdr mylist) mystring))]
           )))

(define gt-list-string?
  (lambda (mylist mystring)
        (cond
           [(string? mylist) (string>? mylist mystring)]
           [(and (eq? (length mylist) 1) (string? (car mylist))) (string>? (car mylist) mystring)]
           [else (and (gt-list-string? (car mylist) mystring) (gt-list-string? (cdr mylist) mystring))]
           )))

(define eq-list-element?
  (lambda (mylist element)
        (cond
           [(list? mylist) (equal? mylist element)]
           [(and (eq? (length mylist) 1) (not (list? (car mylist)))) (equal? (car mylist) element)]
           [else (and (eq-list-element? (car mylist) element) (eq-list-element? (cdr mylist) element))]
           )))

(define apply-arithop-list1
  (lambda (mylist num op)
    (cond
      [(number? mylist) (op mylist num)]
      [(null? mylist) (list)]
      [else (cons (apply-arithop-list1 (car mylist) num op) (apply-arithop-list1 (cdr mylist) num op))]
      )))

(define apply-arithop-list2
  (lambda (num mylist op)
    (cond
      [(number? mylist) (op num mylist)]
      [(null? mylist) (list)]
      [else (cons (apply-arithop-list2 num (car mylist) op) (apply-arithop-list2 num (cdr mylist) op))]
      )))


(define reduce-or
  (lambda (mylist mybool)
    (cond
      [(null? mylist) '()]
      [else (cons (or (car mylist) mybool) (reduce-or (cdr mylist) mybool))]
      )
    ))

(define reduce-and
  (lambda (mylist mybool)
    (cond
      [(null? mylist) '()]
      [else (cons (and (car mylist) mybool) (reduce-and (cdr mylist) mybool))]
      )
    ))

(define zip (lambda (l1 l2) (map list l1 l2)))

(define list-string-append
    (lambda (mylist mystring)
        (cond
           [(string? mylist) (string-append mylist mystring)]
           [(and (eq? (length mylist) 1) (string? (car mylist))) (string-append (car mylist) mystring)]
           [else (list (list-string-append (car mylist) mystring) (list-string-append (cdr mylist) mystring))]
           )))


(define string-list-append
    (lambda (mystring mylist)
        (cond
           [(string? mylist) (string-append mystring mylist)]
           [(and (eq? (length mylist) 1) (string? (car mylist))) (string-append mystring (car mylist))]
           [else (list (string-list-append mystring (car mylist)) (string-list-append mystring (cdr mylist)))]
           )))


(define apply-minus-list-num
  (lambda (mylist mynum)
    (cond
      [(null? mylist) '()]
      [else (cons (- (car mylist) mynum) (apply-minus-list-num (cdr mylist) mynum))]
     )))

    
(define PL-lexer
           (lexer

             ;number
             ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-posNUM (string->number lexeme)))

             ;string
             ((:: #\" (:* (:- any-char #\")) #\") (token-string (substring lexeme 1 (- (string-length lexeme) 1))))

             
             ;variable: uppercase and lowercase characters
             ((:& (complement "while") (complement "return") (complement "false")
                  (complement "true") (complement "do") (complement "end")
                  (complement "null") (complement "if") (complement "else")
                  (complement "then") (complement "endif") (complement "func")
                  (:+ (:& (char-range #\A #\z) (complement "[") (complement "]")))) (token-variable (string->symbol lexeme)))
             
             ("while" (token-while))
             ("return" (token-return))
             ("do" (token-do))
             ("end" (token-end))
             ("null" (token-null))
             ("if" (token-if))
             ("else" (token-else))
             ("then" (token-then))
             ("endif" (token-endif))
             ("true" (token-true))
             ("false" (token-false))
             ("func" (token-func))
    
             
             ("+" (token-plus))
             ("*" (token-mul))
             ("=" (token-eq))
             ("==" (token-condeq))
             ("!=" (token-ne))
             ("/" (token-div))
             ("-" (token-minus))
             ("<" (token-lt))
             (">" (token-gt))
             (";" (token-semicol))
             ("[" (token-obrac))
             ("]" (token-cbrac))
             ("(" (token-opar))
             (")" (token-cpar))
             ("{" (token-cobrac))
             ("}" (token-ccbrac))
             ("," (token-comma))
             
             (whitespace (PL-lexer input-port))
             ((eof) (token-EOF))))

(define-tokens a (posNUM variable string))
(define-empty-tokens b (EOF plus mul eq condeq ne div minus lt gt return while if endif do
                            end null else then obrac cbrac opar cpar semicol comma true false func cobrac ccbrac))



(define PL-parser
           (parser
             (start command)
             (end EOF)
             (error void)
             (tokens a b)
             (grammar
               (command ((command semicol unitcom) (list 'com-unitcom-com $1 $3)) ((unitcom) (list 'unitcom-com $1)))
               (unitcom ((whilecom)(list 'whilecom-unitcom $1)) ((ifcom)(list 'ifcom-unitcom $1)) ((assign)(list 'assign-unitcom $1)) ((ret)(list 'return-unitcom $1)))
               (whilecom ((while exp do command end)(list 'while-whilecom $2 $4)))
               (ifcom ((if exp then command else command endif)(list 'if-ifcom $2 $4 $6)))
               (assign ((variable eq exp)(list 'variable-assign-exp $1 $3)) ((variable eq function) (list 'variable-assign-function $1 $3)) ((variable eq call) (list 'variable-assign-call $1 $3)))
               (ret ((return exp)(list 'return-return $2)))
               (exp ((aexp)(list 'aexp-exp $1)) ((aexp gt aexp)(list 'gt-exp $1 $3)) ((aexp lt aexp)(list 'lt-exp $1 $3)) ((aexp condeq aexp)(list 'condeq-exp $1 $3))
                    ((aexp ne aexp)(list 'ne-exp $1 $3)))
               (aexp ((bexp)(list 'bexp-aexp $1)) ((bexp minus aexp)(list 'minus-aexp $1 $3)) ((bexp plus aexp)(list 'plus-aexp $1 $3)))
               (bexp ((cexp)(list 'cexp-bexp $1)) ((cexp mul bexp)(list 'mul-bexp $1 $3)) ((cexp div bexp)(list 'div-bexp $1 $3)))
               (cexp ((minus cexp)(list 'minus-cexp $2)) ((opar exp cpar)(list 'par-cexp $2)) ((posNUM)(list 'posnum $1))
                     ((null)(list 'null-cexp)) ((variable)(list 'var-cexp $1)) ((true)(list 'true-cexp)) ((false)(list 'false-cexp))
                     ((string)(list 'string-cexp $1)) ((list)(list 'list-cexp $1)) ((variable listmem)(list 'var-listmem-cexp $1 $2)))
               (list ((obrac listValues cbrac)(list 'listValues-list $2)) ((obrac cbrac)(list 'obrac-list)))
               (listValues ((exp)(list 'exp-listValues $1)) ((exp comma listValues)(list 'exp-comma-listValues $1 $3)))
               (listmem ((obrac exp cbrac)(list 'exp-listmem $2)) ((obrac exp cbrac listmem)(list 'exp-listmem-listmem $2 $4)))
               (function ((func opar vars cpar cobrac command ccbrac) (list 'define-function $3 $6)))
               (vars ((variable) (list 'function-vars $1)) ((variable comma vars) (list 'function-variable-comma-vars $1 $3)))
               (call ((variable opar args cpar) (list 'perform-call $1 $3)))
               (args ((exp) (list 'call-args $1)) ((exp comma args) (list 'call-exp-comma-args $1 $3)))
             )))


(define empty-env
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

(define extend-env-multi
  (lambda (vars vals env)
    (begin
    (if (not (list? vars)) (extend-env vars vals env)
    (if (null? vars) env (extend-env (car vars) (car vals) (extend-env-multi (cdr vars) (cdr vals) env)))))))

(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env) (null))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var) (if (procedure? saved-val) (saved-val) saved-val)
             (apply-env saved-env search-var))))
      (else null))))

(define find-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env) (null))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var) saved-env
             (find-env saved-env search-var))))
      (else null))))



(define interpreter
  (lambda (input myenv)
    (cond
      [(null? input) null]
      [else
       (let ((state (car input)))
          (cond
            ;command
            [(eq? state 'com-unitcom-com) (cond
                                            [(eq? (cadr (interpreter (cadr input) myenv)) 'null) (interpreter (caddr input) (car (interpreter (cadr input) myenv)))]
                                            [else (interpreter (cadr input) myenv)])]
            [(eq? state 'unitcom-com) (interpreter (cadr input) myenv)]
            ;unitcom
            [(eq? state 'whilecom-unitcom) (interpreter (cadr input) myenv)]
            [(eq? state 'ifcom-unitcom) (interpreter (cadr input) myenv)]
            [(eq? state 'assign-unitcom) (interpreter (cadr input) myenv)]
            [(eq? state 'return-unitcom) (interpreter (cadr input) myenv)]
            ;whilecom
            [(eq? state 'while-whilecom) (cond
                                           [(interpreter (cadr input) myenv) (cond
                                                                               [(eq? (cadr (interpreter (caddr input) myenv)) 'null) (interpreter input (car (interpreter (caddr input) myenv)))]
                                                                               [else (interpreter (caddr input) myenv)])]
                                           [else (list myenv 'null)])]
            ;ifcom
            [(eq? state 'if-ifcom) (cond
                                     [(interpreter (cadr input) myenv) (interpreter (caddr input) myenv)]
                                     [else (interpreter (cadddr input) myenv)])]
            ;assign 
            [(eq? state 'variable-assign-exp) (list (extend-env (cadr input) (lambda () (interpreter (caddr input) myenv)) myenv) 'null)]
            [(eq? state 'variable-assign-function) (list (extend-env (cadr input) (lambda () (caddr input)) myenv) 'null)]
            [(eq? state 'variable-assign-call) (list (extend-env (cadr input) (lambda () (cadr (interpreter (caddr input) myenv))) myenv) 'null)]
            ;return
            [(eq? state 'return-return) (list myenv (interpreter (cadr input) myenv))]
            ;exp
            [(eq? state 'aexp-exp) (interpreter (cadr input) myenv)]
            [(eq? state 'gt-exp) (let((op1 (interpreter (cadr input) myenv)) (op2 (interpreter (caddr input) myenv)))
                                   (cond
                                     [(and (number? op1) (number? op2)) (> op1 op2)]
                                     [(and (string? op1) (string? op2)) (string>? op1 op2)]
                                     [(and (list? op1) (number? op2)) (gt-list-number? op1 op2)]  
                                     [(and (list? op1) (string? op2)) (gt-list-string? op1 op2)]))]
            
            [(eq? state 'lt-exp) (let((op1 (interpreter (cadr input) myenv)) (op2 (interpreter (caddr input) myenv)))
                                   (cond
                                     [(and (number? op1) (number? op2)) (< op1 op2)]
                                     [(and (string? op1) (string? op2)) (string<? op1 op2)]
                                     [(and (list? op1) (number? op2)) (lt-list-number? op1 op2)]
                                     [(and (list? op1) (string? op2)) (lt-list-string? op1 op2)]))]
            
            [(eq? state 'condeq-exp) (let((op1 (interpreter (cadr input) myenv)) (op2 (interpreter (caddr input) myenv)))
                                   (cond
                                     [(and (number? op1) (number? op2)) (equal? op1 op2)]
                                     [(and (string? op1) (string? op2)) (equal? op1 op2)]
                                     [(and (null? op1) (null? op2)) #t]
                                     [(and (boolean? op1) (boolean? op2)) (eq? op1 op2)]
                                     [(list? op1) (eq-list-element? op1 op2)]
                                     [#t #f]))]
            
            [(eq? state 'ne-exp) (let((op1 (interpreter (cadr input) myenv)) (op2 (interpreter (caddr input) myenv)))
                                   (cond
                                     [(and (number? op1) (number? op2)) (not (equal? op1 op2))]
                                     [(and (string? op1) (string? op2)) (not (equal? op1 op2))]
                                     [(and (null? op1) (null? op2)) #f]
                                     [(and (boolean? op1) (boolean? op2)) (not (eq? op1 op2))]
                                     [(list? op1) (not (eq-list-element? op1 op2))]))]
            ;aexp
            [(eq? state 'bexp-aexp) (interpreter (cadr input) myenv)]

            [(eq? state 'minus-aexp) (let((op1 (interpreter (cadr input) myenv)) (op2 (interpreter (caddr input) myenv)))
                                   (cond
                                     [(and (number? op1) (number? op2)) (- op1 op2)]
                                     [(and (list? op1) (number? op2)) (apply-minus-list-num op1 op2)]
                                     ))]
            
            ;[(eq? state 'minus-aexp) (- (interpreter (cadr input) myenv) (interpreter (caddr input) myenv))]
            
            [(eq? state 'plus-aexp) (let((op1 (interpreter (cadr input) myenv)) (op2 (interpreter (caddr input) myenv)))
                                   (cond
                                     [(and (number? op1) (number? op2)) (+ op1 op2)]
                                     [(and (number? op1) (list? op2)) (apply-arithop-list2 op1 op2 +)] 
                                     [(and (list? op1) (number? op2)) (apply-arithop-list1 op1 op2 +)]
                                     [(and (boolean? op1) (boolean? op2)) (or op1 op2)]
                                     [(and (boolean? op1) (list? op2)) (or (reduce-or op2) op1)]
                                     [(and (list? op1) (boolean? op2)) (reduce-or op1 op2)]
                                     [(and (boolean? op1) (list? op2)) (reduce-or op2 op1)]
                                     [(and (list? op1) (list? op2)) (append op1 op2)]
                                     [(and (string? op1) (string? op2)) (string-append op1 op2)]
                                     [(and (list? op1) (string? op2)) (list-string-append op1 op2)]
                                     [(and (string? op1) (list? op2)) (string-list-append op1 op2)]
                                     ))]
                                     
            ;bexp
            [(eq? state 'cexp-bexp) (interpreter (cadr input) myenv)]
            
            [(eq? state 'mul-bexp) (let((op1 (interpreter (cadr input) myenv)))
                                     (cond
                                     [(and (number? op1) (eq? op1 0)) 0]
                                     [(and (boolean? op1) (eq? op1 #f)) #f]
                                     [#t (let((op2 (interpreter (caddr input) myenv)))
                                   (cond
                                     [(and (number? op1) (number? op2)) (* op1 op2)]
                                     [(and (number? op1) (list? op2)) (apply-arithop-list2 op1 op2 *)] 
                                     [(and (list? op1) (number? op2)) (apply-arithop-list1 op1 op2 *)]
                                     [(and (boolean? op1) (boolean? op2)) (and op1 op2)]
                                     [(and (boolean? op1) (list? op2)) (reduce-and op2 op1)]
                                     [(and (list? op1) (boolean? op2)) (reduce-and op1 op2)]
                                     ))]
                                     )
                                     )]
            
            [(eq? state 'div-bexp) (let((op1 (interpreter (cadr input) myenv)) (op2 (interpreter (caddr input) myenv)))
                                   (cond
                                     [(and (number? op1) (number? op2)) (/ op1 op2)]
                                     [(and (number? op1) (list? op2)) (apply-arithop-list2 op1 op2 /)] 
                                     [(and (list? op1) (number? op2)) (apply-arithop-list1 op1 op2 /)]
                                     ))]

            ;cexp
            [(eq? state 'minus-cexp) (let((op1 (interpreter (cadr input) myenv)))
                                   (cond
                                          [(number? op1) (- op1)]
                                          [(boolean? op1) (not op1)]
                                          [(list? op1) (neg-list op1)]
                                     ))]
            


            [(eq? state 'par-cexp) (interpreter (cadr input) myenv)]
            [(eq? state 'posnum) (cadr input)]
            [(eq? state 'null-cexp) 'null]
            [(eq? state 'var-cexp) (apply-env myenv (cadr input))]
            [(eq? state 'true-cexp) #t]
            [(eq? state 'false-cexp) #f] 
            [(eq? state 'string-cexp) (cadr input)]
            [(eq? state 'list-cexp) (interpreter (cadr input) myenv)]
            [(eq? state 'var-listmem-cexp) (find-memval-in-list (apply-env myenv (cadr input)) (interpreter (caddr input) myenv))]
            ;list
            [(eq? state 'listValues-list) (interpreter (cadr input) myenv)]
            [(eq? state 'obrac-list) (list)]
            ;listValues
            [(eq? state 'exp-listValues) (list (interpreter (cadr input) myenv))]
            [(eq? state 'exp-comma-listValues) (append (list (interpreter (cadr input) myenv)) (interpreter (caddr input) myenv))]
            ;listmem
            [(eq? state 'exp-listmem) (list (interpreter (cadr input) myenv))]
            [(eq? state 'exp-listmem-listmem) (append (list (interpreter (cadr input) myenv)) (interpreter (caddr input) myenv))]
            ;vars
            [(eq? state 'function-vars) (list( cadr input))]
            [(eq? state 'function-variable-comma-vars) (append (list (cadr input)) (interpreter (caddr input) myenv))]
            ;args
            [(eq? state 'call-args) (list (lambda () (interpreter (cadr input) myenv)))]
            [(eq? state 'call-exp-comma-args) (append (list (lambda () (interpreter (cadr input) myenv))) (interpreter (caddr input) myenv))]
            ;call
            [(eq? state 'perform-call) (interpreter (caddr (apply-env myenv (cadr input))) (extend-env-multi (interpreter (cadr (apply-env myenv (cadr input))) myenv) (interpreter (caddr input) myenv) (extend-env (cadr input) (apply-env myenv (cadr input)) (find-env myenv (cadr input)))))]
            )
       )]
      )))
 

(define basic-functions "pow = func(a, b){
if b == 0 then return 1 else f = pow(a, b - 1); return a * f endif
};
make_list = func(a, b){
if a < 1 then return [] else a = make_list(a - 1, b); return a + [b] endif
};
length = func(a){
counter = 0;
while a[counter] != [] do counter = counter + 1 end;
return counter
};
reverse = func(a){
l = length(a);
l = l - 1;
new_arr = [];
while l > -1 do new_arr = new_arr + [a[l]]; l = l - 1 end;
return new_arr
};
set =func(a, index, value){
l = length(a);
i = 0;
new_arr = [];
while i < l do if i == index then new_arr = new_arr + [value] else new_arr = new_arr + [a[i]] endif; i = i + 1 end;
return new_arr
};
merge = func(a, b){
n = length(a);
m = length(b);
new_arr = [];
i = 0;
j = 0;
continue = true;
while continue == true do if a[i] < b[j] then new_arr = new_arr + [a[i]]; i = i + 1 else new_arr = new_arr + [b[j]]; j = j + 1 endif; if i < n then continue = true else continue = false endif; if j < m then continue = true else continue = false endif end;
while i < n do new_arr = new_arr + [a[i]]; i = i + 1 end;
while j < m do new_arr = new_arr + [b[j]]; j = j + 1 end;
return new_arr
};")

(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer
  (lambda (program)
    (lex-this PL-lexer (open-input-string program))))


(define evaluate
  (lambda(path) (let ((res (interpreter (PL-parser (my-lexer (string-append basic-functions (string-replace (file->string path) "\n" "")))) (empty-env)) ))
              (begin (display "Env: ") (print (car res)) (newline)
                   (display "Output: ") (display (cadr res))))))

 
(evaluate "a.txt") 

