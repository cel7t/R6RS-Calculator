#!/usr/bin/chez --script
;; Infix Calculator for Scheme
;; Copyright © 2022 Sarthak Shah

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Library General Public
;; License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Library General Public License for more details.

;; You should have received a copy of the GNU Library General Public
;; License along with this library; if not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301, USA.

;; Details:
;; Supports +, -, **, ^, *, %, ( and ).
;; Works with floating-point numbers.
;; As Scheme is infinite-precision, be careful if you set the output as Decimal
;; Should work with all R6RS Scheme Implementations
;; Tested with Chez and Guile Scheme
;; For Chez, append at the top of the file:
;; #!/usr/bin/chez --script
;; For Guile, append at the top of the file:
;; #!/usr/bin/guile -s
;; !#
;; For other R6RS compliant scheme implementations,
;; check the 'script' function and add it to the top of this file.
;; Usage:
;; ./infix.scm "<expression>"

;; define with optional args: we want an empty list
;; define* is non-standard scheme, so here's an implementation
(define-syntax define&
	(syntax-rules ()
		((_ (name a b) body ...)
		 (define name
			 (case-lambda
				 [(b)
					(let ((a '()))
						body ...)]
				 [(a b)
					body ...])))))

(define (parse-num carry lst)
  "Parse the characters of the first number of the list."
  (let ((num (string->number (string (car lst)))))
    (if (and (not (null? (cdr lst)))
             (char-numeric? (cadr lst)))
        (parse-num (+ (* 10 carry) num) ; new carry
                   (cdr lst))
        (list (+ (* 10 carry) num)
              (cdr lst)))))

(define& (parsify parsed lst)
  "Parse numbers and arithmetic operators in a list."
  (if (null? lst)
      parsed
      (case (char-general-category (car lst))
        ;; ((Ll Lu Lt Ln Lo) "letter") ; for eventual variable support
        ((Nd Nl No) ; number
         (let ((parsed-num (parse-num 0 lst)))
           (parsify (append parsed
                            (list (car parsed-num)))
                    (cadr parsed-num))))
        ((Ps) ; we use < for parens open
         (parsify (append parsed
                          (list '<))
                  (cdr lst)))
        ((Pe) ; we use > for parens close
         (parsify (append parsed
                          (list '>))
                  (cdr lst)))
        ((Sm Pd Po Sk) ; symbol
         (let ((sym (string->symbol (string (car lst)))))
           (parsify (append parsed
                            (list sym))
                    (cdr lst))))
        ((Zs Cc) ; whitespace
         (parsify parsed (cdr lst))) ; just ignore it
        (else "error")))) ; should error

(define& (exp-replace repd lst)
  "Replaces '* '* with '^ for easier exponentiation."
  (if (null? lst)
      repd
      (if (and (eq? (car lst) '*)
               (eq? (cadr lst) '*))
          (exp-replace (append repd (list '^)) (cddr lst))
          (exp-replace (append repd (list (car lst))) (cdr lst)))))

(define (number-or-paren? chr)
  (if (or (number? chr)
          (eq? chr '<)
          (eq? chr '>))
      #t
      #f))

(define& (plus-minus pmd lst)
  "Converts num - num to num + - num"
  (if (< (length lst) 3)
      (append pmd lst)
      (if (and (number-or-paren? (car lst))
               (eq? (cadr lst) '-)
               (number-or-paren? (caddr lst)))
          (plus-minus (append pmd
                              (list (car lst)
                                    '+
                                    '-
                                    (caddr lst)))
                      (cdddr lst))
          (plus-minus (append pmd (list (car lst)))
                      (cdr lst)))))

(define& (minus-minus mmd lst)
  "Converts num - - num to num + num"
  (if (< (length lst) 4)
      (append mmd lst)
      (if (and (number-or-paren? (car lst))
               (eq? (cadr lst) '-)
               (eq? (caddr lst) '-)
               (number-or-paren? (cadddr lst)))
          (minus-minus (append mmd
                               (list (car lst)
                                     '+
                                     (cadddr lst)))
                       (cddddr lst))
          (minus-minus (append mmd (list (car lst)))
                       (cdr lst)))))

(define& (negativize ntd lst)
  "Converts - num to -num"
  (if (< (length lst) 2)
      (append ntd lst)
      (if (and (eq? (car lst) '-)
               (number? (cadr lst)))
          (negativize (append ntd (list (- (cadr lst))))
                      (cddr lst))
          (negativize (append ntd (list (car lst)))
                      (cdr lst)))))

(define& (eval-parens carry lst)
  "Evaluate procedures inside parens first."
  (if (null? lst)
      carry
      (if (eq? (car lst) '<)
          (let ((result (calculate-infix (cdr lst))))
            (eval-parens (append carry (list (car result)))
                         (cadr result)))
          (eval-parens (append carry (list (car lst)))
                       (cdr lst)))))


(define (matching? lst num)
	(let ((paren-error
				 (lambda () (and (display "Error: Parens unbalanced.\n") (exit)))))
		(if (null? lst)
				(if (not (= num 0)) (paren-error))
				(case (car lst)
					((<) (matching? (cdr lst) (1+ num))) ; plus
					((>) (matching? (cdr lst) (1- num))) ; minus
					(else (matching? (cdr lst) num))))))


(define& (verify verified lst)
	"Verify if arithmetically sound, error otherwise"
	;; Are braces matching?
	;; This is necessary for Nestedness to work
	(matching? lst 0)
	;; Are arithmetic operations singular? (i.e. no 1*/2 etc)
	;; Scheme takes care of this
	lst)

;; Thread-first Macro from Clojure
(define-syntax ->
	(syntax-rules ()
		((_ a) a) ; return VALUE if it is just VALUE
		((_ a (f . body) g ...) (-> (f a . body) g ...)))) ; use VALUE as the argument for the 1st FUNCTION

(define (infix-calc str)
  "Calculates the value of an infix arithmetic string"
	(-> str
			(string->list)
			(parsify)
			(exp-replace)
			(minus-minus)
			(plus-minus)
			(verify)
			(calculate-infix)
			(car)))

(define (frac-add intg frac)
  (if (= frac 0)
      intg
  (let ((digits (+ 1 (floor (/ (log frac) (log 10))))))
    (+ intg
       (* (if (negative? intg) -1 1)
          (/ frac
          (expt 10 digits)))))))

(define-syntax infix-op
  (syntax-rules ()
    ((infix-op op-name docstr sym fn)
     (define op-name
       ;; case-lambda docstrings are non-standard, so chez scheme does not have them.
       ;; if you are using Guile, you can uncomment docstr.
       (case-lambda ; docstr
         [(lst)
          (let ((carry '()))
            (if (or (< (length lst) 3)
                    (eq? (car lst) '>))
                (append carry lst)
                (if (and (number? (car lst))
                         (eq? (cadr lst) sym)) ; true then caddr will always be num
                    (op-name (append (list (fn (caddr lst) (car lst))) (cdddr lst))
                             carry)
                    (op-name (cdr lst)
                             (append carry (list (car lst)))))))]
         [(lst carry)
          (if (or (< (length lst) 3)
                  (eq? (car lst) '>))
              (append carry lst)
              (if (and (number? (car lst))
                       (eq? (cadr lst) sym)) ; true then caddr will always be num
                  (op-name (append (list (fn (caddr lst) (car lst))) (cdddr lst))
                           carry)
                  (op-name (cdr lst)
                           (append carry (list (car lst))))))])))))

(infix-op infix-^ "Calculates all num ^ num in list." '^ expt)
(infix-op infix-/ "Calculates all num / num in list." '/ /)
(infix-op infix-* "Calculates all num * num in list." '* *)
(infix-op infix-% "Calculates all num % num in list." '% modulo)
(infix-op infix-+ "Calculates all num + num in list." '+ +)
(infix-op infix-dot "Calculates all num . frac in list." (string->symbol ".") frac-add)

(define split-at->
  (case-lambda
    ;; docstring is non-standard. Uncomment if using Guile
    ;; "Splits '(a..b ^ c..d) into '((a..b) (c..d))"
    [(lst)
     (let ((carry '()))
       (if (null? lst)
           (list carry '())
           (if (eq? (car lst) '>)
               (list carry (cdr lst))
               (split-at-> (cdr lst)
                           (append carry (list (car lst)))))))]
    [(lst carry)
     (if (null? lst)
         (list carry '())
         (if (eq? (car lst) '>)
             (list carry (cdr lst))
             (split-at-> (cdr lst)
                         (append carry (list (car lst))))))]))

(define (calculate-infix lst)
	"Calculate the value of a cleaned infix arithmetic list"
	(let* ((args (-> lst
									 (eval-parens)
									 (negativize)
									 (split-at->)))
				 (result (-> args
										 (car)
										 (reverse)
										 (infix-dot)
										 (infix-^)
										 (infix-/)
										 (infix-*)
										 (infix-%)
										 (infix-+))))
    (if (null? (cdr args))
        result
        (list (car result) (cadr args)))))

(define (display-help-string)
	(display
	 "R6RS Infix Expression Calculator
Supports +, -, **, ^, *, %, ( and ).
Works with floating-point numbers.
As Scheme is infinite-precision, be careful if you set the output as Decimal
It should work with all R6RS Scheme Implementations and has been tested with Chez and Guile Scheme
For Chez, append at the top of the file:
#!/usr/bin/chez --script
For Guile, append at the top of the file:
#!/usr/bin/guile -s
!#
For other R6RS compliant scheme implementations,
check the 'script' function and add it to the top of this file.
Usage:
./infix.scm \"<expression>\"
The <expression> may contain integer and floating point numbers
and the symbols +, -, *, **, ^, (, ), [, ], {, } and .
Example:
./infix.scm \"2*5 + 2^(2+1) -2*2\"
Output:
The Result of the Evaluation is 14.
Passing help or -h to infix.scm prints this message.")
	(newline))



;; ./infix.scm "<expression>"
;; (calculate-infix-string (cadr (command-line)))
;; (format #t "The Result of the Evaluation is ~S.~%" (infix-calc (cadr (command-line))))

(let ((command-line-argument (cadr (command-line))))
	(cond
	 ((member command-line-argument '("help" "-h")) (display-help-string))
	 (#t
		(format #t "The Result of the Evaluation is ~S.~%" (infix-calc command-line-argument)))))

;; New idea
;; Instead of calling recursion on each nesting, we use the Nestedness theorem
;; Nestedness theorem takes the rightmost ( and evaluates it until it hits )
;; The ( * ) is replaced with the evaluated value
;; We now go to the next rightmost (, and repeat until we hit )
;; For additional completeness, ( ) could be added to open expressions too
;; i.e 2 * (2 + 2) becomes (2 * (2 + 2))
;; Then when no more parens left, evaluation is complete
