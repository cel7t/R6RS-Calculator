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
;; Supports +, -, **, *, ( and ).
;; Should work with all R6RS Scheme Implementations
;; Tested with Chez and Guile Scheme
;; For Chez, append at the top of the file:
;; #!/usr/bin/chez --script
;; For Guile, append at the top of the file:
;; #!/usr/bin/guile -s
;; !#
;; For other R6RS compliant scheme implementations,
;; check the 'script' function and add it to the top of this file.


(define (parse-num carry lst)
  "Parse the characters of the first number of the list."
  (let ((num (string->number (string (car lst)))))
    (if (and (not (null? (cdr lst)))
             (char-numeric? (cadr lst)))
        (parse-num (+ (* 10 carry) num) ; new carry
                   (cdr lst))
        (list (+ (* 10 carry) num)
              (cdr lst)))))

(define (parsify parsed lst)
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

(define (exp-replace repd lst)
  "Replaces '* '* with '^ for easier exponentiation."
  (if (null? lst)
      repd
      (if (and (eq? (car lst) '*)
               (eq? (cadr lst) '*))
          (exp-replace (append repd (list '^)) (cddr lst))
          (exp-replace (append repd (list (car lst))) (cdr lst)))))

(define (number-or-paren? chr)
  (if (or (number? chr)
          (eq? chr #\<)
          (eq? chr #\>))
      #t
      #f))

(define (plus-minus pmd lst)
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

(define (minus-minus mmd lst)
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

(define (negativize ntd lst)
  "Converts - num to -num"
  (if (< (length lst) 2)
      (append ntd lst)
      (if (and (eq? (car lst) '-)
               (number? (cadr lst)))
          (negativize (append ntd (list (- (cadr lst))))
                      (cddr lst))
          (negativize (append ntd (list (car lst)))
                      (cdr lst)))))

(define (eval-parens carry lst)
  "Evaluate procedures inside parens first."
  (if (null? lst)
      carry
      (if (eq? (car lst) '<)
          (let ((result (calculate-infix (cdr lst))))
            (eval-parens (append carry (list (car result)))
                         (cadr result)))
          (eval-parens (append carry (list (car lst)))
                       (cdr lst)))))


(define (verify verified lst)
  "Verify if arithmetically sound, error otherwise"
  ;; Are braces matching?
  ;; Are arithmetic operations singular? (i.e. no 1*/2 etc)
  lst)

(define (calculate-infix-string str)
  "Calculates the value of an infix arithmetic string"
  (let* ((parsed (parsify '() (string->list str)))
         (exp-replaced (exp-replace '() parsed))
         (minus-minused (minus-minus '() exp-replaced))
         (plus-minused (plus-minus '() minus-minused))
         (verified (verify '() plus-minused)))
    (format #t "The Expression Evaluates to ~S.~%" (car (calculate-infix verified)))))

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
  "Calculates the value of a cleaned infix arithmetic list"
  (let* ((parensed (eval-parens '() lst))
         (negated (negativize '() parensed))
         (consed-args (split-at-> negated))
         (result (infix-+ (infix-% (infix-* (infix-/ (infix-^ (reverse (car consed-args)))))))))
    (if (null? (cdr consed-args))
        result
        (list (car result) (cadr consed-args)))))

;; combines all args after ./infix.scm
;; example - ./infix.scm a b ... c -> "ab...c"
(calculate-infix-string (apply string-append (cdr (command-line))))
