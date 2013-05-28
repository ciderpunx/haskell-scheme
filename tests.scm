(* Bunch of tests for bits of functionality. By no means complete! *)
(load "prelude.scm") ; because we need to test the functions in it
(display "Run tests?")
(define x (read))
(if (eq? x 'n)
  (exit)
  (display "\nRunning tests..."))

(display "...testing types")
#\x
#\newline
1
-3
1.4 
-6.5
-6.5s
-6.5s0
1.3849489382493839248392E3
-6.49320490329430940942
2.5+1.7i
#b1101
#b-111
#xfe23
#x-fe23
#o616
#o-616
"string"
'quoted
'(quoted lst of things)
#(1 2 "cat")
#t
#f
(λ x y (x y))
(vector "3" 'r)
(make-vector 5 #\x)

(display "...testing type? sort of functions")
(type? #(1 3 "earwig" "four" '(9 9 9)))
(type? (1 2.4 3.5 3/4 7.549354 2.6+4.5i #o456 #e-f00))
(type? "boot")
(type? 'qstr)
(type? ())
(type? map)
(boolean? #t)
(pair? '(1 . 3))
(string? "my string")
(list? '(1 2 3))
(list? (list 1 2 3))
(vector? #(1 3 2))
(symbol? 'a)
(char? #\nl)

(display "...testing maths")
(+ 1 1)
(- 2.0 1)
(/ 4 4)
(* 16.0 8)
(+ 1/5 2.22)
(+ 3 7.6 2/3 2)
(+ 1 1.2+3i)
(- 3 1.2+3i)
(+ "4.5" "2")
(/ 2+3i 2+3i)
(- -3 -1.2+-3i)
(+ -2.4 2 -1.3+-4i 2/5 4.54893489 2+8.32819i #b11 #xf00 #o-236 "5" "4.5")

(display "...testing conditionals")
(if (> 3 2) 'yes 'no)                   
(if (> 2 3) 'yes 'no)                   
(if (> 3 2) (- 3 2) (+ 3 2))                            

(cond ((> 3 2) 'greater) ((< 3 2) 'less))             
(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))  

(case (* 2 3)
  ((2 3 5 7) 'prime)
  ((1 4 6 8 9) 'composite))  

(case (car '(c d))
  ((a e i o u) 'vowel)
  ((w y) 'semivowel)
  (else 'consonant))         

(display "...testing lambdas(λs)")
(lambda a b (* 2 b))
(λ a b (* a b))

(display "...testing defines")
(define x 3)
(define y 4)
(define z (+ x y))
(define a #("rat" "bat" "cat"))

(display "...testing function definitions")
(define (prc0 (a b) (+ a b)))
(define (prc1 (a . b) (+ a b)))

(display "...testing function application")
(prc0 2 2)
(prc1 4 4)

(display "...testing sequencing")
(begin 
  (+ 4 4)
  (* 3 3)
  (/ 5 2.5))

(display "...testing library functions")
(list 1 2 3 4)
(count 2 2 "x")
(length 12 12 "y")
(min 2 3 1 0)
(max 2 4 2 6 2)
(foldl + 0 (list 1 2 3))
(map (λ (x) (+ x 1)) (list 1 2 3))
(id 2)
(any? (λ (x) (eq? #t x)) #f #t #f)
(every? (λ (x) (eq? #t x)) #t #t #t)
(not (every? (λ (x) (eq? #t x)) #t #t #t))
(null? '())
(zero? 3)
(positive? 2)
(negative? -1.5)
(define x (compose (λ (x) (eq? x #t)) zero?))
(x 0)
(list_last (list 1 2 3))
(last 1 2 3)
(vector-copy #(1 3 5 7))

(display "")
"Tests ran OK"
