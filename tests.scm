(* Bunch of tests for bits of functionality. By no means complete! *)
(load "prelude.scm")
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
#t
#f
(λ x y (x y))

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

(display "...testing function definitions")
(define (prc0 (a b) (+ a b)))
(define (prc1 (a . b) (+ a b)))

(display "...testing function application")
(prc0 2 2)
(prc1 4 4)

(display "")
"Tests ran OK"
