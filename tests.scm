(* Bunch of tests for bits of functionality. By no means complete! *)
(load "prelude.scm")

(* types *)
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
(* TODO IOFunc, Port*)


(* conditionals *)
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

(* lambda/λ *)
(lambda a b (* 2 b))
(λ a b (* a b))

(* defines *)
(define x 3)
(define y 4)
(define z (+ x y))

(* function definition *)
(define (prc0 (a b) (+ a b)))
(define (prc1 (a . b) (+ a b)))

(* function application *)
(prc0 2 2)
(prc1 4 4)

(* binary numeric operators *)
(+ 1 1)
(- 3 2)

"Tests ran OK"
