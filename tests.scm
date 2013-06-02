(* Bunch of tests for bits of functionality. By no means complete! *)
(load "prelude.scm") ; because we need to test the functions in it
(displayln "Run tests?")
(define x (read))
(if (eq? x 'n)
  (exit)
  (displayln "\nRunning tests..."))

(displayln "...testing types")
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
(λ (x y) (+ x y))
(vector "3" 'r)

(displayln "...testing list functions")
(assert (list 1 2 3) '(1 2 3) "List construction with list")
(assert (length '()) 0 "Empty list has zero length")
(assert (length '(1 2 3)) 3 "3 element list has length")

(displayln "...testing vector functions")
(make-vector 5 #\x)
(define vec (vector 0 '(2 2 2 2) "Anna"))
(vector-set! vec 1 '("Sue" "Sue"))
(assert (vector-length vec) 3 "3 element vector of unexpected length")
(assert (vector-length #()) 0 "empty vector, vector-length")

(displayln "...testing string functions")
(assert (string-append) "" "0 elem string append")
(assert (string-append "") "" "1 elem empty string append")
(assert (string-append "cat") "cat" "1 elem string append")
(assert (string-append "cat" "piss") "catpiss" "2 elem string append")
(assert (string-append "cat" "piss" "monkey") "catpissmonkey" "3 elem string append")
(assert (string->list "") '() "string->list of empty string")
(assert (string->list "dawg") '(#\d #\a #\w #\g) "string->list of dawg")
(assert (string-length "") 0 "string-length, empty string")
(assert (string-length "d") 1 "string-length, 1 letter string")
(assert (string-length "da") 2 "string-length, 2 letter string")
(assert (string-length "daw") 3 "string-length, 3 letter string")
(assert (string-length "dawg") 4 "string-length, 4 letter string")
(assert (string-null? "") #t "Empty string is string-null?")
(assert (string-null? "lala") #f "Non-empty string is not string-null?")


(displayln "...testing set!")
(define q 5)
(set! q 6)
(assert q 6 "set! didn't set a var as expected") 

(displayln "...testing type? sort of functions")
(type? #(1 3 "earwig" "four" '(9 9 9)))
(type? '(1 2.4 3.5 3/4 7.549354 2.6+4.5i #o456 #x-f00))
(type? "boot")
(type? 'qstr)
(type? '())
(type? map)
(assert (boolean? #t) #t "True is not true")
(pair? '(1 . 3))
(string? "my string")
(list? '(1 2 3))
(list? (list 1 2 3))
(vector? #(1 3 2))
(symbol? 'a)
(char? #\nl)

(displayln "...testing maths")
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
(assert (+ 2.3 1.5) 3.8 "Can't add up")
(assert (+ 2+3i 1+5i) 3.0+8.0i "Can't add up Complex numbers.")


(displayln "...testing conditionals")
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

(displayln "...testing lambdas(λs)")
(lambda (a b) (* 2 b))
(define a (λ (a b) (* a b)))
(assert (type? a) "Lambda (λ (\"a\" \"b\") ...)" "Lambda not correctly lambda-ing")

(displayln "...testing defines")
(define x 3)
(define y 4)
(define z (+ x y))
(define a #("rat" "bat" "cat"))
(assert (null? a) #f "Bound var not in scope")

(displayln "...testing function definitions")
(define (prc0 a b) (+ a b))
(define (prc1 a . b) (+ a (car b)))
(assert (and (null? prc0) (null? prc1)) #f "Function binding comes up null")

(displayln "...testing function application")
(prc0 2 2)
(assert (prc1 4 4) 8 "Function calling does not return expected rsult (8)")

(displayln "...testing sequencing")
(begin 
  (+ 4 4)
  (* 3 3)
  (/ 5 2.5))

(displayln "...testing as-list")
(as-list #\t)
(as-list #t)
(as-list #(1 2 3))
(as-list '(1 2 3))
(as-list '(1 . 3))
(as-list (λ (x y) (+ x y)))
(assert (as-list 'earwig) (as-list "earwig") "asList function breaks")


(displayln "...testing library functions")
(list 1 2 3 4)
(count 2 2 "x")
(length '(12 12 "y"))
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

(displayln "")
"Tests ran OK"
