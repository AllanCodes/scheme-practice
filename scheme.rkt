;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname scheme) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
#|
Allan Eivazian
Assignment #3
CS 141
Scheme
|#


; Problem 1: fourth-element: This function takes a list as input and returns the 4th element

; find the nth element, expects that the list has at least n elements, here checked by fourth-element
(define nth-helper
  (lambda (lst idx)
    (if (equal? idx 1)
        (first lst)
        (nth-helper (rest lst) (- idx 1)))))
   

; fourth-element
(define fourth-element
  (lambda (L)
    (if (not (list? L))
        `()
        (if (< (list-length L) 4)
            `()
            (nth-helper L 4)))))
          





; Problem 2: The list-length function takes a list L and returns the number of elements in the list


; Find the length of the list - helper, allows to pass starting count
(define len-helper
  (lambda (lst count)
    (if (empty? lst)
        count
        (len-helper (rest lst) (+ count 1)))))

; list-length
(define list-length
  (lambda (L)
    (if (list? L)
        (if (empty? L)
            0
            (len-helper L 0))
        "not list")))


; Problem 3: The count-matches function takes a symbol S and a simple list L of symbols and returns the number of times that S occurs in L

(define count-matches-helper
  (lambda (x lst count)
    (if (empty? lst)
        count
        (if (equal? (first lst) x)
            (count-matches-helper x (rest lst) (+ count 1))
            (count-matches-helper x (rest lst) count)))))

(define count-matches
  (lambda (S L)
    (if (list? L)
        (count-matches-helper S L 0)
        "not a list")))

; Problem 4: The my-append function takes two lists, L1 and L2, and returns the concatenation of L1 and L2.
;            ("Concatenation" means to stick one on the end of the other.)
;             Note that concatenation is not the same thing as what "cons" does to two lists.

(define last-element
  (lambda (lst)
    (cond
     [(equal? (list-length lst) 1) (first lst)]
     [(empty? lst) `()]
     [(not (empty? lst)) (last-element (rest lst))])))

(define list-without-last
  (lambda (lst)
  (cond
    [(empty? (rest lst)) empty]
    [(cons? lst) (cons (first lst) (list-without-last (rest lst)))])))

(define my-append-helper
  (lambda (L1 L2)
    (if (empty? L1)
        L2
        (my-append-helper (list-without-last L1) (cons (last-element L1) L2)))))

(define my-append
  (lambda (L1 L2)
    (cond
      [(empty? L1) L2]
      [(empty? L2) L1]
      [else (my-append-helper L1 L2)])))
    

; Problem 5: The is-increasing? function takes a simple list L of numbers and returns true if the numbers in the list are increasing as you read them from beginning to end, and false if they aren't. 


(define is-increasing-helper
  (lambda (L prev)
    (if (empty? L)
        "true"
        (if (< (first L) prev)
            "false"
            (is-increasing-helper (rest L) (first L))))))


(define is-increasing?
  (lambda (L)
    (if (list? L)
        (if (empty? L)
            "true"
            (is-increasing-helper L (first L)))
        "not list")))

; Problem 6: The remove-duplicates function takes a simple list L and returns a new list with all of the duplicate objects in L removed.

(define remove-duplicates-helper
  (lambda (L newList duplicates)
    (if (empty? L)
        newList
        (if (> (count-matches (first L) duplicates) 0)
            (remove-duplicates-helper (rest L) newList duplicates)
            (remove-duplicates-helper (rest L) (my-append newList (list (first L))) (my-append duplicates (list (first L))))))))
        
(define remove-duplicates
  (lambda (L)
    (remove-duplicates-helper L `() `())))
