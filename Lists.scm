
;;; Procedure:
;;;     list-append!
;;; Parameters:
;;;     lst, a list
;;;     val, a scheme object
;;; Purpose:
;;;     append val to the end of lst
;;; Produces:
;;;     Nothing, called for side-effects
;;; Preconditions:
;;;     [none]
;;; Postconditions:
;;;     lst will have val appended to it.  
          
(define list-append!
  (lambda (lst val)
    (if (null? (cdr lst))
        (set-cdr! lst (cons val '()))
        (list-append! (cdr lst) val))
    lst))

;;; Procedure:
;;;     list-add
;;; Parameters:
;;;     lst, a list of numbers
;;; Purpose:
;;;     add the elements of lst
;;; Produces:
;;;     Nothing, called for side-effects
;;; Preconditions:
;;;     all elements of lst are numbers
;;; Postconditions:
;;;     the total of list-add will be equal to the sum of all numbers in the list.  
  
(define list-add
   (lambda (lst)
      (if (null? lst)
          0
          (+ (car lst) (list-add (cdr lst))))))



;;; Procedure:
;;;     get-distance
;;; Parameters:
;;;     lst, a list
;;;     val, a value
;;;     start, an integer
;;; Purpose:
;;;     returns the number of positions between value and start index.
;;; Produces:
;;;     Nothing, called for side-effects
;;; Preconditions:
;;;     [none]
;;; Postconditions:
;;;     if val is not in list, (get-distance lst val start) will return #f.  Otherwise,
;;;     (list-ref lst (+ (get-distance lst val start) start)) will return val.

(define get-distance
  (lambda (lst val start)
      (cond ((equal? 0 (length lst))
             #f)
            ((eq? (car lst) val)
             start)
            (else
             (get-distance (cdr lst) val (+ start 1))))))
             
;;; Procedure:
;;;     get-index
;;; Parameters:
;;;     lst, a list
;;;     val, a value
;;; Purpose:
;;;     returns the index of val
;;; Produces:
;;;     Nothing, called for side-effects
;;; Preconditions:
;;;     get-distance is defined as above
;;; Postconditions:
;;;     if val is not in lst, (get-index (lst val)) will return #f.  Otherwise, it will return the
;;;     index of the first occurance of val in lst. In other words, as long as val is an element of
;;;     lst, (list-ref lst (get-index lst val)) will return val.

(define get-index 
  (lambda (lst val)
    (get-distance lst val 0)))
