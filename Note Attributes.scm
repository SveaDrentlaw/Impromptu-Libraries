(define VOLUME 80)
(define DURATION *second*)


;;; Procedure:
;;;    set-volume!
;;; Parameters:
;;;    new, an integer
;;; Purpose:
;;;    sets value of VOLUME to new
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    VOLUME is now defined as new


(define set-volume!
   (lambda (new)
      (set! VOLUME new)))

;;; Procedure:
;;;    change-volume!
;;; Parameters:
;;;    amt, an integer
;;; Purpose:
;;;    changes VOLUME by amt
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    VOLUME is now defined as (+ VOLUME amt)

(define change-volume!
   (lambda (amt)
      (set! VOLUME (+ VOLUME amt))))


;;; Procedure:
;;;    set-duration!
;;; Parameters:
;;;    new, a duration
;;; Purpose:
;;;    sets value of DURATION to new
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    DURATION is now defined as new
(define set-duration!
   (lambda (new)
      (set! DURATION new)))


;;; Procedure:
;;;    change-duration!
;;; Parameters:
;;;    amt, a duration
;;; Purpose:
;;;    changes DURATION by amt
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    DURATION is now defined as (+ DURATION amt)

(define change-duration!
   (lambda (amt)
      (set! DURATION (+ DURATION amt))))

