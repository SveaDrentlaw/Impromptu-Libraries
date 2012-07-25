(define MELODY '())
(define TDONE #f)
(define LDONE #f)
(define TFIRST #t)
(define LFIRST #t)




;;; Procedure:
;;;    make-melody!
;;; Parameters:
;;;    nmelody, a list of midi pitch numbers
;;; Purpose:
;;;    sets MELODY to nmelody
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    MELODY will be set to nmelody, and play-melody and play-melody loop will default to MELODY if 
;;;    no alternate melody is chosen.  
  
(define make-melody!
   (lambda (nmelody)
      (set! MELODY nmelody)))



;;; Procedure:
;;;    add-note!
;;; Parameters:
;;;    new, a midi pitch number
;;; Purpose:
;;;    appends new to MELODY
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    MELODY is not null
;;; Postconditions:
;;;    length of MELODY will be one more than it was previously and have new in its last position.
;;;    The previous positions of MELODY will be  unchanged.  

(define add-note!
   (lambda (new)
      (list-append! MELODY new)))


;;; Procedure:
;;;    play-melody
;;; Parameters:
;;;    time, a time to start the melody
;;;    *optional parameters*
;;;    instr, a midi instrument, defaults to INSTRUMENT
;;;    dur, a duration, defaults to DURATION
;;;    vol, a volume, defaults to VOLUME
;;;    mel, a list of notes, defaults to MELODY
;;; Purpose:
;;;    Plays a melody
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    instr is defined in CHANNELLIST. 
;;; Postconditions:
;;;    A melody following the notes in mel will be played through once, sounding in the midi 
;;;    instrument instr, each note at duration dur and volume vol.  


(define play-melody
   (lambda (time . settings)
           (cond (TDONE)
            (else (let ((dur DURATION)
                        (instr INSTRUMENT)
                        (vol VOLUME)
                        (mel MELODY))
                     (let kernel ((remaining-settings settings))                        
                        (cond ((null? remaining-settings))
                              ((eq? (car remaining-settings) 'dur)
                               (set! dur (cadr remaining-settings))                          
                               (kernel (cddr remaining-settings)))
                              ((eq? (car remaining-settings) 'vol)
                               (set! vol (cadr remaining-settings))
                               (kernel (cddr remaining-settings)))
                              ((eq? (car remaining-settings) 'instr)
                               (set! instr (cadr remaining-settings))
                               (kernel (cddr remaining-settings)))
                              ((eq? (car remaining-settings) 'mel)
                               (set! mel (cadr remaining-settings))
                               (kernel (cddr remaining-settings)))
                              (else (print "Unknown setting, only 'vol, 'dur, and 'instr are recognized at this time.")
                               (kernel (cddr remaining-settings)))))
                     (cond ((null? mel))
                           (else (play-inst time (car mel) 'vol vol 'dur dur 'instr instr)
                                 (callback (+ time dur 500) 'play-melody (+ time dur 1000) 
                                           'instr instr 'dur dur 'vol vol 'mel (cdr mel)))))))))



;;; Procedure:
;;;    play-melody-loop
;;; Parameters:
;;;    time, a time to start the loop
;;;    *optional parameters*
;;;    instr, a midi instrument, defaults to INSTRUMENT
;;;    dur, a duration, defaults to DURATION
;;;    vol, a volume, defaults to VOLUME
;;;    mel, a list of notes, defaults to MELODY
;;; Purpose:
;;;    plays and loops a melody
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    instr is defined in CHANNELLIST.
;;;    play-melody is defined as above 
;;; Postconditions:
;;;    A melody following the notes in mel will be played through and looped back to the 
;;;    beginning until stop-melody is called.  Each note will sound in the midi instrument 
;;;    instr, with duration dur, and with volume vol.  

(define play-melody-loop
   (lambda (time . settings)
      (cond  (LDONE (print "Loop has been stopped"))
             (else (if LFIRST ()
                       (set! settings (car settings)))
                   (let ((dur DURATION)
                         (mel MELODY)
                         (instr INSTRUMENT)
                         (vol VOLUME))
                      (let kernel ((remaining-settings settings))
                         (cond ((null? remaining-settings))
                               ((eq? (car remaining-settings) 'dur)
                                (set! dur (cadr remaining-settings))
                                (kernel (cddr remaining-settings)))
                               ((eq? (car remaining-settings) 'mel)
                                (set! mel (cadr remaining-settings))
                                (kernel (cddr remaining-settings)))
                               ((eq? (car remaining-settings) 'instr)
                                (set! instr (cadr remaining-settings))
                                (kernel (cddr remaining-settings)))
                               ((eq? (car remaining-settings) 'vol)
                                (set! vol (cadr remaining-settings))
                                (kernel (cddr remaining-settings)))
                               (else (if LFIRST (print "Only 'vol, 'dur, 'mel, and 'instr are recognized"))
                                     (kernel (cddr remaining-settings)))))
                      (play-melody time 'vol vol 'mel mel 'dur dur 'instr instr)
                      (set! LFIRST #f)
                      (callback (+ time (* (+ dur 1500) (length mel))) 'play-melody-loop 
                                (+ time (* (+ dur 1500) (length mel)) 500) settings))))))

            

;;; Procedure:
;;;    stop-loop
;;; Parameters:
;;;    [none]
;;; Purpose:
;;;    stops the melody loop that is playing.
;;; Produces:
;;;    nothing, called for side-effects
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    The loop that is currently playing stops when it reaches the last element of the melody.
                
(define stop-loop
   (lambda ()
      (set! LDONE #t)
      ))

;; Procedure:
;;;    ready-loop
;;; Parameters:
;;;    [none]
;;; Purpose:
;;;    allows loops and melodies to be played again after having been stopped with (stop-loop) 
;;;    or (stop-loop-now).
;;; Produces:
;;;    nothing, called for side-effects
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    play-melody and play-melody-loop will be able to play correctly.

(define ready-loop
   (lambda ()
      (set! LDONE #f)
      (set! LFIRST #t)
      (set! TDONE #f)
      (set! TFIRST #t)))



;;; Procedure:
;;;    stop-loop-now
;;; Parameters:
;;;    [none]
;;; Purpose:
;;;    stops the loop that is playing now.
;;; Produces:
;;;    nothing, called for side-effects
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    The loop currently playing is stopped immediately.

(define stop-loop-now
   (lambda ()
      (set! TDONE #t)
      (set! LDONE #t)))

            
                 