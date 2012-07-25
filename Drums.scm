(define DVOL 80)
(define DDUR (* .5 *second*))

;;; Procedure:
;;;    set-drum-volume!
;;; Parameters:
;;;    new, an integer
;;; Purpose:
;;;    sets value of DVOL to new
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    DVOL is now defined as new

(define set-drum-volume!
   (lambda (new)
      (set! DVOL new)))

;;; Procedure:
;;;    set-drum-duration!
;;; Parameters:
;;;    new, a duration
;;; Purpose:
;;;    sets value of DDUR to new
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    DDUR is now defined as new

(define set-drum-duration!
   (lambda (new)
      (set! DDUR new)))

;;; Procedure: 
;;;    play-drums
;;; Parameters:
;;;    time, a time to play drums
;;;    drum, a string or an integer
;;;    vol, a volume
;;;    dur, a duration
;;; Purpose: 
;;;    Plays a drum sound
;;; Produces:
;;;    Nothing, called for side-effects
;;; Preconditions: 
;;;    If drum is a string, it is defined in drumlist
;;;    volume is in the range of hearable volume
;;;    time is after (now)
;;; Postconditions:
;;;    A single sound of drum with duration dur and volume vol will occur at time time.

(define play-drums
  (lambda (time drum vol dur)
     (if (string? drum) 
         (set! drum (cadr (assoc drum drumlist))))     
     (play-note time INST1 drum vol dur 9)))
           

;;; Procedure:
;;;    play-pattern 
;;; Parameters:
;;;    time, a time to start the pattern
;;;    drum, a string or integer
;;;    durlist, a list of durations
;;;    vol, a volume
;;; Purpose:
;;;    Plays a rhythmic pattern
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    drum is defined in drumlist
;;; Postconditions:
;;;    A rhythmic pattern consisting of sounds of drum with durations found in durlist, starting
;;;    with the first element of durlist and stepping through each durlist element until the end
;;;    of durlist, when it will stop playing.  

(define play-pattern
   (lambda (time drum durlist vol)
      (let ((dur (car durlist)))
         (play-drums time drum vol dur)      
         (if (null? (cdr durlist))
             ()
         (callback (+ time dur 500) 'play-pattern (+ dur time 1000) drum (cdr durlist) vol)))))

;;; Procedure:
;;;    play-drum-loop
;;; Parameters:
;;;    drum, a type of drum to use
;;;    durlist, a list of durations
;;;    *optional parameters*
;;;    time, a time to start the loop.  Defaults to (now).
;;;    vol, a volume.  Defaults to DVOL
;;; Purpose:
;;;    Plays a looped drum pattern
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    drum is defined in drumlist.  
;;;    play-pattern is defined as above.
;;; Postconditions:
;;;    A drum pattern will be played in a loop.  

(define play-drum-loop
   (lambda (drum durlist . settings)
      (let ((time (now))
            (vol DVOL)
            (dur (car durlist)))
         (if (string? drum)
             (set! drum (cadr (assoc drum drumlist))))           
         (let kernel ((remaining-settings settings))
            (cond ((null? remaining-settings))
                  ((eq? (car remaining-settings) 'time)
                   (set! time (cadr remaining-settings))
                   (kernel (cddr remaining-settings)))
                  ((eq? (car remaining-settings) 'vol)
                   (set! vol (cadr remaining-settings))
                   (kernel (cddr remaining-settings)))
                  (else (print "Unknown setting, accepted are 'vol and 'time")
                        (kernel (cddr remaining-settings)))))
         (play-pattern time drum durlist vol)
         (callback (+ time (+ (list-add durlist) (* 1000 (length durlist)))) 'play-drum-loop drum durlist 'time (+ time (+ (list-add durlist) (* 1000 (length durlist)))) 'vol vol))))
    
; gm drums
(define drumlist
   '(("kick" 35)
     ("kick-2" 36)
     ("side-stick" 37)
     ("snare" 38)
     ("hand-clap" 39)
     ("snare-2" 40)
     ("low-floor-tom" 41)
     ("closed-hi-hat" 42)
     ("hi-floor-tom" 43)
     ("pedal-hi-hat" 44)
     ("low-tom" 45)
     ("open-hi-hat" 46)
     ("low-mid-tom" 47)
     ("hi-mid-tom" 48)
     ("crash" 49)
     ("hi-tom" 50)
     ("ride" 51)
     ("chinese" 52)
     ("ride-bell" 53)
     ("tambourine" 54)
     ("splash" 55)
     ("cowbell" 56)
     ("crash-2" 57)
     ("vibraslap" 58)
     ("ride-2" 59)
     ("hi-bongo" 60)
     ("low-bongo" 61)
     ("mute-hi-conga" 62)
     ("hi-conga" 63)
     ("low-conga" 64)
     ("hi-timbale" 65)
     ("low-timbale" 66)
     ("hi-agogo" 67)
     ("low-agogo" 68)
     ("cabasa" 69)
     ("maracas" 70)
     ("short-whistle" 71)
     ("long-whistle" 72)
     ("short-guiro" 73)
     ("long-guiro" 74)
     ("claves" 75)
     ("hi-wood-block" 76)
     ("low-wood-block" 77)
     ("mute-cuica" 78)
     ("open-cuica" 79)
     ("mute-triangle" 80)
     ("open-triangle" 81)
     ("mute-surdo" 86)
     ("open-surdo" 87)))