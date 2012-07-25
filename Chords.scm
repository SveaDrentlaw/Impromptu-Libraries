(define MDONE #f)
(define CDONE #f)
(define CHORD 'i)
(define CHORDVOL 80)
(define CHORDSIZE 4)
(define CHORDLOW 40)
(define CHORDHIGH 100)
(define MARKOV '((i)))
(define CHORDLIST '(i))
(define SETCHORD '(i))
(define SCALE (list 0 '^))
(define MFIRST #t)
(define CFIRST #t)
(define CINSTRUMENT "Piano")
(define MINSTRUMENT "Piano")



;;; Procedure:
;;;    set-instrument-markov!
;;; Parameters:
;;;    new, an instrument
;;; Purpose:
;;;    sets MINSTRUMENT to new
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    new is defined in CHANNELLIST
;;; Postconditions:
;;;    MINSTRUMENT will now be new, so play-markov will use new as it's midi sound.  

(define set-instrument-markov!
   (lambda (new)
      (set! MINSTRUMENT new)))



;;; Procedure:
;;;    set-instrument-chords!
;;; Parameters:
;;;    new, an instrument
;;; Purpose:
;;;    sets CINSTRUMENT to new
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    new is defined in CHANNELLIST
;;; Postconditions:
;;;    CINSTRUMENT is set to new, so play-chord-list and play-chord-loop will use the new midi sound.


(define set-instrument-chords!
   (lambda (new)
      (set! CINSTRUMENT new)))


;;; Procedure:
;;;    set-chordlow!
;;; Parameters:
;;;    new, an integer
;;; Purpose:
;;;    sets value of CHORDLOW to new
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    If new is greater than CHORDHIGH, nothing will be changed. Otherwise, CHORDLOW will now be 
;;;    defined as new.  

(define set-chordlow! 
   (lambda (new)
      (if (> new CHORDHIGH)
          (print "new is higher than the high threshold.")
          (set! CHORDLOW new))))

;;; Procedure:
;;;    set-chordhigh!
;;; Parameters:
;;;    new, an integer
;;; Purpose:
;;;    sets value of CHORDHIGH to new
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    If new is less than CHORDLOW, nothing will be changed. Otherwise, CHORDHIGH will now be 
;;;    defined as new.  

(define set-chordhigh!
     (lambda (new)
      (if (< new CHORDLOW)
          (print "new is lower than the low threshold.")
          (set! CHORDHIGH new))))

;;; Procedure:
;;;    set-chordsize!
;;; Parameters:
;;;    new, an integer
;;; Purpose:
;;;    sets value of CHORDSIZE to new
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    If new is less than 0 nothing will be changed. Otherwise, CHORDLOW will now be defined as new.


(define set-chordsize!
   (lambda (new)
      (if (< new 1)
          (print "You must have at least 1 note in a chord")
          (set! CHORDSIZE new))))

 
;;; Procedure:
;;;    set-markov-start-chord!
;;; Parameters:
;;;    new, a new chord to start markov progression with
;;; Purpose:
;;;    sets value of CHORD to new
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    pitch class library is installed
;;;    chord is in MARKOV
;;; Postconditions:
;;;    CHORD will be set to new.
;;;    when play-markov is called, the chord progression will start with new.

(define set-markov-chord!
  (lambda (new)
     (set! CHORD new)
       ))

;;; Procedure:
;;;    set-markov-scale!
;;; Parameters:
;;;    key, an integer representing the tonic of the scale
;;;    type, '^ meaning major or '- meaning minor
;;; Purpose:
;;;    sets SCALE to (list key type)
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    play-markov will use the scale defined by key and type to determine what key it plays 
;;;    in (so what chords exactly it plays)

(define set-markov-scale!
   (lambda (key type)
      (set! SCALE (list key type))))

;;; Procedure:
;;;    add-chord-markov!
;;; Parameters:
;;;    new, a chord from *pc:diatonicmajor* or *pc:diatonicminor*
;;;    fromlist, a list of chords in MARKOV that can lead to playing the chord new
;;;    tolist, a list of chords in MARKOV that can be played after playing the chord new
;;; Purpose:
;;;    adds the chord new to MARKOV by putting it in each list beginning with a chord in 
;;;    fromlist and adding a list to MARKOV of the form (new x1 x2 x3...) where each xi 
;;;    is a distinct element of tolist.
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    list-append! is defined. 
;;;    the pitch class library is installed.
;;;    all chords in fromlist are in MARKOV
;;;    all chords in tolist are in  MARKOV
;;; Postconditions:
;;;    MARKOV will now have new in it as the car of an element of MARKOV where the cdr is 
;;;    tolist and as an element of each list in MARKOV where the car of that list is in tolist. 



(define add-chord-markov!
   (lambda (new fromlist tolist)
      (for-each (lambda (c)
                   (list-append! (assoc c MARKOV) new))
                fromlist)
      (list-append! MARKOV (list new))
      (for-each (lambda (p)
                   (list-append! (assoc new MARKOV) p))
                   tolist)))
              
                                 

;;; Procedure:
;;;    play-markov
;;; Parameters:
;;;    time, a time to start the markov progression
;;;    args, 
;;; Purpose:
;;;    Plays a markov generated chord progression from the list MARKOV.  
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    instr is defined in CHANNELLIST.
;;;    the pitch class library is installed.
;;;    start-markov has been called after the last time stop-markov was used. 
;;; Postconditions:
;;;    A chord progression made from the markovian list MARKOV will be played until stop-markov
;;;    is called or if MARKOV has not been changed using add-chord-markov! a single i chord will 
;;;    be played. If no optional parameters were specified, it will be played in the sound 
;;;    associated with the midi instrument instr and each chord will have duration dur and 
;;;    volume CHORDVOL.

(define play-markov
   (lambda (time . settings)
      (cond (MDONE '())
            (MFIRST (let ((vol VOLUME)
                          (dur DURATION)
                          (instr MINSTRUMENT))
                       (let kernel ((remaining-settings settings))
                          (cond ((null? remaining-settings))
                                ((eq? (car remaining-settings) 'vol)
                                 (set! vol (cadr remaining-settings))
                                 (kernel (cddr remaining-settings)))
                                ((eq? (car remaining-settings) 'instr)
                                 (set! instr (cadr remaining-settings))
                                 (kernel (cddr remaining-settings)))
                                ((eq? (car remaining-settings) 'dur)
                                 (set! dur (cadr remaining-settings))
                                 (kernel (cddr remaining-settings)))
                                (else (print "Unknown setting.  We accept 'vol, 'instr, and 'dur")
                                      (kernel (cddr remaining-settings)))))
                       (for-each (lambda (p) 
                                    (play-inst time p 'instr instr 'vol vol 'dur dur))
                                 (pc:make-chord CHORDLOW CHORDHIGH CHORDSIZE (pc:diatonic (car SCALE) (cadr SCALE) CHORD)))
                       (set-markov-chord! (random (cdr (assoc CHORD MARKOV))))
                       (set! MFIRST #f)
                       (callback (+ time dur 400) 'play-markov (+ time dur 1000) instr vol dur)))
            (else (let ((vol (cadr settings))
                        (dur (caddr settings))
                        (instr (car settings)))
                        (for-each (lambda (p)
                               (play-inst time p 'instr instr 'vol vol 'dur dur))
                               (pc:make-chord CHORDLOW CHORDHIGH CHORDSIZE (pc:diatonic (car SCALE) (cadr SCALE) CHORD)))
                            (set-markov-chord! (random (cdr (assoc CHORD MARKOV))))
                            (callback (+ time dur 400) 'play-markov (+ time dur 1000) instr vol dur))))))
      

;;; Procedure:
;;;    stop-markov
;;; Parameters:
;;;    [none]
;;; Purpose:
;;;    stops the markov chord progression started with play-markov.
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    If a markov chord progression was previously playing, it will no longer be playing. 
                        
(define stop-markov
   (lambda ()
      (set! MDONE #t)))

;;; Procedure:
;;;    ready-markov
;;; Parameters:
;;;    [none]
;;; Purpose:
;;;    allows markov progressions to be played again using play-markov after having been 
;;;    stopped by stop-markov.
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    play-markov will be able to play markov progressions. 

(define ready-markov
   (lambda ()
      (set! MDONE #f)
      (set! MFIRST #t)))

;;; Procedure:
;;;    make-chord-prog!
;;; Parameters:
;;;    chords, a list of chords
;;; Purpose:
;;;    sets CHORDLIST to the list chords
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    the pitch class library is installed
;;; Postconditions:
;;;    CHORDLIST will be set to chords.

(define make-chord-prog!
   (lambda (chords)
      (set! CHORDLIST chords)))

;;; Procedure:
;;;    add-chord!
;;; Parameters:
;;;    new, a new chord to add to CHORDLIST
;;; Purpose:
;;;    adds new to the end of CHORDLIST
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    list-append! is defined
;;; Postconditions:
;;;    CHORDLIST now has new as the last element 

(define add-chord!
   (lambda (new)
      (list-append! CHORDLIST new)))
      


;;; Procedure:
;;;    stop-chords
;;; Parameters:
;;;    [none]
;;; Purpose:
;;;    stops the chord progression started with play-chords
;;; Produces:
;;;    nothing, called for side-effects
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    The progression that is currently playing when it gets to the last chord in CHORDLIST 
                        
(define stop-chords
   (lambda ()
      (set! CDONE #t)))

;;; Procedure:
;;;    ready-chords
;;; Parameters:
;;;    [none]
;;; Purpose:
;;;    allows chord progression to be played again using play-chords after having been stopped 
;;;    by stop-chords
;;; Produces:
;;;    nothing, called for side-effects
;;; Preconditions:
;;;    [none]
;;; Postconditions:
;;;    play-chords will be able to play progressions

(define ready-chords
   (lambda ()
      (set! CDONE #f)
      (set! CFIRST #t)))
 


;;; Procedure:
;;;    play-chord-list
;;; Parameters:
;;;    time, a time
;;;    instr, a string that is a midi instrument name
;;;    vol, an integer volume
;;;    dur, a duration
;;;    lst, a list of chords
;;; Purpose:
;;;    plays the chords in lst once
;;; Produces:
;;;    nothing, called for side-effects
;;; Preconditions:
;;;    pitch class library is installed
;;;    instr is a midi sound found in CHANNELLIST
;;;    time is after (now)
;;; Postconditions:
;;;    the chords in lst will be played on the instrument instr with duration dur and volume vol.
;;;    They will be played in order from the first element of lst to the last, and they will only be
;;;    played once.  

(define play-chord-list
   (lambda (time instr vol dur lst)
      (cond ((null? lst)
             '())
            (else (for-each (lambda (p)
                               (play-inst time p 'instr instr 'vol vol 'dur dur))
                            (pc:make-chord CHORDLOW CHORDHIGH CHORDSIZE (pc:diatonic (car SCALE) (cadr SCALE) (car lst))))
                  (callback (+ time dur 1000) 'play-chord-list (+ time dur 1400) instr vol dur (cdr lst))))))
                                
               

;;; Procedure:
;;;    play-chord-loop
;;; Parameters:
;;;    time, a time to play chords
;;;    instr, a string that is a midi instrument name
;;;    vol, an integer volume
;;;    dur, a duration
;;; Purpose:
;;;    plays a loop made up of the chords in CHORDLIST, exactly as listed there. 
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    pitch class library is installed
;;;    play-chord-list is defined
;;;    instr is a midi sound found in CHANNELLIST
;;; Postconditions:
;;;    the chords in CHORDLIST will be looped, each chord volume vol, duration dur, and using the 
;;;    midi sound associated with instr until stopped by stop-chords

(define play-chord-loop
   (lambda (time . settings)
      (cond (CDONE ())
            (CFIRST (let ((vol VOLUME)
                        (dur DURATION)
                        (instr CINSTRUMENT))
                     (let kernel ((remaining-settings settings))
                        (cond ((null? remaining-settings))
                              ((eq? (car remaining-settings) 'vol)
                               (set! vol (cadr remaining-settings))
                               (kernel (cddr remaining-settings)))
                              ((eq? (car remaining-settings) 'dur)
                               (set! dur (cadr remaining-settings))
                               (kernel (cddr remaining-settings)))
                              ((eq? (car remaining-settings) 'instr)
                               (set! instr (cadr remaining-settings))
                               (kernel (cddr remaining-settings)))
                              (else (print "Unknown setting, accepted are 'vol, 'dur, and 'instr")
                                    (kernel (cddr remaining-settings)))))
                       (play-chord-list time instr vol dur CHORDLIST)
                       (set! CFIRST #f)
                       (callback (+ time (* (length CHORDLIST) (+ 1400 dur)) 400) 'play-chord-loop (+ time (* (length CHORDLIST) (+ 1400 dur)) 800) instr vol dur)))
            (else (let ((vol (cadr settings))
                        (dur (caddr settings))
                        (instr (car settings)))
                     (play-chord-list time instr vol dur CHORDLIST)
                     (callback (+ time (* (length CHORDLIST) (+ 1400 dur)) 400) 'play-chord-loop (+ time (* (length CHORDLIST) (+ 1400 dur)) 800) instr vol dur))))))

                                                            
                             





