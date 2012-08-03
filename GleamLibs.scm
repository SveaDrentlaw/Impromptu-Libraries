;;2 instruments made in order to accomodate 30 instruments, not counting untuned percussion sounds.  
(define INST1
   (au:make-node "aumu" "dls " "appl"))
(define INST2
   (au:make-node "aumu" "dls " "appl"))
(define merger
   (au:make-node "aufc" "merg" "appl"))

(au:connect-node INST1 0 merger 0)
(au:connect-node INST2 0 merger 1)
(au:connect-node merger 0 *au:output-node* 0)
(au:update-graph)

;;; ********************************************globals****************************************** ;;;
(define VOLUME 80)
(define DURATION *second*)
(define CHANNELCOUNTER 1)
(define CHANNELLIST
   (list (list "Piano" 0 INST1)))
(define INSTRUMENT "Piano")
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
(define MELODY '())
(define TDONE #f)
(define LDONE #f)
(define TFIRST #t)
(define LFIRST #t)
(define DVOL 80)
(define DDUR (* .5 *second*))


;;;****************************************functions**********************************************;;;

;;;*******************************list functions********************************;;;


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






;;;*******************************attribute functions****************************;;;

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


;;;******************************Instrument functions*****************************;;;

;;; Procedure:
;;;    set-instrument!
;;; Parameters:
;;;    instr, an instrument
;;; Purpose:
;;;    sets value of INSTRUMENT to instr
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    instr is defined in CHANNELLIST
;;; Postconditions:
;;;    INSTRUMENT is now defined as instr

(define set-instrument!
   (lambda (instr)
      (set! INSTRUMENT instr)))

;;; Procedure:
;;;    next-instrument!
;;; Parameters:
;;;    [none]
;;; Purpose:
;;;    sets value of INSTRUMENT to the instrument directly following the current INSTRUMENT in
;;;    CHANNELLIST
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    get-index is defined as above
;;; Postconditions:
;;;    INSTRUMENT is now set to the instrument at the index of CHANNELLIST one more than the index
;;;    of the previous definition of INSTRUMENT.  If the previous definition of INSTRUMENT was the
;;;    last instrument in the list CHANNELLIST, then INSTRUMENT is now defined as the instrument 
;;;    at index 0 of CHANNELLIST.


(define next-instrument!
   (lambda ()
      (set! INSTRUMENT 
            (car (list-ref CHANNELLIST 
                      (modulo (+ 1 (get-index CHANNELLIST (assoc INSTRUMENT CHANNELLIST)))
                              (length CHANNELLIST)))))))

;;; Procedure:
;;;    previous-instrument!
;;; Parameters:
;;;    [none]
;;; Purpose:
;;;    sets value of INSTRUMENT to the instrument directly before the current INSTRUMENT in
;;;    CHANNELLIST
;;; Produces:
;;;    nothing, called for side-effects.
;;; Preconditions:
;;;    get-index is defined as above
;;; Postconditions:
;;;    INSTRUMENT is now set to the instrument at the index of CHANNELLIST one less than the index
;;;    of the previous definition of INSTRUMENT.  If the previous definition of INSTRUMENT was the
;;;    first instrument in the list CHANNELLIST, then INSTRUMENT is now defined as the last 
;;;    instrument in the list CHANNELLIST. 

(define previous-instrument!
   (lambda ()
      (set! INSTRUMENT 
            (car (list-ref CHANNELLIST 
                      (modulo (- (get-index CHANNELLIST (assoc INSTRUMENT CHANNELLIST)) 1)
                              (length CHANNELLIST)))))))

;;; Procedure:
;;;    add-inst!
;;; Parameters:
;;;    instr, an instrument to add
;;; Purpose:
;;;    Adds an instrument to the list CHANNELLIST and sets a channel and au to make the midi
;;;    sound associated with that instrument.
;;; Produces: 
;;;    Nothing, called for side effects.
;;; Preconditons:
;;;    instr is an instrument from the list instlist.
;;;    time is after (now)
;;;    list-append! is defined correctly.
;;;    CHANNELCOUNTER, CHANNELLIST, instlist, INST1, INST2 are defined correctly.
;;; Postconditions:
;;;    A list of the form (instr channel au) will be added to the end of CHANNELLIST, where channel
;;;    is the channel assigned to instr and au is the audio unit that is assigned to instr.  Channel
;;;    will be from 0-15 and not be 9.  This channel will be mapped to the midi sound that instr
;;;    makes.  This channel will not have been already used for a different instrument.  If there are
;;;    already 30 instruments defined, it will ask you to use replace-inst! instead.  


(define add-inst!
   (lambda (instr)
      (cond ((= CHANNELCOUNTER 31)
             (print "Sorry, we only support 30 instruments at this time.  Please use replace-inst! instead."))
            ((= (modulo CHANNELCOUNTER 16) 9)             ;;channel 9 is always percussion sounds
             (set! CHANNELCOUNTER (+ 1 CHANNELCOUNTER))
             (add-inst! instr))
            ((> CHANNELCOUNTER 15)
             (au:midi-out (now) INST2 *io:midi-pc* (modulo CHANNELCOUNTER 16) (list-ref (assoc instr instlist) 1) 0)
             (list-append! CHANNELLIST (list instr (modulo CHANNELCOUNTER 16) INST2))
             (set! CHANNELCOUNTER (+ 1 CHANNELCOUNTER)))
            (else (au:midi-out (now) INST1 *io:midi-pc* CHANNELCOUNTER (list-ref (assoc instr instlist) 1) 0)
                   (list-append! CHANNELLIST (list instr CHANNELCOUNTER INST1))
                  (set! CHANNELCOUNTER (+ 1 CHANNELCOUNTER)))
            )))


;;; Procedure:
;;;    replace-inst!
;;; Parameters:
;;;    time, a time
;;;    old, an instrument to remove
;;;    new, an instrument to add
;;; Purpose: 
;;;    Replaces old instrument in list CHANNELLIST with new instrument and sets the channel and au
;;;    previously assigned to old instrument to new instrument.  
;;; Produces:
;;;    Nothing, called for side-effects.  
;;; Preconditions:
;;;    old is in the list CHANNELLIST exactly once.
;;;    new is in the list instlist.
;;;    time is after (now).
;;;    CHANNELLIST and instlist are defined.
;;; Postconditions:
;;;    old will no longer be in CHANNELLIST or defined on a channel.  
;;;    new will be in the position that old used to be in CHANNELLIST. 
;;;    When instr iscalled using play-inst or any functions that use play-inst, the notes played will 
;;;    make the midi noise associated with instr.  

(define replace-inst!
   (lambda (time old new)
          (set-car! (assoc old CHANNELLIST) new)
          (au:midi-out time (list-ref (assoc new CHANNELLIST) 2) *io:midi-pc* (list-ref (assoc new CHANNELLIST) 1) (list-ref (assoc new instlist) 1) 0)))



;;; Procedure: 
;;;    play-inst
;;; Parameters:
;;;    time, a time to play instrument
;;;    pitch, the pitch to be played
;;;    args, 
;;;
;;; Purpose: 
;;;    Play a note
;;; Produces:
;;;    Nothing, called for side-effects
;;; Preconditions: 
;;;    instr is defined in CHANNELLIST.
;;;    volume is in the range of hearable volume
;;;    pitch is an integer within the midi range of the instrument
;;;    time is after (now)
;;;    CHANNELLIST is defined
;;; Postconditions:
;;;    A note of midi pitch pitch, duration dur and volume vol will be played at time time out 
;;;    of the channel associated with instrument instr.  


(define play-inst
   (lambda (time pitch . args)
      (let ((vol VOLUME)
            (dur DURATION)
            (instr INSTRUMENT))
         (let kernel ((remaining-args args))
            (cond ((null? remaining-args))
                  ((eq? (car remaining-args) 'vol)
                   (set! vol (cadr remaining-args))
                   (kernel (cddr remaining-args)))
                  ((eq? (car remaining-args) 'dur)
                   (set! dur (cadr remaining-args))
                   (kernel (cddr remaining-args)))
                  ((eq? (car remaining-args) 'instr)
                   (set! instr (cadr remaining-args))
                   (kernel (cddr remaining-args)))
                  (else (print "Unknown setting, accepted are 'vol, 'dur, and 'instr")
                        (kernel (cddr remaining-args)))))
         (play-note time (list-ref (assoc instr CHANNELLIST) 2) pitch vol dur (list-ref (assoc instr CHANNELLIST) 1))))) 

          
;;list of common names for midi instruments and the settings that a channel should be put to to
;;achieve the midi sound associated with these instruments

(define instlist
   '(("Piano" 0)
     ("Acoustic Grand Piano" 0)
     ("Bright Acoustic Piano" 1)
     ("Electric Grand Piano" 2)
     ("Honky-tonk Piano" 3)
     ("Electric Piano 1" 4)
     ("Electric Piano 2" 5)
     ("Harpsichord" 6)
     ("Clavinet" 7)

     ("Celesta" 8)
     ("Glockenspiel" 9)
     ("Music Box" 10)
     ("Vibraphone" 11)
     ("Marimba" 12)
     ("Xylophone" 13)
     ("Tubular Bells" 14)
     ("Dulcimer" 15)

     ("Drawbar Organ" 16)
     ("Percussive Organ" 17)
     ("Rock Organ" 18)
     ("Church Organ" 19)
     ("Reed Organ" 20)
     ("Accordion" 21)
     ("Harmonica" 22)
     ("Tango Accordion" 23)
       
     ("Guitar" 25)
     ("Acoustic Guitar (nylon)" 24)
     ("Acoustic Guitar (steel)" 25)
     ("Electric Guitar (jazz)" 26)
     ("Electric Guitar (clean)" 27)
     ("Electric Guitar (muted)" 28)
     ("Overdriven Guitar" 29)
     ("Distortion Guitar" 30)
     ("Guitar Harmonics" 31)

     ("Bass" 32)
     ("Acoustic Bass" 32)
     ("Electric Bass (finger)" 33)
     ("Electric Bass (pick)" 34)
     ("Fretless Bass" 35)
     ("Slap Bass 1" 36)
     ("Slap Bass 2" 37)
     ("Synth Bass 1" 38)
     ("Synth Bass 2" 39)

     ("Violin" 40)
     ("Viola" 41)
     ("Cello" 42)
     ("Contrabass" 43)
     ("Tremolo Strings" 44)
     ("Pizzicato Strings" 45)
     ("Orchestral Harp" 46)
     ("Timpani" 47)

     ("Strings" 48)
     ("String Ensemble 1" 48)
     ("String Ensemble 2" 49)
     ("Synth Strings 1" 50)
     ("Synth Strings 2" 51)
     ("Aahs" 52)
     ("Choir Aahs" 52)
     ("Oohs" 53)
     ("Voice Oohs" 53)
     ("Synth Choir" 54)
     ("Orchestra Hit" 55)

     ("Trumpet" 56)
     ("Trombone" 57)
     ("Tuba" 58)
     ("Muted Trumpet" 59)
     ("French Horn" 60)
     ("Brass Section" 61)
     ("Synth Brass 1" 62)
     ("Synth Brass 2" 63)

     ("Soprano Sax" 64)
     ("Alto Sax" 65)
     ("Tenor Sax" 66)
     ("Baritone Sax" 67)
     ("Oboe" 68)
     ("English Horn" 69)
     ("Bassoon" 70)
     ("Clarinet" 71)

     ("Piccolo" 72)
     ("Flute" 73)
     ("Recorder" 74)
     ("Pan Flute" 75)
     ("Blown Bottle" 76)
     ("Shakuhachi" 77)
     ("Whistle" 78)
     ("Ocarina" 79)

     ("Square" 80)
     ("Lead 1 (square)" 80)
     ("Sawtooth" 81)
     ("Lead 2 (sawtooth)" 81)
     ("Calliope" 82)
     ("Lead 3 (calliope)" 82)
     ("Chiff" 83)
     ("Lead 4 (chiff)" 83)
     ("Charang" 84)
     ("Lead 5 (charang)" 84)
     ("Voice" 85)
     ("Lead 6 (voice)" 85)
     ("Fifths" 86)
     ("Lead 7 (fifths)" 86)
     ("Bass + Lead" 87)
     ("Lead 8 (bass + lead)" 87)

     ("New Age" 88)
     ("Pad 1 (new age)" 88)
     ("Warm" 89)
     ("Pad 2 (warm)" 89)
     ("Polysynth" 90)
     ("Pad 3 (polysynth)" 90)
     ("Choir" 91)
     ("Pad 4 (choir)" 91)
     ("Bowed" 92)
     ("Pad 5 (bowed)" 92)
     ("Metallic" 93)
     ("Pad 6 (metallic)" 93)
     ("Halo" 94)
     ("Pad 7 (halo)" 94)
     ("Sweep" 95)
     ("Pad 8 (sweep)" 95)

     ("Rain" 96)
     ("FX 1 (rain)" 96)
     ("Soundtrack" 97)
     ("FX 2 (soundtrack)" 97)
     ("Crystal" 98)
     ("FX 3 (crystal)" 98)
     ("Atmosphere" 99)
     ("FX 4 (atmosphere)" 99)
     ("Brightnees" 100)
     ("FX 5 (brightness)" 100)
     ("Goblins" 101)
     ("FX 6 (goblins)" 101)
     ("Echoes" 102)
     ("FX 7 (echoes)" 102)
     ("Sci-Fi" 103)
     ("FX 8 (sci-fi)" 103)

     ("Sitar" 104)
     ("Banjo" 105)
     ("Shamisen" 106)
     ("Koto" 107)
     ("Kalimba" 108)
     ("Bagpipe" 109)
     ("Fiddle" 110)
     ("Shanai" 111)

     ("Tinkle Bell" 112)
     ("Agogo" 113)
     ("Steel Drums" 114)
     ("Woodblock" 115)
     ("Taiko Drum" 116)
     ("Melodic Tom" 117)
     ("Synth Drum" 118)
     ("Reverse Cymbal" 119)
 
     ("Guitar Fret Noise" 120)
     ("Breath Noise" 121) 
     ("Seashore" 122)
     ("Bird Tweet" 123)
     ("Telephone Ring" 124)
     ("Helicopter" 125) 
     ("Applause" 126)
     ("Gunshot" 127)))

;;;**************************************chord functions**************************;;;



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

                                                            
                             
;;;*************************************melody functions****************************;;;




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
;;;    *optional parameters*
;;;    time, a time to start the melody, defaults to (now)
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
   (lambda settings
           (cond (TDONE)
            (else (let ((time (now))
                        (dur DURATION)
                        (instr INSTRUMENT)
                        (vol VOLUME)
                        (mel MELODY))
                     (let kernel ((remaining-settings settings))                        
                        (cond ((null? remaining-settings))
                              ((eq? (car remaining-settings) 'time)
                               (set! time (cadr remaining-settings))
                               (kernel (cddr remaining-settings)))
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
;;;    *optional parameters*
;;;    time, a time to start the loop, defaults to (now)
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
   (lambda settings
      (cond  (LDONE (print "Loop has been stopped"))
             (else (if LFIRST ()
                       (set! settings (car settings)))
                   (let ((time (now))
                         (dur DURATION)
                         (mel MELODY)
                         (instr INSTRUMENT)
                         (vol VOLUME))
                      (let kernel ((remaining-settings settings))
                         (cond ((null? remaining-settings))
                               ((eq? (car remaining-settings) 'time)
                                (set! time (cadr remaining-settings))
                                (kernel (cddr remaining-settings)))
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

;;;**********************************drum functions******************************;;;



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
                 


         
                   
                                       