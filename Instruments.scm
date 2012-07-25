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

;;; ********************************************definitions****************************************** ;;;

(define CHANNELCOUNTER 1)
(define CHANNELLIST
   (list (list "Piano" 0 INST1)))
(define INSTRUMENT "Piano")

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
         
                   
                                       