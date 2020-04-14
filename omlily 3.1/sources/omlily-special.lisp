;;; ====================================================================
;;;
;;;      OM2Lily 2.0 v1.3
;;;
;;;      © 2005 IRCAM - Karim Haddad
;;;
;;; ====================================================================




(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;general tools 

;for finding previous or next obj, rest, chord or continuation...

(defun lil-prv-cont (self)
  (om::previous-container self '(om::chord om::rest)))

(defun lil-nxt-cont (self)
  (om::next-container self '(om::chord om::rest)))


(defun first-of-this-group (self grp)
       (let ((frst (car (om::collect-chords grp))))
         (equal self frst)))

(defun last-of-this-group (self grp)
       (let ((lst (car (reverse (om::collect-chords grp)))))
         (equal self lst)))

;;;;


;;tools for tempo

(defmethod! tempo->list ((self voice))
                        :initvals '(t 1)
            :indoc '("voice" "ratio")
            :icon 136
            :doc "returns a simplified list of tempi of voice or poly, when tempo is on 1/4 and on each measure -- should rewrite it..."
            (let* ((lgt-meas (1- (length (get-d self))))
                   (tempo (tempo self))
                   (frst (float (second (car tempo))))
                   (scnd (cadr tempo))
                   (list2 (loop for i in scnd
                                collect (float (second (cadr i))))))
              (x-append frst list2)))


(defmethod! tempo->list ((self poly))
            (let ((voices (inside self)))
              (mapcar #'tempo->list voices)))


(defmethod! tempo->list ((self measure))
            (let ((tempo (qtempo self)))
              (if (atom tempo) tempo 
                (cadar tempo))))

(defmethod! tempo->list ((self t))
nil)


;;;tools for dyn
#|
(setf *lily-dyn-list*
      (list "\\ppp" "\\pp" "\\p" "\\mp" "\\mf" "\\f" "\\ff" "\\fff"))


;;il y a un probleme avec une valeur dans les display-pref des dynamiques !
(defun display-dyn-pref ()
  "outputs dynamics custom settings from preferences"
  (let* ((module :score)
        (prefs-to-display (list (find module *current-pref* :key 'car)))
        (dyn (nth 11 (cadar prefs-to-display)))
        (newdyn (x-append (first-n dyn 7) 127)))
    newdyn
))

(setf *custom-dyn*
(loop for i in (cons 0 (om+ 1 (display-dyn-pref)))
      for j in (display-dyn-pref)
      for d in *lily-dyn-list*
      collect (list (list i j) d)))


|#



;;compatible dyn in lily mapping.

(defun get-dyn-from-om (elmt)
  (let (res)
    (loop 
     for i in *custom-dyn*
     do (if (and (<= (caar i) elmt) (>= (second (car i)) elmt))
            (push i res)))
    (second (car res))))

;(get-dyn-from-om 127)


#|
(setf *custom-dyn* 
      (list (list (list 0 31)  "\\ppppp")
            (list (list 32 42)  "\\pppp")
            (list (list 43 52)  "\\ppp")
            (list (list 53 61)  "\\pp")
            (list (list 62 69)  "\\p")
            (list (list 70 76)  "\\mp")
            (list (list 77 85)  "\\mf")
            (list (list 86 94)  "\\f")
            (list (list 95 101)  "\\ff")
            (list (list 102 107)  "\\fff")
            (list (list 108 116)  "\\ffff")
            (list (list 117 120)  "\\fffff")
            (list (list 121 127)  "\\sf")))
|#

;om default settings
(setf *custom-dyn* 
      (list (list (list 0 20)  "\\ppp")
            (list (list 21 40)  "\\pp")
            (list (list 41 55)  "\\p")
            (list (list 56 60)  "\\mp")
            (list (list 61 85)  "\\mf")
            (list (list 86 100)  "\\f")
            (list (list 101 115)  "\\ff")
            (list (list 116 127)  "\\fff")
            ))

(defmethod get-chan-of-voice ((voice om::voice))
            :initvals '(t ) 
            :indoc '("voice")
            :icon 135
            :doc  "Returns the first channel of the voice in order to sort it afterwards with 
sort-mono-voices-by-chan assuming that the voice has uniform channel"
  (let ((chords (om::chords voice)))
    (if chords
    (car (om::lchan (car chords))))))

#|
(defmethod get-chan-of-voice ((voice om::poly))
            (let ((voices (om::inside voice)))
              (mapcar #'get-chan-of-voice voices)))
|#

;;;for extras

(defun massq (item list)
(cdr (assoc item list :test 'equal)))

(setf *vel-for-lil*
      '(("h" . "\\fff") ("g" . "\\ff")("f" . "\\f") ("F" . "\\mf")
      ("P" . "\\mp") ("p" . "\\p")("Q" . "\\pp") ("R" . "\\ppp")))
;;;


;;;;;;;;;;;;;DA CODE;;;;;;;;;;;;;;;
;;;;where everything is transcribed



(defmethod cons-lily-tempo-ex-expr ((self om::poly) listclefs freestore switch)
  (let ((rep (list " ")) 
        (voices (om::inside self)))
    (setf *voice-num* 0)
    (loop for staff in voices
          for i = 0 then (+ i 1) 
          do
          (progn 
          (setf *voice-num* (incf *voice-num*))
          (setf rep (append rep (cons-lily-tempo-ex-expr staff (nth i listclefs) freestore switch)))))
    ;(setf rep (append rep (list ">>")))

      (setf rep (append rep (list (format nil "\\score { ~% { ~%~%"))))
      (setf rep (append rep (list (format nil "~%\% \#(with-output-to-file \"temp.lisp\"" ))))
      (setf rep (append rep (list (format nil "\% (lambda () #{ \\displayMusic {"))))
      (setf rep (append rep (list (format nil "<<"))))
      (setf rep (append rep (list (format nil "\\new ChoirStaff~%<< ~%~%"))))


    (setf *voice-num* 0)
    (loop for staff in voices
          for i = 0 then (+ i 1) 
          do
          (progn 
          (setf *voice-num* (incf *voice-num*))
          (setf rep (append rep (list (format nil "~%\\new Staff  {"))))
          (setf rep (append rep (list (string+ "\\"(cassq *voice-num* *voice-rank* )))))
          (setf rep (append rep (list "}")))
))
(setf rep (append rep (list (format nil " >> ~% >>"))))
(setf rep (append rep (list "\% } #}))")))
(setf rep (append rep (list (format nil "~%} ~% ~%"))))

rep))




(defmethod cons-lily-tempo-ex-expr ((self om::voice) clef freestore switch)
  ;(setf *chords-and-cont* (collect-chords  self))
  ;(setf *treeratios* (om-abs (treeratio (om-round (tree self)))))
  ;(setf *switch* nil)
  (setf *tempdyn* nil)
  (setf *clef-switch* 0);new
  (setf *clef-switch-b* 0);new
  (setf *mesure-num* 0)
  (setf *mesure-qtempo* (qtempo self))
  (let ((rep (list (format nil "~s=" (cassq *voice-num* *voice-rank* )) "{" 
                   (format nil "\%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
                   (format nil "\%%%%%%%%%%%%%%%%%%%%%%% VOICE : ~d %%%%%%%%%%%%%%%%%%%%%%%%" *voice-num*)
                   (format nil "\%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
                   (if *cautionnary*
                     ;(format nil "~%#(set-accidental-style 'forget)~%");;;pas si sur !!! et ne marche pas
                     (format nil "~%#(set-accidental-style 'dodecaphonic)~%")
                     (format nil "~%#(set-accidental-style 'dodecaphonic)~%"))
                   ;(format nil  "\\set tupletNumberFormatFunction = #fraction-tuplet-formatter")
                   ))
        (mesures (om::inside self))
        (lastmes nil))
    (when clef
      (setf rep (append rep (list (format nil "\\clef ~s" clef)))))
    (loop for mes in mesures
          for i = 1 then (+ i 1) do
          (setf rep (append rep (cons-lily-tempo-ex-expr mes lastmes freestore switch)))
          (setf lastmes mes))
    (setf rep (append rep (list (format nil"} ~% ~%"))))
    rep))


;symb-beat-val= For a key signature equivalent to 3//3 will be the half note (blanche)
;real-beat-val= For the same key sign, this will be the halfnote of a triplet (blanche de triolet)
;These refer to the beats in a measure, and for special cases using non-standard key signature

;;25/03/17- changed here to input measure nums individualy for each measure in each voice!



(defmethod cons-lily-tempo-ex-expr ((self om::measure) lastmes chiffrage switch)
  
  (let* (;(qtempo (qtempo self))
         ;(tempo (if (listp qtempo) (last-elem (car qtempo)) qtempo ))
         (tempo (tempo->list self))
         (tempofact (/ 60 (rationalize tempo))));; a voir le probleme ici !!!!

    (setf *chords-and-cont* (collect-chords  self))
    (setf *treeratios* (get-note-figure self))
    (setf *switch* nil)
    (let* ((inside (om::inside self))
           (tree (om::tree self))
           (ratio (list (om::fnumerator (first tree))
                        (om::fdenominator (first tree))))
           (multratio  (if (powerof2? (second ratio))
                           1/1
                         (/ (find-beat-symbol (second ratio)) (second ratio))))
          ; (withtempo (* multratio tempofact))
           (withtempo (rationalize (float (* multratio tempofact))))
           (real-beat-val (/ 1 (om::fdenominator (first tree))))
           (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
           (rep nil))
   ; (print (list (tempo->list lastmes) tempo ))
    ;  (print (list multratio withtempo))

      (setf *mesure-num* (incf *mesure-num*))
      (setf rep (list (format nil  "\%%%%%%%%%%%%%%%%%%%%%%% MESURE : ~d %%%%%%%%%%%%%%%%%%%%%%%" *mesure-num*)))
    
     ; (unless (and lastmes (equal (first tree) (first (tree lastmes))))
          
        (setf rep (append rep (list (format nil  "\\scaleDurations ~d/~d {" (numerator withtempo) (denominator withtempo)))))
        (setf rep (append rep (list (format nil  "\\time ~d/~d" (car ratio) (second ratio)))))
        (setf rep (append rep (list (format nil  "\\set Staff.timeSignatureFraction = #'(~d . ~d)" 
                                            (car ratio) (second ratio)))))
       (if (equal tempo (tempo->list lastmes)) nil
        (setf rep (append rep (list (format nil  "\\mark \\markup {\\left-column {{\\line { \\smaller \\general-align #Y #DOWN  \\note #\"4\" #1 \" = ~d \"}} \\tiny \"~d\"}} " tempo *mesure-num*))))
        )
      ;  )
      (loop for obj in inside do
            (setf rep (append rep 
                              (let* ((dur-obj-noire (/ (om::extent obj) (om::qvalue obj)))
                                     (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
                                     (exp (cons-lily-tempo-ex-expr obj (* symb-beat-val factor) (car (tree self)) switch)))
                                exp
                                )
                              )))
    ;(setf *mesure-num* (incf *mesure-num*))
    ;(setf rep (append rep (list (format nil  "\%%%%%%%%%%%%%%%%%%%%%%% MESURE : ~d %%%%%%%%%%%%%%%%%%%%%%%" *mesure-num*))))
      (setf rep (append rep (list "|")))
      (setf rep (append rep (list "}"))) 
      (setf rep (append rep (list (format nil "\\once \\set Staff.whichBar = \"|\"")))) ;ajout
      rep))
  )

(defmethod cons-lily-tempo-ex-expr ((self om::group) dur ratio switch)
  (let* ((durtot (if (listp dur) (car dur) dur))
         (cpt (if (listp dur) (cadr dur) 0))
         (num (or (om::get-group-ratio self)  (om::extent self)))
         (denom (om::find-denom num durtot))
         (num (if (listp denom) (car denom) num))
         (denom (if (listp denom) (second denom) denom))
         (unite (/ durtot denom))
         (inside (om::inside self))
         (sympli (/ num denom))
         (rep nil) (val nil))
    (cond
     ((not (om::get-group-ratio self)) 
      (loop for obj in inside
            do (setf rep (append rep (let* ((dur-obj (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                        (/ (om::extent self) (om::qvalue self)))))
                                       (cons-lily-tempo-ex-expr obj (* dur-obj durtot) ratio switch))))))
     ((= sympli 1)
      (loop for obj in inside
            do (setf rep (append rep (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                          (/ (om::extent self) (om::qvalue self))))
                                            (dur-obj (numerator (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                                   (/ (om::extent self) (om::qvalue self))))))
                                       (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                       (cons-lily-tempo-ex-expr obj (* dur-obj unite) ratio switch)))))
      
      )
     
     (t
      (let ((pos (length rep))
            (depth 0))
        (setf rep (append rep (list (format nil "~%")))) ;;;a la place de ce qui suit
        ;(setf rep (append rep (list (format nil "\\once \\override TupletBracket #'padding = #"))))
        (let* ((corratio (reduce-num-den (list num denom)))
               (numer (car corratio))
               (den (second corratio)))
          
          
#|
          (if (= 2 den)
            (setf rep (append rep (list (format nil "\\tweak #'text #tuplet-number::calc-denominator-text"))))
            (setf rep (append rep (list (format nil "\\tweak #'text #tuplet-number::calc-fraction-text"))))
            );;new since 2.9.13


|#


          (setf rep (append rep (list (format nil "\\tuplet ~d/~d {"  numer den))))
          )
        (loop for obj in inside do
              (setf rep (append rep (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                         (/ (om::extent self) (om::qvalue self))))
                                           (dur-obj (numerator operation))
                                           exp tmp)
                                      (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                      (setf tmp (multiple-value-list 
                                                 (cons-lily-tempo-ex-expr obj (list (* dur-obj unite) cpt) ratio switch)))
                                      (setf exp (car tmp))
                                      (when (and (cadr tmp) (> (cadr tmp) depth))
                                        (setf depth (cadr tmp)))
                                      exp
                                      ))))
        (setf val (+ depth 1))
        (if (= depth 0)
          (setf (nth pos rep) "")
          (setf (nth pos rep) "");;;a la place de ce qui suit
          ;(setf (nth pos rep) (string+ (nth pos rep) (format nil "2")))
          )
        (setf rep (append rep (list "}")))
        )
      ))
    (values rep val)))




  ;;reste le probleme des quintuples et plus croches
  ;;la c'est pas bien. 


;;;les beams a ce niveaux !!! chord et rest




(defmethod cons-lily-tempo-ex-expr ((self om::chord) dur ratio switch)
    (if *split-mode* ;switch 
             (cons-lily-tempo-expr-switch self dur ratio switch)
           (cons-lily-tempo-expr-norm self dur ratio))
         )


#|
  (let* ((vel (car (om::lvel self)))
         (dyn (list (get-dyn-from-om vel)))
         (str ""))

    (if (not (equal dyn *tempdyn*))
        (progn
        (setf strg (format nil "~d" dyn))
        (setf *tempdyn* dyn)))

|#


;;;another to check it out !
;;;and to include in omlilly-specmotets4.lisp (karim lib)

(defmethod cons-lily-tempo-expr-norm ((self om::chord) dur ratio)
  (let* ((notes (inside self))
         (extra (car (mapcar #'extra-obj-list notes)))
         (text (if (text-extra-p (car extra))
                   (thetext (car extra))))
         (velex (if (vel-extra-p (car extra))
                    (thechar (car extra))))
         (durtot (if (listp dur) (car dur) dur))
         (inside (om::inside self))
         (vel (car (om::lvel self)))
         (dyn (get-dyn-from-om vel))
         (chans (om::lchan self))
         (str "")
         )
    ;(print (list "chord ratio:" ratio)) 
    (if (= (length inside) 1)
        (setf str (cons-lily-note (car inside)))
      (let ((notes ""))
        (if (and *lily-chan-on* (not (om::cont-chord-p self)))
            (loop for note in inside 
                  for ch in chans
                  do (setf notes 
                           (string+ notes " " (cons-lily-note note) 
                                    (format nil "-~D" ch)
                                    ))) 
          (loop for note in inside 
                do (setf notes 
                         (string+ notes " " (cons-lily-note note))))
          ) 
        (setf str (string+ "<" notes ">")))
      )
    
    (if (>= durtot 16)
      
        (let* ((durconv (get-head-and-points durtot))
               (head (first durconv))
               (ratio-ord (if (powerof2? (second ratio))
                              1
                            (/ (find-beat-symbol (second ratio)) (second ratio))))
               (mutl ; (if (powerof2? (second ratio))
                (/ (car durconv) 2))
                          ; (* (/ (car durconv) 2) (/ (find-beat-symbol (second ratio)) (second ratio)))))
             ;(mutlratio (* ratio-ord mutl))
               (mutlratio mutl);;check...
               (points (if (< 0 (second durconv))
                           (append-str (om::repeat-n "." (second durconv))))))
        
          (setf str (proappend-str 
                     (list (format nil "\\once \\override NoteHead  #'style = #'default~%")
                           str)))
          (setf str (string+ str 
                             (if (not points)
                                 (format nil "\\breve*~d-\\markup {\\finger \"~d\"}" mutlratio (car durconv))
                               (format nil "\\breve~A*~d-\\markup {\\finger \"~d\"}" points mutlratio (car durconv))
                               )
                             )))
      
      (let* ((durconv (get-head-and-points durtot))
             (head (first durconv))
             (mutlratio  (if (powerof2? (second ratio))
                             1
                           (/ (find-beat-symbol (second ratio)) (second ratio))))
             (points (if (< 0 (second durconv))
                         (append-str (om::repeat-n "." (second durconv))))))
        

        (setf str (string+ str 
                           (if (not points)
                               (format nil "~d" head)
                             (format nil "~d~A" head points)
                             )
                           ))
        )
      
      )
    
    (if (and (= (length inside) 1) *lily-chan-on* (not (om::cont-chord-p self)))
        (setf str (string+ str (format nil "-~D" (car chans))))
      )

    (when (or (and (not (om::cont-chord-p self))
                   (om::cont-chord-p (om::next-container self '(om::chord))))
              (and (om::cont-chord-p self)
                   (om::cont-chord-p (om::next-container self '(om::chord))))
              )
      (setf str (string+ str "~"))
      )
    
    ;;;; FOR STEMING
    ;;;;
    (let ((elmpos (car (element-position self *chords-and-cont*))))
   (if (= elmpos (- (length *chords-and-cont*) 1))

       (if (and *switch* (< durtot 1/4)) ;;;;this is for the last one 
           (setf str (string+ str "]")))

    (cond 
     ((and 
       (not *switch*)
       (< durtot 1/4)
       (< (nth (+ 1 elmpos) *treeratios*) 1/4)
       (not (rest-p (lil-nxt-cont self)))
       (not(last-of-this-group self (parent self))))
       
      (progn
        (setf str (string+ str "["))
        (setf *switch* t)))
       
      
     ((and 
       *switch*
       (< durtot 1/4)
       (>= (nth (+ 1 elmpos) *treeratios*) 1/4))
      (progn
        (setf str (string+ str "]"))
        (setf *switch* nil)))

     ((and 
       *switch*
       (< durtot 1/4)
       (last-of-this-group self (parent self)))
      (progn
        (setf str (string+ str "]"))
        (setf *switch* nil)))
      
     ((and 
       *switch*
       (< durtot 1/4)
       (rest-p (lil-nxt-cont self))
       )
      (progn
        (setf str (string+ str "]"))
        (setf *switch* nil)))
      
     (t 
      ))))
 
    (if *lily-dyn-on*
        (if (not (equal dyn *tempdyn*))
            (progn
              (setf str (string+ str (format nil " ~d" dyn)))
              (setf *tempdyn* dyn)))
      )

    ;;;extras
    (if velex 
        (setf str (string+ str  (massq velex *vel-for-lil*)))
      )

    (if text     
        (setf str (string+ str  (format nil " _\\markup{ ~S}" text)))
      )
    ;;;;
    
    (list str)
    )
  )



(defmethod cons-lily-tempo-expr-switch ((self om::chord) dur ratio switch)
 ; (print (next-chord self))
  (let* ((clef *clef-switch*)
         (reg (if (< (car (lmidic self)) switch) 
                  (setf *clef-switch* 1)
                (setf *clef-switch* 0)))
         (notes (inside self))
         (extra (car (mapcar #'extra-obj-list notes)))
         (text (if (text-extra-p (car extra))
                   (thetext (car extra))))
         (velex (if (vel-extra-p (car extra))
                    (thechar (car extra))))         
         (durtot (if (listp dur) (car dur) dur))
         (inside (om::inside self))
         (vel (car (om::lvel self)))
         (dyn (get-dyn-from-om vel))
         (chans (om::lchan self))
         (str ""))
    
    (if (= (length inside) 1)
        
        (if (= *clef-switch* *clef-switch-b*)
            (setf str (cons-lily-note (car inside)))
          (let ((clef (if (= 0 *clef-switch*) 
                          (format nil "\\clef \"G\"~%")
                        (format nil "\\clef \"F\"~%"))))
                       
            (progn
              (setf str (string+ clef))
              (setf str (string+ str (cons-lily-note (car inside))))
              (setf *clef-switch-b* *clef-switch*))))
        

      (let* ((notes "")
             (clef (if (= 0 *clef-switch*) 
                       (format nil "\\clef \"G\"~%")
                     (format nil "\\clef \"F\"~%")
                     )))

        (if (and *lily-chan-on* (not (om::cont-chord-p self)))
            (loop for note in inside 
                  for ch in chans
                  do (setf notes 
                           (string+ notes " " (cons-lily-note note) 
                                    (format nil "-~D" ch)
                                    ))) 
          (loop for note in inside 
                do (setf notes 
                         (string+ notes " " (cons-lily-note note))))
          )
        

        (progn
          (setf str (string+ clef))
          (setf str (string+ str "<" notes ">"))
          (setf *clef-switch-b* *clef-switch*)))
      )      

    
    (if (>= durtot 16)
      
        (let* ((durconv (get-head-and-points durtot))
               (head (first durconv))
               (ratio-ord (if (powerof2? (second ratio))
                              1
                            (/ (find-beat-symbol (second ratio)) (second ratio))))
               (mutl  ;(if (powerof2? (second ratio))
                (/ (car durconv) 2))
                          ; (* (/ (car durconv) 2) (/ (find-beat-symbol (second ratio)) (second ratio)))))
             ;(mutlratio (* ratio-ord mutl))
               (mutlratio mutl);;ICI
               (points (if (< 0 (second durconv))
                           (append-str (om::repeat-n "." (second durconv))))))

;(print (* (/ (car durconv) 2) (/ (/ (find-beat-symbol (second ratio)) 1) (second ratio))))
        
          (setf str (proappend-str 
                     (list (format nil "\\once \\override NoteHead  #'style = #'default~%")
                           str)))
          (setf str (string+ str 
                             (if (not points)
                                 (format nil "\\breve*~d-\\markup {\\finger \"~d\"}" mutlratio (car durconv))
                               (format nil "\\breve~A*~d-\\markup {\\finger \"~d\"}" points mutlratio (car durconv))
                               )
                             )))
      
      (let* ((durconv (get-head-and-points durtot))
             (head (first durconv))
             (mutlratio  (if (powerof2? (second ratio))
                             1
                           (/ (find-beat-symbol (second ratio)) (second ratio))))
             (points (if (< 0 (second durconv))
                         (append-str (om::repeat-n "." (second durconv))))))
        

        (setf str (string+ str 
                           (if (not points)
                               (format nil "~d" head)
                             (format nil "~d~A" head points)
                             )

                           ))

        )
      
      )
    
    (if (and (= (length inside) 1) *lily-chan-on* (not (om::cont-chord-p self)))
        (setf str (string+ str (format nil "-~D" (car chans))))
      )

    
    (when (or (and (not (om::cont-chord-p self))
                   (om::cont-chord-p (om::next-container self '(om::chord))))
              (and (om::cont-chord-p self)
                   (om::cont-chord-p (om::next-container self '(om::chord))))
              )
      (setf str (string+ str "~"))
      )
    
    ;;;; FOR STEMING
    ;;;;
    (let ((elmpos (car (element-position self *chords-and-cont*))))
      (if (= elmpos (- (length *chords-and-cont*) 1))

          (if (and *switch* (< durtot 1/4)) ;;;;this is for the last one 
              (setf str (string+ str "]")))

        (cond 
         ((and 
           (not *switch*)
           (< durtot 1/4)
           (< (nth (+ 1 elmpos) *treeratios*) 1/4)
           (not (rest-p (lil-nxt-cont self)))
           (not(last-of-this-group self (parent self))))
       
          (progn
            (setf str (string+ str "["))
            (setf *switch* t)))
       
      
         ((and 
           *switch*
           (< durtot 1/4)
           (>= (nth (+ 1 elmpos) *treeratios*) 1/4))
          (progn
            (setf str (string+ str "]"))
            (setf *switch* nil)))

         ((and 
           *switch*
           (< durtot 1/4)
           (last-of-this-group self (parent self)))
          (progn
            (setf str (string+ str "]"))
            (setf *switch* nil)))
      
         ((and 
           *switch*
           (< durtot 1/4)
           (rest-p (lil-nxt-cont self))
           )
          (progn
            (setf str (string+ str "]"))
            (setf *switch* nil)))
      
         (t 
          ))))
 
    (if *lily-dyn-on*
        (if (not (equal dyn *tempdyn*))
            (progn
              (setf str (string+ str (format nil " ~d" dyn)))
              (setf *tempdyn* dyn)))
      )

    ;;;extras
    (if velex 
        (setf str (string+ str  (massq velex *vel-for-lil*)))
      )

    (if text     
        (setf str (string+ str  (format nil " _\\markup{ ~S}" text)))
      )
    ;;;;

    (list str)
    )
  )

(defmethod cons-lily-tempo-ex-expr ((self om::rest) dur ratio switch)
  (let* ((durtot (if (listp dur) (car dur) dur))
         (durconv (get-head-and-points durtot))
         (head (first durconv))
         (points (if (< 0 (second durconv))
                     (append-str (om::repeat-n "." (second durconv)))))
         (str ""))
    (prog1
        (if (>= durtot 16)
        
            (let* ((durconv (get-head-and-points durtot))
                   (head (first durconv))
                   (ratio-ord (if (powerof2? (second ratio))
                                  1
                                (/ (find-beat-symbol (second ratio)) (second ratio))))
                   (mutl 
                    (/ (car durconv) 2))
                   (mutlratio mutl)
                   (points (if (< 0 (second durconv))
                               (append-str (om::repeat-n "." (second durconv))))))

              (setf str (string+ str 
                                 (if (not points)
                                     (format nil "r\\breve*~d-\\markup {\\finger \"~d\"}" mutlratio (car durconv))
                                   (format nil "r\\breve~A*~d-\\markup {\\finger \"~d\"}" points mutlratio (car durconv))
                                   )
                                 )))
          (if (stringp head)
              (if (not points)
                  (setf str (string+ str  (format nil "r~d" head)))
                (setf str (string+ str  (format nil "r~d~A" head points)))
                )
            ;;;added this section when rests are >= 256
            ;;;these are not available in lilypond <= 2.20
            (cond
             ((and (>= head 256) points)
              (setf str (string+ str  (format nil "r~D~A*1/~D" 128 points (/ head 128)))))
             ((and (>= head 256) (not points))
              (setf str (string+ str  (format nil "r~D*1/~D" 128 (/ head 128)))))
             ((and (< head 256) points)
              (setf str (string+ str  (format nil "r~d~A" head points))))
             ((and (< head 256) (not points))
              (setf str (string+ str  (format nil "r~d" head))))
             (t ))
            )
          )
      )
    (list str)
    ))


;(string+ (car (mc->lilynotes '(6000))) "-5"))

;(string+ (car (mc->lilynotes '(6000))) (format nil "-~D" 5))



(setf *cautionnary* t)


(defmethod cons-lily-note ((self om::note))
  (if (and *cautionnary* (not (cont-chord-p (parent self))))
  (car (mc->lilynotes-nat (list (midic self))))  
  (car (mc->lilynotes (list (midic self))))))


;;;;;;;;;;;;;;;;;;;;i/o and interface

;;;peut-etre ajouter l'encoding du text : unix
;;Attention:
;;Lily genere des erreur dues aux commentaires
;;;une fois que l'on les encodes en unix tout marche





