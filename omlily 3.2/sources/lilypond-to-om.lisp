(in-package :om)

(defpackage :lily-dummy (:nicknames :ly))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;this is to have durations or pitch. t => durations
;;;; *lil-imp-pitch*  -> nil ---> pitch
;;;; *lil-imp-pitch*  -> t ---> RT

(defvar *lil-imp-pitch* t)


(setf *lilnotes*
      '((0 . 6000) (1 . 6200) (2 . 6400) (3 . 6500) (4 . 6700)
        (5 . 6900) (6 . 7100)))



;(cassq 2 *lilnotes*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;INITIALIZE A METHOD (if redefined)
;;; (fmakunbound 'make-music) 

(defun find-value-in-lily-args (arglist name)
  (let ((args (group-list arglist 2 'circular)))
    (second (find name args :key 'car))))

(defun find-value-in-lily-args-deep (arglist name)
    (let* ((lgt (length arglist))
          (args (group-list arglist 2 'linear)))
    (find name args :key 'car)))


(defun find-value-in-lily-args-simp (arglist name)
    (find name arglist :key 'car))


(defun find-position (item sequence)
  (remove nil  (loop for i in sequence
                     for pos from 0 to (length sequence)
                     collect (if (equal i item) pos))))




;;;;;;;;;;;;

(defun ly-make-moment (a &rest other-args)
 (x-append a other-args))


;;;joker

(defmethod make-music ((type T) &rest other-args)
   (let* ((elements (append 
                     (find-value-in-lily-args other-args 'elements)
                     (find-value-in-lily-args other-args 'element)
                     )))
    (remove 'nil elements  )
     ))

;;;;;;;;;;;;;
;;;for voices
;;; (fmakunbound 'make-music) 


(defmethod make-music ((type (eql 'ContextSpeccedMusic)) &rest other-args)
(find-value-in-lily-args other-args 'element))




;;;;;;Articuation

(defmethod make-music ((type (eql 'articulations)) &rest other-args)
(list
(list type (find-value-in-lily-args other-args 'tieevent))
(list type (find-value-in-lily-args other-args 'absolutedynamicevent))
))


;;;;;;TieEvent
(defmethod make-music ((type (eql 'TieEvent)) &rest other-args)
  type)

;;;;;;AbsoluteDynamicEvent

;(defmethod make-music ((type (eql 'AbsoluteDynamicEvent)) &rest other-args))

(defmethod make-music ((type (eql 'AbsoluteDynamicEvent)) &rest other-args)
(list type (find-value-in-lily-args other-args 'text)))




;;;;;;;;;;;;;;;;Lily Functions to be retreived ;;;;;;;;;;;;;;;;;;;;;;
;;;macro n'evalue pas les arguments !

(defmacro markup (a &rest other-args) nil)
(defvar ly-set-middle-C! nil)
(defvar cross-staff-connect nil)
;(defvar constante-hairpin nil)
;;;RestEvent



(defmethod make-music ((type (eql 'RestEvent)) &rest other-args)
  (let* ((durs (find-value-in-lily-args other-args 'duration))
         (fig (car durs))
         (dot (second durs))
         (fact (third durs))
         )
    (if *lil-imp-pitch*
    (make-instance 'lily-dur
                   :figure fig
                   :dot dot
                   :fact fact
                   :tieevent 0
                   :restevent 1
                   )
    )
        ))


(defmethod make-music ((type (eql 'SkipEvent)) &rest other-args)
  (let* ((durs (find-value-in-lily-args other-args 'duration))
         (fig (car durs))
         (dot (second durs))
         (fact (third durs))
         )
    (if *lil-imp-pitch*
    (make-instance 'lily-dur
                   :figure fig
                   :dot dot
                   :fact fact
                   :tieevent 0
                   :restevent 1
                   )
    )
        ))



(defmethod make-music ((type (eql 'MultiMeasureRestMusic)) &rest other-args)
  (let* ((durs (find-value-in-lily-args other-args 'duration))
         (fig (car durs))
         (dot (second durs))
         (fact (third durs))
         )
    (make-instance 'lily-dur
                   :figure fig
                   :dot dot
                   :fact fact
                   :tieevent 0
                   :restevent 1
                   )
    
        ))


;(member 'gracemusic (flat '(("f") (gracemusic) (tieevent "pp") ("fff"))))



;;;;;;;;;;;
;;; NoteEvent
;;; (fmakunbound 'make-music) 

(defmethod make-music ((type (eql 'NoteEvent)) &rest other-args)
  (let* ((art (find-value-in-lily-args other-args 'articulations))
        ; (toto (find-value-in-lily-args other-args 'tieevent))
         (tie (if (member 'tieevent (flat art)) 1 0))) 
   ; (print (list "tie:" art))
    (if *lil-imp-pitch*
        (let* ((durs (find-value-in-lily-args other-args 'duration))
               (fig (car durs))
               (dot (second durs))
               (fact (third durs))
               )
          (make-instance 'lily-dur
                         :figure fig
                         :dot dot
                         :fact fact
                         :tieevent tie
                         :restevent 0
                         )
          )
      
      (x-append
       (if (= tie 0)
           (remove nil (list (find-value-in-lily-args other-args 'pitch)))
           )
       (last-elem art))
      )))


;;;EventChord
;;; (fmakunbound 'make-music) 

(defmethod make-music ((type (eql 'EventChord)) &rest other-args)
  (let* ((contents (remove nil (find-value-in-lily-args other-args 'elements)))
        ; (tie-ev (last-elem contents))
         (tie-ev (second (reverse contents)))
         dur chord tie)
   ;(print (list "contents:" contents))
   
    (if *lil-imp-pitch*
        (if (equal tie-ev 'tieevent) 
            (progn 
              (setf (tieevent (car contents)) 1) 
              (car contents))
          (car contents))

      
     ; (let ((test (member 'gracemusic (flat contents))))
     ;   (if test
     ;       (print (list "contents:" (flat contents)))
          
      
      (x-append 
       (progn
         (loop for i in (butlast contents)
               do (if (listp i)
                      (push (last-elem i) chord)
                    (push i tie )
                    ))
       ;(print (list "chord:" (flat (reverse chord))))
         (let* ((chrd (flat (reverse chord)))
                (res 
                 (if chrd
                     (list (objfromobjs chrd (make-instance 'chord)) tie)
                   )))
           (if (member 'tieevent (flat res)) nil (remove nil res))  
           )
         ) 
       (last-elem contents) ;getting the dynamics
       )
      
      
      )
    ))
;))




;;;Signature
;;;when declared in .ly

(defmethod make-music ((type (eql 'TimeSignatureMusic)) &rest other-args)

 (list type
  (list (find-value-in-lily-args other-args 'numerator)
   (find-value-in-lily-args other-args 'denominator))
   )
)


;;;BarCheck !!! IMPORTANT !!!! SHOULD BE PRESENT ! and surtout a closing one
;;;c-a-d une barcheck a la fin !!!!
;;;when declared in .ly

(defmethod make-music ((type (eql 'BarCheck)) &rest other-args)
  (list type)
  )



;;;TEMPO
;;(dotted-lily nil)
(defmethod make-music ((type (eql 'TempoChangeEvent)) &rest other-args)
  (let* ((dur (find-value-in-lily-args other-args 'tempo-unit))
         (calc (* (third dur) (/ 1 (expt 2 (car dur))) (dotted-lily (second dur)))));;dotted- format changed since 2.19 ! so voir le changement de ly::make-duration !
    (list type
          (find-value-in-lily-args other-args 'metronome-count)
          calc   
          )
    ))




(defmethod make-music ((type (eql 'TimeScaledMusic)) &rest other-args)
 (list (make-instance 'lily-tuplet 
                 :num (find-value-in-lily-args other-args 'denominator)
                 :denom (find-value-in-lily-args other-args 'numerator)
                 :objs (list (find-value-in-lily-args other-args 'element)))
        (find-value-in-lily-args other-args 'element)))

;;;;;For lilySpec , un-orthodox time sig! ;;;;

;;;substitute \once \set Staff.whichBar = "|"  into a BarCheck
;;;Because BarChecks don't work in this context

(defmethod make-music ((type (eql 'PropertySet)) &rest other-args)
  (let* ((bar (find-value-in-lily-args other-args 'value))
         )
    (if (equal bar "|") (list 'barcheck))

))

;;;;;;;;;;;;;;;;;
;;; (fmakunbound 'make-music) 

#|
(defmethod make-music ((type (eql 'SequentialMusic)) &rest other-args)
    (let* ((elements (find-value-in-lily-args other-args 'elements)))
      (print elements)
      )
)
|#


(defmethod make-music ((type (eql 'ApplyContext)) &rest other-args)
  )

(defmethod make-music ((type (eql 'MarkEvent)) &rest other-args)
  )

(defmethod make-music ((type (eql 'GraceMusic)) &rest other-args)
    (if (not *lil-imp-pitch*)
        (list type)
      ))

(defmethod make-music ((type (eql 'SlurEvent)) &rest other-args)
  )

(defmethod make-music ((type (eql 'OverrideProperty)) &rest other-args)
  )

(defmethod make-music ((type (eql 'RevertProperty)) &rest other-args)
  )

(defmethod make-music ((type (eql 'FingeringEvent)) &rest other-args)
  )

;;;;;;;;;;;;;;;;;


(defun ly-octave (oct)
  ( * oct 1200))

(defun ly-alter (alter)
(if alter
  ( * 200 alter) 0))


(defun ly-make-pitch (a &rest other-args)
 (let* ((oct (ly-octave a))
        (note (if (car other-args) (cassq (car other-args) *lilnotes* ) 6000 ));;since 2.19
        (alt (if (second other-args) (ly-alter (second other-args)) 0 )))
   
(make-instance 'chord 
               :lmidic (list (+ oct note alt)))
 ))

;;;;;;;durations

;if restevent = 1 -> it is a rest

(defclass lily-dur ()
  ((figure :initform 2 :initarg :figure :accessor figure)
   (dot :initform 0 :initarg :dot :accessor dot)
   (fact :initform 1 :initarg :fact :accessor fact)
   (restevent :initform 0 :initarg :restevent :accessor restevent)
   (tieevent :initform 0 :initarg :tieevent :accessor tieevent)
   (lydur :initform 1 :initarg :lydur :accessor lydur)
   ))



(defclass lily-tuplet ()
  ((num :initform 3 :initarg :num :accessor num)
   (denom :initform 2 :initarg :denom :accessor denom)
   (objs :initform nil :initarg :objs :accessor objs)
   (tup-d :initform 1 :initarg :tup-d :accessor tup-d)))





;;;;;;;;;;;UTILITIES;;;;;;;;;;;;;;;


(defun eq-tempochange (elmt)
  (equal elmt 'tempochangeevent))

(defun eq-barcheck (elmt)
  (equal elmt 'barcheck))

(defun eq-timesig (elmt)
  (equal elmt 'timesignaturemusic))

(defun eq-group (elmt)
  (equal elmt 'timescaledmusic))



(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 




;;;;;;;;;;;;;Test Operations on RTs;;;;;;;;;;;;;;;;

(defun all-atoms? (liste)
  "test in a list if all are atoms. If not 
this means there's a list inside- c-a-d un S de RT"
  (let* ((res t) 
         (test (if (atom liste) (setf res nil)
                   (mapcar 'atom liste))))
    (if (listp liste)
    (loop 
      for i in liste
      while res
          do  (if (atom i )
                t
                (setf res nil))))
    res))



(defun not-all-atom-lst (list)
  (if (and (listp list) (not (all-atoms? list))) list))



(defun RT? (list)
"test si c'est une RT"
  (if
  (and (listp list) 
       (atom (car list))
       (listp (second list))
       (= 1 (length (cdr list))))
  list))


(defun D-of-RT? (list)
  (if (and (RT? list)  (atom (car list)))
      (car list)))

(defun not-D-of-RT? (list)
"nul ne marche pas"
  (not (D-of-RT? list)))

;(not-D-of-RT? '(3 4 ( 1 1)))



(defun simple-rt? (list)
  (if
  (and (RT? list) (all-atoms? (second list)))
  list))

(defun complex-rt? (list)
"gives list if the list is in the form of a complex RT
meaning RT containing other RTs"
(if
  (and (RT? list) (not (all-atoms? (second list))))
  list))


(defun atom-not-s-? (list)
"returns list of atoms in a non RT list"
  (if (not (RT? list))
      (remove nil (loop for i in list
                        collect (if (atom i) 
                                    i)))))

(defun give-D-in-non-RT-lst (list)
"returns list of atoms in a non RT list"
  (if (not (RT? list))
      (remove nil (loop for i in list
                        collect (if (atom i) 
                                    i
                                  (if (RT? i)
                                      (car i)))))))


(defun simp-D-non-RT-lst (list)
"returns list of atoms in a non RT list"
  (tutuguri (if (not (RT? list))
      (remove nil (loop for i in list
                        collect (if (atom i) 
                                    i
                                  (if (RT? i)
                                      (car i))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defun pgcd-of-list (liste)
       (let ((res (car liste)))
         (if (cdr liste)
         (loop for deb in (cdr liste)
               do (setf res (pgcd res deb)))
           res)
         res))
|#
(defun pgcd-of-list (liste)
       (let ((res (car liste)))
         (loop for deb in (cdr liste)
               do (setf res (pgcd res deb)))
         res))

(defun dotted-lily (n)
  (let ((dot (loop for cnt from 1 to n
                   collect (/ 1 (expt 2 cnt)))))
    (+ 1 (apply '+ dot))))

(defun ly-make-duration (a &rest other-args)
(let* ((b (if (car other-args)  
              (car other-args) 0))
       (c (if (second other-args)
              (second other-args) 1)))
  (x-append a b c)))
               



(defun comp-lily-dur (l-dur)
  (if (typep l-dur 'lily-dur)
      (let* ((fig (figure l-dur))
             (dot (if (dot l-dur) (dot l-dur) 0))
             (fact (if (fact l-dur) (fact l-dur) 1))
             (restevent (restevent l-dur))
             (tieevent (tieevent l-dur)))
       ; (print "toto")
        (setf (lydur l-dur) (* fact (/ 1 (expt 2 fig)) (dotted-lily dot)))

        ) 
    l-dur))




(defun comp-lily-tup (l-tup)
  (if (typep l-tup 'lily-tuplet)
      (let* ((num (num l-tup))
             (denom (denom l-tup))
             (objs (objs l-tup))
             )
        (/ num denom))
    l-tup))


;;;watch for these !!!!
(defun comp-obj (obj)
  (if (atom obj)
      (comp-lily-dur obj)
    (if (and (atom (car obj)) (listp (second obj)))
        (x-append (comp-lily-dur (car obj)) 
                  (mapcar 'comp-obj (cdr obj)))
      (if (all-atoms? obj)
          (mapcar 'comp-lily-dur obj)
        (mapcar 'comp-obj obj)))))



;;this one removes the tuplets that we will not need if we use
;; Carlos strategy : calculate ONLY the durations
;; then build up 


(defun c-obj-tup (obj)
  (if (atom obj)
      obj
    (if (typep (car obj) 'lily-tuplet) 
        (mapcar 'c-obj-tup (second obj))
      (mapcar 'c-obj-tup obj))))




(defun Simplify-S (tree)
"applies the pgcd simplification on simple lists and NOT on trees!"
  (let ((fact (/ 1 (pgcd-of-list (om-abs tree)))))
    (om* tree fact)))


(defmethod pgcd-of-self ((self number)) self)

#|
(defmethod pgcd-of-self ((self list))
       (let ((res (car self)))
         (if (cadr self)
         (loop for deb in (cdr self)
               do (setf res (pgcd res deb)))
           res)
         res))
|#

(defmethod pgcd-of-self ((self list))
       (let ((res (car self)))

         (loop for deb in (cdr self)
               do (setf res (pgcd res deb)))
         res))

(defmethod reduce-S ((self number))
  (let ((fact (/ 1 (pgcd-of-self self))))
    (om* self fact)))


(defmethod reduce-S ((self list))
  (let ((fact (/ 1 (pgcd-of-self self))))
    (om* self fact)))




(defun reduce-rt-ratio (tree)
  (if (atom tree)
      tree
    (if (all-atoms? tree) ;;;;here !
      (list (apply '+ tree) (Simplify-S tree))
    (mapcar 'reduce-rt-ratio tree))))




;;;;################From CARLOS##############


(defmethod get-dur-list ((self number)) self)
;(defmethod get-dur-list ((self t)))

(defmethod get-dur-list ((self list))
  (apply '+ (mapcar 'get-dur-list self)))

;;;;#########################################


(defun get-durs-of-ratios (tree)
   (if (atom tree)
       (get-dur-list tree)
    (mapcar 'get-dur-list tree)))





(defun translate->RT (tree)
  (let* ((tree (if (atom tree) (list tree) tree))
         (get-dur (get-durs-of-ratios tree))
         (reduce-dur (reduce-S get-dur)))
        (loop for i in tree
              for em in reduce-dur
                        collect (if (atom i) em
                                  (list em (translate->RT i))
                                  )
                        )))


;(translate->RT '(5/24 5/24 (5/36 (5/216 5/216 5/216))))

;;########################################rests and floats positions#######################################

(defun durs-rests (list)
 (remove nil (loop for i in (flat list)
                   collect (if (typep i 'lily-dur) i))))

(defun restpos (list) 
  (remove nil (loop for i in list
                    for n from 0 to (1- (length list))
                    collect (if (= (restevent i) 1) n))))

(defun tiepos (list) 
  (remove nil (loop for i in list
                    for n from 0 to (1- (length list))
                    collect (if (= (tieevent i) 1) n))))


(defun tie-rest-list (list)
  (let* ((dur-rest (durs-rests list))
         (rests-pos (restpos dur-rest))
         (ties-pos (om+ 1 (tiepos dur-rest)))
         (res (repeat-n 1 (length dur-rest)))
         (res-ties (loop for i in ties-pos
                         do (setf (nth i res) 1.0))))
    (loop for i in rests-pos
          do (setf (nth i res) -1))
  res))



(defun givepulses (liste)
  (let* ((D (first liste))
         (n (second liste)))
    (loop for item in n append
          (if (atom item) (list item)
              (givepulses item)))))


;;;;;GREAT GERARD'S FUNCTION CLOSURE
(defun copy-rtree (tree subst)
  "Closure from Gerard"
(let ((substit subst))
    (labels ((copy-rtree-rec (tree)
               (list (car tree)
                     (mapcar #'copy-rgroup  (cadr tree))))
            (copy-rgroup (group)
              (if (numberp group)
                  (pop substit)
                (copy-rtree-rec group))))
    (copy-rtree-rec tree))))


;;;;;


(defun remove-markup  (lst)
  (if (atom lst) lst
    (if (equal (car lst) 'markup) nil 
      (x-append (car lst)
      (mapcar 'remove-markup (cdr lst))))))


(defun read-lily-file (pathname)
  (with-open-file (file pathname :direction :input)
    (let ((current-line (read-line file nil nil)))
      (let* ((str 
              (reduce #'(lambda (s1 s2) (concatenate 'string s1 s2))
                     
                      (cons current-line 
                            (loop while current-line collect
                                  (setf current-line 
                                        (substitute #\- #\:
                                                    (read-line file nil nil)           
                                                    ))
                                  ))))
             (str (replace-all str "#f" "'f"));;; eliminated space after "#f " in "#f"
             (str (replace-all str "#t" "'t"));;; eliminated space after "#t " in "#t"
             (str (replace-all str "<" "'("))
             (str (replace-all str ">" ")"))
             (str (replace-all str "#" "'"));;;here replaced by quote instead of nothing 
             ;in case we have ('t 't 'f) we will have '('t 't 'f)
            ; (str (replace-all str ":" " "))
            )
        (read-from-string str)
     ;  (remove-markup (read-from-string str))
))))




;;this acts as a filter - chords or durs objects

;;groups measures in one voice
(defun grp-meas (list)
  (let* ((ord (car list)) ;;cAr
         (resultat '(nil))
         timesig
         (grouping (loop for i in ord
                         for n in (cdr ord)
                         do (cond  ((and (listp i) (listp (car i)) (equal (caar i) 'tempochangeevent))
                                    )
                                    ((and (listp i) (equal (car i) 'timesignaturemusic)) 
                                    (progn 
                                      (setf timesig (second i))
                                      (push (remove nil (list timesig (cddr i))) resultat)
                                      (pop ord)))
                                   ((and (listp i) (listp n) (equal i '(barcheck))  (equal (car n) 'timesignaturemusic)) ;;added (listp n)
                                    (progn 
                                      nil
                                      (pop ord)))
                                   ((equal i '(barcheck)) 
                                    (progn 
                                      (push (remove nil (list timesig (cdr i))) resultat)
                                      (pop ord)))
                                   (t (progn 
                                        (push i (first resultat))
                                        (pop ord))))))
         )
    ;(print grouping)
    (cdr (reverse (loop for i in resultat     ;;;cAdr NO !!
                        collect  (let ((rev (reverse i)))
                                   (if (typep (cadr rev) 'lily-dur)
                                       (list (car rev) (cdr rev))
                                     rev)))))))




(defun all-grp-meas (list)
  (loop for i in list
        collect (grp-meas i)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;notes
(defun massq (item list)
(cdr (assoc item list :test 'equal)))

(setf *dyn-imp* 
      '(
        ("ppppp" . 10)
        ("pppp" . 20)
        ("ppp" . 30)
        ("pp" . 40)
        ("p" . 50)
        ("mp" . 60)
        ("mf" . 70)
        ("f" . 80)
        ("ff" . 90)
        ("fff" . 100)
        ("ffff" . 110)
        ("fffff" . 120)
        ("sf" . 127)
        ))

(defun get-dyn-from-lily (elmt)
  (massq elmt *dyn-imp*))
;(get-dyn-from-lily "pp")


(defun chordlistonly (tree)
  (if (atom tree)
      tree
      (mapcar (lambda (subtree) (if (and (atom subtree) (chord-p  subtree)) 
                                    subtree 
                                  (chordlistonly subtree)))
              tree)))

(defun all-chords (list)
  (if (= (length list) 1) nil
  (let ((res t))
    (loop for i in list 
          do (if (not (chord-p i)) (setf res nil)))
    res)))

(defun merge-chords (list)
  (loop for i in list 
        collect (if (all-chords i) 
                    (list (reduce #'merger i))
                  i)))


;;per voice
(defun lily-notes (file)
  (let* ((clean (deep-remove-items 
                 (list 'absolutedynamicevent
                       'gracemusic
                       nil)
                 file))
         (chord-pair (chordlistonly clean))
         (voice (flat-once (mapcar 'cdr chord-pair)))
         (voice (merge-chords voice))
         buf)
   ; (print (list "lily-notes:" voice))
    (loop for i in voice
          do (cond 
              ((and (= 1 (length i)) (stringp (car i))) (setf buf (car i)))
              ((and (= 1 (length i)) (chord-p (car i))) (setf (lvel (car i)) (list (get-dyn-from-lily buf))))
              ((and (chord-p (car i)) (second i)) 
               (prog1
                   (setf (lvel (car i)) (list (get-dyn-from-lily (second i))))
                 (setf buf (second i))))
              (t)))
    
    (remove nil (loop for i in (flat voice)
                      collect (if (chord-p i) i ))))) 


;;per poly
(defun get-lily-chords (file)
  (let* ((stream (lily-chord-or-dur file nil))
         (voices (all-grp-meas (car stream))))
    (loop for i in voices collect (lily-notes i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;tree


(defun remove-tempi (list)
  (loop for lst in list
        collect (remove nil (loop for i in lst
                      collect  (if (not (and (listp i) 
                                             (listp (car i)) 
                                             (member 'tempochangeevent (car i)))) i)))))

(defun get-lily-measures (file)
  (loop for i in file
          collect (list (car i) (cdr i))))


(defun deep-remove (item tree)
  (if (atom tree)
      tree
      (mapcar (lambda (subtree) (deep-remove item subtree))
              (remove item tree :test #'equal))))

(defun deep-remove-items (items tree)
  (let* ((res (clone tree)))
    (loop for i in items
          do (setf res (deep-remove i res)))
    res))

(defun lily-tree (voice)
  "one voice"
  ;(print (all-grp-meas file))
  (let* ((timesig (mapcar 'car voice))
         (lildurs (mapcar 'cdr voice))
         (lildurs (deep-remove 'gracemusic lildurs));removes gracemusic
         (ratios (loop for i in lildurs
                       collect  (let ((obj (if (= 1 (length i)) (car i) i)))
                                  (c-obj-tup (comp-obj obj)))))
         (conv (loop for i in ratios
                     collect (translate->rt i)))
         (measures (list '? (loop for m in timesig
                                  for s in conv
                                  collect (list m s))))
         (puls (givepulses measures))
         (rst-ties (tie-rest-list voice)) 
;;;prevoir si derniere note est liee ENLEVER LE SHIT !
;;donne error index too large
         (newpuls (om* puls rst-ties))
         (tree (copy-rtree measures newpuls)))
   ; (print lildurs)
    tree
    ))

;a voir probleme dans lily-tree
;(setf ratio '(1 (3/4 7/32 1/32) 1 1 1 1 (1/8 1/8 3/4) 1 (1/2 1/2) 1 (1/2 7/32 1/32 1/4) 1 (3/4 7/32 1/32) 1 1 (3/4 1/4) 1 1 1 (1/4 (1/5 1/20) 1/2) 1 1))

;(translate->rt ratio)


(defun lily-chord-or-dur (pathname mode)
"if mode = t => dur, nil => chords"
(let ((mod (if mode 
               (setf *lil-imp-pitch* t)
             (setf *lil-imp-pitch* nil))))
       (eval (read-lily-file pathname))))

(defun get-lily-tree (file)
  (let* ((stream (lily-chord-or-dur file t))
         (grouped (all-grp-meas (car stream)))) ;fix
    ;(print grouped)
    (loop for i in grouped
          collect (lily-tree i))))



;This error is due to a missing barcheck
;Error while evaluating the box LILY->OM 2 : Division-by-zero caused by / of (1 0).


;;;tempo

(defun lildur-p (item)
  (equal (type-of item) 'lily-dur))



(defun filt-tempo-meas (list)
  (let ((flt1 (remove 'nil
                      (loop for i in list 
                            collect (if (not (lildur-p i)) i))))
        )
    (remove 'nil
    (loop for i in flt1
          collect (if (not (equal (car i) 'timesignaturemusic)) i)))))




#|
(defun get-li-tempo (file)
  (let* ((stream (car (lily-chord-or-dur file t)))
|#

(defun filter-tmp-chng (list)
                (let* ((ord list)
                       (flt1 (mapcar 'cadr ord))
                       (counter -1)
                       (tempos '()))
                  (remove nil (loop for i in flt1
                                    collect  (if (and (listp i) 
                                                      (listp (car i)) 
                                                      (member 'tempochangeevent (car i))) 
                                                 (progn (incf counter)
                                                   (list counter (car i)))  
                                               (progn
                                                 (incf counter)
                                                 nil)
                                               )))))




(defun get-l-tempo (file)
  (let* ((head (caaar file))
         (init-tempo (if (equal head 'timesignaturemusic) 
                         (list (list 0 (list 'tempochangeevent 60 1/4))) ;;;if no tempo while using spec
                       (list (list 0 (caaar file)))))
         (other-tempi (filter-tmp-chng (all-grp-meas file))))
    (x-append init-tempo other-tempi)))


  ;;;;faire le tempo comme suit :
;;;if single :
;((1/4 60) nil)
;;;if changes:
;;((1/4 36) (((1 0) (1/4 40 nil)) ((4 0) (1/4 51 nil)) ((5 0) (1/4 60 nil)) ((6 0) (1/4 68 nil))))  
;;; et pour l'instant uniquement sur le changement de mesure!
;;;note : we can apparently skip the nils....


(defun format-om-temp (lst)
  "turns a lily scheme expression like : 
   (1 (tempochangeevent 40 1/4))
     into 
   ((1 0) (1/4 40))"
  (let ((rst (second lst)))
    (list (list (car lst) 0) (list (third rst) (second rst) nil))))  

        
(defun tempo-lily->rt (liste)
  (let* ((init-tempo (cadar liste))
         (first-tempo (list (third init-tempo) (second init-tempo)))
         (rest-tempo (cdr liste)))
    (if rest-tempo
        (let ((conv (loop for i in rest-tempo
                          collect (format-om-temp i))))
          (list first-tempo conv))
      (list first-tempo nil))))


(defun lily-tempo (file)
  (tempo-lily->rt (get-l-tempo file)))


;;;Lilypond doesn't support correctly polytempi
;;;so duplicate the first voice tempi changes to all parts!

#|
(defun get-lily-tempo (file)
  (let* ((stream (lily-chord-or-dur file t)))
    (loop for i in (car stream);fix
          collect (lily-tempo i))))
|#


(defun get-lily-tempo (file)
  (let* ((stream (car (lily-chord-or-dur file t)))
         (lgt (length stream))
         (tempo (lily-tempo (car stream))))
    (repeat-n tempo lgt)))

;;;;;;



(defmethod! lily->om (&optional (path nil))
            :icon 161
  ;:indoc '("path" )
  ;:initvals '(nil nil)
            :doc "Imports lilypond format to OM"
            (let ((file (catch-cancel (or path (om-choose-file-dialog)))))
              (when file
                (let* ((notes (get-lily-chords file))
                       (tree (get-lily-tree file))
                       (tempo (car (get-lily-tempo file)));;;needs to be fixed [indexing tempo measures]
                       (voices (loop for n in notes
                                     for tr in tree
                                     collect (make-instance 'voice
                                                            :tree tr
                                                            :chords n 
                                                            :tempo tempo))))
                  (make-instance 'poly 
                                 :voices voices)))
              ))