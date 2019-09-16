;;; ====================================================================
;;;
;;;      OM2Lily 1.0 v1.1
;;;
;;;      © 2005 IRCAM - Karim Haddad, Jean Bresson & Carlos Agon
;;;
;;; ====================================================================

;;;;la c;est la version generique

(in-package :om)



;(defun massq (item list)
;(format nil "~S" (cdr (assoc item list :test 'equal))))  

(defun massq (item list)
(cdr (assoc item list :test 'equal)))

(setf *vel-for-lil*
      '(("h" . "\\fff") ("g" . "\\ff")("f" . "\\f") ("F" . "\\mf")
      ("P" . "\\mp") ("p" . "\\p")("Q" . "\\pp") ("R" . "\\ppp")))




;(format nil "~S" (cdr (assoc "h" *vel-for-lil* :test 'equal)))
;(massq  "h" *vel-for-lil*)






(defmethod cons-lil-expr-extra ((self om::poly) listclefs)
  (let ((rep (list " ")) 
        (voices (om::inside self)))
    (setf *voice-num* 0)
    (loop for staff in voices
          for i = 0 then (+ i 1) 
          do
          (progn 
          (setf *voice-num* (incf *voice-num*))
          (setf rep (append rep (cons-lil-expr-extra staff (nth i listclefs))))))
    ;(setf rep (append rep (list ">>")))

      (setf rep (append rep (list (format nil "\\score { ~% { ~%~% <<"))))
      (setf rep (append rep (list (format nil "\\new StaffGroup~% << ~%~%"))))


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

(setf rep (append rep (list (format nil " >> ~% >> ~%} ~% ~%"))))
rep))





(defmethod cons-lil-expr-extra ((self om::voice) clef)
  (setf *mesure-num* 0)
  (let ((rep (list (format nil "~s=" (cassq *voice-num* *voice-rank* )) "{" 
                   (format nil "\%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
                   (format nil "\%%%%%%%%%%%%%%%%%%%%%%% VOICE : ~d %%%%%%%%%%%%%%%%%%%%%%%%" *voice-num*)
                   (format nil "\%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
                   (if (= 1 *voice-num*) 
                     (format nil "~%\\tempo 4=~D ~%" (round (tempo-a-la-noire (car (tempo self)))))
                     (format nil ""))
                   (format nil "~%#(set-accidental-style 'dodecaphonic)~%")
                   ;(format nil  "\\set tupletNumberFormatFunction = #fraction-tuplet-formatter")
                   ))
        (mesures (om::inside self))
        (lastmes nil))
    (when clef
      (setf rep (append rep (list (format nil "\\clef ~s" clef)))))
    (loop for mes in mesures
          for i = 1 then (+ i 1) do
          
          
          (setf *mesure-num* (incf *mesure-num*))
          (setf rep (append rep (list (format nil  "\%%%%%%%%%%%%%%%%%%%%%%% MESURE : ~d %%%%%%%%%%%%%%%%%%%%%%%" *mesure-num*))))
          (setf rep (append rep (cons-lil-expr-extra mes lastmes)))
          (setf lastmes mes))
    (setf rep (append rep (list (format nil"} ~% ~%"))))
    rep))




;symb-beat-val= For a key signature equivalent to 3//3 will be the half note (blanche)
;real-beat-val= For the same key sign, this will be the halfnote of a triplet (blanche de triolet)
;These refer to the beats in a measure, and for special cases using non-standard key signature

(defmethod cons-lil-expr-extra ((self om::measure) lastmes)
  (setf *chords-and-cont* (collect-chords  self))
  (setf *treeratios* (get-note-figure self))
  (setf *switch* nil)
  (let* ((inside (om::inside self))
         (tree (om::tree self))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (rep nil))
    ;(setf *mesure-num* (incf *mesure-num*))
    ;(setf rep (list (format nil  "\%%%%%%%%%%%%%%%%%%%%%%% MESURE : ~d %%%%%%%%%%%%%%%%%%%%%%%" *mesure-num*)))
    
    (unless (and lastmes (equal (first tree) (first (tree lastmes))))
      ;(setf *mesure-num* (incf *mesure-num*))
      ;(setf rep (list (format nil  "\%%%%%%%%%%%%%%%%%%%%%%% MESURE : ~d %%%%%%%%%%%%%%%%%%%%%%%" *mesure-num*)))
      (setf rep (list (format nil  "\\time ~d/~d" (om::fnumerator (first tree)) (om::fdenominator (first tree)))))
      )
    (loop for obj in inside do
          (setf rep (append rep 
                            (let* ((dur-obj-noire (/ (om::extent obj) (om::qvalue obj)))
                                   (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
                                   (exp (cons-lil-expr-extra obj (* symb-beat-val factor))))
                              exp
                              )
                            )))
    rep))



(defmethod cons-lil-expr-extra ((self om::group) dur)
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
    ;(print durtot)
    (cond
     ((not (om::get-group-ratio self)) 
      (loop for obj in inside
            do (setf rep (append rep (let* ((dur-obj (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                        (/ (om::extent self) (om::qvalue self)))))
                                       (cons-lil-expr-extra obj (* dur-obj durtot)))))))
     ((= sympli 1)
      (loop for obj in inside
            do (setf rep (append rep (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                          (/ (om::extent self) (om::qvalue self))))
                                            (dur-obj (numerator (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                                   (/ (om::extent self) (om::qvalue self))))))
                                       (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                       (cons-lil-expr-extra obj (* dur-obj unite)))))))
     
     (t
      (let ((pos (length rep))
            (depth 0))
        (setf rep (append rep (list (format nil "\\once \\override TupletBracket #'padding = #"))))
        (setf rep (append rep (list (format nil "\\times ~d/~d {" denom num))))
        (loop for obj in inside do
              (setf rep (append rep (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                         (/ (om::extent self) (om::qvalue self))))
                                           (dur-obj (numerator operation))
                                           exp tmp)
                                      (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                      (setf tmp (multiple-value-list 
                                                 (cons-lil-expr-extra obj (list (* dur-obj unite) cpt))))
                                      (setf exp (car tmp))
                                      (when (and (cadr tmp) (> (cadr tmp) depth))
                                        (setf depth (cadr tmp)))
                                      exp
                                      ))))
        (setf val (+ depth 1))
        (if (= depth 0)
          (setf (nth pos rep) "")
          (setf (nth pos rep) (string+ (nth pos rep) (format nil "~D" (float (* 3 depth)))))
          )
        (setf rep (append rep (list "}")))
        )
      ))
    (values rep val)))



(defun get-extra-text (liste)
  (remove 'nil 
  (loop for i in liste
        collect (if (text-extra-p i) (thetext i)))))


(defun get-extra-vel (liste)
  (remove 'nil 
  (loop for i in liste
        collect (if (vel-extra-p i) (thechar i)))))


(defmethod cons-lil-expr-extra ((self om::chord) dur)
  (let* ((notes (inside self))
        (extra (car (mapcar #'extra-obj-list notes)))
        (text (if (text-extra-p (car extra))
                  (thetext (car extra))))
        ;(text (if (text-extra-p (car extra))
        ;          (car (mapcar #'thetext extra))))
        (vel (if (vel-extra-p (car extra))
                  (thechar (car extra))))
        (durtot (if (listp dur) (car dur) dur))
        (inside (om::inside self))
        (str "")) ;(print (list "eeee" extra vel)) 
    (if (= (length inside) 1)
      (setf str (cons-lily-note (car inside)))
      (let ((notes ""))
        (loop for note in inside do (setf notes (string+ notes " " (cons-lily-note note)))) 
        (setf str (string+ "<" notes ">")))
      )
    (let* ((durconv (get-head-and-points durtot))
           (head (first durconv))
           (points (if (< 0 (second durconv))
                     (append-str (om::repeat-n "." (second durconv))))))
      
      (setf str (string+ str 
                         (if (not points)
                           (format nil "~d" head)
                           (format nil "~d~A" head points)
                           ))))
    (when (or (and (not (om::cont-chord-p self))
                   (om::cont-chord-p (om::next-container self '(om::chord))))
              (and (om::cont-chord-p self)
                   (om::cont-chord-p (om::next-container self '(om::chord))))
              )
      (setf str (string+ str "~"))
      )


    (if vel 
        (setf str (string+ str  (massq vel *vel-for-lil*)))
      )

    (if text     
        (setf str (string+ str  (format nil " ^\\markup{ ~S}" text)))
      )
    
    (list str)
    ))



                      
(defmethod cons-lil-expr-extra ((self om::rest) dur)
(let* ((durtot (if (listp dur) (car dur) dur))
       (durconv (get-head-and-points durtot))
           (head (first durconv))
           (points (if (< 0 (second durconv))
                     (append-str (om::repeat-n "." (second durconv))))))
  

                     (if (not points)
                       (list (format nil "r~d" head))
                       (list (format nil "r~d~A" head points))
                       )))




;(defmethod cons-lily-note ((self om::note))
;  (car (mc->lilynotes (list (midic self)))))



;;;;;;;;;;;;;;;;;;;;i/o and interface

;;;peut-etre ajouter l'encoding du text : unix
;;Attention:
;;Lily genere des erreur dues aux commentaires
;;;une fois que l'on les encodes en unix tout marche



(defun write-lil-file-ex (list path paper layout)
  (let ((pathname (or path (om-choose-new-file-dialog)))
        ;(pathname1 "CL:userlibrary;karim;resources;lily-templates;sizes;a3land.ly")
        ;(pathname2 "CL:userlibrary;karim;resources;lily-templates;layouts;template.ly")
        )
    (setf *voice-num* 0)
    (setf *mesure-num* 0)
    (WITH-OPEN-FILE (out pathname :direction :output  :if-does-not-exist :create :if-exists :supersede)
      ;;;HERE COMES THE HEADER and GENERAL SETTINGS
      (format out "\\version \"2.14\"~%~%")
      
      ;;;;page sizes
      (WITH-OPEN-FILE (in paper :direction :input)
        (loop 
         while (not (file-eof-p in))
         do (let ((line (read-line in)))
              (write-line  line out))))
      ;;;;The music
     ;; (format out "\\score { ~% { ~%~%")
     ;; (format out "\\new StaffGroup~%")
      ;;;;;
      (loop for elt in list do
            (format out "~A~%" elt))

      
      ;;(format out "}~%~%~%")
     
      
      ;;;;layout templates
      (WITH-OPEN-FILE (in layout :direction :input)
        (loop 
         while (not (file-eof-p in))
         do (let ((line (read-line in)))
              (write-line  line out))))
      (format out "}~%~%~%")

 )
    pathname))




#|
(defun run-lilypond (path)
  (let* ((folder (pathname-directory path))
         (folderpath (make-pathname :directory folder))
         (outfile (make-pathname :directory folder :name (pathname-name path)))
         (pdffile (make-pathname :directory folder :name (pathname-name path) :type "pdf")))


(if (equal *om-os* :mac)

       (progn
         
         (om-cmd-line "open -a console")
         (om-cmd-line (format nil "cd ~s; /usr/local/bin/lilypond ~s  >/dev/console" 
                              (om-path2cmdpath folderpath) 
                              (om-path2cmdpath path)))
         (om-cmd-line (format nil "open ~s" (om-path2cmdpath  pdffile)))
         pdffile) outfile)))
      
|#


(defmethod! om->lily-extras ((self poly) &optional 
                             (clef nil)
                             (paper "a3landmarg")
                             (layout "template")
                             (path nil))
            :icon 161
            :indoc '("self" "clef" "paper" "layout" "path" )
            :initvals '(t ("G") "a3landmarg" "template" t)
            :menuins '((2 (("A3 Landscape" "a3landmarg" )
                           ))
                       (3 (("template" "template")
                           )))
            :doc "Exports voice, poly,chordseq to lilypond format"
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile (merge-pathnames (string+ "lily-templates/sizes/" paper ".ly") ressource-folder))
                   (pathname (or path (om-choose-new-file-dialog)))
                   (layoutfile
                    (merge-pathnames (string+ "lily-templates/layouts/" layout ".ly") ressource-folder))

                   (lilyfile (write-lil-file-ex (cons-lil-expr-extra self clef) pathname paperfile layoutfile)))
              (run-lilypond lilyfile)))


(defmethod! om->lily-extras ((self voice) &optional
                             (clef nil)
                             (paper "a3landmarg")
                             (layout "template")
                             (path nil))
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile (merge-pathnames (string+ "lily-templates/sizes/" paper ".ly") ressource-folder))
                   (pathname (or path (om-choose-new-file-dialog)))
                   (layoutfile
                    (merge-pathnames (string+ "lily-templates/layouts/" layout ".ly") ressource-folder))

                   (lilyfile (write-lil-file-ex 
                              (cons-lil-expr-extra (make-instance 'poly
                                                                  :voices self) clef ) pathname paperfile layoutfile)))
              (run-lilypond lilyfile)))




;;;;;;;;
