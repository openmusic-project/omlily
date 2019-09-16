(in-package :om)

;;;;2015



(defmethod* n-grouplst ((tree list) &optional (n-elem 1))
   :initvals (list '(1 2) 1)
   :indoc '("tree" "nth-elem")
   :icon 235
   :doc "transform a tree or a list into a list of lists of <n-elem> 
elements."
   (let* ((lst '())
          (liste tree)
          )
     (do
       ((n 0 (+ n n-elem))
        (lst '() (push (first-n (nthcdr n liste) n-elem) lst)))
       ((>= n (length liste)) (reverse lst)))))




(defmethod* chordseq-spliter ((self chord-seq))
  "splits a chrdseq into two high low accroding
to c4 and returns a multiseq"

  (let* ((chords (inside self))
         (onsets (lonset self))
         lst1 lst2 ons1 ons2)
    (loop for i in chords
          for on in onsets
          do (let* ((notes (inside i))
                    (toto (progn
                            (push '() lst1)
                            (push '() lst2))))
               (loop for n in notes
                     do 
                     (if  (>= (midic n) 6000) ;;;here put a param for chord splitting note (origin 6000)
                       (progn
                         (push (midic n) (car lst1))
                         (if (not (equal (car ons1) on)) 
                           (push on ons1)))
                       (progn 
                         (push (midic n) (car lst2))
                         (if (not (equal (car ons2) on)) 
                           (push on ons2)))))))



      (push (last-elem onsets) ons2)
      (push (last-elem onsets) ons1)


    
    (list 
     (make-instance 'chord-seq 
       :lmidic (reverse (remove nil lst1))
       :lonset (reverse ons1)
       )
     (make-instance 'chord-seq 
       :lmidic (reverse (remove nil lst2))
       :lonset (reverse ons2)
       )
       )))


(defun chordseq-high (self beg end)
  (let* ((highone (car self))
         (lowone (second self))
         (highons 
          (if (not (null (lmidic highone)))
          (lonset highone)))
         (lowons (lonset lowone))
         (communs (x-intersect highons lowons))
         (pitch (mc->lilynotes (lmidic highone)))
         (strpitch 
          (let ((notes ""))
            (loop for acc in pitch collect
                  (if (= 1 (length acc))
                    (string+ (car acc) "4")
                    (let ((str ""))
                      (loop for i in acc
                            do (setf str (string+ str i " ")))
                      (setf str (string+ "<" str ">4")))))))
         (dur (om/ (x->dx (lonset highone)) 1000)))


(let ((res 
           (if (not (= 0 beg))
             ;(format nil "\\hideNotes~%c4*~D~%\\unHideNotes~%" (/ beg 1000));;pour l'offset
             (format nil "s4*~D~%~%" (/ beg 1000));;pour l'offset
             "")))
      (loop for pt in strpitch
            for dr in dur
            for on in highons
            do 
            (if (equal (car communs)  on)
              (progn  
                (pop communs)
                (setf res (string+ res (format nil "\\stemUp ~%") pt "*" (format nil "~D" dr) (format nil "\\stemNeutral ~%"))))
              (setf res (string+ res pt "*" (format nil "~D" dr) (format nil "~%")))
              
              
              ))
(if (not (= 0 end))
          (progn
            ;(setf res (string+ res (format nil "\\hideNotes~%c4*~D~%\\unHideNotes~%" (/ end 1000))))
            (setf res (string+ res (format nil "s4*~D~%~%" (/ end 1000))))
            res)
        res)
    
    )

))


(defun chordseq-low (self beg end)
  (let* ((highone (car self))
         (lowone (second self))
         (highons 
          (if (not (null (lmidic highone)))
          (lonset highone)))
         (lowons (lonset lowone))
         (communs (x-intersect highons lowons))
         (pitch (mc->lilynotes (lmidic lowone)))
         (strpitch 
          (let ((notes ""))
            (loop for acc in pitch collect
                  (if (= 1 (length acc))
                    (string+ (car acc) "4")
                    (let ((str ""))
                      (loop for i in acc
                            do (setf str (string+ str i " ")))
                      (setf str (string+ "<" str ">4")))))))
         (dur (om/ (x->dx (lonset lowone)) 1000)))
        (let ((res 
           (if (not (= 0 beg))
             ;(format nil "\\hideNotes~%c4*~D~%\\unHideNotes~%" (/ beg 1000));;pour l'offset
             (format nil "s4*~D~%~%" (/ beg 1000));;pour l'offset
             "")))
      (loop for pt in strpitch
            for dr in dur
            for on in lowons
            do 
            (if (equal (car communs)  on)
              (progn  
                (pop communs)
                (setf res (string+ res (format nil "\\stemUp ~%") pt "*" (format nil "~D" dr) (format nil "\\stemNeutral ~%"))))
              (setf res (string+ res pt "*" (format nil "~D" dr) (format nil "~%")))
              
              
              ))
(if (not (= 0 end))
          (progn
            ;(setf res (string+ res (format nil "\\hideNotes~%c4*~D~%\\unHideNotes~%" (/ end 1000))))
            (setf res (string+ res (format nil "s4*~D~%~%" (/ end 1000))))
            res)
        res)
    
    ))
     )

(defmethod* staff-data ((self chord-seq))

(let* ((things  (chordseq-spliter self))
       (durs (loop for i in things 
                   collect (get-obj-dur i)))
       (max (list-max durs))
       (offsets (loop for i in things
                      collect (car (lonset i))))
       (end-diffs (om-abs (om- durs max)))
       (up (chordseq-high things (car offsets) (car end-diffs)))
       (down (chordseq-low things (second offsets) (second end-diffs)))
       )
 (list up down)

  ))



(defmethod* staff-data ((self multi-seq))

            (let* ((things (flat (loop for i in (inside self)
                                       collect (chordseq-spliter i))))
                   (twins (n-grouplst things 2))
                   (durs (loop for i in things 
                               collect (get-obj-dur i)))
                   (max (list-max durs))
                   (offsets (n-grouplst
                             (loop for i in things
                                   collect (car (lonset i))) 2 ))
                   (end-diffs (n-grouplst (om-abs (om- durs max)) 2 ))
                   (chrdseqs (loop 
                              for i in twins
                              for off in offsets
                              for end in end-diffs
                              collect (let* ((up (chordseq-high i (car off) (car end)))
                                             (down (chordseq-low i (second off) (second end))))
                                        (list up down)))))
              chrdseqs
              
              ))



(defun chrdseqlil (list out) 
        
        (format out "\\new PianoStaff~%")
        (format out "<<~%")
        (format out "\\new Staff~%")
        (format out "{ \\time 1/32 ~%")
        (if (null (first list))
            (format out "s32")
            (format out "~A" (first list))
            )
        (format out "}~%")
        (format out "\\new Staff~%")
       ; (format out "\\with {\\override VerticalAxisGroup #'keep-fixed-while-stretching = ##t }~%")
        (format out "{ \\clef bass ~%")
         (format out " \\crossStaff { ~%")
        (format out "~A" (second list))
        (format out "  }~%")
        (format out "}~%")
        (format out ">>~%")
        )




(defun write-crdseq-lily-file (list path)
  (let ((pathname (or path (om-choose-new-file-dialog))))
    (WITH-OPEN-FILE (out pathname :direction :output  :if-does-not-exist :create :if-exists :supersede)
      ;;;HERE COMES THE HEADER and GENERAL SETTINGS
      (format out "\\version \"2.18\"~%~%")
      (format out "#(set-default-paper-size \"a4\" 'portrait)~%~%") ;;paper size ,portrait, landscape,a4,a3,etc...
      (format out "#(set-global-staff-size 18)~%~%") ;;;this is the main size of display 
      ;;;available sizes are: 11,13,14,16,18,20,23,26
      (format out "
\\paper {
     system-separator-markup = \\slashSeparator
    #(define after-title-space (* 0.5 cm))
    #(define head-separation (* 0.5 cm))
    print-page-number = ##t
   %% top-margin = 2\\cm
   bottom-margin = 3\\cm
%%%%%these come together:%%%%
%	left-margin = 20\\mm
%    line-width = 380\\mm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
  
}~%~%"
              )
      
      
     ;; (chrdseqlil list out)

      (format out "\\layout {~%")
      (format out "indent = 0.0~%") ;;;pour eliminer l'indentation du premier systeme
      ;; (format out "raggedlast = ##t~%") ;;;pour que la derniere mesure ne prenne pas toute 
      ;;;la page ---mais mieux sans pour les grandes mesures
      (format out "\\context {\\PianoStaff ~%")
      (format out "\\consists #Span_stem_engraver~%}~%")
      (format out "\\context {\\Score ~%")
      (format out "barAlways = ##t~%")
      (format out "\\override TimeSignature #'transparent = ##t~%")
      (format out "\\override BarLine #'transparent = ##t~%") 
      (format out "\\override BarLine #'allow-span-bar = ##f~%") 
      (format out "\\override BarNumber.transparent = ##t~%") 
      (format out "\\override SpacingSpanner #'strict-note-spacing = ##t~%")
      (format out "proportionalNotationDuration = #(ly:make-moment 1 32)~%")

      (format out "defaultBarType = \"\"}~%")
      
      (format out "\\context {\\Staff ~%")
      (format out "\\override VerticalAxisGroup #'minimum-Y-extent = #'(-3 . 3)~%")
      (format out "\\override Stem #'cross-staff = ##t~%")
      
      ;(format out "minimumVerticalExtent = #'(-6 . 6)~%")
      (format out "\\override TimeSignature #'break-visibility = #all-invisible~%")
      (format out "\\override TimeSignature #'style = #'()~%")
      (format out "\\override NoteHead #'style = #'baroque~%}~%")
      ;(format out "}~%~%~%")
      
      (format out "\\context {\\Voice ~%")
      (format out "\\remove \"Forbid_line_break_engraver\"~%}~%")
      (format out "}~%~%~%")
      ;;;;;

     ; (format out "stemExtend = \\once \\override Stem #'length = #26~%")
     ; (format out "noFlag = \\once \\override Stem #'flag-style = #'no-flag~%")

    (chrdseqlil list out)
      )
    pathname))




(defun write-multiseq-lily-file (list path)
  (let ((pathname (or path (om-choose-new-file-dialog))))
    (WITH-OPEN-FILE (out pathname :direction :output  :if-does-not-exist :create :if-exists :supersede)
      ;;;HERE COMES THE HEADER and GENERAL SETTINGS
      (format out "\\version \"2.18\"~%~%")
      (format out "#(set-default-paper-size \"a4\" 'portrait)~%~%") ;;paper size ,portrait, landscape,a4,a3,etc...
      (format out "#(set-global-staff-size 18)~%~%") ;;;this is the main size of display 
      ;;;available sizes are: 11,13,14,16,18,20,23,26
      (format out "
\\paper {
     system-separator-markup = \\slashSeparator
    #(define after-title-space (* 0.5 cm))
    #(define head-separation (* 0.5 cm))
    print-page-number = ##t
   %% top-margin = 2\\cm
   bottom-margin = 3\\cm
%%%%%these come together:%%%%
%	left-margin = 20\\mm
%    line-width = 380\\mm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

}~%~%"
              )
      
      
      (format out "\\layout {~%")
      (format out "indent = 0.0~%") ;;;pour eliminer l'indentation du premier systeme
      ;; (format out "raggedlast = ##t~%") ;;;pour que la derniere mesure ne prenne pas toute 
      ;;;la page ---mais mieux sans pour les grandes mesures

      (format out "\\context {\\Score ~%")
      (format out "barAlways = ##t~%")
      (format out "\\override TimeSignature #'transparent = ##t~%")
      (format out "\\override BarLine #'transparent = ##t~%") 
      (format out "\\override BarLine #'allow-span-bar = ##f~%") 
      (format out "\\override BarNumber.transparent = ##t~%") 
      
      
      (format out "defaultBarType = \"\"}~%")
      


      (format out "\\context {\\Staff ~%")
      (format out "\\override VerticalAxisGroup #'minimum-Y-extent = #'(-3 . 3)~%")
      (format out "\\override Stem #'cross-staff = ##t~%")
      (format out "\\override TimeSignature #'style = #'()~%")
      (format out "\\override NoteHead #'style = #'baroque~%")
      (format out "\\override TimeSignature #'break-visibility = #all-invisible ~%}~%")
      ;(format out "}~%~%~%")
      (format out "\\context {\\Voice ~%")
      (format out "\\remove \"Forbid_line_break_engraver\"~%}~%")
      (format out "}~%~%~%")
      (format out "stemExtend = \\once \\override Stem #'length = #26~%")
      (format out "noFlag = \\once \\override Stem #'flag-style = #'no-flag~%")
      
      
      ;;;;;
      
      (format out "\\new StaffGroup~%")
      (format out "<<~%")

      (loop for i in list
            do (chrdseqlil i out))
      (format out ">>~%")
      )
    pathname
    ))





(defmethod! om->lily ((self chord-seq) 
                             (clef list)
                             (switch number)
                             (mode symbol)
                             &optional
                             (paper "a3landmarg")
                             (layout "template")
                             (path nil))
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile (merge-pathnames (string+ "lily-templates/sizes/" paper ".ly") ressource-folder))
                   (layoutfile (merge-pathnames (string+ "lily-templates/layouts/" layout ".ly") ressource-folder))
                   (pathname (or path (om-choose-new-file-dialog)))
                   (lilyfile (write-crdseq-lily-file (staff-data self) pathname))
                   )
              (run-lilypond lilyfile)))


(defmethod! om->lily ((self multi-seq) 
                             (clef list)
                             (switch number)
                             (mode symbol)
                             &optional
                             (paper "a3landmarg")
                             (layout "template")
                             (path nil))
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile (merge-pathnames (string+ "lily-templates/sizes/" paper ".ly") ressource-folder))
                   (layoutfile (merge-pathnames (string+ "lily-templates/layouts/" layout ".ly") ressource-folder))
                   (pathname (or path (om-choose-new-file-dialog)))
                   (lilyfile (write-multiseq-lily-file 
                              (staff-data self)
                              pathname))
                   )
              (run-lilypond lilyfile)))
