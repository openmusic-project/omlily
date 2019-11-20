;;;===================================================
;;; omlily
;;; lilypond and openmusic file interchange
;;;
;;; Preferences
;;; K . Haddad , IRCAM 2015
;;;===================================================

(in-package :om)


(defvar *LILYPOND-PATH* "path to Lilypond binary")

(pushr 'lilypond *external-prefs*)



(defmethod get-external-name ((module (eql 'lilypond))) "Lilypond")
(defmethod get-external-icon ((module (eql 'lilypond))) (and (find-library "omlily") (list 606 (find-library "omlily"))))


(defmethod get-external-module-path ((module (eql 'lilypond)) modulepref) (get-pref modulepref :lilypond-path))

(defmethod set-external-module-path ((module (eql 'lilypond)) modulepref path) 
  (set-pref modulepref :lilypond-path path))



(defmethod get-external-def-vals ((module (eql 'lilypond)))
    (cond
     ((equal *om-os* :linux) 
      (list :lilypond-path (pathname (find-lilypond)))
      )
     ((equal *om-os* :mac) 
      (list :lilypond-path (pathname (find-lilypond))
               )) 
     (t (list :lilypond-path (pathname "~/bin/lilypond")))))


(defmethod put-external-preferences ((module (eql 'lilypond)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :lilypond-options)))

    (when (get-pref moduleprefs :lilypond-path)
      (setf *LILYPOND-PATH* (find-true-external (get-pref moduleprefs :lilypond-path))))
    t))
      

(put-external-preferences 'lilypond (find-pref-module :externals))


;;;===========================
;;; PDF



(defvar *PDF-READER-PATH* "path to pdf reader")


(if (equal *om-os* :linux)
(pushr 'xpdf *external-prefs*))



(defmethod get-external-name ((module (eql 'xpdf))) "Pdf Reader")
(defmethod get-external-icon ((module (eql 'xpdf))) (and (find-library "omlily") (list 606 (find-library "omlily"))))


(defmethod get-external-module-path ((module (eql 'xpdf)) modulepref) (get-pref modulepref :pdf-path))

(defmethod set-external-module-path ((module (eql 'xpdf)) modulepref path) 
  (set-pref modulepref :pdf-path path))



(defmethod get-external-def-vals ((module (eql 'xpdf)))
  (cond
   ((equal *om-os* :linux) 
    (list :pdf-path (pathname "/usr/bin/xpdf"))
    )
   ((equal *om-os* :mac) 
    (list :pdf-path nil)
    ) 
   (t (list :pdf-path (pathname "/usr/bin/xpdf")))))


(defmethod put-external-preferences ((module (eql 'xpdf)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :pdf-options)))

    (when (get-pref moduleprefs :pdf-path)
      (setf *PDF-READER-PATH* (find-true-external (get-pref moduleprefs :pdf-path))))
    t))
      

(put-external-preferences 'xpdf (find-pref-module :externals))

;========================================================================================================================
;         PREFERENCES PANEL MODULE
;=========================================================================================================================


(defvar *lily-composer-name* nil)
(setf *lily-composer-name* "El Guapo")

(defvar *lily-title* nil)
(setf *lily-title* "Title")

(defvar *lily-clef* nil)
(setf *lily-clef* (format nil "(\"~D\")" "G"))

(defvar *split-mode* nil)

(defvar *split-note* "6000")
(setf *split-note* "6000")

(defvar *default-comp-mode* 0)

(defvar *lily-paper-other* (merge-pathnames (string+ "lily-templates/paper/" "paper" ".ly") (lib-resources-folder (find-library "omlily"))))
(defvar *lily-layout-file* (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") (lib-resources-folder (find-library "omlily"))))

(defvar *lily-dyn-on* nil)
(setf *lily-dyn-on* t)

(defvar *lily-chan-on* nil)
;(setf *lily-chan-on* t)

(defvar *lily-paper-size* nil)
(setf *lily-paper-size* "a4")

(defvar *lily-paper-orientation* nil)
(setf *lily-paper-orientation* "landscape")

(defvar *lily-staff-size* nil)
(setf *lily-staff-size* 18)

(defvar *midioutput* nil)



(defmethod get-def-vals ((iconID (eql :lilypond)))
   (list 
    :lily-title "Title"
    :user-name "El Guapo" 
    :lily-clef (format nil "(\"~D\")" "G")
    :split-mode nil
    :split-note "6000"
    :comp-mode 0
    :lily-dyn-on t
    :lily-chan-on nil
    :midioutput nil
    :paper-size "a4"
    :paper-orientation "portrait"
    :staff-size 18
    :paper-other (merge-pathnames (string+ "lily-templates/paper/" "paper" ".ly") (lib-resources-folder (find-library "omlily"))) 
    :layout-file (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") (lib-resources-folder (find-library "omlily")))
    ))


; (get-pref (find-pref-module :lilypond) :lily-clef)

(defmethod put-preferences ((iconID (eql :lilypond)))

  (let* ((modulepref (find-pref-module iconID))
         (quantparams (get-pref modulepref :quantify))
         )
    (setf *lily-title* (get-pref modulepref :lily-title))
    (setf *lily-composer-name* (get-pref modulepref :user-name))
    (setf *lily-clef* (get-pref modulepref :lily-clef))
    (setf *split-mode* (get-pref modulepref :split-mode))
    (setf *split-note* (get-pref modulepref :split-note))
    (setf *default-comp-mode*     (get-pref modulepref :comp-mode))
    (setf *lily-dyn-on* (get-pref modulepref :lily-dyn-on))
    (setf *lily-chan-on* (get-pref modulepref :lily-chan-on))
    (setf *midioutput* (get-pref modulepref :midioutput))
    (setf *lily-paper-size* (get-pref modulepref :paper-size))
    (setf *lily-paper-orientation* (get-pref modulepref :paper-orientation))
    (setf *lily-staff-size* (get-pref modulepref :staff-size))
    (setf *lily-paper-other* (get-pref modulepref :paper-other))
    (setf *lily-layout-file* (get-pref modulepref :layout-file))
    ))

(defmethod save-pref-module ((iconID (eql :lilypond)) item)
   (list iconID `(list 
                  :lily-title ,*lily-title*
                  :user-name ,*lily-composer-name* 
                  :lily-clef ,*lily-clef*
                  :split-mode ,*split-mode*
                  :split-note ,*split-note*
                  :comp-mode ,*default-comp-mode*
                  :lily-dyn-on ,*lily-dyn-on*
                  :lily-chan-on ,*lily-chan-on*
                  :midioutput ,*midioutput*
                  :paper-size ,*lily-paper-size*
                  :paper-orientation ,*lily-paper-orientation*
                  :staff-size ,*lily-staff-size*
                  :paper-other ,(om-save-pathname *lily-paper-other*)
                  :layout-file ,(om-save-pathname *lily-layout-file*)
                  ) *om-version*))



(defmethod make-new-pref-scroll  ((num (eql :lilypond)) modulepref)
  (let ((thescroll (om-make-view 'preference-pane
                                 :pref-id num
                                 :name "Lilypond"
                                 :size (get-pref-scroll-size)
                                 :position (om-make-point 0 0)
                                 :font *controls-font* 
                                ;:scrollbars :v 
                                ;:retain-scrollbars t
                                 :bg-color *om-light-gray-color*
                                 ))
        (l1 50)
	(l2 (round (om-point-h (get-pref-scroll-size)) 2))
	(l3 (- (om-point-h (get-pref-scroll-size)) 60))
        (i 40)
        (posy 0)
	(dy 40)
        outtxt tmptxt)
    
    (om-add-subviews thescroll 
                     ;(om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy 5)) (om-make-point 200 30) "General"
                     ;                      :font *om-default-font4b*)
                     

                     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy dy)) (om-make-point 90 24) "Title"
                                          :font *controls-font*) 

                     (om-make-dialog-item 'om-editable-text (om-make-point (+ l1 100) posy)
                                          (om-make-point 200 15)
                                          (get-pref modulepref :lily-title)
                                          :after-action (om-dialog-item-act item 
                                                          (set-pref modulepref :lily-title (om-dialog-item-text item)))
                                          :font *controls-font*
                                          )

                     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy dy)) (om-make-point 90 24) "Composer"
                                          :font *controls-font*) 

                     (om-make-dialog-item 'om-editable-text (om-make-point (+ l1 100) posy)
                                          (om-make-point 200 15)
                                          (get-pref modulepref :user-name)
                                          :after-action (om-dialog-item-act item 
                                                          (set-pref modulepref :user-name (om-dialog-item-text item)))
                                          :font *controls-font*
                                          )
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 80)) (om-make-point 120 20) "Options:"
                                          :font *controls-font*)
                     
                  
                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 40)) (om-make-point 120 20) "Clefs"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-editable-text (om-make-point 170 i)  (om-make-point 200 15) ;(om-make-point 37 13)
                                          (format nil "~D" (get-pref modulepref :lily-clef))
                                          :modify-action (om-dialog-item-act item
                                                           (let ((text (om-dialog-item-text item))
                                                                 number)
                                                             (unless (string= "" text)
                                                               (setf number (ignore-errors (read-from-string text)))
                                                               (when number 
                                                                 (set-pref modulepref :lily-clef (om-dialog-item-text item))
                                                                 ))))
                                          :font *om-default-font2*)

                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 40)) (om-make-point 120 20) "Clef Change"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-check-box (om-make-point 165 i) (om-make-point 30 10) ""
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :split-mode)
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :split-mode (om-checked-p item))))

                     (om-make-dialog-item 'om-editable-text (om-make-point 200 i)  (om-make-point 60 13)
                                          (format nil "~D" (get-pref modulepref :split-note))
                                          :modify-action (om-dialog-item-act item (set-pref modulepref :split-note (om-dialog-item-text item)))
                                          :font *om-default-font2*)

                     
                     (om-make-dialog-item 'om-static-text (om-make-point 50 (incf i 40)) (om-make-point 120 20) "Compilation Mode"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 165 (- i 5)) (om-make-point 90 10) ""
                                          :range '("generic" "polymetric")
                                          :value (if (= 0 *default-comp-mode*) "generic" "polymetric")
					  :di-action (om-dialog-item-act item 
                                                       (let ((choice (om-get-selected-item item)))
                                                         (set-pref modulepref :comp-mode
                                                                   (if (string-equal choice "generic") 0 1))))
					  :font *controls-font*)
                     
                      (om-make-dialog-item 'om-static-text (om-make-point 50 (incf i 40)) (om-make-point 120 20) "Export Dynamics"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-check-box (om-make-point 165 i) (om-make-point 30 10) ""
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :lily-dyn-on)
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :lily-dyn-on (om-checked-p item))))
                     
                     (om-make-dialog-item 'om-static-text (om-make-point (+ 50 165)  i) (om-make-point 120 20) "Export Midi"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-check-box (om-make-point (+ 165 125)  i) (om-make-point 30 10) ""
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :midioutput)
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :midioutput (om-checked-p item))))

                     (om-make-dialog-item 'om-static-text (om-make-point (+ 50 300)  i) (om-make-point 120 20) "Export Channels"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-check-box (om-make-point (+ 165 290)  i) (om-make-point 30 10) ""
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :lily-chan-on)
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :lily-chan-on (om-checked-p item))))
                     )

    (setf posy 0)
    
    (om-add-subviews thescroll
                    
                    ; (om-make-dialog-item 'om-static-text (om-make-point l2 (incf posy 50)) (om-make-point 200 30) "Template Files"
                    ;                      :font *om-default-font2b*)

                     (om-make-dialog-item 'om-static-text (om-make-point l2 (incf posy dy)) (om-make-point 80 22) "Paper Size"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l2 75) (- posy 5)) (om-make-point 80 22) ""
                                          :range '("a4" "a3")
                                          :value (if (equal "a4"  *lily-paper-size*) "a4" "a3")
					  :di-action (om-dialog-item-act item 
                                                       (let ((choice (om-get-selected-item item)))
                                                         (set-pref modulepref :paper-size
                                                                   (if (string-equal choice "a4") "a4" "a3"))))
					  :font *controls-font*)


                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 200) posy) (om-make-point 80 22) "Staff Size"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-editable-text (om-make-point (+ l2 280) posy)  (om-make-point 80 22)
                                          (format nil "~D" (get-pref modulepref :staff-size))
                                          :modify-action (om-dialog-item-act item (set-pref modulepref :staff-size (om-dialog-item-text item)))
                                          :font *om-default-font2*)


                     (om-make-dialog-item 'om-static-text (om-make-point l2 (incf posy dy)) (om-make-point 80 22) "Orientation"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l2 75) (- posy 5)) (om-make-point 100 22) ""
                                          :range '("portrait" "landscape")
                                          :value (if (equal "portrait"  *lily-paper-orientation*) "portrait" "landscape")
					  :di-action (om-dialog-item-act item 
                                                       (let ((choice (om-get-selected-item item)))
                                                         (set-pref modulepref :paper-orientation
                                                                   (if (string-equal choice "portrait") "portrait" "landscape"))))
					  :font *controls-font*)
                     

                     (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 40)) (om-make-point 80 22) "Paper:" :font *controls-font*)
                     (setf outtxt (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 25)) (om-make-point 320 45)
                                                       (om-namestring (get-pref modulepref :paper-other))
                                          :font *om-default-font1*))
                     
                     (om-make-view 'om-icon-button 
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om-make-point l3 (- posy 5)) :size (om-make-point 26 25) 
                                   :action (om-dialog-item-act item
                                             (declare (ignore item))
                                             (let ((file (om-choose-file-dialog)))
                                               (when file
                                                 (om-set-dialog-item-text outtxt (om-namestring file))
                                                                             (set-pref modulepref :paper-other file)))))
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 40)) (om-make-point 80 22) "Layout:" :font *controls-font*)
                     (setf tmptxt (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 25)) (om-make-point 320 45)
                                                       (om-namestring (get-pref modulepref :layout-file))
                                          :font *om-default-font1*))
                     
                     (om-make-view 'om-icon-button 
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om-make-point l3 (- posy 5)) :size (om-make-point 26 25) 
                                   :action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (let ((file (om-choose-file-dialog)))
                                                         (when file
                                                           (om-set-dialog-item-text tmptxt (om-namestring file))
                                                          (set-pref modulepref :layout-file file)))))
                     )
          
    thescroll))




;set and load tab in om preferences panel 
(pushr :lilypond *pref-order*)
(push-pref-module (list :lilypond (get-def-vals :lilypond)))



