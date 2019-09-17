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

;=================================================
;PREFERENCES PANEL MODULE
;=================================================

(defvar *composer-name* nil)
(setf *composer-name* "El Guapo")
(defvar *omlily-def-params* '(("G") 6000 gen))

(defvar *default-comp-mode* 0)

(defun set-quantipar (module param value)
  (let ((list (get-pref module :quantify)))
    (setf (nth param list) value)
    (set-pref module :quantify list)))

(defun get-quantipar (module param)
  (nth param (get-pref module :quantify)))


(defmethod get-def-vals ((iconID (eql :lilypond)))
   (list 
    :user-name "El Guapo" 
    :quantify '(("G") 6000 gen)
    :comp-mode 0
    :out-files-dir (check-folder (merge-pathnames (make-pathname :directory '(:relative "out-files")) (mypathname *current-workspace*)))
    :tmp-files-dir (check-folder (merge-pathnames (make-pathname :directory '(:relative "out-files")) (mypathname *current-workspace*)))
    :in-files-dir (check-folder (merge-pathnames (make-pathname :directory '(:relative "in-files")) (mypathname *current-workspace*)))
    ))


; (get-pref (find-pref-module :lilypond) :quantify)

(defmethod put-preferences ((iconID (eql :lilypond)))

  (let* ((modulepref (find-pref-module iconID))
         (quantparams (get-pref modulepref :quantify))
         )
    
    (setf *composer-name* (get-pref modulepref :user-name))
    ;(set-pref modulepref :quantify quantparams)
    (setf *omlily-def-params* (get-pref modulepref :quantify))

    (setf *default-comp-mode*     (get-pref modulepref :comp-mode))

    (if (probe-file (get-pref modulepref :out-files-dir))
        (setf *om-outfiles-folder* (get-pref modulepref :out-files-dir))
      (push :out-files-dir *restore-defaults*))

    (if (probe-file (get-pref modulepref :tmp-files-dir))
        (setf *om-tmpfiles-folder* (get-pref modulepref :tmp-files-dir))
      (push :tmp-files-dir *restore-defaults*))

    (if (probe-file (get-pref modulepref :in-files-dir))
        (setf *om-infiles-folder* (get-pref modulepref :in-files-dir))
      (push :tmp-files-dir *restore-defaults*))
     
    ))

(defmethod save-pref-module ((iconID (eql :lilypond)) item)
   (list iconID `(list 
                       :user-name ,*composer-name* 
                       
                       :out-files-dir ,(om-save-pathname *om-outfiles-folder*)
                       :tmp-files-dir ,(om-save-pathname *om-tmpfiles-folder*)
                       :in-files-dir ,(om-save-pathname *om-infiles-folder*)
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
        (l1 20)
	(l2 (round (om-point-h (get-pref-scroll-size)) 2))
	(l3 (- (om-point-h (get-pref-scroll-size)) 60))
        (i 40)
        (posy 0)
	(dy 30)
        outtxt tmptxt intxt)
    
    (om-add-subviews thescroll 
                     ;(om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy 5)) (om-make-point 200 30) "General"
                     ;                      :font *om-default-font4b*)
                     

                     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy dy)) (om-make-point 90 24) "User Name"
                                          :font *controls-font*) 

                     (om-make-dialog-item 'om-editable-text (om-make-point (+ l1 100) posy)
                                          (om-make-point 200 15)
                                          (get-pref modulepref :user-name)
                                          :after-action (om-dialog-item-act item 
                                                          (set-pref modulepref :user-name (om-dialog-item-text item)))
                                          :font *controls-font*
                                          )
                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 40)) (om-make-point 120 20) "Options:"
                                          :font *controls-font*)
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point 20 (incf i 20)) (om-make-point 350 20) " "
                                          :font *om-default-font1*)
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 30)) (om-make-point 120 20) "Clef(s)"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-editable-text (om-make-point 170 i)  (om-make-point 37 13)
                                          (format nil "~D" (get-quantipar modulepref 0)) 
                                          :modify-action (om-dialog-item-act item
                                                           (let ((text (om-dialog-item-text item))
                                                                 number)
                                                             (unless (string= "" text)
                                                               (setf number (ignore-errors (read-from-string text)))
                                                               (when number 
                                                                 (set-quantipar modulepref 0 number)
                                                                 ))))
                                          :font *om-default-font2*)

                     
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 30)) (om-make-point 120 20) "Clef Change"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-editable-text (om-make-point 170 i)  (om-make-point 37 13)
                                          (format nil "~D" (get-quantipar modulepref 1)) 
                                          :modify-action (om-dialog-item-act item
                                                           (let ((text (om-dialog-item-text item))
                                                                 number)
                                                             (unless (string= "" text)
                                                               (setf number (ignore-errors (read-from-string text)))
                                                               (if number
                                                                   (set-quantipar modulepref 1 number)
                                                                 ))))
                                          :font *om-default-font2*)
                     #|    
                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 30)) (om-make-point 120 20) "Clef Change"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-editable-text (om-make-point 170 i) (om-make-point 37 13)
                                          (format nil "~D" (get-quantipar modulepref 2)) 
                                          :modify-action (om-dialog-item-act item
                                                           (let ((text (om-dialog-item-text item))
                                                                 number)
                                                             (unless (string= "" text)
                                                               (setf number (ignore-errors (read-from-string text)))
                                                               (if number
                                                                   (set-quantipar modulepref 2 number)
                                                                 ))))
                                          :font *om-default-font2*)
                     |#
                     
                     (om-make-dialog-item 'om-static-text (om-make-point 50 (incf i 30)) (om-make-point 120 20) "Compilation Mode"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 160 i) (om-make-point 90 10) ""
                                          :range '("generic" "polymetric")
                                          :value (if (= 0 *default-comp-mode*) "generic" "polymetric")
					  :di-action (om-dialog-item-act item 
                                                       (let ((choice (om-get-selected-item item)))
                                                         (set-pref modulepref :comp-mode
                                                                   (if (string-equal choice "generic") 0 1))))
					  :font *controls-font*)
       



                     
                     #|
                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 30)) (om-make-point 120 20) "Max. Division"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-editable-text (om-make-point 150 i) (om-make-point 37 13)
                                          (format nil "~D" (get-quantipar modulepref 2)) 
                                          :modify-action (om-dialog-item-act item
                                                           (let ((text (om-dialog-item-text item))
                                                                 number)
                                                             (unless (string= "" text)
                                                               (setf number (ignore-errors (read-from-string text)))
                                                               (if number
                                                                   (set-quantipar modulepref 2 number)
                                                                 ))))
                                          :font *om-default-font2*)
                     |#
                     )

    (setf posy 0)
    
    (om-add-subviews thescroll
                     ;(om-make-dialog-item 'om-static-text (om-make-point l2 (incf posy 50)) (om-make-point 200 30) "Init folder"
                     ;                     :font *om-default-font2b*)
                     ;(om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 20)) (om-make-point 360 40) 
                     ;                     "Lisp files in this folder will be automatically loaded at startup"
                     ;                     :font *om-default-font1*)

                     (om-make-dialog-item 'om-static-text (om-make-point l2 (incf posy 50)) (om-make-point 200 30) "Default Folders"
                                          :font *om-default-font2b*)

                     (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy dy)) (om-make-point 80 22) "Output Files:"
                                          :font *controls-font*)
                     (setf outtxt (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 25)) (om-make-point 320 45)
                                                       (om-namestring (get-pref modulepref :out-files-dir))
                                          :font *om-default-font1*))
                     
                     (om-make-view 'om-icon-button 
                                                      :icon1 "folder" :icon2 "folder-pushed"
                                                      :position (om-make-point l3 (- posy 5)) :size (om-make-point 26 25) 
                                                      :action (om-dialog-item-act item
                                                                         (declare (ignore item))
                                                                         (let ((newfolder (om-choose-directory-dialog :directory
                                                                                                                      (get-pref modulepref :out-files-dir))))
                                                                           (when newfolder
                                                                             (om-set-dialog-item-text outtxt (om-namestring newfolder))
                                                                             (set-pref modulepref :out-files-dir newfolder)))))
                     
                      
                     (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 40)) (om-make-point 80 22) "Input Files:" :font *controls-font*)
                     (setf intxt (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 25)) (om-make-point 320 45)
                                                      (om-namestring (get-pref modulepref :in-files-dir))
                                                      :font *om-default-font1*))
                     
                     (om-make-view 'om-icon-button 
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om-make-point l3 (- posy 5)) :size (om-make-point 26 25) 
                                   :action (om-dialog-item-act item
                                             (declare (ignore item))
                                             (let ((newfolder (om-choose-directory-dialog :directory (get-pref modulepref :in-files-dir))))
                                               (when newfolder
                                                 (om-set-dialog-item-text intxt (om-namestring newfolder))
                                                 (set-pref modulepref :in-files-dir newfolder)))))
                      

                     (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 40)) (om-make-point 80 22) "Temp Files:" :font *controls-font*)
                     (setf tmptxt (om-make-dialog-item 'om-static-text  (om-make-point l2 (incf posy 25)) (om-make-point 320 45)
                                                       (om-namestring (get-pref modulepref :tmp-files-dir))
                                          :font *om-default-font1*))
                     
                     (om-make-view 'om-icon-button 
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om-make-point l3 (- posy 5)) :size (om-make-point 26 25) 
                                   :action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (let ((newfolder (om-choose-file-dialog)))
                                                              ;(om-choose-directory-dialog :directory (get-pref modulepref :tmp-files-dir))))
                                                         (when newfolder
                                                           (om-set-dialog-item-text tmptxt (om-namestring newfolder))
                                                          (set-pref modulepref :tmp-files-dir newfolder)))))
                     )
          
    thescroll))



;set and load tab in om preferences panel 
(pushr :lilypond *pref-order*)
(push-pref-module (list :lilypond (get-def-vals :lilypond)))



