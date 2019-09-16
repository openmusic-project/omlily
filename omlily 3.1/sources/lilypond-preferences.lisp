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

