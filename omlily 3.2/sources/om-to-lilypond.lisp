(in-package :om)

(defvar *approx-midic* nil)
(setf *approx-midic* 2)

;change
(defun run-lilypond (path &optional mode)
  (let* ((lily-path (pathname-name *LILYPOND-PATH*))
         (pdf-path (pathname-name *PDF-READER-PATH*))
         (folder (pathname-directory path))
         (folderpath (make-pathname :directory folder))
         (outfile (make-pathname :directory folder :name (pathname-name path)))
         (pdffile (make-pathname :directory folder :name (pathname-name path) :type "pdf")))
   ; (print (list (namestring folderpath) (om-path2cmdpath folderpath)))
   ; (print lily-path)
    (cond
     ((equal *om-os* :linux)
      (if (equal (test-lilypond-bin) 1)
          (om-beep-msg "LILYPOND NOT FOUND")
      (if mode
          (om-cmd-line (format nil "cd ~s; sh -c '~s ~s'" 
                             (om-path2cmdpath folderpath) 
                             lily-path
                             (om-path2cmdpath path)))
          (progn
            (om-cmd-line (format nil "cd ~s; sh -c '~s ~s'" 
                                 (om-path2cmdpath folderpath) 
                                 lily-path
                                 (om-path2cmdpath path)))
            (om-cmd-line (format nil "~s ~s &" pdf-path (om-path2cmdpath  pdffile)))
            pdffile
            )
          )))

     ((equal *om-os* :mac)
       (if mode
           (om-cmd-line (format nil "cd ~s; bash -l -c '~s ~s'" 
                                  (om-path2cmdpath folderpath) 
                                  lily-path
                                  (om-path2cmdpath path)))
           (progn
             (om-cmd-line (format nil "cd ~s; bash -l -c '~s ~s'" 
                                  (om-path2cmdpath folderpath) 
                                  lily-path
                                  (om-path2cmdpath path)))
         
             (om-cmd-line (format nil "open ~s" (om-path2cmdpath  pdffile)))
             pdffile)
         
         ))
     
     ((equal *om-os* :win)
      (if mode
          (sys:call-system (format nil "chdir ~A & lilypond ~A"
                                   (namestring folderpath)
                               ;(om-path2cmdpath *LILYPOND-PATH*);doesn't work
                                   (om-path2cmdpath path)))
        (progn
          (sys:call-system (format nil "chdir ~A & lilypond ~A"
                                   (namestring folderpath)
                               ;(om-path2cmdpath *LILYPOND-PATH*);doesn't work
                                   (om-path2cmdpath path)))
          (sys:call-system (format nil "~A" (om-path2cmdpath  pdffile)))
          pdffile
          )
          
        ))
      
     (t outfile))))


(defun get-score-param (self param)
  (get-edit-param (associated-box self) param))


(defmethod! om->lily ((self poly) 
                      &key 
                      (mode nil)
                      (clef nil)
                      (switch nil)
                      (path nil)
                      (batch nil))
  :icon 161
  :indoc '("self" "clef" "switch" "mode" "paper" "layout" "path" "batch")
  :initvals '(t nil nil nil nil nil nil nil)
  :menuins '((1 (("generic" 'gen )
                 ("polymetric" 'poly )
                 ))
             )
  :doc "Exports voice, poly,chordseq to lilypond format"
            ;(setf paper "paper")
  (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
         (paperfile *lily-paper-other*) 
         (pathname (or path (om-choose-new-file-dialog)))
         (layoutfile
          (if (or (equal mode 'gen) (= *default-comp-mode* 0))
              (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") ressource-folder)
            (merge-pathnames (string+ "lily-templates/layouts/" "template1" ".ly") ressource-folder))
          )
         (setapprox (setf *approx-midic* (get-score-param self 'approx)))
         (lilyfile 
          (if (or (equal mode 'gen) (= *default-comp-mode* 0))
              (write-lil-file (cons-lil-expr-extr self (or clef (read-from-string *lily-clef*)) (or switch (read-from-string *split-note*))) pathname paperfile layoutfile)
            (write-lil-file (cons-lily-tempo-ex-expr self (or clef (read-from-string *lily-clef*)) nil (or switch (read-from-string *split-note*))) pathname paperfile layoutfile)
            )))
    (run-lilypond lilyfile batch)))


(defmethod! om->lily ((self voice) 
                      &key
                      (mode nil)
                      (clef nil)
                      (switch nil)
                      (path nil)
                      (batch nil))
  (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
         (paperfile *lily-paper-other*) 
         (pathname (or path (om-choose-new-file-dialog)))
         (layoutfile
          (if (or (equal mode 'gen) (= *default-comp-mode* 0))
              (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") ressource-folder)
            (merge-pathnames (string+ "lily-templates/layouts/" "template1" ".ly") ressource-folder))
          )
         (setapprox (setf *approx-midic* (get-score-param self 'approx)))
         (lilyfile 
          (if (or (equal mode 'gen) (= *default-comp-mode* 0))
              (write-lil-file  (cons-lil-expr-extr 
                                (make-instance 'poly
                                               :voices self) 
                                (or clef (read-from-string *lily-clef*)) 
                                (or switch (read-from-string *split-note*))) pathname paperfile layoutfile)
            (write-lil-file 
             (cons-lily-tempo-ex-expr (make-instance 'poly
                                                     :voices self) 
                                      (or clef (read-from-string *lily-clef*)) 
                                      nil 
                                      (or switch (read-from-string *split-note*))) pathname paperfile layoutfile)
            )))
                   
    (run-lilypond lilyfile batch)
    ))






(defmethod! om->lily ((self chord-seq) 
	                     &key	
                             (mode nil)
                             (clef nil)
                             (switch nil)
                             (path nil)
                             (batch nil))
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile *lily-paper-other*)
                   (layoutfile (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") ressource-folder))
                   (pathname (or path (om-choose-new-file-dialog)))
                   (lilyfile (write-crdseq-lily-file (staff-data self) pathname))
                   )
              (run-lilypond lilyfile batch)))


(defmethod! om->lily ((self multi-seq) 
	               &key
                       (mode nil)
                       (clef nil)
                       (switch nil)
                       (path nil)
                       (batch nil))
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile *lily-paper-other*)
                   (layoutfile (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") ressource-folder))
                   (pathname (or path (om-choose-new-file-dialog)))
                   (lilyfile (write-multiseq-lily-file 
                              (staff-data self)
                              pathname))
                   )
              (run-lilypond lilyfile batch)))



;for batch processing
(defmethod! om->lily ((self list) 
                        &key
                        (mode nil)
                        (clef nil)
                        (switch nil)
                        (path nil)
                        (batch nil))
            (let ((path (om-choose-new-file-dialog)))
            (loop for i in self
                  for n from 1 to (length self)
                  do (om->lily i :path (format nil "~D~D.ly" path n) :batch 't)
            )))
