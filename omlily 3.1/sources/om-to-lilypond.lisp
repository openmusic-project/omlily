(in-package :om)




(defmethod! om->lily ((self poly) 
                      &key 
                      (mode nil)
                      (clef nil)
                      (switch nil)
                      (path nil))
            :icon 161
            :indoc '("self" "clef" "switch" "mode" "paper" "layout" "path" )
            :initvals '(t nil nil nil nil nil nil)
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
                    
                   (lilyfile 
                    (if (or (equal mode 'gen) (= *default-comp-mode* 0))
                        (write-lil-file (cons-lil-expr-extr self (or clef (read-from-string *lily-clef*)) (or switch (read-from-string *split-note*))) pathname paperfile layoutfile)
                      (write-lil-file (cons-lily-tempo-ex-expr self (or clef (read-from-string *lily-clef*)) nil (or switch (read-from-string *split-note*))) pathname paperfile layoutfile)
                      )))
              (run-lilypond lilyfile)))


(defmethod! om->lily ((self voice) 
                        &key
                        (mode nil)
                        (clef nil)
                        (switch nil)
                        (path nil))
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile *lily-paper-other*) 
                   (pathname (or path (om-choose-new-file-dialog)))
                   (layoutfile
                    (if (or (equal mode 'gen) (= *default-comp-mode* 0))
                        (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") ressource-folder)
                      (merge-pathnames (string+ "lily-templates/layouts/" "template1" ".ly") ressource-folder))
                    )
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
                   
              (run-lilypond lilyfile)))






(defmethod! om->lily ((self chord-seq) 
	                     &key	
                             (mode nil)
                             (clef nil)
                             (switch nil)
                             (path nil))
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile *lily-paper-other*)
                   (layoutfile (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") ressource-folder))
                   (pathname (or path (om-choose-new-file-dialog)))
                   (lilyfile (write-crdseq-lily-file (staff-data self) pathname))
                   )
              (run-lilypond lilyfile)))


(defmethod! om->lily ((self multi-seq) 
	               &key
                       (mode nil)
                       (clef nil)
                       (switch nil)
                       (path nil))
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile *lily-paper-other*)
                   (layoutfile (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") ressource-folder))
                   (pathname (or path (om-choose-new-file-dialog)))
                   (lilyfile (write-multiseq-lily-file 
                              (staff-data self)
                              pathname))
                   )
              (run-lilypond lilyfile)))



