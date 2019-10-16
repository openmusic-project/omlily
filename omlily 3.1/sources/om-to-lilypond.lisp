(in-package :om)

(defmethod! om->lily ((self poly))
            :icon 161
            :indoc '("self")
            :initvals '(t)
            :doc "Exports voice, poly,chordseq to lilypond format"
            ;(setf paper "paper")
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile *lily-paper-other*) 
                   (pathname (om-choose-new-file-dialog))
                   (layoutfile
                    (if (= *default-comp-mode* 0)
                        (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") ressource-folder)
                      (merge-pathnames (string+ "lily-templates/layouts/" "template1" ".ly") ressource-folder))
                    )
                   (lilyfile 
                    (if (= *default-comp-mode* 0)
                        (write-lil-file (cons-lil-expr-extr self (read-from-string *lily-clef*) (read-from-string *split-note*)) pathname paperfile layoutfile)
                      (write-lil-file (cons-lily-tempo-ex-expr self (read-from-string *lily-clef*) nil (read-from-string *split-note*)) pathname paperfile layoutfile)
                      )))
              (run-lilypond lilyfile)))


(defmethod! om->lily ((self voice))
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile *lily-paper-other*) 
                   (pathname (om-choose-new-file-dialog))
                   (layoutfile
                    (if (= *default-comp-mode* 0)
                        (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") ressource-folder)
                      (merge-pathnames (string+ "lily-templates/layouts/" "template1" ".ly") ressource-folder))
                    )
                   (lilyfile 
                    (if (= *default-comp-mode* 0)
                        (write-lil-file  (cons-lil-expr-extr 
                                          (make-instance 'poly
                                                         :voices self) 
                                          (read-from-string *lily-clef*)
                                          (read-from-string *split-note*)) pathname paperfile layoutfile)
                      (write-lil-file 
                              (cons-lily-tempo-ex-expr (make-instance 'poly
                                                                  :voices self) 
                                                       (read-from-string *lily-clef*)
                                                       nil 
                                                       (read-from-string *split-note*)) pathname paperfile layoutfile)
                      )))
                   
              (run-lilypond lilyfile)))






(defmethod! om->lily ((self chord-seq))
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile *lily-paper-other*)
                   (layoutfile (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") ressource-folder))
                   (pathname (om-choose-new-file-dialog))
                   (lilyfile (write-crdseq-lily-file (staff-data self) pathname))
                   )
              (run-lilypond lilyfile)))


(defmethod! om->lily ((self multi-seq))
            (let* ((ressource-folder (lib-resources-folder (find-library "omlily")))
                   (paperfile *lily-paper-other*)
                   (layoutfile (merge-pathnames (string+ "lily-templates/layouts/" "template" ".ly") ressource-folder))
                   (pathname (om-choose-new-file-dialog))
                   (lilyfile (write-multiseq-lily-file 
                              (staff-data self)
                              pathname))
                   )
              (run-lilypond lilyfile)))



