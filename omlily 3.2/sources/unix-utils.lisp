(in-package :om)



(defun test-lilypond-bin ()
  "returns 0 if found, 1 if not"
  #+macosx(sys:run-shell-command "bash -l -c 'which lilypond'")
  #+linux(sys:run-shell-command "sh -c 'which lilypond'")
  #+win32(sys:run-shell-command "where Lilypond.exe")		
  )


(defun find-lilypond-path ()
  "outputs lilypond binary unix path"
  (multiple-value-bind (out pid)
      #+linux(sys:run-shell-command "sh -c 'which lilypond'"
                                    :wait nil
                                    :output :stream
                                    :error-output nil)
    #+macosx(sys:run-shell-command "bash -l -c 'which lilypond'"
                                   :wait nil
                                   :output :stream
                                   :error-output nil)
    #+win32(sys:run-shell-command "where Lilypond.exe"
                                   :wait nil
                                   :output :stream
                                   :error-output nil)
    (with-open-stream (out out)
      (values (read-line out) ))))


(defun lilypond? ()
  "tests if lilypond is installed"
  (let ((test (test-lilypond-bin)))
    (if (not (= 1 test))
      (find-lilypond-path)
      "NOT FOUND"
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;(format nil "~s" (find-lilypond-path))


(defun find-lilypond-version ()
  "outputs lilypond binary unix path"
  (multiple-value-bind (out pid)
      #+linux(sys:run-shell-command "sh -c 'lilypond -v'"
                             :wait nil
                             :output :stream
                             :error-output nil)
      #+macosx(sys:run-shell-command "bash -l -c 'lilypond -v'"
                             :wait nil
                             :output :stream
                             :error-output nil)
      #+win32(sys:run-shell-command "lilypond -v"
                                    :wait nil
                                    :output :stream
                                    :error-output nil
                                    )
    (with-open-stream (out out)
      (values (read-line out) ))))

;changed
(defun lilypond-version? ()
  "tests if lilypond is installed"
  (let ((test (test-lilypond-bin)))
    (if (equal 1 test)
        "\no-version"
      (string-from-space3 (find-lilypond-version))))
  )

;;from om string-from-space function

;depreciated
(defun string-from-space2 (string)
   (let* ((index (search " " string))
         (frst (if index (subseq string (1+ index)) string)))
     (let ((indx (search " " frst)))
       (if indx (subseq frst (1+ indx)) frst))))

;compiles with new lilypond version
(defun string-from-space3 (string)
  (let* ((dig (position-if #'digit-char-p string)))
    (subseq string dig (+ dig 6))))

(defun lilypond-version ()
"returns the lilypond version installed"
 #+linux(lilypond-version?);change
 #+macosx(lilypond-version?)
 #+win32 (lilypond-version?)
 )

