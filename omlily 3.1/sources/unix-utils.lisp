(in-package :om)


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
    (with-open-stream (out out)
      (values (read-line out) ))))



(defun lilypond? ()
  "tests if lilypond is installed"
  (let ((test (sys:run-shell-command "bash -l -c 'which lilypond'")))
    (if (= 1 test)
        (om-print 
         "Warning : Either Lilypond is not installed or you didn't installed
        or 
        It is no where to be used in command line.
      Please follow these instruction in order to make the library work :
     http://www.lilypond.org/macos-x.html")
      (find-lilypond-path))))


(defun find-lilypond ()
  "outputs lilypond binary unix path"
 #+linux(find-lilypond-path)
 #+macosx(lilypond?))

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
    (with-open-stream (out out)
      (values (read-line out) ))))

(defun lilypond-version? ()
  "tests if lilypond is installed"
  (let ((test (sys:run-shell-command "bash -l -c 'lilypond -v'")))
    (if (= 127 test)
        nil
      (string-from-space2 (find-lilypond-version)))))

;;from om string-from-space function

(defun string-from-space2 (string)
   (let* ((index (search " " string))
         (frst (if index (subseq string (1+ index)) string)))
     (let ((indx (search " " frst)))
       (if indx (subseq frst (1+ indx)) frst))))


(defun lilypond-version ()
"returns the lilypond version installed"
 #+linux(string-from-space2 (find-lilypond-version))
 #+macosx(lilypond-version?))

