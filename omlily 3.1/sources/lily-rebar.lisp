(in-package :om)

;;; (fmakunbound 'make-music) 
;;;makemusic issue :

;;must be like that :

;(defmethod make-music ((type (eql 'ContextSpeccedMusic)) &rest other-args)
;(find-value-in-lily-args other-args 'element))


;;when no timesignature is given
(defun timesig? (meas)
"is there a timesig in this meas"
  (if (listp (car meas))
      (if (equal (caar meas) 'timesignaturemusic) (print "yes"))))

(defun get-lily-meas (file)
  (loop for i in file
          collect (list (car i) (cdr i))))

(defun get-onlylily-dur (liste)
  (remove 'nil
  (loop for i in liste
        collect (if (typep i 'lily-dur) i))))


(defun get-lily-rests (liste)
  (loop for i in liste
        collect (if (= 1 (restevent i)) -1 1)))

(defun get-lily-tie (liste)
  (loop for i in liste
        collect (if (= 1 (tieevent i)) 1 0)))

(defun group-1-0 (liste)
"gives grouping length of ties"
  (let ((res '())
        (switch nil))
    
    (loop for i in liste
          do (cond
              ((and (= 0 i) (not switch))
               (progn 
                 (push (list i) res)
                 (pop liste)
                 (setf switch nil)))
              
              ((and (= 0 i) switch)
               (progn
                 (push i (car res))
                 (pop liste)
                 (setf switch nil)))

              ((and (= 1 i) (not switch))
               (progn 
                 (push (list i) res)
                 (pop liste)
                 (setf switch t)))
              
              ((and (= 1 i) switch)
               (progn 
                 (push i (car res))
                 (pop liste)))
              t))
          (mapcar 'length (reverse res))))




#|
(defun get-lily-props (file)
  "this is for one voice"
  (c-obj-tup 
   (comp-obj
    (get-onlylily-dur (flat file)))))
|#

(defun get-lily-props (file)
  "this is for one voice"
  (c-obj-tup 
   (comp-obj
    file)))


(defun get-real-props (file)
  "this sets rests and group ties in one ratios"
  (let* ((durs (get-onlylily-dur (flat file)))
         (props (get-lily-props durs))
         (ties (get-lily-tie durs))
         ;(rests (appminusp (om* (get-lily-rests durs) props)));;no good should be done afterwards.
         (rests (om* (get-lily-rests durs) props))
         (groups (group-list rests (group-1-0 ties) 'linear)))
    (loop for i in groups
          collect (apply '+ i))))


#|
(defun get-lily-ratios (file)
  (let ((stream (lily-chord-or-dur file t)))
    (loop for i in stream
          collect (get-lily-props i))))
|#

(defun get-lily-ratios (file)
  (let ((stream (lily-chord-or-dur file t)))
    (loop for i in stream
          collect (get-real-props i))))



(defun get-onlylily-chords (liste)
  (remove 'nil
  (loop for i in liste
        collect (if (typep i 'chord) i))))

(defun get-lily-omchords (file)
  (let ((stream (lily-chord-or-dur file nil)))
    (loop for i in stream
          collect (get-onlylily-chords (flat i)))))

;;;;;;;;;;;;;;;;;;;;;

(defmethod! lily->om-rebar ((measures list) &optional (path nil))
            :icon 161
  ;:indoc '("path" )
  :initvals '('(4 4) nil)
            :doc "Imports lilypond format to OM and rebars the music using mktree method meaning that most complex rhythm such as 
embeded tuplets will be lost and reducd to simple ones."
            (let ((file (catch-cancel (or path (om-choose-file-dialog)))))
              (when file
                (let* ((ratios (get-lily-ratios file))
                       (chords (get-lily-omchords file))
                       (voices (loop for rat in ratios
                                     for chrd in chords
                                     collect (let ((tree (mktree rat measures)))
                                               (make-instance 'voice
                                                              :tree tree
                                                              :chords chrd)))))
                  (make-instance 'poly
                                 :voices voices)))))



