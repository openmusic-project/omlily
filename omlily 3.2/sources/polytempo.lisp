(in-package :om)

(defmethod! poly-tempo? ((self poly))
            (let* ((voices (inside self))
                   (tempi (loop for i in voices
                                collect (qtempo i)))
                   (test (remove-dup tempi 'equal '1)))
              (if (not (= 1 (length test))) tempi)))
              

(defun tempo-ratio2float (tree)
"This converts all ratio tempi in a tempo format tree"
  (if (atom tree)
      tree
    (list  (if (and (all-atoms? (first tree)) (ratiop (second (first tree)))) 
               (list (car (first tree)) (float (second (first tree))))
             (first tree))
           (mapcar 'tempo-ratio2float
                   (if (and (all-atoms? (second tree)) (ratiop (second (second tree)))) 
                       (list (car (second tree)) (float (second (second tree))) (last-elem (second tree)))
                     (second tree))))))
             

                   
;;;from scoretools.lisp
;; this will DRAW (not output) tempo in float even if it is in ratio format !

(defun draw-tempo (fig num x y size name)
  (let ((textsize (round size 2)))
    (when num
      (om-with-font (get-font-to-draw 8)
                    (om-draw-string x y (get-string-for-tempo fig)))
      (om-with-font (get-font-to-draw 6) 
                    (om-draw-string (+ x (round size 3)) y (format nil "=~D" (float num)))));;;the change ...
    (when name
      (om-with-font (get-font-to-draw 6) 
                    (om-draw-string (+ x (round size 2)) (if num (- y (round size 1.8)) y) name)))))