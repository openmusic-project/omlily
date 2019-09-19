;; ==================================================================================== 
;;                                OMLILY
;; ==================================================================================== 
;;
;;                                  
;;                          author : Karim Haddad   
;;                     
;;
;;
;;                           $Revision: 2.1 $
;;                      $Date: 2010/04/16 13:08:12 $
;;
;;
;;
;;
;;
;This program is free software; you can redistribute it and/or 
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;--------------------------------------------------
;Package Definition (Optional, else use package :OM) 
;--------------------------------------------------
(in-package :om)


;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------
(defparameter *omlily-files* '(
                               "unix-utils"
                               "lilypond-preferences"
                               "chord-seq-to-pdf"
                               "omlily-gen"
                               "lilypond-to-om"
			       "omlily-special-tempo-dyn_extra"
                               "lily-rebar"
                               ))
                               
                               



;--------------------------------------------------
;Loading files 
;--------------------------------------------------
(mapc #'(lambda (file) (compile&load (om-relative-path '("sources") file)))*omlily-files*)


;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *subpackages-omlily-var* nil)
(setf *subpackages-omlily-var*
      '(("music editing" nil nil (
                                  om->lily
                                  lily->om
                                  lily->om-rebar
                                  ) nil)))

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-omlily-var*)

;--------------------------------------------------
;doc & info
;--------------------------------------------------

(doc-library "omlily is a library for interaction with OpenMusic and Lilypond.
 ---- TO DO ----
" 
             (find-library "omlily"))

; (gen-lib-reference (find-library "omlily"))

(unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))


(set-lib-release 3.1) 


(om-print "
;;;============================================================                                
;;               OMLily  
;;      author : Karim Haddad 
;;
;;
;;          $Revision: 2.12 $
;;     $Date: 2016/04/24 21:33:29 $
;;; (c) Hyperion -  - 2016
;;;============================================================
")

;;; (gen-lib-reference (find-library "omlily"))



