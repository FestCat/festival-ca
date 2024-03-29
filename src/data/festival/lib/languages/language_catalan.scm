;;; Copyright (C) 2009-2011  Sergio Oller
;;; License: LGPL-2.1
;;;  This script is free software; you can redistribute it and/or
;;;  modify it under the terms of the GNU Lesser General Public
;;;  License as published by the Free Software Foundation,
;;;  version 2.1 of the License.
;;;
;;;  This script is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;  Lesser General Public License for more details.
;;;
;;;  You should have received a copy of the GNU Lesser General Public
;;;  License along with this library; if not, write to the Free Software
;;;  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;
;; Catalan language description
;;
;;

(define (set_voice_default voices)
 "set_voice_default VOICES sets as voice_default the first voice available from VOICES list"
  (let ( (finish nil)
         (voice nil)
       )
       (while (and voices (not finish))
	 (set! voice (car voices))
	 (if (assoc voice voice-locations)
            (begin 
	     (set! voice_default (intern (string-append "voice_" voice)))
               (set! finish t)
            )
	 )
	 (set! voices (cdr voices))
       )
       (if (eq finish nil)
         (begin 
            (print "Could not find any of these voices")
            (print voices)
         )
       )
  finish
  )
)

(defvar catalan-default-voices (list 'upc_ca_ona_hts 'upc_ca_pau_hts)
  "catalan-default-voices
  This variable's value contains the list of catalan voices ordered
  by a default preference. When choosing Catalan as a language, 
  the first available voice of this list will be used.")

(define (language_catalan)
"(language_catalan)
Inicialitza les veus catalanes per defecte."
  ;;;  Add the directory that contains catalan stuff (normalization, tagger, etc.) to load-path
   (defvar catalan-path (path-append (if (boundp 'datadir) datadir libdir) "upc_catalan/"))
   (if (not (member_string catalan-path load-path))
                      (set! load-path (cons catalan-path load-path)))
  (set! female1 (lambda () (voice_upc_ca_ona_hts)))
  (set! male1   (lambda () (voice_upc_ca_pau_hts)))
  (Param.set 'Language 'catalan)

  (set_voice_default catalan-default-voices)
  (eval (list voice_default))
)

(language.names.add 'catalan (list 'catala))

