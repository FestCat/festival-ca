;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                     ;;;
;;;                     Carnegie Mellon University                      ;;;
;;;                  and Alan W Black and Kevin Lenzo                   ;;;
;;;                      Copyright (c) 1998-2000                        ;;;
;;;                        All Rights Reserved.                         ;;;
;;;                                                                     ;;;
;;; Permission is hereby granted, free of charge, to use and distribute ;;;
;;; this software and its documentation without restriction, including  ;;;
;;; without limitation the rights to use, copy, modify, merge, publish, ;;;
;;; distribute, sublicense, and/or sell copies of this work, and to     ;;;
;;; permit persons to whom this work is furnished to do so, subject to  ;;;
;;; the following conditions:                                           ;;;
;;;  1. The code must retain the above copyright notice, this list of   ;;;
;;;     conditions and the following disclaimer.                        ;;;
;;;  2. Any modifications must be clearly marked as such.               ;;;
;;;  3. Original authors' names are not deleted.                        ;;;
;;;  4. The authors' names are not used to endorse or promote products  ;;;
;;;     derived from this software without specific prior written       ;;;
;;;     permission.                                                     ;;;
;;;                                                                     ;;;
;;; CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK        ;;;
;;; DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING     ;;;
;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT  ;;;
;;; SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE     ;;;
;;; FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   ;;;
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  ;;;
;;; AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,         ;;;
;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF      ;;;
;;; THIS SOFTWARE.                                                      ;;;
;;;                                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ------------------------------------------------------------------- ;;;
;;; Extension to Catalan:                                               ;;;
;;; Antonio Bonafonte                                                   ;;;
;;; Universitat Politecnica de Catalunya                                ;;;
;;; 2007, Barcelona, Spain                                              ;;;
;;; ------------------------------------------------------------------- ;;;

;;;
;;; Phoneset for Catalan
;;; uses festival/lib/upc_catalan_phones.scm; 
;;; Basically the SAMPA symbols defined by Llisterri, including lexical stress
;;;

(require 'upc_catalan_phones-central)
(require 'upc_catalan_phones-valencia)


(define (upc_ca_generic::select_phoneset)
  "(upc_ca_generic::select_phoneset)
Set up phone set for catalan"
  (upc_ca_generic::select_phoneset_dialect "central")
)

(define (upc_ca_generic::select_phoneset_dialect dialect)
  "(upc_ca_generic::select_phoneset)
Set up phone set for catalan"
  (cond 
    ( (string-matches dialect "central")
         (Param.set 'PhoneSet 'upc_catalan-central)
         (PhoneSet.select 'upc_catalan-central)
    )

    ( (string-matches dialect "valencian")
         (Param.set 'PhoneSet 'upc_catalan-valencia)
         (PhoneSet.select 'upc_catalan-valencia)
    )
  )
)

(define (upc_ca_generic::reset_phoneset)
  "(upc_ca_generic::reset_phoneset)
Reset phone set for upc_ca"
  t
)

(provide 'upc_ca_generic_phoneset)
