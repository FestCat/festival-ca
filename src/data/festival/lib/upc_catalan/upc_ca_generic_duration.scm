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
;;;
;;; Duration model
;;;

;;; Load any necessary files here

(define (upc_ca_generic::select_duration)
  "(upc_ca_generic::select_duration)
Set up duration model."
  (upc_ca_generic::select_duration_dialect "central")
)


(define (upc_ca_generic::select_duration_dialect dialect)
  "(upc_ca_generic::select_duration_dialect)
Set up duration model."
  (cond 
    ( (string-equal dialect "central")
      (require 'upc_ca_generic_durdata-central)
      (set! duration_cart_tree upc_ca_generic::zdur_tree-central)
      (set! duration_ph_info upc_ca_generic::phone_durs-central)
      (Param.set 'Duration_Method 'Tree_ZScores)
      (Param.set 'Duration_Stretch 1.1)
    )
    ( (string-equal dialect "valencia")
      (require 'upc_ca_generic_durdata-valencia)
      (set! duration_cart_tree upc_ca_generic::zdur_tree-valencia)
      (set! duration_ph_info upc_ca_generic::phone_durs-valencia)
      (Param.set 'Duration_Method 'Tree_ZScores)
      (Param.set 'Duration_Stretch 1.1)
    )
  )
)






(define (upc_ca_generic::reset_duration)
  "(upc_ca_generic::reset_duration)
Reset duration information."
  t
)

(provide 'upc_ca_generic_duration)
