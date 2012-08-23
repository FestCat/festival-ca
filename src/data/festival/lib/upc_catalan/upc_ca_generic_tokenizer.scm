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
;;; Select Catalan Tokenizer
;;;


;;; Load any other required files
(require 'upc_catalan_tokenizer)

;;; "Voice/ca token_to_word rules 
(define (upc_ca_generic::token_to_words token name)
  "(upc_ca_generic::token_to_words token name)
Specific token to word rules for the voice upc_ca_generic.  Returns a list
of words that expand given token with name."
  (cond
   ;; voice specific token to word rules
   (t ;; when no specific rules apply do the general ones
    (upc_catalan::token_to_words token name)))
)



(define (upc_ca_generic::select_tokenizer)
  "(upc_ca_generic::select_tokenizer)
Set up tokenizer for Catalan."
  (Param.set 'Token_Method 'Token_Any)
  (set! token.punctuation upc_catalan::token.punctuation)
  (set! token.prepunctuation upc_catalan::token.prepunctuation)
  (set! token.whitespace upc_catalan::token.whitespace)
  (set! token.singlecharsymbols upc_catalan::token.singlecharsymbols)
  (set! token_to_words upc_ca_generic::token_to_words)
)

(define (upc_ca_generic::reset_tokenizer)
  "(upc_ca_generic::reset_tokenizer)
Reset any globals modified for this voice.  Called by 
(upc_ca_generic::voice_reset)."
  ;; None

  t
)

(provide 'upc_ca_generic_tokenizer)
