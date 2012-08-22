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
;;; Phonset for upc_ca
;;;

;;;  Feeel free to add new feature values, or new features to this
;;;  list to make it more appropriate to your language

;; This is where it'll fall over if you haven't defined a 
;; a phoneset yet, if you have, delete this, if you haven't
;; define one then delete this error message
;; (error "You have not yet defined a phoneset for ca (and others things ?)\n            Define it in festvox/upc_ca_mar_phoneset.scm\n")

(defPhoneSet
  upc_catalan
  ;;;  Phone Features
  (;; vowel or consonant
   (vc + -)  
   ;; vowel length: short long dipthong schwa
   (vlng s l d a 0)
   ;; vowel height: high mid low
   (vheight 1 2 3 0)
   ;; vowel frontness: front mid back
   (vfront 1 2 3 0)
   ;; lip rounding
   (vrnd + - 0)
   ;; consonant type: stop fricative affricative nasal liquid
   (ctype s f a n l 0)
   ;; place of articulation: labial alveolar palatal labio-dental
   ;;                         dental velar
   (cplace l a p b d v 0)
   ;; consonant voicing
   (cvox + - 0)
   )
  (
;   (@ + a 2 2 - 0 0 +)  @ => ax, schwa
   (ax + a 2 2 - 0 0 +)
   (a  + l 1 2 - 0 0 +)
   (a1 + l 1 2 - 0 0 +)
;   (a2 + l 1 2 - 0 0 +)
   (b - 0 0 0 0 s l -)
;   (B - 0 0 0 0 f l -)
;   (bb - 0 0 0 0 s l -)
   (d - 0 0 0 0 s d -)
;   (D - 0 0 0 0 f d -)
;   (dz - 0 0 0 0 a a -)
;   (dZ - 0 0 0 0 a p -)
   (e + l 2 1 - 0 0 +)
   (e1 + l 2 1 - 0 0 +)
;   (e2 + l 2 1 - 0 0 +)
   (E + l 2 1 - 0 0 +)
   (E1 + l 2 1 - 0 0 +)
 ;  (E2 + l 2 1 - 0 0 +)
   (f - 0 0 0 0 f b +)
   (g - 0 0 0 0 s v -)
;   (G - 0 0 0 0 f v -)
;   (gg - 0 0 0 0 s v -)
;   (gz - 0 0 0 0 s v -)
   (i + l 3 1 - 0 0 +)
   (i1 + l 3 1 - 0 0 +)
;   (i2 + l 3 1 - 0 0 +)
   (j + s 3 1 - 0 0 +)
   (J - 0 0 0 0 n p -)
   (k - 0 0 0 0 s v +)
;   (ks - 0 0 0 0 s v +)
   (l - 0 0 0 0 l a -)
   (L - 0 0 0 0 l p -)
   (m - 0 0 0 0 n l -)
   (n - 0 0 0 0 n a -)
;   (N - 0 0 0 0 n v -)
   (o  + l 2 3 + 0 0 +)
   (o1 + l 2 3 + 0 0 +)
;   (o2 + l 2 3 + 0 0 +)
   (O + l 2 3 + 0 0 +)
   (O1 + l 2 3 + 0 0 +)
;   (O2 + l 2 3 + 0 0 +)
   (p - 0 0 0 0 s l +)
   (r - 0 0 0 0 l a -)
   (rr - 0 0 0 0 l a -)
   (s - 0 0 0 0 f a +)
   (S - 0 0 0 0 f p +)
   (t - 0 0 0 0 s d +)
;   (tS - 0 0 0 0 a a +)
   (u + l 3 3 - 0 0 +)
   (u1 + l 3 3 - 0 0 +)
;   (u2 + l 3 3 - 0 0 +)
;   (uw + s 3 3 - 0 0 +)
   (w + s 3 3 - 0 0 +)
;   (y + s 3 1 - 0 0 +)
   (z - 0 0 0 0 f a -)
   (Z - 0 0 0 0 f p -)
   (pau - 0 0 0 0 0 0 -)
   (_ - 0 0 0 0 0 0 -)
   (# - 0 0 0 0 0 0 -)

   
   ;; insert the phones here, see examples in 
   ;; festival/lib/*_phones.scm

  )
)

(PhoneSet.silences '(pau))
(provide 'upc_catalan_phones)
