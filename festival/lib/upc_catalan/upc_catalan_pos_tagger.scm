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
;;; POS tagger for catalan
;;;

;;; Load any necessary files here


(defvar upc_poslexdir (path-append lexdir "upc"))

(setq upc_catalan::lex_to_tagger
      '(
	(( VAG VAI VAM VAN VAP VAS VMG VMI VMM VMN VMP VMS VSG VSI VSM VSN VSP VSS ) v)
	(( NC NP ) n)
	(( DA DD DE DI DN DP DR DT ) dt)
	(( AO AQ ) j)
	(( CC CS Fc Fd Fe Fg Fh Fp Fpa Fpt Fs Fx Fz Faa Fat FcaFct Fit Fla Flt P0 
		PD PE PI PN PP PR PT PX RG RN SP W Y Z Zm ZP ) o)
	))




(set! upc_catalan_guess_pos 
      '(
	;; ARTICLES
	(ART un una uns unes el la els les "l'" en na)
	;; ARTICLES + PREPOSICIÓ
	(P_ART al del pel dels pels)
	;; DETERMINANTS
	(DET 
	 ;; Demostratius
	 aquest aquesta aqueix aqueixa aquell aquella aquests aquestes aqueixos aqueixes açò això aquells aquelles allò
	 ;; Possessius tònics
	 meu teu seu meva teva seva meus teus seus meves teves seves nostre vostre seu nostra vostra seva llur nostres vostres seus seves llurs
	 ;; Possessius àtons
	 mon ton son ma ta sa mos tos sos mes tes se)
	
	;; PRONOMS PERSONALS
	(PRO 
	 ;; Forts
	 jo mi tu "vós" "vostè" ell ella si nosaltres "nós" vosaltres "vostès" ells elles
	 ;; Febles
	 em ens et us el la els les li els es ho hi en )
	;; INTERROGATIUS
	(INT qui què quin quant on quan com)
	;; VERBS AUXILIARS
	(VAUX he has ha hem heu han vaig vas va vam vau van haver havia havies "havíem" "havíeu" havien "hauré" "hauràs" "haurà" haurem haureu hauran "haguí" hagueres "hagué" "haguérem" "haguéreu" hagueren hauria hauries hauria "hauríem" "hauriéu" haurien hagi hagis "hàgim" "hàgiu" hagin haja hages haja "hàgem" "hàgueu" hagen "hagués" haguessis "hagués" "haguéssim" "haguéssiu" haguessin) 
	;; PREPOSICIONS
	(PRE cap contra devers entre envers fins malgrat pro segons sense ultra vers cap a des de per amb en com)
	;; CONJUNCIONS
	(CON i ni o "adés" "sinó" "però" tanmateix altrament nogensmenys ans "emperò" doncs ergo que quan mentre "perquè" "atès" si baldament)
	(PUNT "/." "," "!" "¡" "?" "¿" "\'" "{" "}" "[" "]" "+" "=" "-" ":" ";" )
	))


(defvar guess_pos upc_catalan_guess_pos
  "guess_pos
  An assoc-list of simple part of speech tag to list of words in that
  class.  This basically only contains closed class words all other 
  words may be assumed to be content words.  This was built from information
  in the f2b database and is used by the ffeature gpos.")

;;;  These models were build from (TODO)


(defvar pos_p_start_tag "Fp"
  "pos_p_start_tag
  This variable's value is the tag most likely to appear before
  the start of a sentence.  It is used when looking for pos context
  before an utterance.  Typically it should be some type of punctuation
  tag.")

(defvar pos_pp_start_tag "NC"
  "pos_pp_start_tag
  This variable's value is the tag most likely to appear before
  pos_p_start_tag and any position preceding that.  It is typically
  some type of noun tag.  This is used to provide pos context for
  early words in an utterance.")

(defvar pos_supported nil
  "pos_supported
  If set to non-nil use part of speech prediction, if nil just get
  pos information from the lexicon.")

(defvar pos_ngram_name nil
  "pos_ngram_name
  The name of a loaded ngram containing the a posteriori ngram model for 
  predicting part of speech.  The a priori model is held as a 
  lexicon call poslex.")

(defvar pos_map nil
 "pos_map
 If set this should be a reverse assoc-list mapping on part of speech
 tag set to another.  It is used after using the defined POS models to
 map the pos feature on each word to a new tagset.")





(define (upc_catalan::select_pos_tagger)
  "(upc_catalan::select_pos_tagger)
  Set up the POS tagger for ca."
  (set! pos_lex_name nil)
  (set! guess_pos upc_catalan_guess_pos)

;  (format t "carLex: %l\n" (lex.list))

  (set! pos_supported nil)
  (if (not (member_string "upc_catalan_poslex" (lex.list)))
      (begin
	(if (probe_file(path-append upc_poslexdir "upc_freeling_catalan.poslex"))
	    (begin
	      (require 'pos)
	      (lex.create "upc_catalan_poslex")
	      (lex.set.compile.file (path-append upc_poslexdir "upc_freeling_catalan.poslex"))
	      (lex.set.phoneset "upc_catalan")
	      (lex.set.lts.method nil)
	      (lex.set.pos.map 'upc_catalan::lex_to_tagger)
	      


	      ;;REPASSAR PROBABILITATS PERQUÈ TINGUIN SENTIT
	      ;;(lex.add.entry '("OOV" ((NC -2.9144) ) ()))
	      
	      (lex.add.entry '("!" ((Fat 0)) () ))
	      (lex.add.entry '("¡" ((Faa 0)) () ))
	      (lex.add.entry '("," ((Fc  0)) () ))
	      (lex.add.entry '("[" ((Fca 0)) () ))
	      (lex.add.entry '("]" ((Fct 0)) () ))
	      (lex.add.entry '("-" ((Fg  0)) () ))
	      (lex.add.entry '(":" ((Fd  0)) () ))
	      (lex.add.entry '("/" ((Fh  0)) () ))
	      (lex.add.entry '("?" ((Fit 0)) () ))
	      (lex.add.entry '("¿" ((Fia 0)) () ))
	      (lex.add.entry '("}" ((Flt 0)) () ))
	      (lex.add.entry '("{" ((Fla 0)) () ))
	      (lex.add.entry '("." ((Fp  0)) () ))
	      (lex.add.entry '("(" ((Fpa 0)) () ))
	      (lex.add.entry '(")" ((Fpt 0)) () ))
	      (lex.add.entry '("«" ((Fra 0)) () ))
	      (lex.add.entry '("»" ((Frc 0)) () ))
	      (lex.add.entry '("%" ((Ft  0)) () ))
	      (lex.add.entry '(";" ((Fx  0)) () ))
	      (lex.add.entry '("`" ((Fz  -0.47712)) () ))
	      (lex.add.entry '("'" ((Fz  -0.47712)) () ))
	      (lex.add.entry '("\"" ((Fe -0.47712)) () ));"
;	      (lex.add.entry '("+" ((Fz -1.1104)) () ))
;	      (lex.add.entry '("=" ((Fz -1.1104)) () ))	      	      
	      (lex.add.entry '("_OOV_" 
			       ((AQ -1.234) (AO -1.234) (RG -1.234) (RN -1.234) (DD -1.234) 
				(DP -1.234) (DT -1.234) (DI -1.234) (DA -1.234) (DN -1.234) 
				(NC -1.234) (NP -1.234) (VMI -1.234)(VMS -1.234)(VMM -1.234) 
				(VMN -1.234)(VMG -1.234)(VMP -1.234)(VAI -1.234)(VAS -1.234)
				(VAN -1.234)(VAG -1.234)(VAP -1.234)(VSI -1.234)(VSS -1.234) 
				(VSM -1.234)(VSN -1.234)(VSG -1.234)(VSP -1.234)(PP -1.234) 
				(PD -1.234) (PX -1.234) (PI -1.234) (PT -1.234) (PR -1.234)
				(PN -1.234) (CC -1.234) (CS -1.234) (SP -1.234) (Z -1.234)
				(Zp -1.234) (Zm -1.234) (W -1.234) (P0 -1.234) (DR -1.234))
			       ()))
      
	      (ngram.load 'catalan_pos_ngram (path-append upc_poslexdir "upc_freeling_catalan.tri.ngrambin"))
	    )
         )
    )
   )
   (set! pos_lex_name "upc_catalan_poslex")
   (set! pos_p_start_tag "Fp")
   (set! pos_pp_start_tag "NC")
	      (set! pos_ngram_name 'catalan_pos_ngram)
  (set! pos_map '((( Fat Faa Fc Fca Fct Fg Fd Fh Fia Fit Flt Fla Fp Fpa Fpt Fx Fe) punc)))
   (set! pos_supported t)

)





(define (upc_catalan::reset_pos_tagger)
  "(upc_catalan::reset_pos_tagger)
Reset tagging information."
  t
)


;; Utility to show the output of the tagger
;; Example: after synthesis, (output-pos-tag utt0)

(define (output-pos-tag utt)
  "Output the word/pos for each word in utt"
  (mapcar 
   (lambda (posinfo)
     (format t "WORD:%l\tPOS=%l\tGPOS=%l\n" (car posinfo) (car (cdr posinfo)) (car (cdr (cdr posinfo)))  )
   )
   (utt.features utt 'Word '(name pos gpos))
  )
)

(provide 'upc_catalan_pos_tagger)
