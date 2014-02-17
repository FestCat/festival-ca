;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lexicon, LTS and Postlexical rules for upc_catalan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adapted for Catalan by the UPC team
;;;
;;; (c) Antonio Bonafonte
;;;     Universitat Politècnica de Catalunya, Barcelona, Spain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Postlexical Rules 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (upc_catalan::postlex_rule1 utt)
  "(upc_ca::postlex_rule1 utt)
A postlexical rule form adding the phoneme /r/ to an infinitive verb followed 
with a dashed weak pronoun (pronoms febles amb guio)"
  (mapcar
   (lambda (w)
     ;; do something
     (cond ((and (string-matches (item.name w) "-.*")
		 ;; item starts with a dash "-"

		 (string-matches (item.feat w "p.name") ".*r$")
		 ;; previous word ends with the character "r"

		 (not (string-matches (item.feat w "R:SylStructure.daughtern.daughtern.name" ) "r"))
		 ;; last phoneme of previous word is NOT "r"
		 )
	    
;	    (format t "upc_catalan::postlex_rule1 - add r to previous word (%s) %s\n" (item.feat w "p.name") (item.name w))

	    (set! last_pho (item.relation (item.daughtern (item.relation.daughtern (item.prev w) 'SylStructure)) 'Segment))

	    (item.relation.insert last_pho 'Segment (list "r") 'after)
	    (item.relation.insert last_pho 'SylStructure (item.next last_pho) 'after)))
     )
   (utt.relation.items utt 'Word))
  utt)

(define (upc_catalan::postlex_rule2 utt)
  "(upc_ca::postlex_rule1 utt)
A postlexical rule form correcting phenomena over word boundaries."
  (mapcar
   (lambda (s)
     ;; do something
     )
   (utt.relation.items utt 'Segment))
  utt)


(provide 'upclex_postlex)
