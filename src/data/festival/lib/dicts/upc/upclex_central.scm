;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lexicon, LTS and Postlexical rules for upc_catalan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adapted for Catalan by the UPC team
;;;
;;; (c) Antonio Bonafonte
;;;     Universitat Politècnica de Catalunya, Barcelona, Spain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (upc_catalan_addenda_central)
  "(upc_catalan_addenda_central)
Basic lexicon should (must ?) have basic letters, symbols and punctuation."
;;; Words to be added in future versions:
   ; Currently prediction is bad for these words:
   ; <Empty (for now)>

;;; Common words on the internet
  ; gmail, hotmail, facebook
  (lex.add.entry '("hotmail" n ( ((o1 t) 1 ) ( ( m E1 i1 l ) 0 ) ) ) )
  (lex.add.entry '("gmail" n ( ((Z E1) 1 ) ( ( m E1 i1 l ) 0 ) ) ) )
  (lex.add.entry '("facebook" n ( ((f E1 i1 s) 1 ) ( ( b u k ) 0 ) ) ) )
  (lex.add.entry '("copyright" n ( ((k O1) 1 ) ((p i) 0 ) ((rr a i t) 0) ) ))
  (lex.add.entry '("wikipedia" n ( ( (g w i) 0  ) ((k i) 0  ) ( (p E1) 1 ) ( (d i) 0 ) ( (ax) 0) ) ))
  (lex.add.entry '("twitter" n   ( ( (t w i1) 1 ) ( (t e r) 0 ) ) ) )
  (lex.add.entry '("debian" n    ( ( (d e1) 1   ) ( (b i ) 0  ) ( ( ax n) 0) )))
  (lex.add.entry '("linux"  n    ( ( (l i1) 1   ) ( (n u k s) 0 ) )))
  (lex.add.entry '("google" n    ( ( (g u) 1   ) ( (g ax l ) 0 ) )))

; ;;; Symbols ...
  (lex.add.entry '("*" n (((ax s) 0) ((t e1) 0) ((r i1 s k) 1))))
  (lex.add.entry '("%" n (((p ax r) 0) ((s e1 n) 1))))
  (lex.add.entry '("+" n (((m e1 s) 1 ))))
  (lex.add.entry '("=" n (((i) 0) ((G w a1 l) 1))))
  (lex.add.entry '("€" n (((e1 uw) 1) ((r u) 0))))
  (lex.add.entry '("$" n (((d o1 ) 1) ((l ax r) 0))))
  (lex.add.entry '("@" n (((ax ) 0) ((rr o1) 1) ((b ax) 0))))
  (lex.add.entry '("%" n (((p ax r) 0) ((s e1 n ) 1))))
  (lex.add.entry '("/" n (((b a1 ) 1) ((rr ax ) 0))))

  ;; Basic punctuation must be in with nil pronunciation
  )


(provide 'upclex_central)
