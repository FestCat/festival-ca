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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lexicon, LTS and Postlexical rules for upc_catalan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adapted for Catalan by the UPC team
;;;
;;; (c) Antonio Bonafonte
;;;     Universitat Politècnica de Catalunya, Barcelona, Spain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar upclexdir (path-append lexdir "upc"))
(defvar upc_catalan::dialect "central") ; we need a default value. if it is already set then it will not be changed

;;;"  Function called when word not found in lexicon
;;;  and you've trained letter to sound rules
(define (upc_catalan_lts_function word features)

  "(upc_catalan_lts_function word features)
Return pronunciation of word not in lexicon."
  (require 'lts)
; (format t "Word not found in lexicon: \"%s\"\n" word)
  
  (set! ltsrules_dialect (string-append "upc_catalan_lts_rules-" upc_catalan::dialect) )
  (if (not (boundp (intern ltsrules_dialect)))
      (load (path-append upclexdir (string-append ltsrules_dialect ".scm"))))

  (let ((dcword) (syls) (phones))
     (if (lts.in.alphabet word 'catala_downcase)
         (begin 
              (set! dcword (apply string-append (lts.apply word 'catala_downcase)))
              (set! phones (lts_predict dcword ltsrules_dialect))
    (set! syl (lts.apply phones 'upc_catalan_syl)) ;; Handwritten rules for syllabification
;;  (set! syls (upc_ca_lex_syllabify_phstress phones)) ;; Automatic rules for syllabification
              (set! syls (upc_catalan_brackets syl))
         )
         (begin
              (set! syls nil) ;If we can't downcase the word, there are strange chars. Play nothing and warn user.
              (format t "upc_catalan_lts_function: Word ommitted: %s.\n" word)
         )
     )
    (list word features syls)
  )
)



(define (upc_catalan_addenda)
  "(upc_catalan_addenda)
Basic lexicon should (must ?) have basic letters, symbols and punctuation."
;;; Words to be added in future versions:
   ; Currently prediction is bad for these words:
   ; <Empty (for now)>

;;; Common words on the internet
  ; gmail, hotmail, facebook

; ;;; Symbols ...

  ;; Basic punctuation must be in with nil pronunciation
  (lex.add.entry '("." punc nil))
 ;(lex.add.entry '("." nn (((p u1 n) 1) ((t o) 0))))
  (lex.add.entry '("'" punc nil))
  (lex.add.entry '(":" punc nil))
  (lex.add.entry '(";" punc nil))
  (lex.add.entry '("," punc nil))
 ;(lex.add.entry '("," nn (((k o1) 1) ((m a) 0))))
  (lex.add.entry '("-" punc nil))
  (lex.add.entry '("\"" punc nil))
  (lex.add.entry '("`" punc nil))
  (lex.add.entry '("?" punc nil))
  (lex.add.entry '("¿" punc nil))
  (lex.add.entry '("!" punc nil))
  (lex.add.entry '("¡" punc nil))
  (lex.add.entry '("(" punc nil))
  (lex.add.entry '(")" punc nil))
  (lex.add.entry '("[" punc nil))
  (lex.add.entry '("]" punc nil))
  (lex.add.entry '("{" punc nil))
  (lex.add.entry '("}" punc nil))
  (lex.add.entry '("<" punc nil))
  (lex.add.entry '(">" punc nil))
  )



(define (upc_catalan_brackets phones)
  "(upc_catalan_brackets phones)
Takes a list of phones containing - as syllable boundary.  Construct the
Festival bracket structure."
  (let ((syl nil) (syls nil) (p phones) (stress 0))
    (if (string-matches (car p) '-) (set! p (cdr p)))
    (while p
	   (set! syl nil)
	   (set! stress 0)
	   (while (and p (not (eq? '- (car p))))
		  (set! syl (cons (car p) syl))
		  (if (string-matches (car p) "[a e E i O o u]1")
		      (set! stress 1))
		  (set! p (cdr p)))
	   (set! p (cdr p))  ;; skip the syllable separator
	   (if (eq? '- (car p))
	       (set! p (cdr p)))
	   (set! syls (cons (list (reverse syl) stress) syls)))
    (reverse syls)))   

(define (catala_downcase word)
  "(catala_downcase WORD)
Downs case word by letter to sound rules because or accented form
this can't use the builtin downcase function."


   (if (lts.in.alphabet word 'catala_downcase)
     (lts.apply word 'catala_downcase)
     (symbolexplode word)
   )
)

 (define (catala_simplify_vowels word)
   "(catala_simplify_vowels WORD)
 Simplifies forbidden vowels such as î."
   (lts.apply word 'catala_simplify_vowels))


(define (catala_trans word)
  " (catala_trans WORD)
Downs case word, apply LTS, stress and syllabification"
  (let ((dword (apply string-append (lts.apply word 'catala_downcase))) (phones) (phones2) (syls))
    (set! phones (lts_predict dword upc_catalan_lts_rules))
    (set! phones2 (lts.apply phones 'upc_ca_correct_LTS))
    (set! syl (lts.apply phones2 'upc_ca_syl))
    (set! syls (upc_ca_brackets syl))
    (list word syls)
  )
)

(define (catala_trans2 word phones)
  " (catala_trans2 WORD)
Apply stress and syllabification to phone list (phones)"
  (let ((syl) (syls))
    (set! syl (lts.apply phones 'upc_ca_syl))
    (set! syls (upc_ca_brackets syl))
    (list word syls)))

(define (catala_trans3_festival word phones)
  " (catala_trans3 WORD)
Apply stress and automatic syllabification to phone list (phones)"
  (let ((syl) (syls))
    (set! syls (upc_ca_lex_syllabify_phstress phones)) 
    (list word syls)))

(define (catala_trans4 word)
  " (catala_trans WORD)
Downs case word, apply LTS, stress and syllabification"
  (let ((dword (apply string-append (lts.apply word 'catala_downcase))) (phones) (phones2) (syls))
					; (format t "Paraula %s\n" dword)
    (set! phones (lts_predict dword upc_catalan_lts_rules))
    (set! phones2 (lts.apply phones 'upc_ca_correct_LTS))
    (set! syl (upc_ca_lex_syllabify_phstress phones2)) 
					;(set! syls (upc_ca_brackets syl))
    (list word syl)))  

;Thanks to Nickolay V. Shmyrev for strconcat:
(define (strconcat list)
    "convert list into string"
    (cond ((null list) "")
          (t (string-append (car list) (strconcat (cdr list))))
    )
)

(define (catala_downcase_string word)
;  (format t "catala_downcase_string word :%l\n)
  (if (string-matches word 0) 
      (string-append "")
      (strconcat (catala_downcase word))
  )
)

(define (upc_ca_is_vowel x)
  (string-equal "+" (phone_feature x "vc")))

(define (upc_ca_contains_vowel l)
  (member_string
   t
   (mapcar (lambda (x) (upc_ca_is_vowel x)) l)))

(define (upc_ca_lex_sylbreak currentsyl remainder)
  "(upc_ca_lex_sylbreak currentsyl remainder)
t if this is a syl break, nil otherwise."
  (cond
   ((not (upc_ca_contains_vowel remainder))
    nil)
   ((not (upc_ca_contains_vowel currentsyl))
    nil)
   (t
    ;; overly naive, I mean wrong
    t))
  )

(define (upc_ca_lex_syllabify_phstress phones)
  (let ((syl nil) (syls nil) (p phones) (stress 0))
    (while p
	   (set! syl nil)
	   (set! stress 0)
	   (while (and p (not (upc_ca_lex_sylbreak syl p)))
		  (if (string-matches (car p) "xxxx")
		      (begin
			;; whatever you do to identify stress
			(set! stress 1)
			(set syl (cons (car p-stress) syl)))
		      (set! syl (cons (car p) syl)))
		  (set! p (cdr p)))
	   (set! syls (cons (list (reverse syl) stress) syls)))
    (reverse syls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexicon definition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lex.create "upc_catalan-generic")

;(lex.set.lts.ruleset 'catala) ; this is useless I believe


(lts.ruleset
 catala_downcase
 ( )

 ;; Transforma els caràcters en el seu format en minúscula.

 (

  ;; MINÚSCULES

  ;; Vocals accentuades codificació occidental
  ( [ á ] = à )
  ( [ à ] = à )
  ( [ é ] = é )
  ( [ è ] = è )
  ( [ í ] = í )
  ( [ ì ] = ì )
  ( [ ó ] = ó )
  ( [ ò ] = ò )
  ( [ ú ] = ú )
  ( [ ù ] = ù )

  ;; Vocals no accentuades 
  ( [ a ] = a )
  ( [ e ] = e )
  ( [ i ] = i )
  ( [ o ] = o )
  ( [ u ] = u )
  
  ;; Vocals amb diéresi
  ( [ ä ] = ä ) ;some foreign word
  ( [ ë ] = ë ) ;some foreign word
  ( [ ï ] = ï )
  ( [ ö ] = ö ) ;some foreign word
  ( [ ü ] = ü )


  ;; Vocals amb circumflex
  ( [ â ] = â ) ;some foreign word
  ( [ ê ] = ê ) ;some foreign word
  ( [ î ] = î ) ;some foreign word
  ( [ ô ] = ô ) ;some foreign word
  ( [ û ] = û ) ;some foreign word


  ;; Consonants
  ( [ b ] = b )
  ( [ c ] = c )
  ( [ ç ] = ç )
  ( [ d ] = d )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ h ] = h )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ ñ ] = ñ )
  ( [ p ] = p )
  ( [ q ] = q )
  ( [ r ] = r )
  ( [ s ] = s )
  ( [ t ] = t )
  ( [ v ] = v )
  ( [ w ] = w )
  ( [ x ] = x )
  ( [ y ] = y )
  ( [ z ] = z )

;;; Símbols
  ( [ "·" ] = "·" )
  ( [ "'" ] = "" )
  ( [ "\·" ] = "\·" )
  ( [ "\!" ] = "\!" )
  ( [ "\"" ] = "\"" )
  ( [ "\#" ] = "\#" )
  ( [ "\$" ] = "\$" )
  ( [ "\£" ] = "\£" )
  ( [ "\¤" ] = "\¤" )
  ( [ "\%" ] = "\%" )
  ( [ "\&" ] = "\&" )
  ( [ "\'" ] = "\'" )
  ( [ "\(" ] = "\(" )
  ( [ "\)" ] = "\)" )
  ( [ "\*" ] = "\*" )
  ( [ "\+" ] = "\+" )
  ( [ "\," ] = "\," )
  ( [ "\-" ] = "\-" )
  ( [ "\." ] = "\." )
  ( [ "\/" ] = "\/" )
  ( [ "\:" ] = "\:" )
  ( [ "\;" ] = "\;" )
  ( [ "\<" ] = "\<" )
  ( [ "\=" ] = "\=" )
  ( [ "\>" ] = "\>" )
  ( [ "\?" ] = "\?" )
  ( [ "\@" ] = "\@" )
  ( [ "\[" ] = "\[" )
  ( [ "\\" ] = "\\" )
  ( [ "\^" ] = "\^" )
  ( [ "\_" ] = "\_" )
  ( [ "\`" ] = "\`" )
  ( [ "\{" ] = "\{" )
  ( [ "\|" ] = "\|" )
  ( [ "\}" ] = "\}" )
  ( [ "\~" ] = "\~" )
  ( [ " " ] =  " "  )
  ( [ "%" ] = "%" )
  
  ;; MAJÚSCULES

  
  ;; Vocals accentuades amb codificació occidental

  ( [ Á ] = á )
  ( [ À ] = à )
  ( [ É ] = é )
  ( [ È ] = è )
  ( [ Í ] = í )
  ( [ Ì ] = ì )
  ( [ Ó ] = ó )
  ( [ Ò ] = ò )
  ( [ Ú ] = ú )
  ( [ Ù ] = ù )


  ;; Vocals amb diéresi
  ( [ Ä ] = à ) ;some foreign word
  ( [ Ë ] = é ) ;some foreign word
  ( [ Ï ] = ï )
  ( [ Ü ] = ü )
  ( [ Ö ] = ö ) ;some foreign word

  ;; Vocals amb circumflex
  ( [ Â ] = â ) ;some foreign word
  ( [ Ê ] = ê ) ;some foreign word
  ( [ Î ] = î ) ;some foreign word
  ( [ Ô ] = ô ) ;some foreign word
  ( [ Û ] = û ) ;some foreign word

  ;; Vocals no accentuades
  
  ( [ A ] = a )
  ( [ E ] = e )
  ( [ I ] = i )
  ( [ O ] = o )
  ( [ U ] = u )
  ;; Consonants 
  ( [ B ] = b )
  ( [ C ] = c )
  ( [ Ç ] = ç )
  ( [ D ] = d )
  ( [ F ] = f )
  ( [ G ] = g )
  ( [ H ] = h )
  ( [ J ] = j )
  ( [ K ] = k )
  ( [ L ] = l )
  ( [ M ] = m )
  ( [ N ] = n )
  ( [ Ñ ] = ñ )
  ( [ P ] = p )
  ( [ Q ] = q )
  ( [ R ] = r )
  ( [ S ] = s )
  ( [ T ] = t )
  ( [ V ] = v )
  ( [ W ] = w )
  ( [ X ] = x )
  ( [ Y ] = y )
  ( [ Z ] = z )

  ;; Números

  ( [ 0 ] = 0 )
  ( [ 1 ] = 1 )
  ( [ 2 ] = 2 )
  ( [ 3 ] = 3 )
  ( [ 4 ] = 4 )
  ( [ 5 ] = 5 )
  ( [ 6 ] = 6 )
  ( [ 7 ] = 7 )
  ( [ 8 ] = 8 )
  ( [ 9 ] = 9 )
  ))

(lts.ruleset
 catala_simplify_vowels
 ( )

 ;; Transforma vocals lletges. S'utilitza en el tokenizer

 (
  ;; MINÚSCULES
  ;; Vocals accentuades codificació occidental
  ( [ a ] = a )
  ( [ à ] = a )
  ( [ á ] = a )
  ( [ ä ] = a )
  ( [ â ] = a )

  ( [ e ] = e )
  ( [ é ] = e )
  ( [ è ] = e )
  ( [ ë ] = e )
  ( [ ê ] = e )

  ( [ i ] = i )
  ( [ ì ] = i )
  ( [ í ] = i )
  ( [ ï ] = i )
  ( [ î ] = i )

  ( [ o ] = o )
  ( [ ó ] = o )
  ( [ ò ] = o )
  ( [ ö ] = o )
  ( [ ô ] = o )

  ( [ u ] = u )
  ( [ ú ] = u )
  ( [ ù ] = u )
  ( [ ü ] = u )
  ( [ û ] = u )

  ))



(lts.ruleset
 upc_ca_correct_LTS
 ( )
 (

  ( E1 [ j ] ax = j )
  ( ax [ j ] E1 = j ) 
  ( ax [ j ] e1 = j )
  ( u1 [ j ] ax = j )
  ( [ j ] = i )

  ( [ b ] = b )
  ( [ z ] = z )
  ( [ d ] = d )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ S ] = S )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ Z ] = Z )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ J ] = J )
  ( [ p ] = p )
  ( [ r ] = r )
  ( [ rr ] = rr )
  ( [ s ] = s )
  ( [ t ] = t )
  ( [ L ] = L )
  ( [ j ] = j )
  ( [ w ] = w )
  ( [ j ] = j )

  ( [ a1  ] = a1  )
  ( [ a   ] = a   )
  ( [ E1  ] = E1  )
  ( [ E   ] = E   )
  ( [ e1  ] = e1  )
  ( [ e   ] = e   )
  ( [ i1  ] = i1  )
  ( [ i  ] = i  )
  ( [ O1  ] = O1  )
  ( [ O   ] = O   )
  ( [ o1  ] = o1  )
  ( [ o   ] = o   )
  ( [ u1  ] = u1  )
  ( [ u   ] = u   )
  ( [ ax  ] = ax  )
  ))


(require 'upclex_syl)
(require 'upclex_postlex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexicon definition (continue)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lex.set.compile.file (path-append upclexdir "upcdict_catalan-1.0.out"))
(lex.set.phoneset "upc_catalan")
(lex.set.lts.method 'upc_catalan_lts_function)
(upc_catalan_addenda)

(provide 'upclex_catalan)
