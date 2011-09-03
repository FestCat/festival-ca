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
;;; (c) Antonio Bonafonte et al.
;;;     Universitat Politècnica de Catalunya, Barcelona, Spain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar upclexdir (path-append lexdir "upc"))

;;;"  Function called when word not found in lexicon
;;;  and you've trained letter to sound rules
(define (upc_catalan_lts_function word features)

  "(upc_catalan_lts_function word features)
Return pronunciation of word not in lexicon."
  (require 'lts)
;  (format t "Word not found in lexicon: \"%s\"\n" word)

  (if (not (boundp 'upc_catalan_lts_rules))
      (load (path-append upclexdir "upc_catalan_lts_rules.scm")))

  (let ((dcword) (syls) (phones))
     (if (lts.in.alphabet word 'catala_downcase)
         (begin 
              (set! dcword (apply string-append (lts.apply word 'catala_downcase)))
              (set! phones (lts_predict dcword upc_catalan_lts_rules))
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

;;; Pronunciation of letters in the alphabet
  ; gmail, hotmail
  (lex.add.entry '("hotmail" n ( ((o1 t) 1 ) ( ( m E1 i1 l ) 0 ) ) ) )
  (lex.add.entry '("gmail" n ( ((Z E1) 1 ) ( ( m E1 i1 l ) 0 ) ) ) )

; ;;; Symbols ...
  (lex.add.entry '("*" n (((ax s) 0) ((t e1) 0) ((r i1 s k) 1))))
  (lex.add.entry '("%" n (((p ax r) 0) ((s e1 n) 1))))
  (lex.add.entry '("+" n (((m e1 s) 1 ))))
  (lex.add.entry '("=" n (((i) 0) ((G w a1 l) 1))))
  (lex.add.entry '("¤" n (((e1 uw) 1) ((r u) 0))))
  (lex.add.entry '("$" n (((d o1 ) 1) ((l ax r) 0))))
  (lex.add.entry '("@" n (((ax ) 0) ((r o1) 1) ((B ax) 0))))
  (lex.add.entry '("%" n (((p ax r) 0) ((s e1 n ) 1))))
  (lex.add.entry '("/" n (((b a1 ) 1) ((rr ax ) 0))))

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
		  (if (string-matches (car p) "[a e E O o u]r")
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

; (define (catala_downcase_letters word)
;   "(catala_downcase WORD)
; Downs case word by letter to sound rules because or accented form
; this can't use the builtin downcase function."
;   (lts.apply word 'catala_downcase_letters))

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

(lex.create "upc_catalan")

(lex.set.lts.ruleset 'catala)


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
  ( [ ì ] = í )
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
  ( [ ï ] = ï )
  ( [ ü ] = ü )
  ( [ ö ] = o ) ;some foreign word

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
  ( [ Ï ] = ï )
  ( [ Ü ] = ü )
  ( [ Ö ] = o )

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


; I think this is used only from the tokenizer to now if there is some character which is not a letter
; Toni, July 2007

(lts.ruleset
 catala_downcase_letters
 ( )

 ;; Transforma els caràcters en el seu format en minúscula. Versió només per a lletres.

 (
  ;; MINÚSCULES
  ;; Vocals accentuades codificació occidental
  ( [ á ] = á )
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
  ( [ "ï" ] = ï )
  ( [ "ü" ] = ü )
  ;; Consonants
  ( [ b ] = b )
  ( [ c ] = c )
  ( [ "ç" ] = "ç" )
  ( [ d ] = d )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ h ] = h )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ "ñ" ] = ñ )
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

  ;; Aprostrofs, guions, l·l
  ( ["\-" ] ="\-" )
  ( ["'" ] = "'" )
  ( ["·" ] = "·" )

  
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
  ( [ "Ï" ] = "ï" )
  ( [ "Ü" ] = "ü" )

  ;; Vocals no accentuades
  
  ( [ A ] = a )
  ( [ E ] = e )
  ( [ I ] = i )
  ( [ O ] = o )
  ( [ U ] = u )
  ;; Consonants 
  ( [ B ] = b )
  ( [ C ] = c )
  ( [ "Ç" ] = ç )
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Catalan sylabification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lts.ruleset
 upc_catalan_syl
 (  (V a1 E1 e1 e O1 o1 o u1 i1 i u ax ) ;; Sons vocàlics
    (V a1 E1 e1 e O1 o1 o u1 i1 i u ax)
    (VA2 ax e i o u ) ;; Sons vocàlics àtons
    (VA e i o u )
    (VNIU ax a1 e1 E1 e O1 O o o1) 
    (VGQ ax a1 o1 O1 o u)
    (VW i u ) ;; Sons i u àtons 
    (VWT i1 u1) ;; Sons i u tònics
    (VT i1 u1 O1 o1 a1 e1 E1 ) ;; Sons vocàlics
;    (SC y w ) ;; Sons semi-consonàntics
;    (SV j uw ) ;; Sons semi-vocàlics
    (SC j w ) ;; Sons semi-consonàntics (allophone removed)
    (SV j w ) ;; Sons semi-vocàlics (allophones removed)
    (GQ k g ) 
    (C p t b d k g f s z S Z m n J l L r rr)
    (A a a1 E E1 e e1 O O1 o o1 u u1 i i1 ax p t k b d g f s z S Z m n J l L r rr w j )
    
    )
 ;; Rules will add - at syllable boundary
 (
  
  ;; Diftongs creixents
  ;; ( [ GQ w VGQ ] = GQ w VQG ) ;; Ex: quotidià: k w u - t i - d i - a1
  ( [ k w ax ] = - k w ax )
  ( [ k w a1 ] = - k w a1 )
  ( [ k w o1 ] = - k w o1 )
  ( [ k w O1 ] = - k w O1 )
  ( [ k w o ] = - k w o )
  ( [ k w u ] = - k w u )
  ( [ g w ax ] = - g w ax )
  ( [ g w a1 ] = - g w a1 )
  ( [ g w o1 ] = - g w o1 )
  ( [ g w O1 ] = - g w O1 )
  ( [ g w o ] = - g w o )
  ( [ g w u ] = - g w u )

  ;; Correccions LTS
  ( V [ s j ] = - s j )
  ( j [ t ] s V = t - )
  
  ;; Semi-consonants break
  ;; ( # SC V [ A ] = - A ) ;; Ex: iode: y o - d ax
  ( # SC V [ a1 ] = - a1 ) 
  ( # SC V [ E1 ] = - E1 )
  ( # SC V [ e1 ] = - e1 ) 
  ( # SC V [ e ] = - e ) 
  ( # SC V [ O1 ] = - O1 ) 
  ( # SC V [ o1 ] = - o1 ) 
  ( # SC V [ o ] = - o ) 
  ( # SC V [ u1 ] = - u1 )
  ( # SC V [ i1 ] = - i1 ) 
  ( # SC V [ i ] = - i ) 
  ( # SC V [ u ] = - u ) 
  ( # SC V [ ax ] = - ax ) 
  ( [ u i1 ] = u - i1 ) 
  ;;     ( V [ SC ] V = - SC ) ;; Ex: duia: d u - y ax
  ( V [ w ] V = - w )
  ( V [ j ] V = - j )

  ;; hiat break
  ;; ( [ VWT VA ] = VWT - VA ) ;; Ex: hiat: i1 - ax t
  ( [ i1 ax ] = i1 - ax )
  ( [ i1 e ] = i1 - e )
  ( [ i1 i ] = i1 - i )
  ( [ i1 o ] = i1 - o )
  ( [ i1 u ] = i1 - u )
  ( [ i1 ax ] = i1 - ax )
  ( [ i1 e ] = i1 - e )
  ( [ i1 i ] = i1 - i )
  ( [ i1 o ] = i1 - o )
  ( [ i1 u ] = i1 - u )

  ( [ u1 ax ] = u1 - ax )
  ( [ u1 e ] = u1 - e )
  ( [ u1 i ] = u1 - i )
  ( [ u1 o ] = u1 - o )
  ( [ u1 u ] = u1 - u )
  ( [ u1 ax ] = u1 - ax )
  ( [ u1 e ] = u1 - e )
  ( [ u1 i ] = u1 - i )
  ( [ u1 o ] = u1 - o )
  ( [ u1 u ] = u1 - u )
  
  ( C u [ ax ] C = - ax )
  
  ( VT [ a1  ] = - a1  )
  ( VT [ E1  ] = - E1  )
  ( VT [ e1  ] = - e1  )
  ( VT [ O1  ] = - O1  )
  ( VT [ o1  ] = - o1  )
  ( VT [ u1  ] = - u1  )
  ( VT [ i1  ] = - i1  )
  

  ( ax [ i ] = - i )
  ( ax [ u ] = - u )
  ( i1 [ ax ] = - ax )
  ( u [ i1 ] = - i1 )
  ( u [ u1 ] = - u1 )
  ( [ i u ] = i - u )
  ( [ ax i1 ] = ax - i1 )	

  ( C * [ i ax ] C * = i - ax )
  ( C * [ u ax ] C * = u - ax )
  ( C * [ i ax ] # = i - ax )
  ( C * [ u ax ] # = u - ax )
  
  ( VNIU [ ax ] = - ax )
  ( VNIU [ a1 ] = - a1 )
  ( VNIU [ e1 ] = - e1 )
  ( VNIU [ E1 ] = - E1 )
  ( VNIU [ e ] = - e )
  ( VNIU [ O1 ] = - O1 )
  ( VNIU [ O ] = - O )
  ( VNIU [ o ] = - o )
  ( VNIU [ o1 ] = - o1 )
  ( [ e1 i ] = e1 - i )
  
  ;; ( VA [ VWT ] = - VWT ) ;; Ex: beneït: b ax - n ax - i1 t
  ( VA [ i1 ] = - i1 )
  ( VA [ u1 ] = - u1 )
  ( ax [ i1 ] = - i1 )
  ( ax [ u1 ] = - u1 )
  
  

  ;; ( VW [ VT ] = - VT ) ;; Ex: marià: m ax - r i - a1
  ( u  [ i1 ] = - i1 )
  ( VW [ i1 ] = - i1 )
  ( VW [ u1 ] = - u1 )
  ( VW [ O1 ] = - O1 )
  ( VW [ o1 ] = - o1 )
  ( VW [ a1 ] = - a1 )
  ( VW [ e1 ] = - e1 )   
  ( VW [ E1 ] = - E1 ) 

  ;; prova
;  ( [ bb ] = b - b )
;  ( [ gg ] = g - g )

  
  ;;  ( [ V SV ] = V SV ) ;; Ex: noi: n O1 j      
  ( [ a1 j ] C V = a1 j - )
  ( [ a1 j ] = a1 j )
  ( [ E1 j ] = E1 j )
  ( [ e1 j ] = e1 j )
  ( [ e j ] = e j )
  ( [ O1 j ] = O1 j )
  ( [ o1 j ] = o1 j )
  ( [ o j ] = o j )
  ( [ u1 j ] = u1 j )
  ( [ i1 j ] = i1 j )
  ( [ i j ] = i j )
  ( [ u j ] = u j )
  ( [ ax j ] = ax j )

  ( [ a1 w ] = a1 w )
  ( [ E1 w ] = E1 w )
  ( [ e1 w ] = e1 w )
  ( [ e w ] = e w )
  ( [ O1 w ] = O1 w )
  ( [ o1 w ] = o1 w )
  ( [ o w ] = o w )
  ( [ u1 w ] = u1 w )
  ( [ i1 w ] = i1 w )
  ( [ i w ] = i w )
  ( [ u w ] = u w )
  ( [ ax w ] = ax w )
  ( ax w [ s ] t r =  s - )
  ( ax w [ s ] t V =  s - )
  ( ax w [ k ] C V =  k - )

  ;; Others hiat
  ;;  ( V [ V ] = - V ) ;; 

  ( V [ a1  ] = a1  )
  ( V [ E1  ] = E1  )
  ( V [ e1  ] = e1  )
  ( V [ e  ] = e  )
  ( V [ O1  ] = O1  )
  ( V [ o1  ] = o1  )
  ( V [ o  ] = o  )
  ( V [ u1  ] = u1  )
  ( V [ i1  ] = i1  )
  ( V [ i  ] = i  )
  ( V [ u  ] = u  )
  ( V [ ax  ] = ax  )
  ( C [ u ] VW C = u - )
  ( C [ i ] VW C = i - )
  ( C [ u ] ax = u - )
  ( V [ s ] SC = - s )
  ( j [ o1 ] = - o1 )
  ( [ e1 u ] = e1 - u1 )
  ( [ e i1 ] = e - i1 )

  ;; Correcions LTS - finals de paraula
  ( A [ g w ] = - g w )
  ( C [ k w ] = - k w )
  ( [ g m ] = g - m )
  ( VA [ i ] t = - i1 )  
  ( VA [ u ] t = - u1 )  
  ( V [ n t ] # = n )
  ( [ n t s ] = n s )
  ( [ d s ] # = s )
  ( [ d ] # = t )
  ( C [ t ] # = t ) 
  ( V [ l k ] # = l k )
  ( V [ rr p s ] # = rr p s )
  ( V [ rr p ] # = rr p )
  ( V [ s t s ] # = s t s )
  ( V [ m f s ] # = m f s )
  ( V [ m f ] # = m f )
  ( [ rr n ] # = rr n )
  ( j [ t ] # = t )
  ( [ rr n s ] # = rr n s )
  ( [ l m ] # = l m )
  ( [ l m s ] # = l m s )
  ( [ k s t ] # = k s t )
  ( j [ k ] = k )
  ( j [ k s ] = k s )
  ( [ j k ] # = j k )
  ( [ k s t s ] # = k s t s )
  ( # [ p s ] = p s )
  ( [ s k s ] = s k s )
  ( [ rr m ] # = rr m )
  ( [ rr m s ] # = rr m s )
  ( [ rr k ] # = rr k )
  ( [ rr k s ] # = rr k s )
  ( [ l k s ] # = l k s )
  ( [ l t s ] # = l t s )
  ( j [ t s ] # = t s )
  ( [ t r ] = - t r )
  ( [ t rr ] = - t rr )

  ;; Semi-vowels break
  ( V SV [ s ] # = s ) ;; Ex: reis: rr E1 j s
  ;; ( V SV [ C ] = - C ) ;; Ex: seitó: s ax j - t o1
  ( # ax u [ g ] = g )
  ( V SV [ p ] = - p )
  ( V SV [ b ] = - b )
  ( V SV [ t ] = - t )
  ( V SV [ d ] = - d )
  ( V SV [ k ] = - k )
  ( V SV [ g ] = - g )
  ( V SV [ f ] = - f )
  ( V SV [ s ] = - s )
  ( V SV [ z ] = - z )
  ( V SV [ S ] = - S )
  ( V SV [ Z ] = - Z )
  ( V SV [ m ] = - m )
  ( V SV [ n ] = - n )
  ( V SV [ J ] = - J )
  ( V SV [ l ] = - l )
  ( V SV [ L ] = - L )
  ( V SV [ r ] = - r )
  ( V SV [ rr ] = - rr )
  ( V SV [ w ] = - w )
  ( V SV [ j ] = - j )
  ( V SV [ w ] = - w )
 
  ;; valid CC groups
  ( V * [ s ] t = s )
  ( V C * [ b l ] V = - b l ) ;; Ex: blanca : b l a1 N - k ax
  ( V C * [ b r ] V = - b r ) ;; Ex: setembre : s ax - t e1 m - b r ax
  ( V C * [ k l ] V = - k l ) ;; Ex: inclús :  i N - k l u1 s
  ( V C * [ k r ] V = - k r ) ;; Ex: concret : k u N - k r e1 t
  ( V [ k s ] # = k s )   ;; Ex: focs : f O1 k s
  ( V C * [ d r ] V = - d r ) ;; Ex : drenar : d r ax - n a1
  ( V C * [ f l ] V = - f l ) ;; Ex: inflar : i m - f l a1 
  ( V C * [ f r ] V = - f r ) ;; Ex: fraticida : f r ax - t i - s i1 - D ax
  ( V C * [ f rr ] V = - f rr )
  ( V C * [ g l ] V = - g l ) ;; Ex: aglà : ax - g l a1 
  ( V C * [ g r ] V = - g r ) ;; Ex: grapa: g r a1 - p ax
  ( V C * [ p l ] V = - p l ) ;; Ex: platja: p l a1 d - Z ax
  ( V C * [ p r ] V = - p r ) ;; Ex: promoció: p r u - m u - s i - o1
  ( V C * [ p rr ] V = - p r )
  ( [ t rr ] = - t r ) ;; Ex: treball: t r ax - B a1 L
  ( [ t r ] = - t r )
  ( V C * [ n s ] C = n s )
  ( V C * [ n s ] V = n - s )
  ( V C * [ J s ] = J s )
  ( w [ t r ] V = - t r )
  ( V [ s k ] # = s k ) ;; Ex: francesc : f r ax n - s e1 s k
  ( j [ b r ] V = - b r )

  ( # [ b l ] V = b l ) ;; Ex: blanca : b l a1 N k ax
  ( # [ b r ] V = b r ) ;; Ex: setembre : s ax t e1 m b r ax
  ( # [ k l ] V = k l ) ;; Ex: inclús :  i N k l u1 s
  ( # [ k r ] V = k r ) ;; Ex: concret : k u N k r e1 t
  ( # [ k s ] # = k s )   ;; Ex: focs : f O1 k s
  ( # [ d r ] V = d r ) ;; Ex : drenar : d r ax n a1
  ( # [ f l ] V = f l ) ;; Ex: inflar : i m f l a1 
  ( # [ f r ] V = f r ) ;; Ex: fraticida : f r ax t i s i1 D ax
  ( # [ f rr ] V = f rr )
  ( # [ g l ] V = g l ) ;; Ex: aglà : ax g l a1 
  ( # [ g r ] V = g r ) ;; Ex: grapa: g r a1 p ax
  ( # [ p l ] V = p l ) ;; Ex: platja: p l a1 d Z ax
  ( # [ p r ] V = p r ) ;; Ex: promoció: p r u m u s i o1
  ( # [ p rr ] V = p r )
  ( # [ t rr ] V = t r ) ;; Ex: treball: t r ax B a1 L
  ( # [ t r ] V = t r )
  ( V [ s k ] # = s k ) ;; Ex: francesc : f r ax n - s e1 s k
  ( V [ k s ] C V = k s - )
  ( V [ k s ] C C V = k s - )
  ( V [ k w ] = - k w )
  ( [ ax k w ] = ax - k w )
  ( V [ p s ] C = p s - )
  ( [ n z ] C V * = n z - )
  ( j [ t ] C V = t - )

  
  ;; ( V [ L ] C = L - ) ;; Ex: Vallter: B ax L - t e1 rr 
  ;; If any consonant is followed by a vowel and there is a vowel
  ;; before it, its a syl break
  ;; the consonant cluster are dealt with above
  ( w [ k ] s = k )
  ( rr [ s ] C = s - )	
  ( n [ g w ] = - g w )
  ( n [ g ] = - g ) 
  ( V C * [ b ] V = - b )
  ( V C * [ z ] V = - z )
  ( V C * [ d ] V = - d )
  ( V C * [ f ] V = - f )
  ( V C * [ g ] V = - g )
  ( V C * [ S ] V = - S )
  ( V C * [ k ] V = - k )
  ( V C * [ l ] V = - l )
  ( V C * [ Z ] V = - Z )
  ( V C * [ m ] V = - m )
  ( V C * [ n ] V = - n )
  ( V C * [ J ] V = - J )
  ( V C * [ p ] V = - p )
  ( V C * [ r ] V = - r )
  ( V C * [ rr ] V = - rr )
  ( V C * [ s ] V = - s )
  ( V C * [ t ] V = - t )
  ( V C * [ L ] V = - L )
  ;;	( V C * [ y ] V = - y )
  ( V C * [ w ] V = - w )
  ( V C * [ j ] V = - j )
  ( V C * [ w ] V = - w )
  ( C [ j ] C V = j - )
  ( [ n s ] C = n s - )

  ;; Catch all consonants on their own (at end of word)
  ;; and vowels not preceded by vowels are just written as it
  ( C [ s ] # = s )
  ( C [ b ] = - b )
  ( C [ z ] = - z )
  ( C [ d ] = - d )
  ( C [ f ] = - f )
  ( C [ g ] = - g )
  ( C [ S ] = - S )
  ( C [ k ] = - k )
  ( C [ l ] = - l )
  ( C [ Z ] = - Z )
  ( C [ m ] = - m )
  ( C [ n ] = - n )
  ( C [ J ] = - J )
  ( C [ p ] = - p )
  ( t [ r ] = r )
  ( C [ r ] = - r )
  ( C [ rr ] = - rr )
  ( C [ s ] = - s )
  ( C [ t ] = - t )
  ( C [ L ] = - L )
					;( C [ y ] = - y )
  ( C [ w ] = - w )
  ( C [ j ] = - j )


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
  ( [ w ] = w )
  ( [ j ] = j )
  ( [ w ] = w )
  ( [ a1 ] = a1  )
  ( [ a  ] = a  )
  ( [ E1 ] = E1  )
  ( [ E  ] = E  )
  ( [ e1 ] = e1  )
  ( [ e  ] = e  )
  ( [ O1 ] = O1  )
  ( [ O  ] = O   )
  ( [ o1 ] = o1  )
  ( [ o  ] = o  )
  ( [ u1 ] = u1  )
  ( [ u  ] = u  )
  ( [ i1 ] = i1  )
  ( [ i  ] = i  )
  ( [ ax ] = ax  )

  )
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syllabification Test
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define (upc_ca_test_syl file)
  (let ((fd (fopen file "r")) (w 0) (c 0) (entry) (wordlex) (wordlts))
    (while (not (equal? (set! entry (readfp fd)) (eof-val)))
	   (set! w (+ w 1))
	   (set! wordlex (car (cdr (cdr (lex.lookup entry)))))
	   (set! wordlts (car (cdr (catala_trans entry))))
	   (if (equal? (upc_ca_compare_syl wordlex wordlts) 0)
	       (format t "PARAULA ERRÒNIA: %s LEX: %l LTS: %l\n" entry wordlex wordlts)
	       (set! c (+ c 1)))   
	   )

    (format t "PARAULES TOTALS: %d ENCERTS %d (%2.2f)\n" w  c (/ (* c 100) w))
    (fclose fd)
    ))


(define (upc_ca_test_nolts_syl filelts filewords)
  (let ((fd (fopen filewords "r")) (fs (fopen filelts "r")) (w 0) (c 0) (entry) (wordlex) (wordlts))
    (while (not (equal? (set! entry_word (readfp fd)) (eof-val)))
	   (set! entry_phones (readfp fs))
	   (set! w (+ w 1))
	   (set! wordlex (car (cdr (cdr (lex.lookup entry_word)))))
	   (set! wordlts (car (cdr (catala_trans2 entry_word entry_phones))))
	   (if (equal? (upc_ca_compare_syl wordlex wordlts) 0)
	       (format t "PARAULA ERRÒNIA: %s LEX: %l LTS: %l\n" entry_word wordlex wordlts)
	       (set! c (+ c 1)))   
	   )

    (format t "PARAULES TOTALS: %d ENCERTS %d (%2.2f)\n" w  c (/ (* c 100) w))
    (fclose fd)
    (fclose fs)
    ))

(define (upc_ca_test_auto_syl filelts filewords)
  (let ((fd (fopen filewords "r")) (fs (fopen filelts "r")) (w 0) (c 0) (entry) (wordlex) (wordlts))
    (while (not (equal? (set! entry_word (readfp fd)) (eof-val)))
	   (set! entry_phones (readfp fs))
	   (set! w (+ w 1))
	   (set! wordlex (car (cdr (cdr (lex.lookup entry_word)))))
	   (set! wordlts (car (cdr (catala_trans3_festival entry_word entry_phones))))
	   (if (equal? (upc_ca_compare_syl wordlex wordlts) 0)
	       (format t "PARAULA ERRÒNIA: %s LEX: %l LTS: %l\n" entry_word wordlex wordlts)
	       (set! c (+ c 1)))   
	   )

    (format t "TEST AMB SIL·LABIFICACIÓ AUTOMÀTICA \n PARAULES TOTALS: %d ENCERTS %d (%2.2f)\n" w  c (/ (* c 100) w))
    (fclose fd)
    (fclose fs)
    ))

(define (upc_ca_test_auto_syl_with_lts file)
  (let ((fd (fopen file "r")) (w 0) (c 0) (entry) (wordlex) (wordlts))
    (while (not (equal? (set! entry (readfp fd)) (eof-val)))
	   (set! w (+ w 1))
	   (set! wordlex (car (cdr (cdr (lex.lookup entry)))))
	   (set! wordlts (car (cdr (catala_trans4 entry))))
	   (if (equal? (upc_ca_compare_syl wordlex wordlts) 0)
	       (format t "PARAULA ERRÒNIA: %s LEX: %l LTS: %l\n" entry wordlex wordlts)
	       (set! c (+ c 1)))   
	   )

    (format t "TEST AMB SIL·LABIFICACIÓ AUTOMÀTICA I APLICANT LTS \n PARAULES TOTALS: %d ENCERTS %d (%2.2f)\n" w  c (/ (* c 100) w))
    (fclose fd)
    ))


(define (count_items lista)
  (let ((i 20))
    (while (equal? (nth i lista) nil)
	   (set! i (- i 1)))
    (set! i (+ i 1))
    i)) 

(define (upc_ca_compare_syl wordlex wordlts)
  (cond
   ((equal? (count_items wordlex) (count_items wordlts))
    (set! num (count_items wordlex))
    (set! equal 1)
    (set! num (- num 1))
    (while (equal? num -1)
	   (set! syla (car (nth num wordlex)))
	   (set! sylb (car (nth num wordlts)))
	   (if (not (equal? (count_items syla) (count_items sylb)))
	       (set! equal 0))
	   (set! num (- num 1)))
    equal)
   (t 
    0)))


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
