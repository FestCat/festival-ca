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

(require 'lts)

; depend on dialect information
(require 'upclex_syl)
(require 'upclex_central)
(require 'upclex_valencia)
(require 'upclex_postlex)


(define (upc_catalan_lex_select_dialect dialect)
"(upc_catalan_lex_select_dialect dialect)
Sets the lexicon attributes depending on the dialect given"
   (cond 
      ( (string-equal dialect "central")
           (require 'upclex_central)
           (if (not (member_string "upc_catalan-central" (lex.list)))
               (begin
		   (lex.create "upc_catalan-central")
		   (lex.set.compile.file (path-append upclexdir "upcdict_catalan-1.0-central.out"))
		   (lex.set.phoneset "upc_catalan-central")
                   (upc_catalan_add_ruleset_catala_downcase)
                   (upc_catalan_add_ruleset_catala_simplify_vowels)
                   (upc_catalan_add_ruleset_catala_simplify_vowels_lts)
                   (upclex_catalan_define_ruleset_syl_central)
		   (lex.set.lts.method 'upc_catalan_lts_function)
		   (upc_catalan_addenda)
                   (upc_catalan_addenda_central)
                   (require 'upc_catalan_lts_rules-central)
                   (set! upc_catalan::dialect "central")
               )
               (begin
                   (lex.select "upc_catalan-central")
                   (set! upc_catalan::dialect "central")
               )
           )
           
      )
      ( (string-equal dialect "valencia")
           (require 'upclex_valencia)
           (if (not (member_string "upc_catalan-valencia" (lex.list)))
               (begin
		   (lex.create "upc_catalan-valencia")
		   (lex.set.compile.file (path-append upclexdir "upcdict_catalan-1.0-valencia.out"))
		   (lex.set.phoneset "upc_catalan-valencia")
                   (upc_catalan_add_ruleset_catala_downcase)
                   (upc_catalan_add_ruleset_catala_simplify_vowels)
                   (upc_catalan_add_ruleset_catala_simplify_vowels_lts)
                   (upclex_catalan_define_ruleset_syl_valencia)
		   (lex.set.lts.method 'upc_catalan_lts_function)
		   (upc_catalan_addenda)
                   (upc_catalan_addenda_valencia)
                   (require 'upc_catalan_lts_rules-valencia)
                   (set! upc_catalan::dialect "valencia")
               )
               (begin
                   (lex.select "upc_catalan-valencia")
                   (set! upc_catalan::dialect "valencia")
               )
           )
      )
   )
)


(define (upc_catalan_select_lts_rules dialect)
"(upc_catalan_select_lts_rules dialect)
Sets the lexicon attributes depending on the dialect given"
   (cond 
      ( (string-equal dialect "central")
           upc_catalan_lts_rules-central
      )
      ( (string-equal dialect "valencia")
           upc_catalan_lts_rules-valencia
      )
   )
)

;;;  Function called when word not found in lexicon
;;;  and you've trained letter to sound rules
(define (upc_catalan_lts_function word features)
  "(upc_catalan_lts_function word features)
Return pronunciation of word not in lexicon."
; (format t "Word not found in lexicon: \"%s\"\n" word)
  (let ((dcword) (syls) (phones))
     (if (lts.in.alphabet word 'catala_downcase)
         (begin 
              (set! dcword (apply string-append (lts.apply word 'catala_downcase)))
              (set! dcword (lts.apply_if_exists dcword 'catala_simplify_vowels_lts))
              (set! phones (lts_predict dcword (upc_catalan_select_lts_rules upc_catalan::dialect)))
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

(define (lts.apply_if_exists word ruleset)
  "(lts.apply_if_exists)
Applies ruleset to word for the characters available in the ruleset.
Leave the rest characters as they are.
Not robust if the ruleset has rules, as context between characters is lost."
 (let ( (symb (symbolexplode word) ) (current) (output "") )
    (while (> (length symb) 0)
        (set! current (car symb))
        (set! symb (cdr symb))
        (if (lts.in.alphabet current ruleset)
           (set! output (string-append output (apply string-append (lts.apply current ruleset))))
           (set! output (string-append output current))
        )
    )
    output
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
Downs case word using a letter to sound rules table because accented characters
are note included in the builtin function."
   (if (lts.in.alphabet word 'catala_downcase)
     (lts.apply word 'catala_downcase)
     (symbolexplode word)
   )
)

(define (catala_simplify_vowels word)
   "(catala_simplify_vowels WORD)
 Simplifies forbidden vowels such as î by converting them in regular i."
   (lts.apply word 'catala_simplify_vowels)
)


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
  (let ((syls))
    (set! syls (upc_ca_lex_syllabify_phstress phones)) 
    (list word syls)))

(define (catala_trans4 word)
  " (catala_trans WORD)
Downs case word, apply LTS, stress and syllabification"
  (let ((dword (apply string-append (lts.apply word 'catala_downcase))) (syl) (phones) (phones2) (syls))
					; (format t "Paraula %s\n" dword)
    (set! phones (lts_predict dword upc_catalan_lts_rules))
    (set! phones2 (lts.apply phones 'upc_ca_correct_LTS))
    (set! syl (upc_ca_lex_syllabify_phstress phones2)) 
					;(set! syls (upc_ca_brackets syl))
    (list word syl)
  )
)  

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
    (reverse syls)
  )
)


(define (upc_catalan_add_ruleset_catala_simplify_vowels_lts)
"(upc_catalan_add_ruleset_catala_simplify_vowels_lts)
Loads into the current lexicon a ruleset used to simplify foreign vowels."
   (lts.ruleset
    catala_simplify_vowels_lts
    ( )
   
    ;; Simplifica les lletres que no tenen LTS.
   
    (   
     ;; Vocals amb dièresi
     ( [ ä ] = a ) ;some foreign word
     ( [ ë ] = e ) ;some foreign word
     ( [ ï ] = i )
     ( [ ö ] = o ) ;some foreign word
     ( [ ü ] = u )
   
   
     ;; Vocals amb circumflex
     ( [ â ] = a ) ;some foreign word
     ( [ ê ] = e ) ;some foreign word
     ( [ î ] = i ) ;some foreign word
     ( [ ô ] = o ) ;some foreign word
     ( [ û ] = u ) ;some foreign word
   
   )
  )
)


(define (upc_catalan_add_ruleset_catala_downcase)
"(upc_catalan_add_ruleset_catala_downcase)
Loads into the current lexicon a ruleset used to downcase words.
Ideally it should be loaded in a single Catalan-generic lexicon, but
code needs to be cleaned in order to do this."
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
     
     ;; Vocals amb dièresi
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
)

(define (upc_catalan_add_ruleset_catala_simplify_vowels)
"(upc_catalan_add_ruleset_catala_simplify_vowels)
Loads into the current lexicon a ruleset used to simplify weird accents in characters
in order to spell words in tokenizer.
Ideally it should be loaded in a single Catalan-generic lexicon, but
code needs to be cleaned in order to do this."

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
)

(provide 'upclex_catalan)
