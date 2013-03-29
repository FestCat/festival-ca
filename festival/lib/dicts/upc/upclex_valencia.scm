;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lexicon, LTS and Postlexical rules for upc_catalan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adapted for Catalan by the UPC team
;;;
;;; (c) Antonio Bonafonte
;;;     Universitat Politècnica de Catalunya, Barcelona, Spain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (upc_catalan_addenda_valencia)
  "(upc_catalan_addenda_valencia)
Addenda for valencian lexicon."
;;; Words to be added in future versions:
   ; Currently prediction is bad for these words:
   ; <Empty (for now)>

;;; Common words on the internet
  ; gmail, hotmail, facebook
  (lex.add.entry '("hotmail" n ( ((o1 t) 1 ) ( ( m E1 i1 l ) 0 ) ) ) )
  (lex.add.entry '("gmail" n ( ((Z E1) 1 ) ( ( m E1 i1 l ) 0 ) ) ) )
  (lex.add.entry '("facebook" n ( ((f E1 i1 s) 1 ) ( ( b u k ) 0 ) ) ) )
  (lex.add.entry '("copyright" n ( ((k O1) 1 ) ((p i) 0 ) ((rr a i t) 0) ) ))
  (lex.add.entry '("wikipedia" n ( ( (g w i) 0  ) ((k i) 0  ) ( (p E1) 1 ) ( (d i) 0 ) ( (a) 0) ) ))
  (lex.add.entry '("twitter" n   ( ( (t w i1) 1 ) ( (t e r) 0 ) ) ) )
  (lex.add.entry '("debian" n    ( ( (d e1) 1   ) ( (b i ) 0  ) ( ( a n) 0) )))
  (lex.add.entry '("linux"  n    ( ( (l i1) 1   ) ( (n u k s) 0 ) )))
  (lex.add.entry '("google" n    ( ( (g u) 1   ) ( (g e l ) 0 ) )))

; ;;; Symbols ...
  (lex.add.entry '("*" n (((a s) 0) ((t e1) 0) ((r i1 s k) 1))))
  (lex.add.entry '("%" n (((p e r) 0) ((s e1 n) 1))))
  (lex.add.entry '("+" n (((m e1 s) 1 ))))
  (lex.add.entry '("=" n (((i) 0) ((G w a1 l) 1))))
  (lex.add.entry '("€" n (((e1 uw) 1) ((r o) 0))))
  (lex.add.entry '("$" n (((d o1 ) 1) ((l a r) 0))))
  (lex.add.entry '("@" n (((a ) 0) ((rr o1) 1) ((b a) 0))))
  (lex.add.entry '("/" n (((b a1 ) 1) ((rr a ) 0))))

  ;; Basic punctuation must be in with nil pronunciation
)

(define (upclex_catalan_define_ruleset_syl_valencia )
"Define a ruleset to do syllabification"
; FIXME: This has been copied from central Catalan.
; Only considered things are:
;    - "ax" rules are removed
;    - dZ is not splitted ; FIXME: Check if any particular rule is needed.
;    - N is added ; FIXME:  Rules copied from "n".
;    - tS is added ; FIXME:  Check if any particular rule is needed.
;    - v is added ; FIXME:  Rules copied from "b"


  (let (
         (oldlex (lex.select "upc_catalan-valencia"))
       )
  (lts.ruleset
   upc_catalan_syl
   (  (V a1 E1 e1 e O1 o1 o u1 i1 i u ) ;; Sons vocàlics
      (V a1 E1 e1 e O1 o1 o u1 i1 i u )
      (VA2  e i o u ) ;; Sons vocàlics àtons
      (VA e i o u )
      (VNIU a1 e1 E1 e O1 O o o1) 
      (VGQ a1 o1 O1 o u)
      (VW i u ) ;; Sons i u àtons 
      (VWT i1 u1) ;; Sons i u tònics
      (VT i1 u1 O1 o1 a1 e1 E1 ) ;; Sons vocàlics
  ;    (SC y w ) ;; Sons semi-consonàntics
  ;    (SV j uw ) ;; Sons semi-vocàlics
      (SC j w ) ;; Sons semi-consonàntics (allophone removed)
      (SV j w ) ;; Sons semi-vocàlics (allophones removed)
      (GQ k g ) 
      (C p t b d k g f s z S Z m n J l L r rr N v dZ tS)
      (A a a1 E E1 e e1 O O1 o o1 u u1 i i1 p t k b d g f s z S Z m n N J l L r rr w j dZ tS)
      
      )
   ;; Rules will add - at syllable boundary
   (
    
    ;; Diftongs creixents
    ;; ( [ GQ w VGQ ] = GQ w VQG ) ;; Ex: quotidià: k w u - t i - d i - a1
    ( [ k w a1 ] = - k w a1 )
    ( [ k w o1 ] = - k w o1 )
    ( [ k w O1 ] = - k w O1 )
    ( [ k w o ] = - k w o )
    ( [ k w u ] = - k w u )
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
    ( [ u i1 ] = u - i1 ) 
    ;;     ( V [ SC ] V = - SC ) ;; Ex: duia: d u - y ax
    ( V [ w ] V = - w )
    ( V [ j ] V = - j )
  
    ;; hiat break
    ;; ( [ VWT VA ] = VWT - VA ) ;; Ex: hiat: i1 - ax t
    ( [ i1 e ] = i1 - e )
    ( [ i1 i ] = i1 - i )
    ( [ i1 o ] = i1 - o )
    ( [ i1 u ] = i1 - u )
    ( [ i1 e ] = i1 - e )
    ( [ i1 i ] = i1 - i )
    ( [ i1 o ] = i1 - o )
    ( [ i1 u ] = i1 - u )
  
    ( [ u1 e ] = u1 - e )
    ( [ u1 i ] = u1 - i )
    ( [ u1 o ] = u1 - o )
    ( [ u1 u ] = u1 - u )
    ( [ u1 e ] = u1 - e )
    ( [ u1 i ] = u1 - i )
    ( [ u1 o ] = u1 - o )
    ( [ u1 u ] = u1 - u )
        
    ( VT [ a1  ] = - a1  )
    ( VT [ E1  ] = - E1  )
    ( VT [ e1  ] = - e1  )
    ( VT [ O1  ] = - O1  )
    ( VT [ o1  ] = - o1  )
    ( VT [ u1  ] = - u1  )
    ( VT [ i1  ] = - i1  )
    

    ( u [ i1 ] = - i1 )
    ( u [ u1 ] = - u1 )
    ( [ i u ] = i - u )

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
    ( C [ u ] VW C = u - )
    ( C [ i ] VW C = i - )
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
    ( V [ N t ] # = N )
    ( [ N t s ] = N s )
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
    ( [ rr N ] # = rr N )
    ( j [ t ] # = t )
    ( [ rr N s ] # = rr N s )
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
    ( V SV [ p ] = - p )
    ( V SV [ b ] = - b )
    ( V SV [ v ] = - v )
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
    ( V SV [ N ] = - N )
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
    ( V C * [ v l ] V = - v l ) ;; Ex: vlanca : b l a1 N - k ax
    ( V C * [ v r ] V = - v r ) ;; Ex: setemvre : s ax - t e1 m - b r ax FIXME: Ok, vr and vl are wrong.. but who knows...

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
    ( V C * [ N s ] C = N s )
    ( V C * [ N s ] V = N - s )
    ( V C * [ J s ] = J s )
    ( w [ t r ] V = - t r )
    ( V [ s k ] # = s k ) ;; Ex: francesc : f r ax n - s e1 s k
    ( j [ b r ] V = - b r )
    ( j [ v r ] V = - v r )
  
    ( # [ b l ] V = b l ) ;; Ex: blanca : b l a1 N k ax
    ( # [ b r ] V = b r ) ;; Ex: setembre : s ax t e1 m b r ax
    ( # [ v l ] V = v l ) ;; Ex: vlanca : v l a1 N k ax
    ( # [ v r ] V = v r ) ;; Ex: setemvre : s ax t e1 m v r ax
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
    ( V [ p s ] C = p s - )
    ( [ n z ] C V * = n z - )
    ( [ N z ] C V * = N z - )
    ( j [ t ] C V = t - )
  
    
    ;; ( V [ L ] C = L - ) ;; Ex: Vallter: B ax L - t e1 rr 
    ;; If any consonant is followed by a vowel and there is a vowel
    ;; before it, its a syl break
    ;; the consonant cluster are dealt with above
    ( w [ k ] s = k )
    ( rr [ s ] C = s - )	
    ( n [ g w ] = - g w )
    ( n [ g ] = - g ) 
    ( N [ g w ] = - g w )
    ( N [ g ] = - g ) 
    ( V C * [ b ] V = - b )
    ( V C * [ v ] V = - v )
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
    ( V C * [ N ] V = - N )
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
    ( [ N s ] C = N s - )
  
    ;; Catch all consonants on their own (at end of word)
    ;; and vowels not preceded by vowels are just written as it
    ( C [ s ] # = s )
    ( C [ b ] = - b )
    ( C [ v ] = - v )
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
    ( C [ N ] = - N )
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
    ( [ v ] = v )
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
    ( [ N ] = N )
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
    ( [ tS  ] = tS  )
    ( [ dZ  ] = dZ  )

    )
   )
   (lex.select oldlex)
   )
)



(provide 'upclex_valencia)
