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

(define (upc_catalan_add_ruleset_correct_LTS)
"(upc_catalan_add_ruleset_correct_LTS)
Loads into the current lexicon a ruleset.
I believe it may be used in syllabification, or maybe is just obsolete code."
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

)

(provide 'upclex_syl)
