
(define (intro-catalan)
"(intro-catalan)
 Synthesize an introduction to the Festival Speech Synthesis System
 in Catalan.  Catalan voice must already be selected for this."
 (if (boundp 'datadir)
   (tts (path-append datadir "upc_catalan/cat_intro.text") nil)
   (tts (path-append libdir "upc_catalan/cat_intro.text") nil))

)
(provide 'upc_catalan)
