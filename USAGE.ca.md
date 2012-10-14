# Ús de FestCat

Hi ha diversos programes que poden utilitzar 'Festival', com 
gnopernicus, o emacs-speak. En aquest document es mostre l'ús
de les veus mitjançant Festival directament.

## Ús directe mitjançant Festival

El 'Festival' espera  codificació **ISO-8859-15**. Assegureu-vos que utilitzeu
aquesta codificació en el vostre terminal o fitxers. Si el vostre sistema
utilitza UTF-8 (tal i com ho fan moltes distribucions actuals), necessiteu
convertir el fitxer abans de la lectura.

Podeu fer servir l'opci&oacute; «Desa» del editor de text `gedit`, o fer servir
programes conversors de format, com `iconv`:

        $ iconv -f utf8 -t ISO-8859-15//TRANSLIT bon_dia_utf8.text > bon_dia_iso.text

 * Un test ràpid:

          $ echo "Bon dia, Catalunya" | festival --tts --language catalan

 * També podeu executar 'Festival' de manera interactiva:

        $ festival
        (language_catalan)
        (intro-catalan)
        (SayText "Bon dia, Catalunya.")
        (SayText "Bona nit.")
        (exit)

 * Si voleu especificar el locutor, introduïu la comanda per seleccionar
el locutor, en lloc de la comanda de selecció de llenguatge:

        (voice_upc_ca_ona_hts)
        (SayText "I tu, qui ets?")
        (voice_upc_ca_pau_hts)
        (SayText "Jo sóc, el que tu ets, i si et faig mal, em faig mal a mi mateix.")
        (voice_upc_ca_ona_hts)
        (SayText "Que maco. Això és de l'assemblea dels infants, oi?")
        (exit)

 * O per llegir un fitxer de text, per exemple `bon_dia.txt`: 

        $ echo "Bon dia, Catalunya." > bon_dia.txt
        $ festival
        (language_catalan)
        (tts_file "bon_dia.txt")
        (exit)

 * O utilitzeu l'script `text2wave` per crear un fitxer `.wav`:

        $ text2wave -o bondia.wav   -eval '(language_catalan)' bon_dia.txt 

 * Si voleu especificar el locutor:

        $ text2wave -o bondia.wav   -eval '(voice_upc_ca_ona_hts)' bon_dia.txt 

