## Using FestCat

There are several front-ends to be used with Festival, as 
gnopernicus, or emacs-speak... Here we only mention the direct use of
Festival.

## Direct usage using Festival

Festival expects **ISO-8859-15** encoding. Be sure that you use
this encoding in your terminal or files. If your system uses UTF-8 (as
do many distributions today) you need to convert the file before reading.
Some front-ends, as gnopernicus, do the conversions for you.

You can use the "save as" options in `gedit`; or use programs to convert the 
format, as `iconv`:

        $ iconv -f utf8 -t ISO-8859-15//TRANSLIT myfile_utf8.text > myfile_latin1.text

 * A quick test:

        $ echo "Bon dia, Catalunya" | festival --tts --language catalan

 * You can also execute Festival in interactive way:

        $ festival
        (language_catalan)
        (intro-catalan)
        (SayText "Bon dia, Catalunya.")
        (SayText "Bona nit.")
        (quit)

 * If you want to specify the speaker, introduce the command to 
   select the speaker instead of the language selection command; 
   or just use it to change the speaker:

        (voice_upc_ca_ona_hts)
        (SayText "I tu, qui ets?")
        (voice_upc_ca_pau_hts)
        (SayText "Jo sóc, el que tu ets, i si et faig mal, em faig mal a mi mateix.")
        (voice_upc_ca_ona_hts)
        (SayText "Que maco. Això és de l'assemblea dels infants, oi?")
        (quit)

 * Or to read a text file, for instance `bon_dia.txt`: 

        $ echo "Bon dia, Catalunya." > bon_dia.txt
        $ festival
        (language_catalan)
        (tts_file "bon_dia.txt")
        (quit)

  * Or use the `text2wave` script to create a `.wav` file:

        $ text2wave -o bondia.wav   -eval '(language_catalan)' bon_dia.txt

  * If you want to specify the speaker:

        $ text2wave -o bondia.wav   -eval '(voice_upc_ca_ona_hts)' bon_dia.txt

