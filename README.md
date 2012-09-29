# FestCat: Speech Synthesis in Catalan using Festival

    http://festcat.talp.cat
    Antonio Bonafonte
    TALP Research Center, UPC
    Barcelona, November 2007

## Requirements and installation
Read INSTALL.md or checkout our 
[website instructions](http://festcat.talp.cat/install.php).

## FestCat Usage
See USAGE.md to learn how to use FestCat.

## About the FestCat project
 
"Festival parla català"

The FestCat package consists of a library providing
analysis of Catalan text, and the data to extend 
Festival so that it can speak Catalan.

Basically, there are two components:

1. Linguistic data and code to extend Festival for Catalan.
 Dictionaries, tokenizer, lts rules, POStagger data, etc.

  This includes two folders:
   - `dicts/upc` (basically dictionaries)
   - `upc_catalan` (basically code)

2. Voices: speaker dependent data. 
   - There is one folder for each voice: `festival/lib/voices/catalan/upc_ca_'speaker-name'`

Several voices have already been developed.
Check the [web page](http://festcat.talp.cat) to get the latest downloads.

## About the authors

This project has been initially developped by the 
[TALP Research Center](http://www.talp.cat), from 
[Universitat Politècnica de Catalunya](http://www.upc.edu), at Barcelona.
The recordings, most of the code and the data has been specifically developed for 
this project. The dictionaries are an exception.


### Dictionaries

The main source for building the dictionaries is the Catalan lexicon
provided by the FreeLing project, also developed by the TALP Research Center
and others. Please visit the [FreeLing web site](http://nlp.lsi.upc.edu/freeling/)
for more information:

The lexicon has been enriched in the following way:
 - Phonetic transcriptions have been automatically generated using the
   the TALP phonetic transcription toolkit

 - New word forms have been added using frequent words found in our corpus
   and words found in our 'speech' data to ensure better coverage when
   designing the voices.

The gramatical model has been trained using the Free database
[Àncora-ca 2.0](http://clic.ub.edu/ancora/), developped at 
[CLiC (Centre de Llenguatge i Computació)](http://clic.ub.edu/)
from [Universitat de Barcelona](http://www.ub.edu).

## Relevant Publicacions

 * Antonio Bonafonte, Jordi Adell, Ignasi Esquerra, Silvia Gallego, Asunción Moreno, Javier Pérez
   "Corpus and Voices for Catalan Speech Synthesis", Proceedings of LREC Conference 2008, p.3325-3329

 * Antonio Bonafonte, Lourdes Aguilar, Ignasi Esquerra, Sergio Oller, Asunción Moreno
   "Recent Work on the FESTCAT Database for Speech Synthesis", SLTECH-2009, 131-132

 * Sílvia Gallego, "Corpus lingüístic pel desenvolupament d'una veu sintètica en català per a Festival",
   Final Degree Project, 2010 http://upcommons.upc.edu/pfc/handle/2099.1/9921

 * Francesc Jarque, "Desenvolupament d'una veu en català per Festival", Final Degree Project 2007, 
   <http://gps-tsc.upc.es/veu/festcat/ext/UPC_Francesc_Jarque.pdf>

### Dictionaries

 * Taulé, M., M.A. Martí, M. Recasens (2008) 'Ancora: Multilevel Annotated Corpora for Catalan and Spanish', 
   Proceedings of 6th International Conference on Language Resources and Evaluation. Marrakesh (Morocco). 


## Terms and Conditions
For updated details on the copyright and license terms, please
see the COPYRIGHT and LICENSE-*.txt files.



## Thanks and funding notice
This work has been supported by the  [Catalan Government](http://www.gencat.cat).

The project was promoted by several departments from the Catalan Government
   - Departament d'Educació
   - Secretaria de Telecomunicacions i Societat de la Informació del Departament de Presidència. 

and from the Universitat Politècnica de Catalunya (UPC)
   - TALP Research Center
   - Càtedra d'Accessibilitat
   - Càtedra de Programari Lliure

Read the AUTHORS and THANKS files to see the list of people that have 
contributed to this project.

