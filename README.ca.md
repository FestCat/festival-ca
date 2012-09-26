# FestCat: Síntesi de la parla en català fent servir Festival

    http://festcat.talp.cat
    Antonio Bonafonte
    Centre de recerca TALP, UPC
    Barcelona, Novembre 2007

## Requisits i instal·lació
Vegeu el fitxer INSTALL.ca.md o llegiu les 
[instruccions del web](http://festcat.talp.cat/install.php).

## Ús de FestCat
Vegeu el fitxer USAGE.ca.md per a obtenir instruccions d'ús.

## Quant al projecte FestCat
 
"Festival parla català"

El paquet FestCat consisteix en una llibreria que permet l'anàlisi de text
en català  i dades per estendre Festival per tal que parli català. 

Està format per dos components principals:

1. Dades lingüístiques i codi per estendre Festival pel català.
   Diccionaris, transcripció fonètica, etiquetador morfo-sintàctic, etc.

  Inclou dues carpetes:
   - `dicts/upc` (bàsicament diccionaris)
   - `upc_catalan` (bàsicament codi)

2. Veus: dades dependents del locutor 
   - Hi ha una carpeta per cada veu: `festival/lib/voices/catalan/upc_ca_'nom-locutor'`

Actualment hi ha diverses veus disponibles.
Visiteu la [pàgina web](http://festcat.talp.cat) per obtenir últimes versions. 

## Quant als autors

Aquest projecte ha estat desenvolupat inicialment pel [Centre TALP](http://www.talp.cat),
de la [Universitat Politècnica de Catalunya](http://www.upc.edu), a Barcelona.
Les gravacions, bona part de les dades i la major part del codi ha estat desenvolupat 
específicament per aquest projecte. Els diccionaris són una excepció.

### Diccionaris

La font més important per construir els diccionaris és el lèxic català
proporcionat pel projecte FreeLing, també desenvolupat, entre altres,
pel Centre de Recerca TALP. Per més informació, visiteu la 
[pàgina web de FreeLing](http://nlp.lsi.upc.edu/freeling/):

El lèxic ha estat enriquit de la forma següent:

 - Les transcripcions fonètiques s'han generat automàticament utilitzant
   les eines de transcripció fonètica del TALP.

 - S'ha afegit noves paraules utilitzant per assegurar millor cobertura en 
   el disseny de les veus.

Pel que fa el model gramatical, s'ha utilitzat la base de dades lliure
[Àncora-ca 2.0](http://clic.ub.edu/ancora/), desenvolupada al 
[CLiC (Centre de Llenguatge i Computació)](http://clic.ub.edu/)
de la Universitat de Barcelona.


## Publicacions rellevants

 * Antonio Bonafonte, Jordi Adell, Ignasi Esquerra, Silvia Gallego, Asunción Moreno, Javier Pérez
   "Corpus and Voices for Catalan Speech Synthesis", Proceedings of LREC Conference 2008, p.3325-3329

 * Antonio Bonafonte, Lourdes Aguilar, Ignasi Esquerra, Sergio Oller, Asunción Moreno
   "Recent Work on the FESTCAT Database for Speech Synthesis", SLTECH-2009, 131-132

 * Sílvia Gallego, "Corpus lingüístic pel desenvolupament d'una veu sintètica en català per a Festival",
   Projecte Final de Carrera, 2010 http://upcommons.upc.edu/pfc/handle/2099.1/9921

 * Francesc Jarque, "Desenvolupament d'una veu en català per Festival", Projecte Final de Carrera 2007, 
   http://gps-tsc.upc.es/veu/festcat/ext/UPC_Francesc_Jarque.pdf

### Diccionaris

 * Taulé, M., M.A. Martí, M. Recasens (2008) 'Ancora: Multilevel Annotated Corpora for Catalan and Spanish', 
   Proceedings of 6th International Conference on Language Resources and Evaluation. Marrakesh (Morocco). 

## Condicions d'ús
La informació actualitzada de copyright i llicència es troba als fitxers
COPYRIGHT i LICENSE-*.txt.

## Agraïments
Aquest treball ha estat finançat per la [Generalitat de Catalunya](http://www.gencat.cat).

El projecte ha estat promogut per diversos Departaments de la Generalitat 
de Catalunya:

  - Departament d'Educació
  - Secretaria de Telecomunicacions i Societat de la Informació 
    del Departament de Presidència. 

i per la Universitat Politècnica de Catalunya (UPC):

  - Centre de Recerca TALP
  - Càtedra d'Accessibilitat
  - Càtedra de Programari Lliure


Llegiu el fitxer AUTHORS.ca i THANKS.ca per veure la llista de gent que ha contribuït a 
aquest projecte.

