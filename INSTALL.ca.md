# Instal·la
## Des d'Ubuntu

Posem un repositori a la vostra disposició preparat per a [Ubuntu](http://www.ubuntu.com).
Aquest repositori és compatible amb [Debian](http://www.debian.org) i derivats.

Les instruccions d'instal·lació són:

        sudo add-apt-repository ppa:zeehio/festcat
        sudo apt-get update
        sudo apt-get install festival-ca festvox-ca-ona-hts festcat-utils
        festcat_llegeix <<< "Demà esmorzaré torrades amb melmelada."

La primera comanda afegeix el nostre [repositori de programari](https://launchpad.net/~zeehio/+archive/festcat)
amb els paquets de les veus.

## Altres distribucions

1. Instal·leu [Festival](http://www.cstr.ed.ac.uk/projects/festival/). Recomanem la versió 2.1.
2. Instal·leu el paquet bàsic: `upc_ca_base`
   1. Descarregueu `upc_ca_base`
   2. `./configure && sudo make install`. Si `./configure` no trobés `festival` o `speech-tools`, haureu d'especificar-ne la ruta
manualment. Feu `./configure --help` per més detalls.

3. Instal·leu alguna veu
   1. Descarregueu alguna veu (p.ex: `upc_ca_ona_hts`).
   2. Descomprimiu el fitxer `.tgz` que heu descarregat.
   3. Esbrineu on heu de copiar el directori. Per fer-ho Executeu: `festival -b "(if (boundp 'datadir) (print datadir) (print libdir))"`. Obtindreu un directori (p.ex: `/usr/share/festival`)
   4. Copieu el directori descomprimit `upc_ca_ona_hts` al directori `$datadir/voices/catalan` (p.ex: `/usr/share/festival/voices/catalan/upc_ca_ona_hts`). És possible que necessiteu permís de superusuari.
4. Instal·leu festcat-utils (opcional)
   1. Instal·leu les dependències: `id3`, `sox`, `file`, `csplit`, `iconv`, `perl`, `xclip`, `zenity`, `vorbiscomment` (de `vorbis-tools`), `grep`.
   2. Descarregueu `festcat-utils`.
   3. `./configure && make && sudo make install`
5. Proveu que funciona: `festcat_llegeix <<< "Demà esmorzaré torrades amb melmelada."`.

