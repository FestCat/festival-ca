# Install instructions
## From Ubuntu

There is a package source available ready for [Ubuntu](http://www.ubuntu.com).
This package source is compatible with [Debian](http://www.debian.org) and derivatives.

Installation instructions:

        sudo add-apt-repository ppa:zeehio/festcat
        sudo apt-get update
        sudo apt-get install festival-ca festvox-ca-ona-hts festcat-utils
        festcat_llegeix <<< "Demà esmorzaré torrades amb melmelada."

The first command adds our [package source](https://launchpad.net/~zeehio/+archive/festcat)
which contains the compiled voices and data.

## Other distributions

1. Install [Festival-2.1](http://www.cstr.ed.ac.uk/projects/festival/).
2. Install the basic package: `upc_ca_base`
   1. Download `upc_ca_base`
   2. `./configure && sudo make install`. If `./configure` was not able to find  `festival` or `speech-tools`, the path should be specified manually. See `./configure --help` for further details.

3. Install a Catalan voice
   1. Download the voice (e.g: `upc_ca_ona_hts`).
   2. Decompress the downloaded `.tgz` file.
   3. Find out where the decompressed directory should be copied to. To do so, run: `festival -b "(if (boundp 'datadir) (print datadir) (print libdir))"`. A directory (e.g: `/usr/share/festival`) will be returned.
   4. Copy the decompressed directory `upc_ca_ona_hts` to the directory `$datadir/voices/catalan` (e.g: `/usr/share/festival/voices/catalan/upc_ca_ona_hts`). You may need superuser rights to do this.
4. Install  festcat-utils (optional)
   1. Install the dependencies: `id3`, `sox`, `file`, `csplit`, `iconv`, `perl`, `xclip`, `zenity`, `vorbiscomment` (from `vorbis-tools`), `grep`.
   2. Download `festcat-utils`.
   3. `./configure && make && sudo make install`
5. Test that it is working: `festcat_llegeix <<< "Demà esmorzaré torrades amb melmelada."`.

