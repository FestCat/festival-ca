#!/bin/sh

cd "01LTSData" && \
./01-formateador.pl && \
./03-OmetAbrev.pl && \
./05-Extractorapostrofs.pl && \
./07-Revisador.pl && \
cd ".." && \
cd "02-LTSMake" && \
./01-BuildLTS.sh
