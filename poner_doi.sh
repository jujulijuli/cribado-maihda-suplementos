#!/bin/bash
# Uso: ./poner_doi.sh 10.5281/zenodo.1234567
# Sustituye el DOI provisional en las figuras, la portada y CITATION.cff
set -e
DOI="$1"; [ -z "$DOI" ] && { echo "Uso: $0 10.5281/zenodo.NNNNNNN"; exit 1; }
for f in Supplementary_Figure_S1.html Supplementary_Figure_S2.html; do
  sed -i.bak "s#\[Zenodo doi:10\.XXXXX/zenodo\.XXXXXXX\]#[Zenodo doi:$DOI]#g; s#doi:10\.XXXXX/zenodo\.XXXXXXX#doi:$DOI#g" "$f"
done
sed -i.bak "s#10\.5281/zenodo\.XXXXXXX#$DOI#g" index.html CITATION.cff
rm -f *.bak; grep -o "zenodo[^ <\"]*" index.html Supplementary_Figure_S*.html | sort -u
