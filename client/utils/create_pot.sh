#!/bin/bash
rm opensoldat.pot
xgettext --omit-header -L javascript ../Unit1.pas -o opensoldat.pot
for i in ../../shared/**/*.pas ../../client/*.pas; do xgettext --omit-header -j --from-code='ISO-8859-3' -L javascript "$i" -o opensoldat.pot; done
echo "Done, saved as opensoldat.pot"
