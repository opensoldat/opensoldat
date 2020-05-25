#!/bin/bash
rm soldat.pot
xgettext --omit-header -L javascript ../Unit1.pas -o soldat.pot
for i in ../../shared/**/*.pas ../../client/*.pas; do xgettext --omit-header -j --from-code='ISO-8859-3' -L javascript "$i" -o soldat.pot; done
echo "Done, saved as soldat.pot"
