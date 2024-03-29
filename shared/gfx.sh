#!/bin/bash

# This file converts gfx.inc.in into a pascal file gfx.inc

# set current dir
cd "$(dirname "$0")" || exit

# input/output files
input="gfx.inc.in"
output="gfx.inc"

# remove empty lines
# remove lines starting with # (comments)
# add line numbers as the first column
# and save into $data
data="$(cat "$input" | sed -E '/^(#.*)?$/d' | nl)"

# define regular expression to divide a line into groups
#
#     whitespace       id after group                     file name
#         |                   |                               |
#   (1)  ( )  (GFX_GOSTEK)(_STOPA)   (FF00FF00)    ('gostek-gfx\stopa.bmp')
#    |              |                     |
# line number     group               color key
re='^\s*(\S+)(\s+)GFX_([^_]+)(\S*)\s+(\S+)\s+('"'"'.*'"'"')$'

(
  # file preamble
  echo "// Note: this file is automatically generated, any changes will be lost."
  echo "// Modify $input instead."
  echo ''

  # start constants section
  echo '{$IFDEF GFXID}'
  echo 'const'

  # GFX_ constants & GFXID_END
  echo "$data" | sed -E "s/$re/  GFX_\\3\\4 = \\1;/g"
  echo "  GFXID_END = $(echo "$data" | wc -l);";
  echo ''

  # make a list of unique groups and prepend a column with numbers
  groups="$(echo "$data" | sed -E "s/$re/GFXG_\\3/g" | sort -u | nl)"

  # output GFXG_ constants with group ids
  echo "$groups" | sed -E 's/^\s*(\S+)\s+(.+)$/  \2 = \1;/g'

  # end of constants section
  echo '{$ENDIF}'
  echo ''

  # data section, used by the texture loading
  replacement='(ID: GFX_\3\4; Group: GFXG_\3; Path: \6; ColorKey: $\5),'

  echo '{$IFDEF GFXDATA}'
  echo "$data" | sed -E "s/$re/  $replacement/g" | sed '$s/,$//'
  echo '{$ENDIF}'
) > "$output"
