#include <stdio.h>
#include <ft2build.h>
#include FT_FREETYPE_H

// renamed record memebers (delphi keyword collision)
#define val value
#define ptr pointer
#define lib library
#define typ type

// this typedef seems to be missing in C header
typedef struct FT_MemoryRec_ FT_MemoryRec;

#include "out/c_check.h"
