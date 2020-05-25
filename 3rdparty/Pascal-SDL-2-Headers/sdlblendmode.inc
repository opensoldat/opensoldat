//from "sdl_blendmode.h"

{**
 *  The blend mode used in SDL_RenderCopy() and drawing operations.
 *}

type
  PSDL_BlendMode = ^TSDL_BlendMode;
  TSDL_BlendMode = DWord;

const
  SDL_BLENDMODE_NONE  = $00000000;    {**< No blending *}
  SDL_BLENDMODE_BLEND = $00000001;    {**< dst = (src * A) + (dst * (1-A)) *}
  SDL_BLENDMODE_ADD   = $00000002;    {**< dst = (src * A) + dst *}
  SDL_BLENDMODE_MOD   = $00000004;    {**< dst = src * dst *}
