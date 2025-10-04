//from "sdl_blendmode.h"

{**
 *  The blend mode used in SDL_RenderCopy() and drawing operations.
 *}

type
  PSDL_BlendMode = ^TSDL_BlendMode;
  TSDL_BlendMode = type DWord;

const
  SDL_BLENDMODE_NONE  = TSDL_BlendMode($00000000);    {**< No blending *}
  SDL_BLENDMODE_BLEND = TSDL_BlendMode($00000001);    {**< dst = (src * A) + (dst * (1-A)) *}
  SDL_BLENDMODE_ADD   = TSDL_BlendMode($00000002);    {**< dst = (src * A) + dst *}
  SDL_BLENDMODE_MOD   = TSDL_BlendMode($00000004);    {**< dst = src * dst *}

type
  PSDL_BlendOperation = ^TSDL_BlendOperation;
  TSDL_BlendOperation = type DWord;

const
  SDL_BLENDOPERATION_ADD              = TSDL_BlendOperation($1);  {**< dst + src: supported by all renderers *}
  SDL_BLENDOPERATION_SUBTRACT         = TSDL_BlendOperation($2);  {**< dst - src : supported by D3D9, D3D11, OpenGL, OpenGLES}
  SDL_BLENDOPERATION_REV_SUBTRACT     = TSDL_BlendOperation($3);  {**< src - dst : supported by D3D9, D3D11, OpenGL, OpenGLES}
  SDL_BLENDOPERATION_MINIMUM          = TSDL_BlendOperation($4);  {**< min(dst, src) : supported by D3D11 *}
  SDL_BLENDOPERATION_MAXIMUM          = TSDL_BlendOperation($5);  {**< max(dst, src) : supported by D3D11 *}

type
  PSDL_BlendFactor = ^TSDL_BlendFactor;
  TSDL_BlendFactor = type DWord;

const
  SDL_BLENDFACTOR_ZERO                = TSDL_BlendFactor($1);  {**< 0, 0, 0, 0 *}
  SDL_BLENDFACTOR_ONE                 = TSDL_BlendFactor($2);  {**< 1, 1, 1, 1 *}
  SDL_BLENDFACTOR_SRC_COLOR           = TSDL_BlendFactor($3);  {**< srcR, srcG, srcB, srcA *}
  SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR = TSDL_BlendFactor($4);  {**< 1-srcR, 1-srcG, 1-srcB, 1-srcA *}
  SDL_BLENDFACTOR_SRC_ALPHA           = TSDL_BlendFactor($5);  {**< srcA, srcA, srcA, srcA *}
  SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA = TSDL_BlendFactor($6);  {**< 1-srcA, 1-srcA, 1-srcA, 1-srcA *}
  SDL_BLENDFACTOR_DST_COLOR           = TSDL_BlendFactor($7);  {**< dstR, dstG, dstB, dstA *}
  SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR = TSDL_BlendFactor($8);  {**< 1-dstR, 1-dstG, 1-dstB, 1-dstA *}
  SDL_BLENDFACTOR_DST_ALPHA           = TSDL_BlendFactor($9);  {**< dstA, dstA, dstA, dstA *}
  SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA = TSDL_BlendFactor($A);  {**< 1-dstA, 1-dstA, 1-dstA, 1-dstA *}

{**
 *  \brief Create a custom blend mode, which may or may not be supported by a given renderer
 *
 *  \param srcColorFactor source color factor
 *  \param dstColorFactor destination color factor
 *  \param colorOperation color operation
 *  \param srcAlphaFactor source alpha factor
 *  \param dstAlphaFactor destination alpha factor
 *  \param alphaOperation alpha operation
 *
 *  The result of the blend mode operation will be:
 *      dstRGB = dstRGB * dstColorFactor colorOperation srcRGB * srcColorFactor
 *  and
 *      dstA = dstA * dstAlphaFactor alphaOperation srcA * srcAlphaFactor
 *}
Function SDL_ComposeCustomBlendMode(srcColorFactor, dstColorFactor : TSDL_BlendFactor; colorOperation : TSDL_BlendOperation; srcAlphaFactor, dstAlphaFactor : TSDL_BlendFactor; alphaOperation : TSDL_BlendOperation) : TSDL_BlendMode cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ComposeCustomBlendMode' {$ENDIF} {$ENDIF};
