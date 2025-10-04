//from "sdl_shape.h"

{**  SDL_shape.h
 *
 * Header file for the shaped window API.
 *}
const
  SDL_NONSHAPEABLE_WINDOW = -1;
  SDL_INVALID_SHAPE_ARGUMENT = -2;
  SDL_WINDOW_LACKS_SHAPE = -3;

type
  PPSDL_Window = ^PSDL_Window;
  PSDL_Window = ^TSDL_Window;

  {** An enum denoting the specific type of contents present in an SDL_WindowShapeParams union. *}
  TWindowShapeMode = ({** The default mode, a binarized alpha cutoff of 1. *}
                      ShapeModeDefault,
                      {** A binarized alpha cutoff with a given integer value. *}
                      ShapeModeBinarizeAlpha,
                      {** A binarized alpha cutoff with a given integer value, but with the opposite comparison. *}
                      ShapeModeReverseBinarizeAlpha,
                      {** A color key is applied. *}
                      ShapeModeColorKey);

//#define SDL_SHAPEMODEALPHA(mode) (mode == ShapeModeDefault || mode == ShapeModeBinarizeAlpha || mode == ShapeModeReverseBinarizeAlpha)

  {** A union containing parameters for shaped windows. *}
  TSDL_WindowShapeParams = record
    case Integer of
      {** a cutoff alpha value for binarization of the window shape's alpha channel. *}
      0: (binarizationCutoff: UInt8;);
      1: (colorKey: TSDL_Color;);
  end;

  {** A struct that tags the SDL_WindowShapeParams union with an enum describing the type of its contents. *}
  PSDL_WindowShapeMode = ^TSDL_WindowShapeMode;
  TSDL_WindowShapeMode = record
    {** The mode of these window-shape parameters. *}
    mode: TWindowShapeMode;
    {** Window-shape parameters. *}
    parameters: TSDL_WindowShapeParams;
  end;
