//from "sdl_renderer.h"

  {**
   *  Flags used when creating a rendering context
   *}
const
  SDL_RENDERER_SOFTWARE = $00000001;          {**< The renderer is a software fallback *}
  SDL_RENDERER_ACCELERATED = $00000002;       {**< The renderer uses hardware
                                                   acceleration *}
  SDL_RENDERER_PRESENTVSYNC = $00000004;      {**< Present is synchronized
                                                   with the refresh rate *}
  SDL_RENDERER_TARGETTEXTURE = $00000008;     {**< The renderer supports
                                                   rendering to texture *}

type
  PSDL_RendererFlags = ^TSDL_RendererFlags;
  TSDL_RendererFlags = Word;

  {**
   *  Information on the capabilities of a render driver or context.
   *}
  PSDL_RendererInfo = ^TSDL_RendererInfo;
  TSDL_RendererInfo = record  
    name: PAnsiChar;                         {**< The name of the renderer *}
    flags: UInt32;                           {**< Supported ::SDL_RendererFlags *}
    num_texture_formats: UInt32;             {**< The number of available texture formats *}
    texture_formats: array[0..15] of UInt32; {**< The available texture formats *}
    max_texture_width: SInt32;               {**< The maximimum texture width *}
    max_texture_height: SInt32;              {**< The maximimum texture height *}
  end;

  {**
   *  The access pattern allowed for a texture.
   *}
type
  PSDL_TextureAccess = ^TSDL_TextureAccess;
  TSDL_TextureAccess = SInt32;

const
  SDL_TEXTUREACCESS_STATIC    = 0; {**< Changes rarely, not lockable *}
  SDL_TEXTUREACCESS_STREAMING = 1; {**< Changes frequently, lockable *}
  SDL_TEXTUREACCESS_TARGET    = 2; {**< Texture can be used as a render target *}

type
  {**
   *  The texture channel modulation used in SDL_RenderCopy().
   *}
  PSDL_TextureModulate = ^TSDL_TextureModulate;
  TSDL_TextureModulate = (
                          SDL_TEXTUREMODULATE_NONE,     {**< No modulation *}
                          SDL_TEXTUREMODULATE_COLOR,    {**< srcC = srcC * color *}
                          SDL_TEXTUREMODULATE_ALPHA     {**< srcA = srcA * alpha *}
                          );

  {**
   *  Flip constants for SDL_RenderCopyEx
   *}
const
  SDL_FLIP_NONE       = $0; {**< Do not flip *}
  SDL_FLIP_HORIZONTAL = $1; {**< flip horizontally *}
  SDL_FLIP_VERTICAL   = $2; {**< flip vertically *}

type
  {**
   *  A structure representing rendering state
   *}

  PPSDL_Renderer = ^PSDL_Renderer;
  PSDL_Renderer = ^TSDL_Renderer;
  TSDL_Renderer = record
    end;

  {**
   *  An efficient driver-specific representation of pixel data
   *}
  PSDL_Texture = ^TSDL_Texture;
  TSDL_Texture = record
    end;

  {* Function prototypes *}

  {**
   *  Get the number of 2D rendering drivers available for the current
   *  display.
   *
   *  A render driver is a set of code that handles rendering and texture
   *  management on a particular display.  Normally there is only one, but
   *  some drivers may have several available with different capabilities.
   *
   *   SDL_GetRenderDriverInfo()
   *   SDL_CreateRenderer()
   *}
function SDL_GetNumRenderDrivers: SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetNumRenderDrivers' {$ENDIF} {$ENDIF};

  {**
   *  Get information about a specific 2D rendering driver for the current
   *  display.
   *
   *   index The index of the driver to query information about.
   *   info  A pointer to an SDL_RendererInfo struct to be filled with
   *               information on the rendering driver.
   *
   *   0 on success, -1 if the index was out of range.
   *
   *   SDL_CreateRenderer()
   *}
function SDL_GetRenderDriverInfo(index: SInt32; info: PSDL_RendererInfo): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetRenderDriverInfo' {$ENDIF} {$ENDIF};

  {**
   *  Create a window and default renderer
   *
   *   width    The width of the window
   *   height   The height of the window
   *   window_flags The flags used to create the window
   *   window   A pointer filled with the window, or NULL on error
   *   renderer A pointer filled with the renderer, or NULL on error
   *
   *   0 on success, or -1 on error
   *}
function SDL_CreateWindowAndRenderer(width: SInt32; height: SInt32; window_flags: UInt32; window: PPSDL_Window; renderer: PPSDL_Renderer): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateWindowAndRenderer' {$ENDIF} {$ENDIF};

  {**
   *  Create a 2D rendering context for a window.
   *
   *   window The window where rendering is displayed.
   *   index    The index of the rendering driver to initialize, or -1 to
   *                  initialize the first one supporting the requested flags.
   *   flags    ::SDL_RendererFlags.
   *
   *   A valid rendering context or NULL if there was an error.
   *
   *   SDL_CreateSoftwareRenderer()
   *   SDL_GetRendererInfo()
   *   SDL_DestroyRenderer()
   *}
function SDL_CreateRenderer(window: PSDL_Window; index: SInt32; flags: UInt32): PSDL_Renderer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateRenderer' {$ENDIF} {$ENDIF};

  {**
   *  Create a 2D software rendering context for a surface.
   *
   *   surface The surface where rendering is done.
   *
   *   A valid rendering context or NULL if there was an error.
   *
   *   SDL_CreateRenderer()
   *   SDL_DestroyRenderer()
   *}
function SDL_CreateSoftwareRenderer(surface: PSDL_Surface): PSDL_Renderer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateSoftwareRenderer' {$ENDIF} {$ENDIF};

  {**
   *  Get the renderer associated with a window.
   *}
function SDL_GetRenderer(window: PSDL_Window): PSDL_Renderer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetRenderer' {$ENDIF} {$ENDIF};

  {**
   *  Get information about a rendering context.
   *}
function SDL_GetRendererInfo(renderer: PSDL_Renderer; info: PSDL_RendererInfo): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetRendererInfo' {$ENDIF} {$ENDIF};

  {**
   *  Get the output size of a rendering context.
   *}
function SDL_GetRendererOutputSize(renderer: PSDL_Renderer; w: PInt; h: PInt): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetRendererOutputSize' {$ENDIF} {$ENDIF};

  {**
   *  Create a texture for a rendering context.
   *
   *   renderer The renderer.
   *   format The format of the texture.
   *   access One of the enumerated values in ::SDL_TextureAccess.
   *   w      The width of the texture in pixels.
   *   h      The height of the texture in pixels.
   *
   *   The created texture is returned, or 0 if no rendering context was
   *   active,  the format was unsupported, or the width or height were out
   *   of range.
   *
   *  SDL_QueryTexture()
   *  SDL_UpdateTexture()
   *  SDL_DestroyTexture()
   *}
function SDL_CreateTexture(renderer: PSDL_Renderer; format: UInt32; access: SInt32; w: SInt32; h: SInt32): PSDL_Texture cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateTexture' {$ENDIF} {$ENDIF};

  {**
   *  Create a texture from an existing surface.
   *
   *   renderer The renderer.
   *   surface The surface containing pixel data used to fill the texture.
   *
   *   The created texture is returned, or 0 on error.
   *
   *   The surface is not modified or freed by this function.
   *
   *   SDL_QueryTexture()
   *   SDL_DestroyTexture()
   *}
function SDL_CreateTextureFromSurface(renderer: PSDL_Renderer; surface: PSDL_Surface): PSDL_Texture cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_CreateTextureFromSurface' {$ENDIF} {$ENDIF};

  {**
   *  Query the attributes of a texture
   *
   *   texture A texture to be queried.
   *   format  A pointer filled in with the raw format of the texture.  The
   *           actual format may differ, but pixel transfers will use this
   *           format.
   *   access  A pointer filled in with the actual access to the texture.
   *   w       A pointer filled in with the width of the texture in pixels.
   *   h       A pointer filled in with the height of the texture in pixels.
   *
   *   0 on success, or -1 if the texture is not valid.
   *}
function SDL_QueryTexture(texture: PSDL_Texture; format: PUInt32; access: PInt; w: PInt; h: PInt): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_QueryTexture' {$ENDIF} {$ENDIF};

  {**
   *  Set an additional color value used in render copy operations.
   *
   *   texture The texture to update.
   *   r       The red color value multiplied into copy operations.
   *   g       The green color value multiplied into copy operations.
   *   b       The blue color value multiplied into copy operations.
   *
   *   0 on success, or -1 if the texture is not valid or color modulation
   *   is not supported.
   *
   *   SDL_GetTextureColorMod()
   *}
function SDL_SetTextureColorMod(texture: PSDL_Texture; r: UInt8; g: UInt8; b: UInt8): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetTextureColorMod' {$ENDIF} {$ENDIF};

  {**
   *  Get the additional color value used in render copy operations.
   *
   *   texture The texture to query.
   *   r         A pointer filled in with the current red color value.
   *   g         A pointer filled in with the current green color value.
   *   b         A pointer filled in with the current blue color value.
   *
   *   0 on success, or -1 if the texture is not valid.
   *
   *   SDL_SetTextureColorMod()
   *}
function SDL_GetTextureColorMod(texture: PSDL_Texture; r: PUInt8; g: PUInt8; b: PUInt8): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetTextureColorMod' {$ENDIF} {$ENDIF};

  {**
   *  Set an additional alpha value used in render copy operations.
   *
   *   texture The texture to update.
   *   alpha     The alpha value multiplied into copy operations.
   *
   *   0 on success, or -1 if the texture is not valid or alpha modulation
   *   is not supported.
   *
   *   SDL_GetTextureAlphaMod()
   *}
function SDL_SetTextureAlphaMod(texture: PSDL_Texture; alpha: UInt8): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetTextureAlphaMod' {$ENDIF} {$ENDIF};

  {**
   *  Get the additional alpha value used in render copy operations.
   *
   *   texture The texture to query.
   *   alpha     A pointer filled in with the current alpha value.
   *
   *   0 on success, or -1 if the texture is not valid.
   *
   *   SDL_SetTextureAlphaMod()
   *}
function SDL_GetTextureAlphaMod(texture: PSDL_Texture; alpha: PUInt8): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetTextureAlphaMod' {$ENDIF} {$ENDIF};

  {**
   *   Set the blend mode used for texture copy operations.
   *
   *   texture The texture to update.
   *   blendMode ::SDL_BlendMode to use for texture blending.
   *
   *   0 on success, or -1 if the texture is not valid or the blend mode is
   *   not supported.
   *
   *   If the blend mode is not supported, the closest supported mode is
   *   chosen.
   *
   *   SDL_GetTextureBlendMode()
   *}
function SDL_SetTextureBlendMode(texture: PSDL_Texture; blendMode: TSDL_BlendMode): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetTextureBlendMode' {$ENDIF} {$ENDIF};

  {**
   *  Get the blend mode used for texture copy operations.
   *
   *   texture   The texture to query.
   *   blendMode A pointer filled in with the current blend mode.
   *
   *   0 on success, or -1 if the texture is not valid.
   *
   *   SDL_SetTextureBlendMode()
   *}
function SDL_GetTextureBlendMode(texture: PSDL_Texture; blendMode: PSDL_BlendMode): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetTextureBlendMode' {$ENDIF} {$ENDIF};

  {**
   *  Update the given texture rectangle with new pixel data.
   *
   *   texture   The texture to update
   *   rect      A pointer to the rectangle of pixels to update, or NULL to
   *                   update the entire texture.
   *   pixels    The raw pixel data.
   *   pitch     The number of bytes between rows of pixel data.
   *
   *   0 on success, or -1 if the texture is not valid.
   *
   *   This is a fairly slow function.
   *}
function SDL_UpdateTexture(texture: PSDL_Texture; rect: PSDL_Rect; pixels: Pointer; pitch: SInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_UpdateTexture' {$ENDIF} {$ENDIF};

  {**
   *  Lock a portion of the texture for write-only pixel access.
   *
   *   texture   The texture to lock for access, which was created with
   *             SDL_TEXTUREACCESS_STREAMING.
   *   rect      A pointer to the rectangle to lock for access. If the rect
   *             is NULL, the entire texture will be locked.
   *   pixels    This is filled in with a pointer to the locked pixels,
   *             appropriately offset by the locked area.
   *   pitch     This is filled in with the pitch of the locked pixels.
   *
   *   0 on success, or -1 if the texture is not valid or was not created with ::SDL_TEXTUREACCESS_STREAMING.
   *
   *   SDL_UnlockTexture()
   *}
function SDL_LockTexture(texture: PSDL_Texture; rect: PSDL_Rect; pixels: PPointer; pitch: PInt): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LockTexture' {$ENDIF} {$ENDIF};

  {**
   *  Unlock a texture, uploading the changes to video memory, if needed.
   *
   *   SDL_LockTexture()
   *}
procedure SDL_UnlockTexture(texture: PSDL_Texture) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_LockTexture' {$ENDIF} {$ENDIF};

  {**
   *  Determines whether a window supports the use of render targets
   *
   *  renderer The renderer that will be checked
   *
   *  SDL_TRUE if supported, SDL_FALSE if not.
   *}
function SDL_RenderTargetSupported(renderer: PSDL_Renderer): Boolean cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderTargetSupported' {$ENDIF} {$ENDIF};

  {**
   *  Set a texture as the current rendering target.
   *
   *  renderer The renderer.
   *  texture The targeted texture, which must be created with the SDL_TEXTUREACCESS_TARGET flag, or NULL for the default render target
   *
   *  0 on success, or -1 on error
   *
   *   SDL_GetRenderTarget()
   *}
function SDL_SetRenderTarget(renderer: PSDL_Renderer; texture: PSDL_Texture): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetRenderTarget' {$ENDIF} {$ENDIF};

  {**
   *  Get the current render target or NULL for the default render target.
   *
   *  The current render target
   *
   *   SDL_SetRenderTarget()
   *}
function SDL_GetRenderTarget(renderer: PSDL_Renderer): PSDL_Texture cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetRenderTarget' {$ENDIF} {$ENDIF};

  {**
   *  Set device independent resolution for rendering
   *
   *   renderer The renderer for which resolution should be set.
   *   w      The width of the logical resolution
   *   h      The height of the logical resolution
   *
   *  This function uses the viewport and scaling functionality to allow a fixed logical
   *  resolution for rendering, regardless of the actual output resolution.  If the actual
   *  output resolution doesn't have the same aspect ratio the output rendering will be
   *  centered within the output display.
   *
   *  If the output display is a window, mouse events in the window will be filtered
   *  and scaled so they seem to arrive within the logical resolution.
   *
   *   If this function results in scaling or subpixel drawing by the
   *   rendering backend, it will be handled using the appropriate
   *   quality hints.
   *
   *   SDL_RenderGetLogicalSize()
   *   SDL_RenderSetScale()
   *   SDL_RenderSetViewport()
   *}
function SDL_RenderSetLogicalSize(renderer: PSDL_Renderer; w: SInt32; h: SInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderSetLogicalSize' {$ENDIF} {$ENDIF};

  {**
   *  Get device independent resolution for rendering
   *
   *   renderer The renderer from which resolution should be queried.
   *   w      A pointer filled with the width of the logical resolution
   *   h      A pointer filled with the height of the logical resolution
   *
   *   SDL_RenderSetLogicalSize()
   *}
procedure SDL_RenderGetLogicalSize(renderer: PSDL_Renderer; w: PInt; h: PInt) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderGetLogicalSize' {$ENDIF} {$ENDIF};

{**
 *  \brief Set whether to force integer scales for resolution-independent rendering
 *
 *  \param renderer The renderer for which integer scaling should be set.
 *  \param enable   Enable or disable integer scaling
 *
 *  This function restricts the logical viewport to integer values - that is, when
 *  a resolution is between two multiples of a logical size, the viewport size is
 *  rounded down to the lower multiple.
 *
 *  \sa SDL_RenderSetLogicalSize()
 *}
function SDL_RenderSetIntegerScale(renderer: PSDL_Renderer; enable : TSDL_bool): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderSetIntegerScale' {$ENDIF} {$ENDIF};

{**
 *  \brief Get whether integer scales are forced for resolution-independent rendering
 *
 *  \param renderer The renderer from which integer scaling should be queried.
 *
 *  \sa SDL_RenderSetIntegerScale()
 *}
function SDL_RenderGetIntegerScale(renderer: PSDL_Renderer): TSDL_bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderGetIntegerScale' {$ENDIF} {$ENDIF};

  {**
   *  Set the drawing area for rendering on the current target.
   *
   *   renderer The renderer for which the drawing area should be set.
   *   rect The rectangle representing the drawing area, or NULL to set the viewport to the entire target.
   *
   *  The x,y of the viewport rect represents the origin for rendering.
   *
   *   0 on success, or -1 on error
   *
   *  If the window associated with the renderer is resized, the viewport is automatically reset.
   *
   *   SDL_RenderGetViewport()
   *   SDL_RenderSetLogicalSize()
   *}
function SDL_RenderSetViewport(renderer: PSDL_Renderer; const rect: PSDL_Rect): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderSetViewport' {$ENDIF} {$ENDIF};

  {**
   *  Get the drawing area for the current target.
   *
   *   SDL_RenderSetViewport()
   *}
procedure SDL_RenderGetViewport(renderer: PSDL_Renderer; rect: PSDL_Rect) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderGetViewport' {$ENDIF} {$ENDIF};

  {**
   *  Set the clip rectangle for the current target.
   *
   *   renderer The renderer for which clip rectangle should be set.
   *   rect   A pointer to the rectangle to set as the clip rectangle, or
   *          NULL to disable clipping.
   *
   *   0 on success, or -1 on error
   *
   *   SDL_RenderGetClipRect()
   *}
function SDL_RenderSetClipRect(renderer: PSDL_Renderer; rect: PSDL_Rect): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderSetClipRect' {$ENDIF} {$ENDIF};

  {**
   *  Get the clip rectangle for the current target.
   *
   *   renderer The renderer from which clip rectangle should be queried.
   *   rect   A pointer filled in with the current clip rectangle, or
   *          an empty rectangle if clipping is disabled.
   *
   *   SDL_RenderSetClipRect()
   *}
procedure SDL_RenderGetClipRect(renderer: PSDL_Renderer; rect: PSDL_Rect) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderGetClipRect' {$ENDIF} {$ENDIF};

  {**
   *  \brief Get whether clipping is enabled on the given renderer.
   *
   *  \param renderer The renderer from which clip state should be queried.
   *
   *  \sa SDL_RenderGetClipRect()
   *}
function SDL_RenderIsClipEnabled(renderer: PSDL_Renderer): TSDL_Bool; cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderIsClipEnabled' {$ENDIF} {$ENDIF};

  {**
   *  Set the drawing scale for rendering on the current target.
   *
   *   renderer The renderer for which the drawing scale should be set.
   *   scaleX The horizontal scaling factor
   *   scaleY The vertical scaling factor
   *
   *  The drawing coordinates are scaled by the x/y scaling factors
   *  before they are used by the renderer.  This allows resolution
   *  independent drawing with a single coordinate system.
   *
   *  If this results in scaling or subpixel drawing by the
   *  rendering backend, it will be handled using the appropriate
   *  quality hints.  For best results use integer scaling factors.
   *
   *   SDL_RenderGetScale()
   *   SDL_RenderSetLogicalSize()
   *}
function SDL_RenderSetScale(renderer: PSDL_Renderer; scaleX: Float; scaleY: Float): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderSetScale' {$ENDIF} {$ENDIF};

  {**
   *  Get the drawing scale for the current target.
   *
   *   renderer The renderer from which drawing scale should be queried.
   *   scaleX A pointer filled in with the horizontal scaling factor
   *   scaleY A pointer filled in with the vertical scaling factor
   *
   *   SDL_RenderSetScale()
   *}
procedure SDL_RenderGetScale(renderer: PSDL_Renderer; scaleX: PFloat; scaleY: PFloat) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderGetScale' {$ENDIF} {$ENDIF};

  {**
   *  Set the color used for drawing operations (Rect, Line and Clear).
   *
   *   renderer The renderer for which drawing color should be set.
   *   r The red value used to draw on the rendering target.
   *   g The green value used to draw on the rendering target.
   *   b The blue value used to draw on the rendering target.
   *   a The alpha value used to draw on the rendering target, usually
   *     SDL_ALPHA_OPAQUE (255).
   *
   *   0 on success, or -1 on error
   *}
function SDL_SetRenderDrawColor(renderer: PSDL_Renderer; r: UInt8; g: UInt8; b: UInt8; a: UInt8): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetRenderDrawColor' {$ENDIF} {$ENDIF};

  {**
   *  Get the color used for drawing operations (Rect, Line and Clear).
   *
   *   renderer The renderer from which drawing color should be queried.
   *   r A pointer to the red value used to draw on the rendering target.
   *   g A pointer to the green value used to draw on the rendering target.
   *   b A pointer to the blue value used to draw on the rendering target.
   *   a A pointer to the alpha value used to draw on the rendering target,
   *     usually SDL_ALPHA_OPAQUE (255).
   *
   *   0 on success, or -1 on error
   *}
function SDL_GetRenderDrawColor(renderer: PSDL_Renderer; r: PUInt8; g: PUInt8; b: PUInt8; a: PUInt8): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetRenderDrawColor' {$ENDIF} {$ENDIF};

  {**
   *  Set the blend mode used for drawing operations (Fill and Line).
   *
   *   renderer The renderer for which blend mode should be set.
   *   blendMode SDL_BlendMode to use for blending.
   *
   *   0 on success, or -1 on error
   *
   *   If the blend mode is not supported, the closest supported mode is
   *        chosen.
   *
   *   SDL_GetRenderDrawBlendMode()
   *}
function SDL_SetRenderDrawBlendMode(renderer: PSDL_Renderer; blendMode: TSDL_BlendMode): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetRenderDrawBlendMode' {$ENDIF} {$ENDIF};

  {**
   *  Get the blend mode used for drawing operations.
   *
   *   renderer The renderer from which blend mode should be queried.
   *   blendMode A pointer filled in with the current blend mode.
   *
   *   0 on success, or -1 on error
   *
   *   SDL_SetRenderDrawBlendMode()
   *}
function SDL_GetRenderDrawBlendMode(renderer: PSDL_Renderer; blendMode: PSDL_BlendMode): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetRenderDrawBlendMode' {$ENDIF} {$ENDIF};

  {**
   *  Clear the current rendering target with the drawing color
   *
   *  This function clears the entire rendering target, ignoring the viewport.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderClear(renderer: PSDL_Renderer): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderClear' {$ENDIF} {$ENDIF};

  {**
   *  Draw a point on the current rendering target.
   *
   *   renderer The renderer which should draw a point.
   *   x The x coordinate of the point.
   *   y The y coordinate of the point.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderDrawPoint(renderer: PSDL_Renderer; x: SInt32; y: SInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderDrawPoint' {$ENDIF} {$ENDIF};

  {**
   *  Draw multiple points on the current rendering target.
   *
   *   renderer The renderer which should draw multiple points.
   *   points The points to draw
   *   count The number of points to draw
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderDrawPoints(renderer: PSDL_Renderer; points: PSDL_Point; count: SInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderDrawPoints' {$ENDIF} {$ENDIF};

  {**
   *  Draw a line on the current rendering target.
   *
   *   renderer The renderer which should draw a line.
   *   x1 The x coordinate of the start point.
   *   y1 The y coordinate of the start point.
   *   x2 The x coordinate of the end point.
   *   y2 The y coordinate of the end point.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderDrawLine(renderer: PSDL_Renderer; x1: SInt32; y1: SInt32; x2: SInt32; y2: SInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderDrawLine' {$ENDIF} {$ENDIF};

  {**
   *  \brief Draw a series of connected lines on the current rendering target.
   *
   *  \param renderer The renderer which should draw multiple lines.
   *  \param points The points along the lines
   *  \param count The number of points, drawing count-1 lines
   *
   *  \return 0 on success, or -1 on error
   *}
function SDL_RenderDrawLines(renderer: PSDL_Renderer; points: PSDL_Point; count: SInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderDrawLines' {$ENDIF} {$ENDIF};

  {**
   *  Draw a rectangle on the current rendering target.
   *
   *   renderer The renderer which should draw a rectangle.
   *   rect A pointer to the destination rectangle, or NULL to outline the entire rendering target.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderDrawRect(renderer: PSDL_Renderer; rect: PSDL_Rect): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderDrawRect' {$ENDIF} {$ENDIF};

  {**
   *  Draw some number of rectangles on the current rendering target.
   *
   *   renderer The renderer which should draw multiple rectangles.
   *   rects A pointer to an array of destination rectangles.
   *   count The number of rectangles.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderDrawRects(renderer: PSDL_Renderer; rects: PSDL_Rect; count: SInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderDrawRects' {$ENDIF} {$ENDIF};

  {**
   *  Fill a rectangle on the current rendering target with the drawing color.
   *
   *   renderer The renderer which should fill a rectangle.
   *   rect A pointer to the destination rectangle, or NULL for the entire
   *        rendering target.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderFillRect(renderer: PSDL_Renderer; rect: PSDL_Rect): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderFillRect' {$ENDIF} {$ENDIF};

  {**
   *  Fill some number of rectangles on the current rendering target with the drawing color.
   *
   *   renderer The renderer which should fill multiple rectangles.
   *   rects A pointer to an array of destination rectangles.
   *   count The number of rectangles.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderFillRects(renderer: PSDL_Renderer; rects: PSDL_Rect; count: SInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderFillRects' {$ENDIF} {$ENDIF};

  {**
   *  Copy a portion of the texture to the current rendering target.
   *
   *   renderer The renderer which should copy parts of a texture.
   *   texture The source texture.
   *   srcrect   A pointer to the source rectangle, or NULL for the entire
   *             texture.
   *   dstrect   A pointer to the destination rectangle, or NULL for the
   *             entire rendering target.
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderCopy(renderer: PSDL_Renderer; texture: PSDL_Texture; srcrect: PSDL_Rect; dstrect: PSDL_Rect): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderCopy' {$ENDIF} {$ENDIF};

  {**
   *  Copy a portion of the source texture to the current rendering target, rotating it by angle around the given center
   *
   *   renderer The renderer which should copy parts of a texture.
   *   texture The source texture.
   *   srcrect   A pointer to the source rectangle, or NULL for the entire
   *                   texture.
   *   dstrect   A pointer to the destination rectangle, or NULL for the
   *                   entire rendering target.
   *   angle    An angle in degrees that indicates the rotation that will be applied to dstrect
   *   center   A pointer to a point indicating the point around which dstrect will be rotated (if NULL, rotation will be done aroud dstrect.w/2, dstrect.h/2)
   *   flip     An SDL_RendererFlip value stating which flipping actions should be performed on the texture
   *
   *   0 on success, or -1 on error
   *}
function SDL_RenderCopyEx(renderer: PSDL_Renderer; texture: PSDL_Texture; const srcrect: PSDL_Rect; dstrect: PSDL_Rect; angle: Double; center: PSDL_Point; flip: Integer): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderCopyEx' {$ENDIF} {$ENDIF};

  {**
   *  Read pixels from the current rendering target.
   *
   *   renderer The renderer from which pixels should be read.
   *   rect   A pointer to the rectangle to read, or NULL for the entire
   *                render target.
   *   format The desired format of the pixel data, or 0 to use the format
   *                of the rendering target
   *   pixels A pointer to be filled in with the pixel data
   *   pitch  The pitch of the pixels parameter.
   *
   *   0 on success, or -1 if pixel reading is not supported.
   *
   *   This is a very slow operation, and should not be used frequently.
   *}
function SDL_RenderReadPixels(renderer: PSDL_Renderer; rect: PSDL_Rect; format: UInt32; pixels: Pointer; pitch: SInt32): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderReadPixels' {$ENDIF} {$ENDIF};

  {**
   *  Update the screen with rendering performed.
   *}
procedure SDL_RenderPresent(renderer: PSDL_Renderer) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_RenderPresent' {$ENDIF} {$ENDIF};

  {**
   *  Destroy the specified texture.
   *
   *   SDL_CreateTexture()
   *   SDL_CreateTextureFromSurface()
   *}
procedure SDL_DestroyTexture(texture: PSDL_Texture) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_DestroyTexture' {$ENDIF} {$ENDIF};

  {**
   *  Destroy the rendering context for a window and free associated
   *  textures.
   *
   *   SDL_CreateRenderer()
   *}
procedure SDL_DestroyRenderer(renderer: PSDL_Renderer) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_DestroyRenderer' {$ENDIF} {$ENDIF};

  {**
   *  Bind the texture to the current OpenGL/ES/ES2 context for use with
   *  OpenGL instructions.
   *
   *   texture  The SDL texture to bind
   *   texw     A pointer to a float that will be filled with the texture width
   *   texh     A pointer to a float that will be filled with the texture height
   *
   *   0 on success, or -1 if the operation is not supported
   *}
function SDL_GL_BindTexture(texture: PSDL_Texture; texw: PFloat; texh: PFloat): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GL_BindTexture' {$ENDIF} {$ENDIF};

  {**
   *  Unbind a texture from the current OpenGL/ES/ES2 context.
   *
   *   texture  The SDL texture to unbind
   *
   *   0 on success, or -1 if the operation is not supported
   *}
function SDL_GL_UnbindTexture(texture: PSDL_Texture): SInt32 cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GL_UnbindTexture' {$ENDIF} {$ENDIF};

  {**
   *  Update a rectangle within a planar YV12 or IYUV texture with new pixel data.
   *
   *  texture   The texture to update
   *  rect      A pointer to the rectangle of pixels to update, or NULL to update the entire texture.
   *  Yplane    The raw pixel data for the Y plane.
   *  Ypitch    The number of bytes between rows of pixel data for the Y plane.
   *  Uplane    The raw pixel data for the U plane.
   *  Upitch    The number of bytes between rows of pixel data for the U plane.
   *  Vplane    The raw pixel data for the V plane.
   *  Vpitch    The number of bytes between rows of pixel data for the V plane.
   *
   *  0 on success, or -1 if the texture is not valid.
   *
   *  You can use SDL_UpdateTexture() as long as your pixel data is
   *  a contiguous block of Y and U/V planes in the proper order, but
   *  this function is available if your pixel data is not contiguous.
   *}
function SDL_UpdateYUVTexture(texture: PSDL_Texture; rect: PSDL_Rect; Yplane: PUInt8; Ypitch: SInt32; Uplane: PUInt8; UPitch: SInt32; Vplane: UInt8; VPitch: SInt32):SInt32;
   cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_UpdateYUVTexture' {$ENDIF} {$ENDIF};
