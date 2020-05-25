//from "sdl_rect.h"

type
  {**
   *  The structure that defines a point
   *
   *  SDL_EnclosePoints
   *}

  PSDL_Point = ^TSDL_Point;
  TSDL_Point = record
    x: SInt32;
    y: SInt32;
  end;

  {**
   *  A rectangle, with the origin at the upper left.
   *
   *  SDL_RectEmpty
   *  SDL_RectEquals
   *  SDL_HasIntersection
   *  SDL_IntersectRect
   *  SDL_UnionRect
   *  SDL_EnclosePoints
   *}

  PSDL_Rect = ^TSDL_Rect;
  TSDL_Rect = record
    x,y: SInt32;
    w,h: SInt32;
  end;

  {**
   *  \brief Returns true if point resides inside a rectangle.
   *}
function SDL_PointInRect(const p: PSDL_Point; const r: PSDL_Rect): Boolean; Inline;

  {**
   *  Returns true if the rectangle has no area.
   *}

  //changed from variant(b�����h!) to TSDL_Rect
  //maybe PSDL_Rect?
function SDL_RectEmpty(const r: PSDL_Rect): Boolean; inline;

    {**
     *  Returns true if the two rectangles are equal.
     *}

function SDL_RectEquals(const a, b: PSDL_Rect): Boolean; inline;

  {**
   *  Determine whether two rectangles intersect.
   *
   *  SDL_TRUE if there is an intersection, SDL_FALSE otherwise.
   *}

function SDL_HasIntersection(const a, b: PSDL_Rect): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasIntersection' {$ENDIF} {$ENDIF};

  {**
   *  Calculate the intersection of two rectangles.
   *
   *  SDL_TRUE if there is an intersection, SDL_FALSE otherwise.
   *}

function SDL_IntersectRect(const A, B: PSDL_Rect; result: PSDL_Rect): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_IntersectRect' {$ENDIF} {$ENDIF};

  {**
   *  Calculate the union of two rectangles.
   *}

procedure SDL_UnionRect(const A, B: PSDL_Rect; result: PSDL_Rect) cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_UnionRect' {$ENDIF} {$ENDIF};

  {**
   *  Calculate a minimal rectangle enclosing a set of points
   *
   *  SDL_TRUE if any points were within the clipping rect
   *}

function SDL_EnclosePoints(const points: PSDL_Point; count: SInt32; const clip: PSDL_Rect; result: PSDL_Rect): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_EnclosePoints' {$ENDIF} {$ENDIF};

  {**
   *  Calculate the intersection of a rectangle and line segment.
   *
   *  SDL_TRUE if there is an intersection, SDL_FALSE otherwise.
   *}

function SDL_IntersectRectAndLine(const rect: PSDL_Rect; X1, Y1, X2, Y2: PInt): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_IntersectRectAndLine' {$ENDIF} {$ENDIF};
