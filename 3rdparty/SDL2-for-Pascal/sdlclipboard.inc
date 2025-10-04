
{* Function prototypes *}

{**
 * \brief Put UTF-8 text into the clipboard
 *
 * \sa SDL_GetClipboardText()
 *}
function SDL_SetClipboardText(const text: PAnsiChar): Integer cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_SetClipboardText' {$ENDIF} {$ENDIF};

{**
 * \brief Get UTF-8 text from the clipboard, which must be freed with SDL_free()
 *
 * \sa SDL_SetClipboardText()
 *}
function SDL_GetClipboardText(): PAnsiChar cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_GetClipboardText' {$ENDIF} {$ENDIF};

{**
 * \brief Returns a flag indicating whether the clipboard exists and contains a text string that is non-empty
 *
 * \sa SDL_GetClipboardText()
 *}
function SDL_HasClipboardText(): TSDL_Bool cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_HasClipboardText' {$ENDIF} {$ENDIF};

