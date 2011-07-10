{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://andru-kun.inf.ua
 *
 *  This file is part of ZenGL.
 *
 *  ZenGL is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *  published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *
 *  ZenGL is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with ZenGL. If not, see http://www.gnu.org/licenses/
}
unit zgl_export_c;

{$I zgl_config.cfg}

interface
uses
  zgl_main,
  zgl_window,
  zgl_log,
  zgl_file,
  zgl_memory,
  zgl_keyboard,
  zgl_textures,
  zgl_font,
  zgl_text,
  zgl_sound,
  zgl_math_2d,
  zgl_utils;

procedure _wnd_SetCaption( NewCaption : PChar );

procedure _log_Add( Message : PChar; Timings : Boolean = TRUE );

procedure _key_BeginReadText( Text : PChar; MaxSymbols : Integer = -1 );
procedure _key_UpdateReadText( Text : PChar; MaxSymbols : Integer = -1 );

function _tex_LoadFromFile( FileName : PChar; TransparentColor : LongWord = $FF000000; Flags : LongWord = TEX_DEFAULT_2D ) : zglPTexture;
function _tex_LoadFromMemory( Memory : zglTMemory; Extension : PChar; TransparentColor : LongWord = $FF000000; Flags : LongWord = TEX_DEFAULT_2D ) : zglPTexture;

function _font_LoadFromFile( FileName : PChar ) : zglPFont;

procedure _text_Draw( Font : zglPFont; X, Y : Single; Text : PChar; Flags : LongWord = 0 );
procedure _text_DrawEx( Font : zglPFont; X, Y, Scale, Step : Single; Text : PChar; Alpha : Byte = 255; Color : LongWord = $FFFFFF; Flags : LongWord = 0 );
procedure _text_DrawInRect( Font : zglPFont; Rect : zglTRect; Text : PChar; Flags : LongWord = 0 );
procedure _text_DrawInRectEx( Font : zglPFont; Rect : zglTRect; Scale, Step : Single; Text : PChar; Alpha : Byte = 0; Color : LongWord = $FFFFFF; Flags : LongWord = 0 );
function  _text_GetWidth( Font : zglPFont; Text : PChar; Step : Single = 0.0 ) : Single;
function  _text_GetHeight( Font : zglPFont; Width : Single; Text : PChar; Scale : Single = 1.0; Step : Single = 0.0 ) : Single;

function _snd_LoadFromFile( FileName : PChar; SourceCount : Integer = 8 ) : zglPSound;
function _snd_LoadFromMemory( Memory : zglTMemory; Extension : PChar; SourceCount : Integer = 8 ) : zglPSound;
function _snd_PlayFile( FileName : PChar; Loop : Boolean = FALSE ) : Integer;
function _snd_PlayMemory( Memory : zglTMemory; Extension : PChar; Loop : Boolean = FALSE ) : Integer;

implementation

procedure _wnd_SetCaption( NewCaption : PChar );
begin
  wnd_SetCaption( NewCaption );
end;

procedure _log_Add( Message : PChar; Timings : Boolean = TRUE );
begin
  log_Add( Message, Timings );
end;

procedure _key_BeginReadText( Text : PChar; MaxSymbols : Integer = -1 );
begin
  key_BeginReadText( Text, MaxSymbols );
end;

procedure _key_UpdateReadText( Text : PChar; MaxSymbols : Integer = -1 );
begin
  key_UpdateReadText( Text, MaxSymbols );
end;

function _tex_LoadFromFile( FileName : PChar; TransparentColor : LongWord = $FF000000; Flags : LongWord = TEX_DEFAULT_2D ) : zglPTexture;
begin
  Result := tex_LoadFromFile( FileName, TransparentColor, Flags );
end;

function _tex_LoadFromMemory( Memory : zglTMemory; Extension : PChar; TransparentColor : LongWord = $FF000000; Flags : LongWord = TEX_DEFAULT_2D ) : zglPTexture;
begin
  Result := tex_LoadFromMemory( Memory, Extension, TransparentColor, Flags );
end;

function _font_LoadFromFile( FileName : PChar ) : zglPFont;
begin
  Result := font_LoadFromFile( FileName );
end;

procedure _text_Draw( Font : zglPFont; X, Y : Single; Text : PChar; Flags : LongWord = 0 );
begin
  text_Draw( Font, X, Y, Text, Flags );
end;

procedure _text_DrawEx( Font : zglPFont; X, Y, Scale, Step : Single; Text : PChar; Alpha : Byte = 255; Color : LongWord = $FFFFFF; Flags : LongWord = 0 );
begin
  text_DrawEx( Font, X, Y, Scale, Step, Text, Alpha, Color, Flags );
end;

procedure _text_DrawInRect( Font : zglPFont; Rect : zglTRect; Text : PChar; Flags : LongWord = 0 );
begin
  text_DrawInRect( Font, Rect, Text, Flags );
end;

procedure _text_DrawInRectEx( Font : zglPFont; Rect : zglTRect; Scale, Step : Single; Text : PChar; Alpha : Byte = 0; Color : LongWord = $FFFFFF; Flags : LongWord = 0 );
begin
  text_DrawInRectEx( Font, Rect, Scale, Step, Text, Alpha, Color, Flags );
end;

function  _text_GetWidth( Font : zglPFont; Text : PChar; Step : Single = 0.0 ) : Single;
begin
  Result := text_GetWidth( Font, Text, Step );
end;

function  _text_GetHeight( Font : zglPFont; Width : Single; Text : PChar; Scale : Single = 1.0; Step : Single = 0.0 ) : Single;
begin
  Result := text_GetHeight( Font, Width, Text, Scale, Step );
end;

function _snd_LoadFromFile( FileName : PChar; SourceCount : Integer = 8 ) : zglPSound;
begin
  Result := snd_LoadFromFile( FileName, SourceCount );
end;

function _snd_LoadFromMemory( Memory : zglTMemory; Extension : PChar; SourceCount : Integer = 8 ) : zglPSound;
begin
  Result := snd_LoadFromMemory( Memory, Extension, SourceCount );
end;

function _snd_PlayFile( FileName : PChar; Loop : Boolean = FALSE ) : Integer;
begin
  Result := snd_PlayFile( FileName, Loop );
end;

function _snd_PlayMemory( Memory : zglTMemory; Extension : PChar; Loop : Boolean = FALSE ) : Integer;
begin
  Result := snd_PlayMemory( Memory, Extension, Loop );
end;

end.
