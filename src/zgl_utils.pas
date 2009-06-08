{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.ru
 *
 * This file is part of ZenGL
 *
 * ZenGL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * ZenGL is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_utils;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF LINUX_OR_DARWIN}
  Unix,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll,
  {$ENDIF}
  zgl_log
  ;

function u_IntToStr( const Value : Integer ) : String;
function u_StrToInt( const Value : String ) : Integer;
function u_BoolToStr( const Value : Boolean ) : String;
function u_StrToBool( const Value : String ) : Boolean;

// Только для английских символов попадающих в диапазон 0..127
function u_StrUp( const str : String ) : String;
function u_StrDown( const str : String ) : String;
// Удаляет один символ из utf8-строки
procedure u_Backspace( var str : String );
// Возвращает количество символов в utf8-строке
function  u_Length( const str : String ) : Integer;
// Возвращает количество слов, разделеных разделителем d
function  u_Words( const str : String; const d : Char = ' ' ) : Integer;
function  u_GetWord( const Str : String; const n : Integer; const d : Char = ' ' ) : String;

procedure u_Error( const errStr : String );
procedure u_Warning( const errStr : String );

function u_GetPOT( const v : Integer ) : Integer;

procedure u_Sleep( const msec : DWORD );

{$IFDEF LINUX_OR_DARWIN}
function dlopen ( Name : PChar; Flags : longint) : Pointer; cdecl; external 'dl';
function dlclose( Lib : Pointer) : Longint; cdecl; external 'dl';
function dlsym  ( Lib : Pointer; Name : Pchar) : Pointer; cdecl; external 'dl';

function select( n : longint; readfds, writefds, exceptfds : Pointer; var timeout : timeVal ):longint;cdecl;external 'libc';
function getenv(_para1:Pchar):Pchar;cdecl;external 'libc' name 'getenv';
{$ENDIF}

{$IFDEF WIN32}
function dlopen ( lpLibFileName : PAnsiChar) : HMODULE; stdcall; external 'kernel32.dll' name 'LoadLibraryA';
function dlclose( hLibModule : HMODULE ) : Boolean; stdcall; external 'kernel32.dll' name 'FreeLibrary';
function dlsym  ( hModule : HMODULE; lpProcName : PAnsiChar) : Pointer; stdcall; external 'kernel32.dll' name 'GetProcAddress';
{$ENDIF}

implementation
uses
  zgl_font;

function u_IntToStr;
begin
  Str( Value, Result );
end;

function u_StrToInt;
  var
    E : Integer;
begin
  Val( Value, Result, E );
end;

function u_BoolToStr;
begin
  if Value Then
    Result := 'TRUE'
  else
    Result := 'FALSE';
end;

function u_StrToBool;
begin
  if Value = '1' Then
    Result := TRUE
  else
    if u_StrUp( Value ) = 'TRUE' Then
      Result := TRUE
    else
      Result := FALSE;
end;

function u_StrUp;
  var
    i, l : Integer;
begin
  l := length( Str );
  SetLength( Result, l );
  for i := 1 to l do
    if ( Byte( Str[ i ] ) >= 97 ) and ( Byte( Str[ i ] ) <= 122 ) Then
      Result[ i ] := Chr( Byte( Str[ i ] ) - 32 )
    else
      Result[ i ] := Str[ i ];
end;

function u_StrDown;
  var
    i, l : Integer;
begin
  l := length( Str );
  SetLength( Result, l );
  for i := 1 to l do
    if ( Byte( Str[ i ] ) >= 65 ) and ( Byte( Str[ i ] ) <= 90 ) Then
      Result[ i ] := Chr( Byte( Str[ i ] ) + 32 )
    else
      Result[ i ] := Str[ i ];
end;

procedure u_Backspace;
  var
    i, last : Integer;
begin
  if str = '' Then exit;
  i := 0;
  last := 0;
  while i <= length( str ) do
    begin
      last := i;
      font_GetUID( str, last, @i );
    end;

  SetLength( str, last - 1 )
end;

function u_Length;
  var
    i : Integer;
begin
  Result := 0;
  i := 0;
  while i < length( str ) do
    begin
      INC( Result );
      font_GetUID( str, i, @i );
    end;
end;

function u_Words;
  var
    i : Integer;
begin
  Result := 0;
  if Str = '' Then exit;
  for i := 1 to length( Str ) do
    if Str[ i ] = d Then
      INC( Result );
  if Str[ 1 ] <> ' ' Then
    INC( Result );
end;

function u_GetWord;
  label b;
  var
    i, p : Integer;
begin
  i := 0;
  Result := d + Str;

b:
  INC( i );
  p := Pos( d, Result );
  while Result[ p ] = d do Delete( Result, p, 1 );

  p := Pos( d, Result );
  if n > i Then
    begin
      Delete( Result, 1, p - 1 );
      goto b;
    end;

  Delete( Result, p, length( Result ) - p + 1 );
end;

procedure u_Error;
  {$IFDEF DARWIN}
  var
    outItemHit: SInt16;
  {$ENDIF}
begin
{$IFDEF LINUX}
  WriteLn( 'ERROR: ' + errStr );
{$ENDIF}
{$IFDEF WIN32}
  MessageBox( 0, PChar( errStr ), 'ERROR!', MB_OK or MB_ICONERROR );
{$ENDIF}
{$IFDEF DARWIN}
  StandardAlert( kAlertNoteAlert, 'ERROR!', errStr, nil, outItemHit );
{$ENDIF}

  log_Add( 'ERROR: ' + errStr );
end;

procedure u_Warning;
  {$IFDEF DARWIN}
  var
    outItemHit: SInt16;
  {$ENDIF}
begin
{$IFDEF LINUX}
  WriteLn( 'WARNING: ' + errStr );
{$ENDIF}
{$IFDEF WIN32}
  MessageBox( 0, PChar( errStr ), 'WARNING!', MB_OK or MB_ICONWARNING );
{$ENDIF}
{$IFDEF DARWIN}
  StandardAlert( kAlertNoteAlert, 'WARNING!', errStr, nil, outItemHit );
{$ENDIF}

  log_Add( 'WARNING: ' + errStr );
end;

function u_GetPOT;
begin
  Result := v - 1;
  Result := Result or ( Result shr 1 );
  Result := Result or ( Result shr 2 );
  Result := Result or ( Result shr 4 );
  Result := Result or ( Result shr 8 );
  Result := Result or ( Result shr 16 );
  Result := Result + 1;
end;

procedure u_Sleep;
  {$IFDEF LINUX_OR_DARWIN}
  var
    tv : TimeVal;
  {$ENDIF}
begin
{$IFDEF LINUX_OR_DARWIN}
  tv.tv_sec  := msec div 1000;
  tv.tv_usec := ( msec mod 1000 ) * 1000;
  select( 0, nil, nil, nil, tv );
{$ENDIF}
{$IFDEF WIN32}
  Sleep( msec );
{$ENDIF}
end;

end.
