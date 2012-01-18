{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://zengl.org
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
unit zgl_utils;

{$I zgl_config.cfg}
{$IFDEF iOS}
  {$modeswitch objectivec1}
{$ENDIF}

interface
uses
  {$IFDEF UNIX}
  UnixType,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF MACOSX}
  MacOSAll,
  {$ENDIF}
  {$IFDEF iOS}
  iPhoneAll, CFString,
  {$ENDIF}
  zgl_types;

const
  LIB_ERROR  = {$IFDEF UNIX} nil {$ELSE} 0 {$ENDIF};

function u_IntToStr( Value : Integer ) : String;
function u_StrToInt( const Value : String ) : Integer;
function u_FloatToStr( Value : Single; Digits : Integer = 2 ) : String;
function u_StrToFloat( const Value : String ) : Single;
function u_BoolToStr( Value : Boolean ) : String;
function u_StrToBool( const Value : String ) : Boolean;

function u_CopyAnsiStr( const Str : AnsiString ) : AnsiString;
function u_CopyStr( const Str : String ) : String;
function u_GetPAnsiChar( const Str : AnsiString ) : PAnsiChar;
function u_GetPChar( const Str : String ) : PChar;
// Только для английских символов попадающих в диапазон 0..127
function u_StrUp( const Str : String ) : String;
function u_StrDown( const Str : String ) : String;
// Удаляет один символ из utf8-строки
procedure u_Backspace( var Str : String );
// Возвращает количество символов в utf8-строке
function  u_Length( const Str : String ) : Integer;
// Возвращает количество слов, разделеных разделителем d
function  u_Words( const Str : String; D : Char = ' ' ) : Integer;
function  u_GetWord( const Str : String; N : Integer; D : Char = ' ' ) : String;
procedure u_SortList( var List : zglTStringList; iLo, iHi: Integer );
//
function u_Hash( const Str : AnsiString ) : LongWord;

procedure u_Error( const ErrStr : String );
procedure u_Warning( const ErrStr : String );

function u_GetPOT( Value : Integer ) : Integer;

procedure u_Sleep( Msec : LongWord );

{$IFDEF UNIX}
function dlopen ( Name : PChar; Flags : longint) : Pointer; cdecl; external 'dl';
function dlclose( Lib : Pointer) : Longint; cdecl; external 'dl';
function dlsym  ( Lib : Pointer; Name : Pchar) : Pointer; cdecl; external 'dl';

function select( n : longint; readfds, writefds, exceptfds : Pointer; var timeout : timeVal ):longint;cdecl;external 'libc';
{$ENDIF}
{$IFDEF ANDROID}
type
  ppthread_t      = ^pthread_t;
  ppthread_attr_t = ^pthread_attr_t;
  psem_t          = ^sem_t;

function pthread_create( __thread : ppthread_t; __attr : ppthread_attr_t; __start_routine : Pointer; __arg : Pointer ) : LongInt; cdecl; external;
function sem_init( __sem : psem_t; __pshared : LongInt; __value : DWORD ) : LongInt; cdecl; external;
function sem_destroy ( __sem : psem_t ) : LongInt; cdecl; external;
function sem_wait( __sem : psem_t ) : LongInt; cdecl; external;
function sem_post( __sem : psem_t ) : LongInt; cdecl; external;
{$ENDIF}
{$IFDEF WINDESKTOP}
function dlopen ( lpLibFileName : PAnsiChar) : HMODULE; stdcall; external 'kernel32.dll' name 'LoadLibraryA';
function dlclose( hLibModule : HMODULE ) : Boolean; stdcall; external 'kernel32.dll' name 'FreeLibrary';
function dlsym  ( hModule : HMODULE; lpProcName : PAnsiChar) : Pointer; stdcall; external 'kernel32.dll' name 'GetProcAddress';
{$ENDIF}
{$IFDEF WINCE}
function dlopen ( lpLibFileName : PWideChar) : HMODULE; stdcall; external 'coredll.dll' name 'LoadLibraryW';
function dlclose( hLibModule : HMODULE ) : Boolean; stdcall; external 'coredll.dll' name 'FreeLibrary';
function dlsym  ( hModule : HMODULE; lpProcName : PWideChar) : Pointer; stdcall; external 'coredll.dll' name 'GetProcAddressW';

function u_GetPWideChar( const Str : String ) : PWideChar;
{$ENDIF}
{$IFDEF iOS}
function u_GetNSString( const Str : String ) : NSString;
{$ENDIF}

implementation
uses
  {$IFDEF WINCE}
  zgl_application,
  zgl_main,
  {$ENDIF}
  zgl_font,
  zgl_log;

function u_IntToStr( Value : Integer ) : String;
begin
  Str( Value, Result );
end;

function u_StrToInt( const Value : String ) : Integer;
  var
    e : Integer;
begin
  Val( Value, Result, e );
  if e <> 0 Then
    Result := 0;
end;

function u_FloatToStr( Value : Single; Digits : Integer = 2 ) : String;
begin
  Str( Value:0:Digits, Result );
end;

function u_StrToFloat( const Value : String ) : Single;
  var
    e : Integer;
begin
  Val( Value, Result, e );
  if e <> 0 Then
    Result := 0;
end;

function u_BoolToStr( Value : Boolean ) : String;
begin
  if Value Then
    Result := 'TRUE'
  else
    Result := 'FALSE';
end;

function u_StrToBool( const Value : String ) : Boolean;
begin
  if Value = '1' Then
    Result := TRUE
  else
    if u_StrUp( Value ) = 'TRUE' Then
      Result := TRUE
    else
      Result := FALSE;
end;

function u_CopyAnsiStr( const Str : AnsiString ) : AnsiString;
  var
    len : Integer;
begin
  len := length( Str );
  SetLength( Result, len );
  if len > 0 Then
    System.Move( Str[ 1 ], Result[ 1 ], len );
end;

function u_CopyStr( const Str : String ) : String;
  var
    len : Integer;
begin
  len := length( Str );
  SetLength( Result, len );
  if len > 0 Then
    System.Move( Str[ 1 ], Result[ 1 ], len * SizeOf( Char ) );
end;

function u_GetPAnsiChar( const Str : AnsiString ) : PAnsiChar;
  var
    len : Integer;
begin
  len := length( Str );
  GetMem( Result, len + 1 );
  Result[ len ] := #0;
  if len > 0 Then
    System.Move( Str[ 1 ], Result^, len );
end;

function u_GetPChar( const Str : String ) : PChar;
  var
    len : Integer;
begin
  len := length( Str );
  GetMem( Result, ( len + 1 ) * SizeOf( Char ) );
  Result[ len ] := #0;
  if len > 0 Then
    System.Move( Str[ 1 ], Result^, len * SizeOf( Char ) );
end;

{$IFDEF WINCE}
function u_GetPWideChar( const Str : String ) : PWideChar;
  var
    len    : Integer;
    newStr : String;
begin
  if appFlags and APP_USE_UTF8 = 0 Then
    newStr := AnsiToUtf8( Str )
  else
    newStr := Str;

  len := MultiByteToWideChar( CP_UTF8, 0, @newStr[ 1 ], length( newStr ), nil, 0 );
  GetMem( Result, len * 2 + 2 );
  Result[ len ] := #0;
  MultiByteToWideChar( CP_UTF8, 0, @newStr[ 1 ], length( newStr ), Result, len );
end;
{$ENDIF}

{$IFDEF iOS}
function u_GetNSString( const Str : String ) : NSString;
begin
  Result := NSString( CFStr( PChar( Str ) ) );
end;
{$ENDIF}

function u_StrUp( const Str : String ) : String;
  var
    i, l : Integer;
begin
  l := length( Str );
  SetLength( Result, l );
  for i := 1 to l do
    if ( Byte( Str[ i ] ) >= 97 ) and ( Byte( Str[ i ] ) <= 122 ) Then
      Result[ i ] := Char( Byte( Str[ i ] ) - 32 )
    else
      Result[ i ] := Str[ i ];
end;

function u_StrDown( const Str : String ) : String;
  var
    i, l : Integer;
begin
  l := length( Str );
  SetLength( Result, l );
  for i := 1 to l do
    if ( Byte( Str[ i ] ) >= 65 ) and ( Byte( Str[ i ] ) <= 90 ) Then
      Result[ i ] := Char( Byte( Str[ i ] ) + 32 )
    else
      Result[ i ] := Str[ i ];
end;

procedure u_Backspace( var Str : String );
  var
    i, last : Integer;
begin
  if str = '' Then exit;
  i := 1;
  last := 0;
  while i <= length( Str ) do
    begin
      last := i;
      font_GetCID( Str, last, @i );
    end;

  SetLength( Str, last - 1 )
end;

function u_Length( const Str : String ) : Integer;
  var
    i : Integer;
begin
  Result := 0;
  i := 1;
  while i <= length( Str ) do
    begin
      INC( Result );
      font_GetCID( Str, i, @i );
    end;
end;

function u_Words( const Str : String; D : Char = ' ' ) : Integer;
  var
    i, m : Integer;
begin
  Result := 0;
  m := 0;
  for i := 1 to length( Str ) do
    begin
      if ( Str[ i ] <> D ) and ( m = 0 ) Then
        begin
          INC( Result );
          m := 1;
        end;
      if ( Str[ i ] = D ) and ( m = 1 ) Then m := 0;
    end;
end;

function u_GetWord( const Str : String; N : Integer; D : Char = ' ' ) : String;
  label b;
  var
    i, p : Integer;
begin
  i := 0;
  Result := D + Str;

b:
  INC( i );
  p := Pos( D, Result );
  while Result[ p ] = d do Delete( Result, p, 1 );

  p := Pos( D, Result );
  if N > i Then
    begin
      Delete( Result, 1, p - 1 );
      goto b;
    end;

  Delete( Result, p, length( Result ) - p + 1 );
end;

procedure u_SortList( var List : zglTStringList; iLo, iHi: Integer );
  var
    lo, hi : Integer;
    mid, t : String;
begin
  lo  := iLo;
  hi  := iHi;
  mid := List.Items[ ( lo + hi ) shr 1 ];

  repeat
    while List.Items[ lo ] < mid do INC( lo );
    while List.Items[ hi ] > mid do DEC( hi );
    if lo <= hi then
      begin
        t                := List.Items[ lo ];
        List.Items[ lo ] := List.Items[ hi ];
        List.Items[ hi ] := t;
        INC( lo );
        DEC( hi );
      end;
  until lo > hi;

  if hi > iLo Then u_SortList( List, iLo, hi );
  if lo < iHi Then u_SortList( List, lo, iHi );
end;

function u_Hash( const Str : AnsiString ) : LongWord;
  var
    data      : PAnsiChar;
    hash, tmp : LongWord;
    rem, len  : Integer;
begin
  Result := 0;
  if Str = '' Then exit;
  len  := length( Str );
  hash := len;
  data := @Str[ 1 ];

  rem := len and 3;
  len := len shr 2;

  while len > 0 do
    begin
      hash := hash + PWord( data )^;
      INC( data, 2 );
      tmp  := ( PWord( data )^ shl 11 ) xor hash;
      hash := ( hash shl 16 ) xor tmp;
      INC( data, 2 );
      hash := hash + ( hash shr 11 );
      dec( len );
    end;

  case rem of
    3:
      begin
        hash := hash + PWord( data )^;
        hash := hash xor ( hash shl 16 );
        hash := hash xor ( Byte( data[ 2 ] ) shl 18 );
        hash := hash + ( hash shr 11 );
      end;
    2:
      begin
        hash := hash + PWord( data )^;
        hash := hash xor ( hash shl 11 );
        hash := hash + ( hash shr 17 );
      end;
    1:
      begin
        hash := hash + PByte( data )^;
        hash := hash xor ( hash shl 10 );
        hash := hash + ( hash shr 1 );
      end;
  end;

  hash := hash xor ( hash shl 3 );
  hash := hash +   ( hash shr 5 );
  hash := hash xor ( hash shl 4 );
  hash := hash +   ( hash shr 17 );
  hash := hash xor ( hash shl 25 );
  hash := hash +   ( hash shr 6 );

  Result := hash;
end;

procedure u_Error( const ErrStr : String );
  {$IFDEF MACOSX}
  var
    outItemHit: SInt16;
  {$ENDIF}
  {$IFDEF WINCE}
  var
    wideStr : PWideChar;
  {$ENDIF}
begin
{$IFDEF LINUX}
  WriteLn( 'ERROR: ' + ErrStr );
{$ENDIF}
{$IFDEF WINDESKTOP}
  MessageBox( 0, PChar( ErrStr ), 'ERROR!', MB_OK or MB_ICONERROR );
{$ENDIF}
{$IFDEF WINCE}
  wideStr := u_GetPWideChar( ErrStr );
  MessageBox( 0, wideStr, 'ERROR!', MB_OK or MB_ICONERROR );
  FreeMem( wideStr );
{$ENDIF}
{$IFDEF MACOSX}
  StandardAlert( kAlertNoteAlert, 'ERROR!', ErrStr, nil, outItemHit );
{$ENDIF}
{$IFDEF iOS}
  WriteLn( 'ERROR: ' + ErrStr );
{$ENDIF}

  log_Add( 'ERROR: ' + ErrStr );
end;

procedure u_Warning( const ErrStr : String );
  {$IFDEF MACOSX}
  var
    outItemHit: SInt16;
  {$ENDIF}
  {$IFDEF WINCE}
  var
    wideStr : PWideChar;
  {$ENDIF}
begin
{$IFDEF LINUX}
  WriteLn( 'WARNING: ' + ErrStr );
{$ENDIF}
{$IFDEF WINDESKTOP}
  MessageBox( 0, PChar( ErrStr ), 'WARNING!', MB_OK or MB_ICONWARNING );
{$ENDIF}
{$IFDEF WINCE}
  wideStr := u_GetPWideChar( ErrStr );
  MessageBox( 0, wideStr, 'WARNING!', MB_OK or MB_ICONWARNING );
  FreeMem( wideStr );
{$ENDIF}
{$IFDEF MACOSX}
  StandardAlert( kAlertNoteAlert, 'WARNING!', ErrStr, nil, outItemHit );
{$ENDIF}
{$IFDEF iOS}
  WriteLn( 'WARNING: ' + ErrStr );
{$ENDIF}

  log_Add( 'WARNING: ' + ErrStr );
end;

function u_GetPOT( Value : Integer ) : Integer;
begin
  Result := Value - 1;
  Result := Result or ( Result shr 1 );
  Result := Result or ( Result shr 2 );
  Result := Result or ( Result shr 4 );
  Result := Result or ( Result shr 8 );
  Result := Result or ( Result shr 16 );
  Result := Result + 1;
end;

procedure u_Sleep( Msec : LongWord );
  {$IFDEF UNIX}
  var
    tv : TimeVal;
  {$ENDIF}
begin
{$IFDEF UNIX}
  tv.tv_sec  := Msec div 1000;
  tv.tv_usec := ( Msec mod 1000 ) * 1000;
  select( 0, nil, nil, nil, tv );
{$ENDIF}
{$IFDEF WINDOWS}
  Sleep( Msec );
{$ENDIF}
end;

end.
