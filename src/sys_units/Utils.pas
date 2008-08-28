{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru.2x4.ru
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit Utils;

{$I define.inc}

interface

uses
{$IFDEF LINUX}
  X, XLib, XUtil
{$ENDIF}
{$IFDEF WIN32}
  Windows, Messages
{$ENDIF}
  ;
  
function u_IntToStr( Value : Integer ) : String; extdecl;
function u_StrToInt( Value : String ) : Integer; extdecl;
function u_BoolToStr( Value : Boolean ) : String; extdecl;
function u_StrToBool( Value : String ) : Boolean; extdecl;

procedure u_Error( errStr : String ); extdecl;
procedure u_Warning( errStr : String ); extdecl;

function StrUp( str : String ) : String;
function StrDown( str : String ) : String;

// Весьма кривая функция
function Getchar( A, B : String ) : Byte;

function GetPOT( v : Integer ) : Integer;

implementation
uses
  zgl_Log;

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
    if StrUp( Value ) = 'TRUE' Then
      Result := TRUE
    else
      Result := FALSE;
end;

procedure u_Error;
begin
{$IFDEF LINUX}
  WriteLn( 'ERROR: ' + errStr );
{$ENDIF}

{$IFDEF WIN32}
  MessageBox( 0, PChar( errStr ), 'ERROR!', MB_OK or MB_ICONERROR );
{$ENDIF}

  log_Add( 'ERROR: ' + errStr );
end;

procedure u_Warning;
begin
{$IFDEF LINUX}
  WriteLn( 'WARNING: ' + errStr );
{$ENDIF}

{$IFDEF WIN32}
  MessageBox( 0, PChar( errStr ), 'WARNING!', MB_OK or MB_ICONWARNING );
{$ENDIF}

  log_Add( 'WARNING: ' + errStr );
end;

function StrUp;
  var
    i, l : Integer;
begin
  l := length( Str );
  for i := 1 to l do
    if ( ( Byte( Str[ i ] ) >= 97 ) and ( Byte( Str[ i ] ) <= 122 ) ) or
       ( ( Byte( Str[ i ] ) >= 224 ) and ( Byte( Str[ i ] ) <= 255 ) ) Then
      Str[ i ] := Chr( Byte( Str[ i ] ) - 32 );
  Result := Str;
end;

function StrDown;
  var
    i, l : Integer;
begin
  l := length( Str );
  for i := 1 to l do
    if ( ( Byte( Str[ i ] ) >= 65 ) and ( Byte( Str[ i ] ) <= 90 ) ) or
       ( ( Byte( Str[ i ] ) >= 192 ) and ( Byte( Str[ i ] ) <= 223 ) ) Then
      Str[ i ] := Chr( Byte( Str[ i ] ) + 32 );
  Result := Str;
end;

function Getchar;
begin
  if A[ 1 ] = #208 Then
    begin
      case B[ 1 ] of
        #129       : Result := 168;
        #144..#191 : Result := Byte( Ord( B[ 1 ] ) + 48 );
      end;
    end else
      if A[ 1 ] = #209 Then
        begin
          case B[ 1 ] of
            #128..#143 : Result := Byte( Ord( B[ 1 ] ) + 112 );
            #145       : Result := 184;
          end;
        end else
          Result := Byte( A[ 1 ] );
end;

function GetPOT;
begin
  Result := v - 1;
  Result := Result or ( Result shr 1 );
  Result := Result or ( Result shr 2 );
  Result := Result or ( Result shr 4 );
  Result := Result or ( Result shr 8 );
  Result := Result or ( Result shr 16 );
  Result := Result + 1;
end;

end.

