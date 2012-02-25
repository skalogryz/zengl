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
unit zgl_msvcrt;

{$I zgl_config.cfg}

interface
uses
  zgl_types;

{$IFDEF FPC}
  {$LINKLIB libmsvcrt.a}

  {$IFDEF WIN64}
  {$IFDEF NAME_MANGLING}
  function msvcrt_pow( x, y : Double ) : Double; cdecl; public name 'pow';
  {$ENDIF}
  {$ENDIF}

  function pow( x, y : Double ) : Double; cdecl; external 'msvcrt.dll';
{$ELSE}
  function memcpy( dest : Pointer; src : Pointer; count : csize_t ) : Pointer; cdecl; external 'msvcrt.dll';
  function memset( dest : Pointer; c : Integer; count : csize_t ) : Pointer; cdecl; external 'msvcrt.dll';
  function malloc( size : csize_t ) : Pointer; cdecl; external 'msvcrt.dll';
  procedure free( memblock : Pointer ); cdecl; external 'msvcrt.dll';
{$ENDIF}

implementation

{$IFDEF FPC}
{$IFDEF WIN64}
{$IFDEF NAME_MANGLING}
function msvcrt_pow( x, y : Double ) : Double;
begin
  Result := pow( x, y );
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

end.