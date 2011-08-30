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
unit zgl_touch;

{$I zgl_config.cfg}

interface

function touch_X( Finger : Byte ) : Integer;
function touch_Y( Finger : Byte ) : Integer;
function touch_Down( Finger : Byte ) : Boolean;
function touch_Up( Finger : Byte ) : Boolean;
function touch_Tap( Finger : Byte ) : Boolean;
function touch_DblTap( Finger : Byte ) : Boolean;
procedure touch_ClearState;

var
  touchX      : array[ 0..255 ] of Integer;
  touchY      : array[ 0..255 ] of Integer;
  touchDown   : array[ 0..255 ] of Boolean;
  touchUp     : array[ 0..255 ] of Boolean;
  touchTap    : array[ 0..255 ] of Boolean;
  touchCanTap : array[ 0..255 ] of Boolean;
  touchDblTap : array[ 0..255 ] of Boolean;
  touchList   : array[ 0..255 ] of Pointer;
  touchCount  : Integer;

  // callback
  touch_PMove    : procedure( Finger : Byte; X, Y : Integer );
  touch_PPress   : procedure( Finger : Byte );
  touch_PRelease : procedure( Finger : Byte );

implementation
uses
  zgl_screen;

function touch_X( Finger : Byte ) : Integer;
begin
  Result := touchX[ Finger ];
end;

function touch_Y( Finger : Byte ) : Integer;
begin
  Result := touchY[ Finger ];
end;

function touch_Down( Finger : Byte ) : Boolean;
begin
  Result := touchDown[ Finger ];
end;

function touch_Up( Finger : Byte ) : Boolean;
begin
  Result := touchUp[ Finger ];
end;

function touch_Tap( Finger : Byte ) : Boolean;
begin
  Result := touchTap[ Finger ];
end;

function touch_DblTap( Finger : Byte ) : Boolean;
begin
  Result := touchDblTap[ Finger ];
end;

procedure touch_ClearState;
begin
  FillChar( touchUp[ 0 ], 255, 0 );
  FillChar( touchTap[ 0 ], 255, 0 );
  FillChar( touchDblTap[ 0 ], 255, 0 );
  FillChar( touchCanTap[ 0 ], 255, 1 );
end;

end.
