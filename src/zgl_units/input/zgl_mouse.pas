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
unit zgl_mouse;

{$I define.inc}

interface
uses
  {$IFDEF LINUX}
  X, XLib,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll,
  {$ENDIF}
  zgl_const,
  zgl_global_var,
  zgl_math;

const
  M_BLEFT  = 0;
  M_BMIDLE = 1;
  M_BRIGHT = 2;
  M_WUP    = 0;
  M_WDOWN  = 1;

function mouse_X : Integer;
function mouse_Y : Integer;
function mouse_DX : Integer;
function mouse_DY : Integer;
function mouse_Down( const Button : Byte ) : Boolean;
function mouse_Up( const Button : Byte ) : Boolean;
function mouse_Click( const Button : Byte ) : Boolean;
function mouse_Wheel( const Axis : Byte ) : Boolean;
procedure mouse_ClearState;
procedure mouse_Lock;

var
  mouseX        : WORD;
  mouseY        : WORD;
  mouseDown     : array[ 0..2 ] of Boolean;
  mouseUp       : array[ 0..2 ] of Boolean;
  mouseClick    : array[ 0..2 ] of Boolean;
  mouseCanClick : array[ 0..2 ] of Boolean;
  mouseWheel    : array[ 0..1 ] of Boolean;
  mouseLock     : Boolean;
  {$IFDEF WIN32}
  cursorpos : TPoint;
  {$ENDIF}

implementation

function mouse_X;
begin
  {$IFDEF LINUX_OR_DARWIN}
  Result := mouseX;
  {$ENDIF}
  {$IFDEF WIN32}
  GetCursorPos( cursorpos );
  Result := cursorpos.X - wnd_X - wnd_BrdSizeX;
  {$ENDIF}
  Result := Round( Result / scr_ResCX );
end;

function mouse_Y;
begin
  {$IFDEF LINUX}
  Result := mouseY;
  {$ENDIF}
  {$IFDEF WIN32}
  GetCursorPos( cursorpos );
  Result := cursorpos.Y - wnd_Y - wnd_BrdSizeY - wnd_CpnSize;
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := mouseY - wnd_CpnSize;
  {$ENDIF}
  Result := Round( Result / scr_ResCY );
end;

function mouse_DX;
begin
  Result := mouse_X() - wnd_Width div 2;
end;

function mouse_DY;
begin
  Result := mouse_Y() - wnd_Height div 2;
end;

function mouse_Down;
begin
  Result := mouseDown[ Button ];
end;

function mouse_Up;
begin
  Result := mouseUp[ Button ];
//  mouseUp[ Button ] := FALSE;
end;

function mouse_Click;
begin
  Result := mouseClick[ Button ];
//  mouseClick[ Button ] := FALSE;
end;

function mouse_Wheel;
begin
  Result := mouseWheel[ Axis ];
//  mouseWheel[ Axis ] := FALSE;
end;

procedure mouse_ClearState;
begin
  mouseUp[ M_BLEFT  ] := FALSE;
  mouseUp[ M_BMIDLE ] := FALSE;
  mouseUp[ M_BRIGHT ] := FALSE;
  
  mouseClick[ M_BLEFT  ] := FALSE;
  mouseClick[ M_BMIDLE ] := FALSE;
  mouseClick[ M_BRIGHT ] := FALSE;
  
  mouseCanClick[ M_BLEFT  ] := TRUE;
  mouseCanClick[ M_BMIDLE ] := TRUE;
  mouseCanClick[ M_BRIGHT ] := TRUE;
  
  mouseWheel[ M_WUP   ] := FALSE;
  mouseWheel[ M_WDOWN ] := FALSE;
end;

procedure mouse_Lock;
  {$IFDEF DARWIN}
  var
    Point : CGPoint;
  {$ENDIF}
begin
  {$IFDEF LINUX}
  XWarpPointer( scr_display, None, wnd_Handle, 0, 0, 0, 0, wnd_X + wnd_Width div 2, wnd_Y + wnd_Height div 2 );
  {$ENDIF}
  {$IFDEF WIN32}
  SetCursorPos( wnd_X + wnd_BrdSizeX + wnd_Width div 2, wnd_Y + wnd_BrdSizeY + wnd_CpnSize + wnd_Height div 2 );
  {$ENDIF}
  {$IFDEF DARWIN}
  Point.X := wnd_X + wnd_Width / 2;
  Point.Y := wnd_Y + wnd_Height / 2;
  CGWarpMouseCursorPosition( Point );
  {$ENDIF}
end;

end.
