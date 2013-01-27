{
 *  Copyright (c) 2012 Andrey Kemka
 *
 *  This software is provided 'as-is', without any express or
 *  implied warranty. In no event will the authors be held
 *  liable for any damages arising from the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute
 *  it freely, subject to the following restrictions:
 *
 *  1. The origin of this software must not be misrepresented;
 *     you must not claim that you wrote the original software.
 *     If you use this software in a product, an acknowledgment
 *     in the product documentation would be appreciated but
 *     is not required.
 *
 *  2. Altered source versions must be plainly marked as such,
 *     and must not be misrepresented as being the original software.
 *
 *  3. This notice may not be removed or altered from any
 *     source distribution.
}
unit zgl_mouse;

{$I zgl_config.cfg}

interface
uses
  Windows;

const
  M_BLEFT   = 0;
  M_BMIDDLE = 1;
  M_BRIGHT  = 2;
  M_WUP     = 0;
  M_WDOWN   = 1;

function mouse_X : Integer;
function mouse_Y : Integer;
function mouse_DX : Integer;
function mouse_DY : Integer;
function mouse_Down( Button : Byte ) : Boolean;
function mouse_Up( Button : Byte ) : Boolean;
function mouse_Click( Button : Byte ) : Boolean;
function mouse_DblClick( Button : Byte ) : Boolean;
function mouse_Wheel( Axis : Byte ) : Boolean;
procedure mouse_ClearState;
procedure mouse_Lock( X : Integer = -1; Y : Integer = -1 );

var
  mouseX        : Integer;
  mouseY        : Integer;
  mouseDX       : Integer;
  mouseDY       : Integer;
  mouseLX       : Integer;
  mouseLY       : Integer;
  mouseDown     : array[ 0..2 ] of Boolean;
  mouseUp       : array[ 0..2 ] of Boolean;
  mouseClick    : array[ 0..2 ] of Boolean;
  mouseCanClick : array[ 0..2 ] of Boolean;
  mouseDblClick : array[ 0..2 ] of Boolean;
  mouseDblCTime : array[ 0..2 ] of Double;
  mouseDblCInt  : Integer = 250;
  mouseWheel    : array[ 0..1 ] of Boolean;
  mouseLock     : Boolean;

  // callback
  mouse_PMove    : procedure( X, Y : Integer );
  mouse_PPress   : procedure( Button : Byte );
  mouse_PRelease : procedure( Button : Byte );
  mouse_PWheel   : procedure( Axis : Byte );

implementation
uses
  zgl_window,
  zgl_screen;

function mouse_X : Integer;
begin
  Result := mouseX;
end;

function mouse_Y : Integer;
begin
  Result := mouseY;
end;

function mouse_DX : Integer;
begin
  Result := mouseDX;
end;

function mouse_DY : Integer;
begin
  Result := mouseDY;
end;

function mouse_Down( Button : Byte ) : Boolean;
begin
  if GetSystemMetrics( SM_SWAPBUTTON ) = 0 Then
    begin
      case Button of
        M_BLEFT:   Result := GetAsyncKeyState( VK_LBUTTON ) and $8000 <> 0;
        M_BMIDDLE: Result := GetAsyncKeyState( VK_MBUTTON ) and $8000 <> 0;
        M_BRIGHT:  Result := GetAsyncKeyState( VK_RBUTTON ) and $8000 <> 0;
      else
        Result := FALSE;
      end;
    end else
      case Button of
        M_BLEFT:   Result := GetAsyncKeyState( VK_RBUTTON ) and $8000 <> 0;
        M_BMIDDLE: Result := GetAsyncKeyState( VK_MBUTTON ) and $8000 <> 0;
        M_BRIGHT:  Result := GetAsyncKeyState( VK_LBUTTON ) and $8000 <> 0;
      else
        Result := FALSE;
      end;
end;

function mouse_Up( Button : Byte ) : Boolean;
begin
  Result := mouseUp[ Button ];
end;

function mouse_Click( Button : Byte ) : Boolean;
begin
  Result := mouseClick[ Button ];
end;

function mouse_DblClick( Button : Byte ) : Boolean;
begin
  Result := mouseDblClick[ Button ];
end;

function mouse_Wheel( Axis : Byte ) : Boolean;
begin
  Result := mouseWheel[ Axis ];
end;

procedure mouse_ClearState;
begin
  FillChar( mouseUp[ 0 ], 3, 0 );
  FillChar( mouseClick[ 0 ], 3, 0 );
  FillChar( mouseDblClick[ 0 ], 3, 0 );
  FillChar( mouseCanClick[ 0 ], 3, 1 );
  FillChar( mouseWheel[ 0 ], 2, 0 );
end;

procedure mouse_Lock( X : Integer = -1; Y : Integer = -1 );
begin
  if ( X = -1 ) and ( Y = -1 ) Then
    begin
      if wndFullScreen Then
        begin
          X := wndWidth div 2;
          Y := wndHeight div 2;
        end else
          begin
            X := wndX + wndBrdSizeX + wndWidth div 2;
            Y := wndY + wndBrdSizeY + wndCpnSize + wndHeight div 2;
          end;
    end else
      begin
        X := wndX + X;
        Y := wndY + Y;
      end;

  SetCursorPos( X, Y );
end;

end.
