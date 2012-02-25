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
unit zgl_window;

{$I zgl_config.cfg}

interface
uses
  Windows,
  zgl_direct3d,
  zgl_direct3d_all;

function  wnd_Create( Width, Height : Integer ) : Boolean;
procedure wnd_Destroy;
procedure wnd_Update;

procedure wnd_SetCaption( const NewCaption : String );
procedure wnd_SetSize( Width, Height : Integer );
procedure wnd_SetPos( X, Y : Integer );
procedure wnd_ShowCursor( Show : Boolean );
procedure wnd_Select;

var
  wndX          : Integer;
  wndY          : Integer;
  wndWidth      : Integer = 800;
  wndHeight     : Integer = 600;
  wndFullScreen : Boolean;
  wndCaption    : String;

  wndHandle    : HWND;
  //wndDC        : HDC;
  wndINST      : HINST;
  wndClass     : TWndClassExW;
  wndClassName : PWideChar = 'ZenGL';
  wndStyle     : LongWord;
  wndCpnSize   : Integer;
  wndBrdSizeX  : Integer;
  wndBrdSizeY  : Integer;
  wndCaptionW  : PWideChar;

implementation
uses
  zgl_main,
  zgl_application,
  zgl_screen,
  zgl_utils;

{$IFNDEF FPC}
// Various versions of Delphi... sucks again
function LoadCursorW(hInstance: HINST; lpCursorName: PWideChar): HCURSOR; stdcall; external user32 name 'LoadCursorW';
{$ENDIF}

function wnd_Create( Width, Height : Integer ) : Boolean;
begin
  Result    := FALSE;
  wndX      := 0;
  wndY      := 0;
  wndWidth  := Width;
  wndHeight := Height;

  if ( not wndFullScreen ) and ( appFlags and WND_USE_AUTOCENTER > 0 ) Then
    begin
      wndX := ( zgl_Get( DESKTOP_WIDTH ) - wndWidth ) div 2;
      wndY := ( zgl_Get( DESKTOP_HEIGHT ) - wndHeight ) div 2;
    end;

  wndCpnSize  := GetSystemMetrics( SM_CYCAPTION  );
  wndBrdSizeX := GetSystemMetrics( SM_CXDLGFRAME );
  wndBrdSizeY := GetSystemMetrics( SM_CYDLGFRAME );

  with wndClass do
    begin
      cbSize        := SizeOf( TWndClassExW );
      style         := CS_DBLCLKS or CS_OWNDC;
      lpfnWndProc   := @app_ProcessMessages;
      cbClsExtra    := 0;
      cbWndExtra    := 0;
      hInstance     := wndINST;
      hIcon         := LoadIconW  ( wndINST, 'MAINICON' );
      hIconSm       := LoadIconW  ( wndINST, 'MAINICON' );
      hCursor       := LoadCursorW( wndINST, PWideChar( IDC_ARROW ) );
      lpszMenuName  := nil;
      hbrBackGround := GetStockObject( BLACK_BRUSH );
      lpszClassName := wndClassName;
    end;

  if RegisterClassExW( wndClass ) = 0 Then
    begin
      u_Error( 'Cannot register window class' );
      exit;
    end;

  if wndFullScreen Then
    wndStyle := WS_POPUP or WS_VISIBLE or WS_SYSMENU
  else
    wndStyle := WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU or WS_VISIBLE;
  wndHandle := CreateWindowExW( WS_EX_APPWINDOW or WS_EX_TOPMOST * Byte( wndFullScreen ), wndClassName, wndCaptionW, wndStyle, wndX, wndY,
                                wndWidth  + ( wndBrdSizeX * 2 ) * Byte( not wndFullScreen ),
                                wndHeight + ( wndBrdSizeY * 2 + wndCpnSize ) * Byte( not wndFullScreen ), 0, 0, wndINST, nil );

  if wndHandle = 0 Then
    begin
      u_Error( 'Cannot create window' );
      exit;
    end;

  //wndDC := GetDC( wndHandle );
  //if wndDC = 0 Then
  //  begin
  //    u_Error( 'Cannot get device context' );
  //    exit;
  //  end;
  wnd_Select();

  Result := TRUE;
end;

procedure wnd_Destroy;
begin
  //if ( wndDC > 0 ) and ( ReleaseDC( wndHandle, wndDC ) = 0 ) Then
  //  begin
  //    u_Error( 'Cannot release device context' );
  //    wndDC := 0;
  //  end;

  if ( wndHandle <> 0 ) and ( not DestroyWindow( wndHandle ) ) Then
    begin
      u_Error( 'Cannot destroy window' );
      wndHandle := 0;
    end;

  if not UnRegisterClassW( wndClassName, wndINST ) Then
    begin
      u_Error( 'Cannot unregister window class' );
      wndINST := 0;
    end;
  wndHandle := 0;
end;

procedure wnd_Update;
  var
    FullScreen : Boolean;
begin
  if appFocus Then
    FullScreen := wndFullScreen
  else
    FullScreen := FALSE;

  if FullScreen Then
    wndStyle := WS_POPUP or WS_VISIBLE or WS_SYSMENU
  else
    wndStyle := WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU or WS_VISIBLE;

  SetWindowLongW( wndHandle, GWL_STYLE, wndStyle );
  SetWindowLongW( wndHandle, GWL_EXSTYLE, WS_EX_APPWINDOW or WS_EX_TOPMOST * Byte( FullScreen ) );

  appWork := TRUE;
  wnd_SetCaption( wndCaption );
  wnd_SetSize( wndWidth, wndHeight );

  if ( not wndFullScreen ) and ( appFlags and WND_USE_AUTOCENTER > 0 ) Then
    wnd_SetPos( ( zgl_Get( DESKTOP_WIDTH ) - wndWidth ) div 2, ( zgl_Get( DESKTOP_HEIGHT ) - wndHeight ) div 2 );
end;

procedure wnd_SetCaption( const NewCaption : String );
  var
    len : Integer;
begin
  wndCaption := u_CopyStr( NewCaption );

  {$IFNDEF FPC}
  if SizeOf( Char ) = 2 Then
    len := 2
  else
  {$ENDIF}
  len := 1;
  if wndHandle <> 0 Then
    begin
      if len = 1 Then
        begin
          if appFlags and APP_USE_UTF8 = 0 Then
            wndCaption := AnsiToUtf8( wndCaption );
          len := MultiByteToWideChar( CP_UTF8, 0, @wndCaption[ 1 ], length( wndCaption ), nil, 0 );
          if Assigned( wndCaptionW ) Then
            FreeMem( wndCaptionW );
          GetMem( wndCaptionW, len * 2 + 2 );
          wndCaptionW[ len ] := #0;
          MultiByteToWideChar( CP_UTF8, 0, @wndCaption[ 1 ], length( wndCaption ), wndCaptionW, len );

          SetWindowTextW( wndHandle, wndCaptionW );
        end else
          begin
            wndCaptionW := PWideChar( u_GetPChar( wndCaption ) );
            SetWindowTextW( wndHandle, wndCaptionW );
          end;
    end;
end;

procedure wnd_SetSize( Width, Height : Integer );
begin
  wndWidth  := Width;
  wndHeight := Height;

  if not appInitedToHandle Then
    wnd_SetPos( wndX, wndY );

  d3d_Restore();

  oglWidth  := Width;
  oglHeight := Height;
  if appFlags and CORRECT_RESOLUTION > 0 Then
    scr_CorrectResolution( scrResW, scrResH )
  else
    SetCurrentMode();
end;

procedure wnd_SetPos( X, Y : Integer );
begin
  wndX := X;
  wndY := Y;

  if wndHandle <> 0 Then
    if ( not wndFullScreen ) or ( not appFocus ) Then
      SetWindowPos( wndHandle, HWND_NOTOPMOST, wndX, wndY, wndWidth + ( wndBrdSizeX * 2 ), wndHeight + ( wndBrdSizeY * 2 + wndCpnSize ), SWP_NOACTIVATE )
    else
      SetWindowPos( wndHandle, HWND_TOPMOST, 0, 0, wndWidth, wndHeight, SWP_NOACTIVATE );
end;

procedure wnd_ShowCursor( Show : Boolean );
begin
  appShowCursor := Show;
end;

procedure wnd_Select;
begin
  BringWindowToTop( wndHandle );
end;

initialization
  wndCaption := cs_ZenGL;

finalization
  FreeMem( wndCaptionW );

end.
