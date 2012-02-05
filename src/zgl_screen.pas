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
unit zgl_screen;

{$I zgl_config.cfg}

interface
uses
  Windows,
  zgl_direct3d,
  zgl_direct3d_all;

const
  REFRESH_MAXIMUM = 0;
  REFRESH_DEFAULT = 1;

procedure scr_Init;
function  scr_Create : Boolean;
procedure scr_GetResList;
procedure scr_Destroy;
procedure scr_Reset;
procedure scr_Clear;
procedure scr_Flush;

procedure scr_SetOptions( Width, Height, Refresh : Word; FullScreen, VSync : Boolean );
procedure scr_CorrectResolution( Width, Height : Word );
procedure scr_SetViewPort;
procedure scr_SetVSync( VSync : Boolean );
procedure scr_SetFSAA( FSAA : Byte );
procedure scr_ReadPixels( var pData : Pointer; X, Y, Width, Height : Word );

type
  zglPResolutionList = ^zglTResolutionList;
  zglTResolutionList = record
    Count  : Integer;
    Width  : array of Integer;
    Height : array of Integer;
end;

var
  scrWidth   : Integer = 800;
  scrHeight  : Integer = 600;
  scrRefresh : Integer;
  scrVSync   : Boolean;
  scrResList : zglTResolutionList;
  scrInitialized : Boolean;
  scrChanging : Boolean;

  // Resolution Correct
  scrResW  : Integer;
  scrResH  : Integer;
  scrResCX : Single  = 1;
  scrResCY : Single  = 1;
  scrAddCX : Integer = 0;
  scrAddCY : Integer = 0;
  scrSubCX : Integer = 0;
  scrSubCY : Integer = 0;

  scrSettings : DEVMODE;
  scrDesktop  : DEVMODE;

implementation
uses
  zgl_types,
  zgl_main,
  zgl_application,
  zgl_window,
  zgl_render_2d,
  zgl_camera_2d,
  zgl_log,
  zgl_utils;

function GetDisplayColors : Integer;
  var
    tHDC: hdc;
begin
  tHDC := GetDC( 0 );
  Result := GetDeviceCaps( tHDC, BITSPIXEL ) * GetDeviceCaps( tHDC, PLANES );
  ReleaseDC( 0, tHDC );
end;

function GetDisplayRefresh : Integer;
  var
    tHDC: hdc;
begin
  tHDC := GetDC( 0 );
  Result := GetDeviceCaps( tHDC, VREFRESH );
  ReleaseDC( 0, tHDC );
end;

procedure scr_Init;
begin
  scrInitialized := TRUE;
  with scrDesktop do
    begin
      dmSize             := SizeOf( DEVMODE );
      dmPelsWidth        := GetSystemMetrics( SM_CXSCREEN );
      dmPelsHeight       := GetSystemMetrics( SM_CYSCREEN );
      dmBitsPerPel       := GetDisplayColors();
      dmDisplayFrequency := GetDisplayRefresh();
      dmFields           := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or DM_DISPLAYFREQUENCY;
    end;
end;

function scr_Create : Boolean;
  var
    settings : DEVMODE;
begin
  Result := FALSE;
  scr_Init();
  if scrDesktop.dmBitsPerPel <> 32 Then
    begin
      settings              := scrDesktop;
      settings.dmBitsPerPel := 32;

      if ChangeDisplaySettings( settings, CDS_TEST or CDS_FULLSCREEN ) <> DISP_CHANGE_SUCCESSFUL Then
        begin
          u_Error( 'Desktop doesn''t support 32-bit color mode.' );
          zgl_Exit;
        end else
          ChangeDisplaySettings( settings, CDS_FULLSCREEN );
    end;
  log_Add( 'Current mode: ' + u_IntToStr( zgl_Get( DESKTOP_WIDTH ) ) + ' x ' + u_IntToStr( zgl_Get( DESKTOP_HEIGHT ) ) );
  scr_GetResList();
  Result := TRUE;
end;

procedure scr_GetResList;
  var
    i : Integer;
    tmpSettings : DEVMODE;
  function Already( Width, Height : Integer ) : Boolean;
    var
      j : Integer;
  begin
    Result := FALSE;
    for j := 0 to scrResList.Count - 1 do
      if ( scrResList.Width[ j ] = Width ) and ( scrResList.Height[ j ] = Height ) Then Result := TRUE;
  end;
begin
  i := 0;
  while EnumDisplaySettings( nil, i, tmpSettings ) <> FALSE do
    begin
      if not Already( tmpSettings.dmPelsWidth, tmpSettings.dmPelsHeight ) Then
        begin
          INC( scrResList.Count );
          SetLength( scrResList.Width, scrResList.Count );
          SetLength( scrResList.Height, scrResList.Count );
          scrResList.Width[ scrResList.Count - 1 ]  := tmpSettings.dmPelsWidth;
          scrResList.Height[ scrResList.Count - 1 ] := tmpSettings.dmPelsHeight;
        end;
      INC( i );
    end;
end;

procedure scr_Destroy;
begin
  scr_Reset();
end;

procedure scr_Reset;
begin
end;

procedure scr_Clear;
begin
  batch2d_Flush();
  glClear( GL_COLOR_BUFFER_BIT * Byte( appFlags and COLOR_BUFFER_CLEAR > 0 ) or GL_DEPTH_BUFFER_BIT * Byte( appFlags and DEPTH_BUFFER_CLEAR > 0 ) or
           GL_STENCIL_BUFFER_BIT * Byte( appFlags and STENCIL_BUFFER_CLEAR > 0 ) );
end;

procedure scr_Flush;
begin
  batch2d_Flush();
end;

procedure scr_SetOptions( Width, Height, Refresh : Word; FullScreen, VSync : Boolean );
begin
  scrChanging   := TRUE;
  oglWidth      := Width;
  oglHeight     := Height;
  oglTargetW    := Width;
  oglTargetH    := Height;
  wndWidth      := Width;
  wndHeight     := Height;
  wndFullScreen := FullScreen;
  scrVsync      := VSync;

  if Height >= zgl_Get( DESKTOP_HEIGHT ) Then
    wndFullScreen := TRUE;
  if wndFullScreen Then
    begin
      scrWidth  := Width;
      scrHeight := Height;
    end else
      begin
        scrWidth   := zgl_Get( DESKTOP_WIDTH );
        scrHeight  := zgl_Get( DESKTOP_HEIGHT );
        scrRefresh := GetDisplayRefresh();
      end;

  if not appInitialized Then exit;
  scr_SetVSync( scrVSync );

  if Assigned( d3dDevice ) Then
    glClear( GL_COLOR_BUFFER_BIT );

  if wndFullScreen Then
    log_Add( 'Screen options changed to: ' + u_IntToStr( scrWidth ) + ' x ' + u_IntToStr( scrHeight ) + ' fullscreen' )
  else
    log_Add( 'Screen options changed to: ' + u_IntToStr( wndWidth ) + ' x ' + u_IntToStr( wndHeight ) + ' windowed' );
  if appWork Then
    wnd_Update();
end;

procedure scr_CorrectResolution( Width, Height : Word );
begin
  scrResW  := Width;
  scrResH  := Height;
  scrResCX := wndWidth  / Width;
  scrResCY := wndHeight / Height;

  if scrResCX < scrResCY Then
    begin
      scrAddCX := 0;
      scrAddCY := Round( ( wndHeight - Height * scrResCX ) / 2 );
      scrResCY := scrResCX;
    end else
      begin
        scrAddCX := Round( ( wndWidth - Width * scrResCY ) / 2 );
        scrAddCY := 0;
        scrResCX := scrResCY;
      end;

  if appFlags and CORRECT_HEIGHT = 0 Then
    begin
      scrResCY := wndHeight / Height;
      scrAddCY := 0;
    end;
  if appFlags and CORRECT_WIDTH = 0 Then
    begin
      scrResCX := wndWidth / Width;
      scrAddCX := 0;
    end;

  oglWidth  := Round( wndWidth / scrResCX );
  oglHeight := Round( wndHeight / scrResCY );
  scrSubCX  := oglWidth - Width;
  scrSubCY  := oglHeight - Height;
  SetCurrentMode();
end;

procedure scr_SetViewPort;
begin
  if oglTarget = TARGET_SCREEN Then
    begin
      if ( appFlags and CORRECT_RESOLUTION > 0 ) and ( oglMode = 2 ) Then
        begin
          oglClipX := 0;
          oglClipY := 0;
          oglClipW := wndWidth - scrAddCX * 2;
          oglClipH := wndHeight - scrAddCY * 2;
          glViewPort( scrAddCX, scrAddCY, oglClipW, oglClipH );
        end else
          begin
            oglClipX := 0;
            oglClipY := 0;
            oglClipW := wndWidth;
            oglClipH := wndHeight;
            glViewPort( 0, 0, oglClipW, oglClipH );
          end;
    end else
      begin
        oglClipX := 0;
        oglClipY := 0;
        oglClipW := oglWidth;
        oglClipH := oglHeight;
        glViewPort( 0, 0, oglTargetW, oglTargetH );
      end;
end;

procedure scr_SetVSync( VSync : Boolean );
begin
  scrVSync := VSync;
  if wndHandle <> 0 Then
    wnd_Update();
end;

procedure scr_SetFSAA( FSAA : Byte );
begin
  if oglFSAA = FSAA Then exit;
  oglFSAA := FSAA;

  if oglFSAA <> 0 Then
    log_Add( 'FSAA changed to: ' + u_IntToStr( oglFSAA ) + 'x' )
  else
    log_Add( 'FSAA changed to: off' );

  if wndHandle <> 0 Then
    wnd_Update();
end;

procedure scr_ReadPixels( var pData : Pointer; X, Y, Width, Height : Word );
begin
  batch2d_Flush();
  GetMem( pData, Width * Height * 4 );
  glReadPixels( X, oglClipH - Height - Y, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData );
end;

end.
