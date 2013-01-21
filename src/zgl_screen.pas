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

function  scr_SetOptions( Width, Height, Refresh : Word; FullScreen, VSync : Boolean ) : Boolean;
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

type
  HMONITOR = THANDLE;
  MONITORINFOEX = record
    cbSize    : LongWord;
    rcMonitor : TRect;
    rcWork    : TRect;
    dwFlags   : LongWord;
    szDevice  : array[ 0..CCHDEVICENAME - 1 ] of WideChar;
  end;

var
  scrWidth       : Integer = 800;
  scrHeight      : Integer = 600;
  scrRefresh     : Integer;
  scrVSync       : Boolean;
  scrResList     : zglTResolutionList;
  scrInitialized : Boolean;

  // Viewport
  scrViewportX : Integer;
  scrViewportY : Integer;
  scrViewportW : Integer;
  scrViewportH : Integer;

  // Resolution Correct
  scrResW  : Integer;
  scrResH  : Integer;
  scrResCX : Single  = 1;
  scrResCY : Single  = 1;
  scrAddCX : Integer = 0;
  scrAddCY : Integer = 0;
  scrSubCX : Integer = 0;
  scrSubCY : Integer = 0;

  scrSettings : DEVMODEW;
  scrDesktop  : DEVMODEW;
  scrMonitor  : HMONITOR;
  scrMonInfo  : MONITORINFOEX;

implementation
uses
  zgl_types,
  zgl_main,
  zgl_application,
  zgl_window,
  zgl_render,
  zgl_render_2d,
  zgl_camera_2d,
  zgl_log,
  zgl_utils;

const
  MONITOR_DEFAULTTOPRIMARY = $00000001;
function MonitorFromWindow( hwnd : HWND; dwFlags : LongWord ) : THandle; stdcall; external 'user32.dll';
function GetMonitorInfoW( monitor : HMONITOR; var moninfo : MONITORINFOEX ) : BOOL; stdcall; external 'user32.dll';

procedure scr_Init;
begin
  scrMonitor := MonitorFromWindow( wndHandle, MONITOR_DEFAULTTOPRIMARY );
  FillChar( scrMonInfo, SizeOf( MONITORINFOEX ), 0 );
  scrMonInfo.cbSize := SizeOf( MONITORINFOEX );
  GetMonitorInfoW( scrMonitor, scrMonInfo );

  if appInitialized and ( not wndFullScreen ) Then
    begin
      scrWidth  := scrMonInfo.rcMonitor.Right - scrMonInfo.rcMonitor.Left;
      scrHeight := scrMonInfo.rcMonitor.Bottom - scrMonInfo.rcMonitor.Top;
    end;

  FillChar( scrDesktop, SizeOf( DEVMODEW ), 0 );
  scrDesktop.dmSize := SizeOf( DEVMODEW );
  // Delphi: standard ENUM_REGISTRY_SETTINGS doesn't exist in Windows unit, no comments...
  EnumDisplaySettingsW( scrMonInfo.szDevice, LongWord(-2), scrDesktop );
  scrInitialized := TRUE;
end;

function scr_Create : Boolean;
  var
    settings : DEVMODEW;
begin
  scr_Init();
  if scrDesktop.dmBitsPerPel <> 32 Then
    begin
      settings              := scrDesktop;
      settings.dmBitsPerPel := 32;

      if ChangeDisplaySettingsExW( scrMonInfo.szDevice, settings, 0, CDS_TEST, nil ) <> DISP_CHANGE_SUCCESSFUL Then
        begin
          u_Error( 'Desktop doesn''t support 32-bit color mode.' );
          zgl_Exit();
        end else
          ChangeDisplaySettingsExW( scrMonInfo.szDevice, settings, 0, CDS_FULLSCREEN, nil );
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

  scrInitialized := FALSE;
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
  d3d_EndScene();
  d3d_BeginScene();
end;

function scr_SetOptions( Width, Height, Refresh : Word; FullScreen, VSync : Boolean ) : Boolean;
begin
  Result        := TRUE;
  wndWidth      := Width;
  wndHeight     := Height;
  scrRefresh    := Refresh;
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
        scrWidth  := zgl_Get( DESKTOP_WIDTH );
        scrHeight := zgl_Get( DESKTOP_HEIGHT );
      end;

  if not appInitialized Then
    begin
      oglWidth   := Width;
      oglHeight  := Height;
      oglTargetW := Width;
      oglTargetH := Height;
      exit;
    end;
  scr_SetVSync( scrVSync );

  if Assigned( d3dDevice ) Then
    glClear( GL_COLOR_BUFFER_BIT );

  if wndFullScreen Then
    log_Add( 'Screen options changed to: ' + u_IntToStr( scrWidth ) + ' x ' + u_IntToStr( scrHeight ) + ' fullscreen' )
  else
    log_Add( 'Screen options changed to: ' + u_IntToStr( wndWidth ) + ' x ' + u_IntToStr( wndHeight ) + ' windowed' );
  if appWork Then
    begin
      wnd_Update();
      scrRefresh := scrDesktop.dmDisplayFrequency;
    end;
end;

procedure scr_CorrectResolution( Width, Height : Word );
begin
  scrResW        := Width;
  scrResH        := Height;
  scrResCX       := wndWidth  / Width;
  scrResCY       := wndHeight / Height;
  render2dClipW  := Width;
  render2dClipH  := Height;
  render2dClipXW := render2dClipX + render2dClipW;
  render2dClipYH := render2dClipY + render2dClipH;

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
  if Assigned( d3dDevice ) Then
    SetCurrentMode();
end;

procedure scr_SetViewPort;
begin
  if oglTarget = TARGET_SCREEN Then
    begin
      if ( appFlags and CORRECT_RESOLUTION > 0 ) and ( oglMode = 2 ) Then
        begin
          scrViewportX := scrAddCX;
          scrViewportY := scrAddCY;
          scrViewportW := wndWidth- scrAddCX * 2;
          scrViewportH := wndHeight - scrAddCY * 2;
        end else
          begin
            scrViewportX := 0;
            scrViewportY := 0;
            scrViewportW := wndWidth;
            scrViewportH := wndHeight;
          end;
    end else
      begin
        scrViewportX := 0;
        scrViewportY := 0;
        scrViewportW := oglTargetW;
        scrViewportH := oglTargetH;
      end;

  if appFlags and CORRECT_RESOLUTION > 0 Then
    begin
      render2dClipW  := scrResW;
      render2dClipH  := scrResH;
      render2dClipXW := render2dClipX + render2dClipW;
      render2dClipYH := render2dClipY + render2dClipH;
    end else
      begin
        render2dClipW  := scrViewportW;
        render2dClipH  := scrViewportH;
        render2dClipXW := render2dClipX + render2dClipW;
        render2dClipYH := render2dClipY + render2dClipH;
      end;

  glViewPort( scrViewportX, scrViewportY, scrViewportW, scrViewportH );
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
  glReadPixels( X, oglHeight - Height - Y, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData );
end;

end.
