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
{$IFDEF iOS}
  {$modeswitch objectivec1}
{$ENDIF}

interface
{$IFDEF USE_X11}
  uses X, XLib, XRandr, UnixType;
{$ENDIF}
{$IFDEF WINDOWS}
  uses Windows;
{$ENDIF}
{$IFDEF MACOSX}
  uses MacOSAll;
{$ENDIF}
{$IFDEF iOS}
  uses iPhoneAll, CFBase, CFString;
{$ENDIF}

const
  REFRESH_MAXIMUM = 0;
  REFRESH_DEFAULT = 1;

procedure scr_Init;
procedure scr_Reset;
function  scr_Create : Boolean;
procedure scr_Destroy;
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

{$IFDEF WINDOWS}
type
  HMONITOR = THANDLE;
  MONITORINFOEX = record
    cbSize    : LongWord;
    rcMonitor : TRect;
    rcWork    : TRect;
    dwFlags   : LongWord;
    szDevice  : array[ 0..CCHDEVICENAME - 1 ] of WideChar;
  end;
{$ENDIF}

var
  scrWidth       : Integer = 800;
  scrHeight      : Integer = 600;
  scrRefresh     : Integer;
  scrVSync       : Boolean;
  scrResList     : zglTResolutionList;
  scrInitialized : Boolean;
  scrAutoFullScreen : Boolean = True;

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

  {$IFDEF USE_X11}
  scrDisplay   : PDisplay;
  scrDefault   : cint;
  scrSettings  : Pointer;
  scrDesktop   : LongInt;
  scrCurrent   : LongInt;
  scrModeCount : LongInt;
  scrModeList  : PXRRScreenSize;
  scrEventBase : cint;
  scrErrorBase : cint;
  scrRotation  : Word;
  {$ENDIF}
  {$IFDEF WINDOWS}
  scrSettings : DEVMODEW;
  scrDesktop  : DEVMODEW;
  scrMonitor  : HMONITOR;
  scrMonInfo  : MONITORINFOEX;
  {$ENDIF}
  {$IFDEF MACOSX}
  scrDisplay   : CGDirectDisplayID;
  scrDesktop   : CFDictionaryRef;
  scrDesktopW  : Integer;
  scrDesktopH  : Integer;
  scrSettings  : CFDictionaryRef;
  scrModeCount : CFIndex;
  scrModeList  : CFArrayRef;
  {$ENDIF}
  {$IFDEF iOS}
  scrDisplayLink  : CADisplayLink;
  scrCurrModeW    : Integer;
  scrCurrModeH    : Integer;
  scrDesktopW     : Integer;
  scrDesktopH     : Integer;
  scrOrientation  : UIInterfaceOrientation;
  scrCanLandscape : Boolean;
  scrCanPortrait  : Boolean;
  {$ENDIF}
  {$IFDEF ANDROID}
  scrDesktopW : Integer;
  scrDesktopH : Integer;
  {$ENDIF}

implementation
uses
  zgl_main,
  zgl_application,
  zgl_window,
  {$IFNDEF USE_GLES}
  zgl_opengl,
  zgl_opengl_all,
  {$ELSE}
  zgl_opengles,
  zgl_opengles_all,
  {$ENDIF}
  zgl_render,
  zgl_render_2d,
  zgl_camera_2d,
  zgl_log,
  zgl_utils;

{$IFDEF USE_X11}
function XOpenIM(para1:PDisplay; para2:PXrmHashBucketRec; para3:PAnsiChar; para4:Pchar):PXIM;cdecl;external;
function XCloseIM(im : PXIM) : TStatus;cdecl;external;
function XCreateIC(para1 : PXIM; para2 : array of const):PXIC;cdecl;external;
procedure XDestroyIC(ic : PXIC);cdecl;external;
{$ENDIF}
{$IFDEF WINDOWS}
const
  MONITOR_DEFAULTTOPRIMARY = $00000001;
function MonitorFromWindow( hwnd : HWND; dwFlags : LongWord ) : THandle; stdcall; external 'user32.dll';
function GetMonitorInfoW( monitor : HMONITOR; var moninfo : MONITORINFOEX ) : BOOL; stdcall; external 'user32.dll';
{$ENDIF}

procedure scr_GetResList;
  var
    i : Integer;
  {$IFDEF USE_X11}
    tmpSettings : PXRRScreenSize;
  {$ENDIF}
  {$IFDEF WINDOWS}
    tmpSettings : DEVMODEW;
  {$ENDIF}
  {$IFDEF MACOSX}
    tmpSettings   : UnivPtr;
    width, height : Integer;
  {$ENDIF}
  function Already( Width, Height : Integer ) : Boolean;
    var
      j : Integer;
  begin
    Result := FALSE;
    for j := 0 to scrResList.Count - 1 do
      if ( scrResList.Width[ j ] = Width ) and ( scrResList.Height[ j ] = Height ) Then Result := TRUE;
  end;
begin
{$IFDEF USE_X11}
  tmpSettings := scrModeList;
  for i := 0 to scrModeCount - 1 do
    begin
      if not Already( tmpSettings.Width, tmpSettings.Height ) Then
        begin
          INC( scrResList.Count );
          SetLength( scrResList.Width, scrResList.Count );
          SetLength( scrResList.Height, scrResList.Count );
          scrResList.Width[ scrResList.Count - 1 ]  := tmpSettings.Width;
          scrResList.Height[ scrResList.Count - 1 ] := tmpSettings.Height;
        end;
      INC( tmpSettings );
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  i := 0;
  FillChar( tmpSettings, SizeOf( DEVMODEW ), 0 );
  tmpSettings.dmSize := SizeOf( DEVMODEW );
  while EnumDisplaySettingsW( scrMonInfo.szDevice, i, tmpSettings ) <> FALSE do
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
{$ENDIF}
{$IFDEF MACOSX}
  tmpSettings := scrModeList;
  for i := 0 to scrModeCount - 1 do
    begin
      tmpSettings := CFArrayGetValueAtIndex( scrModeList, i );
      CFNumberGetValue( CFDictionaryGetValue( tmpSettings, CFSTRP('Width') ), kCFNumberIntType, @width );
      CFNumberGetValue( CFDictionaryGetValue( tmpSettings, CFSTRP('Height') ), kCFNumberIntType, @height );
      if not Already( width, height ) Then
        begin
          INC( scrResList.Count );
          SetLength( scrResList.Width, scrResList.Count );
          SetLength( scrResList.Height, scrResList.Count );
          scrResList.Width[ scrResList.Count - 1 ]  := width;
          scrResList.Height[ scrResList.Count - 1 ] := height;
        end;
    end;
{$ENDIF}
{$IFDEF iOS}
  scrResList.Count := 1;
  SetLength( scrResList.Width, 1 );
  SetLength( scrResList.Height, 1 );
  scrResList.Width[ 0 ] := scrDesktopW;
  scrResList.Height[ 0 ] := scrDesktopH;
{$ENDIF}
end;

procedure scr_Init;
  {$IFDEF iOS}
  var
    i            : Integer;
    orientations : NSArray;
    tmp          : array[ 0..255 ] of AnsiChar;
  {$ENDIF}
begin
{$IFDEF USE_X11}
  if not appInitialized Then
    begin
      log_Init();

      if Assigned( scrDisplay ) Then
        XCloseDisplay( scrDisplay );

      scrDisplay := XOpenDisplay( nil );
      if not Assigned( scrDisplay ) Then
        begin
          u_Error( 'Cannot connect to X server.' );
          exit;
        end;

      scrDefault := DefaultScreen( scrDisplay );
      wndRoot    := DefaultRootWindow( scrDisplay );

      XRRSelectInput( scrDisplay, wndRoot, RRScreenChangeNotifyMask );
      XRRQueryExtension( scrDisplay, @scrEventBase, @scrErrorBase ) ;
    end;

  scrModeList := XRRSizes( scrDisplay, XRRRootToScreen( scrDisplay, wndRoot ), @scrModeCount );
  scrSettings := XRRGetScreenInfo( scrDisplay, wndRoot );
  scrDesktop  := XRRConfigCurrentConfiguration( scrSettings, @scrRotation );
  if appInitialized Then
    scr_GetResList();
{$ENDIF}
{$IFDEF WINDOWS}
  if scrMonInfo.cbSize <> SizeOf( MONITORINFOEX ) Then
    begin
      scrMonitor := MonitorFromWindow( wndHandle, MONITOR_DEFAULTTOPRIMARY );
      FillChar( scrMonInfo, SizeOf( MONITORINFOEX ), 0 );
      scrMonInfo.cbSize := SizeOf( MONITORINFOEX );
      GetMonitorInfoW( scrMonitor, scrMonInfo );
    end;

  if appInitialized and ( not wndFullScreen ) Then
    begin
      scrWidth  := scrMonInfo.rcMonitor.Right - scrMonInfo.rcMonitor.Left;
      scrHeight := scrMonInfo.rcMonitor.Bottom - scrMonInfo.rcMonitor.Top;
    end;

  FillChar( scrDesktop, SizeOf( DEVMODEW ), 0 );
  scrDesktop.dmSize := SizeOf( DEVMODEW );
  // Delphi: standard ENUM_REGISTRY_SETTINGS doesn't exist in Windows unit, no comments...
  EnumDisplaySettingsW( scrMonInfo.szDevice, LongWord(-2), scrDesktop );
{$ENDIF}
{$IFDEF MACOSX}
  scrDisplay  := CGMainDisplayID();
  scrDesktop  := CGDisplayCurrentMode( scrDisplay );
  scrDesktopW := CGDisplayPixelsWide( scrDisplay );
  scrDesktopH := CGDisplayPixelsHigh( scrDisplay );

  scrModeList  := CGDisplayAvailableModes( scrDisplay );
  scrModeCount := CFArrayGetCount( scrModeList );
{$ENDIF}
{$IFDEF iOS}
  if not appInitialized Then exit;

  app_InitPool();

  orientations := NSBundle.mainBundle.infoDictionary.objectForKey( utf8_GetNSString( 'UISupportedInterfaceOrientations' ) );
  for i := 0 to orientations.count() - 1 do
    begin
      CFStringGetCString( CFStringRef( orientations.objectAtIndex( i ) ), @tmp[ 0 ], 255, kCFStringEncodingUTF8 );
      if ( tmp = 'UIInterfaceOrientationLandscapeLeft' ) or ( tmp = 'UIInterfaceOrientationLandscapeRight' ) Then
        scrCanLandscape := TRUE;
      if ( tmp = 'UIInterfaceOrientationPortrait' ) or ( tmp = 'UIInterfaceOrientationPortraitUpsideDown' ) Then
        scrCanPortrait := TRUE;
    end;

  if UIDevice.currentDevice.systemVersion.floatValue >= 3.2 Then
    begin
      scrCurrModeW := Round( UIScreen.mainScreen.currentMode.size.width );
      scrCurrModeH := Round( UIScreen.mainScreen.currentMode.size.height );

      if ( UIDevice.currentDevice.userInterfaceIdiom = UIUserInterfaceIdiomPhone ) and ( UIDevice.currentDevice.model.rangeOfString( utf8_GetNSString( 'iPad' ) ).location <> NSNotFound ) Then
        begin
          scrCurrModeW := Round( 480 * UIScreen.mainScreen.scale );
          scrCurrModeH := Round( 320 * UIScreen.mainScreen.scale );
        end;
    end else
      begin
        scrCurrModeW := Round( UIScreen.mainScreen.bounds.size.width );
        scrCurrModeH := Round( UIScreen.mainScreen.bounds.size.height );
      end;

  if ( scrCurrModeW > scrCurrModeH ) Then
    begin
      scrDesktopH  := scrCurrModeW;
      scrCurrModeW := scrCurrModeH;
      scrCurrModeH := scrDesktopH;
    end;

  scrOrientation := UIApplication.sharedApplication.statusBarOrientation();
  if scrCanPortrait and ( ( scrOrientation = UIInterfaceOrientationPortrait ) or ( scrOrientation = UIInterfaceOrientationPortraitUpsideDown ) ) Then
    begin
      wndPortrait := TRUE;
      scrDesktopW := scrCurrModeW;
      scrDesktopH := scrCurrModeH;
    end else
      if scrCanLandscape and ( ( scrOrientation = UIInterfaceOrientationLandscapeLeft ) or ( scrOrientation = UIInterfaceOrientationLandscapeRight ) ) Then
        begin
          wndPortrait := FALSE;
          scrDesktopW := scrCurrModeH;
          scrDesktopH := scrCurrModeW;
        end else
          begin
            wndPortrait := scrCanPortrait;
            scrDesktopW := scrCurrModeW;
            scrDesktopH := scrCurrModeH;
          end;

  oglWidth    := scrDesktopW;
  oglHeight   := scrDesktopH;
  oglTargetW  := scrDesktopW;
  oglTargetH  := scrDesktopH;
{$ENDIF}
  scrInitialized := TRUE;
end;

procedure scr_Reset;
begin
{$IFDEF USE_X11}
  XRRSetScreenConfig( scrDisplay, scrSettings, wndRoot, scrDesktop, scrRotation, CurrentTime );
{$ENDIF}
{$IFDEF WINDOWS}
  ChangeDisplaySettingsExW( scrMonInfo.szDevice, DEVMODEW( nil^ ), 0, CDS_FULLSCREEN, nil );
{$ENDIF}
{$IFDEF MACOSX}
  CGDisplaySwitchToMode( scrDisplay, scrDesktop );
  //CGDisplayRelease( scrDisplay );
{$ENDIF}
end;

procedure scr_SetWindowedMode;
  {$IFDEF WINDOWS}
  var
    settings : DEVMODEW;
  {$ENDIF}
begin
  {$IFDEF USE_X11}
  scr_Reset();
  XMapWindow( scrDisplay, wndHandle );
  {$ENDIF}
  {$IFDEF WINDOWS}
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

      scrRefresh := scrDesktop.dmDisplayFrequency;
    end else
      scr_Reset();
  {$ENDIF}
  {$IFDEF MACOSX}
  scr_Reset();
  ShowMenuBar();
  {$ENDIF}
end;

function scr_Create : Boolean;
begin
  scr_Init();
{$IFDEF USE_X11}
  if DefaultDepth( scrDisplay, scrDefault ) < 24 Then
    begin
      u_Error( 'DefaultDepth not set to 24-bit.' );
      zgl_Exit();
      Result := FALSE;
      exit;
    end;

  appXIM := XOpenIM( scrDisplay, nil, nil, nil );
  if not Assigned( appXIM ) Then
    log_Add( 'XOpenIM - Fail' )
  else
    log_Add( 'XOpenIM - ok' );

  appXIC := XCreateIC( appXIM, [ XNInputStyle, XIMPreeditNothing or XIMStatusNothing, 0 ] );
  if not Assigned( appXIC ) Then
    log_Add( 'XCreateIC - Fail' )
  else
    log_Add( 'XCreateIC - ok' );
{$ENDIF}
{$IFDEF WINDOWS}
  if ( not wndFullScreen ) and ( scrDesktop.dmBitsPerPel <> 32 ) Then
    scr_SetWindowedMode();
{$ENDIF}
{$IFDEF MACOSX}
  if CGDisplayBitsPerPixel( scrDisplay ) <> 32 Then
    begin
      u_Error( 'Desktop not set to 32-bit mode.' );
      zgl_Exit();
      Result := FALSE;
      exit;
    end;
{$ENDIF}
  log_Add( 'Current mode: ' + u_IntToStr( zgl_Get( DESKTOP_WIDTH ) ) + ' x ' + u_IntToStr( zgl_Get( DESKTOP_HEIGHT ) ) );
  scr_GetResList();
  Result := TRUE;
end;

procedure scr_Destroy;
begin
  if wndFullScreen Then
    scr_Reset();
  {$IFDEF USE_X11}
  XRRFreeScreenConfigInfo( scrSettings );

  XDestroyIC( appXIC );
  XCloseIM( appXIM );
  {$ENDIF}

  scrResList.Count := 0;
  SetLength( scrResList.Width, 0 );
  SetLength( scrResList.Height, 0 );

  scrInitialized := FALSE;
end;

procedure scr_Clear;
var
  depthClr: Byte;
  m  : GLboolean;
  dp : GLInt;
  dch : Boolean;
begin
  batch2d_Flush();

  depthClr:=Byte( appFlags and DEPTH_BUFFER_CLEAR > 0 );
  dch:=depthClr>0;
  if dch then begin
    glGetIntegerv(GL_DEPTH_WRITEMASK, @dp);
    dch:=dp<>GL_TRUE;
    if dch then glDepthMask(GL_TRUE);
  end else
    dch:=false;

  glClear( GL_COLOR_BUFFER_BIT * Byte( appFlags and COLOR_BUFFER_CLEAR > 0 )
           or GL_DEPTH_BUFFER_BIT * depthClr
           or GL_STENCIL_BUFFER_BIT * Byte( appFlags and STENCIL_BUFFER_CLEAR > 0 ) );

  if dch then glDepthMask(dp);
end;

procedure scr_Flush;
begin
  batch2d_Flush();
{$IFNDEF USE_GLES}
  {$IFDEF LINUX}
  glXSwapBuffers( scrDisplay, wndHandle );
  {$ENDIF}
  {$IFDEF WINDOWS}
  SwapBuffers( wndDC );
  {$ENDIF}
  {$IFDEF MACOSX}
  aglSwapBuffers( oglContext );
  {$ENDIF}
{$ELSE}
  {$IFNDEF NO_EGL}
  eglSwapBuffers( oglDisplay, oglSurface );
  {$ENDIF}
  {$IFDEF iOS}
  eglContext.presentRenderbuffer( GL_RENDERBUFFER );
  {$ENDIF}
{$ENDIF}
end;

function scr_SetOptions( Width, Height, Refresh : Word; FullScreen, VSync : Boolean ) : Boolean;
  {$IFDEF USE_X11}
  var
    modeToSet : Integer;
    mode      : PXRRScreenSize;
  {$ENDIF}
  {$IFDEF WINDOWS}
  var
    i : Integer;
    r : Integer;
  {$ENDIF}
  {$IFDEF MACOSX}
  var
    b : Integer;
  {$ENDIF}
begin
{$IF DEFINED(iOS) or DEFINED(ANDROID)}
  Width      := scrDesktopW;
  Height     := scrDesktopH;
  Refresh    := REFRESH_DEFAULT;
  FullScreen := TRUE;
  VSync      := TRUE;
{$IFEND}


  // On Windows Width = 0 and Height = 0 can cause a BSOD xD
  if ( FullScreen ) and ( ( Width = 0 ) or ( Height = 0 ) ) Then
    begin
      FullScreen := FALSE;
      Width := 1;
      Height := 1;
    end;

  Result        := TRUE;
  wndWidth      := Width;
  wndHeight     := Height;
  scrRefresh    := Refresh;
  wndFullScreen := FullScreen;
  scrVsync      := VSync;

  if (Height >= zgl_Get( DESKTOP_HEIGHT )) and scrAutoFullScreen Then
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
{$IFDEF USE_X11}
  if wndFullScreen Then
    begin
      scrCurrent := -1;
      mode       := scrModeList;

      for modeToSet := 0 to scrModeCount - 1 do
        if ( mode.Width = scrWidth ) and ( mode.Height = scrHeight ) Then
          begin
            scrCurrent := modeToSet;
            break;
          end else
            INC( mode );

      if ( scrCurrent = -1 ) or ( XRRSetScreenConfig( scrDisplay, scrSettings, wndRoot, scrCurrent, scrRotation, CurrentTime ) <> 0 ) Then
        begin
          scrCurrent    := scrDesktop;
          wndFullScreen := FALSE;
          Result        := FALSE;
          if appWork Then
            begin
              wnd_Update();
              u_Warning( 'Cannot set fullscreen mode: ' + u_IntToStr( Width ) + 'x' + u_IntToStr( Height ) );
            end;
          exit;
        end;
    end else
      scr_SetWindowedMode();
{$ENDIF}
{$IFDEF WINDOWS}
  if wndFullScreen Then
    begin
      i := 0;
      r := 0;
      FillChar( scrSettings, SizeOf( DEVMODEW ), 0 );
      scrSettings.dmSize := SizeOf( DEVMODEW );
      while EnumDisplaySettingsW( scrMonInfo.szDevice, i, scrSettings ) <> FALSE do
        with scrSettings do
          begin
            dmFields := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or DM_DISPLAYFREQUENCY;
            if ( dmPelsWidth = scrWidth  ) and ( dmPelsHeight = scrHeight ) and ( dmBitsPerPel = 32 ) and ( dmDisplayFrequency > r ) and
               ( dmDisplayFrequency <= scrDesktop.dmDisplayFrequency ) Then
              begin
                if ChangeDisplaySettingsExW( scrMonInfo.szDevice, scrSettings, 0, CDS_TEST, nil ) = DISP_CHANGE_SUCCESSFUL Then
                  r := dmDisplayFrequency
                else
                  break;
              end;
            INC( i );
          end;

      with scrSettings do
        begin
          if scrRefresh = REFRESH_MAXIMUM Then scrRefresh := r;
          if scrRefresh = REFRESH_DEFAULT Then scrRefresh := 0;

          dmPelsWidth        := scrWidth;
          dmPelsHeight       := scrHeight;
          dmBitsPerPel       := 32;
          dmDisplayFrequency := scrRefresh;
          dmFields           := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL or DM_DISPLAYFREQUENCY;
        end;

      if ChangeDisplaySettingsExW( scrMonInfo.szDevice, scrSettings, 0, CDS_TEST, nil ) <> DISP_CHANGE_SUCCESSFUL Then
        begin
          wndFullScreen := FALSE;
          Result        := FALSE;
          if appWork Then
            begin
              wnd_Update();
              u_Warning( 'Cannot set fullscreen mode: ' + u_IntToStr( Width ) + 'x' + u_IntToStr( Height ) );
            end;
          exit;
        end else
          ChangeDisplaySettingsExW( scrMonInfo.szDevice, scrSettings, 0, CDS_FULLSCREEN, nil );
    end else
      scr_SetWindowedMode();
{$ENDIF}
{$IFDEF MACOSX}
  if wndFullScreen Then
    begin
      if ( scrRefresh <> 0 ) and ( scrRefresh <> 1 ) Then
        begin
          scrSettings := CGDisplayBestModeForParametersAndRefreshRate( scrDisplay, 32, scrWidth, scrHeight, scrRefresh, b );
          scrRefresh  := b;
        end;
      if ( scrRefresh = 0 ) or ( scrRefresh = 1 ) Then
        scrSettings := CGDisplayBestModeForParameters( scrDisplay, 32, scrWidth, scrHeight, b );

      if ( b <> 1 ) or ( CGDisplaySwitchToMode( scrDisplay, scrSettings ) <> kCGErrorSuccess ) Then
        begin
          wndFullScreen := FALSE;
          Result        := FALSE;
          if appWork Then
            begin
              wnd_Update();
              u_Warning( 'Cannot set fullscreen mode: ' + u_IntToStr( Width ) + 'x' + u_IntToStr( Height ) );
            end;
          exit;
        end;

      HideMenuBar();
    end else
      scr_SetWindowedMode();
{$ENDIF}
  if wndFullScreen Then
    log_Add( 'Screen options changed to: ' + u_IntToStr( scrWidth ) + ' x ' + u_IntToStr( scrHeight ) + ' fullscreen' )
  else
    log_Add( 'Screen options changed to: ' + u_IntToStr( wndWidth ) + ' x ' + u_IntToStr( wndHeight ) + ' windowed' );
  if appWork Then
    wnd_Update();
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
{$IFNDEF USE_GLES}
  {$IFDEF USE_X11}
  if oglCanVSync Then
    glXSwapIntervalSGI( Integer( scrVSync ) );
  {$ENDIF}
  {$IFDEF WINDOWS}
  if oglCanVSync Then
    wglSwapInterval( Integer( scrVSync ) );
  {$ENDIF}
  {$IFDEF MACOSX}
  if Assigned( oglContext ) Then
    aglSetInt( oglContext, AGL_SWAP_INTERVAL, Byte( scrVSync ) );
  {$ENDIF}
{$ELSE}
  {$IFNDEF NO_EGL}
  if oglCanVSync Then
    eglSwapInterval( oglDisplay, Integer( scrVSync ) );
  {$ENDIF}
{$ENDIF}
end;

procedure scr_SetFSAA( FSAA : Byte );
begin
  if oglFSAA = FSAA Then exit;
  oglFSAA := FSAA;

  gl_Destroy();
  log_Add( 'Start reinitialization of OpenGL' );

  gl_Create();
  wnd_Destroy();
  wnd_Create( wndWidth, wndHeight );
  wnd_SetCaption( wndCaption );
  gl_Initialize();
  if oglFSAA <> 0 Then
    log_Add( 'FSAA changed to: ' + u_IntToStr( oglFSAA ) + 'x' )
  else
    log_Add( 'FSAA changed to: off' );
end;

procedure scr_ReadPixels( var pData : Pointer; X, Y, Width, Height : Word );
begin
  batch2d_Flush();
  GetMem( pData, Width * Height * 4 );
  glReadPixels( X, oglHeight - Height - Y, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData );
end;

end.
