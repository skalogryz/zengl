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
unit zgl_window;

{$I zgl_config.cfg}
{$IFDEF iOS}
  {$modeswitch objectivec1}
{$ENDIF}

{$IFDEF WINCE}
  {$R zgl_wince.rc}
{$ENDIF}

interface
uses
  {$IFDEF LINUX}
  X, XLib, XUtil
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows
  {$ENDIF}
  {$IFDEF MACOSX}
  MacOSAll
  {$ENDIF}
  {$IFDEF iOS}
  iPhoneAll, CGGeometry, CGAffineTransforms
  {$ENDIF}
  ;

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

  {$IFDEF LINUX}
  wndHandle      : TWindow;
  wndRoot        : TWindow;
  wndClass       : TXClassHint;
  wndAttr        : TXSetWindowAttributes;
  wndTitle       : TXTextProperty;
  wndValueMask   : LongWord;
  wndDestroyAtom : TAtom;
  wndProtocols   : TAtom;
  {$ENDIF}
  {$IFDEF WINDOWS}
  wndFirst     : Boolean = TRUE; // Microsoft Sucks! :)
  wndHandle    : HWND;
  wndDC        : HDC;
  wndINST      : HINST;
  wndClass     : {$IFNDEF WINCE}TWndClassExW{$ELSE}TWndClassW{$ENDIF};
  wndClassName : PWideChar = 'ZenGL';
  wndStyle     : LongWord;
  wndCpnSize   : Integer;
  wndBrdSizeX  : Integer;
  wndBrdSizeY  : Integer;
  wndCaptionW  : PWideChar;
  {$ENDIF}
  {$IFDEF MACOSX}
  wndHandle  : WindowRef;
  wndAttr    : WindowAttributes;
  wndEvents  : array[ 0..14 ] of EventTypeSpec;
  wndMouseIn : Boolean;
  {$ENDIF}
  {$IFDEF iOS}
  wndHandle   : UIWindow;
  wndViewCtrl : UIViewController;
  wndPortrait : Boolean;
  {$ENDIF}

implementation
uses
  zgl_main,
  zgl_application,
  zgl_screen,
  {$IFNDEF USE_GLES}
  zgl_opengl,
  zgl_opengl_all,
  {$ELSE}
  zgl_opengles,
  zgl_opengles_all,
  {$ENDIF}
  zgl_opengl_simple,
  zgl_utils;

{$IFNDEF FPC}
// Various versions of Delphi... sucks again
function LoadCursorW(hInstance: HINST; lpCursorName: PWideChar): HCURSOR; stdcall; external user32 name 'LoadCursorW';
{$ENDIF}

{$IFDEF LINUX}
procedure wnd_SetHints( Initialized : Boolean = TRUE );
  var
    sizehints : TXSizeHints;
begin
  FillChar( sizehints, SizeOf( TXSizeHints ), 0 );
  if wndFullScreen and Initialized Then
    sizehints.flags    := PBaseSize or PWinGravity
  else
    sizehints.flags    := PPosition or PSize or PMinSize or PMaxSize;
  sizehints.x          := wndX;
  sizehints.y          := wndY;
  sizehints.width      := wndWidth;
  sizehints.height     := wndHeight;
  sizehints.min_width  := wndWidth;
  sizehints.max_width  := wndWidth;
  sizehints.min_height := wndHeight;
  sizehints.max_height := wndHeight;
  XSetWMNormalHints( scrDisplay, wndHandle, @sizehints );
end;
{$ENDIF}

function wnd_Create( Width, Height : Integer ) : Boolean;
  {$IFDEF MACOSX}
  var
    size   : MacOSAll.Rect;
    status : OSStatus;
  {$ENDIF}
begin
  if wndHandle <> {$IFNDEF DARWIN} 0 {$ELSE} nil {$ENDIF} Then exit;

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
{$IFDEF LINUX}
  FillChar( wndAttr, SizeOf( wndAttr ), 0 );
  wndAttr.colormap   := XCreateColormap( scrDisplay, wndRoot, oglVisualInfo.visual, AllocNone );
  wndAttr.event_mask := ExposureMask or FocusChangeMask or ButtonPressMask or ButtonReleaseMask or PointerMotionMask or KeyPressMask or KeyReleaseMask or StructureNotifyMask;
  wndValueMask       := CWColormap or CWEventMask or CWOverrideRedirect or CWBorderPixel or CWBackPixel;
  wndHandle          := XCreateWindow( scrDisplay, wndRoot, wndX, wndY, wndWidth, wndHeight, 0, oglVisualInfo.depth, InputOutput, oglVisualInfo.visual, wndValueMask, @wndAttr );

  if wndHandle = 0 Then
    begin
      u_Error( 'Cannot create window' );
      exit;
    end;

  wnd_Select();

  wndClass.res_name  := 'ZenGL';
  wndClass.res_class := 'ZenGL Class';
  XSetClassHint( scrDisplay, wndHandle, @wndClass );
  wnd_SetHints( FALSE );

  wndDestroyAtom := XInternAtom( scrDisplay, 'WM_DELETE_WINDOW', FALSE );
  wndProtocols   := XInternAtom( scrDisplay, 'WM_PROTOCOLS', FALSE );
  XSetWMProtocols( scrDisplay, wndHandle, @wndDestroyAtom, 1 );
{$ENDIF}
{$IFDEF WINDOWS}
  wndCpnSize  := GetSystemMetrics( SM_CYCAPTION  );
  wndBrdSizeX := GetSystemMetrics( SM_CXDLGFRAME );
  {$IFDEF WINDESKTOP}
  wndBrdSizeY := GetSystemMetrics( SM_CYDLGFRAME );
  {$ENDIF}

  with wndClass do
    begin
      {$IFDEF WINDESKTOP}
      cbSize        := SizeOf( TWndClassExW );
      {$ENDIF}
      style         := CS_DBLCLKS {$IFDEF WINDESKTOP}or CS_OWNDC{$ENDIF};
      lpfnWndProc   := @app_ProcessMessages;
      cbClsExtra    := 0;
      cbWndExtra    := 0;
      hInstance     := wndINST;
      hIcon         := LoadIconW  ( wndINST, 'MAINICON' );
      {$IFDEF WINDESKTOP}
      hIconSm       := LoadIconW  ( wndINST, 'MAINICON' );
      {$ENDIF}
      hCursor       := LoadCursorW( wndINST, PWideChar( IDC_ARROW ) );
      lpszMenuName  := nil;
      hbrBackGround := GetStockObject( BLACK_BRUSH );
      lpszClassName := wndClassName;
    end;

  {$IFDEF WINDESKTOP}
  if RegisterClassExW( wndClass ) = 0 Then
  {$ELSE}
  if RegisterClass( wndClass ) = 0 Then
  {$ENDIF}
    begin
      u_Error( 'Cannot register window class' );
      exit;
    end;

  if wndFullScreen Then
    wndStyle := WS_POPUP or WS_VISIBLE or WS_SYSMENU
  else
    wndStyle := WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU or WS_VISIBLE;
  {$IFNDEF USE_GLES}
  if oglFormat = 0 Then
    wndHandle := CreateWindowExW( WS_EX_TOOLWINDOW, wndClassName, wndCaptionW, WS_POPUP, 0, 0, 0, 0, 0, 0, 0, nil )
  else
  {$ENDIF}
    wndHandle := CreateWindowExW( {$IFDEF WINDESKTOP}WS_EX_APPWINDOW or{$ENDIF} WS_EX_TOPMOST * Byte( wndFullScreen ), wndClassName, wndCaptionW, wndStyle, wndX, wndY,
                                  wndWidth  + ( wndBrdSizeX * 2 ) * Byte( not wndFullScreen ),
                                  wndHeight + ( wndBrdSizeY * 2 + wndCpnSize ) * Byte( not wndFullScreen ), 0, 0, wndINST, nil );

  if wndHandle = 0 Then
    begin
      u_Error( 'Cannot create window' );
      exit;
    end;

  wndDC := GetDC( wndHandle );
  if wndDC = 0 Then
    begin
      u_Error( 'Cannot get device context' );
      exit;
    end;
  wnd_Select();
{$ENDIF}
{$IFDEF MACOSX}
  size.Left   := wndX;
  size.Top    := wndY;
  size.Right  := wndX + wndWidth;
  size.Bottom := wndY + wndHeight;
  wndAttr     := kWindowCloseBoxAttribute or kWindowCollapseBoxAttribute or kWindowStandardHandlerAttribute;// or kWindowCompositingAttribute;
  if wndFullScreen Then
    wndAttr := wndAttr or kWindowNoTitleBarAttribute;
  status      := CreateNewWindow( kDocumentWindowClass, wndAttr, size, wndHandle );

  if ( status <> noErr ) or ( wndHandle = nil ) Then
    begin
      u_Error( 'Cannot create window' );
      exit;
    end;

  // Window
  wndEvents[ 0 ].eventClass := kEventClassWindow;
  wndEvents[ 0 ].eventKind  := kEventWindowClosed;
  wndEvents[ 1 ].eventClass := kEventClassWindow;
  wndEvents[ 1 ].eventKind  := kEventWindowActivated;
  wndEvents[ 2 ].eventClass := kEventClassWindow;
  wndEvents[ 2 ].eventKind  := kEventWindowDeactivated;
  wndEvents[ 3 ].eventClass := kEventClassWindow;
  wndEvents[ 3 ].eventKind  := kEventWindowCollapsed;
  wndEvents[ 4 ].eventClass := kEventClassWindow;
  wndEvents[ 4 ].eventKind  := kEventWindowBoundsChanged;
  // Keyboard
  wndEvents[ 5 ].eventClass := kEventClassKeyboard;
  wndEvents[ 5 ].eventKind  := kEventRawKeyDown;
  wndEvents[ 6 ].eventClass := kEventClassKeyboard;
  wndEvents[ 6 ].eventKind  := kEventRawKeyUp;
  wndEvents[ 7 ].eventClass := kEventClassKeyboard;
  wndEvents[ 7 ].eventKind  := kEventRawKeyRepeat;
  wndEvents[ 8 ].eventClass := kEventClassKeyboard;
  wndEvents[ 8 ].eventKind  := kEventRawKeyModifiersChanged;
  // Mouse
  wndEvents[ 9 ].eventClass  := kEventClassMouse;
  wndEvents[ 9 ].eventKind   := kEventMouseMoved;
  wndEvents[ 10 ].eventClass := kEventClassMouse;
  wndEvents[ 10 ].eventKind  := kEventMouseDown;
  wndEvents[ 11 ].eventClass := kEventClassMouse;
  wndEvents[ 11 ].eventKind  := kEventMouseUp;
  wndEvents[ 12 ].eventClass := kEventClassMouse;
  wndEvents[ 12 ].eventKind  := kEventMouseWheelMoved;
  wndEvents[ 13 ].eventClass := kEventClassMouse;
  wndEvents[ 13 ].eventKind  := kEventMouseDragged;
  // Command
  wndEvents[ 14 ].eventClass := kEventClassCommand;
  wndEvents[ 14 ].eventKind  := kEventProcessCommand;
  InstallEventHandler( GetApplicationEventTarget, NewEventHandlerUPP( @app_ProcessMessages ), 15, @wndEvents[ 0 ], nil, nil );
  wnd_Select();
{$ENDIF}
{$IFDEF iOS}
  UIApplication.sharedApplication.setStatusBarHidden( wndFullScreen );
  wndHandle := zglCiOSWindow.alloc().initWithFrame( CGRectMake( wndX, wndY, Width, Height ) );
  wndHandle.setMultipleTouchEnabled( TRUE );
  wndViewCtrl := zglCiOSViewController.alloc().init();
  wndHandle.addSubview( wndViewCtrl.view );

  wnd_Select();
{$ENDIF}
  Result := TRUE;
end;

procedure wnd_Destroy;
begin
{$IFDEF LINUX}
  XDestroyWindow( scrDisplay, wndHandle );
  XSync( scrDisplay, X_FALSE );
{$ENDIF}
{$IFDEF WINDOWS}
  if ( wndDC > 0 ) and ( ReleaseDC( wndHandle, wndDC ) = 0 ) Then
    begin
      u_Error( 'Cannot release device context' );
      wndDC := 0;
    end;

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
{$ENDIF}
{$IFDEF MACOSX}
  ReleaseWindow( wndHandle );
{$ENDIF}
  wndHandle := {$IFNDEF DARWIN} 0 {$ELSE} nil {$ENDIF};
end;

procedure wnd_Update;
  {$IFDEF LINUX}
  var
    event : TXEvent;
  {$ENDIF}
  {$IFDEF WINDOWS}
  var
    FullScreen : Boolean;
  {$ENDIF}
begin
{$IFDEF LINUX}
  XSync( scrDisplay, X_TRUE );
  wnd_SetHints();

  FillChar( event, SizeOf( TXEvent ), 0 );
  event._type                := ClientMessage;
  event.xclient._type        := ClientMessage;
  event.xclient.send_event   := X_TRUE;
  event.xclient.window       := wndHandle;
  event.xclient.message_type := XInternAtom( scrDisplay, '_NET_WM_STATE', FALSE );
  event.xclient.format       := 32;
  event.xclient.data.l[ 0 ]  := Integer( wndFullScreen );
  event.xclient.data.l[ 1 ]  := XInternAtom( scrDisplay, '_NET_WM_STATE_FULLSCREEN', FALSE );
  XSendEvent( scrDisplay, wndRoot, False, SubstructureRedirectMask or SubstructureNotifyMask, @event );
{$ENDIF}
{$IFDEF WINDOWS}
  if appFocus Then
    FullScreen := wndFullScreen
  else
    begin
      FullScreen := FALSE;
      if ( wndWidth = zgl_Get( DESKTOP_WIDTH ) ) and ( wndHeight = zgl_Get( DESKTOP_HEIGHT ) ) Then
        begin
          ShowWindow( wndHandle, SW_MINIMIZE );
          exit;
        end;
    end;

  if FullScreen Then
    wndStyle := WS_POPUP or WS_VISIBLE or WS_SYSMENU
  else
    wndStyle := WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU or WS_VISIBLE;

  SetWindowLongW( wndHandle, GWL_STYLE, wndStyle );
  SetWindowLongW( wndHandle, GWL_EXSTYLE, {$IFDEF WINDESKTOP}WS_EX_APPWINDOW or{$ENDIF} WS_EX_TOPMOST * Byte( FullScreen ) );
{$ENDIF}
{$IFDEF MACOSX}
  if wndFullScreen Then
    ChangeWindowAttributes( wndHandle, kWindowNoTitleBarAttribute, kWindowResizableAttribute )
  else
    ChangeWindowAttributes( wndHandle, kWindowResizableAttribute, kWindowNoTitleBarAttribute );
  // Какой индус из Apple придумал, что необходимо менять kWindowResizableAttribute вместе с kWindowNoTitleBarAttribute...
  ChangeWindowAttributes( wndHandle, 0, kWindowResizableAttribute );

  aglSetCurrentContext( oglContext );
{$ENDIF}
{$IFDEF iOS}
  UIApplication.sharedApplication.setStatusBarHidden( wndFullScreen );
{$ENDIF}
  appWork := TRUE;
  wnd_SetCaption( wndCaption );

  if ( not wndFullScreen ) and ( appFlags and WND_USE_AUTOCENTER > 0 ) Then
    wnd_SetPos( ( zgl_Get( DESKTOP_WIDTH ) - wndWidth ) div 2, ( zgl_Get( DESKTOP_HEIGHT ) - wndHeight ) div 2 );
  wnd_SetSize( wndWidth, wndHeight );
end;

procedure wnd_SetCaption( const NewCaption : String );
  {$IFNDEF iOS}
  var
  {$ENDIF}
  {$IFDEF LINUX}
    err : Integer;
    str : PChar;
  {$ENDIF}
  {$IFDEF WINDOWS}
    len : Integer;
  {$ENDIF}
  {$IFDEF MACOSX}
    str : CFStringRef;
  {$ENDIF}
begin
  wndCaption := u_CopyStr( NewCaption );
{$IFDEF LINUX}
  if wndHandle <> 0 Then
    begin
      str := u_GetPChar( wndCaption );
      if appFlags and APP_USE_UTF8 > 0 Then
        err := Xutf8TextListToTextProperty( scrDisplay, @str, 1, XUTF8StringStyle, @wndTitle )
      else
        err := XStringListToTextProperty( @str, 1, @wndTitle );

      if err = 0 Then
        begin
          XSetWMName( scrDisplay, wndHandle, @wndTitle );
          XSetWMIconName( scrDisplay, wndHandle, @wndTitle );
        end;
      FreeMem( str );
      XFree( wndTitle.value );
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  {$IFNDEF FPC}
  if SizeOf( Char ) = 2 Then
    begin
      len := 2;
      wndCaptionW := PWideChar( wndCaption );
    end else
  {$ENDIF}
  len := 1;
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
      if appFlags and APP_USE_UTF8 = 0 Then
        wndCaption := wndCaptionW;
    end;

  if wndHandle <> 0 Then
    SetWindowTextW( wndHandle, wndCaptionW );
{$ENDIF}
{$IFDEF MACOSX}
  if Assigned( wndHandle ) Then
    begin
      if appFlags and APP_USE_UTF8 = 0 Then
        str := CFStringCreateWithPascalString( nil, wndCaption, kCFStringEncodingASCII )
      else
        str := CFStringCreateWithPascalString( nil, wndCaption, kCFStringEncodingUTF8 );
      SetWindowTitleWithCFString( wndHandle, str );
      CFRelease( str );
      wnd_Select();
    end;
{$ENDIF}
end;

procedure wnd_SetSize( Width, Height : Integer );
begin
  wndWidth  := Width;
  wndHeight := Height;
{$IFDEF LINUX}
  if ( not appInitedToHandle ) and ( wndHandle <> 0 ) Then
    begin
      wnd_SetHints();
      XResizeWindow( scrDisplay, wndHandle, wndWidth, wndHeight );
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  if not appInitedToHandle Then
    wnd_SetPos( wndX, wndY );
{$ENDIF}
{$IFDEF MACOSX}
  if ( not appInitedToHandle ) and Assigned( wndHandle ) Then
    begin
      SizeWindow( wndHandle, wndWidth, wndHeight, TRUE );
      aglUpdateContext( oglContext );
      wnd_Select();
    end;
{$ENDIF}
{$IFDEF iOS}
  if wndPortrait and scrCanPortrait Then
    eglView.setFrame( CGRectMake( 0, 0, UIScreen.mainScreen.bounds.size.width, UIScreen.mainScreen.bounds.size.height ) )
  else
    eglView.setFrame( CGRectMake( 0, 0, UIScreen.mainScreen.bounds.size.height, UIScreen.mainScreen.bounds.size.width ) );
  eglContext.renderbufferStorage_fromDrawable( GL_RENDERBUFFER, eglSurface );
{$ENDIF}
  oglWidth  := Width;
  oglHeight := Height;
  if appFlags and CORRECT_RESOLUTION > 0 Then
    scr_CorrectResolution( scrResW, scrResH )
  else
    SetCurrentMode();
end;

procedure wnd_SetPos( X, Y : Integer );
  {$IFDEF WINDOWS}
  var
    mode : LongWord;
  {$ENDIF}
begin
  wndX := X;
  wndY := Y;
{$IFDEF LINUX}
  if wndHandle <> 0 Then
    if not wndFullScreen Then
      XMoveWindow( scrDisplay, wndHandle, X, Y )
    else
      XMoveWindow( scrDisplay, wndHandle, 0, 0 );
{$ENDIF}
{$IFDEF WINDOWS}
  if not appFocus Then
    mode := HWND_BOTTOM
  else
    mode := HWND_TOPMOST;

  if wndHandle <> 0 Then
    if ( not wndFullScreen ) or ( not appFocus ) Then
      SetWindowPos( wndHandle, mode, wndX, wndY, wndWidth + ( wndBrdSizeX * 2 ), wndHeight + ( wndBrdSizeY * 2 + wndCpnSize ), SWP_NOACTIVATE )
    else
      SetWindowPos( wndHandle, mode, 0, 0, wndWidth, wndHeight, SWP_NOACTIVATE );
{$ENDIF}
{$IFDEF MACOSX}
  if Assigned( wndHandle ) Then
    if not wndFullScreen Then
      MoveWindow( wndHandle, wndX, wndY, TRUE )
    else
      MoveWindow( wndHandle, 0, 0, TRUE );
{$ENDIF}
{$IFDEF iOS}
  wndX := 0;
  wndY := 0;
{$ENDIF}
end;

procedure wnd_ShowCursor( Show : Boolean );
{$IFDEF LINUX}
  var
    mask   : TPixmap;
    xcolor : TXColor;
begin
  appShowCursor := Show;

  if wndHandle = 0 Then exit;
  if Show Then
    begin
      if appCursor <> None Then
        begin
          XFreeCursor( scrDisplay, appCursor );
          appCursor := None;
          XDefineCursor( scrDisplay, wndHandle, appCursor );
        end;
    end else
      begin
        mask := XCreatePixmap( scrDisplay, wndRoot, 1, 1, 1 );
        FillChar( xcolor, SizeOf( xcolor ), 0 );
        appCursor := XCreatePixmapCursor( scrDisplay, mask, mask, @xcolor, @xcolor, 0, 0 );
        XDefineCursor( scrDisplay, wndHandle, appCursor );
      end;
{$ENDIF}
{$IF DEFINED(WINDOWS) or DEFINED(MACOSX) or DEFINED(iOS)}
begin
  appShowCursor := Show;
{$IFEND}
end;

procedure wnd_Select;
begin
{$IFDEF LINUX}
  XMapWindow( scrDisplay, wndHandle );
{$ENDIF}
{$IFDEF WINDOWS}
  BringWindowToTop( wndHandle );
{$ENDIF}
{$IFDEF MACOSX}
  SelectWindow( wndHandle );
  ShowWindow( wndHandle );
  if wndFullScreen Then
    wnd_SetPos( 0, 0 );
{$ENDIF}
{$IFDEF iOS}
  wndHandle.makeKeyAndVisible();
{$ENDIF}
end;

initialization
  wndCaption := cs_ZenGL;

end.
