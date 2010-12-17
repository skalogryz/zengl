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

interface
uses
  {$IFDEF LINUX}
  X, XLib, XUtil
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll
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
  wnd_X          : Integer;
  wnd_Y          : Integer;
  wnd_Width      : Integer = 800;
  wnd_Height     : Integer = 600;
  wnd_FullScreen : Boolean;
  wnd_Caption    : String;

  {$IFDEF LINUX}
  wnd_Handle      : TWindow;
  wnd_Root        : TWindow;
  wnd_Class       : TXClassHint;
  wnd_Attr        : TXSetWindowAttributes;
  wnd_Title       : TXTextProperty;
  wnd_ValueMask   : LongWord;
  wnd_DestroyAtom : TAtom;
  wnd_Protocols   : TAtom;
  {$ENDIF}
  {$IFDEF WINDOWS}
  wnd_First     : Boolean = TRUE; // Microsoft Sucks! :)
  wnd_Handle    : HWND;
  wnd_DC        : HDC;
  wnd_INST      : HINST;
  wnd_Class     : TWndClassExW;
  wnd_ClassName : PWideChar = 'ZenGL';
  wnd_Style     : LongWord;
  wnd_CpnSize   : Integer;
  wnd_BrdSizeX  : Integer;
  wnd_BrdSizeY  : Integer;
  wnd_CaptionW  : PWideChar;
  {$ENDIF}
  {$IFDEF DARWIN}
  wnd_Handle  : WindowRef;
  wnd_Attr    : WindowAttributes;
  wnd_Events  : array[ 0..14 ] of EventTypeSpec;
  wnd_MouseIn : Boolean;
  {$ENDIF}

implementation
uses
  zgl_main,
  zgl_application,
  zgl_screen,
  zgl_opengl,
  zgl_opengl_all,
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
  if wnd_FullScreen and Initialized Then
    sizehints.flags    := PBaseSize or PWinGravity
  else
    sizehints.flags    := PPosition or PSize or PMinSize or PMaxSize;
  sizehints.x          := wnd_X;
  sizehints.y          := wnd_Y;
  sizehints.width      := wnd_Width;
  sizehints.height     := wnd_Height;
  sizehints.min_width  := wnd_Width;
  sizehints.max_width  := wnd_Width;
  sizehints.min_height := wnd_Height;
  sizehints.max_height := wnd_Height;
  XSetWMNormalHints( scr_Display, wnd_Handle, @sizehints );
end;
{$ENDIF}

function wnd_Create( Width, Height : Integer ) : Boolean;
  {$IFDEF DARWIN}
  var
    size   : MacOSAll.Rect;
    status : OSStatus;
  {$ENDIF}
begin
  Result     := FALSE;
  wnd_Width  := Width;
  wnd_Height := Height;

  if app_Flags and WND_USE_AUTOCENTER > 0 Then
    begin
      wnd_X := ( zgl_Get( DESKTOP_WIDTH ) - wnd_Width ) div 2;
      wnd_Y := ( zgl_Get( DESKTOP_HEIGHT ) - wnd_Height ) div 2;
    end;
{$IFDEF LINUX}
  FillChar( wnd_Attr, SizeOf( wnd_Attr ), 0 );
  wnd_Attr.colormap   := XCreateColormap( scr_Display, wnd_Root, ogl_VisualInfo.visual, AllocNone );
  wnd_Attr.event_mask := ExposureMask or FocusChangeMask or ButtonPressMask or ButtonReleaseMask or PointerMotionMask or KeyPressMask or KeyReleaseMask or StructureNotifyMask;
  wnd_ValueMask       := CWColormap or CWEventMask or CWOverrideRedirect or CWBorderPixel or CWBackPixel;
  wnd_Handle          := XCreateWindow( scr_Display, wnd_Root, wnd_X, wnd_Y, wnd_Width, wnd_Height, 0, ogl_VisualInfo.depth, InputOutput, ogl_VisualInfo.visual,
                                        wnd_ValueMask, @wnd_Attr );

  if wnd_Handle = 0 Then
    begin
      u_Error( 'Cannot create window' );
      exit;
    end;

  wnd_Select();

  wnd_Class.res_name  := 'ZenGL';
  wnd_Class.res_class := 'ZenGL Class';
  XSetClassHint( scr_Display, wnd_Handle, @wnd_Class );
  wnd_SetHints( FALSE );

  wnd_DestroyAtom := XInternAtom( scr_Display, 'WM_DELETE_WINDOW', FALSE );
  wnd_Protocols   := XInternAtom( scr_Display, 'WM_PROTOCOLS', FALSE );
  XSetWMProtocols( scr_Display, wnd_Handle, @wnd_DestroyAtom, 1 );
{$ENDIF}
{$IFDEF WINDOWS}
  wnd_CpnSize  := GetSystemMetrics( SM_CYCAPTION  );
  wnd_BrdSizeX := GetSystemMetrics( SM_CXDLGFRAME );
  wnd_BrdSizeY := GetSystemMetrics( SM_CYDLGFRAME );

  with wnd_Class do
    begin
      cbSize        := SizeOf( TWndClassExW );
      style         := CS_DBLCLKS or CS_OWNDC;
      lpfnWndProc   := @app_ProcessMessages;
      cbClsExtra    := 0;
      cbWndExtra    := 0;
      hInstance     := wnd_INST;
      hIcon         := LoadIconW  ( wnd_INST, 'MAINICON' );
      hIconSm       := LoadIconW  ( wnd_INST, 'MAINICON' );
      hCursor       := LoadCursorW( wnd_INST, PWideChar( IDC_ARROW ) );
      lpszMenuName  := nil;
      hbrBackGround := GetStockObject( BLACK_BRUSH );
      lpszClassName := wnd_ClassName;
    end;

  if RegisterClassExW( wnd_Class ) = 0 Then
    begin
      u_Error( 'Cannot register window class' );
      exit;
    end;

  if wnd_FullScreen Then
    begin
      wnd_X     := 0;
      wnd_Y     := 0;
      wnd_Style := WS_POPUP or WS_VISIBLE or WS_SYSMENU;
    end else
      wnd_Style := WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU or WS_VISIBLE;
  if ogl_Format = 0 Then
    wnd_Handle := CreateWindowExW( WS_EX_TOOLWINDOW, wnd_ClassName, wnd_CaptionW, WS_POPUP, 0, 0, 0, 0, 0, 0, 0, nil )
  else
    wnd_Handle := CreateWindowExW( WS_EX_APPWINDOW or WS_EX_TOPMOST * Byte( wnd_FullScreen ), wnd_ClassName, wnd_CaptionW, wnd_Style, wnd_X, wnd_Y,
                                   wnd_Width  + ( wnd_BrdSizeX * 2 ) * Byte( not wnd_FullScreen ),
                                   wnd_Height + ( wnd_BrdSizeY * 2 + wnd_CpnSize ) * Byte( not wnd_FullScreen ), 0, 0, wnd_INST, nil );

  if wnd_Handle = 0 Then
    begin
      u_Error( 'Cannot create window' );
      exit;
    end;

  wnd_DC := GetDC( wnd_Handle );
  if wnd_DC = 0 Then
    begin
      u_Error( 'Cannot get device context' );
      exit;
    end;
  wnd_Select();
{$ENDIF}
{$IFDEF DARWIN}
  size.Left   := wnd_X;
  size.Top    := wnd_Y;
  size.Right  := wnd_X + wnd_Width;
  size.Bottom := wnd_Y + wnd_Height;
  wnd_Attr    := kWindowCloseBoxAttribute or kWindowCollapseBoxAttribute or kWindowStandardHandlerAttribute;// or kWindowCompositingAttribute;
  if wnd_FullScreen Then
    wnd_Attr := wnd_Attr or kWindowNoTitleBarAttribute;
  status      := CreateNewWindow( kDocumentWindowClass, wnd_Attr, size, wnd_Handle );

  if ( status <> noErr ) or ( wnd_Handle = nil ) Then
    begin
      u_Error( 'Cannot create window' );
      exit;
    end;

  // Window
  wnd_Events[ 0 ].eventClass := kEventClassWindow;
  wnd_Events[ 0 ].eventKind  := kEventWindowClosed;
  wnd_Events[ 1 ].eventClass := kEventClassWindow;
  wnd_Events[ 1 ].eventKind  := kEventWindowActivated;
  wnd_Events[ 2 ].eventClass := kEventClassWindow;
  wnd_Events[ 2 ].eventKind  := kEventWindowDeactivated;
  wnd_Events[ 3 ].eventClass := kEventClassWindow;
  wnd_Events[ 3 ].eventKind  := kEventWindowCollapsed;
  wnd_Events[ 4 ].eventClass := kEventClassWindow;
  wnd_Events[ 4 ].eventKind  := kEventWindowBoundsChanged;
  // Keyboard
  wnd_Events[ 5 ].eventClass := kEventClassKeyboard;
  wnd_Events[ 5 ].eventKind  := kEventRawKeyDown;
  wnd_Events[ 6 ].eventClass := kEventClassKeyboard;
  wnd_Events[ 6 ].eventKind  := kEventRawKeyUp;
  wnd_Events[ 7 ].eventClass := kEventClassKeyboard;
  wnd_Events[ 7 ].eventKind  := kEventRawKeyRepeat;
  wnd_Events[ 8 ].eventClass := kEventClassKeyboard;
  wnd_Events[ 8 ].eventKind  := kEventRawKeyModifiersChanged;
  // Mouse
  wnd_Events[ 9 ].eventClass  := kEventClassMouse;
  wnd_Events[ 9 ].eventKind   := kEventMouseMoved;
  wnd_Events[ 10 ].eventClass := kEventClassMouse;
  wnd_Events[ 10 ].eventKind  := kEventMouseDown;
  wnd_Events[ 11 ].eventClass := kEventClassMouse;
  wnd_Events[ 11 ].eventKind  := kEventMouseUp;
  wnd_Events[ 12 ].eventClass := kEventClassMouse;
  wnd_Events[ 12 ].eventKind  := kEventMouseWheelMoved;
  wnd_Events[ 13 ].eventClass := kEventClassMouse;
  wnd_Events[ 13 ].eventKind  := kEventMouseDragged;
  // Command
  wnd_Events[ 14 ].eventClass := kEventClassCommand;
  wnd_Events[ 14 ].eventKind  := kEventProcessCommand;
  InstallEventHandler( GetApplicationEventTarget, NewEventHandlerUPP( @app_ProcessMessages ), 15, @wnd_Events[ 0 ], nil, nil );
  wnd_Select();
{$ENDIF}
  Result := TRUE;
end;

procedure wnd_Destroy;
begin
{$IFDEF LINUX}
  XDestroyWindow( scr_Display, wnd_Handle );
{$ENDIF}
{$IFDEF WINDOWS}
  if ( wnd_DC > 0 ) and ( ReleaseDC( wnd_Handle, wnd_DC ) = 0 ) Then
    begin
      u_Error( 'Cannot release device context' );
      wnd_DC := 0;
    end;

  if ( wnd_Handle <> 0 ) and ( not DestroyWindow( wnd_Handle ) ) Then
    begin
      u_Error( 'Cannot destroy window' );
      wnd_Handle := 0;
    end;

  if not UnRegisterClassW( wnd_ClassName, wnd_INST ) Then
    begin
      u_Error( 'Cannot unregister window class' );
      wnd_INST := 0;
    end;
{$ENDIF}
{$IFDEF DARWIN}
  ReleaseWindow( wnd_Handle );
{$ENDIF}
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
  XSync( scr_Display, X_TRUE );
  wnd_SetHints();

  FillChar( event, SizeOf( TXEvent ), 0 );
  event._type                := ClientMessage;
  event.xclient._type        := ClientMessage;
  event.xclient.send_event   := X_TRUE;
  event.xclient.window       := wnd_Handle;
  event.xclient.message_type := XInternAtom( scr_Display, '_NET_WM_STATE', FALSE );
  event.xclient.format       := 32;
  event.xclient.data.l[ 0 ]  := Integer( wnd_FullScreen );
  event.xclient.data.l[ 1 ]  := XInternAtom( scr_Display, '_NET_WM_STATE_FULLSCREEN', FALSE );
  XSendEvent( scr_Display, wnd_Root, False, SubstructureRedirectMask or SubstructureNotifyMask, @event );
{$ENDIF}
{$IFDEF WINDOWS}
  if app_Focus Then
    FullScreen := wnd_FullScreen
  else
    FullScreen := FALSE;

  if FullScreen Then
    wnd_Style := WS_POPUP or WS_VISIBLE or WS_SYSMENU
  else
    wnd_Style := WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU or WS_VISIBLE;

  SetWindowLongW( wnd_Handle, GWL_STYLE, wnd_Style );
  SetWindowLongW( wnd_Handle, GWL_EXSTYLE, WS_EX_APPWINDOW or WS_EX_TOPMOST * Byte( FullScreen ) );
{$ENDIF}
{$IFDEF DARWIN}
  if wnd_FullScreen Then
    ChangeWindowAttributes( wnd_Handle, kWindowNoTitleBarAttribute, kWindowResizableAttribute )
  else
    ChangeWindowAttributes( wnd_Handle, kWindowResizableAttribute, kWindowNoTitleBarAttribute );
  // Какой индус из Apple придумал, что необходимо менять kWindowResizableAttribute вместе с kWindowNoTitleBarAttribute...
  ChangeWindowAttributes( wnd_Handle, 0, kWindowResizableAttribute );

  aglSetCurrentContext( ogl_Context );
{$ENDIF}
  app_Work := TRUE;
  wnd_SetCaption( wnd_Caption );

  if app_Flags and WND_USE_AUTOCENTER > 0 Then
    wnd_SetPos( ( zgl_Get( DESKTOP_WIDTH ) - wnd_Width ) div 2, ( zgl_Get( DESKTOP_HEIGHT ) - wnd_Height ) div 2 );
  wnd_SetSize( wnd_Width, wnd_Height );
end;

procedure wnd_SetCaption( const NewCaption : String );
  {$IFDEF WINDOWS}
  var
    len : Integer;
  {$ENDIF}
begin
  wnd_Caption := NewCaption + #0;
{$IFDEF LINUX}
  if wnd_Handle <> 0 Then
    begin
      if app_Flags and APP_USE_UTF8 > 0 Then
        Xutf8TextListToTextProperty( scr_Display, @wnd_Caption, 1, XUTF8StringStyle, @wnd_Title )
      else
        XStringListToTextProperty( @wnd_Caption, 1, @wnd_Title );
      XSetWMName( scr_Display, wnd_Handle, @wnd_Title );
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  {$IFNDEF FPC}
  if SizeOf( Char ) = 2 Then
    begin
      len := 2;
      wnd_CaptionW := PWideChar( wnd_Caption );
    end else
  {$ENDIF}
  len := 1;
  if len = 1 Then
    begin
      if app_Flags and APP_USE_UTF8 = 0 Then
        wnd_Caption := AnsiToUtf8( NewCaption ) + #0;
      len := MultiByteToWideChar( CP_UTF8, 0, @wnd_Caption[ 1 ], length( wnd_Caption ), nil, 0 );
      if Assigned( wnd_CaptionW ) Then
        FreeMem( wnd_CaptionW );
      GetMem( wnd_CaptionW, len * 2 );
      MultiByteToWideChar( CP_UTF8, 0, @wnd_Caption[ 1 ], length( wnd_Caption ), wnd_CaptionW, len );
      if app_Flags and APP_USE_UTF8 = 0 Then
        wnd_Caption := wnd_CaptionW;
    end;

  if wnd_Handle <> 0 Then
    SetWindowTextW( wnd_Handle, wnd_CaptionW );
{$ENDIF}
{$IFDEF DARWIN}
  if Assigned( wnd_Handle ) Then
    begin
      SetWTitle( wnd_Handle, wnd_Caption );
      wnd_Select();
    end;
{$ENDIF}
end;

procedure wnd_SetSize( Width, Height : Integer );
begin
  wnd_Width  := Width;
  wnd_Height := Height;
{$IFDEF LINUX}
  if ( not app_InitToHandle ) and ( wnd_Handle <> 0 ) Then
    begin
      wnd_SetHints();
      XResizeWindow( scr_Display, wnd_Handle, wnd_Width, wnd_Height );
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  if not app_InitToHandle Then
    wnd_SetPos( wnd_X, wnd_Y );
{$ENDIF}
{$IFDEF DARWIN}
  if ( not app_InitToHandle ) and Assigned( wnd_Handle ) Then
    begin
      SizeWindow( wnd_Handle, wnd_Width, wnd_Height, TRUE );
      aglUpdateContext( ogl_Context );
      wnd_Select();
    end;
{$ENDIF}
  ogl_Width  := Width;
  ogl_Height := Height;
  if app_Flags and CORRECT_RESOLUTION > 0 Then
    scr_CorrectResolution( scr_ResW, scr_ResH )
  else
    SetCurrentMode();
end;

procedure wnd_SetPos( X, Y : Integer );
  {$IFDEF WINDOWS}
  var
    mode : LongWord;
  {$ENDIF}
begin
  wnd_X := X;
  wnd_Y := Y;
{$IFDEF LINUX}
  if wnd_Handle <> 0 Then
    if not wnd_FullScreen Then
      XMoveWindow( scr_Display, wnd_Handle, X, Y )
    else
      XMoveWindow( scr_Display, wnd_Handle, 0, 0 );
{$ENDIF}
{$IFDEF WINDOWS}
  if not app_Focus Then
    mode := HWND_BOTTOM
  else
    mode := HWND_TOPMOST;

  if wnd_Handle <> 0 Then
    if ( not wnd_FullScreen ) or ( not app_Focus ) Then
      SetWindowPos( wnd_Handle, mode, wnd_X, wnd_Y, wnd_Width + ( wnd_BrdSizeX * 2 ), wnd_Height + ( wnd_BrdSizeY * 2 + wnd_CpnSize ), SWP_NOACTIVATE )
    else
      SetWindowPos( wnd_Handle, mode, 0, 0, wnd_Width, wnd_Height, SWP_NOACTIVATE );
{$ENDIF}
{$IFDEF DARWIN}
  if Assigned( wnd_Handle ) Then
    if not wnd_FullScreen Then
      MoveWindow( wnd_Handle, wnd_X, wnd_Y, TRUE )
    else
      MoveWindow( wnd_Handle, 0, 0, TRUE );
{$ENDIF}
end;

procedure wnd_ShowCursor( Show : Boolean );
{$IFDEF LINUX}
  var
    mask   : TPixmap;
    xcolor : TXColor;
begin
  app_ShowCursor := Show;

  if wnd_Handle = 0 Then exit;
  if Show Then
    begin
      if app_Cursor <> None Then
        begin
          XFreeCursor( scr_Display, app_Cursor );
          app_Cursor := None;
          XDefineCursor( scr_Display, wnd_Handle, app_Cursor );
        end;
    end else
      begin
        mask := XCreatePixmap( scr_Display, wnd_Root, 1, 1, 1 );
        FillChar( xcolor, SizeOf( xcolor ), 0 );
        app_Cursor := XCreatePixmapCursor( scr_Display, mask, mask, @xcolor, @xcolor, 0, 0 );
        XDefineCursor( scr_Display, wnd_Handle, app_Cursor );
      end;
{$ENDIF}
{$IFDEF WINDOWS}
begin
  app_ShowCursor := Show;
{$ENDIF}
{$IFDEF DARWIN}
begin
  app_ShowCursor := Show;
{$ENDIF}
end;

procedure wnd_Select;
begin
{$IFDEF LINUX}
  XMapWindow( scr_Display, wnd_Handle );
{$ENDIF}
{$IFDEF WINDOWS}
  BringWindowToTop( wnd_Handle );
{$ENDIF}
{$IFDEF DARWIN}
  SelectWindow( wnd_Handle );
  ShowWindow( wnd_Handle );
  if wnd_FullScreen Then
    wnd_SetPos( 0, 0 );
{$ENDIF}
end;

initialization
  wnd_Caption := cs_ZenGL;

end.
