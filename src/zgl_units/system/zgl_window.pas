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
unit zgl_window;

{$I define.inc}

interface
uses
  GL, GLExt,
  {$IFDEF LINUX}
  GLX, X, XLib, XUtil,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  {$IFDEF DARWIN}
  AGL, MacOSAll,
  {$ENDIF}
  zgl_global_var,
  zgl_opengl_simple,
  zgl_log,

  Utils;

{const
  MWM_DECOR_ALL         = 1 shl 0;
  MWM_HINTS_DECORATIONS = 1 shl 1;

  PROP_MOTIF_WM_HINTS_ELEMENTS = 5;

type
  TPropMotifWmHints = record
    flags       : LongWord;
    functions   : LongWord;
    decorations : LongWord;
    inputMode   : LongInt;
    Status      : LongWord;
  end;}

function  wnd_Create( const Width, Height : WORD ) : Boolean;
procedure wnd_Destroy;
procedure wnd_Update;

procedure wnd_SetCaption( const NewCaption : String );
procedure wnd_SetSize( const Width, Height : WORD );
procedure wnd_SetPos( const X, Y : WORD );
procedure wnd_SetOnTop( const OnTop : Boolean );
procedure wnd_ShowCursor( const Show : Boolean );

implementation
uses
  zgl_main, zgl_screen;

function wnd_Create;
  {$IFDEF LINUX}
  var
    sizehints  : TXSizeHints;
    //motifhints : TPropMotifWmHints;
  {$ENDIF}
  {$IFDEF DARWIN}
  var
    size   : MAcOSAll.Rect;
    status : OSStatus;
    Events : array[ 0..9 ] of EventTypeSpec;
  {$ENDIF}
begin
  Result     := FALSE;
  wnd_Width  := Width;
  wnd_Height := Height;
{$IFDEF LINUX}
  ogl_X := 0;
  ogl_Y := 0;

  wnd_Attr.colormap   := XCreateColormap( scr_Display, wnd_Root, ogl_VisualInfo^.visual, AllocNone );
  wnd_Attr.event_mask := ExposureMask or
                         FocusChangeMask or
                         ButtonPressMask or
                         ButtonReleaseMask or
                         PointerMotionMask or
                         KeyPressMask or
                         KeyReleaseMask;

  if app_FullScreen Then
    begin
      wnd_X := 0;
      wnd_Y := 0;
      wnd_Attr.override_redirect := True;
      wnd_ValueMask := CWColormap or CWEventMask or CWOverrideRedirect or CWX or CWY or CWCursor;
    end else
      begin
        wnd_Attr.override_redirect := False;
        wnd_ValueMask := CWColormap or CWEventMask or CWX or CWY or CWCursor;
      end;

  wnd_Handle := XCreateWindow( scr_Display,
                               wnd_Root,
                               wnd_X, wnd_Y,
                               wnd_Width, wnd_Height,
                               0,
                               ogl_VisualInfo^.depth,
                               InputOutput,
                               ogl_VisualInfo^.visual,
                               wnd_ValueMask,
                               @wnd_Attr );

  if wnd_Handle = 0 Then
    begin
      u_Error( 'Cannot create window' );
      exit;
    end;

  sizehints.flags      := PMinSize or PMaxSize;
  sizehints.min_width  := wnd_Width;
  sizehints.max_width  := wnd_Width;
  sizehints.min_height := wnd_Height;
  sizehints.max_height := wnd_Height;

  //wnd_MotifAtom          := XInternAtom( scr_Display, '_MOTIF_WM_HINTS', TRUE );
  //motifhints.flags       := MWM_HINTS_DECORATIONS;
  //motifhints.decorations := MWM_DECOR_ALL * Byte( not app_FullScreen );
  //XChangeProperty( scr_Display, wnd_Handle, wnd_MotifAtom, wnd_MotifAtom, 32, 0, @motifhints, PROP_MOTIF_WM_HINTS_ELEMENTS );
  XSetWMNormalHints( scr_Display, wnd_Handle, @sizehints );

  XMapRaised( scr_Display, wnd_Handle );

  wnd_DestroyAtom := XInternAtom( scr_Display, 'WM_DELETE_WINDOW', TRUE );
  wnd_Protocols   := XInternAtom( scr_Display, 'WM_PROTOCOLS', TRUE );
  XSetWMProtocols( scr_Display, wnd_Handle, @wnd_DestroyAtom, 1 );

  if app_FullScreen Then
    begin
      XGrabKeyboard( scr_Display, wnd_Handle, True, GrabModeAsync, GrabModeAsync, CurrentTime );
      XGrabPointer( scr_Display, wnd_Handle, True, ButtonPressMask, GrabModeAsync, GrabModeAsync, wnd_Handle, None, CurrentTime );
    end else
      begin
        XUngrabKeyboard( scr_Display, CurrentTime );
        XUngrabPointer( scr_Display, CurrentTime );
      end;

  glXWaitX;
{$ENDIF}
{$IFDEF WIN32}
  wnd_CpnSize  := GetSystemMetrics( SM_CYCAPTION  );
  wnd_BrdSizeX := GetSystemMetrics( SM_CXDLGFRAME );
  wnd_BrdSizeY := GetSystemMetrics( SM_CYDLGFRAME );

  wnd_X := 0;
  wnd_Y := 0;

  with wnd_Class do
    begin
      cbSize        := SizeOf( TWndClassEx );
      style         := CS_DBLCLKS or CS_OWNDC;
      lpfnWndProc   := @zgl_Mess;
      cbClsExtra    := 0;
      cbWndExtra    := 0;
      hInstance     := wnd_INST;
      hIcon         := LoadIcon  ( wnd_INST, MakeIntResource( 'MAINICON' ) );
      hIconSm       := LoadIcon  ( wnd_INST, MakeIntResource( 'MAINICON' ) );
      hCursor       := LoadCursor( wnd_INST, IDC_ARROW );
      lpszMenuName  := nil;
      hbrBackGround := GetStockObject( NULL_BRUSH );
      lpszClassName := wnd_ClassName;
    end;

  if RegisterClassEx( wnd_Class ) = 0 Then
    begin
      u_Error( 'Cannot register window class' );
      exit;
    end;

  if app_FullScreen Then
    wnd_Style := WS_POPUP or WS_VISIBLE
  else
    wnd_Style := WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU or WS_VISIBLE;
  wnd_Handle := CreateWindowEx(
                                wnd_StyleEx * Byte( not app_FullScreen ),
                                wnd_ClassName,
                                PChar( wnd_Caption ),
                                wnd_Style,
                                wnd_X, wnd_Y,
                                wnd_Width  + ( wnd_BrdSizeX * 2 ) * Byte( not app_FullScreen ),
                                wnd_Height + ( wnd_BrdSizeY * 2 + wnd_CpnSize ) * Byte( not app_FullScreen ),
                                0,
                                0,
                                wnd_INST,
                                nil );

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
{$ENDIF}
{$IFDEF DARWIN}
  size.Left   := wnd_X;
  size.Top    := wnd_Y;
  size.Right  := wnd_X + wnd_Width;
  size.Bottom := wnd_Y + wnd_Height;
  wnd_Attr    := kWindowCloseBoxAttribute or kWindowCollapseBoxAttribute or kWindowStandardHandlerAttribute;// or kWindowCompositingAttribute;
  status      := CreateNewWindow( kDocumentWindowClass, wnd_Attr, size, wnd_Handle );

  if ( status <> noErr ) or ( wnd_Handle = nil ) Then
    begin
      u_Error( 'Cannot create window' );
      exit;
    end;

  // Window
  Events[ 0 ].eventClass := kEventClassWindow;
  Events[ 0 ].eventKind  := kEventWindowClosed;
  Events[ 1 ].eventClass := kEventClassWindow;
  Events[ 1 ].eventKind  := kEventWindowActivated;
  Events[ 2 ].eventClass := kEventClassWindow;
  Events[ 2 ].eventKind  := kEventWindowDeactivated;
  // Keyboard
  Events[ 3 ].eventClass := kEventClassKeyboard;
  Events[ 3 ].eventKind  := kEventRawKeyDown;
  Events[ 4 ].eventClass := kEventClassKeyboard;
  Events[ 4 ].eventKind  := kEventRawKeyUp;
  Events[ 5 ].eventClass := kEventClassKeyboard;
  Events[ 5 ].eventKind  := kEventRawKeyRepeat;
  // Mouse
  Events[ 6 ].eventClass := kEventClassMouse;
  Events[ 6 ].eventKind  := kEventMouseMoved;
  Events[ 7 ].eventClass := kEventClassMouse;
  Events[ 7 ].eventKind  := kEventMouseDown;
  Events[ 8 ].eventClass := kEventClassMouse;
  Events[ 8 ].eventKind  := kEventMouseUp;
  Events[ 9 ].eventClass := kEventClassMouse;
  Events[ 9 ].eventKind  := kEventMouseWheelMoved;
  InstallEventHandler( GetWindowEventTarget( wnd_Handle ), NewEventHandlerUPP( @zgl_mess ), 10, @Events[ 0 ], nil, nil );

  SelectWindow( wnd_Handle );
  ShowWindow( wnd_Handle );
{$ENDIF}
  wnd_SetCaption( wnd_Caption );

  Result := TRUE;
end;

procedure wnd_Destroy;
begin
{$IFDEF LINUX}
  XDestroyWindow( scr_Display, wnd_Handle );
  glXWaitX;
{$ENDIF}
{$IFDEF WIN32}
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

  if not UnRegisterClass( wnd_ClassName, wnd_INST ) Then
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
{$IFDEF WIN32}
  var
    r : TRect;
{$ENDIF}
begin
{$IFDEF LINUX}
  wnd_Destroy;
  wnd_Create( wnd_Width, wnd_Height );
  glXMakeCurrent( scr_Display, wnd_Handle, ogl_Context );
  glXWaitGL;
  wnd_ShowCursor( app_ShowCursor );
{$ENDIF}
{$IFDEF WIN32}
  // Странный костыль, но без него падает FPS после смены параметров окна
  wglMakeCurrent( wnd_DC, 0 );

  if app_FullScreen Then
    wnd_Style := WS_POPUP or WS_VISIBLE
  else
    wnd_Style := WS_EX_TOPMOST or WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU or WS_VISIBLE;

  if app_FullScreen Then
    begin
      ogl_X := 0;
      ogl_Y := 0;
      wnd_X := 0;
      wnd_Y := 0;
      SetRect( r, 0, 0, wnd_Width, wnd_Height )
    end else
      begin
        ogl_X := GetSystemMetrics( SM_CXSIZEFRAME );
        ogl_Y := GetSystemMetrics( SM_CYCAPTION ) + GetSystemMetrics( SM_CYSIZEFRAME );
        SetRect( r, 0, 0,
                 wnd_Width  + ogl_X,
                 wnd_Height + ogl_Y );
      end;

  AdjustWindowRectEx( r, 0, FALSE, 0 );
  SetWindowLong( wnd_Handle, GWL_STYLE, wnd_Style );
  SetWindowLong( wnd_Handle, GWL_EXSTYLE, wnd_StyleEx );

  wglMakeCurrent( wnd_DC, ogl_Context );
{$ENDIF}
{$IFDEF DARWIN}
  aglSetCurrentContext( ogl_Context );
{$ENDIF}
  app_Work := TRUE;
  wnd_SetSize( wnd_Width, wnd_Height );
end;

procedure wnd_SetCaption;
begin
  wnd_Caption := NewCaption;
  {$IFDEF LINUX}
  XStringListToTextProperty( @wnd_Caption, 1, @wnd_Title );
  if wnd_Handle <> 0 Then
    XSetWMName( scr_Display, wnd_Handle, @wnd_Title );
  {$ENDIF}
  {$IFDEF WIN32}
  if wnd_Handle <> 0 Then
    SetWindowText( wnd_Handle, PChar( wnd_Caption ) );
  {$ENDIF}
  {$IFDEF DARWIN}
  SetWTitle( wnd_Handle, wnd_Caption );
  SelectWindow( wnd_Handle );
  ShowWindow( wnd_Handle );
  {$ENDIF}
end;

procedure wnd_SetSize;
  {$IFDEF WIN32}
  var
    r : TRect;
  {$ENDIF}
begin
  wnd_Width  := Width;
  wnd_Height := Height;
{$IFDEF LINUX}
  XResizewindow( scr_Display, wnd_Handle, Width, Height );
  wnd_SetPos( wnd_X, wnd_Y );
{$ENDIF}
{$IFDEF WIN32}
  if not app_InitToHandle Then
    SetWindowPos( wnd_Handle, 0, wnd_X, wnd_Y, wnd_Width + ogl_X, wnd_Height + ogl_Y, SWP_NOZORDER or SWP_SHOWWINDOW );
{$ENDIF}
{$IFDEF DARWIN}
  SizeWindow( wnd_Handle, wnd_Width, wnd_Height, TRUE );
{$ENDIF}
  ogl_Width  := Width;
  ogl_Height := Height;
  glViewport( 0, 0, Width, Height );
  SetCurrentMode;
end;

procedure wnd_SetPos;
  {$IFDEF WIN32}
  var
    Rect : TRect;
  {$ENDIF}
begin
  wnd_X := X;
  wnd_Y := Y;
{$IFDEF LINUX}
  if wnd_Handle <> 0 Then
    if not app_FullScreen Then
      XMoveWindow( scr_Display, wnd_Handle, X, Y )
    else
      XMoveWindow( scr_Display, wnd_Handle, 0, 0 );
{$ENDIF}
{$IFDEF WIN32}
  if wnd_Handle <> 0 Then
    if not app_FullScreen Then
      begin
        GetWindowRect( wnd_Handle, Rect );
        SetWindowPos( wnd_Handle, 0, X, Y, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, SWP_NOZORDER or SWP_SHOWWINDOW );
      end else
        begin
          GetWindowRect( wnd_Handle, Rect );
          SetWindowPos( wnd_Handle, 0, 0, 0, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, SWP_NOZORDER or SWP_SHOWWINDOW );
        end;
{$ENDIF}
{$IFDEF DARWIN}
  if Assigned( wnd_Handle ) Then
    if not app_FullScreen Then
      MoveWindow( wnd_Handle, wnd_X, wnd_Y, TRUE )
    else
      MoveWindow( wnd_Handle, 0, 0, TRUE );
{$ENDIF}
end;

procedure wnd_SetOnTop;
begin
{$IFDEF WIN32}
  if ( OnTop ) and ( wnd_StyleEx and WS_EX_TOPMOST = 0 ) Then
    wnd_StyleEx := wnd_StyleEx or WS_EX_TOPMOST
  else
    if ( not OnTop ) and ( wnd_StyleEx and WS_EX_TOPMOST > 0 ) Then
      wnd_StyleEx := wnd_StyleEx xor WS_EX_TOPMOST;
{$ENDIF}
  wnd_Update;
end;

procedure wnd_ShowCursor;
{$IFDEF LINUX}
  var
    mask   : TPixmap;
    xcolor : TXColor;
begin
  if wnd_Handle = 0 Then exit;

  app_ShowCursor := Show;
  case Show of
    TRUE:
      if app_Cursor <> None Then
        begin
          XFreeCursor( scr_Display, app_Cursor );
          app_Cursor := None;
          XDefineCursor( scr_Display, wnd_Handle, app_Cursor );
        end;
    FALSE:
      begin
        mask := XCreatePixmap( scr_Display, wnd_Handle, 1, 1, 1 );
        FillChar( xcolor, SizeOf( xcolor ), 0 );
        app_Cursor := XCreatePixmapCursor( scr_Display, mask, mask, @xcolor, @xcolor, 0, 0 );
        XDefineCursor( scr_Display, wnd_Handle, app_Cursor );
      end;
   end;
{$ENDIF}
{$IFDEF WIN32}
begin
  if ( app_ShowCursor ) and ( not Show ) Then
    ShowCursor( FALSE )
  else
    ShowCursor( TRUE );
  app_ShowCursor := Show;
{$ENDIF}
{$IFDEF DARWIN}
begin
  app_ShowCursor := Show;
  if app_ShowCursor Then
    ShowCursor
  else
    HideCursor;
{$ENDIF}
end;

end.
