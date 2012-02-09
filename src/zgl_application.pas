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
unit zgl_application;

{$I zgl_config.cfg}
{$IFDEF iOS}
  {$modeswitch objectivec1}
{$ENDIF}

interface
uses
  {$IFDEF USE_X11}
  X, XLib
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows, Messages
  {$ENDIF}
  {$IFDEF MACOSX}
  MacOSAll
  {$ENDIF}
  {$IFDEF iOS}
  iPhoneAll, CFRunLoop, CGGeometry, CFBase, CFString
  {$ENDIF}
  {$IFDEF ANDROID}
  jni
  {$ENDIF}
  ;

procedure app_Init;
procedure app_MainLoop;
procedure app_ProcessOS;
{$IFDEF LINUX}
function app_ProcessMessages : LongWord;
{$ENDIF}
{$IFDEF WINDOWS}
function app_ProcessMessages( hWnd : HWND; Msg : UINT; wParam : WPARAM; lParam : LPARAM ) : LRESULT; stdcall;
{$ENDIF}
{$IFDEF MACOSX}
function app_ProcessMessages( inHandlerCallRef: EventHandlerCallRef; inEvent: EventRef; inUserData: UnivPtr ): OSStatus; cdecl;
{$ENDIF}
{$IFDEF iOS}
procedure app_InitPool;
procedure app_FreePool;

type
  zglCAppDelegate = objcclass(NSObject)
    procedure EnterMainLoop; message 'EnterMainLoop';
    procedure MainLoop; message 'MainLoop';
    procedure applicationDidFinishLaunching( application: UIApplication ); message 'applicationDidFinishLaunching:';
    procedure applicationWillResignActive( application: UIApplication ); message 'applicationWillResignActive:';
    procedure applicationDidEnterBackground( application: UIApplication ); message 'applicationDidEnterBackground:';
    procedure applicationWillTerminate( application: UIApplication ); message 'applicationWillTerminate:';
    procedure applicationWillEnterForeground( application: UIApplication ); message 'applicationWillEnterForeground:';
    procedure applicationDidBecomeActive( application: UIApplication ); message 'applicationDidBecomeActive:';
    procedure applicationDidReceiveMemoryWarning( application: UIApplication ); message 'applicationDidReceiveMemoryWarning:';

    // TextField
    function textFieldShouldBeginEditing( textField : UITextField ) : Boolean; message 'textFieldShouldBeginEditing:';
    function textField_shouldChangeCharactersInRange_replacementString( textField : UITextField; range : NSRange; string_ : NSString ) : Boolean; message 'textField:shouldChangeCharactersInRange:replacementString:';
    function textFieldShouldReturn( textField : UITextField ) : Boolean; message 'textFieldShouldReturn:';
    function textFieldShouldEndEditing( textField : UITextField ) : Boolean; message 'textFieldShouldEndEditing:';
    procedure textFieldEditingChanged; message 'textFieldEditingChanged';
  end;

type
  zglCiOSViewController = objcclass(UIViewController)
  public
    function shouldAutorotateToInterfaceOrientation( interfaceOrientation : UIInterfaceOrientation ) : Boolean; override;
    procedure didRotateFromInterfaceOrientation( fromInterfaceOrientation : UIInterfaceOrientation ); override;
  end;

type
  zglCiOSWindow = objcclass(UIWindow)
  protected
    procedure SetTouchPos_id( touch : UITouch; id : Byte ); message 'SetTouchPos:id:';
    function  GetTouchID( touch : UITouch ) : Byte; message 'GetTouchID:';
    procedure UpdateTouches( touches : NSSet ); message 'UpdateTouches:';
  public
    procedure touchesBegan_withEvent( touches : NSSet; event : UIevent ); override;
    procedure touchesMoved_withEvent( touches : NSSet; event : UIevent ); override;
    procedure touchesEnded_withEvent( touches : NSSet; event : UIevent ); override;
    procedure touchesCancelled_withEvent( touches : NSSet; event : UIevent ); override;
  end;
{$ENDIF}
{$IFDEF ANDROID}
procedure Java_zengl_android_ZenGL_zglNativeSurfaceCreated( var env : JNIEnv; var thiz : jobject; path : jstring ); cdecl;
procedure Java_zengl_android_ZenGL_zglNativeSurfaceChanged( var env : JNIEnv; var thiz : jobject; Width, Height : jint ); cdecl;
procedure Java_zengl_android_ZenGL_zglNativeDrawFrame( var env : JNIEnv; var thiz : jobject ); cdecl;
procedure Java_zengl_android_ZenGL_zglNativeActivate( var env : JNIEnv; var thiz : jobject; Activate : jboolean ); cdecl;
procedure Java_zengl_android_ZenGL_zglNativeTouch( var env : JNIEnv; var thiz : jobject; ID : jint; X, Y, Pressure : jfloat ); cdecl;
{$ENDIF}

procedure app_ZeroProc;
procedure app_ZeroUpdate( dt : Double );
procedure app_ZeroActivate( activate : Boolean );
function  app_ZeroCloseQuery : Boolean;
{$IFDEF iOS}
procedure app_ZeroOrientation( orientation : UIInterfaceOrientation );
{$ENDIF}

var
  appInitialized    : Boolean;
  appGotSysDirs     : Boolean;
  appWork           : Boolean;
  appWorkTime       : LongWord;
  appPause          : Boolean;
  appAutoPause      : Boolean = TRUE;
  appFocus          : Boolean = TRUE;
  appLog            : Boolean;
  appInitedToHandle : Boolean;
  appWorkDir        : UTF8String;
  appHomeDir        : UTF8String;

  // call-back
  app_PInit       : procedure;
  app_PLoop       : procedure;
  app_PLoad       : procedure;
  app_PDraw       : procedure;
  app_PExit       : procedure;
  app_PUpdate     : procedure( dt : Double );
  app_PActivate   : procedure( activate : Boolean );
  app_PCloseQuery : function : Boolean;
  {$IFDEF iOS}
  app_PMemoryWarn  : procedure;
  app_POrientation : procedure( orientation : UIInterfaceOrientation );
  {$ENDIF}

  {$IFDEF USE_X11}
  appCursor : TCursor = None;
  appXIM    : PXIM;
  appXIC    : PXIC;
  {$ENDIF}
  {$IFDEF WINDOWS}
  appTimer     : LongWord;
  appMinimized : Boolean;
  {$ENDIF}
  {$IFDEF iOS}
  appPool            : NSAutoreleasePool;
  appPoolInitialized : Boolean;
  appDelegate        : zglCAppDelegate;
  {$ENDIF}
  appShowCursor : Boolean;

  appdt : Double;

  appFPS      : LongWord;
  appFPSCount : LongWord;
  appFPSAll   : LongWord;

  appFlags : LongWord;

implementation
uses
  zgl_main,
  zgl_screen,
  zgl_window,
  {$IFNDEF USE_GLES}
  zgl_opengl,
  {$ELSE}
  zgl_opengles,
  {$ENDIF}
  zgl_render,
  {$IF DEFINED(iOS) or DEFINED(ANDROID)}
  zgl_touch,
  {$IFEND}
  zgl_mouse,
  zgl_keyboard,
  {$IFDEF USE_JOYSTICK}
  zgl_joystick,
  {$ENDIF}
  zgl_timers,
  zgl_resources,
  zgl_font,
  {$IFDEF USE_SOUND}
  zgl_sound,
  {$ENDIF}
  zgl_utils;

procedure app_ZeroProc; begin end;
procedure app_ZeroUpdate( dt : Double ); begin end;
procedure app_ZeroActivate( activate : Boolean ); begin end;
function  app_ZeroCloseQuery : Boolean; begin Result := TRUE; end;
{$IFDEF iOS}
procedure app_ZeroOrientation( orientation : UIInterfaceOrientation ); begin end;
{$ENDIF}

procedure app_Draw;
begin
  SetCurrentMode();
  scr_Clear();
  app_PDraw();
  scr_Flush();
  if not appPause Then
    INC( appFPSCount );
end;

procedure app_CalcFPS;
begin
  appFPS      := appFPSCount;
  appFPSAll   := appFPSAll + appFPSCount;
  appFPSCount := 0;
  INC( appWorkTime );
end;

procedure app_Init;
begin
  SetCurrentMode();
  scr_Clear();
  app_PLoad();
  scr_Flush();

  res_Init();

  appdt := timer_GetTicks();
  timer_Reset();
  timer_Add( @app_CalcFPS, 1000 );
end;

procedure app_MainLoop;
  var
    t : Double;
begin
  while appWork do
    begin
      app_ProcessOS();
      res_Proc();
      {$IFDEF USE_JOYSTICK}
      joy_Proc();
      {$ENDIF}
      {$IFDEF USE_SOUND}
      snd_MainLoop();
      {$ENDIF}

      if appPause Then
        begin
          timer_Reset();
          appdt := timer_GetTicks();
          u_Sleep( 10 );
          continue;
        end else
          timer_MainLoop();

      t := timer_GetTicks();
      {$IFDEF WINDESKTOP}
      // Workaround for bug with unstable time between frames...
      if ( scrVSync ) and ( appFPS > 0 ) and ( appFPS = scrRefresh ) and ( appFlags and APP_USE_DT_CORRECTION > 0 ) Then
        app_PUpdate( 1000 / appFPS )
      else
      {$ENDIF}
      app_PUpdate( timer_GetTicks() - appdt );
      appdt := t;

      app_Draw();
    end;
end;

procedure app_ProcessOS;
  {$IFDEF USE_X11}
  var
    root_return   : TWindow;
    child_return  : TWindow;
    root_x_return : Integer;
    root_y_return : Integer;
    mask_return   : LongWord;
  {$ENDIF}
  {$IFDEF WINDOWS}
  var
    m         : tagMsg;
    cursorpos : TPoint;
  {$ENDIF}
  {$IFDEF MACOSX}
  var
    event : EventRecord;
    mPos  : Point;
  {$ENDIF}
begin
{$IFDEF USE_X11}
  XQueryPointer( scrDisplay, wndHandle, @root_return, @child_return, @root_x_return, @root_y_return, @mouseX, @mouseY, @mask_return );

  mouseDX := Round( ( mouseX - wndWidth div 2 ) / scrResCX );
  mouseDY := Round( ( mouseY - wndHeight div 2 ) / scrResCY );
  mouseX  := Round( ( mouseX - scrAddCX ) / scrResCX );
  mouseY  := Round( ( mouseY - scrAddCY ) / scrResCY );
  if ( mouseLX <> mouseX ) or ( mouseLY <> mouseY ) Then
    begin
      mouseLX := mouseX;
      mouseLY := mouseY;

      if Assigned( mouse_PMove ) Then
        mouse_PMove( mouseX, mouseY );
    end;

  app_ProcessMessages();
  keysRepeat := 0;
{$ENDIF}
{$IFDEF WINDOWS}
  GetCursorPos( cursorpos );
  if wndFullScreen Then
    begin
      mouseX := cursorpos.X;
      mouseY := cursorpos.Y;
    end else
      begin
        mouseX := cursorpos.X - wndX - wndBrdSizeX;
        mouseY := cursorpos.Y - wndY - wndBrdSizeY - wndCpnSize;
      end;

  mouseDX := Round( ( mouseX - wndWidth div 2 ) / scrResCX );
  mouseDY := Round( ( mouseY - wndHeight div 2 ) / scrResCY );
  mouseX  := Round( ( mouseX - scrAddCX ) / scrResCX );
  mouseY  := Round( ( mouseY - scrAddCY ) / scrResCY );
  if ( mouseLX <> mouseX ) or ( mouseLY <> mouseY ) Then
    begin
      mouseLX := mouseX;
      mouseLY := mouseY;

      if Assigned( mouse_PMove ) Then
        mouse_PMove( mouseX, mouseY );
    end;

  while PeekMessageW( m, 0{wnd_Handle}, 0, 0, PM_REMOVE ) do
    begin
      TranslateMessage( m );
      DispatchMessageW( m );
    end;
{$ENDIF}
{$IFDEF MACOSX}
  GetGlobalMouse( mPos );
  mouseX := mPos.h - wndX;
  mouseY := mPos.v - wndY;

  mouseDX := Round( ( mouseX - wndWidth div 2 ) / scrResCX );
  mouseDY := Round( ( mouseY - wndHeight div 2 ) / scrResCY );
  mouseX  := Round( ( mouseX - scrAddCX ) / scrResCX );
  mouseY  := Round( ( mouseY - scrAddCY ) / scrResCY );
  if ( mouseLX <> mouseX ) or ( mouseLY <> mouseY ) Then
    begin
      mouseLX := mouseX;
      mouseLY := mouseY;

      if Assigned( mouse_PMove ) Then
        mouse_PMove( mouseX, mouseY );
    end;

  while GetNextEvent( everyEvent, event ) do;
{$ENDIF}
{$IFDEF iOS}
  while CFRunLoopRunInMode( kCFRunLoopDefaultMode, 0.01, TRUE ) = kCFRunLoopRunHandledSource do;
{$ENDIF}
end;

{$IFNDEF iOS}
function app_ProcessMessages;
  var
  {$IFDEF USE_X11}
    event  : TXEvent;
    keysym : TKeySym;
    status : TStatus;
  {$ENDIF}
  {$IFDEF MACOSX}
    eClass  : UInt32;
    eKind   : UInt32;
    command : HICommand;
    mButton : EventMouseButton;
    mWheel  : Integer;
    bounds  : HIRect;
    SCAKey  : LongWord;
  {$ENDIF}
    i   : Integer;
    len : Integer;
    c   : array[ 0..5 ] of AnsiChar;
    str : UTF8String;
    key : LongWord;
begin
  Result := 0;
{$IFDEF USE_X11}
  while XPending( scrDisplay ) <> 0 do
    begin
      XNextEvent( scrDisplay, @event );

      if appWork Then
      case event._type of
        ClientMessage:
          if ( event.xclient.message_type = wndProtocols ) and ( event.xclient.data.l[ 0 ] = wndDestroyAtom ) Then appWork := not app_PCloseQuery();

        Expose:
          if appWork and appAutoPause Then
            app_Draw();
        FocusIn:
          begin
            appFocus := TRUE;
            appPause := FALSE;
            if appWork Then app_PActivate( TRUE );
            FillChar( keysDown[ 0 ], 256, 0 );
            key_ClearState();
            FillChar( mouseDown[ 0 ], 3, 0 );
            mouse_ClearState();
          end;
        FocusOut:
          begin
            appFocus := FALSE;
            if appAutoPause Then appPause := TRUE;
            if appWork Then app_PActivate( FALSE );
          end;
        ConfigureNotify:
          begin
            // Для особо одаренных оконных менеджеров :)
            if wndFullScreen and ( ( event.xconfigure.x <> 0 ) or ( event.xconfigure.y <> 0 ) ) Then
              wnd_SetPos( 0, 0 );
            if ( event.xconfigure.width <> wndWidth ) or ( event.xconfigure.height <> wndHeight ) Then
              wnd_SetSize( wndWidth, wndHeight );
          end;

        ButtonPress:
          begin
            case event.xbutton.button of
              1: // Left
                begin
                  mouseDown[ M_BLEFT ] := TRUE;
                  if mouseCanClick[ M_BLEFT ] Then
                    begin
                      mouseClick[ M_BLEFT ] := TRUE;
                      mouseCanClick[ M_BLEFT ] := FALSE;
                      if timer_GetTicks - mouseDblCTime[ M_BLEFT ] < mouseDblCInt Then
                        mouseDblClick[ M_BLEFT ] := TRUE;
                      mouseDblCTime[ M_BLEFT ] := timer_GetTicks;
                    end;

                  if Assigned( mouse_PPress ) Then
                    mouse_PPress( M_BLEFT );
                end;
              2: // Middle
                begin
                  mouseDown[ M_BMIDDLE ] := TRUE;
                  if mouseCanClick[ M_BMIDDLE ] Then
                    begin
                      mouseClick[ M_BMIDDLE ] := TRUE;
                      mouseCanClick[ M_BMIDDLE ] := FALSE;
                      if timer_GetTicks() - mouseDblCTime[ M_BMIDDLE ] < mouseDblCInt Then
                        mouseDblClick[ M_BMIDDLE ] := TRUE;
                      mouseDblCTime[ M_BMIDDLE ] := timer_GetTicks();
                    end;

                  if Assigned( mouse_PPress ) Then
                    mouse_PPress( M_BMIDDLE );
                end;
              3: // Right
                begin
                  mouseDown[ M_BRIGHT ] := TRUE;
                  if mouseCanClick[ M_BRIGHT ] Then
                    begin
                      mouseClick[ M_BRIGHT ] := TRUE;
                      mouseCanClick[ M_BRIGHT ] := FALSE;
                      if timer_GetTicks() - mouseDblCTime[ M_BRIGHT ] < mouseDblCInt Then
                        mouseDblClick[ M_BRIGHT ] := TRUE;
                      mouseDblCTime[ M_BRIGHT ] := timer_GetTicks();
                    end;

                  if Assigned( mouse_PPress ) Then
                    mouse_PPress( M_BRIGHT );
                end;
            end;
          end;
        ButtonRelease:
          begin
            case event.xbutton.button of
              1: // Left
                begin
                  mouseDown[ M_BLEFT ]     := FALSE;
                  mouseUp  [ M_BLEFT ]     := TRUE;
                  mouseCanClick[ M_BLEFT ] := TRUE;

                  if Assigned( mouse_PRelease ) Then
                    mouse_PRelease( M_BLEFT );
                end;
              2: // Middle
                begin
                  mouseDown[ M_BMIDDLE ]     := FALSE;
                  mouseUp  [ M_BMIDDLE ]     := TRUE;
                  mouseCanClick[ M_BMIDDLE ] := TRUE;

                  if Assigned( mouse_PRelease ) Then
                    mouse_PRelease( M_BMIDDLE );
                end;
              3: // Right
                begin
                  mouseDown[ M_BRIGHT ]     := FALSE;
                  mouseUp  [ M_BRIGHT ]     := TRUE;
                  mouseCanClick[ M_BRIGHT ] := TRUE;

                  if Assigned( mouse_PRelease ) Then
                    mouse_PRelease( M_BRIGHT );
                end;
              4: // Up Wheel
                begin
                  mouseWheel[ M_WUP ] := TRUE;

                  if Assigned( mouse_PWheel ) Then
                    mouse_PWheel( M_WUP );
                end;
              5: // Down Wheel
                begin
                  mouseWheel[ M_WDOWN ] := TRUE;

                  if Assigned( mouse_PWheel ) Then
                    mouse_PWheel( M_WDOWN );
                end;
            end;
          end;

        KeyPress:
          begin
            INC( keysRepeat );
            key := xkey_to_scancode( XLookupKeysym( @event.xkey, 0 ), event.xkey.keycode );
            keysDown[ key ]     := TRUE;
            keysUp  [ key ]     := FALSE;
            keysLast[ KA_DOWN ] := key;
            doKeyPress( key );

            key := SCA( key );
            keysDown[ key ] := TRUE;
            keysUp  [ key ] := FALSE;
            doKeyPress( key );

            if Assigned( key_PPress ) Then
              key_PPress( key );

            if keysCanText Then
            case key of
              K_SYSRQ, K_PAUSE,
              K_ESCAPE, K_ENTER, K_KP_ENTER,
              K_UP, K_DOWN, K_LEFT, K_RIGHT,
              K_INSERT, K_DELETE, K_HOME, K_END,
              K_PAGEUP, K_PAGEDOWN,
              K_CTRL_L, K_CTRL_R,
              K_ALT_L, K_ALT_R,
              K_SHIFT_L, K_SHIFT_R,
              K_SUPER_L, K_SUPER_R,
              K_APP_MENU,
              K_CAPSLOCK, K_NUMLOCK, K_SCROLL:;
              K_BACKSPACE: u_Backspace( keysText );
              K_TAB:       key_InputText( '  ' );
            else
              len := Xutf8LookupString( appXIC, @event, @c[ 0 ], 6, @keysym, @status );
              if len > 0 Then
                begin
                  SetLength( str, len );
                  Move( c[ 0 ], str[ 1 ], len );
                  key_InputText( str );
                end;
            end;
          end;
        KeyRelease:
          begin
            INC( keysRepeat );
            key := xkey_to_scancode( XLookupKeysym( @event.xkey, 0 ), event.xkey.keycode );
            keysDown[ key ]   := FALSE;
            keysUp  [ key ]   := TRUE;
            keysLast[ KA_UP ] := key;

            key := SCA( key );
            keysDown[ key ] := FALSE;
            keysUp  [ key ] := TRUE;

            if Assigned( key_PRelease ) Then
              key_PRelease( key );
          end;
      end
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  if ( not appWork ) and ( Msg <> WM_ACTIVATEAPP ) Then
    begin
      Result := DefWindowProcW( hWnd, Msg, wParam, lParam );
      exit;
    end;
  case Msg of
    WM_CLOSE, WM_DESTROY, WM_QUIT:
      appWork := not app_PCloseQuery();

    WM_PAINT:
      begin
        app_Draw();
        ValidateRect( wndHandle, nil );
      end;
    WM_DISPLAYCHANGE:
      begin
        if scrChanging Then
          begin
            scrChanging := FALSE;
            exit;
          end;
        scr_Init();
        scrWidth  := scrDesktop.dmPelsWidth;
        scrHeight := scrDesktop.dmPelsHeight;
        if not wndFullScreen Then
          wnd_Update();
      end;
    WM_ACTIVATEAPP:
      begin
        if appMinimized Then exit;
        if ( wParam > 0 ) and ( not appFocus ) Then
          begin
            appFocus := TRUE;
            appPause := FALSE;
            if appWork Then app_PActivate( TRUE );
            FillChar( keysDown[ 0 ], 256, 0 );
            key_ClearState();
            FillChar( mouseDown[ 0 ], 3, 0 );
            mouse_ClearState();
            if ( wndFullScreen ) and ( not wndFirst ) Then
              scr_SetOptions( scrWidth, scrHeight, scrRefresh, wndFullScreen, scrVSync );
          end else
            if ( wParam = 0 ) and ( appFocus ) Then
              begin
                appFocus := FALSE;
                if appAutoPause Then appPause := TRUE;
                if appWork Then
                  begin
                    app_PActivate( FALSE );
                    if ( wndFullScreen ) and ( not wndFirst ) Then
                      begin
                        scr_Reset();
                        wnd_Update();
                      end;
                  end;
              end;
      end;
    WM_SIZE:
      begin
        if wParam = SIZE_MINIMIZED Then
          begin
            SendMessage( wndHandle, WM_ACTIVATEAPP, 0, 0 );
            appMinimized := TRUE;
          end;
        if ( wParam = SIZE_MAXIMIZED ) or ( wParam = SIZE_RESTORED ) Then
          begin
            appMinimized := FALSE;
            SendMessage( wndHandle, WM_ACTIVATEAPP, 1, 0 );
          end;
      end;
    WM_NCHITTEST:
      begin
        Result := DefWindowProcW( hWnd, Msg, wParam, lParam );
        if ( not appFocus ) and ( Result = HTCAPTION ) Then
          Result := HTCLIENT;
      end;
    WM_ENTERSIZEMOVE:
      begin
        if not appAutoPause Then
          appTimer := SetTimer( wndHandle, 1, 1, nil );
      end;
    WM_EXITSIZEMOVE:
      begin
        if appTimer > 0 Then
          begin
            KillTimer( wndHandle, appTimer );
            appTimer := 0;
          end;
      end;
    WM_MOVING:
      begin
        wndX := PRect( lParam ).Left;
        wndY := PRect( lParam ).Top;
        if appAutoPause Then
          timer_Reset();
      end;
    WM_TIMER:
      begin
        timer_MainLoop();
        app_Draw();
      end;
    WM_SETCURSOR:
      begin
        if ( appFocus ) and ( LOWORD ( lparam ) = HTCLIENT ) and ( not appShowCursor ) Then
          SetCursor( 0 )
        else
          SetCursor( LoadCursor( 0, IDC_ARROW ) );
      end;

    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      begin
        mouseDown[ M_BLEFT ] := TRUE;
        if mouseCanClick[ M_BLEFT ] Then
          begin
            mouseClick[ M_BLEFT ]    := TRUE;
            mouseCanClick[ M_BLEFT ] := FALSE;
          end;
        if Msg = WM_LBUTTONDBLCLK Then
          mouseDblClick[ M_BLEFT ] := TRUE;

        if Assigned( mouse_PPress ) Then
          mouse_PPress( M_BLEFT );
      end;
    WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
      begin
        mouseDown[ M_BMIDDLE ] := TRUE;
        if mouseCanClick[ M_BMIDDLE ] Then
          begin
            mouseClick[ M_BMIDDLE ]    := TRUE;
            mouseCanClick[ M_BMIDDLE ] := FALSE;
          end;
        if Msg = WM_MBUTTONDBLCLK Then
          mouseDblClick[ M_BMIDDLE ] := TRUE;

        if Assigned( mouse_PPress ) Then
          mouse_PPress( M_BMIDDLE );
      end;
    WM_RBUTTONDOWN, WM_RBUTTONDBLCLK:
      begin
        mouseDown[ M_BRIGHT ] := TRUE;
        if mouseCanClick[ M_BRIGHT ] Then
          begin
            mouseClick[ M_BRIGHT ]    := TRUE;
            mouseCanClick[ M_BRIGHT ] := FALSE;
          end;
        if Msg = WM_RBUTTONDBLCLK Then
          mouseDblClick[ M_BRIGHT ] := TRUE;

        if Assigned( mouse_PPress ) Then
          mouse_PPress( M_BRIGHT );
      end;
    WM_LBUTTONUP:
      begin
        mouseDown[ M_BLEFT ]     := FALSE;
        mouseUp  [ M_BLEFT ]     := TRUE;
        mouseCanClick[ M_BLEFT ] := TRUE;

        if Assigned( mouse_PRelease ) Then
          mouse_PRelease( M_BLEFT );
      end;
    WM_MBUTTONUP:
      begin
        mouseDown[ M_BMIDDLE ]     := FALSE;
        mouseUp  [ M_BMIDDLE ]     := TRUE;
        mouseCanClick[ M_BMIDDLE ] := TRUE;

        if Assigned( mouse_PRelease ) Then
          mouse_PRelease( M_BMIDDLE );
      end;
    WM_RBUTTONUP:
      begin
        mouseDown[ M_BRIGHT ]     := FALSE;
        mouseUp  [ M_BRIGHT ]     := TRUE;
        mouseCanClick[ M_BRIGHT ] := TRUE;

        if Assigned( mouse_PRelease ) Then
          mouse_PRelease( M_BRIGHT );
      end;
    WM_MOUSEWHEEL:
      begin
        if wParam > 0 Then
          begin
            mouseWheel[ M_WUP   ] := TRUE;
            mouseWheel[ M_WDOWN ] := FALSE;

            if Assigned( mouse_PWheel ) Then
              mouse_PWheel( M_WUP );
          end else
            begin
              mouseWheel[ M_WUP   ] := FALSE;
              mouseWheel[ M_WDOWN ] := TRUE;

              if Assigned( mouse_PWheel ) Then
                mouse_PWheel( M_WDOWN );
            end;
      end;

    WM_KEYDOWN, WM_SYSKEYDOWN:
      begin
        key := winkey_to_scancode( wParam );
        keysDown[ key ]     := TRUE;
        keysUp  [ key ]     := FALSE;
        keysLast[ KA_DOWN ] := key;
        doKeyPress( key );

        key := SCA( key );
        keysDown[ key ] := TRUE;
        keysUp  [ key ] := FALSE;
        doKeyPress( key );

        if Assigned( key_PPress ) Then
          key_PPress( key );

        if ( Msg = WM_SYSKEYDOWN ) and ( key = K_F4 ) Then
          appWork := not app_PCloseQuery();
      end;
    WM_KEYUP, WM_SYSKEYUP:
      begin
        key := winkey_to_scancode( wParam );
        keysDown[ key ]   := FALSE;
        keysUp  [ key ]   := TRUE;
        keysLast[ KA_UP ] := key;

        key := SCA( key );
        keysDown[ key ] := FALSE;
        keysUp  [ key ] := TRUE;

        if Assigned( key_PRelease ) Then
          key_PRelease( key );
      end;
    WM_CHAR:
      begin
        if keysCanText Then
        case winkey_to_scancode( wParam ) of
          K_BACKSPACE: u_Backspace( keysText );
          K_TAB:       key_InputText( '  ' );
        else
          len := WideCharToMultiByte( CP_UTF8, 0, @wParam, 1, nil, 0, nil, nil );
          if len > 0 Then
            begin
              WideCharToMultiByte( CP_UTF8, 0, @wParam, 1, @c[ 0 ], 5, nil, nil );
              SetLength( str, len );
              Move( c[ 0 ], str[ 1 ], len );
              key_InputText( str );
            end;
        end;
      end;
  else
    Result := DefWindowProcW( hWnd, Msg, wParam, lParam );
  end;
{$ENDIF}
{$IFDEF MACOSX}
  eClass := GetEventClass( inEvent );
  eKind  := GetEventKind( inEvent );
  Result := CallNextEventHandler( inHandlerCallRef, inEvent );

  if appWork Then
  case eClass of
    kEventClassCommand:
      case eKind of
        kEventProcessCommand:
          begin
            GetEventParameter( inEvent, kEventParamDirectObject, kEventParamHICommand, nil, SizeOf( HICommand ), nil, @command );
            if command.commandID = kHICommandQuit Then
              appWork := not app_PCloseQuery();
          end;
      end;

    kEventClassWindow:
      case eKind of
        kEventWindowDrawContent:
          begin
            app_Draw();
          end;
        kEventWindowActivated:
          begin
            appFocus := TRUE;
            appPause := FALSE;
            app_PActivate( TRUE );
            FillChar( keysDown[ 0 ], 256, 0 );
            key_ClearState();
            FillChar( mouseDown[ 0 ], 3, 0 );
            mouse_ClearState();
            if wndFullScreen Then
              scr_SetOptions( scrWidth, scrHeight, scrRefresh, wndFullScreen, scrVSync );
          end;
        kEventWindowDeactivated:
          begin
            appFocus := FALSE;
            if appAutoPause Then appPause := TRUE;
            app_PActivate( FALSE );
            if wndFullScreen Then scr_Reset();
          end;
        kEventWindowCollapsed:
          begin
            appFocus := FALSE;
            appPause := TRUE;
          end;
        kEventWindowClosed:
          begin
            wndHandle := nil;
            appWork   := FALSE;
          end;
        kEventWindowBoundsChanged:
          begin
            if not wndFullScreen Then
              begin
                GetEventParameter( inEvent, kEventParamCurrentBounds, typeHIRect, nil, SizeOf( bounds ), nil, @bounds );
                wndX := Round( bounds.origin.x - ( bounds.size.width - wndWidth ) / 2 );
                wndY := Round( bounds.origin.y - ( bounds.size.height - wndHeight ) / 2 );
              end else
                begin
                  wndX := 0;
                  wndY := 0;
                end;
          end;
      end;

    kEventClassKeyboard:
      begin
        GetEventParameter( inEvent, kEventParamKeyCode, typeUInt32, nil, 4, nil, @Key );

        case eKind of
          kEventRawKeyModifiersChanged:
            begin
              GetEventParameter( inEvent, kEventParamKeyModifiers, typeUInt32, nil, 4, nil, @SCAKey );
              for i := 0 to 7 do
                if SCAKey and Modifier[ i ].bit > 0 Then
                  begin
                    if not keysDown[ Modifier[ i ].key ] Then
                      doKeyPress( Modifier[ i ].key );
                    keysDown[ Modifier[ i ].key ] := TRUE;
                    keysUp  [ Modifier[ i ].key ] := FALSE;
                    keysLast[ KA_DOWN ]           := Modifier[ i ].key;

                    key := SCA( Modifier[ i ].key );
                    if not keysDown[ key ] Then
                      doKeyPress( key );
                    keysDown[ key ] := TRUE;
                    keysUp  [ key ] := FALSE;
                  end else
                    begin
                      if keysDown[ Modifier[ i ].key ] Then
                        begin
                          keysUp[ Modifier[ i ].key ] := TRUE;
                          keysLast[ KA_UP ]           := Modifier[ i ].key;
                        end;
                      keysDown[ Modifier[ i ].key ] := FALSE;

                      key := SCA( Modifier[ i ].key );
                      if keysDown[ key ] Then
                        keysUp[ key ] := TRUE;
                      keysDown[ key ] := FALSE;
                    end;
            end;
          kEventRawKeyDown, kEventRawKeyRepeat:
            begin
              key := mackey_to_scancode( key );
              keysDown[ key ]     := TRUE;
              keysUp  [ key ]     := FALSE;
              keysLast[ KA_DOWN ] := key;
              if eKind <> kEventRawKeyRepeat Then
                doKeyPress( key );

              key := SCA( key );
              keysDown[ key ] := TRUE;
              keysUp  [ key ] := FALSE;
              if eKind <> kEventRawKeyRepeat Then
                doKeyPress( key );

              if Assigned( key_PPress ) Then
                key_PPress( key );

              if keysCanText Then
              case key of
                K_SYSRQ, K_PAUSE,
                K_ESCAPE, K_ENTER, K_KP_ENTER,
                K_UP, K_DOWN, K_LEFT, K_RIGHT,
                K_INSERT, K_DELETE, K_HOME, K_END,
                K_PAGEUP, K_PAGEDOWN,
                K_CTRL_L, K_CTRL_R,
                K_ALT_L, K_ALT_R,
                K_SHIFT_L, K_SHIFT_R,
                K_SUPER_L, K_SUPER_R,
                K_APP_MENU,
                K_CAPSLOCK, K_NUMLOCK, K_SCROLL:;
                K_BACKSPACE: u_Backspace( keysText );
                K_TAB:       key_InputText( '  ' );
              else
                GetEventParameter( inEvent, kEventParamKeyUnicodes, typeUTF8Text, nil, 6, @len, @c[ 0 ] );
                if len > 0 Then
                  begin
                    SetLength( str, len );
                    System.Move( c[ 0 ], str[ 1 ], len );
                    key_InputText( str );
                  end;
              end;
            end;
          kEventRawKeyUp:
            begin
              key := mackey_to_scancode( key );
              keysDown[ key ]   := FALSE;
              keysUp  [ key ]   := TRUE;
              keysLast[ KA_UP ] := key;

              key := SCA( key );
              keysDown[ key ] := FALSE;
              keysUp  [ key ] := TRUE;

              if Assigned( key_Prelease ) Then
                key_Prelease( key );
            end;
        end;
      end;

    kEventClassMouse:
      case eKind of
        kEventMouseMoved, kEventMouseDragged:
          begin
            wndMouseIn := ( mouseX > 0 ) and ( mouseX < wndWidth ) and ( mouseY > 0 ) and ( mouseY < wndHeight );
            if wndMouseIn Then
              begin
                if ( not appShowCursor ) and ( CGCursorIsVisible = 1 ) Then
                  CGDisplayHideCursor( scrDisplay );
                if ( appShowCursor ) and ( CGCursorIsVisible = 0 ) Then
                  CGDisplayShowCursor( scrDisplay );
              end else
              if CGCursorIsVisible = 0 Then
                CGDisplayShowCursor( scrDisplay );
          end;
        kEventMouseDown:
          begin
            GetEventParameter( inEvent, kEventParamMouseButton, typeMouseButton, nil, SizeOf( EventMouseButton ), nil, @mButton );

            // Magic Mouse !!! XD
            if keysDown[ K_SUPER ] and ( mButton = kEventMouseButtonPrimary ) Then
              mButton := kEventMouseButtonSecondary;

            case mButton of
              kEventMouseButtonPrimary: // Left
                begin
                  mouseDown[ M_BLEFT ] := TRUE;
                  if mouseCanClick[ M_BLEFT ] Then
                    begin
                      mouseClick[ M_BLEFT ] := TRUE;
                      mouseCanClick[ M_BLEFT ] := FALSE;
                      if timer_GetTicks() - mouseDblCTime[ M_BLEFT ] < mouseDblCInt Then
                        mouseDblClick[ M_BLEFT ] := TRUE;
                      mouseDblCTime[ M_BLEFT ] := timer_GetTicks();
                    end;

                  if Assigned( mouse_PPress ) Then
                    mouse_PPress( M_BLEFT );
                end;
              kEventMouseButtonTertiary: // Middle
                begin
                  mouseDown[ M_BMIDDLE ] := TRUE;
                  if mouseCanClick[ M_BMIDDLE ] Then
                    begin
                      mouseClick[ M_BMIDDLE ] := TRUE;
                      mouseCanClick[ M_BMIDDLE ] := FALSE;
                      if timer_GetTicks() - mouseDblCTime[ M_BMIDDLE ] < mouseDblCInt Then
                        mouseDblClick[ M_BMIDDLE ] := TRUE;
                      mouseDblCTime[ M_BMIDDLE ] := timer_GetTicks();
                    end;

                  if Assigned( mouse_PPress ) Then
                    mouse_PPress( M_BMIDDLE );
                end;
              kEventMouseButtonSecondary: // Right
                begin
                  mouseDown[ M_BRIGHT ] := TRUE;
                  if mouseCanClick[ M_BRIGHT ] Then
                    begin
                      mouseClick[ M_BRIGHT ] := TRUE;
                      mouseCanClick[ M_BRIGHT ] := FALSE;
                      if timer_GetTicks() - mouseDblCTime[ M_BRIGHT ] < mouseDblCInt Then
                        mouseDblClick[ M_BRIGHT ] := TRUE;
                      mouseDblCTime[ M_BRIGHT ] := timer_GetTicks();
                    end;

                  if Assigned( mouse_PPress ) Then
                    mouse_PPress( M_BRIGHT );
                end;
            end;
          end;
        kEventMouseUp:
          begin
            GetEventParameter( inEvent, kEventParamMouseButton, typeMouseButton, nil, SizeOf( EventMouseButton ), nil, @mButton );

            // Magic Mouse !!! XD
            if keysDown[ K_SUPER ] and ( mButton = kEventMouseButtonPrimary ) Then
              mButton := kEventMouseButtonSecondary;

            case mButton of
              kEventMouseButtonPrimary: // Left
                begin
                  mouseDown[ M_BLEFT ]     := FALSE;
                  mouseUp  [ M_BLEFT ]     := TRUE;
                  mouseCanClick[ M_BLEFT ] := TRUE;

                  if Assigned( mouse_PRelease ) Then
                    mouse_PRelease( M_BLEFT );
                end;
              kEventMouseButtonTertiary: // Middle
                begin
                  mouseDown[ M_BMIDDLE ]     := FALSE;
                  mouseUp  [ M_BMIDDLE ]     := TRUE;
                  mouseCanClick[ M_BMIDDLE ] := TRUE;

                  if Assigned( mouse_PRelease ) Then
                    mouse_PRelease( M_BMIDDLE );
                end;
              kEventMouseButtonSecondary: // Right
                begin
                  mouseDown[ M_BRIGHT ]     := FALSE;
                  mouseUp  [ M_BRIGHT ]     := TRUE;
                  mouseCanClick[ M_BRIGHT ] := TRUE;

                  if Assigned( mouse_PRelease ) Then
                    mouse_PRelease( M_BRIGHT );
                end;
            end;
          end;
        kEventMouseWheelMoved:
          begin
            GetEventParameter( inEvent, kEventParamMouseWheelDelta, typeSInt32, nil, 4, nil, @mWheel );

            if mWheel > 0 then
              begin
                mouseWheel[ M_WUP ] := TRUE;

                if Assigned( mouse_PWheel ) Then
                  mouse_PWheel( M_WUP );
              end else
                begin
                  mouseWheel[ M_WDOWN ] := TRUE;

                  if Assigned( mouse_PWheel ) Then
                    mouse_PWheel( M_WDOWN );
                end;
          end;
      end;
  end;
{$ENDIF}
end;
{$ELSE}
procedure app_InitPool;
begin
  if not Assigned( appPool ) Then
    appPool := NSAutoreleasePool.alloc.init();
end;

procedure app_FreePool;
begin
  if Assigned( appPool ) Then
    appPool.release();
end;

procedure zglCAppDelegate.EnterMainLoop;
begin
  zgl_Init( oglFSAA, oglStencil );
end;

procedure zglCAppDelegate.MainLoop;
  var
    t : Double;
begin
  res_Proc();
  {$IFDEF USE_JOYSTICK}
  joy_Proc();
  {$ENDIF}
  {$IFDEF USE_SOUND}
  snd_MainLoop();
  {$ENDIF}

  if appPause Then
    begin
      timer_Reset();
      appdt := timer_GetTicks();
      exit;
    end else
      timer_MainLoop();

  t := timer_GetTicks();
  app_PUpdate( timer_GetTicks() - appdt );
  appdt := t;

  app_Draw();
end;

procedure zglCAppDelegate.applicationDidFinishLaunching( application: UIApplication );
begin
  appDelegate := Self;

  scr_Init();
  performSelector_withObject_afterDelay( objcselector( 'EnterMainLoop' ), nil, 0.2{magic} );
end;

procedure zglCAppDelegate.applicationWillResignActive( application : UIApplication );
begin
  if appAutoPause Then appPause := TRUE;
  if appWork Then app_PActivate( FALSE );
end;

procedure zglCAppDelegate.applicationDidEnterBackground( application: UIApplication );
begin
//  appWork := FALSE;
end;

procedure zglCAppDelegate.applicationWillTerminate( application: UIApplication );
begin
//  appWork := FALSE;
end;

procedure zglCAppDelegate.applicationWillEnterForeground( application: UIApplication );
begin
end;

procedure zglCAppDelegate.applicationDidBecomeActive( application: UIApplication );
begin
  appPause := FALSE;
  if appWork Then app_PActivate( TRUE );
end;

procedure zglCAppDelegate.applicationDidReceiveMemoryWarning;
begin
  app_PMemoryWarn();
end;

function zglCAppDelegate.textFieldShouldBeginEditing( textField : UITextField ) : Boolean;
begin
  Result := keysCanText;
end;

function zglCAppDelegate.textField_shouldChangeCharactersInRange_replacementString( textField : UITextField; range : NSRange; string_ : NSString ) : Boolean;
  var
    buffer : array[ 0..3 ] of AnsiChar;
begin
  Result := TRUE;
  keysTextChanged := TRUE;

  FillChar( buffer, 4, 0 );
  CFStringGetCString( CFStringRef( string_ ), @buffer[ 0 ], 4, kCFStringEncodingUTF8 );

  if buffer[ 0 ] = #0 Then
    u_Backspace( keysText )
  else
    key_InputText( buffer );
end;

function zglCAppDelegate.textFieldShouldReturn( textField : UITextField ) : Boolean;
begin
  Result := TRUE;
  keysCanText := FALSE;
  keysTextField.resignFirstResponder();
  keysTextField.removeFromSuperview();
end;

function zglCAppDelegate.textFieldShouldEndEditing( textField : UITextField ) : Boolean;
begin
  Result := textFieldShouldReturn( textField );
end;

procedure zglCAppDelegate.textFieldEditingChanged;
  var
    i, len : Integer;
    buffer : PAnsiChar;
begin
  if not keysTextChanged Then
    begin
      len := CFStringGetLength( CFStringRef( keysTextField.text() ) ) * 2;
      zgl_GetMem( buffer, len );
      CFStringGetCString( CFStringRef( keysTextField.text() ), @buffer[ 0 ], len, kCFStringEncodingUTF8 );
      keysText := PAnsiChar( @buffer[ 0 ] );
      zgl_FreeMem( buffer );
    end else
      keysTextChanged := FALSE;
end;

function zglCiOSViewController.shouldAutorotateToInterfaceOrientation( interfaceOrientation : UIInterfaceOrientation ) : Boolean;
begin
  Result := FALSE;
  if scrCanPortrait Then
    begin
      if interfaceOrientation = UIInterfaceOrientationPortrait Then
        begin
          scrAngle := 0;
          Result   := TRUE;
        end;
      if interfaceOrientation = UIInterfaceOrientationPortraitUpsideDown Then
        begin
          scrAngle := 180;
          Result   := TRUE;
        end;
    end;
  if scrCanLandscape Then
    begin
      if interfaceOrientation = UIInterfaceOrientationLandscapeLeft Then
        begin
          scrAngle := 90;
          Result   := TRUE;
        end;
      if interfaceOrientation = UIInterfaceOrientationLandscapeRight Then
        begin
          scrAngle := 270;
          Result   := TRUE;
        end;
    end;
end;

procedure zglCiOSViewController.didRotateFromInterfaceOrientation( fromInterfaceOrientation : UIInterfaceOrientation );
begin
  scrOrientation := Self.interfaceOrientation;

  if scrCanPortrait and ( ( scrOrientation = UIInterfaceOrientationPortrait ) or ( scrOrientation = UIInterfaceOrientationPortraitUpsideDown ) ) Then
    begin
      wndPortrait := TRUE;
      scrDesktopW := scrCurrModeW;
      scrDesktopH := scrCurrModeH;
    end;

  if scrCanLandscape and ( ( scrOrientation = UIInterfaceOrientationLandscapeLeft ) or ( scrOrientation = UIInterfaceOrientationLandscapeRight ) ) Then
    begin
      wndPortrait := FALSE;
      scrDesktopW := scrCurrModeH;
      scrDesktopH := scrCurrModeW;
    end;

  scr_SetOptions( scrDesktopW, scrDesktopH, REFRESH_MAXIMUM, TRUE, TRUE );

  if appWork Then
    app_POrientation( scrOrientation );
end;

procedure zglCiOSWindow.SetTouchPos_id( touch : UITouch; id : Byte );
  var
    point : CGPoint;
begin
  point   := touch.locationInView( Window );
  point.x := point.x * eglView.contentScaleFactor();
  point.y := point.y * eglView.contentScaleFactor();

  case scrAngle of
    0:
      begin
        touchX[ id ] := Round( point.x );
        touchY[ id ] := Round( point.y );
      end;
    180:
      begin
        touchX[ id ] := Round( wndWidth - point.x );
        touchY[ id ] := Round( wndHeight - point.y );
      end;
    270:
      begin
        touchX[ id ] := Round( point.y );
        touchY[ id ] := Round( wndHeight - point.x );
      end;
    90:
      begin
        touchX[ id ] := Round( wndWidth - point.y );
        touchY[ id ] := Round( point.x );
      end;
    end;

  touchX[ id ] := Round( ( touchX[ id ] - scrAddCX ) / scrResCX );
  touchY[ id ] := Round( ( touchY[ id ] - scrAddCY ) / scrResCY );
  mouseX := touchX[ id ];
  mouseY := touchY[ id ];
end;

function zglCiOSWindow.GetTouchID( touch : UITouch ) : Byte;
  var
    i : Integer;
begin
  for i := 0 to touchCount - 1 do
    if touchList[ i ] = Pointer( touch ) Then
      begin
        Result := i;
        exit;
      end;

  for i := 0 to 255 do
    if not Assigned( touchList[ i ] ) Then
      begin
        INC( touchCount );
        Result := i;
        exit;
      end;
end;

procedure zglCiOSWindow.UpdateTouches( touches : NSSet );
  var
    i, j  : Integer;
    id    : Byte;
    touch : UITouch;
begin
  for i := 0 to touches.count - 1 do
    begin
      touch := UITouch( touches.allObjects().objectAtIndex( i ) );
      id    := GetTouchID( touch );
      SetTouchPos_id( touch, id );

      case touch.phase of
        UITouchPhaseBegan:
          begin
            touchList[ id ] := touch;
            touchDown[ id ] := TRUE;
            if touchCanTap[ id ] Then
              begin
                touchTap   [ id ] := TRUE;
                touchCanTap[ id ] := FALSE;
                if touch.tapCount = 2 Then
                  touchDblTap[ id ] := TRUE;
              end;

            if Assigned( touch_PPress ) Then
              touch_PPress( id );
          end;
        UITouchPhaseMoved:
          begin
            touchList[ id ] := touch;
            touchDown[ id ] := TRUE;
            if touchCanTap[ id ] Then
              begin
                touchTap   [ id ] := TRUE;
                touchCanTap[ id ] := FALSE;
                if touch.tapCount = 2 Then
                  touchDblTap[ id ] := TRUE;
              end;

            if Assigned( touch_PMove ) Then
              touch_PMove( id, touchX[ id ], touchY[ id ] );
          end;
        UITouchPhaseEnded, UITouchPhaseCancelled:
          begin
            touchList[ id ]   := nil;
            touchDown[ id ]   := FALSE;
            touchUp  [ id ]   := TRUE;
            touchCanTap[ id ] := FALSE;

            if Assigned( touch_PRelease ) Then
              touch_PRelease( id );
          end;
      end;
    end;
end;

procedure zglCiOSWindow.touchesBegan_withEvent( touches : NSSet; event : UIevent );
begin
  UpdateTouches( event.touchesForWindow( Self ) );

  // mouse emulation
  mouseDown[ M_BLEFT ] := TRUE;
  if mouseCanClick[ M_BLEFT ] Then
    begin
      mouseClick[ M_BLEFT ] := TRUE;
      mouseCanClick[ M_BLEFT ] := FALSE;
      if timer_GetTicks - mouseDblCTime[ M_BLEFT ] < mouseDblCInt Then
        mouseDblClick[ M_BLEFT ] := TRUE;
      mouseDblCTime[ M_BLEFT ] := timer_GetTicks;
    end;
end;

procedure zglCiOSWindow.touchesMoved_withEvent( touches : NSSet; event : UIevent );
begin
  UpdateTouches( event.touchesForWindow( Self ) );
end;

procedure zglCiOSWindow.touchesEnded_withEvent( touches : NSSet; event : UIevent );
  var
    i : Integer;
begin
  touchCount := 0;
  for i := 0 to 255 do
    begin
      if Assigned( touch_PRelease ) and Assigned( touchList[ i ] ) Then
        touch_PRelease( i );

      touchList[ i ]   := nil;
      touchDown[ i ]   := FALSE;
      touchUp  [ i ]   := TRUE;
      touchCanTap[ i ] := FALSE;
    end;

  // mouse emulation
  mouseDown[ M_BLEFT ]     := FALSE;
  mouseUp  [ M_BLEFT ]     := TRUE;
  mouseCanClick[ M_BLEFT ] := TRUE;
end;

procedure zglCiOSWindow.touchesCancelled_withEvent( touches : NSSet; event : UIevent );
begin
  touchesEnded_withEvent( touches, event );
end;
{$ENDIF}

{$IFDEF ANDROID}
procedure Java_zengl_android_ZenGL_zglNativeSurfaceCreated( var env : JNIEnv; var thiz : jobject; path : jstring );
  var
    isCopy : jboolean;
begin
  isCopy     := 0;
  appWorkDir := env^.GetStringUTFChars( @env, path, isCopy );
end;

procedure Java_zengl_android_ZenGL_zglNativeSurfaceChanged( var env : JNIEnv; var thiz : jobject; Width, Height : jint );
begin
  if not appInitialized Then
    begin
      scrDesktopW := Width;
      scrDesktopH := Height;
      wndWidth    := Width;
      wndHeight   := Height;

      zgl_Init();
    end else
      wnd_SetSize( Width, Height );
end;

procedure Java_zengl_android_ZenGL_zglNativeDrawFrame( var env : JNIEnv; var thiz : jobject );
  var
    t : Double;
begin
  res_Proc();
  {$IFDEF USE_JOYSTICK}
  joy_Proc();
  {$ENDIF}
  {$IFDEF USE_SOUND}
  snd_MainLoop();
  {$ENDIF}

  if appPause Then
    begin
      timer_Reset();
      appdt := timer_GetTicks();
      exit;
    end else
      timer_MainLoop();

  t := timer_GetTicks();
  app_PUpdate( timer_GetTicks() - appdt );
  appdt := t;

  app_Draw();
end;

procedure Java_zengl_android_ZenGL_zglNativeActivate( var env : JNIEnv; var thiz : jobject; Activate : jboolean );
begin
  if Activate > 0 Then
    begin
      appFocus := TRUE;
      appPause := FALSE;
      if appWork Then app_PActivate( TRUE );
      FillChar( keysDown[ 0 ], 256, 0 );
      key_ClearState();
      FillChar( mouseDown[ 0 ], 3, 0 );
      mouse_ClearState();
      touch_ClearState();
    end else
      begin
        appFocus := FALSE;
        appPause := TRUE;
        if appWork Then app_PActivate( FALSE );
      end;
end;

procedure Java_zengl_android_ZenGL_zglNativeTouch( var env : JNIEnv; var thiz : jobject; ID : jint; X, Y, Pressure : jfloat );
begin
  if appFlags and CORRECT_RESOLUTION > 0 Then
    begin
      mouseX  := Round( ( X - scrAddCX ) / scrResCX );
      mouseY  := Round( ( Y - scrAddCY ) / scrResCY );
    end else
      begin
        mouseX := Round( X );
        mouseY := Round( Y );
      end;

  touchX[ ID ] := mouseX;
  touchY[ ID ] := mouseY;
  if ( touchDown[ ID ] ) and ( Pressure > 0 ) Then exit;
  touchDown[ ID ] := Pressure > 0;
  if not touchDown[ ID ] Then
    touchUp[ ID ] := TRUE;

  mouseDown[ M_BLEFT ] := Pressure > 0;
  if mouseDown[ M_BLEFT ] Then
    begin
      if mouseCanClick[ M_BLEFT ] Then
        begin
          mouseClick[ M_BLEFT ] := TRUE;
          mouseCanClick[ M_BLEFT ] := FALSE;
          if timer_GetTicks - mouseDblCTime[ M_BLEFT ] < mouseDblCInt Then
            mouseDblClick[ M_BLEFT ] := TRUE;
          mouseDblCTime[ M_BLEFT ] := timer_GetTicks;
        end;
    end else
      begin
        mouseDown[ M_BLEFT ]     := FALSE;
        mouseUp  [ M_BLEFT ]     := TRUE;
        mouseCanClick[ M_BLEFT ] := TRUE;
      end;
end;
{$ENDIF}

initialization
  app_PInit       := app_Init;
  app_PLoop       := app_MainLoop;
  app_PLoad       := app_ZeroProc;
  app_PDraw       := app_ZeroProc;
  app_PExit       := app_ZeroProc;
  app_PUpdate     := app_ZeroUpdate;
  app_PActivate   := app_ZeroActivate;
  app_PCloseQuery := app_ZeroCloseQuery;
{$IFDEF iOS}
  app_PMemoryWarn  := app_ZeroProc;
  app_POrientation := app_ZeroOrientation;
{$ENDIF}

  appFlags := WND_USE_AUTOCENTER or APP_USE_LOG or COLOR_BUFFER_CLEAR or CLIP_INVISIBLE {$IFDEF WINDESKTOP} or APP_USE_DT_CORRECTION {$ENDIF};
{$IFDEF iOS}
  appFlags := appFlags or SCR_ORIENTATION_LANDSCAPE or SCR_ORIENTATION_PORTRAIT;
{$ENDIF}

end.
