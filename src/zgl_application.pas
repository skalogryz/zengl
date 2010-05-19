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
unit zgl_application;

{$I zgl_config.cfg}

interface
uses
  {$IFDEF LINUX}
  X, XLib,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  Messages,
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll,
  {$ENDIF}
  zgl_types;

procedure zero;
procedure zerou( dt : Double );
procedure zeroa( activate : Boolean );

procedure app_Init;
procedure app_MainLoop;
procedure app_ProcessOS;
{$IFDEF LINUX}
function app_ProcessMessages : LongWord;
{$ENDIF}
{$IFDEF WINDOWS}
function app_ProcessMessages( hWnd : HWND; Msg : UINT; wParam : WPARAM; lParam : LPARAM ) : LRESULT; stdcall;
{$ENDIF}
{$IFDEF DARWIN}
function app_ProcessMessages( inHandlerCallRef: EventHandlerCallRef; inEvent: EventRef; inUserData: UnivPtr ): OSStatus; cdecl;
{$ENDIF}
procedure app_CalcFPS;

var
  app_Initialized  : Boolean;
  app_GetSysDirs   : Boolean;
  app_Work         : Boolean;
  app_WorkTime     : LongWord;
  app_Pause        : Boolean;
  app_AutoPause    : Boolean = TRUE;
  app_Focus        : Boolean = TRUE;
  app_Log          : Boolean;
  app_InitToHandle : Boolean;
  app_WorkDir      : AnsiString;
  app_UsrHomeDir   : AnsiString;

  // call-back
  app_PInit     : procedure = app_Init;
  app_PLoop     : procedure = app_MainLoop;
  app_PLoad     : procedure = zero;
  app_PDraw     : procedure = zero;
  app_PExit     : procedure = zero;
  app_PUpdate   : procedure( dt : Double ) = zerou;
  app_PActivate : procedure( activate : Boolean ) = zeroa;

  {$IFDEF LINUX}
  app_Cursor : TCursor = None;
  app_XIM    : PXIM;
  app_XIC    : PXIC;
  {$ENDIF}
  app_ShowCursor : Boolean;

  app_dt : Double;

  app_FPS      : LongWord;
  app_FPSCount : LongWord;
  app_FPSAll   : LongWord;

  app_Flags : LongWord;

implementation
uses
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_opengl,
  zgl_opengl_all,
  zgl_opengl_simple,
  zgl_log,
  zgl_mouse,
  zgl_keyboard,
  {$IFDEF USE_JOYSTICK}
  zgl_joystick,
  {$ENDIF}
  zgl_timers,
  zgl_font,
  {$IFDEF USE_SOUND}
  zgl_sound,
  {$ENDIF}
  zgl_utils;

procedure zero;  begin end;
procedure zerou; begin end;
procedure zeroa; begin end;

procedure app_Draw;
begin
  SetCurrentMode;
  scr_Clear;
  app_PDraw;
  scr_Flush;
  if not app_Pause Then
    INC( app_FPSCount );
end;

procedure app_CalcFPS;
begin
  app_FPS      := app_FPSCount;
  app_FPSAll   := app_FPSAll + app_FPSCount;
  app_FPSCount := 0;
  INC( app_WorkTime );
end;

procedure app_Init;
  {$IFDEF WINDOWS}
  var
    sysInfo : _SYSTEM_INFO;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Багнутое MS-поделко требует патча :)
  // Вешаем все на одно ядро
  GetSystemInfo( sysInfo );
  SetProcessAffinityMask( GetCurrentProcess(), sysInfo.dwActiveProcessorMask );
  {$ENDIF}

  scr_Clear();
  app_PLoad();
  scr_Flush();

  app_dt := timer_GetTicks();
  timer_Reset();
  timer_Add( @app_CalcFPS, 1000 );
end;

procedure app_MainLoop;
  var
    t : Double;
begin
  while app_Work do
    begin
      app_ProcessOS();
      {$IFDEF USE_JOYSTICK}
      joy_Proc();
      {$ENDIF}
      {$IFDEF USE_SOUND}
      snd_MainLoop();
      {$ENDIF}

      if app_Pause Then
        begin
          timer_Reset();
          app_dt := timer_GetTicks();
          u_Sleep( 10 );
          continue;
        end else
          timer_MainLoop();

      t := timer_GetTicks();
      app_PUpdate( timer_GetTicks() - app_dt );
      app_dt := t;

      app_Draw();
    end;
end;

procedure app_ProcessOS;
  {$IFDEF WINDOWS}
  var
    m : tagMsg;
  {$ENDIF}
  {$IFDEF DARWIN}
  var
    event : EventRecord;
  {$ENDIF}
begin
{$IFDEF LINUX}
  app_ProcessMessages();
  keysRepeat := 0;
{$ENDIF}
{$IFDEF WINDOWS}
  while PeekMessageW( m, 0{wnd_Handle}, 0, 0, PM_REMOVE ) do
    begin
      TranslateMessage( m );
      DispatchMessageW( m );
    end;
{$ENDIF}
{$IFDEF DARWIN}
  while GetNextEvent( everyEvent, event ) do;
{$ENDIF}
end;

function app_ProcessMessages;
  var
  {$IFDEF LINUX}
    event  : TXEvent;
    keysym : TKeySym;
    status : TStatus;
  {$ENDIF}
  {$IFDEF DARWIN}
    eClass  : UInt32;
    eKind   : UInt32;
    command : HICommand;
    mPos    : HIPoint;
    mButton : EventMouseButton;
    mWheel  : Integer;
    bounds  : HIRect;
    where   : Point;
    SCAKey  : LongWord;
  {$ENDIF}
    i   : Integer;
    len : Integer;
    c   : array[ 0..5 ] of AnsiChar;
    str : AnsiString;
    key : LongWord;
begin
{$IFDEF LINUX}
  Result := 0;
  while XPending( scr_Display ) <> 0 do
    begin
      XNextEvent( scr_Display, @event );

      case event._type of
        ClientMessage:
          if ( event.xclient.message_type = wnd_Protocols ) and ( event.xclient.data.l[ 0 ] = wnd_DestroyAtom ) Then app_Work := FALSE;

        Expose:
          if app_Work Then
            app_Draw();
        FocusIn:
          begin
            app_Focus := TRUE;
            app_Pause := FALSE;
            app_PActivate( TRUE );
            FillChar( keysDown[ 0 ], 256, 0 );
            key_ClearState();
            FillChar( mouseDown[ 0 ], 3, 0 );
            mouse_ClearState();
          end;
        FocusOut:
          begin
            app_Focus := FALSE;
            if app_AutoPause Then app_Pause := TRUE;
            app_PActivate( FALSE );
          end;
        ConfigureNotify:
          begin
            // Для особо одаренных оконных менеджеров :)
            if wnd_FullScreen and ( ( event.xconfigure.x <> 0 ) or ( event.xconfigure.y <> 0 ) ) Then
              wnd_SetPos( 0, 0 );
            if ( event.xconfigure.width <> wnd_Width ) or ( event.xconfigure.height <> wnd_Height ) Then
              wnd_SetSize( wnd_Width, wnd_Height );
          end;

        MotionNotify:
          begin
            if not mouseLock Then
              begin
                mouseX := event.xmotion.X;
                mouseY := event.xmotion.Y;
              end else
                begin
                  mouseX := event.xmotion.X - wnd_Width  div 2;
                  mouseY := event.xmotion.Y - wnd_Height div 2;
                end;
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
                end;
              2: // Midle
                begin
                  mouseDown[ M_BMIDLE ] := TRUE;
                  if mouseCanClick[ M_BMIDLE ] Then
                    begin
                      mouseClick[ M_BMIDLE ] := TRUE;
                      mouseCanClick[ M_BMIDLE ] := FALSE;
                      if timer_GetTicks() - mouseDblCTime[ M_BMIDLE ] < mouseDblCInt Then
                        mouseDblClick[ M_BMIDLE ] := TRUE;
                      mouseDblCTime[ M_BMIDLE ] := timer_GetTicks();
                    end;
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
                end;
              2: // Midle
                begin
                  mouseDown[ M_BMIDLE ]     := FALSE;
                  mouseUp  [ M_BMIDLE ]     := TRUE;
                  mouseCanClick[ M_BMIDLE ] := TRUE;
                end;
              3: // Right
                begin
                  mouseDown[ M_BRIGHT ]     := FALSE;
                  mouseUp  [ M_BRIGHT ]     := TRUE;
                  mouseCanClick[ M_BRIGHT ] := TRUE;
                end;
              4: // Up Wheel
                begin
                  mouseWheel[ M_WUP ] := TRUE;
                end;
              5: // Down Wheel
                begin
                  mouseWheel[ M_WDOWN ] := TRUE;
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
              len := Xutf8LookupString( app_XIC, @event, @c[ 0 ], 6, @keysym, @status );
              str := '';
              for i := 0 to len - 1 do
                str := str + c[ i ];
              if str <> '' Then
                key_InputText( str );
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
          end;
      end
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  Result := 0;
  if ( not app_Work ) and ( Msg <> WM_ACTIVATE ) Then
    begin
      Result := DefWindowProcW( hWnd, Msg, wParam, lParam );
      exit;
    end;
  case Msg of
    WM_CLOSE, WM_DESTROY, WM_QUIT:
      app_Work := FALSE;

    WM_PAINT:
      begin
        app_Draw();
        ValidateRect( wnd_Handle, nil );
      end;
    WM_DISPLAYCHANGE:
      begin
        if scr_Changing Then
          begin
            scr_Changing := FALSE;
            exit;
          end;
        scr_Init();
        if not wnd_FullScreen Then
          begin
            scr_Width  := scr_Desktop.dmPelsWidth;
            scr_Height := scr_Desktop.dmPelsHeight;
            wnd_Update();
          end else
            begin
              scr_Width  := wnd_Width;
              scr_Height := wnd_Height;
            end;
      end;
    WM_ACTIVATE:
      begin
        app_Focus := ( LOWORD( wParam ) <> WA_INACTIVE );
        if app_Focus Then
          begin
            app_Pause := FALSE;
            app_PActivate( TRUE );
            FillChar( keysDown[ 0 ], 256, 0 );
            key_ClearState();
            FillChar( mouseDown[ 0 ], 3, 0 );
            mouse_ClearState();
            if ( wnd_FullScreen ) and ( not wnd_First ) Then
              scr_SetOptions( scr_Width, scr_Height, scr_Refresh, wnd_FullScreen, scr_VSync );
          end else
            begin
              if app_AutoPause Then app_Pause := TRUE;
              app_PActivate( FALSE );
              if app_Work and ( wnd_FullScreen ) and ( not wnd_First ) Then
                begin
                  scr_Reset();
                  wnd_Update();
                end;
            end;
       end;
    WM_NCHITTEST:
      begin
        Result := DefWindowProcW( hWnd, Msg, wParam, lParam );
        if ( not app_Focus ) and ( Result = HTCAPTION ) Then
          Result := HTCLIENT;
      end;
    WM_MOVING:
      begin
        wnd_X := PRect( lParam ).Left;
        wnd_Y := PRect( lParam ).Top;
      end;
    WM_SETCURSOR:
      begin
        if ( app_Focus ) and ( LOWORD ( lparam ) = HTCLIENT ) and ( not app_ShowCursor ) Then
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
      end;
    WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
      begin
        mouseDown[ M_BMIDLE ] := TRUE;
        if mouseCanClick[ M_BMIDLE ] Then
          begin
            mouseClick[ M_BMIDLE ]    := TRUE;
            mouseCanClick[ M_BMIDLE ] := FALSE;
          end;
        if Msg = WM_MBUTTONDBLCLK Then
          mouseDblClick[ M_BMIDLE ] := TRUE;
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
      end;
    WM_LBUTTONUP:
      begin
        mouseDown[ M_BLEFT ]     := FALSE;
        mouseUp  [ M_BLEFT ]     := TRUE;
        mouseCanClick[ M_BLEFT ] := TRUE;
      end;
    WM_MBUTTONUP:
      begin
        mouseDown[ M_BMIDLE ]     := FALSE;
        mouseUp  [ M_BMIDLE ]     := TRUE;
        mouseCanClick[ M_BMIDLE ] := TRUE;
      end;
    WM_RBUTTONUP:
      begin
        mouseDown[ M_BRIGHT ]     := FALSE;
        mouseUp  [ M_BRIGHT ]     := TRUE;
        mouseCanClick[ M_BRIGHT ] := TRUE;
      end;
    WM_MOUSEWHEEL:
      begin
        if wParam > 0 Then
          begin
            mouseWheel[ M_WUP   ] := TRUE;
            mouseWheel[ M_WDOWN ] := FALSE;
          end else
            begin
              mouseWheel[ M_WUP   ] := FALSE;
              mouseWheel[ M_WDOWN ] := TRUE;
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

        if ( Msg = WM_SYSKEYDOWN ) and ( key = K_F4 ) Then
          app_Work := FALSE;
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
      end;
    WM_CHAR:
      begin
        if keysCanText Then
        case winkey_to_scancode( wParam ) of
          K_BACKSPACE: u_Backspace( keysText );
          K_TAB:       key_InputText( '  ' );
        else
          if app_Flags and APP_USE_UTF8 > 0 Then
            begin
              len := WideCharToMultiByte( CP_UTF8, 0, @wParam, 1, nil, 0, nil, nil );
              WideCharToMultiByte( CP_UTF8, 0, @wParam, 1, @c[ 0 ], 5, nil, nil );
              str := '';
              for i := 0 to len - 1 do
                str := str + c[ i ];
              if str <> '' Then
                key_InputText( str );
            end else
              if wParam < 128 Then
                key_InputText( Char( CP1251_TO_UTF8[ wParam ] ) )
              else
                for i := 128 to 255 do
                  if wParam = CP1251_TO_UTF8[ i ] Then
                    begin
                      key_InputText( Char( i ) );
                      break;
                    end;
        end;
      end;
  else
    Result := DefWindowProcW( hWnd, Msg, wParam, lParam );
  end;
{$ENDIF}
{$IFDEF DARWIN}
  eClass := GetEventClass( inEvent );
  eKind  := GetEventKind( inEvent );
  Result := CallNextEventHandler( inHandlerCallRef, inEvent );

  case eClass of
    kEventClassCommand:
      case eKind of
        kEventProcessCommand:
          begin
            GetEventParameter( inEvent, kEventParamDirectObject, kEventParamHICommand, nil, SizeOf( HICommand ), nil, @command );
            if command.commandID = kHICommandQuit Then
              zgl_Exit();
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
            app_Focus := TRUE;
            app_Pause := FALSE;
            app_PActivate( TRUE );
            FillChar( keysDown[ 0 ], 256, 0 );
            key_ClearState();
            FillChar( mouseDown[ 0 ], 3, 0 );
            mouse_ClearState();
            if wnd_FullScreen Then
              scr_SetOptions( scr_Width, scr_Height, scr_Refresh, wnd_FullScreen, scr_VSync );
          end;
        kEventWindowDeactivated:
          begin
            app_Focus := FALSE;
            if app_AutoPause Then app_Pause := TRUE;
            app_PActivate( FALSE );
            if wnd_FullScreen Then
              scr_Reset();
          end;
        kEventWindowCollapsed:
          begin
            app_Focus := FALSE;
            app_Pause := TRUE;
          end;
        kEventWindowClosed:
          begin
            wnd_Handle := nil;
            app_Work   := FALSE;
          end;
        kEventWindowBoundsChanged:
          begin
            if not wnd_FullScreen Then
              begin
                GetEventParameter( inEvent, kEventParamCurrentBounds, typeHIRect, nil, SizeOf( bounds ), nil, @bounds );
                wnd_X := Round( bounds.origin.x );
                wnd_Y := Round( bounds.origin.y );
              end else
                begin
                  wnd_X := 0;
                  wnd_Y := 0;
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
              for i := 0 to 2 do
                if SCAKey and Modifier[ i ].bit > 0 Then
                  begin
                    if not keysDown[ Modifier[ i ].key ] Then
                      doKeyPress( Modifier[ i ].key );
                    keysDown[ Modifier[ i ].key ] := TRUE;
                    keysUp  [ Modifier[ i ].key ] := FALSE;
                    keysLast[ KA_DOWN ]           := Modifier[ i ].key;
                  end else
                    begin
                      if keysDown[ Modifier[ i ].key ] Then
                        begin
                          keysUp[ Modifier[ i ].key ] := TRUE;
                          keysLast[ KA_UP ]           := Modifier[ i ].key;
                        end;
                      keysDown[ Modifier[ i ].key ] := FALSE;
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
                str := '';
                for i := 0 to len - 1 do
                  str := str + c[ i ];
                if str <> '' Then
                  key_InputText( str );
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
            end;
        end;
      end;

    kEventClassMouse:
      case eKind of
        kEventMouseMoved, kEventMouseDragged:
          begin
            GetEventParameter( inEvent, kEventParamMouseLocation, typeHIPoint, nil, SizeOf( HIPoint ), nil, @mPos );

            if not mouseLock Then
              begin
                mouseX := Round( mPos.X ) - wnd_X;
                mouseY := Round( mPos.Y ) - wnd_Y;
              end else
                begin
                  mouseX := Round( mPos.X - wnd_Width  / 2 );
                  mouseY := Round( mPos.Y - wnd_Height / 2 );
                end;

            wnd_MouseIn := ( mPos.X > wnd_X ) and ( mPos.X < wnd_X + wnd_Width ) and ( mPos.Y > wnd_Y ) and ( mPos.Y < wnd_Y + wnd_Height );
            if wnd_MouseIn Then
              begin
                if ( not app_ShowCursor ) and ( CGCursorIsVisible = 1 ) Then
                  CGDisplayHideCursor( scr_Display );
                if ( app_ShowCursor ) and ( CGCursorIsVisible = 0 ) Then
                  CGDisplayShowCursor( scr_Display );
              end else
              if CGCursorIsVisible = 0 Then
                CGDisplayShowCursor( scr_Display );
          end;
        kEventMouseDown:
          begin
            GetEventParameter( inEvent, kEventParamMouseButton, typeMouseButton, nil, SizeOf( EventMouseButton ), nil, @mButton );

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
                end;
              kEventMouseButtonTertiary: // Midle
                begin
                  mouseDown[ M_BMIDLE ] := TRUE;
                  if mouseCanClick[ M_BMIDLE ] Then
                    begin
                      mouseClick[ M_BMIDLE ] := TRUE;
                      mouseCanClick[ M_BMIDLE ] := FALSE;
                      if timer_GetTicks() - mouseDblCTime[ M_BMIDLE ] < mouseDblCInt Then
                        mouseDblClick[ M_BMIDLE ] := TRUE;
                      mouseDblCTime[ M_BMIDLE ] := timer_GetTicks();
                    end;
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
                end;
            end;
          end;
        kEventMouseUp:
          begin
            GetEventParameter( inEvent, kEventParamMouseButton, typeMouseButton, nil, SizeOf( EventMouseButton ), nil, @mButton );

            case mButton of
              kEventMouseButtonPrimary: // Left
                begin
                  mouseDown[ M_BLEFT ]     := FALSE;
                  mouseUp  [ M_BLEFT ]     := TRUE;
                  mouseCanClick[ M_BLEFT ] := TRUE;
                end;
              kEventMouseButtonTertiary: // Midle
                begin
                  mouseDown[ M_BMIDLE ]     := FALSE;
                  mouseUp  [ M_BMIDLE ]     := TRUE;
                  mouseCanClick[ M_BMIDLE ] := TRUE;
                end;
              kEventMouseButtonSecondary: // Right
                begin
                  mouseDown[ M_BRIGHT ]     := FALSE;
                  mouseUp  [ M_BRIGHT ]     := TRUE;
                  mouseCanClick[ M_BRIGHT ] := TRUE;
                end;
            end;
          end;
        kEventMouseWheelMoved:
          begin
            GetEventParameter( inEvent, kEventParamMouseWheelDelta, typeSInt32, nil, 4, nil, @mWheel );

            if mWheel > 0 then
              mouseWheel[ M_WUP ] := TRUE
            else
              mouseWheel[ M_WDOWN ] := TRUE;
          end;
      end;
  end;
{$ENDIF}
end;

initialization
  app_Flags := WND_USE_AUTOCENTER or APP_USE_LOG or COLOR_BUFFER_CLEAR or CLIP_INVISIBLE;

end.
