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
  X, XLib
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  Messages
  {$ENDIF}
  {$IFDEF DARWIN}
  MacOSAll
  {$ENDIF}
  ;

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
  appWorkDir        : String;
  appHomeDir        : String;

  // call-back
  app_PInit     : procedure = app_Init;
  app_PLoop     : procedure = app_MainLoop;
  app_PLoad     : procedure = zero;
  app_PDraw     : procedure = zero;
  app_PExit     : procedure = zero;
  app_PUpdate   : procedure( dt : Double ) = zerou;
  app_PActivate : procedure( activate : Boolean ) = zeroa;

  {$IFDEF LINUX}
  appCursor : TCursor = None;
  appXIM    : PXIM;
  appXIC    : PXIC;
  {$ENDIF}
  {$IFDEF WINDOWS}
  appTimer : LongWord;
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
  zgl_opengl,
  zgl_opengl_simple,
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
  scr_Clear();
  app_PLoad();
  scr_Flush();

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
      app_PUpdate( timer_GetTicks() - appdt );
      appdt := t;

      app_Draw();
    end;
end;

procedure app_ProcessOS;
  var
  {$IFDEF LINUX}
    root_return   : TWindow;
    child_return  : TWindow;
    root_x_return : Integer;
    root_y_return : Integer;
    mask_return   : LongWord;
  {$ENDIF}
  {$IFDEF WINDOWS}
    m         : tagMsg;
    cursorpos : TPoint;
  {$ENDIF}
  {$IFDEF DARWIN}
    event : EventRecord;
    mPos  : Point;
  {$ENDIF}
begin
{$IFDEF LINUX}
  XQueryPointer( scrDisplay, wndHandle, @root_return, @child_return, @root_x_return, @root_y_return, @mouseX, @mouseY, @mask_return );

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

  while PeekMessageW( m, 0{wnd_Handle}, 0, 0, PM_REMOVE ) do
    begin
      TranslateMessage( m );
      DispatchMessageW( m );
    end;
{$ENDIF}
{$IFDEF DARWIN}
  GetGlobalMouse( mPos );
  mouseX := mPos.h - wndX;
  mouseY := mPos.v - wndY;

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
    mButton : EventMouseButton;
    mWheel  : Integer;
    bounds  : HIRect;
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
  while XPending( scrDisplay ) <> 0 do
    begin
      XNextEvent( scrDisplay, @event );

      case event._type of
        ClientMessage:
          if ( event.xclient.message_type = wndProtocols ) and ( event.xclient.data.l[ 0 ] = wndDestroyAtom ) Then appWork := FALSE;

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
                end;
              2: // Midle
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
                  mouseDown[ M_BMIDDLE ]     := FALSE;
                  mouseUp  [ M_BMIDDLE ]     := TRUE;
                  mouseCanClick[ M_BMIDDLE ] := TRUE;
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
              len := Xutf8LookupString( appXIC, @event, @c[ 0 ], 6, @keysym, @status );
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
  if ( not appWork ) and ( Msg <> WM_ACTIVATE ) Then
    begin
      Result := DefWindowProcW( hWnd, Msg, wParam, lParam );
      exit;
    end;
  case Msg of
    WM_CLOSE, WM_DESTROY, WM_QUIT:
      appWork := FALSE;

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
    WM_ACTIVATE:
      begin
        appFocus := ( LOWORD( wParam ) <> WA_INACTIVE );
        if appFocus Then
          begin
            appPause := FALSE;
            if appWork Then app_PActivate( TRUE );
            FillChar( keysDown[ 0 ], 256, 0 );
            key_ClearState();
            FillChar( mouseDown[ 0 ], 3, 0 );
            mouse_ClearState();
            if ( wndFullScreen ) and ( not wndFirst ) Then
              scr_SetOptions( scrWidth, scrHeight, scrRefresh, wndFullScreen, scrVSync );
          end else
            begin
              if appAutoPause Then appPause := TRUE;
              if appWork Then app_PActivate( FALSE );
              if appWork and ( wndFullScreen ) and ( not wndFirst ) Then
                begin
                  scr_Reset();
                  wnd_Update();
                end;
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
        mouseDown[ M_BMIDDLE ]     := FALSE;
        mouseUp  [ M_BMIDDLE ]     := TRUE;
        mouseCanClick[ M_BMIDDLE ] := TRUE;
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
          appWork := FALSE;
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
          if appFlags and APP_USE_UTF8 > 0 Then
            begin
              {$IFNDEF FPC}
              if SizeOf( Char ) = 1 Then
                begin
              {$ENDIF}
                  len := WideCharToMultiByte( CP_UTF8, 0, @wParam, 1, nil, 0, nil, nil );
                  WideCharToMultiByte( CP_UTF8, 0, @wParam, 1, @c[ 0 ], 5, nil, nil );
                  str := '';
                  for i := 0 to len - 1 do
                    str := str + c[ i ];
                  if str <> '' Then
                    key_InputText( str );
              {$IFNDEF FPC}
                end else
                  key_InputText( Char( wParam ) );
              {$ENDIF}
            end else
            {$IFNDEF FPC}
              if SizeOf( Char ) = 2 Then
                key_InputText( Char( wParam ) )
              else
            {$ENDIF}
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

  if appWork Then
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
                end;
              kEventMouseButtonTertiary: // Midle
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

            // Magic Mouse !!! XD
            if keysDown[ K_SUPER ] and ( mButton = kEventMouseButtonPrimary ) Then
              mButton := kEventMouseButtonSecondary;

            case mButton of
              kEventMouseButtonPrimary: // Left
                begin
                  mouseDown[ M_BLEFT ]     := FALSE;
                  mouseUp  [ M_BLEFT ]     := TRUE;
                  mouseCanClick[ M_BLEFT ] := TRUE;
                end;
              kEventMouseButtonTertiary: // Midle
                begin
                  mouseDown[ M_BMIDDLE ]     := FALSE;
                  mouseUp  [ M_BMIDDLE ]     := TRUE;
                  mouseCanClick[ M_BMIDDLE ] := TRUE;
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
  appFlags := WND_USE_AUTOCENTER or APP_USE_LOG or COLOR_BUFFER_CLEAR or CLIP_INVISIBLE;

end.
