program demo02;

{$DEFINE STATIC}

uses
  {$IFNDEF STATIC}
  zglHeader
  {$ELSE}
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_mouse,
  zgl_keyboard,
  zgl_joystick,
  zgl_font,
  zgl_text,
  zgl_textures,
  zgl_textures_png,
  zgl_primitives_2d,
  zgl_utils
  {$ENDIF}
  ;

var
  dirRes     : UTF8String {$IFNDEF DARWIN} = '../data/' {$ENDIF};
  fullScreen : Boolean;
  fnt        : zglPFont;
  something  : UTF8String;
  lineAlpha  : Byte;

procedure Init;
begin
  // RU: Загрузка шрифта и вывод текста освещен в "04 - Text".
  // EN: Font loading and text rendering will be described in this demo - "04 - Text".
  fnt := font_LoadFromFile( dirRes + 'font.zfi' );

  // RU: Начнем считывать текст с клавиатуры и ограничимся 20 символами.
  // EN: Start to read a text and set maximum count of symbols to 20.
  key_BeginReadText( something, 20 );

  // RU: Инициализируем поддержку джойстиков.
  // EN: Initialize joystick support.
  joy_Init();
end;

procedure Draw;
  var
    w : Single;
begin
  text_Draw( fnt, 0, 0, 'Escape - Exit' );
  text_Draw( fnt, 0, 20, 'Alt+Enter - FullScreen/Windowed mode' );
  text_Draw( fnt, 0, 40, 'Left mouse button - lock mouse :)' );

  text_Draw( fnt, 400, 300 - 100, 'Enter something(maximum - 20 symbols):', TEXT_HALIGN_CENTER );
  text_Draw( fnt, 400, 300 - 70, something, TEXT_HALIGN_CENTER );
  w := text_GetWidth( fnt, something );
  pr2d_Rect( 400 + w / 2, 300 - 70, 10, 20, $FFFFFF, lineAlpha, PR2D_FILL );

  text_Draw( fnt, 400, 360, 'JOYSTICK', TEXT_HALIGN_CENTER );

  // RU: Вывод состояния осей и кнопок первого джойстика в системе.
  // EN: Show the state of axes and buttons of first joystick in the system.
  text_Draw( fnt, 100, 400, 'Axis X: ' + u_FloatToStr( joy_AxisPos( 0, JOY_AXIS_X ) ) );
  text_Draw( fnt, 100, 420, 'Axis Y: ' + u_FloatToStr( joy_AxisPos( 0, JOY_AXIS_Y ) ) );
  text_Draw( fnt, 100, 440, 'Axis Z: ' + u_FloatToStr( joy_AxisPos( 0, JOY_AXIS_Z ) ) );
  text_Draw( fnt, 100, 460, 'Axis R: ' + u_FloatToStr( joy_AxisPos( 0, JOY_AXIS_R ) ) );
  text_Draw( fnt, 100, 480, 'Axis U: ' + u_FloatToStr( joy_AxisPos( 0, JOY_AXIS_U ) ) );
  text_Draw( fnt, 100, 500, 'Axis V: ' + u_FloatToStr( joy_AxisPos( 0, JOY_AXIS_V ) ) );
  text_Draw( fnt, 100, 520, 'POVX: ' + u_FloatToStr( joy_AxisPos( 0, JOY_POVX ) ) );
  text_Draw( fnt, 100, 540, 'POVY: ' + u_FloatToStr( joy_AxisPos( 0, JOY_POVY ) ) );

  text_Draw( fnt, 400, 400, 'Button1: ' + u_BoolToStr( joy_Down( 0, 0 ) ) );
  text_Draw( fnt, 400, 420, 'Button2: ' + u_BoolToStr( joy_Down( 0, 1 ) ) );
  text_Draw( fnt, 400, 440, 'Button3: ' + u_BoolToStr( joy_Down( 0, 2 ) ) );
  text_Draw( fnt, 400, 460, 'Button4: ' + u_BoolToStr( joy_Down( 0, 3 ) ) );
  text_Draw( fnt, 400, 480, 'Button5: ' + u_BoolToStr( joy_Down( 0, 4 ) ) );
  text_Draw( fnt, 400, 500, 'Button6: ' + u_BoolToStr( joy_Down( 0, 5 ) ) );
  text_Draw( fnt, 400, 520, 'Button7: ' + u_BoolToStr( joy_Down( 0, 6 ) ) );
  text_Draw( fnt, 400, 540, 'Button8: ' + u_BoolToStr( joy_Down( 0, 7 ) ) );
  text_Draw( fnt, 550, 400, 'Button9: ' + u_BoolToStr( joy_Down( 0, 8 ) ) );
  text_Draw( fnt, 550, 420, 'Button10: ' + u_BoolToStr( joy_Down( 0, 9 ) ) );
  text_Draw( fnt, 550, 440, 'Button11: ' + u_BoolToStr( joy_Down( 0, 10 ) ) );
  text_Draw( fnt, 550, 460, 'Button12: ' + u_BoolToStr( joy_Down( 0, 11 ) ) );
  text_Draw( fnt, 550, 480, 'Button13: ' + u_BoolToStr( joy_Down( 0, 12 ) ) );
  text_Draw( fnt, 550, 500, 'Button14: ' + u_BoolToStr( joy_Down( 0, 13 ) ) );
  text_Draw( fnt, 550, 520, 'Button15: ' + u_BoolToStr( joy_Down( 0, 14 ) ) );
  text_Draw( fnt, 550, 540, 'Button16: ' + u_BoolToStr( joy_Down( 0, 15 ) ) );
end;

procedure Timer;
begin
  DEC( lineAlpha, 10 );

  // RU: Если зажат Alt и был нажат Enter - переключиться в полноэкранный или оконный режим.
  // EN: If Alt+Enter was pressed - switch to fullscreen or windowed mode.
  if key_Down( K_ALT ) and key_Press( K_ENTER ) Then
    begin
      FullScreen := not FullScreen;
      scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FullScreen, FALSE );
    end;
  // RU: По нажатию Escape завершить приложение.
  // EN: If Escape was pressed - shutdown the application.
  if key_Press( K_ESCAPE ) Then zgl_Exit();

  // RU: Если зажата левая кнопка мыши - заблокируем мышку по центру экрана.
  // Смещения можно получать используя функции mouse_DX и mouse_DY вызывая их до mouse_Lock.
  // EN: If left mouse button is down - lock the mouse cursor in center of screen.
  // Delta can be obtained from functions mouse_DX and mouse_DY by calling them before mouse_Lock.
  if mouse_Down( M_BLEFT ) Then
    mouse_Lock();

  // RU: "Считываем" в переменную введеный текст.
  // EN: "Read" the text to variable.
  something := key_GetText();

  // RU: Обязательно очищаем состояния.
  // EN: Necessarily clear all the states.
  key_ClearState();
  mouse_ClearState();
  joy_ClearState();
end;

Begin
  {$IFNDEF STATIC}
  zglLoad( libZenGL );
  {$ENDIF}

  timer_Add( @Timer, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  wnd_SetCaption( '02 - Input' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
