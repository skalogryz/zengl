program demo02;

uses
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_mouse,
  zgl_font,
  zgl_text,
  zgl_textures,
  zgl_textures_png,
  zgl_primitives_2d,
  zgl_utils
  ;

var
  FullScreen : Boolean;
  fnt        : zglPFont;
  something  : String;
  lineAlpha  : Byte;


procedure Init;
  var
    i : Integer;
begin
  // Загрузка шрифта и вывод текста освещен в "04 - Text"
  fnt := font_LoadFromFile( '../res/font.zfi' );
  for i := 0 to fnt.Count.Pages - 1 do
    fnt.Pages[ i ] := tex_LoadFromFile( '../res/font_' + u_IntToStr( i ) + '.png', $FF000000, TEX_DEFAULT_2D );

  // Начнем считывать текст с клавиатуры и ограничимся 20 символами
  key_BeginReadText( something, 20 );
end;

procedure Draw;
  var
    w : Single;
begin
  text_Draw( fnt, 0, 0, 'Escape - Exit' );
  text_Draw( fnt, 0, 20, 'Alt+Enter - FullScreen/Windowed mode' );
  text_Draw( fnt, 0, 40, 'Left mouse button - lock mouse :)' );

  text_Draw( fnt, 400, 300 - 30, 'Enter something(maximum - 20 symbols):', TEXT_HALIGN_CENTER );
  text_Draw( fnt, 400, 300, something, TEXT_HALIGN_CENTER );
  w := text_GetWidth( fnt, something );
  pr2d_Rect( 400 + w / 2, 300, 10, 20, $FFFFFF, lineAlpha, PR2D_FILL );
end;

procedure Timer;
begin
  DEC( lineAlpha, 10 );

  // Если зажат Alt и был нажат Enter - переключиться в полноэкранный или оконный режим
  if key_Down( K_ALT ) and key_Press( K_ENTER ) Then
    begin
      FullScreen := not FullScreen;
      scr_SetOptions( 800, 600, 32, REFRESH_MAXIMUM, FullScreen, FALSE );
    end;
  // По нажатию Escape завершить приложение
  if key_Press( K_ESCAPE ) Then zgl_Exit;

  // Если зажата левая кнопка мыши - заблокируем мышку по центру экрана
  // смещения можно получать используя функции mouse_DX и mouse_DY вызывая их до mouse_Lock
  if mouse_Down( M_BLEFT ) Then
    mouse_Lock;

  // "Считываем" в переменную введеный текст
  key_EndReadText( something );

  // Обязательно очищаем состояния
  key_ClearState;
  mouse_ClearState;
end;

Begin
  timer_Add( @Timer, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  // Т.к. модуль сохранен в кодировке UTF-8 и в нем используются строковые переменные
  // следует указать использования этой кодировки
  zgl_Enable( APP_USE_UTF8 );

  wnd_SetCaption( '02 - Input' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, 32, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init;
End.
