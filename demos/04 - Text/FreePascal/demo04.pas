program demo04;

uses
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_render_2d,
  zgl_fx,
  zgl_primitives_2d,
  zgl_textures,
  zgl_textures_png, // Важный момент, обязательно один раз подключить модуль с поддержкой нужного формата данных
  zgl_font,
  zgl_text,
  zgl_math_2d,
  zgl_utils;

var
  fnt : zglPFont;

procedure Init;
  var
    i : Integer;
begin
  // Загружаем данные о шрифте
  fnt := font_LoadFromFile( '../res/font.zfi' );
  // Загружаем текстуры
  for i := 0 to fnt.Count.Pages - 1 do
    fnt.Pages[ i ] := tex_LoadFromFile( '../res/font_' + u_IntToStr( i ) + '.png', $FF000000, TEX_DEFAULT_2D );
end;

procedure Draw;
  var
    i : Integer;
    r : zglTRect;
    s : AnsiString;
begin
  batch2d_Begin;
  text_Draw( fnt, 400, 25, 'Строка с выравниванием по центру', TEXT_HALIGN_CENTER );
  text_DrawEx( fnt, 400, 65, 2, 0, 'Масштабирование', 255, $FFFFFF, TEXT_HALIGN_CENTER );
  fx2d_SetVCA( $FF0000, $00FF00, $0000FF, $FFFFFF, 255, 255, 255, 255 );
  text_Draw( fnt, 400, 125, 'Градация цвета для каждого символа', TEXT_FX_VCA or TEXT_HALIGN_CENTER );

  r.X := 0;
  r.Y := 300 - 128;
  r.W := 192;
  r.H := 256;
  text_DrawInRect( fnt, r, 'Обычный вывод текста в прямоугольнике' );
  pr2d_Rect( r.X, r.Y, r.W, r.H, $FF0000 );

  r.X := 800 - 192;
  r.Y := 300 - 128;
  r.W := 192;
  r.H := 256;
  text_DrawInRect( fnt, r, 'Вывод текста используя выравнивание по правому краю и размещение снизу', TEXT_HALIGN_RIGHT or TEXT_VALIGN_BOTTOM );
  pr2d_Rect( r.X, r.Y, r.W, r.H, $FF0000 );

  r.X := 400 - 192;
  r.Y := 300 - 128;
  r.W := 384;
  r.H := 256;
  // Если возникает вопрос почему я разделил текст на две части, то отвечу - FreePascal капризничает, и не хочет
  // обрабатывать константные строки длиннее 255 символов :)
  text_DrawInRect( fnt, r, 'Этот текст использует выравнивание по ширине и центрируется по вертикали.' +
                           ' Текст, который не помещается в пределах прямоугольника будет отсечен.', TEXT_HALIGN_JUSTIFY or TEXT_VALIGN_CENTER );
  pr2d_Rect( r.X, r.Y, r.W, r.H, $FF0000 );

  r.X := 400 - 320;
  r.Y := 300 + 160;
  r.W := 640;
  r.H := 128;
  text_DrawInRect( fnt, r, 'Для переноса строк можно использовать LF-символ' + #10 + 'код которого равен 10 и обозначен в таблице Unicode как "Line Feed"', TEXT_HALIGN_CENTER or TEXT_VALIGN_CENTER );
  pr2d_Rect( r.X, r.Y, r.W, r.H, $FF0000 );

  // Выводим количество FPS в правом углу, используя text_GetWidth
  s := 'FPS: ' + u_IntToStr( zgl_Get( SYS_FPS ) );
  text_Draw( fnt, 800 - text_GetWidth( fnt, s ), 0, s );
  batch2d_End;
end;

procedure Proc;
begin
  if key_Press( K_ESCAPE ) Then zgl_Exit;
  key_ClearState;
end;

Begin
  randomize;

  timer_Add( @Proc, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  // Т.к. модуль сохранен в кодировке UTF-8 и в нем используются строковые переменные
  // следует указать использования этой кодировки
  zgl_Enable( APP_USE_UTF8 );

  wnd_SetCaption( '04 - Text' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, 32, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init;
End.
