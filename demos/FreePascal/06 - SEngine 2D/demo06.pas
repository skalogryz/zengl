// RU: Этот пример использует стандартный процедурный спрайтовый менеджер, который
// сгодиться для решения простых задач или для любителей plain-style вроде меня :)
// Этот же пример с использованием спрайтового менеджера на классах ищите в "07 - SEngine 2D(OOP)"
//
// EN: This example use standard procedural sprite engine that can be used for
// simple tasks or for anything if you like plain-style code as me :)
// This example with classes can be found in this demo - "07 - SEngine 2D(OOP)"
program demo06;

{$DEFINE STATIC}

uses
  {$IFNDEF STATIC}
  zglHeader
  {$ELSE}
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_render_2d,
  zgl_fx,
  zgl_textures,
  zgl_textures_png,
  zgl_textures_jpg,
  zgl_sprite_2d,
  zgl_sengine_2d,
  zgl_primitives_2d,
  zgl_font,
  zgl_text,
  zgl_math_2d,
  zgl_utils
  {$ENDIF}
  ;

var
  dirRes    : String {$IFNDEF DARWIN} = '../data/' {$ENDIF};
  fntMain   : zglPFont;
  texLogo   : zglPTexture;
  texMiku   : zglPTexture;
  time      : Integer;
  sengine2d : zglTSEngine2D;

// Miku
procedure MikuInit( const Sprite : zglPSprite2D );
begin
  Sprite.X := 800 + random( 800 );
  Sprite.Y := random( 600 - 128 );
  // RU: Задаем скорость движения. В пользовательском параметре Data выделим память под структуру zglTPoint2D.
  // EN: Set the moving speed. Allocate memory for structure zglTPoint2D in userspace parameter "Data".
  zgl_GetMem( Sprite.Data, SizeOf( zglTPoint2D ) );
  with zglTPoint2D( Sprite.Data^ ) do
    begin
      X := -random( 10 ) / 5 - 0.5;
      Y := ( random( 10 ) - 5 ) / 5;
    end;
end;

procedure MikuDraw( const Sprite : zglPSprite2D );
begin
  with Sprite^ do
    asprite2d_Draw( Texture, X, Y, W, H, Angle, Round( Frame ), Alpha, FxFlags );
end;

procedure MikuProc( const Sprite : zglPSprite2D );
  var
    speed : zglPPoint2D;
begin
  with Sprite^ do
    begin
      speed := Data;

      X := X + speed.X;
      Y := Y + speed.Y;
      Frame := Frame + ( abs( speed.X ) + abs( speed.Y ) ) / 25;
      if Frame > 8 Then
        Frame := 1;
      // RU: Если спрайт выходит за пределы по X, сразу же удаляем его.
      // EN: Delete the sprite if it goes beyond X.
      if X < -128 Then sengine2d_DelSprite( ID );
      // RU: Если спрайт выходит за пределы по Y, ставим его в очередь на удаление.
      // EN: Add sprite to queue for delete if it goes beyond Y.
      if Y < -128 Then Destroy := TRUE;
      if Y > 600  Then Destroy := TRUE;
    end;
end;

procedure MikuFree( const Sprite : zglPSprite2D );
begin
  // RU: Очистим ранее выделенную память.
  // EN: Free the memory allocated for Data.
  zgl_FreeMem( Sprite.Data );
end;

// RU: Добавить 100 спрайтов.
// EN: Add 100 sprites.
procedure AddMiku;
  var
    i : Integer;
begin
  // RU: При добавлении спрайта в менеджер спрайтов указывается текстура, слой(положение по Z) и
  // указатели на основные функции - Инициализация, Рендер, Обработка и Уничтожение.
  // EN: For adding sprite to sprite engine must be set next parameters: texture, layer(Z-coordinate) and
  // pointers to Initialization, Render, Process and Destroy functions.
  for i := 1 to 100 do
    sengine2d_AddSprite( texMiku, random( 10 ), @MikuInit, @MikuDraw, @MikuProc, @MikuFree );
end;

// RU: Удалить 100 спрайтов.
// EN: Delete 100 sprites.
procedure DelMiku;
  var
    i : Integer;
begin
  // RU: Удалим 100 спрайтов со случайным ID.
  // EN: Delete 100 sprites with random ID.
  for i := 1 to 100 do
    sengine2d_DelSprite( random( sengine2d.Count ) );
end;

procedure Init;
  var
    i : Integer;
begin
  texLogo := tex_LoadFromFile( dirRes + 'zengl.png' );

  texMiku := tex_LoadFromFile( dirRes + 'miku.png' );
  tex_SetFrameSize( texMiku, 128, 128 );

  // RU: Устанавливаем текущим менеджером спрайтов свой.
  // EN: Set own sprite engine as current.
  sengine2d_Set( @sengine2d );
  // RU: Создадим 1000 спрайтов Miku-chan :)
  // EN: Create 1000 sprites of Miku-chan :)
  for i := 0 to 9 do
    AddMiku();

  fntMain := font_LoadFromFile( dirRes + 'font.zfi' );
end;

procedure Draw;
begin
  batch2d_Begin();
  // RU: Рисуем все спрайты находящиеся в текущем спрайтовом менеджере.
  // EN: Render all sprites contained in current sprite engine.
  if time > 255 Then
    sengine2d_Draw();

  if time <= 255 Then
    ssprite2d_Draw( texLogo, 400 - 256, 300 - 128, 512, 256, 0, time )
  else
    if time < 510 Then
      begin
        pr2d_Rect( 0, 0, 800, 600, $000000, 510 - time, PR2D_FILL );
        ssprite2d_Draw( texLogo, 400 - 256, 300 - 128, 512, 256, 0, 510 - time );
      end;

  if time > 255 Then
    begin
      pr2d_Rect( 0, 0, 256, 64, $000000, 200, PR2D_FILL );
      text_Draw( fntMain, 0, 0, 'FPS: ' + u_IntToStr( zgl_Get( RENDER_FPS ) ) );
      text_Draw( fntMain, 0, 20, 'Sprites: ' + u_IntToStr( sengine2d.Count ) );
      text_Draw( fntMain, 0, 40, 'Up/Down - Add/Delete Miku :)' );
    end;
  batch2d_End();
end;

procedure Timer;
begin
  INC( time, 2 );

  // RU: Выполняем обработку всех спрайтов в текущем спрайтовом менеджере.
  // EN: Process all sprites contained in current sprite engine.
  sengine2d_Proc();
  // RU: По нажатию пробела очистить все спрайты.
  // EN: Delete all sprites if space was pressed.
  if key_Press( K_SPACE ) Then sengine2d_ClearAll();
  if key_Press( K_UP ) Then AddMiku();
  if key_Press( K_DOWN ) Then DelMiku();
  if key_Press( K_ESCAPE ) Then zgl_Exit();
  key_ClearState();
end;

procedure Quit;
begin
  // RU: Очищаем память от созданных спрайтов.
  // EN: Free allocated memory for sprites.
  sengine2d_Set( @sengine2d );
  sengine2d_ClearAll();
end;

Begin
  {$IFNDEF STATIC}
  zglLoad( libZenGL );
  {$ENDIF}

  randomize;

  timer_Add( @Timer, 16 );
  timer_Add( @AddMiku, 1000 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );
  zgl_Reg( SYS_EXIT, @Quit );

  // RU: Т.к. модуль сохранен в кодировке UTF-8 и в нем используются строковые переменные
  // следует указать использование этой кодировки.
  // EN: Enable using of UTF-8, because this unit saved in UTF-8 encoding and here used
  // string variables.
  zgl_Enable( APP_USE_UTF8 );

  wnd_SetCaption( '06 - SEngine 2D' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
