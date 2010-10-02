// Этот пример аналогичен предыдущему, за исключением того, что использует
// спрайтовый менеджер на основе классов из директории extra
program demo07;

{$DEFINE STATIC}

uses
  zglSpriteEngine, // Этот модуль лежит в директории extra. В нем так же есть дефайн STATIC необходимый для смены режима сборки
  {$IFNDEF STATIC}
  zglHeader,
  {$ELSE}
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_render_2d,
  zgl_fx,
  zgl_textures,
  zgl_textures_png, // Важный момент, обязательно один раз подключить модуль с поддержкой нужного формата данных
  zgl_textures_jpg,
  zgl_sprite_2d,
  zgl_primitives_2d,
  zgl_font,
  zgl_text,
  zgl_math_2d,
  zgl_utils
  {$ENDIF}
  ;

type
  CMiku = class(zglCSprite2D)
  protected
    FSpeed : zglTPoint2D;
  public
    procedure OnInit( const _Texture : zglPTexture; const _Layer : Integer ); override;
    procedure OnDraw; override;
    procedure OnProc; override;
    procedure OnFree; override;
  end;

var
  dirRes    : String = '../../res/';
  fntMain   : zglPFont;
  texLogo   : zglPTexture;
  texMiku   : zglPTexture;
  time      : Integer;
  sengine2d : zglCSEngine2D;

// Miku
procedure CMiku.OnInit;
begin
  // Укажем свою текстуру и Layer для спрайта, заодно установятся стандартные
  // параметры вроде ширины и высоты на основе данных о кадре в текстуре
  inherited OnInit( texMiku, random( 10 ) );

  X := 800 + random( 800 );
  Y := random( 600 - 128 );
  // Задаем скорость движения
  FSpeed.X := -random( 10 ) / 5 - 0.5;
  FSpeed.Y := ( random( 10 ) - 5 ) / 5;
end;

procedure CMiku.OnDraw;
begin
  // Т.к. по сути эта процедура объявлена только для примера, то вызовем основной
  // метод OnDraw класса zglCSprite2D
  inherited;
end;

procedure CMiku.OnProc;
begin
  inherited;
  X := X + FSpeed.X;
  Y := Y + FSpeed.Y;
  Frame := Frame + ( abs( FSpeed.X ) + abs( FSpeed.Y ) ) / 25;
  if Frame > 8 Then
    Frame := 1;
  // Если спрайт выходит за пределы по X, сразу же удаляем его
  if X < -128 Then sengine2d.DelSprite( ID );
  // Если спрайт выходит за пределы по Y, ставим его в очередь на удаление
  if Y < -128 Then Destroy := TRUE;
  if Y > 600  Then Destroy := TRUE;
end;

procedure CMiku.OnFree;
begin
  inherited;
end;

// Добавить 100 спрайтов
procedure AddMiku;
  var
    i, ID : Integer;
begin
  for i := 1 to 100 do
    begin
      // Запрашиваем у спрайтового менеджера новое "место" под спрайт :)
      ID := sengine2d.AddSprite();
      // Создаем экземпляр спрайта CMiku. Аргументами конструктора являются
      // сам менеджер и будещий ID для спрайта
      sengine2d.List[ ID ] := CMiku.Create( sengine2d, ID );
    end;
end;

// Удалить 100 спрайтов
procedure DelMiku;
  var
    i : Integer;
begin
  // Удалим 100 спрайтов со случайным ID
  for i := 1 to 100 do
    sengine2d.DelSprite( random( sengine2d.Count ) );
end;

procedure Init;
  var
    i : Integer;
begin
  {$IFDEF DARWIN}
  dirRes := PChar( zgl_Get( APP_DIRECTORY ) ) + 'Contents/Resources/';
  {$ENDIF}

  texLogo := tex_LoadFromFile( dirRes + 'zengl.png', $FF000000, TEX_DEFAULT_2D );

  texMiku := tex_LoadFromFile( dirRes + 'miku.png', $FF000000, TEX_DEFAULT_2D );
  tex_SetFrameSize( texMiku, 128, 128 );

  // Создаем экземпляр zglCSEngine2D
  sengine2d := zglCSEngine2D.Create();
  // Создадим 1000 спрайтов Miku-chan :)
  for i := 0 to 9 do
    AddMiku();

  fntMain := font_LoadFromFile( dirRes + 'font.zfi' );
end;

procedure Draw;
begin
  batch2d_Begin();
  // Рисуем все спрайты находящиеся в текущем спрайтовом менеджере
  if time > 255 Then
    sengine2d.Draw();

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
      text_Draw( fntMain, 0, 0, 'FPS: ' + u_IntToStr( zgl_Get( SYS_FPS ) ) );
      text_Draw( fntMain, 0, 20, 'Sprites: ' + u_IntToStr( sengine2d.Count ) );
      text_Draw( fntMain, 0, 40, 'Up/Down - Add/Delete Miku :)' );
    end;
  batch2d_End();
end;

procedure Timer;
begin
  INC( time, 2 );

  // Выполняем обработку всех спрайтов в текущем спрайтовом менеджере
  sengine2d.Proc();
  // По нажатию пробела очистить все спрайты
  if key_Press( K_SPACE ) Then sengine2d.ClearAll();
  if key_Press( K_UP ) Then AddMiku();
  if key_Press( K_DOWN ) Then DelMiku();
  if key_Press( K_ESCAPE ) Then zgl_Exit();
  key_ClearState();
end;

procedure Quit;
begin
  // Очищаем память от созданных спрайтов
  sengine2d.ClearAll();
  sengine2d.Destroy();
end;

Begin
  randomize();

  timer_Add( @Timer, 16 );
  timer_Add( @AddMiku, 1000 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );
  zgl_Reg( SYS_EXIT, @Quit );

  // Т.к. модуль сохранен в кодировке UTF-8 и в нем используются строковые переменные
  // следует указать использования этой кодировки
  zgl_Enable( APP_USE_UTF8 );

  wnd_SetCaption( '07 - SEngine 2D(OOP)' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
