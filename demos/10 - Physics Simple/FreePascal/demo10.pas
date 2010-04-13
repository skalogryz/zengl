program demo10;

uses
  zglChipmunk,     // Собственно сам модуль Chipmunk'а
  zglChipmunkDraw, // Вспомогательный модуль, который отвечает за вывод на экран каркасов физ.
                   // объектов и точки соприкосновения

  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_mouse,
  zgl_textures,
  zgl_textures_png,
  zgl_render_2d,
  zgl_font,
  zgl_text,
  zgl_primitives_2d,
  zgl_math_2d,
  zgl_utils;

var
  fnt    : zglPFont;
  space  : PcpSpace;
  bCount : Integer;
  Bodies : array of PcpBody;
  Shapes : array of PcpShape;

// Добавить объект "шар"
// x, y - координаты центра
// mass - масса
// r    - радиус
// e    - коэффициент эластичности
// u    - коэффициент трения
procedure cpAddBall( const x, y, r, mass, e, u : Single );
begin
  INC( bCount );
  SetLength( Bodies, bCount );
  SetLength( Shapes, bCount );

  Bodies[ bCount - 1 ]   := cpBodyNew( mass, cpMomentForCircle( mass, 0, r, cpvzero ) );
  Bodies[ bCount - 1 ].p := cpv( x, y );
  cpSpaceAddBody( space, Bodies[ bCount - 1 ] );

  Shapes[ bCount - 1 ]   := cpCircleShapeNew( Bodies[ bCount - 1 ], r, cpvzero );
  Shapes[ bCount - 1 ].e := e;
  Shapes[ bCount - 1 ].u := u;
  cpSpaceAddShape( space, Shapes[ bCount - 1 ] );
end;

// Добавить объект "коробка"
// Схож с функцией cppAddBall по параметрам
// x, y - координаты центра
// w, h - ширина и высота
procedure cpAddBox( const x, y, w, h, mass, e, u : single);
  var
    points : array[ 0..3 ] of cpVect;
begin
  INC( bCount );
  SetLength( Bodies, bCount );
  SetLength( Shapes, bCount );

  points[ 0 ].x := - w / 2;
  points[ 0 ].y := - h / 2;
  points[ 1 ].x := - w / 2;
  points[ 1 ].y := h / 2;
  points[ 2 ].x := w / 2;
  points[ 2 ].y := h / 2;
  points[ 3 ].x := w / 2;
  points[ 3 ].y := - h / 2;

  Bodies[ bCount - 1 ]   := cpBodyNew( mass, cpMomentForPoly( mass, 4, @points[ 0 ], cpvzero ) );
  Bodies[ bCount - 1 ].p := cpv( x + w / 2, y + h / 2 );
  cpSpaceAddBody( space, Bodies[ bCount - 1 ] );

  Shapes[ bCount - 1 ]   := cpPolyShapeNew( Bodies[ bCount - 1 ], 4, @points[ 0 ], cpvzero );
  Shapes[ bCount - 1 ].e := e;
  Shapes[ bCount - 1 ].u := u;
  cpSpaceAddShape( space, Shapes[ bCount - 1 ] );
end;

procedure Init;
  var
    staticBody : PcpBody;
    ground     : PcpShape;
    e, u       : Single;
begin
  fnt := font_LoadFromFile( '../res/font.zfi' );
  fnt.Pages[ 0 ] := tex_LoadFromFile( '../res/font_0.png', $FF000000, TEX_DEFAULT_2D );

  // Создаем новый "мир"
  space            := cpSpaceNew;
  // Задаем количество итераций обработки(рекомендуется 10)
  space.iterations := 10;
  space.elasticIterations := 10;
  // Задаем силу гравитацию
  space.gravity    := cpv( 0, 256 );
  // Задаем коэффициент "затухания" движения объектов
  space.damping    := 0.9;

  e := 1;
  u := 0.9;
  // Создадим статичное "тело"
  staticBody := cpBodyNew( INFINITY, INFINITY );
  // Добавим три отрезка для ограничения мира
  // первый параметр - указатель на созданное тело
  // два последующих - координаты точек отрезка
  // последний - толщина отрезка
  ground := cpSegmentShapeNew( staticBody, cpv( 5, 0 ), cpv( 5, 590 ), 1 );
  ground.e := e;
  ground.u := u;
  cpSpaceAddStaticShape( space, ground );
  ground := cpSegmentShapeNew( staticBody, cpv( 795, 0 ), cpv( 795, 590 ), 1 );
  ground.e := e;
  ground.u := u;
  cpSpaceAddStaticShape( space, ground );
  ground := cpSegmentShapeNew( staticBody, cpv( 0, 590 ), cpv( 800, 590 ), 1 );
  ground.e := e;
  ground.u := u;
  cpSpaceAddStaticShape( space, ground );
  // Добавим треугольник
  staticBody := cpBodyNew( INFINITY, INFINITY );
  ground := cpSegmentShapeNew( staticBody, cpv( 400, 300 ), cpv( 200, 350 ), 1 );
  ground.e := e;
  ground.u := u;
  cpSpaceAddStaticShape( space, ground );
  ground := cpSegmentShapeNew( staticBody, cpv( 200, 350 ), cpv( 700, 350 ), 1 );
  ground.e := e;
  ground.u := u;
  cpSpaceAddStaticShape( space, ground );
  ground := cpSegmentShapeNew( staticBody, cpv( 700, 350 ), cpv( 400, 300 ), 1 );
  ground.e := e;
  ground.u := u;
  cpSpaceAddStaticShape( space, ground );
end;

procedure Draw;
begin
  batch2d_Begin;

  // Функция вывода на экран каркасов физ. объектов
  // Второй аргумент функции отвечает за показ точек соприкосновения
  cpDrawSpace( space, TRUE );

  text_Draw( fnt, 10, 5,  'FPS: ' + u_IntToStr( zgl_Get( SYS_FPS ) ) );
  text_Draw( fnt, 10, 25, 'Shapes: ' + u_IntToStr( cpTotalShapes ) );
  text_Draw( fnt, 10, 45, 'Use your mouse: Left Click - box, Right Click - ball' );
  batch2d_End;
end;

procedure Proc;
begin
  if mouse_Click( M_BLEFT ) Then
    cpAddBox( mouse_X - 10, mouse_Y - 10, 48, 32, 1, 0.5, 0.5 );
  if mouse_Click( M_BRIGHT ) Then
    cpAddBall( mouse_X, mouse_Y, 16, 1, 0.5, 0.9 );
  if key_Press( K_ESCAPE ) Then zgl_Exit;

  key_ClearState;
  mouse_ClearState;
end;

procedure Update( dt : Double );
begin
  cpSpaceStep( space, 1 / ( 1000 / dt ) );
end;

procedure Exit;
begin
  // Очистка
  cpSpaceFreeChildren( space );
  cpSpaceFree( space );
end;

Begin
  randomize;
  timer_Add( @Proc, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );
  zgl_Reg( SYS_UPDATE, @Update );
  zgl_Reg( SYS_EXIT, @Exit );

  // Т.к. модуль сохранен в кодировке UTF-8 и в нем используются строковые переменные
  // следует указать использования этой кодировки
  zgl_Enable( APP_USE_UTF8 );

  wnd_SetCaption( '10 - Physics Simple' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init;
End.
