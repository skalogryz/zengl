program demo03;

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
  zgl_primitives_2d,
  zgl_math_2d,
  zgl_utils
  {$ENDIF}
  ;

var
  calc   : Integer;
  points : array[ 0..359 ] of zglTPoint2D;

procedure Init;
  var
    i : Integer;
begin
  for i := 0 to 359 do
    begin
      points[ i ].X := 400 + m_Cos( i ) * ( 96 + random( 32 ) );
      points[ i ].Y := 300 + m_Sin( i ) * ( 96 + random( 32 ) );
    end;
end;

procedure Draw;
  var
    i : Integer;
begin
  // RU: Устанавливаем цвет и альфу для каждой вершины.
  // EN: Set color and alpha for each vertex.
  fx2d_SetVCA( $FF0000, $00FF00, $0000FF, $FFFFFF, 255, 255, 255, 255 );
  // RU: Рисуем прямоугольник с заливкой(флаг PR2D_FILL) с использованием отдельных цветов для каждой вершины(флаг FX2D_VCA).
  // EN: Render filled rectangle(flag PR2D_FILL) and use different colors for each vertex(flag FX2D_VCA).
  pr2d_Rect( 0, 0, 800, 600, $000000, 255, FX2D_VCA or PR2D_FILL );

  // RU: Рисуем в центре экрана круг с радиусом 128 пиксела.
  // EN: Render circle in center of screen with radius 128 pixels.
  pr2d_Circle( 400, 300, 128, $000000, 155, 32, PR2D_FILL );

  // RU: Т.к. далее идет вывод однотипных примитивов(линий) следует воспользоваться оптимизацией по количеству DIP'ов.
  // Не обязательно заключать в batch2d_Begin/batch2d_End только подобные участки рендера, вполне можно заключить в них всю Draw функцию.
  // Учитывая, что данный пример довольно простой в плане нагрузки на видеокарту, вполне возможно что с batch2d функциями FPS будет ниже :)
  // Дело в том, что при высоких FPS'ах ограничителем становится процессор, время которого тратится на проверки и пр.
  //
  // EN: Because code below is render one type of primitive(lines) it's better to enable DIP-optiomization.
  // No need to conclude only this code in batch2d_Begin/batch2d_End. Whole Draw function can be concluded.
  // Because this example is too easy for videocard, maybe FPS will be smaller with batch2d functions :)
  // The fact is that with high FPS's - bottleneck is CPU.
  batch2d_Begin();
  INC( calc );
  if calc > 359 Then calc := 0;
  points[ calc ].X := 400 + m_Cos( calc ) * ( 96 + random( 32 ) );
  points[ calc ].Y := 300 + m_Sin( calc ) * ( 96 + random( 32 ) );
  // RU: Рисуем линии внутри круга.
  // EN: Render lines inside the cricle.
  for i := 0 to 359 do
    pr2d_Line( 400, 300, points[ i ].X, points[ i ].Y, $FFFFFF, 255 );
  batch2d_End();

  // RU: Рисуем эллипсы с заливкой и без, со сглаженными контурами(флаг PR2D_SMOOTH).
  // EN: Render filled ellipses with smoothed edges(flag PR2D_SMOOTH).
  pr2d_Ellipse( 400 + 300, 300, 64, 256, $FFFFFF, 55, 32, PR2D_FILL or PR2D_SMOOTH );
  pr2d_Ellipse( 400 + 300, 300, 64, 256, $000000, 255, 32, PR2D_SMOOTH );

  pr2d_Ellipse( 400 - 300, 300, 64, 256, $FFFFFF, 55, 32, PR2D_FILL or PR2D_SMOOTH );
  pr2d_Ellipse( 400 - 300, 300, 64, 256, $000000, 255, 32, PR2D_SMOOTH );
end;

procedure Timer;
begin
  wnd_SetCaption( '03 - Primitives 2D[ FPS: ' + u_IntToStr( zgl_Get( RENDER_FPS ) ) + ' ]' );
end;

procedure Proc;
begin
  if key_Press( K_ESCAPE ) Then zgl_Exit();
  key_ClearState();
end;

Begin
  {$IFNDEF STATIC}
  zglLoad( libZenGL );
  {$ENDIF}

  randomize();

  timer_Add( @Proc, 16 );
  timer_Add( @Timer, 1000 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  wnd_SetCaption( '03 - Primitives 2D' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
