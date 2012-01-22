#define ZGL_IMPORT
#include "zglHeader.h"

int calc;
zglTPoint2D points[360];

void Init()
{
  for ( int i = 0; i < 360; i++ )
  {
    points[ i ].X = 400 + m_Cos( i ) * ( 96 + rand() % 32 );
    points[ i ].Y = 300 + m_Sin( i ) * ( 96 + rand() % 32 );
  }
}

void Draw()
{
  // RU: Устанавливаем цвет и альфу для каждой вершины.
  // EN: Set color and alpha for each vertex.
  fx2d_SetVCA( 0xFF0000, 0x00FF00, 0x0000FF, 0xFFFFFF, 255, 255, 255, 255 );
  // RU: Рисуем прямоугольник с заливкой(флаг PR2D_FILL) с использованием отдельных цветов для каждой вершины(флаг FX2D_VCA).
  // EN: Render filled rectangle(flag PR2D_FILL) and use different colors for each vertex(flag FX2D_VCA).
  pr2d_Rect( 0, 0, 800, 600, 0x000000, 255, FX2D_VCA | PR2D_FILL );

  // RU: Рисуем в центре экрана круг с радиусом 128 пиксела.
  // EN: Render circle in center of screen with radius 128 pixels.
  pr2d_Circle( 400, 300, 128, 0x000000, 155, 32, PR2D_FILL );

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
  calc++;
  if ( calc > 359 )
    calc = 0;
  points[ calc ].X = 400 + m_Cos( calc ) * ( 96 + rand() % 32 );
  points[ calc ].Y = 300 + m_Sin( calc ) * ( 96 + rand() % 32 );
  // RU: Рисуем линии внутри круга.
  // EN: Render lines inside the cricle.
  for ( int i = 0; i < 360; i++ )
    pr2d_Line( 400, 300, points[ i ].X, points[ i ].Y, 0xFFFFFF, 255, 0 );
  batch2d_End();

  // RU: Рисуем эллипсы с заливкой и без, со сглаженными контурами(флаг PR2D_SMOOTH).
  // EN: Render filled ellipses with smoothed edges(flag PR2D_SMOOTH).
  pr2d_Ellipse( 400 + 300, 300, 64, 256, 0xFFFFFF, 55, 32, PR2D_FILL | PR2D_SMOOTH );
  pr2d_Ellipse( 400 + 300, 300, 64, 256, 0x000000, 255, 32, PR2D_SMOOTH );

  pr2d_Ellipse( 400 - 300, 300, 64, 256, 0xFFFFFF, 55, 32, PR2D_FILL | PR2D_SMOOTH );
  pr2d_Ellipse( 400 - 300, 300, 64, 256, 0x000000, 255, 32, PR2D_SMOOTH );
}

void Timer()
{
  char caption[256];
  sprintf_s( caption, "3 - Primitives 2D[ FPS: %i ]", (int)zgl_Get( RENDER_FPS ) );
  wnd_SetCaption( caption );
}

void Proc()
{
  if ( key_Press( K_ESCAPE ) )
    zgl_Exit();
  key_ClearState();
}

int main()
{
  zglLoad( libZenGL );

  srand( 0xDeaDBeeF );

  timer_Add( (void*)&Proc, 16, FALSE, NULL );
  timer_Add( (void*)&Timer, 1000, FALSE, NULL );

  zgl_Reg( SYS_LOAD, (void*)&Init );
  zgl_Reg( SYS_DRAW, (void*)&Draw );

  wnd_SetCaption( "03 - Primitives 2D" );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init( 0, 0 );

  zglFree();
  return 0;
}
