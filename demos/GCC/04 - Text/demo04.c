#include "zglHeader.h"

zglPFont fnt;

void Init()
{
  // RU: Загружаем данные о шрифте.
  // EN: Load the font.
  fnt = font_LoadFromFile( "../data/font.zfi" );
  // RU: Если же текстуры именуются без использования маски вида "FontName-pageN.ext", то загрузку можно произвести вручную следующим образом:
  // EN: If textures were named without special mask - "FontName-pageN.ext", then it can be loaded this way:
  //for i := 0 to fnt.Count.Pages - 1 do
  //  fnt.Pages[ i ] := tex_LoadFromFile( dirRes + 'font-page' + u_IntToStr( i ) + '.png', $FF000000, TEX_DEFAULT_2D );
}

void Draw()
{
  zglTRect r;
  char s[256];

  batch2d_Begin();
  text_Draw( fnt, 400, 25, "Строка с выравниванием по центру", TEXT_HALIGN_CENTER );
  text_DrawEx( fnt, 400, 65, 2, 0, "Масштабирование", 255, 0xFFFFFF, TEXT_HALIGN_CENTER );
  fx2d_SetVCA( 0xFF0000, 0x00FF00, 0x0000FF, 0xFFFFFF, 255, 255, 255, 255 );
  text_Draw( fnt, 400, 125, "Градация цвета для каждого символа", TEXT_FX_VCA | TEXT_HALIGN_CENTER );

  r.X = 0;
  r.Y = 300 - 128;
  r.W = 192;
  r.H = 256;
  text_DrawInRect( fnt, r, "Обычный вывод текста в прямоугольнике", 0 );
  pr2d_Rect( r.X, r.Y, r.W, r.H, 0xFF0000, 255, 0 );

  r.X = 800 - 192;
  r.Y = 300 - 128;
  r.W = 192;
  r.H = 256;
  text_DrawInRect( fnt, r, "Вывод текста используя выравнивание по правому краю и размещение снизу", TEXT_HALIGN_RIGHT | TEXT_VALIGN_BOTTOM );
  pr2d_Rect( r.X, r.Y, r.W, r.H, 0xFF0000, 255, 0 );

  r.X = 400 - 192;
  r.Y = 300 - 128;
  r.W = 384;
  r.H = 256;
  text_DrawInRect( fnt, r, "Этот текст использует выравнивание по ширине и центрируется по вертикали. Текст, который не помещается в пределах прямоугольника будет отсечен.", TEXT_HALIGN_JUSTIFY | TEXT_VALIGN_CENTER );
  pr2d_Rect( r.X, r.Y, r.W, r.H, 0xFF0000, 255, 0 );

  r.X = 400 - 320;
  r.Y = 300 + 160;
  r.W = 640;
  r.H = 128;
  text_DrawInRect( fnt, r, "Для переноса строк можно использовать LF-символ\nкод которого равен 10 и обозначен в таблице Unicode как \"Line Feed\"", TEXT_HALIGN_CENTER | TEXT_VALIGN_CENTER );
  pr2d_Rect( r.X, r.Y, r.W, r.H, 0xFF0000, 255, 0 );

  // RU: Выводим количество FPS в правом углу, используя text_GetWidth.
  // EN: Render frames per second in the top right corner using text_GetWidth.
  sprintf_s( s, "FPS: %i", (int)zgl_Get( RENDER_FPS ) );
  text_Draw( fnt, 800 - text_GetWidth( fnt, s, 0 ), 0, s, 0 );
  batch2d_End();
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

  timer_Add( (void*)&Proc, 16 );

  zgl_Reg( SYS_LOAD, (void*)&Init );
  zgl_Reg( SYS_DRAW, (void*)&Draw );

  // RU: Т.к. модуль сохранен в кодировке UTF-8 и в нем используются строковые переменные
  // следует указать использование этой кодировки.
  // EN: Enable using of UTF-8, because this unit saved in UTF-8 encoding and here used
  // string variables.
  zgl_Enable( APP_USE_UTF8 );

  wnd_SetCaption( "04 - Text" );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init( 0, 0 );

  zglFree();
  return 0;
}
