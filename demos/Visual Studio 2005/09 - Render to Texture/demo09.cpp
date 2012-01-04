#define ZGL_IMPORT
#include "zglHeader.h"

zglPFont			fntMain;
zglPTexture			texTux;
zglPRenderTarget	rtarget;

void Init()
{
	texTux = tex_LoadFromFile( "../data/tux_stand.png" );
	tex_SetFrameSize( &texTux, 64, 64 );

	fntMain = font_LoadFromFile( "../data/font.zfi" );

	// RU: Создаем RenderTarget и "цепляем" пустую текстуру. В процессе текстуру можно сменить присвоив
	// rtarget.Surface другую zglPTexture, главное что бы совпадали размеры с теми, что указаны в
	// tex_CreateZero. Таргету также указан флаг RT_FULL_SCREEN, отвечающий за то, что бы в текстуру
	// помещалось все содержимое экрана а не область 512x512(как с флагом RT_DEFAULT).
	//
	// EN: Create a RenderTarget and "bind" empty texture to it. Later texture can be changed by changing
	// rtarget.Surface to another zglPTexture, the only requirement - the same size of textures, that was
	// set in tex_CreateZero. Also target use flag RT_FULL_SCREEN that responsible for rendering whole
	// content of screen to target, not only region 512x512(like with flag RT_DEFAULT).
	rtarget = rtarget_Add( tex_CreateZero( 512, 512 ), RT_FULL_SCREEN );
}

void Draw()
{
	// RU: Устанавливаем текущий RenderTarget.
	// EN: Set current RenderTarget.
	rtarget_Set( rtarget );
	// RU: Рисуем в него
	// EN: Render to it.
	asprite2d_Draw( texTux, float( rand() % ( 800 - 64 ) ), float( rand() % ( 600 - 64 ) ), 64.0f, 64.0f, 0, rand() % 9 + 1 );
	// RU: Возвращаемся к обычному рендеру.
	// EN: Return to default rendering.
	rtarget_Set( NULL );

	// RU: Теперь рисуем содержимое RenderTarget'а.
	// EN: Render content of RenderTarget.
	ssprite2d_Draw( rtarget->Surface, ( 800 - 512 ) / 2.0f, ( 600 - 512 ) / 2.0f, 512.0f, 512.0f, 0 );

	char text[256];
	sprintf_s( text, "FPS: %i", zgl_Get( RENDER_FPS ) );
	text_Draw( fntMain, 0, 0, text );
}

void Timer()
{
	if ( key_Press( K_ESCAPE ) ) zgl_Exit();
	key_ClearState();
}

int CALLBACK WinMain (
	__in HINSTANCE hInstance,
	__in_opt HINSTANCE hPrevInstance,
	__in_opt LPSTR lpCmdLine,
	__in int nShowCmd
	)
{
	zglLoad( libZenGL );

	srand( 0xDeaDBeeF );

	timer_Add( (void*)&Timer, 16 );

	zgl_Reg( SYS_LOAD, (void*)&Init );
	zgl_Reg( SYS_DRAW, (void*)&Draw );

	// RU: Т.к. модуль сохранен в кодировке UTF-8 и в нем используются строковые переменные
	// следует указать использование этой кодировки.
	// EN: Enable using of UTF-8, because this unit saved in UTF-8 encoding and here used
	// string variables.
	zgl_Enable( APP_USE_UTF8 );

	wnd_SetCaption( "09 - Render to Texture" );

	wnd_ShowCursor( TRUE );

	scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

	zgl_Init();

	zglFree();
	return 0;
}
