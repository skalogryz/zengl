#include <math.h>
#include "zglHeader.h"

zglPFont		fntMain;
zglPTexture		texLogo;
zglPTexture		texMiku;
int				time;
zglTSEngine2D	sengine2d;

// Miku
void MikuInit( zglPSprite2D Sprite )
{
	Sprite->X = 800.0f + rand() % 800;
	Sprite->Y = (float)( rand() % ( 600 - 128 ) );
	// RU: Задаем скорость движения. В пользовательском параметре Data выделим память под структуру zglTPoint2D.
	// EN: Set the moving speed. Allocate memory for structure zglTPoint2D in userspace parameter "Data".
	zgl_GetMem( &Sprite->Data, sizeof( zglTPoint2D ) );
	((zglPPoint2D)Sprite->Data)->X = -( rand() % 10 ) / 5.0f - 0.5f;
	((zglPPoint2D)Sprite->Data)->Y = ( rand() % 10 - 5 ) / 5.0f;
}

void MikuDraw( zglPSprite2D Sprite )
{
	asprite2d_Draw( Sprite->Texture, Sprite->X, Sprite->Y, Sprite->W, Sprite->H, Sprite->Angle, (ushort)Sprite->Frame, Sprite->Alpha, Sprite->FxFlags );
}

void MikuProc( zglPSprite2D Sprite )
{
	zglPPoint2D speed;

	speed = (zglPPoint2D)Sprite->Data;

	Sprite->X += speed->X;
	Sprite->Y += speed->Y;
	Sprite->Frame += ( abs( speed->X ) + abs( speed->Y ) ) / 25.0f;
	if ( Sprite->Frame > 8 )
		Sprite->Frame = 1;
	// RU: Если спрайт выходит за пределы по X, сразу же удаляем его.
	// EN: Delete the sprite if it goes beyond X.
	if ( Sprite->X < -128 )
		sengine2d_DelSprite( Sprite->ID );
	// RU: Если спрайт выходит за пределы по Y, ставим его в очередь на удаление.
	// EN: Add sprite to queue for delete if it goes beyond Y.
	if ( Sprite->Y < -128 )
		Sprite->Destroy = TRUE;
	if ( Sprite->Y > 600 )
		Sprite->Destroy = TRUE;
}

void MikuFree( zglPSprite2D Sprite )
{
	// RU: Очистим ранее выделенную память.
	// EN: Free the memory allocated for Data.
	zgl_FreeMem( &Sprite->Data );
}

// RU: Добавить 100 спрайтов.
// EN: Add 100 sprites.
void AddMiku()
{
	// RU: При добавлении спрайта в менеджер спрайтов указывается текстура, слой(положение по Z) и
	// указатели на основные функции - Инициализация, Рендер, Обработка и Уничтожение.
	// EN: For adding sprite to sprite engine must be set next parameters: texture, layer(Z-coordinate) and
	// pointers to Initialization, Render, Process and Destroy functions.
	for ( int i = 1; i <= 100; i++ )
		sengine2d_AddSprite( texMiku, rand() % 10, (zglSpriteFunc)&MikuInit, (zglSpriteFunc)&MikuDraw, (zglSpriteFunc)&MikuProc, (zglSpriteFunc)&MikuFree );
}

// RU: Удалить 100 спрайтов.
// EN: Delete 100 sprites.
void DelMiku()
{
	if ( sengine2d.Count < 100 )
		return;

	// RU: Удалим 100 спрайтов со случайным ID.
	// EN: Delete 100 sprites with random ID.
	for ( int i = 1; i <= 100; i++ )
		sengine2d_DelSprite( rand() % sengine2d.Count );
}

void Init()
{
	texLogo = tex_LoadFromFile( "../data/zengl.png" );

	texMiku = tex_LoadFromFile( "../data/miku.png" );
	tex_SetFrameSize( &texMiku, 128, 128 );

	// RU: Устанавливаем текущим менеджером спрайтов свой.
	// EN: Set own sprite engine as current.
	sengine2d_Set( &sengine2d );
	// RU: Создадим 1000 спрайтов Miku-chan :)
	// EN: Create 1000 sprites of Miku-chan :)
	for ( int i = 0; i < 10; i++ )
		AddMiku();

	fntMain = font_LoadFromFile( "../data/font.zfi" );
}

void Draw()
{
	batch2d_Begin();
	// RU: Рисуем все спрайты находящиеся в текущем спрайтовом менеджере.
	// EN: Render all sprites contained in current sprite engine.
	if ( time > 255 )
		sengine2d_Draw();

	if ( time <= 255 )
		ssprite2d_Draw( texLogo, 400 - 256, 300 - 128, 512, 256, 0, time );
	else if ( time < 510 )
	{
		pr2d_Rect( 0, 0, 800, 600, 0x000000, 510 - time, PR2D_FILL );
		ssprite2d_Draw( texLogo, 400 - 256, 300 - 128, 512, 256, 0, 510 - time );
	}

	if ( time > 255 )
	{
		char text[256];

		pr2d_Rect( 0, 0, 256, 64, 0x000000, 200, PR2D_FILL );
		sprintf_s( text, "FPS: %i", zgl_Get( RENDER_FPS ) );
		text_Draw( fntMain, 0, 0, text );
		sprintf_s( text, "Sprites: %i", sengine2d.Count );
		text_Draw( fntMain, 0, 20, text );
		text_Draw( fntMain, 0, 40, "Up/Down - Add/Delete Miku :)" );
	}
	batch2d_End();
}

void Timer()
{
	time +=2;

	// RU: Выполняем обработку всех спрайтов в текущем спрайтовом менеджере.
	// EN: Process all sprites contained in current sprite engine.
	sengine2d_Proc();
	// RU: По нажатию пробела очистить все спрайты.
	// EN: Delete all sprites if space was pressed.
	if ( key_Press( K_SPACE ) ) sengine2d_ClearAll();
	if ( key_Press( K_UP ) ) AddMiku();
	if ( key_Press( K_DOWN ) ) DelMiku();
	if ( key_Press( K_ESCAPE ) ) zgl_Exit();
	key_ClearState();
}

void Quit()
{
	// RU: Очищаем память от созданных спрайтов.
	// EN: Free allocated memory for sprites.
	sengine2d_Set( &sengine2d );
	sengine2d_ClearAll();
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
	timer_Add( (void*)&AddMiku, 1000 );

	zgl_Reg( SYS_LOAD, (void*)&Init );
	zgl_Reg( SYS_DRAW, (void*)&Draw );
	zgl_Reg( SYS_EXIT, (void*)&Quit );

	// RU: Т.к. модуль сохранен в кодировке UTF-8 и в нем используются строковые переменные
	// следует указать использование этой кодировки.
	// EN: Enable using of UTF-8, because this unit saved in UTF-8 encoding and here used
	// string variables.
	zgl_Enable( APP_USE_UTF8 );

	wnd_SetCaption( "06 - SEngine 2D" );

	wnd_ShowCursor( TRUE );

	scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

	zgl_Init();

	zglFree();
	return 0;
}
