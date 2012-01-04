#define ZGL_IMPORT
#include "zglHeader.h"

bool		FullScreen;
zglPFont	fnt;
char*		something;
byte		lineAlpha;

void Init()
{
	// RU: Загрузка шрифта и вывод текста освещен в "04 - Text".
	// EN: Font loading and text rendering will be described in this demo - "04 - Text".
	fnt = font_LoadFromFile( "../data/font.zfi" );

	// RU: Начнем считывать текст с клавиатуры и ограничимся 20 символами.
	// EN: Start to read a text and set maximum count of symbols to 20.
	something = NULL;
	key_BeginReadText( something, 20 );

	// RU: Инициализируем поддержку джойстиков.
	// EN: Initialize joystick support.
	joy_Init();
}

void Draw()
{
	float w;

	text_Draw( fnt, 0, 0, "Escape - Exit" );
	text_Draw( fnt, 0, 20, "Alt+Enter - FullScreen/Windowed mode" );
	text_Draw( fnt, 0, 40, "Left mouse button - lock mouse :)" );

	text_Draw( fnt, 400, 300 - 100, "Enter something(maximum - 20 symbols):", TEXT_HALIGN_CENTER );
	text_Draw( fnt, 400, 300 - 70, something, TEXT_HALIGN_CENTER );
	w = text_GetWidth( fnt, something );
	pr2d_Rect( 400 + w / 2, 300 - 70, 10, 20, 0xFFFFFF, lineAlpha, PR2D_FILL );

	text_Draw( fnt, 400, 360, "JOYSTICK", TEXT_HALIGN_CENTER );

	// RU: Вывод состояния осей и кнопок первого джойстика в системе.
	// EN: Show the state of axes and buttons of first joystick in the system.
	char text[256];
	sprintf_s( text, "Axis X: %1.2f", joy_AxisPos( 0, JOY_AXIS_X ) );
	text_Draw( fnt, 100, 400, text, 0 );
	sprintf_s( text, "Axis Y: %1.2f", joy_AxisPos( 0, JOY_AXIS_Y ) );
	text_Draw( fnt, 100, 420, text, 0 );
	sprintf_s( text, "Axis Z: %1.2f", joy_AxisPos( 0, JOY_AXIS_Z ) );
	text_Draw( fnt, 100, 440, text, 0 );
	sprintf_s( text, "Axis R: %1.2f", joy_AxisPos( 0, JOY_AXIS_R ) );
	text_Draw( fnt, 100, 460, text, 0 );
	sprintf_s( text, "Axis U: %1.2f", joy_AxisPos( 0, JOY_AXIS_U ) );
	text_Draw( fnt, 100, 480, text, 0 );
	sprintf_s( text, "Axis V: %1.2f", joy_AxisPos( 0, JOY_AXIS_V ) );
	text_Draw( fnt, 100, 500, text, 0 );
	sprintf_s( text, "POVX: %1.2f", joy_AxisPos( 0, JOY_POVX ) );
	text_Draw( fnt, 100, 520, text, 0 );
	sprintf_s( text, "POVY: %1.2f", joy_AxisPos( 0, JOY_POVY ) );
	text_Draw( fnt, 100, 540, text, 0 );

	sprintf_s( text, "Button1: %s", joy_Down( 0, 0 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 400, 400, text, 0 );
	sprintf_s( text, "Button2: %s", joy_Down( 0, 1 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 400, 420, text, 0 );
	sprintf_s( text, "Button3: %s", joy_Down( 0, 2 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 400, 440, text, 0 );
	sprintf_s( text, "Button4: %s", joy_Down( 0, 3 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 400, 460, text, 0 );
	sprintf_s( text, "Button5: %s", joy_Down( 0, 4 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 400, 480, text, 0 );
	sprintf_s( text, "Button6: %s", joy_Down( 0, 5 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 400, 500, text, 0 );
	sprintf_s( text, "Button7: %s", joy_Down( 0, 6 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 400, 520, text, 0 );
	sprintf_s( text, "Button8: %s", joy_Down( 0, 7 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 400, 540, text, 0 );
	sprintf_s( text, "Button9: %s", joy_Down( 0, 8 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 550, 400, text, 0 );
	sprintf_s( text, "Button10: %s", joy_Down( 0, 9 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 550, 420, text, 0 );
	sprintf_s( text, "Button11: %s", joy_Down( 0, 10 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 550, 440, text, 0 );
	sprintf_s( text, "Button12: %s", joy_Down( 0, 11 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 550, 460, text, 0 );
	sprintf_s( text, "Button13: %s", joy_Down( 0, 12 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 550, 480, text, 0 );
	sprintf_s( text, "Button14: %s", joy_Down( 0, 13 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 550, 500, text, 0 );
	sprintf_s( text, "Button15: %s", joy_Down( 0, 14 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 550, 520, text, 0 );
	sprintf_s( text, "Button16: %s", joy_Down( 0, 15 ) ? "TRUE" : "FALSE" );
	text_Draw( fnt, 550, 540, text, 0 );
}

void Timer()
{
	lineAlpha -= 10;

	// RU: Если зажат Alt и был нажат Enter - переключиться в полноэкранный или оконный режим.
	// EN: If Alt+Enter was pressed - switch to fullscreen or windowed mode.
	if ( key_Down( K_ALT ) && key_Press( K_ENTER ) )
	{
		FullScreen = !FullScreen;
		scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FullScreen, FALSE );
	}
	// RU: По нажатию Escape завершить приложение.
	// EN: If Escape was pressed - shutdown the application.
	if ( key_Press( K_ESCAPE ) )
		zgl_Exit();

	// RU: Если зажата левая кнопка мыши - заблокируем мышку по центру экрана.
	// Смещения можно получать используя функции mouse_DX и mouse_DY вызывая их до mouse_Lock.
	// EN: If left mouse button is down - lock the mouse cursor in center of screen.
	// Delta can be obtained from functions mouse_DX and mouse_DY by calling them before mouse_Lock.
	if ( mouse_Down( M_BLEFT ) )
		mouse_Lock();

	// RU: "Считываем" в переменную введеный текст, но сначала чистим предыдущее содержимое.
	// EN: "Read" the text to variable, but free previos content first.
	if ( something )
		zgl_FreeMem( (void**)&something );
	something = key_GetText();

	// RU: Обязательно очищаем состояния.
	// EN: Necessarily clear all the states.
	key_ClearState();
	mouse_ClearState();
	joy_ClearState();
}

int CALLBACK WinMain (
	__in HINSTANCE hInstance,
	__in_opt HINSTANCE hPrevInstance,
	__in_opt LPSTR lpCmdLine,
	__in int nShowCmd
	)
{
	zglLoad( libZenGL );

	timer_Add( (void*)&Timer, 16 );

	zgl_Reg( SYS_LOAD, (void*)&Init );
	zgl_Reg( SYS_DRAW, (void*)&Draw );

	// RU: Т.к. модуль сохранен в кодировке UTF-8 и в нем используются строковые переменные
	// следует указать использование этой кодировки.
	// EN: Enable using of UTF-8, because this unit saved in UTF-8 encoding and here used
	// string variables.
	zgl_Enable( APP_USE_UTF8 );

	wnd_SetCaption( "02 - Input" );

	wnd_ShowCursor( TRUE );

	scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

	zgl_Init( 0, 0 );

	zglFree();
	return 0;
}
