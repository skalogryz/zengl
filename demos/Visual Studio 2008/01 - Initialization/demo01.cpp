#include "zglHeader.h"

char* DirApp;
char* DirHome;

void Init()
{
	// RU: Тут можно выполнять загрузку основных ресурсов.
	// EN: Here can be loading of main resources.
}

void Draw()
{
	// RU: Тут "рисуем" что угодно :)
	// EN: Here "draw" anything :)
}

void Update( double dt )
{
	// RU: Эта функция наземенима для реализация плавного движения чего-либо, т.к. таймеры зачастую ограничены FPS.
	// EN: This function is the best way to implement smooth moving of something, because timers are restricted by FPS.
}

void Timer()
{
	// RU: Будем в заголовке показывать количество кадров в секунду.
	// EN: Caption will show the frames per second.
	char caption[256];
	sprintf_s( caption, "01 - Initialization[ FPS: %i ]", (int)zgl_Get( RENDER_FPS ) );
	wnd_SetCaption( caption );
}

void Quit()
{
	//
}

int CALLBACK WinMain (
	__in HINSTANCE hInstance,
	__in_opt HINSTANCE hPrevInstance,
	__in_opt LPSTR lpCmdLine,
	__in int nShowCmd
	)
{
	zglLoad( libZenGL );

	// RU: Для загрузки/создания каких-то своих настроек/профилей/etc. можно получить путь к
	// домашенему каталогу пользователя, или к исполняемому файлу(не работает для GNU/Linux).
	// Ни в коем случаи не следует чистить память, т.к. данные char* хранится на стороне ZenGL.
	//
	// EN: For loading/creating your own options/profiles/etc. you can get path to user home
	// directory, or to executable file(not works for GNU/Linux).
	// No need to free memory, because these char* stored on ZenGL side.
	DirApp  = (char*)zgl_Get( DIRECTORY_APPLICATION );
	DirHome = (char*)zgl_Get( DIRECTORY_HOME );

	// RU: Создаем таймер с интервалом 1000мс.
	// EN: Create a timer with interval 1000ms.
	timer_Add( (void*)&Timer, 1000 );

	// RU: Регистрируем функцию, что выполнится сразу после инициализации ZenGL.
	// EN: Register the function, that will be executed after ZenGL initialization.
	zgl_Reg( SYS_LOAD, (void*)&Init );
	// RU: Регистрируем функцию, где будет происходить рендер.
	// EN: Register the render function.
	zgl_Reg( SYS_DRAW, (void*)&Draw );
	// RU: Регистрируем функцию, которая будет принимать разницу времени между кадрами.
	// EN: Register the function, that will get delta time between the frames.
	zgl_Reg( SYS_UPDATE, (void*)&Update );
	// RU: Регистрируем функцию, которая выполнится после завершения работы ZenGL.
	// EN: Register the function, that will be executed after ZenGL shutdown.
	zgl_Reg( SYS_EXIT, (void*)&Quit );

	// RU: Т.к. модуль сохранен в кодировке UTF-8 и в нем используются строковые переменные
	// следует указать использование этой кодировки.
	// EN: Enable using of UTF-8, because this unit saved in UTF-8 encoding and here used
	// string variables.
	zgl_Enable( APP_USE_UTF8 );

	// RU: Устанавливаем заголовок окна.
	// EN: Set the caption of the window.
	wnd_SetCaption( "01 - Initialization" );

	// RU: Разрешаем курсор мыши.
	// EN: Allow to show the mouse cursor.
	wnd_ShowCursor( TRUE );

	// RU: Указываем первоначальные настройки.
	// EN: Set screen options.
	scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

	// RU: Инициализируем ZenGL.
	// EN: Initialize ZenGL.
	zgl_Init();

	zglFree();
	return 0;
}
