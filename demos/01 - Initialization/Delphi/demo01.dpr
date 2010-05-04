program demo01;

// Приложение можно собрать с ZenGL статично, либо использовать so/dll/dylib.
// Для этого закомментируйте объявление ниже. Преимущество статичной компиляции
// заключается в меньшем размере, но требует подключение каждого модуля вручную
// Также статическая компиляция обязывает исполнять условия LGPL-лицензии,
// в частности требуется открытие исходных кодов приложения, которое использует
// исходные коды ZenGL. Использование же только so/dll/dylib этого не требует.
{$DEFINE STATIC}

uses
  {$IFDEF STATIC}
  // Перед использованием модулей, не забудьте указать путь к исходным кодам ZenGL :)
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_utils
  {$ELSE}
  zglHeader
  {$ENDIF}
  ;

var
  DirApp  : AnsiString;
  DirHome : AnsiString;

procedure Init;
begin
  // Тут можно выполнять загрузку основных ресурсов
end;

procedure Draw;
begin
  // Тут "рисуем" что угодно :)
end;

procedure Update( dt : Double );
begin
  //
end;

procedure Timer;
begin
  // Будем в заголовке показывать количество кадров в секунду
  wnd_SetCaption( '01 - Initialization[ FPS: ' + u_IntToStr( zgl_Get( SYS_FPS ) ) + ' ]' );
end;

procedure Quit;
begin
 //
end;

Begin
  {$IFNDEF STATIC}
    zglLoad( 'ZenGL.dll' );
  {$ENDIF}

  // Для загрузки/создания каких-то своих настроек/профилей/etc. можно получить путь к
  // домашеней директории пользователя, или к исполняемому файлу
  DirApp  := PAnsiChar( zgl_Get( APP_DIRECTORY ) );
  DirHome := PAnsiChar( zgl_Get( USR_HOMEDIR ) );

  // Создаем таймер с интервалом 1000мс.
  timer_Add( @Timer, 1000 );

  // Регистрируем процедуру, что выполнится сразу после инициализации ZenGL
  zgl_Reg( SYS_LOAD, @Init );
  // Регистрируем процедуру, где будет происходить рендер
  zgl_Reg( SYS_DRAW, @Draw );
  // Регистрируем процедуру, которая будет принимать разницу времени между кадрами
  zgl_Reg( SYS_UPDATE, @Update );
  // Регистрируем процедуру, которая выполнится после завершения работы ZenGL
  zgl_Reg( SYS_EXIT, @Quit );

  // Устанавливаем заголовок окна
  wnd_SetCaption( '01 - Initialization' );

  // Разрешаем курсор мыши
  wnd_ShowCursor( TRUE );

  // Указываем первоначальные настройки
  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  // Инициализируем ZenGL
  zgl_Init;
End.
