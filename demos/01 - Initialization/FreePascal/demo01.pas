program demo01;

// Приложение можно собрать с ZenGL статично, либо использовать so/dll/dylib.
// Для этого закомментируйте объявление ниже. Преимущество статичной компиляции
// заключается в меньшем размере, но требует подключение каждого модуля вручную
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
  DirApp  : String;
  DirHome : String;

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
    {$IFDEF LINUX}
    // В Linux все библиотеки принято хранить в /usr/lib, поэтому libZenGL.so должна
    // быть предварительно установлена. Если же нужно грузить библиотеку из
    // директории с исполняемым файлом то следует вписать './libZenGL.so'
    zglLoad( 'libZenGL.so' );
    {$ENDIF}
    {$IFDEF WIN32}
    zglLoad( 'ZenGL.dll' );
    {$ENDIF}
    {$IFDEF DARWIN}
    // libZenGL.dylib следует предварительно поместить в директорию
    // MyApp.app/Contents/Frameworks/, где MyApp.app - Bundle вашего приложения
    // Также следует упомянуть, что лог-файл будет создаваться в корневой директории
    // поэтому либо отключайте его, либо указывайте свой путь и имя, как описано в справке
    zglLoad( 'libZenGL.dylib' );
    {$ENDIF}
  {$ENDIF}

  // Для загрузки/создания каких-то своих настроек/профилей/etc. можно получить путь к
  // домашеней директории пользователя, или к исполняемому файлу(не работает для Linux)
  DirApp  := PChar( zgl_Get( APP_DIRECTORY ) );
  DirHome := PChar( zgl_Get( USR_HOMEDIR ) );

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
