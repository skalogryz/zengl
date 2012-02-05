program demo01;

uses
  // RU: Перед использованием модулей, не забудьте указать путь к исходным кодам ZenGL :)
  // EN: Before using the modules don't forget to set path to source code of ZenGL :)
  zgl_application,
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_utils
  ;

var
  DirApp  : UTF8String;
  DirHome : UTF8String;

procedure Init;
begin
  // RU: Тут можно выполнять загрузку основных ресурсов.
  // EN: Here can be loading of main resources.
end;

procedure Draw;
begin
  // RU: Тут "рисуем" что угодно :)
  // EN: Here "draw" anything :)
end;

procedure Update( dt : Double );
begin
  // RU: Эта функция наземенима для реализация плавного движения чего-либо, т.к. таймеры зачастую ограничены FPS.
  // EN: This function is the best way to implement smooth moving of something, because timers are restricted by FPS.
end;

procedure Timer;
begin
end;

Begin
  // RU: Для загрузки/создания каких-то своих настроек/профилей/etc. можно получить путь к
  // домашенему каталогу пользователя, или к исполняемому файлу(не работает для GNU/Linux).
  //
  // EN: For loading/creating your own options/profiles/etc. you can get path to user home
  // directory, or to executable file(not works for GNU/Linux).
  DirApp  := u_CopyUTF8Str( PAnsiChar( zgl_Get( DIRECTORY_APPLICATION ) ) );
  DirHome := u_CopyUTF8Str( PAnsiChar( zgl_Get( DIRECTORY_HOME ) ) );

  // RU: Создаем таймер с интервалом 1000мс.
  // EN: Create a timer with interval 1000ms.
  timer_Add( @Timer, 1000 );

  // RU: Регистрируем процедуру, что выполнится сразу после инициализации ZenGL.
  // EN: Register the procedure, that will be executed after ZenGL initialization.
  zgl_Reg( SYS_LOAD, @Init );
  // RU: Регистрируем процедуру, где будет происходить рендер.
  // EN: Register the render procedure.
  zgl_Reg( SYS_DRAW, @Draw );
  // RU: Регистрируем процедуру, которая будет принимать разницу времени между кадрами.
  // EN: Register the procedure, that will get delta time between the frames.
  zgl_Reg( SYS_UPDATE, @Update );

  // RU: Инициализируем ZenGL.
  // EN: Initialize ZenGL.
  zgl_Init();
End.