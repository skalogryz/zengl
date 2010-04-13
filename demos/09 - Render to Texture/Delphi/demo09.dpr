program demo09;

uses
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_render_2d,
  zgl_fx,
  zgl_textures,
  zgl_textures_png, // Важный момент, обязательно один раз подключить модуль с поддержкой нужного формата данных
  zgl_render_target,
  zgl_sprite_2d,
  zgl_font,
  zgl_text,
  zgl_math_2d,
  zgl_utils;

var
  fntMain : zglPFont;
  texTux  : zglPTexture;
  Mode    : Integer;
  RTarget : array[ 0..1 ] of zglPRenderTarget;

procedure Init;
  var
    i : Integer;
begin
  texTux := tex_LoadFromFile( '../res/tux_stand.png', $FF000000, TEX_DEFAULT_2D );
  tex_SetFrameSize( textux, 64, 64 );

  fntMain := font_LoadFromFile( '../res/font.zfi' );
  for i := 0 to fntMain.Count.Pages - 1 do
    fntMain.Pages[ i ] := tex_LoadFromFile( '../res/font_' + u_IntToStr( i ) + '.png', $FF000000, TEX_DEFAULT_2D );

  // Создаем три различных RenderTarget'а и к каждому "цепляем" пустую текстуру
  // В процессе текстуру можно сменить присвоив RTarget[ n ].Surface другую zglPTexture, главное что бы совпадали размеры
  // с теми, что указаны в tex_CreateZero. Каждому таргету также указан флаг RT_FULL_SCREEN, отвечающий за то, что бы
  // в текстуру помещалось все содержимое экрана а не область 512x512
  RTarget[ 0 ] := rtarget_Add( RT_TYPE_PBUFFER, tex_CreateZero( 512, 512, $00000000, TEX_DEFAULT_2D ), RT_FULL_SCREEN );
  RTarget[ 1 ] := rtarget_Add( RT_TYPE_FBO, tex_CreateZero( 512, 512, $00000000, TEX_DEFAULT_2D ), RT_FULL_SCREEN );
end;

procedure RenderTux;
begin
  asprite2d_Draw( texTux, random( 800 - 64 ), random( 600 - 64 ), 64, 64, 0, random( 9 ) + 1 );
end;

procedure Draw;
  var
    i : Integer;
begin
  // Для показательности разницы в скорости будем рендерить 10 раз
  for i := 0 to 9 do
    begin
      rtarget_Set( RTarget[ Mode ] );
      RenderTux;
      rtarget_Set( nil );
    end;

  ssprite2d_Draw( RTarget[ Mode ].Surface, ( 800 - 512 ) / 2, ( 600 - 512 ) / 2, 512, 512, 0 );

  text_Draw( fntMain, 0, 0, 'FPS: ' + u_IntToStr( zgl_Get( SYS_FPS ) ) );
  text_Draw( fntMain, 0, 20, 'F1 - PBuffer' );
  text_Draw( fntMain, 0, 40, 'F2 - FBO' );
end;

procedure Timer;
begin
  if key_Press( K_F1 ) Then Mode := 0;
  if key_Press( K_F2 ) Then Mode := 1;
  if key_Press( K_ESCAPE ) Then zgl_Exit;
  key_ClearState;
end;

Begin
  randomize;

  timer_Add( @Timer, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  wnd_SetCaption( '09 - Render to Texture' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init;
End.
