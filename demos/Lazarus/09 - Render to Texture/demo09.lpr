program demo09;

{$R *.res}
{$DEFINE STATIC}

uses
  {$IFNDEF STATIC}
  zglHeader
  {$ELSE}
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_render_2d,
  zgl_fx,
  zgl_textures,
  zgl_textures_png,
  zgl_render_target,
  zgl_sprite_2d,
  zgl_font,
  zgl_text,
  zgl_math_2d,
  zgl_utils
  {$ENDIF}
  ;

var
  dirRes  : String {$IFNDEF DARWIN} = '../data/' {$ENDIF};
  fntMain : zglPFont;
  texTux  : zglPTexture;
  rtarget : zglPRenderTarget;

procedure Init;
begin
  texTux := tex_LoadFromFile( dirRes + 'tux_stand.png' );
  tex_SetFrameSize( textux, 64, 64 );

  fntMain := font_LoadFromFile( dirRes + 'font.zfi' );

  // RU: Создаем RenderTarget и "цепляем" пустую текстуру. В процессе текстуру можно сменить присвоив
  // rtarget.Surface другую zglPTexture, главное что бы совпадали размеры с теми, что указаны в
  // tex_CreateZero. Таргету также указан флаг RT_FULL_SCREEN, отвечающий за то, что бы в текстуру
  // помещалось все содержимое экрана а не область 512x512(как с флагом RT_DEFAULT).
  //
  // EN: Create a RenderTarget and "bind" empty texture to it. Later texture can be changed by changing
  // rtarget.Surface to another zglPTexture, the only requirement - the same size of textures, that was
  // set in tex_CreateZero. Also target use flag RT_FULL_SCREEN that responsible for rendering whole
  // content of screen to target, not only region 512x512(like with flag RT_DEFAULT).
  rtarget := rtarget_Add( tex_CreateZero( 512, 512 ), RT_FULL_SCREEN );
end;

procedure Draw;
  var
    i : Integer;
begin
  // RU: Устанавливаем текущий RenderTarget.
  // EN: Set current RenderTarget.
  rtarget_Set( rtarget );
  // RU: Рисуем в него
  // EN: Render to it.
  asprite2d_Draw( texTux, random( 800 - 64 ), random( 600 - 64 ), 64, 64, 0, random( 9 ) + 1 );
  // RU: Возвращаемся к обычному рендеру.
  // EN: Return to default rendering.
  rtarget_Set( nil );

  // RU: Теперь рисуем содержимое RenderTarget'а.
  // EN: Render content of RenderTarget.
  ssprite2d_Draw( rtarget.Surface, ( 800 - 512 ) / 2, ( 600 - 512 ) / 2, 512, 512, 0 );

  text_Draw( fntMain, 0, 0, 'FPS: ' + u_IntToStr( zgl_Get( RENDER_FPS ) ) );
end;

procedure Timer;
begin
  if key_Press( K_ESCAPE ) Then zgl_Exit();
  key_ClearState();
end;

Begin
  {$IFNDEF STATIC}
  zglLoad( libZenGL );
  {$ENDIF}

  randomize();

  timer_Add( @Timer, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  // RU: Т.к. модуль сохранен в кодировке UTF-8 и в нем используются строковые переменные
  // следует указать использование этой кодировки.
  // EN: Enable using of UTF-8, because this unit saved in UTF-8 encoding and here used
  // string variables.
  zgl_Enable( APP_USE_UTF8 );

  wnd_SetCaption( '09 - Render to Texture' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
