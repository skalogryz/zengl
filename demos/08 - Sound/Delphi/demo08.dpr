program demo08;

uses
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_mouse,
  zgl_render_2d,
  zgl_fx,
  zgl_textures,
  zgl_textures_png, // Важный момент, обязательно один раз подключить модуль с поддержкой нужного формата данных
  zgl_font,
  zgl_text,
  zgl_sprite_2d,
  zgl_sound,
  zgl_sound_wav,
  zgl_sound_ogg,    // И это тоже важно :) Включает поддержку ogg
                    // Для декодирования ogg-файлов понадобится либо dll-файлы библиотеки,
                    // либо файлы для статической линковки(только FreePascal).
                    // Все нужные файлы можно найти тут - http://andru-kun.inf.ua/zengl_extra.html
  zgl_math_2d,
  zgl_collision_2d,
  zgl_utils;

const
  SCREEN_WIDTH  = 800;
  SCREEN_HEIGHT = 600;

var
  fnt   : zglPFont;
  icon  : array[ 0..1 ] of zglPTexture;
  Sound : zglPSound;
  Audio : Integer;
  State : Integer;

// Т.к. звуковая подсистема нацелена на 3D, для позиционирования звуков в 2D нужны некоторые ухищрения
function CalcX2D( const X : Single ) : Single;
begin
  Result := ( X - SCREEN_WIDTH / 2 ) * ( 1 / SCREEN_WIDTH / 2 );
end;

function CalcY2D( const Y : Single ) : Single;
begin
  Result := ( Y - SCREEN_HEIGHT / 2 ) * ( 1 / SCREEN_HEIGHT / 2 );
end;

procedure Init;
  var
    i : Integer;
begin
  // Инициализируем звуковую подсистему.
  // Для Windows можно сделать выбор между DirectSound и OpenAL глянув файл zgl_config.cfg
  snd_Init;

  // Загружаем звуковой файл и устанавливаем для него максимальнео количество проигрываемых семлов в 2
  Sound := snd_LoadFromFile( '../res/click.wav', 2 );

  // Загружаем текстуры, которые будут индикаторами
  icon[ 0 ] := tex_LoadFromFile( '../res/audio-stop.png', $FF000000, TEX_DEFAULT_2D );
  icon[ 1 ] := tex_LoadFromFile( '../res/audio-play.png', $FF000000, TEX_DEFAULT_2D );

  fnt := font_LoadFromFile( '../res/font.zfi' );
  for i := 0 to fnt.Count.Pages - 1 do
    fnt.Pages[ i ] := tex_LoadFromFile( '../res/font_' + u_IntToStr( i ) + '.png', $FF000000, TEX_DEFAULT_2D );
end;

procedure Draw;
  var
    r : zglTRect;
begin
  ssprite2d_Draw( icon[ State ], ( SCREEN_WIDTH - 128 ) / 2, ( SCREEN_HEIGHT - 128 ) / 2, 128, 128, 0 );
  text_Draw( fnt, SCREEN_WIDTH / 2, SCREEN_HEIGHT / 2 + 64, 'Skillet - Comatose - Whispers In The Dark', TEXT_HALIGN_CENTER );

  r.X := ( SCREEN_WIDTH - 128 ) / 2;
  r.Y := ( SCREEN_HEIGHT - 128 ) / 2;
  r.W := 128;
  r.H := 128;
  if col2d_PointInRect( mouse_X, mouse_Y, r ) Then
    begin
      fx_SetBlendMode( FX_BLEND_ADD );
      ssprite2d_Draw( icon[ State ], ( SCREEN_WIDTH - 132 ) / 2, ( SCREEN_HEIGHT - 132 ) / 2, 132, 132, 0, 155 );
      fx_SetBlendMode( FX_BLEND_NORMAL );
    end;
end;

procedure Proc;
  var
    r : zglTRect;
    p : Integer;
begin
  // Проверяем играет ли музыка(1 - играет, 0 - не играет). Так же можно проверить и звуки - подставив zglPSound и ID вот так:
  // snd_Get( Sound, ID...
  // ID возвращается функцией snd_Play
  State := snd_Get( zglPSound( Audio ), SND_STREAM, SND_STATE_PLAYING );
  if State = 0 Then
    Audio := 0;

  if mouse_Click( M_BLEFT ) Then
    begin
      // В данном случаи мы начинаем воспроизводить звук сразу в указанных координатах,
      // но их можно менять и в процессе используя процедуру snd_SetPos
      // Важно: Для OpenAL можно позиционировать только mono-звуки
      snd_Play( Sound, FALSE, CalcX2D( mouse_X ), CalcY2D( mouse_Y ) );

      r.X := ( SCREEN_WIDTH - 128 ) / 2;
      r.Y := ( SCREEN_HEIGHT - 128 ) / 2;
      r.W := 128;
      r.H := 128;
      if col2d_PointInRect( mouse_X, mouse_Y, r ) and ( Audio = 0 ) Then
        Audio := snd_PlayFile( '../res/music.ogg' );
    end;

  // Получаем в процентах позицию проигрывания аудиопотока и ставим громкость
  // для плавных переходов
  p := snd_Get( zglPSound( Audio ), SND_STREAM, SND_STATE_PERCENT );
  if ( p >= 0 ) and ( p < 25 ) Then
    snd_SetVolume( zglPSound( Audio ), SND_STREAM, ( 1 / 24 ) * p );
  if ( p >= 75 ) and ( p < 100 ) Then
    snd_SetVolume( zglPSound( Audio ), SND_STREAM, 1 - ( 1 / 24 ) * ( p - 75 ) );

  if key_Press( K_ESCAPE ) Then zgl_Exit;
  key_ClearState;
  mouse_ClearState;
end;

Begin
  randomize;

  timer_Add( @Proc, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  wnd_SetCaption( '08 - Sound' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( SCREEN_WIDTH, SCREEN_HEIGHT, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init;
End.
