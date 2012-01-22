#define ZGL_IMPORT
#include "zglHeader.h"

#define SCREEN_WIDTH  800
#define SCREEN_HEIGHT 600

zglPFont    fnt;
zglPTexture icon[2];
zglPSound   sound;
size_t      audio;
int         state;

// RU: Т.к. звуковая подсистема нацелена на 3D, для позиционирования звуков в 2D нужны некоторые ухищрения
// EN: Because sound subsystem using 3D, there is some tricky way to calculate sound position in 2D
float CalcX2D( float X )
{
  return ( X - SCREEN_WIDTH / 2.0f ) * ( 1.0f / SCREEN_WIDTH / 2.0f );
}

float CalcY2D( float Y )
{
  return ( Y - SCREEN_HEIGHT / 2.0f ) * ( 1.0f / SCREEN_HEIGHT / 2.0f );
}

void Init()
{
  // RU: Инициализируем звуковую подсистему.
  // Для Windows можно сделать выбор между DirectSound и OpenAL глянув файл zgl_config.cfg
  //
  // EN: Initializing sound subsystem
  // For Windows can be used DirectSound or OpenAL, see zgl_config.cfg
  snd_Init();

  // RU: Загружаем звуковой файл и устанавливаем для него максимальноe количество проигрываемых семплов в 2.
  // EN: Load the sound file and set maximum count of samples that can be played to the 2.
  sound = snd_LoadFromFile( "../data/click.wav", 2 );

  // RU: Загружаем текстуры, которые будут индикаторами.
  // EN: Load the textures, that will be indicators.
  icon[ 0 ] = tex_LoadFromFile( "../data/audio-stop.png", 0xFF000000, TEX_DEFAULT_2D );
  icon[ 1 ] = tex_LoadFromFile( "../data/audio-play.png", 0xFF000000, TEX_DEFAULT_2D );

  fnt = font_LoadFromFile( "../data/font.zfi" );
}

void Draw()
{
  zglTRect r;

  ssprite2d_Draw( icon[ state ], ( SCREEN_WIDTH - 128 ) / 2.0f, ( SCREEN_HEIGHT - 128 ) / 2.0f, 128.0f, 128.0f, 0, 255, FX_BLEND );
  text_Draw( fnt, SCREEN_WIDTH / 2.0f, SCREEN_HEIGHT / 2.0f + 64.0f, "Skillet - Comatose - Whispers In The Dark", TEXT_HALIGN_CENTER );

  r.X = ( SCREEN_WIDTH - 128 ) / 2.0f;
  r.Y = ( SCREEN_HEIGHT - 128 ) / 2.0f;
  r.W = 128.0f;
  r.H = 128.0f;
  if ( col2d_PointInRect( (float)mouse_X(), (float)mouse_Y(), r ) )
  {
    fx_SetBlendMode( FX_BLEND_ADD, TRUE );
    ssprite2d_Draw( icon[ state ], ( SCREEN_WIDTH - 132 ) / 2.0f, ( SCREEN_HEIGHT - 132 ) / 2.0f, 132.0f, 132.0f, 0, 155, FX_BLEND );
    fx_SetBlendMode( FX_BLEND_NORMAL, TRUE );
  }
}

void Proc()
{
  zglTRect r;
  int p;

  // RU: Проверяем играет ли музыка(1 - играет, 0 - не играет). Так же можно проверить и звуки - подставив zglPSound и ID вот так:
  // snd_Get( Sound, ID...
  // ID возвращается функцией snd_Play
  //
  // EN: Check if music playing(1 - playing, 0 - not playing). Sounds also can be checked this way - just use zglPSound and ID:
  // snd_Get( Sound, ID...
  // ID returns by function snd_Play.
  state = snd_Get( (zglPSound)audio, SND_STREAM, SND_STATE_PLAYING );
  if ( state == 0 )
    audio = 0;

  if ( mouse_Click( M_BLEFT ) )
  {
    // RU: В данном случаи мы начинаем воспроизводить звук сразу в указанных координатах, но их можно менять и в процессе используя процедуру snd_SetPos.
    // Важно: Для OpenAL можно позиционировать только mono-звуки
    //
    // EN: In this case, we begin to play the sound directly in these coordinates, but they can be changed later using procedure snd_SetPos.
    // Important: OpenAL can position only mono-sounds.
    snd_Play( sound, FALSE, CalcX2D( (float)mouse_X() ), CalcY2D( (float)mouse_Y() ), FALSE );

    r.X = ( SCREEN_WIDTH - 128 ) / 2;
    r.Y = ( SCREEN_HEIGHT - 128 ) / 2;
    r.W = 128;
    r.H = 128;
    if ( col2d_PointInRect( (float)mouse_X(), (float)mouse_Y(), r ) && ( audio == 0 ) )
      audio = snd_PlayFile( "../data/music.ogg", FALSE );
  }

  // RU: Получаем в процентах позицию проигрывания аудиопотока и ставим громкость для плавных переходов.
  // EN: Get position in percent's for audio stream and set volume for smooth playing.
  p = snd_Get( (zglPSound)audio, SND_STREAM, SND_STATE_PERCENT );
  if ( ( p >= 0 ) && ( p < 25 ) )
    snd_SetVolume( (zglPSound)audio, SND_STREAM, ( 1.0f / 24.0f ) * p );
  if ( ( p >= 75 ) && ( p < 100 ) )
    snd_SetVolume( (zglPSound)audio, SND_STREAM, 1 - (float)( ( 1.0f / 24.0f ) * ( p - 75 ) ) );

  if ( key_Press( K_ESCAPE ) ) zgl_Exit();
  key_ClearState();
  mouse_ClearState();
}

int main()
{
  zglLoad( libZenGL );

  srand( 0xDeaDBeeF );

  timer_Add( (void*)&Proc, 16, FALSE, NULL );

  zgl_Reg( SYS_LOAD, (void*)&Init );
  zgl_Reg( SYS_DRAW, (void*)&Draw );

  wnd_SetCaption( "08 - Sound" );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( SCREEN_WIDTH, SCREEN_HEIGHT, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init( 0, 0 );

  zglFree();
  return 0;
}
