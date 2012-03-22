{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://zengl.org
 *
 *  This file is part of ZenGL.
 *
 *  ZenGL is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *  published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *
 *  ZenGL is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with ZenGL. If not, see http://www.gnu.org/licenses/
}
unit zgl_sound;

{$I zgl_config.cfg}
{$IFDEF iOS}
  {$LINKFRAMEWORK AudioToolbox}
{$ENDIF}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF iOS}
  CFBase,
  CFRunLoop,
  {$ENDIF}
  {$IFDEF ANDROID}
  UnixType,
  {$ENDIF}
  zgl_types,
  {$IFDEF USE_OPENAL}
  zgl_sound_openal,
  {$ELSE}
  zgl_sound_dsound,
  {$ENDIF}
  zgl_threads,
  zgl_file,
  zgl_memory;

const
  SND_FORMAT_MONO8    = 1;
  SND_FORMAT_MONO16   = 2;
  SND_FORMAT_STEREO8  = 3;
  SND_FORMAT_STEREO16 = 4;

  SND_VOLUME_DEFAULT = -1;

  SND_ALL        = -$000002;
  SND_ALL_LOOPED = -$000003;
  SND_STREAM     = -$000010;
  SND_ERROR      = {$IFDEF USE_OPENAL} 0 {$ELSE} nil {$ENDIF};

  SND_STATE_PLAYING = 1;
  SND_STATE_LOOPED  = 2;
  SND_STATE_PERCENT = 3;
  SND_STATE_TIME    = 4;
  SND_INFO_LENGTH   = 5;

  SND_MAX           = 8;

type
  zglPSound        = ^zglTSound;
  zglPSoundStream  = ^zglTSoundStream;
  zglPSoundDecoder = ^zglTSoundDecoder;
  zglPSoundFormat  = ^zglTSoundFormat;
  zglPSoundManager = ^zglTSoundManager;

  zglTSoundFileLoader = procedure( const FileName : UTF8String; var Data : Pointer; var Size, Format, Frequency : LongWord );
  zglTSoundMemLoader  = procedure( const Memory : zglTMemory; var Data : Pointer; var Size, Format, Frequency : LongWord );

  zglTSoundChannel = record
    {$IFDEF USE_OPENAL}
    Source     : Ptr;
    {$ELSE}
    Source     : IDirectSoundBuffer;
    {$ENDIF}
    Speed      : Single;
    Volume     : Single;
    Position   : record
      X, Y, Z : Single;
                 end;
  end;

  zglTSound = record
    Buffer      : LongWord;
    SourceCount : Integer;
    Channel     : array of zglTSoundChannel;

    Data        : Pointer;
    Size        : LongWord;
    Length      : Double;
    Frequency   : LongWord;

    prev, next  : zglPSound;
  end;

  zglTSoundStream = record
    _data      : Pointer;
    _file      : zglTFile;
    _memory    : zglTMemory;
    _decoder   : zglPSoundDecoder;
    _playing   : Boolean;
    _paused    : Boolean;
    _waiting   : Boolean;
    _complete  : Double;
    _lastTime  : Double;

    ID         : Integer;

    Buffer     : Pointer;
    BufferSize : LongWord;

    Bits       : LongWord;
    Frequency  : LongWord;
    Channels   : LongWord;
    Length     : Double;

    Loop       : Boolean;
  end;

  zglTSoundDecoder = record
    Ext     : UTF8String;
    Open    : function( var Stream : zglTSoundStream; const FileName : UTF8String ) : Boolean;
    OpenMem : function( var Stream : zglTSoundStream; const Memory : zglTMemory ) : Boolean;
    Read    : function( var Stream : zglTSoundStream; Buffer : Pointer; Bytes : LongWord; var _End : Boolean ) : LongWord;
    Seek    : procedure( var Stream : zglTSoundStream; Milliseconds : Double );
    Loop    : procedure( var Stream : zglTSoundStream );
    Close   : procedure( var Stream : zglTSoundStream );
  end;

  zglTSoundFormat = record
    Extension  : UTF8String;
    Decoder    : zglPSoundDecoder;
    FileLoader : zglTSoundFileLoader;
    MemLoader  : zglTSoundMemLoader;
  end;

  zglTSoundManager = record
    Count   : record
      Items   : Integer;
      Formats : Integer;
              end;
    First   : zglTSound;
    Formats : array of zglTSoundFormat;
  end;

procedure snd_MainLoop;
function  snd_Init : Boolean;
procedure snd_Free;
function  snd_Add( SourceCount : Integer ) : zglPSound;
procedure snd_Del( var Sound : zglPSound );
procedure snd_Create( var Sound : zglTSound; Format : LongWord );
function  snd_LoadFromFile( const FileName : UTF8String; SourceCount : Integer = 8 ) : zglPSound;
function  snd_LoadFromMemory( const Memory : zglTMemory; const Extension : UTF8String; SourceCount : Integer = 8 ) : zglPSound;

function  snd_Play( Sound : zglPSound; Loop : Boolean = FALSE; X : Single = 0; Y : Single = 0; Z : Single = 0; Volume : Single = SND_VOLUME_DEFAULT ) : Integer;
procedure snd_Stop( Sound : zglPSound; ID : Integer );
procedure snd_SetPos( Sound : zglPSound; ID : Integer; X, Y, Z : Single );
procedure snd_SetVolume( Sound : zglPSound; ID : Integer; Volume : Single );
procedure snd_SetSpeed( Sound : zglPSound; ID : Integer; Speed : Single );
function  snd_Get( Sound : zglPSound; ID, What : Integer ) : Integer;

function  snd_PlayFile( const FileName : UTF8String; Loop : Boolean = FALSE; Volume : Single = SND_VOLUME_DEFAULT ) : Integer;
function  snd_PlayMemory( const Memory : zglTMemory; const Extension : UTF8String; Loop : Boolean = FALSE; Volume : Single = SND_VOLUME_DEFAULT ) : Integer;
procedure snd_PauseStream( ID : Integer );
procedure snd_StopStream( ID : Integer );
procedure snd_ResumeStream( ID : Integer );
procedure snd_SeekStream( ID : Integer; Milliseconds : Double );
function  snd_ProcStream( data : Pointer ) : LongInt;

var
  managerSound : zglTSoundManager;

  sndActive      : Boolean;
  sndInitialized : Boolean = FALSE;
  sndVolume      : Single  = 1;
  sndCanPlay     : Boolean = TRUE;
  sndCanPlayFile : Boolean = TRUE;
  sndAutoPaused  : Boolean;

  sfStream    : array[ 1..SND_MAX ] of zglTSoundStream;
  sfVolume    : Single = 1;
  sfVolumes   : array[ 1..SND_MAX ] of Single;
  sfPositions : array[ 1..SND_MAX, 0..2 ] of Single;
  sfSeek      : array[ 1..SND_MAX ] of Double;
  {$IFDEF USE_OPENAL}
  sfFormat  : array[ 1..2 ] of LongInt = ( AL_FORMAT_MONO16, AL_FORMAT_STEREO16 );
  sfSource  : array[ 1..SND_MAX ] of LongWord;
  sfBuffers : array[ 1..SND_MAX, 0..1 ] of LongWord;
  {$ELSE}
  sfNotify      : array[ 1..SND_MAX ] of IDirectSoundNotify;
  sfNotifyPos   : array[ 1..SND_MAX ] of TDSBPositionNotify;
  sfNotifyEvent : array[ 1..SND_MAX ] of THandle;
  sfSource      : array[ 1..SND_MAX ] of IDirectSoundBuffer;
  sfLastPos     : array[ 1..SND_MAX ] of LongWord;
  {$ENDIF}

  sfThread : array[ 1..SND_MAX ] of zglTThread;
  sfCS     : array[ 1..SND_MAX ] of zglTCriticalSection;
  sfEvent  : array[ 1..SND_MAX ] of zglTEvent;

{$IFDEF iOS}
const
  kAudioSessionCategory_AmbientSound                  = 'ibma';
  kAudioSessionProperty_AudioCategory                 = 'taca';
  kAudioSessionProperty_OverrideCategoryMixWithOthers = 'ximc';

var
  sndAllowBackgroundMusic : LongWord;

function AudioSessionInitialize( inRunLoop : CFRunLoopRef; inRunLoopMode : CFStringRef; inInterruptionListener : Pointer; inClientData : Pointer ) : Pointer; cdecl; external;
function AudioSessionSetProperty( inID : LongWord; inDataSize : LongWord; inData : Pointer ) : Pointer; cdecl; external;
function AudioSessionSetActive( active : Boolean ) : Pointer; cdecl; external;
{$ENDIF}

implementation
uses
  zgl_application,
  zgl_main,
  zgl_window,
  zgl_timers,
  zgl_resources,
  zgl_log,
  zgl_utils;

function GetStatusPlaying( const Source : {$IFDEF USE_OPENAL} LongWord {$ELSE} IDirectSoundBuffer {$ENDIF} ) : Integer;
  var
    status : {$IFDEF USE_OPENAL} LongInt {$ELSE} LongWord {$ENDIF};
begin
  {$IFDEF USE_OPENAL}
  alGetSourcei( Source, AL_SOURCE_STATE, status );
  Result := Byte( status = AL_PLAYING );
  {$ELSE}
  if not Assigned( Source ) Then
    begin
      Result := 0;
      exit;
    end;
  Source.GetStatus( status );
  Result := Byte( status and DSBSTATUS_PLAYING > 0 );
  {$ENDIF}
end;

function GetStatusLooped( const Source : {$IFDEF USE_OPENAL} LongWord {$ELSE} IDirectSoundBuffer {$ENDIF} ) : Integer;
  var
    status : {$IFDEF USE_OPENAL} LongInt {$ELSE} LongWord {$ENDIF};
begin
  {$IFDEF USE_OPENAL}
  alGetSourcei( Source, AL_LOOPING, status );
  Result := Byte( status = AL_TRUE );
  {$ELSE}
  if not Assigned( Source ) Then
    begin
      Result := 0;
      exit;
    end;
  Source.GetStatus( status );
  Result := Byte( status and DSBSTATUS_LOOPING > 0 );
  {$ENDIF}
end;

procedure snd_MainLoop;
  var
    i : Integer;
  {$IFDEF USE_OPENAL}
    z : Integer;
  {$ENDIF}
begin
  if not sndInitialized Then exit;

  for i := 1 to SND_MAX do
    begin
      thread_CSEnter( sfCS[ i ] );
      if GetStatusPlaying( sfSource[ i ] ) = 1 Then
        begin
          if timer_GetTicks() - sfStream[ i ]._lastTime >= 10 Then
            begin
              sfStream[ i ]._complete := timer_GetTicks() - sfStream[ i ]._lastTime + sfStream[ i ]._complete;
              if sfStream[ i ]._complete > sfStream[ i ].Length Then
                sfStream[ i ]._complete := sfStream[ i ].Length;
              sfStream[ i ]._lastTime := timer_GetTicks();
            end;
        end else
          sfStream[ i ]._lastTime := timer_GetTicks();
      thread_CSLeave( sfCS[ i ] );
    end;

  if appFocus Then
    begin
      if sndAutoPaused Then
        begin
          sndAutoPaused := FALSE;
          {$IFDEF USE_OPENAL}
          for i := 0 to length( oalSources ) - 1 do
            if oalSrcState[ i ] = AL_PLAYING Then
              alSourcePlay( oalSources[ i ] );
          {$ELSE}
          {$ENDIF}
        end;
      for i := 1 to SND_MAX do
        if sfStream[ i ]._playing and sfStream[ i ]._waiting Then
          begin
            sfStream[ i ]._waiting := FALSE;
            snd_ResumeStream( i );
          end;
    end else
      begin
        if not sndAutoPaused Then
          begin
            sndAutoPaused := TRUE;
            {$IFDEF USE_OPENAL}
            for i := 0 to length( oalSources ) - 1 do
              begin
                alGetSourcei( oalSources[ i ], AL_SOURCE_STATE, z );
                if z = AL_PLAYING Then
                  alSourcePause( oalSources[ i ] );
                oalSrcState[ i ] := z;
              end;
            {$ELSE}
            {$ENDIF}
          end;
        for i := 1 to SND_MAX do
          if sfStream[ i ]._playing and ( not sfStream[ i ]._paused ) and ( not sfStream[ i ]._waiting ) Then
            begin
              snd_PauseStream( i );
              sfStream[ i ]._waiting := TRUE;
            end;
      end;
end;

function snd_Init : Boolean;
  var
    i : Integer;
  {$IFDEF iOS}
    sessionCategory : LongWord;
  {$ENDIF}
  {$IFDEF ANDROID}
    attr : array[ 0..2 ] of Integer;
  {$ENDIF}
begin
  Result := FALSE;

  if sndInitialized Then exit;
{$IFDEF USE_OPENAL}
  {$IFNDEF ANDROID}
  log_Add( 'OpenAL: load ' + libopenal  );
  if not InitOpenAL Then
    begin
      log_Add( 'Error while loading ' + libopenal );
      exit;
    end;
  {$ENDIF}

  {$IFDEF LINUX}
  log_Add( 'OpenAL: opening "ALSA Software"' );
  oalDevice := alcOpenDevice( 'ALSA Software' );
  {$ENDIF}
  {$IFDEF WINDOWS}
  log_Add( 'OpenAL: opening "Generic Software"' );
  oalDevice := alcOpenDevice( 'Generic Software' );
  {$ENDIF}
  {$IFDEF MACOSX}
  log_Add( 'OpenAL: opening "CoreAudio Software"' );
  oalDevice := alcOpenDevice( 'CoreAudio Software' );
  {$ENDIF}
  {$IFDEF iOS}
  log_Add( 'OpenAL: opening default device - "' + alcGetString( nil, ALC_DEFAULT_DEVICE_SPECIFIER ) + '"' );
  oalDevice := alcOpenDevice( nil );
  if AudioSessionInitialize( nil, nil, nil, nil ) = nil Then
    begin
      sessionCategory := LongWord( kAudioSessionCategory_AmbientSound );
      AudioSessionSetProperty( LongWord( kAudioSessionProperty_AudioCategory ), SizeOf( sessionCategory ), @sessionCategory );
      AudioSessionSetProperty( LongWord( kAudioSessionProperty_OverrideCategoryMixWithOthers ), SizeOf( sndAllowBackgroundMusic ), @sndAllowBackgroundMusic );
      AudioSessionSetActive( TRUE );
    end else
      log_Add( 'Unable to initialize Audio Session' );
  {$ELSE}
  if not Assigned( oalDevice ) Then
    begin
      oalDevice := alcOpenDevice( nil );
      log_Add( 'OpenAL: opening default device - "' + alcGetString( nil, ALC_DEFAULT_DEVICE_SPECIFIER ) + '"' );
    end;
  {$ENDIF}
  if not Assigned( oalDevice ) Then
    begin
      log_Add( 'Cannot open sound device' );
      exit;
    end;

  log_Add( 'OpenAL: creating context' );
  {$IFNDEF ANDROID}
  oalContext := alcCreateContext( oalDevice, nil );
  {$ELSE}
  attr[ 0 ] := $1007;
  attr[ 1 ] := 22050;
  attr[ 2 ] := 0;
  oalContext := alcCreateContext( oalDevice, @attr[ 0 ] );
  {$ENDIF}
  if not Assigned( oalContext ) Then
    begin
      log_Add( 'Cannot create sound context' );
      exit;
    end;

  if alcMakeContextCurrent( oalContext ) Then
    log_Add( 'OpenAL: sound system initialized' )
  else
    begin
      log_Add( 'OpenAL: cannot set current context' );
      exit;
    end;

  alListenerfv( AL_POSITION,    @oalPosition );
  alListenerfv( AL_VELOCITY,    @oalVelocity );
  alListenerfv( AL_ORIENTATION, @oalOrientation );

  for i := 1 to SND_MAX do
    begin
      alGenSources( 1, @sfSource[ i ] );
      alGenBuffers( 2, @sfBuffers[ i ] );
    end;

  i := 64;
  SetLength( oalSources, i );
  alGenSources( i, @oalSources[ 0 ] );
  while alGetError( nil ) <> AL_NO_ERROR do
    begin
      DEC( i, 8 );
      if i = 0 Then break;
      SetLength( oalSources, i );
      alGenSources( i, @oalSources[ 0 ] );
    end;
  SetLength( oalSrcPtrs, i );
  SetLength( oalSrcState, i );

  log_Add( 'OpenAL: generated ' + u_IntToStr( length( oalSources ) ) + ' source' );
{$ELSE}
  log_Add( 'DirectSound: loading DSound.dll' );
  if not InitDSound() Then
    log_Add( 'DirectSound: Error while loading libraries' );

  if DirectSoundCreate( nil, dsDevice, nil ) <> DS_OK Then
    begin
      FreeDSound();
      log_Add( 'DirectSound: Error while calling DirectSoundCreate' );
      exit;
    end;

  if dsDevice.SetCooperativeLevel( wndHandle, DSSCL_PRIORITY ) <> DS_OK Then
    log_Add( 'DirectSound: Can''t SetCooperativeLevel' );

  log_Add( 'DirectSound: sound system initialized' );
{$ENDIF}

  for i := 1 to SND_MAX do
    thread_CSInit( sfCS[ i ] );

  sndInitialized := TRUE;
  Result         := TRUE;
end;

procedure snd_Free;
  var
    i   : Integer;
    snd : zglPSound;
begin
  if not sndInitialized Then exit;

  if managerSound.Count.Items <> 0 Then
    log_Add( 'Sounds to free: ' + u_IntToStr( managerSound.Count.Items ) );
  while managerSound.Count.Items > 0 do
    begin
      snd := managerSound.First.next;
      snd_Del( snd );
    end;

  for i := 1 to SND_MAX do
    begin
      snd_StopStream( i );
      while sfEvent[ i ] <> nil do;
      thread_EventDestroy( sfEvent[ i ] );
    end;

  for i := 1 to SND_MAX do
    if Assigned( sfStream[ i ]._decoder ) Then
      begin
        sfStream[ i ]._decoder.Close( sfStream[ i ] );
        if Assigned( sfStream[ i ].Buffer ) Then
          FreeMem( sfStream[ i ].Buffer );
        if Assigned( sfStream[ i ]._data ) Then
          FreeMem( sfStream[ i ]._data );
      end;

{$IFDEF USE_OPENAL}
  for i := 1 to SND_MAX do
    begin
      alDeleteSources( 1, @sfSource[ i ] );
      alDeleteBuffers( 2, @sfBuffers[ i ] );
    end;
  alDeleteSources( length( oalSources ), @oalSources[ 0 ] );
  SetLength( oalSources, 0 );
  SetLength( oalSrcPtrs, 0 );
  SetLength( oalSrcState, 0 );

  log_Add( 'OpenAL: destroying current sound context' );
  alcDestroyContext( oalContext );
  log_Add( 'OpenAL: closing sound device' );
  alcCloseDevice( oalDevice );
  log_Add( 'OpenAL: sound system finalized' );
  FreeOpenAL();
{$ELSE}
  for i := 1 to SND_MAX do
    begin
      CloseHandle( sfNotifyEvent[ i ] );
      sfNotify[ i ] := nil;
      sfSource[ i ] := nil;
    end;
  dsDevice := nil;

  FreeDSound();
  log_Add( 'DirectSound: sound system finalized' );
{$ENDIF}

  sndInitialized := FALSE;
end;

function snd_Add( SourceCount : Integer ) : zglPSound;
  {$IFDEF USE_OPENAL}
  var
    i : Integer;
  {$ENDIF}
begin
  Result := nil;

  if not sndInitialized Then exit;

  Result := @managerSound.First;
  while Assigned( Result.next ) do
    Result := Result.next;

  zgl_GetMem( Pointer( Result.next ), SizeOf( zglTSound ) );
  Result.next.prev := Result;
  Result.next.next := nil;
  Result           := Result.next;

  Result.SourceCount := SourceCount;
  SetLength( Result.Channel, SourceCount );
{$IFDEF USE_OPENAL}
  alGenBuffers( 1, @Result.Buffer );
  for i := 0 to SourceCount - 1 do
    FillChar( Result.Channel[ i ], SizeOf( zglTSoundChannel ), 0 );
{$ENDIF}

  INC( managerSound.Count.Items );
end;

procedure snd_Del( var Sound : zglPSound );
  {$IFNDEF USE_OPENAL}
  var
    i : Integer;
  {$ENDIF}
begin
  if not Assigned( Sound ) Then exit;

  snd_Stop( Sound, SND_ALL );

{$IFDEF USE_OPENAL}
  alDeleteBuffers( 1, @Sound.Buffer );
{$ELSE}
  FreeMem( Sound.Data );
  for i := 0 to Sound.SourceCount - 1 do
    Sound.Channel[ i ].Source := nil;
{$ENDIF}
  SetLength( Sound.Channel, 0 );

  if Assigned( Sound.prev ) Then
    Sound.prev.next := Sound.next;
  if Assigned( Sound.next ) Then
    Sound.next.prev := Sound.prev;

  FreeMem( Sound );
  Sound := nil;

  DEC( managerSound.Count.Items );
end;

procedure snd_Create( var Sound : zglTSound; Format : LongWord );
  {$IFNDEF USE_OPENAL}
  var
    i        : Integer;
    buffDesc : zglTBufferDesc;
  {$ENDIF}
begin
  case Format of
    {$IFDEF USE_OPENAL}
    SND_FORMAT_MONO8: Format := AL_FORMAT_MONO8;
    SND_FORMAT_MONO16: Format := AL_FORMAT_MONO16;
    SND_FORMAT_STEREO8: Format := AL_FORMAT_STEREO8;
    SND_FORMAT_STEREO16: Format := AL_FORMAT_STEREO16;
    {$ELSE}
    SND_FORMAT_MONO8:
      begin
        buffDesc.ChannelNumber := 1;
        buffDesc.BitsPerSample := 8;
      end;
    SND_FORMAT_MONO16:
      begin
        buffDesc.ChannelNumber := 1;
        buffDesc.BitsPerSample := 16;
      end;
    SND_FORMAT_STEREO8:
      begin
        buffDesc.ChannelNumber := 2;
        buffDesc.BitsPerSample := 8;
      end;
    SND_FORMAT_STEREO16:
      begin
        buffDesc.ChannelNumber := 2;
        buffDesc.BitsPerSample := 16;
      end;
    {$ENDIF}
  end;

{$IFDEF USE_OPENAL}
  alBufferData( Sound.Buffer, Format, Sound.Data, Sound.Size, Sound.Frequency );
  FreeMem( Sound.Data );
{$ELSE}
  with buffDesc do
    begin
      FormatCode     := 1;
      SampleRate     := Sound.Frequency;
      BytesPerSample := ( BitsPerSample div 8 ) * ChannelNumber;
      BytesPerSecond := SampleRate * BytesPerSample;
      cbSize         := SizeOf( buffDesc );
    end;

  dsu_CreateBuffer( Sound.Channel[ 0 ].Source, Sound.Size, @buffDesc );
  dsu_FillData( Sound.Channel[ 0 ].Source, Sound.Data, Sound.Size );
  for i := 1 to Sound.SourceCount - 1 do
    dsDevice.DuplicateSoundBuffer( Sound.Channel[ 0 ].Source, Sound.Channel[ i ].Source );
{$ENDIF}
end;

function snd_LoadFromFile( const FileName : UTF8String; SourceCount : Integer = 8 ) : zglPSound;
  var
    i   : Integer;
    ext : UTF8String;
    fmt : LongWord;
    res : zglTSoundResource;
begin
  Result := nil;

  if not sndInitialized Then exit;

  if ( not resUseThreaded ) and ( not file_Exists( FileName ) ) Then
    begin
      log_Add( 'Cannot read "' + FileName + '"' );
      exit;
    end;
  Result := snd_Add( SourceCount );

  ext := u_StrUp( file_GetExtension( FileName ) );
  for i := managerSound.Count.Formats - 1 downto 0 do
    if ext = managerSound.Formats[ i ].Extension Then
      if resUseThreaded Then
        begin
          res.FileName   := FileName;
          res.Sound      := Result;
          res.FileLoader := managerSound.Formats[ i ].FileLoader;
          res_AddToQueue( RES_SOUND, TRUE, @res );
          exit;
        end else
          managerSound.Formats[ i ].FileLoader( FileName, Result.Data, Result.Size, fmt, Result.Frequency );

  if not Assigned( Result.Data ) Then
    begin
      log_Add( 'Unable to load sound: "' + FileName + '"' );
      snd_Del( Result );
      exit;
    end;

  snd_Create( Result^, fmt );

  if Assigned( Result ) Then
    log_Add( 'Sound loaded: "' + FileName + '"' );
end;

function snd_LoadFromMemory( const Memory : zglTMemory; const Extension : UTF8String; SourceCount : Integer = 8 ) : zglPSound;
  var
    i   : Integer;
    ext : UTF8String;
    fmt : LongWord;
    res : zglTSoundResource;
begin
  Result := nil;

  if not sndInitialized Then exit;

  Result := snd_Add( SourceCount );

  ext := u_StrUp( Extension );
  for i := managerSound.Count.Formats - 1 downto 0 do
    if ext = managerSound.Formats[ i ].Extension Then
      if resUseThreaded Then
        begin
          res.Memory    := Memory;
          res.Sound     := Result;
          res.MemLoader := managerSound.Formats[ i ].MemLoader;
          res_AddToQueue( RES_SOUND, FALSE, @res );
          exit;
        end else
          managerSound.Formats[ i ].MemLoader( Memory, Result.Data, Result.Size, fmt, Result.Frequency );

  if not Assigned( Result.Data ) Then
    begin
      log_Add( 'Unable to load sound: From Memory' );
      snd_Del( Result );
      exit;
    end;

  snd_Create( Result^, fmt );
end;

function snd_Play( Sound : zglPSound; Loop : Boolean = FALSE; X : Single = 0; Y : Single = 0; Z : Single = 0; Volume : Single = SND_VOLUME_DEFAULT ) : Integer;
  var
    i        : Integer;
    {$IFNDEF USE_OPENAL}
    dsError  : HRESULT;
    dsStatus : LongWord;
    dsVolume : Single;
    {$ELSE}
    j        : Integer;
    {$ENDIF}
begin
  Result := -1;

  if ( not Assigned( Sound ) ) or
     ( not sndInitialized ) or
     ( not sndCanPlay ) Then exit;

  if Volume = SND_VOLUME_DEFAULT Then
    Volume := sndVolume;

{$IFDEF USE_OPENAL}
  for i := 0 to Sound.SourceCount - 1 do
    begin
      if Sound.Channel[ i ].Source = 0 Then
        Sound.Channel[ i ].Source := oal_Getsource( @Sound.Channel[ i ].Source );

      alGetSourcei( Sound.Channel[ i ].Source, AL_SOURCE_STATE, j );
      if j <> AL_PLAYING Then
         begin
           Result := i;
           break;
         end;
    end;
  if Result = -1 Then exit;

  Sound.Channel[ Result ].Position.X := X;
  Sound.Channel[ Result ].Position.Y := Y;
  Sound.Channel[ Result ].Position.Z := Z;
  Sound.Channel[ Result ].Volume     := Volume;

  alSourcei ( Sound.Channel[ Result ].Source, AL_BUFFER,    Sound.Buffer );
  alSourcefv( Sound.Channel[ Result ].Source, AL_POSITION,  @Sound.Channel[ Result ].Position );
  alSourcefv( Sound.Channel[ Result ].Source, AL_VELOCITY,  @oalVelocity[ 0 ] );
  alSourcef ( Sound.Channel[ Result ].Source, AL_GAIN,      Volume );
  alSourcei ( Sound.Channel[ Result ].Source, AL_FREQUENCY, Sound.Frequency );

  if Loop Then
    alSourcei( Sound.Channel[ Result ].Source, AL_LOOPING, AL_TRUE )
  else
    alSourcei( Sound.Channel[ Result ].Source, AL_LOOPING, AL_FALSE );

  alSourcePlay( Sound.Channel[ Result ].Source );
{$ELSE}
  for i := 0 to Sound.SourceCount - 1 do
    begin
      dsError := Sound.Channel[ i ].Source.GetStatus( dsStatus );
      if dsError <> DS_OK Then dsStatus := 0;
      if ( dsStatus and DSBSTATUS_PLAYING ) = 0 Then
        begin
          if ( dsStatus and DSBSTATUS_BUFFERLOST ) <> 0 Then
            begin
              Sound.Channel[ i ].Source.Restore();
              if i = 0 Then
                dsu_FillData( Sound.Channel[ i ].Source, Sound.Data, Sound.Size )
              else
                dsDevice.DuplicateSoundBuffer( Sound.Channel[ 0 ].Source, Sound.Channel[ i ].Source );
            end;
          Result := i;
          break;
        end;
    end;
  if Result = -1 Then exit;

  Sound.Channel[ Result ].Position.X := X;
  Sound.Channel[ Result ].Position.Y := Y;
  Sound.Channel[ Result ].Position.Z := Z;
  Sound.Channel[ Result ].Volume     := Volume;

  Sound.Channel[ Result ].Source.SetPan( dsu_CalcPos( X, Y, Z, dsVolume ) );
  Sound.Channel[ Result ].Source.SetVolume( dsu_CalcVolume( dsVolume * Volume ) );
  Sound.Channel[ Result ].Source.SetFrequency( Sound.Frequency );
  Sound.Channel[ Result ].Source.Play( 0, 0, DSBPLAY_LOOPING * Byte( Loop = TRUE ) );
{$ENDIF}
end;

procedure snd_Stop( Sound : zglPSound; ID : Integer );
  var
    i, j : Integer;
    snd : zglPSound;
  procedure Stop( Sound : zglPSound; ID : Integer );
  begin
    if Sound.Channel[ ID ].Source <> SND_ERROR Then
      begin
        {$IFDEF USE_OPENAL}
        alSourceStop( Sound.Channel[ ID ].Source );
        alSourceRewind( Sound.Channel[ ID ].Source );
        alSourcei( Sound.Channel[ ID ].Source, AL_BUFFER, AL_NONE );
        {$ELSE}
        Sound.Channel[ ID ].Source.SetCurrentPosition( 0 );
        Sound.Channel[ ID ].Source.Stop();
        {$ENDIF}
      end;
  end;
begin
  if not sndInitialized Then exit;

  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.SourceCount - 1 do
            Stop( Sound, i );
        end else
          if ID = SND_ALL_LOOPED Then
            begin
              for i := 0 to Sound.SourceCount - 1 do
                if snd_Get( Sound, i, SND_STATE_LOOPED ) = 1 Then
                  Stop( Sound, i );
            end else
              if ID >= 0 Then
                Stop( Sound, ID );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.SourceCount - 1 do
                Stop( snd, j );
              snd := snd.next;
            end;
        end else
          if ID = SND_ALL_LOOPED Then
            begin
              snd := managerSound.First.next;
              for i := 0 to managerSound.Count.Items - 1 do
                begin
                  for j := 0 to snd.SourceCount - 1 do
                    if snd_Get( snd, j, SND_STATE_LOOPED ) = 1 Then
                      Stop( snd, j );
                  snd := snd.next;
                end;
            end;
end;

procedure snd_SetPos( Sound : zglPSound; ID : Integer; X, Y, Z : Single );
  var
    i, j : Integer;
    snd  : zglPSound;
    {$IFNDEF USE_OPENAL}
    vol  : Single;
    {$ENDIF}
  procedure SetPos( Sound : zglPSound; ID : Integer; X, Y, Z : Single );
  begin
    Sound.Channel[ ID ].Position.X := X;
    Sound.Channel[ ID ].Position.Y := Y;
    Sound.Channel[ ID ].Position.Z := Z;

    if Sound.Channel[ ID ].Source <> SND_ERROR Then
      begin
        {$IFDEF USE_OPENAL}
        alSourcefv( Sound.Channel[ ID ].Source, AL_POSITION, @Sound.Channel[ ID ].Position );
        {$ELSE}
        Sound.Channel[ ID ].Source.SetPan   ( dsu_CalcPos( X, Y, Z, vol ) );
        Sound.Channel[ ID ].Source.SetVolume( dsu_CalcVolume( Vol * Sound.Channel[ ID ].Volume ) );
        {$ENDIF}
      end;
  end;
begin
  if not sndInitialized Then exit;

  if ( ID = SND_STREAM ) Then
    begin
      if Assigned( Sound ) Then
        begin
          if sfSource[ Ptr( Sound ) ] = SND_ERROR Then exit;
          sfPositions[ Ptr( Sound ), 0 ] := X;
          sfPositions[ Ptr( Sound ), 1 ] := Y;
          sfPositions[ Ptr( Sound ), 2 ] := Z;

          {$IFDEF USE_OPENAL}
          alSourcefv( Ptr( Sound ), AL_POSITION, @sfPositions[ Ptr( Sound ), 0 ] );
          {$ELSE}
          sfSource[ Ptr( Sound ) ].SetPan( dsu_CalcPos( sfPositions[ Ptr( Sound ), 0 ], sfPositions[ Ptr( Sound ), 1 ], sfPositions[ Ptr( Sound ), 2 ], vol ) );
          sfSource[ Ptr( Sound ) ].SetVolume( dsu_CalcVolume( vol * sfVolumes[ Ptr( Sound ) ] ) );
          {$ENDIF}
        end else
          for i := 1 to SND_MAX do
            if sfSource[ i ] <> SND_ERROR Then
              begin
                sfPositions[ i, 0 ] := X;
                sfPositions[ i, 1 ] := Y;
                sfPositions[ i, 2 ] := Z;

                {$IFDEF USE_OPENAL}
                alSourcefv( sfSource[ i ], AL_POSITION, @sfPositions[ i, 0 ] );
                {$ELSE}
                sfSource[ i ].SetPan( dsu_CalcPos( sfPositions[ i, 0 ], sfPositions[ i, 1 ], sfPositions[ i, 2 ], vol ) );
                sfSource[ i ].SetVolume( dsu_CalcVolume( vol * sfVolumes[ i ] ) );
                {$ENDIF}
              end;
      exit;
    end;

  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.SourceCount - 1 do
            SetPos( Sound, i, X, Y, Z );
        end else
          if ID >= 0 Then
            SetPos( Sound, ID, X, Y, Z );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.SourceCount - 1 do
                SetPos( snd, j, X, Y, Z );
              snd := snd.next;
            end;
        end;
end;

procedure snd_SetVolume( Sound : zglPSound; ID : Integer; Volume : Single );
  var
    i, j : Integer;
    snd  : zglPSound;
    {$IFNDEF USE_OPENAL}
    vol  : Single;
    {$ENDIF}
  procedure SetVolume( Sound : zglPSound; ID : Integer; Volume : Single );
  begin
    Sound.Channel[ ID ].Volume := Volume;

    if Sound.Channel[ ID ].Source <> SND_ERROR Then
      begin
        {$IFDEF USE_OPENAL}
        alSourcef( Sound.Channel[ ID ].Source, AL_GAIN, Sound.Channel[ ID ].Volume );
        {$ELSE}
        Sound.Channel[ ID ].Source.SetPan( dsu_CalcPos( Sound.Channel[ ID ].Position.X, Sound.Channel[ ID ].Position.Y, Sound.Channel[ ID ].Position.Z, vol ) );
        Sound.Channel[ ID ].Source.SetVolume( dsu_CalcVolume( vol * Sound.Channel[ ID ].Volume ) );
        {$ENDIF}
      end;
  end;
begin
  if not sndInitialized Then exit;

  if Volume <> SND_VOLUME_DEFAULT Then
    begin
      if ( Sound = nil ) and ( ID = SND_STREAM ) Then
        sfVolume := Volume
      else
        if ( not Assigned( Sound ) ) and ( ID = SND_ALL ) Then
          sndVolume := Volume;
    end else
      if ID = SND_STREAM Then
        Volume := sfVolume
      else
        Volume := sndVolume;

  if ( ID = SND_STREAM ) Then
    begin
      if Assigned( Sound ) Then
        begin
          if sfSource[ Ptr( Sound ) ] = SND_ERROR Then exit;
          sfVolumes[ Ptr( Sound ) ] := Volume;

          {$IFDEF USE_OPENAL}
          alSourcef( sfSource[ Ptr( Sound ) ], AL_GAIN, Volume );
          {$ELSE}
          sfSource[ Ptr( Sound ) ].SetPan( dsu_CalcPos( sfPositions[ Ptr( Sound ), 0 ], sfPositions[ Ptr( Sound ), 1 ], sfPositions[ Ptr( Sound ), 2 ], vol ) );
          sfSource[ Ptr( Sound ) ].SetVolume( dsu_CalcVolume( vol * sfVolumes[ Ptr( Sound ) ] ) );
          {$ENDIF}
        end else
          for i := 1 to SND_MAX do
            if sfSource[ i ] <> SND_ERROR Then
              begin
                sfVolumes[ i ] := Volume;

                {$IFDEF USE_OPENAL}
                alSourcef( sfSource[ i ], AL_GAIN, Volume );
                {$ELSE}
                sfSource[ i ].SetPan( dsu_CalcPos( sfPositions[ i, 0 ], sfPositions[ i, 1 ], sfPositions[ i, 2 ], vol ) );
                sfSource[ i ].SetVolume( dsu_CalcVolume( vol * sfVolumes[ i ] ) );
                {$ENDIF}
              end;
      exit;
    end;

  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.SourceCount - 1 do
            SetVolume( Sound, i, Volume );
        end else
          if ID >= 0 Then
            SetVolume( Sound, ID, Volume );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.SourceCount - 1 do
                SetVolume( snd, j, Volume );
              snd := snd.next;
            end;
        end;
end;

procedure snd_SetSpeed( Sound : zglPSound; ID : Integer; Speed : Single );
  var
    i, j : Integer;
    snd  : zglPSound;
  procedure SetFrequency( Sound : zglPSound; ID : Integer; Speed : Single );
  begin
    Sound.Channel[ ID ].Speed := Speed;

    if Sound.Channel[ ID ].Source <> SND_ERROR Then
      {$IFDEF USE_OPENAL}
      alSourcef( Sound.Channel[ ID ].Source, AL_PITCH, Speed );
      {$ELSE}
      Sound.Channel[ ID ].Source.SetFrequency( Round( Sound.Frequency * Speed ) );
      {$ENDIF}
  end;
begin
  if not sndInitialized Then exit;

  if ( ID = SND_STREAM ) Then
    begin
      if Assigned( Sound ) Then
        begin
          if sfSource[ Ptr( Sound ) ] = SND_ERROR Then exit;

          {$IFDEF USE_OPENAL}
          alSourcef( sfSource[ Ptr( Sound ) ], AL_PITCH, Speed );
          {$ELSE}
          sfSource[ Ptr( Sound ) ].SetFrequency( Round( sfStream[ Ptr( Sound ) ].Frequency * Speed ) );
          {$ENDIF}
        end else
          for i := 1 to SND_MAX do
            if sfSource[ i ] <> SND_ERROR Then
              {$IFDEF USE_OPENAL}
              alSourcef( sfSource[ i ], AL_PITCH, Speed );
              {$ELSE}
              sfSource[ i ].SetFrequency( Round( sfStream[ i ].Frequency * Speed ) );
              {$ENDIF}
      exit;
    end;

  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.SourceCount - 1 do
            SetFrequency( Sound, i, Speed );
        end else
          if ID >= 0 Then
            SetFrequency( Sound, ID, Speed );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.SourceCount - 1 do
                SetFrequency( snd, j, Speed );
              snd := snd.next;
            end;
        end;
end;

function snd_Get( Sound : zglPSound; ID, What : Integer ) : Integer;
begin
  if not sndInitialized Then exit;

  if not Assigned( Sound ) Then
    begin
      Result := 0;
      exit;
    end;

  if ID = SND_STREAM Then
    begin
      thread_CSEnter( sfCS[ What ] );
      case What of
        SND_STATE_PLAYING: Result := GetStatusPlaying( sfSource[ Ptr( Sound ) ] );
        SND_STATE_LOOPED: Result := Byte( sfStream[ Ptr( Sound ) ].Loop );
        SND_STATE_TIME: Result := Round( sfStream[ Ptr( Sound ) ]._complete );
        SND_STATE_PERCENT: Result := Round( 100 / sfStream[ Ptr( Sound ) ].Length * sfStream[ Ptr( Sound ) ]._complete );
        SND_INFO_LENGTH: Result := Round( sfStream[ Ptr( Sound ) ].Length );
      end;
      thread_CSLeave( sfCS[ What ] );
    end else
      case What of
        SND_STATE_PLAYING: Result := GetStatusPlaying( Sound.Channel[ ID ].Source );
        SND_STATE_LOOPED: Result := GetStatusLooped( Sound.Channel[ ID ].Source );
        SND_INFO_LENGTH: Result := Round( Sound.Length );
      end;
end;

function snd_GetStreamID : Integer;
  var
    i : Integer;
begin
  for i := 1 to SND_MAX do
    if ( not sfStream[ i ]._playing ) and ( sfEvent[ i ] = nil ) Then
      begin
        Result := i;
        exit;
      end;
  Result := -1;
end;

procedure snd_PlayStream( ID : Integer; Loop : Boolean; Volume : Single );
  var
    i         : Integer;
    _end      : Boolean;
    bytesRead : Integer;
    {$IFNDEF USE_OPENAL}
    buffDesc : zglTBufferDesc;
    {$ENDIF}
begin
  if Assigned( sfStream[ ID ]._decoder ) Then
    sfStream[ ID ].Loop := Loop;

  if Volume = SND_VOLUME_DEFAULT Then
    Volume := sfVolume;

{$IFDEF USE_OPENAL}
  alSourceStop( sfSource[ ID ] );
  alSourceRewind( sfSource[ ID ] );
  alSourcei( sfSource[ ID ], AL_BUFFER, AL_NONE );

  for i := 0 to 1 do
    begin
      bytesRead := sfStream[ ID ]._decoder.Read( sfStream[ ID ], sfStream[ ID ].Buffer, sfStream[ ID ].BufferSize, _end );
      if bytesRead <= 0 Then break;

      alBufferData( sfBuffers[ ID, i ], sfFormat[ sfStream[ ID ].Channels ], sfStream[ ID ].Buffer, bytesRead, sfStream[ ID ].Frequency );
      alSourceQueueBuffers( sfSource[ ID ], 1, @sfBuffers[ ID, i ] );
    end;

  alSourcei( sfSource[ ID ], AL_LOOPING, AL_FALSE );
  alSourcePlay( sfSource[ ID ] );
  alSourcef( sfSource[ ID ], AL_GAIN, Volume );
  alSourcef( sfSource[ ID ], AL_FREQUENCY, sfStream[ ID ].Frequency );
{$ELSE}
  with buffDesc do
    begin
      FormatCode     := 1;
      ChannelNumber  := sfStream[ ID ].Channels;
      SampleRate     := sfStream[ ID ].Frequency;
      BitsPerSample  := sfStream[ ID ].Bits;
      BytesPerSample := ( BitsPerSample div 8 ) * ChannelNumber;
      BytesPerSecond := SampleRate * BytesPerSample;
      cbSize         := SizeOf( buffDesc );
    end;
  if Assigned( sfSource[ ID ] ) Then sfSource[ ID ] := nil;
  dsu_CreateBuffer( sfSource[ ID ], sfStream[ ID ].BufferSize, @buffDesc );
  bytesRead := sfStream[ ID ]._decoder.Read( sfStream[ ID ], sfStream[ ID ].Buffer, sfStream[ ID ].BufferSize, _end );
  dsu_FillData( sfSource[ ID ], sfStream[ ID ].Buffer, bytesRead );

  sfNotify[ ID ] := nil;
  sfSource[ ID ].QueryInterface( IDirectSoundNotify, sfNotify[ ID ] );
  CloseHandle( sfNotifyEvent[ ID ] );
  sfNotifyEvent[ ID ] := CreateEvent( nil, FALSE, FALSE, nil );
  sfNotifyPos[ ID ].dwOffset := 0;
  sfNotifyPos[ ID ].hEventNotify := sfNotifyEvent[ ID ];
  sfNotify[ ID ].SetNotificationPositions( 1, @sfNotifyPos[ ID ] );

  sfLastPos[ ID ] := 0;
  sfSource[ ID ].SetCurrentPosition( 0 );
  sfSource[ ID ].Play( 0, 0, DSBPLAY_LOOPING );
  sfSource[ ID ].SetVolume( dsu_CalcVolume( Volume ) );
  sfSource[ ID ].SetFrequency( sfStream[ ID ].Frequency );
{$ENDIF}

  sfStream[ ID ].ID        := ID;
  sfStream[ ID ]._playing  := TRUE;
  sfStream[ ID ]._paused   := FALSE;
  sfStream[ ID ]._waiting  := FALSE;
  sfStream[ ID ]._complete := 0;
  sfStream[ ID ]._lastTime := timer_GetTicks;

  thread_EventCreate( sfEvent[ ID ] );
  thread_Create( sfThread[ ID ], @snd_ProcStream, @sfStream[ ID ].ID );
end;

function snd_PlayFile( const FileName : UTF8String; Loop : Boolean = FALSE; Volume : Single = SND_VOLUME_DEFAULT ) : Integer;
  var
    i   : Integer;
    ext : UTF8String;
begin
  if ( not sndInitialized ) or ( not sndCanPlayFile ) Then exit;

  Result := snd_GetStreamID();
  if Result = -1 Then
    exit;

  if Assigned( sfStream[ Result ]._decoder ) Then
    begin
      sfStream[ Result ]._decoder.Close( sfStream[ Result ] );
      if Assigned( sfStream[ Result ].Buffer ) Then
        FreeMem( sfStream[ Result ].Buffer );
      if Assigned( sfStream[ Result ]._data ) Then
        FreeMem( sfStream[ Result ]._data );
    end;

  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read "' + FileName + '"' );
      exit;
    end;

  ext := u_StrUp( file_GetExtension( FileName ) );
  for i := managerSound.Count.Formats - 1 downto 0 do
    if ext = managerSound.Formats[ i ].Extension Then
      sfStream[ Result ]._decoder := managerSound.Formats[ i ].Decoder;

  if ( not Assigned( sfStream[ Result ]._decoder ) ) or ( not sfStream[ Result ]._decoder.Open( sfStream[ Result ], FileName ) ) Then
    begin
      sfStream[ Result ]._decoder := nil;
      log_Add( 'Cannot play: "' + FileName + '"' );
      exit;
    end;

  snd_PlayStream( Result, Loop, Volume );
end;

function snd_PlayMemory( const Memory : zglTMemory; const Extension : UTF8String; Loop : Boolean = FALSE; Volume : Single = SND_VOLUME_DEFAULT ) : Integer;
  var
    i   : Integer;
    ext : UTF8String;
begin
  if ( not sndInitialized ) or ( not sndCanPlayFile ) Then exit;

  Result := snd_GetStreamID();
  if Result = -1 Then
    exit;

  if Assigned( sfStream[ Result ]._decoder ) Then
    begin
      sfStream[ Result ]._decoder.Close( sfStream[ Result ] );
      if Assigned( sfStream[ Result ].Buffer ) Then
        FreeMem( sfStream[ Result ].Buffer );
      if Assigned( sfStream[ Result ]._data ) Then
        FreeMem( sfStream[ Result ]._data );
    end;

  ext := u_StrUp( Extension );
  for i := managerSound.Count.Formats - 1 downto 0 do
    if ext = managerSound.Formats[ i ].Extension Then
      sfStream[ Result ]._decoder := managerSound.Formats[ i ].Decoder;

  if ( not Assigned( sfStream[ Result ]._decoder ) ) or ( not sfStream[ Result ]._decoder.OpenMem( sfStream[ Result ], Memory ) ) Then
    begin
      sfStream[ Result ]._decoder := nil;
      log_Add( 'Cannot play: "From Memory"' );
      exit;
    end;

  snd_PlayStream( Result, Loop, Volume );
end;

procedure snd_PauseStream( ID : Integer );
begin
  if ( not sndInitialized ) or ( not Assigned( sfStream[ ID ]._decoder ) ) or
     ( not sfStream[ ID ]._playing ) or ( sfStream[ ID ]._paused ) or ( sfStream[ ID ]._waiting ) Then exit;

  sfStream[ ID ]._paused := TRUE;

{$IFDEF USE_OPENAL}
  alSourcePause( sfSource[ ID ] );
{$ELSE}
  sfSource[ ID ].Stop();
{$ENDIF}
end;

procedure snd_StopStream( ID : Integer );
begin
  if ( not sndInitialized ) or ( not Assigned( sfStream[ ID ]._decoder ) ) or ( not sfStream[ ID ]._playing ) Then exit;

  sfStream[ ID ]._playing := FALSE;
{$IFDEF USE_OPENAL}
  alSourceStop( sfSource[ ID ] );
{$ELSE}
  sfSource[ ID ].Stop();
{$ENDIF}

  thread_EventSet( sfEvent[ ID ] );
end;

procedure snd_ResumeStream( ID : Integer );
begin
  if ( not sndInitialized ) or ( not Assigned( sfStream[ ID ]._decoder ) ) or
     ( not sfStream[ ID ]._playing ) or ( not sfStream[ ID ]._paused ) or ( sfStream[ ID ]._waiting ) Then exit;

  sfStream[ ID ]._paused := FALSE;
{$IFDEF USE_OPENAL}
  alSourcePlay( sfSource[ ID ] );
{$ELSE}
  sfSource[ ID ].Play( 0, 0, DSBPLAY_LOOPING );
{$ENDIF}
end;

procedure snd_SeekStream( ID : Integer; Milliseconds : Double );
begin
  if ( not sndInitialized ) or ( not Assigned( sfStream[ ID ]._decoder ) ) Then exit;

  thread_CSEnter( sfCS[ ID ] );
  sfSeek[ ID ] := Milliseconds;
  thread_EventSet( sfEvent[ ID ] );
  thread_CSLeave( sfCS[ ID ] );
end;

function snd_ProcStream( data : Pointer ) : LongInt;
  var
    id        : Integer;
    _end      : Boolean;
    bytesRead : Integer;
  {$IFDEF USE_OPENAL}
    processed : LongInt;
    buffer    : LongWord;
  {$ELSE}
    block1, block2 : Pointer;
    b1Size, b2Size : LongWord;
    position       : LongWord;
    fillSize       : LongWord;
  {$ENDIF}
begin
  Result := 0;
  id := PInteger( data )^;

  while sfStream[ id ]._playing do
    begin
      if not sndInitialized Then break;

      thread_EventWait( sfEvent[ id ], 100 );
      thread_EventReset( sfEvent[ id ] );
      while ( sfStream[ id ]._playing ) and ( sfStream[ id ]._paused ) do u_Sleep( 10 );

      thread_CSEnter( sfCS[ id ] );
      if sfSeek[ id ] > 0 Then
        begin
          sfStream[ id ]._decoder.Seek( sfStream[ id ], sfSeek[ id ] );
          sfSeek[ id ] := 0;

          {$IFDEF USE_OPENAL}
          alSourceStop( sfSource[ id ] );
          alSourceRewind( sfSource[ id ] );
          alSourcei( sfSource[ id ], AL_BUFFER, AL_NONE );

          for processed := 0 to 1 do
            begin
              bytesRead := sfStream[ id ]._decoder.Read( sfStream[ id ], sfStream[ id ].Buffer, sfStream[ id ].BufferSize, _end );
              if bytesRead <= 0 Then break;

              alBufferData( sfBuffers[ id, processed ], sfFormat[ sfStream[ id ].Channels ], sfStream[ id ].Buffer, bytesRead, sfStream[ id ].Frequency );
              alSourceQueueBuffers( sfSource[ id ], 1, @sfBuffers[ id, processed ] );

              if processed = 0 Then
                alSourcePlay( sfSource[ id ] );
            end;
          {$ELSE}
          sfSource[ id ].Stop();
          bytesRead := sfStream[ id ]._decoder.Read( sfStream[ id ], sfStream[ id ].Buffer, sfStream[ id ].BufferSize, _end );
          dsu_FillData( sfSource[ id ], sfStream[ ID ].Buffer, bytesRead );

          sfLastPos[ id ] := 0;
          sfSource[ id ].SetCurrentPosition( 0 );
          sfSource[ id ].Play( 0, 0, DSBPLAY_LOOPING );
          {$ENDIF}

          sfStream[ id ]._complete := sfSeek[ id ];
          sfStream[ id ]._lastTime := timer_GetTicks();
        end;
      thread_CSLeave( sfCS[ id ] );

      {$IFDEF USE_OPENAL}
      alGetSourcei( sfSource[ id ], AL_BUFFERS_PROCESSED, processed );
      while ( processed > 0 ) and sfStream[ id ]._playing do
        begin
          alSourceUnQueueBuffers( sfSource[ id ], 1, @buffer );

          bytesRead := sfStream[ id ]._decoder.Read( sfStream[ id ], sfStream[ id ].Buffer, sfStream[ id ].BufferSize, _end );
          alBufferData( buffer, sfFormat[ sfStream[ id ].Channels ], sfStream[ id ].Buffer, bytesRead, sfStream[ id ].Frequency );
          alSourceQueueBuffers( sfSource[ id ], 1, @buffer );

          if _end Then break;

          DEC( processed );
        end;
      {$ELSE}
      while LongWord( sfSource[ id ].GetCurrentPosition( @position, @b1Size ) ) = DSERR_BUFFERLOST do
        sfSource[ id ].Restore();

      fillSize := ( sfStream[ id ].BufferSize + position - sfLastPos[ id ] ) mod sfStream[ id ].BufferSize;

      block1 := nil;
      block2 := nil;
      b1Size := 0;
      b2Size := 0;

      if ( fillSize > 0 {some drivers don't know what is standard...} ) and ( sfSource[ id ].Lock( sfLastPos[ id ], fillSize, block1, b1Size, block2, b2Size, 0 ) = DS_OK ) Then
        begin
          sfLastPos[ id ] := position;

          bytesRead := sfStream[ id ]._decoder.Read( sfStream[ id ], block1, b1Size, _end );
          if ( b2Size <> 0 ) and ( not _end ) Then
            INC( bytesRead, sfStream[ ID ]._decoder.Read( sfStream[ id ], block2, b2Size, _end ) );

          sfSource[ id ].Unlock( block1, b1Size, block2, b2Size );
        end;
      {$ENDIF}
      if _end Then
        begin
          if sfStream[ id ].Loop Then
            begin
              sfStream[ id ]._decoder.Loop( sfStream[ id ] );
              sfStream[ id ]._complete := 0;
              sfStream[ id ]._lastTime := timer_GetTicks();
            end else
              begin
                {$IFNDEF USE_OPENAL}
                sfNotifyPos[ id ].dwOffset := bytesRead;
                ResetEvent( sfNotifyEvent[ id ] );
                WaitForSingleObject( sfNotifyEvent[ id ], INFINITE );
                sfSource[ id ].Stop();
                {$ENDIF}
                while ( sfStream[ id ]._playing ) and ( sfStream[ id ]._complete < sfStream[ id ].Length ) do
                  begin
                    sfStream[ id ]._complete := timer_GetTicks() - sfStream[ id ]._lastTime + sfStream[ id ]._complete;
                    sfStream[ id ]._lastTime := timer_GetTicks();
                    u_Sleep( 10 );
                  end;
                if sfStream[ id ]._complete > sfStream[ id ].Length Then
                  sfStream[ id ]._complete := sfStream[ id ].Length;
                sfStream[ id ]._playing := FALSE;
              end;
        end;
    end;

  thread_EventDestroy( sfEvent[ id ] );
  EndThread( 0 );
end;

end.
