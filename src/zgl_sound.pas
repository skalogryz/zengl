{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://andru-kun.inf.ua
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

interface

uses
  Windows,
  {$IFDEF USE_OPENAL}
  zgl_sound_openal,
  {$ELSE}
  zgl_sound_dsound,
  {$ENDIF}
  zgl_file,
  zgl_memory;

const
  SND_FORMAT_MONO8    = 1;
  SND_FORMAT_MONO16   = 2;
  SND_FORMAT_STEREO8  = 3;
  SND_FORMAT_STEREO16 = 4;

  SND_ALL           = -2;
  SND_STREAM        = -3;
  SND_ERROR         = {$IFDEF USE_OPENAL} 0 {$ELSE} nil {$ENDIF};

  SND_STATE_PLAYING = 1;
  SND_STATE_PERCENT = 2;
  SND_STATE_TIME    = 3;
  SND_INFO_LENGTH   = 4;

  SND_MAX           = 8;

type
  zglPSound        = ^zglTSound;
  zglPSoundStream  = ^zglTSoundStream;
  zglPSoundDecoder = ^zglTSoundDecoder;
  zglPSoundFormat  = ^zglTSoundFormat;
  zglPSoundManager = ^zglTSoundManager;

  zglTSoundChannel = record
    {$IFDEF USE_OPENAL}
    Source     : LongWord;
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
    _decoder   : zglPSoundDecoder;
    _playing   : Boolean;
    _paused    : Boolean;
    _waiting   : Boolean;
    _complete  : Double;
    _lastTime  : Double;

    Buffer     : Pointer;
    BufferSize : LongWord;

    Bits       : LongWord;
    Frequency  : LongWord;
    Channels   : LongWord;
    Length     : Double;

    Loop       : Boolean;
  end;

  zglTSoundDecoder = record
    Ext   : String;
    Open  : function( var Stream : zglTSoundStream; const FileName : String ) : Boolean;
    Read  : function( var Stream : zglTSoundStream; Buffer : Pointer; Bytes : LongWord; var _End : Boolean ) : LongWord;
    Loop  : procedure( var Stream : zglTSoundStream );
    Close : procedure( var Stream : zglTSoundStream );
  end;

  zglTSoundFormat = record
    Extension  : String;
    Decoder    : zglPSoundDecoder;
    FileLoader : procedure( const FileName : String; var Data : Pointer; var Size, Format, Frequency : LongWord );
    MemLoader  : procedure( const Memory : zglTMemory; var Data : Pointer; var Size, Format, Frequency : LongWord );
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
function  snd_LoadFromFile( const FileName : String; SourceCount : Integer = 8 ) : zglPSound;
function  snd_LoadFromMemory( const Memory : zglTMemory; const Extension : String; SourceCount : Integer = 8 ) : zglPSound;

function  snd_Play( Sound : zglPSound; Loop : Boolean = FALSE; X : Single = 0; Y : Single = 0; Z : Single = 0 ) : Integer;
procedure snd_Stop( Sound : zglPSound; ID : Integer );
procedure snd_SetPos( Sound : zglPSound; ID : Integer; X, Y, Z : Single );
procedure snd_SetVolume( Sound : zglPSound; ID : Integer; Volume : Single );
procedure snd_SetSpeed( Sound : zglPSound; ID : Integer; Speed : Single );
function  snd_Get( Sound : zglPSound; ID, What : Integer ) : Integer;

function  snd_PlayFile( const FileName : String; Loop : Boolean = FALSE ) : Integer;
procedure snd_PauseFile( ID : Integer );
procedure snd_StopFile( ID : Integer );
procedure snd_ResumeFile( ID : Integer );
function  snd_ProcFile( data : Pointer ) : LongInt; register;

var
  managerSound : zglTSoundManager;

  sndActive      : Boolean;
  sndInitialized : Boolean = FALSE;
  sndVolume      : Single  = 1;
  sndCanPlay     : Boolean = TRUE;
  sndCanPlayFile : Boolean = TRUE;
  sndAutoPaused  : Boolean;

  sfCanUse    : array[ 1..SND_MAX ] of Integer;
  sfStream    : array[ 1..SND_MAX ] of zglTSoundStream;
  sfVolume    : Single = 1;
  sfVolumes   : array[ 1..SND_MAX ] of Single;
  sfPositions : array[ 1..SND_MAX, 0..2 ] of Single;
  {$IFDEF USE_OPENAL}
  sfFormat   : array[ 1..2 ] of LongInt = ( AL_FORMAT_MONO16, AL_FORMAT_STEREO16 );
  sfBufCount : Integer = 4;
  sfSource   : array[ 1..SND_MAX ] of LongWord;
  sfBuffers  : array[ 1..SND_MAX, 0..3 ] of LongWord;
  {$ELSE}
  sfNotify      : array[ 1..SND_MAX ] of IDirectSoundNotify;
  sfNotifyPos   : array[ 1..SND_MAX ] of TDSBPositionNotify;
  sfNotifyEvent : array[ 1..SND_MAX ] of THandle;
  sfSource      : array[ 1..SND_MAX ] of IDirectSoundBuffer;
  sfLastPos     : array[ 1..SND_MAX ] of LongWord;
  {$ENDIF}

  sfThread : array[ 1..SND_MAX ] of LongWord;
  {$IFNDEF FPC}
  sfThreadID : array[ 1..SND_MAX ] of LongWord;
  {$ENDIF}

implementation
uses
  zgl_application,
  zgl_main,
  zgl_window,
  zgl_timers,
  zgl_log,
  zgl_utils;

function GetStatusPlaying( const Source : {$IFDEF USE_OPENAL} LongWord {$ELSE} IDirectSoundBuffer {$ENDIF} ) : Integer;
  var
    Status : {$IFDEF USE_OPENAL} LongInt {$ELSE} LongWord {$ENDIF};
begin
  {$IFDEF USE_OPENAL}
  alGetSourcei( Source, AL_SOURCE_STATE, Status );
  Result := Byte( Status = AL_PLAYING );
  {$ELSE}
  if not Assigned( Source ) Then
    begin
      Result := 0;
      exit;
    end;
  Source.GetStatus( Status );
  Result := Byte( Status and DSBSTATUS_PLAYING > 0 );
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
    if GetStatusPlaying( sfSource[ i ] ) = 1 Then
      begin
        sfCanUse[ i ] := 0;
        if timer_GetTicks() - sfStream[ i ]._lastTime >= 10 Then
          begin
            sfStream[ i ]._complete := timer_GetTicks() - sfStream[ i ]._lastTime + sfStream[ i ]._complete;
            if sfStream[ i ]._complete > sfStream[ i ].Length Then
              sfStream[ i ]._complete := sfStream[ i ].Length;
            sfStream[ i ]._lastTime := timer_GetTicks();
          end;
      end else
        begin
          if sfCanUse[ i ] < 100 Then
            INC( sfCanUse[ i ] );
          sfStream[ i ]._lastTime := timer_GetTicks();
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
            snd_ResumeFile( i );
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
              snd_PauseFile( i );
              sfStream[ i ]._waiting := TRUE;
            end;
      end;
end;

function snd_Init : Boolean;
  var
    i : Integer;
begin
  Result := FALSE;
{$IFDEF USE_OPENAL}
  log_Add( 'OpenAL: load ' + libopenal  );
  if not InitOpenAL Then
    begin
      log_Add( 'Error while loading ' + libopenal );
      exit;
    end;


  log_Add( 'OpenAL: opening "Generic Software"' );
  oalDevice := alcOpenDevice( 'Generic Software' );
  if not Assigned( oalDevice ) Then
    begin
      oalDevice := alcOpenDevice( nil );
      log_Add( 'OpenAL: opening default device - "' + alcGetString( nil, ALC_DEFAULT_DEVICE_SPECIFIER ) + '"' );
    end;
  if not Assigned( oalDevice ) Then
    begin
      log_Add( 'Cannot open sound device' );
      exit;
    end;

  log_Add( 'OpenAL: creating context' );
  oalContext := alcCreateContext( oalDevice, nil );
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
      alGenBuffers( sfBufCount, @sfBuffers[ i ] );
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
    sfCanUse[ i ] := 100;

  sndInitialized := TRUE;
  Result         := TRUE;
end;

procedure snd_Free;
  var
    i : Integer;
begin
  if not sndInitialized Then exit;

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
      alDeleteBuffers( sfBufCount, @sfBuffers[ i ] );
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

function snd_LoadFromFile( const FileName : String; SourceCount : Integer = 8 ) : zglPSound;
  var
    i   : Integer;
    fmt : LongWord;
  {$IFNDEF USE_OPENAL}
    buffDesc : zglTBufferDesc;
  {$ENDIF}
begin
  Result := nil;

  if not sndInitialized Then exit;

  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read "' + FileName + '"' );
      exit;
    end;
  Result := snd_Add( SourceCount );

  for i := managerSound.Count.Formats - 1 downto 0 do
    if u_StrUp( file_GetExtension( FileName ) ) = managerSound.Formats[ i ].Extension Then
      managerSound.Formats[ i ].FileLoader( FileName, Result.Data, Result.Size, fmt, Result.Frequency );

  if not Assigned( Result.Data ) Then
    begin
      log_Add( 'Unable to load sound: "' + FileName + '"' );
      snd_Del( Result );
      exit;
    end;

  case fmt of
    {$IFDEF USE_OPENAL}
    SND_FORMAT_MONO8: fmt := AL_FORMAT_MONO8;
    SND_FORMAT_MONO16: fmt := AL_FORMAT_MONO16;
    SND_FORMAT_STEREO8: fmt := AL_FORMAT_STEREO8;
    SND_FORMAT_STEREO16: fmt := AL_FORMAT_STEREO16;
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
  else
    begin
      log_Add( 'Unable to determinate sound format: "' + FileName + '"' );
      snd_Del( Result );
      exit;
    end;
  end;

{$IFDEF USE_OPENAL}
  alBufferData( Result.Buffer, fmt, Result.Data, Result.Size, Result.Frequency );
  FreeMem( Result.Data );
{$ELSE}
  with buffDesc do
    begin
      FormatCode     := 1;
      SampleRate     := Result.Frequency;
      BitsPerSample  := 16;
      BytesPerSample := ( BitsPerSample div 8 ) * ChannelNumber;
      BytesPerSecond := SampleRate * BytesPerSample;
      cbSize         := SizeOf( buffDesc );
    end;

  dsu_CreateBuffer( Result.Channel[ 0 ].Source, Result.Size, @buffDesc );
  dsu_FillData( Result.Channel[ 0 ].Source, Result.Data, Result.Size );
  for i := 1 to Result.SourceCount - 1 do
    dsDevice.DuplicateSoundBuffer( Result.Channel[ 0 ].Source, Result.Channel[ i ].Source );
{$ENDIF}

  log_Add( 'Sound loaded: "' + FileName + '"' );
end;

function snd_LoadFromMemory( const Memory : zglTMemory; const Extension : String; SourceCount : Integer = 8 ) : zglPSound;
  var
    i   : Integer;
    fmt : LongWord;
  {$IFNDEF USE_OPENAL}
    buffDesc : zglTBufferDesc;
  {$ENDIF}
begin
  Result := nil;

  if not sndInitialized Then exit;

  Result := snd_Add( SourceCount );

  for i := managerSound.Count.Formats - 1 downto 0 do
    if u_StrUp( Extension ) = managerSound.Formats[ i ].Extension Then
      managerSound.Formats[ i ].MemLoader( Memory, Result.Data, Result.Size, fmt, Result.Frequency );

  if not Assigned( Result.Data ) Then
    begin
      log_Add( 'Unable to load sound: From Memory' );
      snd_Del( Result );
      exit;
    end;

  case fmt of
    {$IFDEF USE_OPENAL}
    SND_FORMAT_MONO8: fmt := AL_FORMAT_MONO8;
    SND_FORMAT_MONO16: fmt := AL_FORMAT_MONO16;
    SND_FORMAT_STEREO8: fmt := AL_FORMAT_STEREO8;
    SND_FORMAT_STEREO16: fmt := AL_FORMAT_STEREO16;
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
  else
    begin
      log_Add( 'Unable to determinate sound format: From memory' );
      snd_Del( Result );
      exit;
    end;
  end;

{$IFDEF USE_OPENAL}
  alBufferData( Result.Buffer, fmt, Result.Data, Result.Size, Result.Frequency );
  FreeMem( Result.Data );
{$ELSE}
  with buffDesc do
    begin
      FormatCode     := 1;
      SampleRate     := Result.Frequency;
      BitsPerSample  := 16;
      BytesPerSample := ( BitsPerSample div 8 ) * ChannelNumber;
      BytesPerSecond := SampleRate * BytesPerSample;
      cbSize         := SizeOf( buffDesc );
    end;

  dsu_CreateBuffer( Result.Channel[ 0 ].Source, Result.Size, @buffDesc );
  dsu_FillData( Result.Channel[ 0 ].Source, Result.Data, Result.Size );
  for i := 1 to Result.SourceCount - 1 do
    dsDevice.DuplicateSoundBuffer( Result.Channel[ 0 ].Source, Result.Channel[ i ].Source );
{$ENDIF}
end;

function snd_Play( Sound : zglPSound; Loop : Boolean = FALSE; X : Single = 0; Y : Single = 0; Z : Single = 0 ) : Integer;
  var
    i : Integer;
    {$IFNDEF USE_OPENAL}
    dsError : HRESULT;
    status  : LongWord;
    volume  : Single;
    {$ELSE}
    j       : Integer;
    {$ENDIF}
begin
  Result := -1;

  if ( not Assigned( Sound ) ) or
     ( not sndInitialized ) or
     ( not sndCanPlay ) Then exit;

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
  Sound.Channel[ Result ].Volume     := sndVolume;

  alSourcei ( Sound.Channel[ Result ].Source, AL_BUFFER,    Sound.Buffer );
  alSourcefv( Sound.Channel[ Result ].Source, AL_POSITION,  @Sound.Channel[ Result ].Position );
  alSourcefv( Sound.Channel[ Result ].Source, AL_VELOCITY,  @oalVelocity[ 0 ] );
  alSourcef ( Sound.Channel[ Result ].Source, AL_GAIN,      sndVolume );
  alSourcei ( Sound.Channel[ Result ].Source, AL_FREQUENCY, Sound.Frequency );

  if Loop Then
    alSourcei( Sound.Channel[ Result ].Source, AL_LOOPING, AL_TRUE )
  else
    alSourcei( Sound.Channel[ Result ].Source, AL_LOOPING, AL_FALSE );

  alSourcePlay( Sound.Channel[ Result ].Source );
{$ELSE}
  for i := 0 to Sound.SourceCount - 1 do
    begin
      dsError := Sound.Channel[ i ].Source.GetStatus( status );
      if dsError <> DS_OK Then status := 0;
      if ( status and DSBSTATUS_PLAYING ) = 0 Then
        begin
          if ( status and DSBSTATUS_BUFFERLOST ) <> 0 Then
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
  Sound.Channel[ Result ].Volume     := sndVolume;

  Sound.Channel[ Result ].Source.SetPan( dsu_CalcPos( X, Y, Z, volume ) );
  Sound.Channel[ Result ].Source.SetVolume( dsu_CalcVolume( volume * sndVolume ) );
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
          if sfSource[ LongWord( Sound ) ] = SND_ERROR Then exit;
          sfPositions[ LongWord( Sound ), 0 ] := X;
          sfPositions[ LongWord( Sound ), 1 ] := Y;
          sfPositions[ LongWord( Sound ), 2 ] := Z;

          {$IFDEF USE_OPENAL}
          alSourcefv( LongWord( Sound ), AL_POSITION, @sfPositions[ LongWord( Sound ), 0 ] );
          {$ELSE}
          sfSource[ LongWord( Sound ) ].SetPan( dsu_CalcPos( sfPositions[ LongWord( Sound ), 0 ],
                                                             sfPositions[ LongWord( Sound ), 1 ],
                                                             sfPositions[ LongWord( Sound ), 2 ], vol ) );
          sfSource[ LongWord( Sound ) ].SetVolume( dsu_CalcVolume( vol * sfVolumes[ LongWord( Sound ) ] ) );
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

  if ( Sound = nil ) and ( ID = SND_STREAM ) Then
    sfVolume := Volume
  else
    if ( not Assigned( Sound ) ) and ( ID = SND_ALL ) Then
      sndVolume := Volume;

  if ( ID = SND_STREAM ) Then
    begin
      if Assigned( Sound ) Then
        begin
          if sfSource[ LongWord( Sound ) ] = SND_ERROR Then exit;
          sfVolumes[ LongWord( Sound ) ] := Volume;

          {$IFDEF USE_OPENAL}
          alSourcef( sfSource[ LongWord( Sound ) ], AL_GAIN, Volume );
          {$ELSE}
          sfSource[ LongWord( Sound ) ].SetPan( dsu_CalcPos( sfPositions[ LongWord( Sound ), 0 ],
                                                             sfPositions[ LongWord( Sound ), 1 ],
                                                             sfPositions[ LongWord( Sound ), 2 ], vol ) );
          sfSource[ LongWord( Sound ) ].SetVolume( dsu_CalcVolume( vol * sfVolumes[ LongWord( Sound ) ] ) );
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
          if sfSource[ LongWord( Sound ) ] = SND_ERROR Then exit;

          {$IFDEF USE_OPENAL}
          alSourcef( sfSource[ LongWord( Sound ) ], AL_PITCH, Speed );
          {$ELSE}
          sfSource[ LongWord( Sound ) ].SetFrequency( Round( sfStream[ LongWord( Sound ) ].Frequency * Speed ) );
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
      case What of
        SND_STATE_PLAYING: Result := GetStatusPlaying( sfSource[ LongWord( Sound ) ] );
        SND_STATE_TIME: Result := Round( sfStream[ LongWord( Sound ) ]._complete );
        SND_STATE_PERCENT: Result := Round( 100 / sfStream[ LongWord( Sound ) ].Length * sfStream[ LongWord( Sound ) ]._complete );
        SND_INFO_LENGTH: Result := Round( sfStream[ LongWord( Sound ) ].Length );
      end;
    end else
      case What of
        SND_STATE_PLAYING: Result := GetStatusPlaying( Sound.Channel[ ID ].Source );
        SND_INFO_LENGTH: Result := Round( Sound.Length );
      end;
end;

function snd_GetStreamID : Integer;
  var
    i : Integer;
begin
  for i := 1 to SND_MAX do
    if ( not sfStream[ i ]._playing ) and ( sfCanUse[ i ] = 100 ) Then
      begin
        Result := i;
        exit;
      end;
  Result := -1;
end;

function snd_PlayFile( const FileName : String; Loop : Boolean = FALSE ) : Integer;
  var
    i         : Integer;
    ext       : String;
    _end      : Boolean;
    bytesRead : Integer;
    {$IFNDEF USE_OPENAL}
    buffDesc : zglTBufferDesc;
    {$ENDIF}
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

  for i := managerSound.Count.Formats - 1 downto 0 do
    begin
      ext := u_StrUp( file_GetExtension( FileName ) );
      if ext = managerSound.Formats[ i ].Extension Then
        sfStream[ Result ]._decoder := managerSound.Formats[ i ].Decoder;
    end;

  if Assigned( sfStream[ Result ]._decoder ) Then
    sfStream[ Result ].Loop := Loop;

  if ( not Assigned( sfStream[ Result ]._decoder ) ) or
     ( not sfStream[ Result ]._decoder.Open( sfStream[ Result ], FileName ) ) Then
    begin
      sfStream[ Result ]._decoder := nil;
      log_Add( 'Cannot play: "' + FileName + '"' );
      exit;
    end;

{$IFDEF USE_OPENAL}
  alSourceStop( sfSource[ Result ] );
  alSourceRewind( sfSource[ Result ] );
  alSourcei( sfSource[ Result ], AL_BUFFER, AL_NONE );

  for i := 0 to sfBufCount - 1 do
    begin
      bytesRead := sfStream[ Result ]._decoder.Read( sfStream[ Result ], sfStream[ Result ].Buffer, sfStream[ Result ].BufferSize, _end );
      if bytesRead <= 0 Then break;

      alBufferData( sfBuffers[ Result, i ], sfFormat[ sfStream[ Result ].Channels ], sfStream[ Result ].Buffer, bytesRead, sfStream[ Result ].Frequency );
      alSourceQueueBuffers( sfSource[ Result ], 1, @sfBuffers[ Result, i ] );
    end;

  alSourcei( sfSource[ Result ], AL_LOOPING, AL_FALSE );
  alSourcePlay( sfSource[ Result ] );
  alSourcef( sfSource[ Result ], AL_GAIN, sfVolume );
  alSourcef( sfSource[ Result ], AL_FREQUENCY, sfStream[ Result ].Frequency );
{$ELSE}
  with buffDesc do
    begin
      FormatCode     := 1;
      ChannelNumber  := sfStream[ Result ].Channels;
      SampleRate     := sfStream[ Result ].Frequency;
      BitsPerSample  := sfStream[ Result ].Bits;
      BytesPerSample := ( BitsPerSample div 8 ) * ChannelNumber;
      BytesPerSecond := SampleRate * BytesPerSample;
      cbSize         := SizeOf( buffDesc );
    end;
  if Assigned( sfSource[ Result ] ) Then sfSource[ Result ] := nil;
  dsu_CreateBuffer( sfSource[ Result ], sfStream[ Result ].BufferSize, @buffDesc );
  bytesRead := sfStream[ Result ]._decoder.Read( sfStream[ Result ], sfStream[ Result ].Buffer, sfStream[ Result ].BufferSize, _end );
  dsu_FillData( sfSource[ Result ], sfStream[ Result ].Buffer, bytesRead );

  sfNotify[ Result ] := nil;
  sfSource[ Result ].QueryInterface( IDirectSoundNotify, sfNotify[ Result ] );
  CloseHandle( sfNotifyEvent[ Result ] );
  sfNotifyEvent[ Result ] := CreateEvent( nil, FALSE, FALSE, nil );
  sfNotifyPos[ Result ].dwOffset := 0;
  sfNotifyPos[ Result ].hEventNotify := sfNotifyEvent[ Result ];
  sfNotify[ Result ].SetNotificationPositions( 1, @sfNotifyPos[ Result ] );

  sfLastPos[ Result ] := 0;
  sfSource[ Result ].SetCurrentPosition( 0 );
  sfSource[ Result ].Play( 0, 0, DSBPLAY_LOOPING );
  sfSource[ Result ].SetVolume( dsu_CalcVolume( sfVolume ) );
  sfSource[ Result ].SetFrequency( sfStream[ Result ].Frequency );
{$ENDIF}

  sfStream[ Result ]._playing  := TRUE;
  sfStream[ Result ]._paused   := FALSE;
  sfStream[ Result ]._waiting  := FALSE;
  sfStream[ Result ]._complete := 0;
  sfStream[ Result ]._lastTime := timer_GetTicks;
{$IFDEF FPC}
  sfThread[ Result ] := LongWord( BeginThread( @snd_ProcFile, Pointer( Result ) ) );
{$ELSE}
  sfThread[ Result ] := BeginThread( nil, 0, @snd_ProcFile, Pointer( Result ), 0, sfThreadID[ Result ] );
{$ENDIF}
end;

procedure snd_PauseFile( ID : Integer );
begin
  if ( not sndInitialized ) or ( not Assigned( sfStream[ ID ]._decoder ) ) or
     ( not sfStream[ ID ]._playing ) or ( sfStream[ ID ]._paused ) or ( sfStream[ ID ]._waiting ) Then exit;

  sfStream[ ID ]._paused := TRUE;

{$IFDEF USE_OPENAL}
  alSourcePause( sfSource[ ID ] );
{$ELSE}
  SuspendThread( sfThread[ ID ] );
  sfSource[ ID ].Stop();
{$ENDIF}
end;

procedure snd_StopFile( ID : Integer );
begin
  if ( not sndInitialized ) or ( not Assigned( sfStream[ ID ]._decoder ) ) or ( not sfStream[ ID ]._playing ) Then exit;

  sfStream[ ID ]._playing := FALSE;
{$IFDEF USE_OPENAL}
  alSourceStop( sfSource[ ID ] );
{$ELSE}
  sfSource[ ID ].Stop();
{$ENDIF}
end;

procedure snd_ResumeFile( ID : Integer );
begin
  if ( not sndInitialized ) or ( not Assigned( sfStream[ ID ]._decoder ) ) or
     ( not sfStream[ ID ]._playing ) or ( not sfStream[ ID ]._paused ) or ( sfStream[ ID ]._waiting ) Then exit;

  sfStream[ ID ]._paused := FALSE;
{$IFDEF USE_OPENAL}
  alSourcePlay( sfSource[ ID ] );
{$ELSE}
  sfSource[ ID ].Play( 0, 0, DSBPLAY_LOOPING );
  ResumeThread( sfThread[ ID ] );
{$ENDIF}
end;

function snd_ProcFile( data : Pointer ) : LongInt; register;
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
  id := LongWord( data );

{$IFDEF USE_OPENAL}
  processed := 0;
  while ( processed < 1 ) and sfStream[ id ]._playing do
    alGetSourcei( sfSource[ id ], AL_BUFFERS_PROCESSED, processed );
{$ENDIF}
  while appWork and sfStream[ id ]._playing do
    begin
      if not sndInitialized Then break;

      u_Sleep( 100 );
      if ( not appWork ) or ( not sfStream[ id ]._playing ) Then break;
      {$IFDEF USE_OPENAL}
      alGetSourcei( sfSource[ id ], AL_BUFFERS_PROCESSED, processed );
      while appWork and ( processed > 0 ) and sfStream[ id ]._playing do
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

      if sfSource[ id ].Lock( sfLastPos[ id ], fillSize, block1, b1Size, block2, b2Size, 0 ) <> DS_OK Then break;
      sfLastPos[ id ] := position;

      bytesRead := sfStream[ id ]._decoder.Read( sfStream[ id ], block1, b1Size, _end );
      if ( b2Size <> 0 ) and ( not _end ) Then
        INC( bytesRead, sfStream[ ID ]._decoder.Read( sfStream[ id ], block2, b2Size, _end ) );

      sfSource[ id ].Unlock( block1, b1Size, block2, b2Size );
      {$ENDIF}
      if sfStream[ id ]._complete >= sfStream[ id ].Length Then
        begin
          sfStream[ id ]._complete := 0;
          sfStream[ id ]._lastTime := timer_GetTicks();
        end;
      if _end Then
        begin
          if sfStream[ id ].Loop Then
            begin
              sfStream[ id ]._decoder.Loop( sfStream[ id ] );
            end else
              begin
                {$IFNDEF USE_OPENAL}
                sfNotifyPos[ id ].dwOffset := bytesRead;
                ResetEvent( sfNotifyEvent[ id ] );
                WaitForSingleObject( sfNotifyEvent[ id ], INFINITE );
                sfSource[ id ].Stop();
                {$ENDIF}
                while sfStream[ id ]._complete < sfStream[ id ].Length do
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

  EndThread( 0 );
end;

end.
