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
  {$IFDEF LINUX_OR_DARWIN}
  {$LINKLIB pthread}
  cthreads,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF USE_OPENAL}
  zgl_sound_openal,
  {$ELSE}
  zgl_sound_dsound,
  {$ENDIF}
  zgl_types,
  zgl_file,
  zgl_memory;

const
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
    Frequency  : Integer;
    Volume     : Single;
    Position   : record
      X, Y, Z : Single;
                 end;
  end;

  zglTSound = record
    Buffer      : LongWord;
    SourceCount : LongWord;
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
    _complete  : Double;
    _lastTime  : Double;

    Buffer     : Pointer;
    BufferSize : LongWord;

    Frequency  : LongWord;
    Channels   : LongWord;
    Length     : Double;

    Loop       : Boolean;
  end;

  zglTSoundDecoder = record
    Ext   : String;
    Open  : function( var Stream : zglTSoundStream; const FileName : String ) : Boolean;
    Read  : function( var Stream : zglTSoundStream; const Buffer : Pointer; const Bytes : LongWord; var _End : Boolean ) : LongWord;
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
      Items   : LongWord;
      Formats : LongWord;
              end;
    First   : zglTSound;
    Formats : array of zglTSoundFormat;
  end;

procedure snd_MainLoop;
function  snd_Init : Boolean;
procedure snd_Free;
function  snd_Add( const SourceCount : Integer ) : zglPSound;
procedure snd_Del( var Sound : zglPSound );
function  snd_LoadFromFile( const FileName : String; const SourceCount : Integer = 8 ) : zglPSound;
function  snd_LoadFromMemory( const Memory : zglTMemory; const Extension : String; const SourceCount : Integer = 8 ) : zglPSound;

function  snd_Play( const Sound : zglPSound; const Loop : Boolean = FALSE; const X : Single = 0; const Y : Single = 0; const Z : Single = 0 ) : Integer;
procedure snd_Stop( const Sound : zglPSound; const ID : Integer );
procedure snd_SetPos( const Sound : zglPSound; const ID : Integer; const X, Y, Z : Single );
procedure snd_SetVolume( const Sound : zglPSound; const ID : Integer; const Volume : Single );
procedure snd_SetFrequency( const Sound : zglPSound; const ID, Frequency : Integer );
procedure snd_SetFrequencyCoeff( const Sound : zglPSound; const ID : Integer; const Coefficient : Single );
function  snd_Get( const Sound : zglPSound; const ID, What : Integer ) : Integer;

function  snd_GetStreamID : Integer;
function  snd_PlayFile( const FileName : String; const Loop : Boolean = FALSE ) : Integer;
procedure snd_PauseFile( const ID : Integer );
procedure snd_StopFile( const ID : Integer );
procedure snd_ResumeFile( const ID : Integer );
function  snd_ProcFile( data : Pointer ) : {$IFDEF WINDOWS} PInteger; stdcall; {$ELSE} LongInt; register; {$ENDIF}

var
  managerSound : zglTSoundManager;

  sndActive      : Boolean;
  sndInitialized : Boolean = FALSE;
  sndVolume      : Single  = 1;
  sndCanPlay     : Boolean = TRUE;
  sndCanPlayFile : Boolean = TRUE;
  sndAutoPaused  : Boolean;

  {$IFNDEF USE_OPENAL}
  sfCS        : TRTLCriticalSection;
  {$ENDIF}
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
  sfSource  : array[ 1..SND_MAX ] of IDirectSoundBuffer;
  sfLastPos : array[ 1..SND_MAX ] of LongWord;
  {$ENDIF}

  {$IFDEF LINUX_OR_DARWIN}
  sfThread  : array[ 1..SND_MAX ] of LongWord;
  {$ENDIF}
  {$IFDEF WINDOWS}
  sfThread   : array[ 1..SND_MAX ] of LongWord;
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

  if app_Focus Then
    begin
      if sndAutoPaused Then
        begin
          sndAutoPaused := FALSE;
          {$IFDEF USE_OPENAL}
          for i := 0 to length( oal_Sources ) - 1 do
            if oal_SrcState[ i ] = AL_PLAYING Then
              alSourcePlay( oal_Sources[ i ] );
          {$ELSE}
          {$ENDIF}
          for i := 1 to SND_MAX do
            if sfStream[ i ]._playing and ( not sfStream[ i ]._paused ) Then
              begin
                sfStream[ i ]._paused := TRUE;
                snd_ResumeFile( i );
              end;
        end;
    end else
      if not sndAutoPaused Then
        begin
          sndAutoPaused := TRUE;
          {$IFDEF USE_OPENAL}
          for i := 0 to length( oal_Sources ) - 1 do
            begin
              alGetSourcei( oal_Sources[ i ], AL_SOURCE_STATE, z );
              if z = AL_PLAYING Then
                alSourcePause( oal_Sources[ i ] );
              oal_SrcState[ i ] := z;
            end;
          {$ELSE}
          {$ENDIF}
          for i := 1 to SND_MAX do
            if sfStream[ i ]._playing and ( not sfStream[ i ]._paused ) Then
              begin
                snd_PauseFile( i );
                sfStream[ i ]._paused := FALSE;
              end;
        end;
end;

function snd_Init;
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

  {$IFDEF LINUX}
  log_Add( 'OpenAL: opening "ALSA Software"' );
  oal_Device := alcOpenDevice( 'ALSA Software' );
  {$ENDIF}
  {$IFDEF WINDOWS}
  log_Add( 'OpenAL: opening "Generic Software"' );
  oal_Device := alcOpenDevice( 'Generic Software' );
  {$ENDIF}
  {$IFDEF DARWIN}
  log_Add( 'OpenAL: opening "CoreAudio Software"' );
  oal_Device := alcOpenDevice( 'CoreAudio Software' );
  {$ENDIF}
  if not Assigned( oal_Device ) Then
    begin
      oal_Device := alcOpenDevice( nil );
      log_Add( 'OpenAL: opening default device - "' + alcGetString( nil, ALC_DEFAULT_DEVICE_SPECIFIER ) + '"' );
    end;
  if not Assigned( oal_Device ) Then
    begin
      log_Add( 'Cannot open sound device' );
      exit;
    end;

  log_Add( 'OpenAL: create context' );
  oal_Context := alcCreateContext( oal_Device, nil );
  if not Assigned( oal_Context ) Then
    begin
      log_Add( 'Cannot create sound context' );
      exit;
    end;

  if alcMakeContextCurrent( oal_Context ) Then
    log_Add( 'OpenAL: sound system initialized successful' )
  else
    begin
      log_Add( 'OpenAL: cannot set current context' );
      exit;
    end;

  alListenerfv( AL_POSITION,    @oal_Position );
  alListenerfv( AL_VELOCITY,    @oal_Velocity );
  alListenerfv( AL_ORIENTATION, @oal_Orientation );

  for i := 1 to SND_MAX do
    begin
      alGenSources( 1, @sfSource[ i ] );
      alGenBuffers( sfBufCount, @sfBuffers[ i ] );
    end;

  i := 64;
  SetLength( oal_Sources, i );
  alGenSources( i, @oal_Sources[ 0 ] );
  while alcGetError( nil ) <> AL_NO_ERROR do
    begin
      DEC( i, 8 );
      if i = 0 Then break;
      SetLength( oal_Sources, i );
      alGenSources( i, @oal_Sources[ 0 ] );
    end;
  SetLength( oal_SrcPtrs, i );
  SetLength( oal_SrcState, i );

  log_Add( 'OpenAL: generate ' + u_IntToStr( length( oal_Sources ) ) + ' source' );
{$ELSE}
  log_Add( 'DirectSound: load DSound.dll' );
  if not InitDSound() Then
    log_Add( 'DirectSound: Error while loading libraries' );

  if DirectSoundCreate( nil, ds_Device, nil ) <> DS_OK Then
    begin
      FreeDSound();
      log_Add( 'DirectSound: Error while calling DirectSoundCreate' );
      exit;
    end;

  if ds_Device.SetCooperativeLevel( wnd_Handle, DSSCL_PRIORITY ) <> DS_OK Then
    log_Add( 'DirectSound: Can''t SetCooperativeLevel' );

  log_Add( 'DirectSound: sound system initialized successful' );
{$ENDIF}

  for i := 1 to SND_MAX do
    sfCanUse[ i ] := 100;

{$IFNDEF USE_OPENAL}
  Windows.InitializeCriticalSection( sfCS );
{$ENDIF}

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
          FreeMemory( sfStream[ i ].Buffer );
        if Assigned( sfStream[ i ]._data ) Then
          FreeMemory( sfStream[ i ]._data );
      end;

{$IFDEF USE_OPENAL}
  for i := 1 to SND_MAX do
    begin
      alDeleteSources( 1, @sfSource[ i ] );
      alDeleteBuffers( sfBufCount, @sfBuffers[ i ] );
    end;
  alDeleteSources( length( oal_Sources ), @oal_Sources[ 0 ] );
  SetLength( oal_Sources, 0 );
  SetLength( oal_SrcPtrs, 0 );
  SetLength( oal_SrcState, 0 );

  log_Add( 'OpenAL: destroy current sound context' );
  alcDestroyContext( oal_Context );
  log_Add( 'OpenAL: close sound device' );
  alcCloseDevice( oal_Device );
  log_Add( 'OpenAL: sound system finalized successful' );
  FreeOpenAL();
{$ELSE}
  for i := 1 to SND_MAX do
    sfSource[ i ]  := nil;
  ds_Device := nil;

  FreeDSound();
  log_Add( 'DirectSound: sound system finalized successful' );
{$ENDIF}

{$IFNDEF USE_OPENAL}
  Windows.DeleteCriticalSection( sfCS );
{$ENDIF}
end;

function snd_Add;
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

procedure snd_Del;
  {$IFNDEF USE_OPENAL}
  var
    i : Integer;
  {$ENDIF}
begin
  if not Assigned( Sound ) Then exit;

{$IFDEF USE_OPENAL}
  alDeleteBuffers( 1, @Sound.Buffer );
{$ELSE}
  FreeMemory( Sound.Data );
  for i := 0 to Sound.SourceCount - 1 do
    Sound.Channel[ i ].Source := nil;
{$ENDIF}
  SetLength( Sound.Channel, 0 );

  if Assigned( Sound.prev ) Then
    Sound.prev.next := Sound.next;
  if Assigned( Sound.next ) Then
    Sound.next.prev := Sound.prev;

  FreeMemory( Sound );
  Sound := nil;

  DEC( managerSound.Count.Items );
end;

function snd_LoadFromFile;
  var
    i   : Integer;
    f   : LongWord;
    ext : String;
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
    begin
      file_GetExtension( FileName, ext );
      if u_StrUp( ext ) = managerSound.Formats[ i ].Extension Then
        managerSound.Formats[ i ].FileLoader( FileName, Result.Data, Result.Size, f, Result.Frequency );
    end;

  if not Assigned( Result.Data ) Then
    begin
      log_Add( 'Cannot load sound: "' + FileName + '"' );
      snd_Del( Result );
      exit;
    end;

{$IFDEF USE_OPENAL}
  alBufferData( Result.Buffer, f, Result.Data, Result.Size, Result.Frequency );
  FreeMemory( Result.Data );
{$ELSE}
  dsu_CreateBuffer( Result.Channel[ 0 ].Source, Result.Size, Pointer( f ) );
  dsu_FillData( Result.Channel[ 0 ].Source, Result.Data, Result.Size );
  for i := 1 to Result.SourceCount - 1 do
    ds_Device.DuplicateSoundBuffer( Result.Channel[ 0 ].Source, Result.Channel[ i ].Source );
{$ENDIF}

  log_Add( 'Successful loading of sound: "' + FileName + '"' );
end;

function snd_LoadFromMemory;
  var
    i : Integer;
    f : LongWord;
begin
  Result := nil;

  if not sndInitialized Then exit;

  Result := snd_Add( SourceCount );

  for i := managerSound.Count.Formats - 1 downto 0 do
    if u_StrUp( Extension ) = managerSound.Formats[ i ].Extension Then
      managerSound.Formats[ i ].MemLoader( Memory, Result.Data, Result.Size, f, Result.Frequency );

  if not Assigned( Result.Data ) Then
    begin
      log_Add( 'Cannot load sound: From Memory' );
      snd_Del( Result );
      exit;
    end;

{$IFDEF USE_OPENAL}
  alBufferData( Result.Buffer, f, Result.Data, Result.Size, Result.Frequency );
  FreeMemory( Result.Data );
{$ELSE}
  dsu_CreateBuffer( Result.Channel[ 0 ].Source, Result.Size, Pointer( f ) );
  dsu_FillData( Result.Channel[ 0 ].Source, Result.Data, Result.Size );
  for i := 1 to Result.SourceCount - 1 do
    ds_Device.DuplicateSoundBuffer( Result.Channel[ 0 ].Source, Result.Channel[ i ].Source );
{$ENDIF}
end;

function snd_Play;
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
  alSourcefv( Sound.Channel[ Result ].Source, AL_VELOCITY,  @oal_Velocity[ 0 ] );
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
                ds_Device.DuplicateSoundBuffer( Sound.Channel[ 0 ].Source, Sound.Channel[ i ].Source );
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

procedure snd_Stop;
  var
    i, j : Integer;
    snd : zglPSound;
  procedure Stop( const Sound : zglPSound; const ID : Integer );
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

procedure snd_SetPos;
  var
    i, j : Integer;
    snd  : zglPSound;
    {$IFNDEF USE_OPENAL}
    vol  : Single;
    {$ENDIF}
  procedure SetPos( const Sound : zglPSound; const ID : Integer; const X, Y, Z : Single );
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

procedure snd_SetVolume;
  var
    i, j : Integer;
    snd  : zglPSound;
    {$IFNDEF USE_OPENAL}
    vol  : Single;
    {$ENDIF}
  procedure SetVolume( const Sound : zglPSound; const ID : Integer; const Volume : Single );
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

procedure snd_SetFrequency;
  var
    i, j : Integer;
    snd  : zglPSound;
  procedure SetFrequency( const Sound : zglPSound; const ID, Frequency : Integer );
  begin
    Sound.Channel[ ID ].Frequency := Frequency;

    if Sound.Channel[ ID ].Source <> SND_ERROR Then
      {$IFDEF USE_OPENAL}
      alSourcei( Sound.Channel[ ID ].Source, AL_FREQUENCY, Sound.Channel[ ID ].Frequency );
      {$ELSE}
      Sound.Channel[ ID ].Source.SetFrequency( Sound.Channel[ ID ].Frequency );
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
          alSourcef( sfSource[ LongWord( Sound ) ], AL_FREQUENCY, Frequency );
          {$ELSE}
          sfSource[ LongWord( Sound ) ].SetFrequency( Frequency );
          {$ENDIF}
        end else
          for i := 1 to SND_MAX do
            if sfSource[ i ] <> SND_ERROR Then
              {$IFDEF USE_OPENAL}
              alSourcef( sfSource[ i ], AL_FREQUENCY, Frequency );
              {$ELSE}
              sfSource[ i ].SetFrequency( Frequency );
              {$ENDIF}
      exit;
    end;

  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.SourceCount - 1 do
            SetFrequency( Sound, i, Frequency );
        end else
          if ID >= 0 Then
            SetFrequency( Sound, ID, Frequency );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.SourceCount - 1 do
                SetFrequency( snd, j, Frequency );
              snd := snd.next;
            end;
        end;
end;

procedure snd_SetFrequencyCoeff;
  var
    i, j : Integer;
    snd  : zglPSound;
  procedure SetFrequency( const Sound : zglPSound; const ID, Frequency : Integer );
  begin
    Sound.Channel[ ID ].Frequency := Frequency;

    if Sound.Channel[ ID ].Source <> SND_ERROR Then
      {$IFDEF USE_OPENAL}
      alSourcei( Sound.Channel[ ID ].Source, AL_FREQUENCY, Sound.Channel[ ID ].Frequency );
      {$ELSE}
      Sound.Channel[ ID ].Source.SetFrequency( Sound.Channel[ ID ].Frequency );
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
          alSourcef( sfSource[ LongWord( Sound ) ], AL_FREQUENCY, Round( sfStream[ LongWord( Sound ) ].Frequency * Coefficient ) );
          {$ELSE}
          sfSource[ LongWord( Sound ) ].SetFrequency( Round( sfStream[ LongWord( Sound ) ].Frequency * Coefficient ) );
          {$ENDIF}
        end else
          for i := 1 to SND_MAX do
            if sfSource[ i ] <> SND_ERROR Then
              {$IFDEF USE_OPENAL}
              alSourcef( sfSource[ i ], AL_FREQUENCY, Round( sfStream[ i ].Frequency * Coefficient ) );
              {$ELSE}
              sfSource[ i ].SetFrequency( Round( sfStream[ i ].Frequency * Coefficient ) );
              {$ENDIF}
      exit;
    end;

  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.SourceCount - 1 do
            SetFrequency( Sound, i, Round( Sound.Frequency * Coefficient ) );
        end else
          if ID >= 0 Then
            SetFrequency( Sound, ID, Round( Sound.Frequency * Coefficient ) );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.SourceCount - 1 do
                SetFrequency( snd, j, Round( snd.Frequency * Coefficient ) );
              snd := snd.next;
            end;
        end;
end;

function snd_Get;
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

function snd_GetStreamID;
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

function snd_PlayFile;
  var
    i         : Integer;
    ext       : String;
    _end      : Boolean;
    bytesRead : Integer;
    {$IFNDEF USE_OPENAL}
    buffDesc  : zglTBufferDesc;
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
        FreeMemory( sfStream[ Result ].Buffer );
      if Assigned( sfStream[ Result ]._data ) Then
        FreeMemory( sfStream[ Result ]._data );
      {$IFDEF WINDOWS}
      CloseHandle( sfThread[ Result ] );
      {$ENDIF}
    end;

  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read "' + FileName + '"' );
      exit;
    end;

  for i := managerSound.Count.Formats - 1 downto 0 do
    begin
      file_GetExtension( FileName, ext );
      if u_StrUp( ext ) = managerSound.Formats[ i ].Extension Then
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
      BitsPerSample  := 16;
      BytesPerSample := ( BitsPerSample div 8 ) * ChannelNumber;
      BytesPerSecond := SampleRate * BytesPerSample;
      cbSize         := SizeOf( buffDesc );
    end;
  if Assigned( sfSource[ Result ] ) Then sfSource[ Result ] := nil;
  dsu_CreateBuffer( sfSource[ Result ], sfStream[ Result ].BufferSize, @buffDesc.FormatCode );
  bytesRead := sfStream[ Result ]._decoder.Read( sfStream[ Result ], sfStream[ Result ].Buffer, sfStream[ Result ].BufferSize, _end );
  dsu_FillData( sfSource[ Result ], sfStream[ Result ].Buffer, bytesRead );

  sfLastPos[ Result ] := 0;
  sfSource[ Result ].SetCurrentPosition( 0 );
  sfSource[ Result ].Play( 0, 0, DSBPLAY_LOOPING );
  sfSource[ Result ].SetVolume( dsu_CalcVolume( sfVolume ) );
  sfSource[ Result ].SetFrequency( sfStream[ Result ].Frequency );
{$ENDIF}

  sfStream[ Result ]._playing  := TRUE;
  sfStream[ Result ]._paused   := FALSE;
  sfStream[ Result ]._complete := 0;
  sfStream[ Result ]._lastTime := timer_GetTicks;
{$IFDEF LINUX_OR_DARWIN}
  sfThread[ Result ] := BeginThread( @snd_ProcFile, Pointer( Result ) );
{$ENDIF}
{$IFDEF WINDOWS}
  sfThread[ Result ] := CreateThread( nil, 0, @snd_ProcFile, Pointer( Result ), 0, sfThreadID[ Result ] );
{$ENDIF}
end;

procedure snd_PauseFile;
begin
  if ( not sndInitialized ) or ( not Assigned( sfStream[ ID ]._decoder ) ) or ( not sfStream[ ID ]._playing ) or ( sfStream[ ID ]._paused ) Then exit;

  sfStream[ ID ]._paused := TRUE;
{$IFDEF WINDOWS}
  SuspendThread( sfThread[ ID ] );
{$ENDIF}

{$IFDEF USE_OPENAL}
  alSourcePause( sfSource[ ID ] );
{$ELSE}
  sfSource[ ID ].Stop();
{$ENDIF}
end;

procedure snd_StopFile;
begin
  if ( not sndInitialized ) or ( not Assigned( sfStream[ ID ]._decoder ) ) or ( not sfStream[ ID ]._playing ) Then exit;

  sfStream[ ID ]._playing := FALSE;
{$IFDEF USE_OPENAL}
  alSourceStop( sfSource[ ID ] );
{$ELSE}
  sfSource[ ID ].Stop();
{$ENDIF}
end;

procedure snd_ResumeFile;
begin
  if ( not sndInitialized ) or ( not Assigned( sfStream[ ID ]._decoder ) ) or ( not sfStream[ ID ]._playing ) or ( not sfStream[ ID ]._paused ) Then exit;

  sfStream[ ID ]._paused := FALSE;
{$IFDEF USE_OPENAL}
  alSourcePlay( sfSource[ ID ] );
{$ELSE}
  sfSource[ ID ].Play( 0, 0, DSBPLAY_LOOPING );
{$ENDIF}

{$IFDEF WINDOWS}
  ResumeThread( sfThread[ ID ] );
{$ENDIF}
end;

function snd_ProcFile;
  var
    id   : Integer;
    _end : Boolean;
  {$IFDEF USE_OPENAL}
    bytesRead : Integer;
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
  while app_Work and sfStream[ id ]._playing do
    begin
      if not sndInitialized Then break;

      u_Sleep( 100 );
      if ( not app_Work ) or ( not sfStream[ id ]._playing ) Then break;
      {$IFDEF USE_OPENAL}
      alGetSourcei( sfSource[ id ], AL_BUFFERS_PROCESSED, processed );
      while app_Work and ( processed > 0 ) and sfStream[ id ]._playing do
        begin
          alSourceUnQueueBuffers( sfSource[ id ], 1, @buffer );

          bytesRead := sfStream[ id ]._decoder.Read( sfStream[ id ], sfStream[ id ].Buffer, sfStream[ id ].BufferSize, _end );
          alBufferData( buffer, sfFormat[ sfStream[ id ].Channels ], sfStream[ id ].Buffer, bytesRead, sfStream[ id ].Frequency );
          alSourceQueueBuffers( sfSource[ id ], 1, @buffer );

          if _end Then break;

          DEC( processed );
        end;
      {$IFDEF LINUX_OR_DARWIN}
      while sfStream[ id ]._paused do u_Sleep( 10 );
      {$ENDIF}
      {$ELSE}
      Windows.EnterCriticalSection( sfCS );
      while LongWord( sfSource[ id ].GetCurrentPosition( @position, @b1Size ) ) = DSERR_BUFFERLOST do
        sfSource[ id ].Restore();
      Windows.LeaveCriticalSection( sfCS );

      FillSize := ( sfStream[ id ].BufferSize + position - sfLastPos[ id ] ) mod sfStream[ id ].BufferSize;

      block1 := nil;
      block2 := nil;
      b1Size := 0;
      b2Size := 0;

      if sfSource[ id ].Lock( sfLastPos[ id ], fillSize, block1, b1Size, block2, b2Size, 0 ) <> DS_OK Then break;
      sfLastPos[ id ] := position;

      sfStream[ id ]._decoder.Read( sfStream[ id ], block1, b1Size, _end );
      if ( b2Size <> 0 ) and ( not _end ) Then
        sfStream[ ID ]._decoder.Read( sfStream[ id ], block2, b2Size, _end );

      sfSource[ id ].Unlock( block1, b1Size, block2, b2Size );
      {$ENDIF}
      if sfStream[ id ]._complete >= sfStream[ id ].Length Then
        begin
          sfStream[ id ]._complete := 0;
          sfStream[ id ]._lastTime := timer_GetTicks();
        end;
      if _end then
        begin
          if sfStream[ id ].Loop Then
            begin
              sfStream[ id ]._decoder.Loop( sfStream[ id ] );
            end else
              begin
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
  if not app_Work Then
    {$IFDEF LINUX_OR_DARWIN} EndThread( 0 ); {$ELSE} exit; {$ENDIF}

{$IFNDEF USE_OPENAL}
  sfSource[ id ].Stop();
{$ENDIF}

{$IFDEF LINUX_OR_DARWIN}
  EndThread( 0 );
{$ENDIF}
end;

end.
