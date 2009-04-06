{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.ru
 *
 * This file is part of ZenGL
 *
 * ZenGL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * ZenGL is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_sound;

{$I zgl_config.cfg}

interface

uses
  {$IFDEF LINUX_OR_DARWIN}
  cthreads,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  {$IFDEF USE_OPENAL}
  zgl_sound_openal,
  {$ELSE}
  zgl_sound_dsound,
  {$ENDIF}
  zgl_types,
  zgl_file,
  zgl_memory
  ;

const
  SND_ALL    = -2;
  SND_STREAM = -3;

{$IFDEF USE_OPENAL}
type
  zglPSound = ^zglTSound;
  zglTSound = record
    Buffer       : DWORD;
    sCount       : DWORD;
    Source       : array of DWORD;

    Data         : Pointer;
    Size         : Integer;
    Frequency    : Integer;

    Prev, Next   : zglPSound;
end;
{$ELSE}
type
  zglPSound = ^zglTSound;
  zglTSound = record
    Buffer       : DWORD; // unused
    sCount       : DWORD;
    Source       : array of IDirectSoundBuffer;

    Data         : Pointer;
    Size         : Integer;
    Frequency    : Integer;

    Prev, Next   : zglPSound;
end;
{$ENDIF}

type
  zglPSoundStream = ^zglTSoundStream;
  zglTSoundStream = record
    _File      : zglTFile;
    Extension  : String;
    CodecOpen  : function( const FileName : String; var Stream : zglPSoundStream ) : Boolean;
    CodecRead  : function( const Buffer : Pointer; const Count : DWORD; var _End : Boolean ) : DWORD;
    CodecLoop  : procedure;
    CodecClose : procedure( var Stream : zglPSoundStream );
    Rate       : DWORD;
    Channels   : DWORD;
    Buffer     : Pointer;
    BufferSize : DWORD;
    Loop       : Boolean;
    Played     : Boolean;
end;

type
  zglPSoundFormat = ^zglTSoundFormat;
  zglTSoundFormat = record
    Extension  : String;
    Stream     : zglPSoundStream;
    FileLoader : procedure( const FileName : String; var Data : Pointer; var Size, Format, Frequency : Integer );
    MemLoader  : procedure( const Memory : zglTMemory; var Data : Pointer; var Size, Format, Frequency : Integer );
end;

type
  zglPSoundManager = ^zglTSoundManager;
  zglTSoundManager = record
    Count   : record
                Items   : DWORD;
                Formats : DWORD;
              end;
    First   : zglTSound;
    Formats : array of zglTSoundFormat;
end;

function  snd_Init : Boolean;
procedure snd_Free;
function  snd_Add( const BufferCount, SourceCount : Integer ) : zglPSound;
procedure snd_Del( Sound : zglPSound );
function  snd_LoadFromFile( const FileName : String; const SourceCount : Integer ) : zglPSound;
function  snd_LoadFromMemory( const Memory : zglTMemory; const Extension : String; const SourceCount : Integer ) : zglPSound;

function  snd_Play( const Sound : zglPSound; const X, Y, Z : Single; const Loop : Boolean = FALSE ) : Integer;
procedure snd_Stop( const Sound : zglPSound; const Source : Integer );
procedure snd_SetVolume( const Sound : zglPSound; const Volume : Single; const ID : Integer );
procedure snd_SetFrequency( const Sound : zglPSound; const Frequency, ID : Integer );
procedure snd_SetFrequencyCoeff( const Sound : zglPSound; const Coefficient : Single; const ID : Integer );

procedure snd_PlayFile( const FileName : String; const Loop : Boolean );
procedure snd_StopFile;
function  snd_ProcFile( data : Pointer ) : PInteger;
procedure snd_ResumeFile;

var
  managerSound : zglTSoundManager;

  sndActive      : Boolean;
  sndInitialized : Boolean = FALSE;
  sndVolume      : Single  = 1;
  sndCanPlay     : Boolean = TRUE;
  sndCanPlayFile : Boolean = TRUE;
  sndStopFile    : Boolean = FALSE;

  sfStream : zglPSoundStream = nil;
  sfVolume : Single = 1;
  {$IFDEF USE_OPENAL}
  sfFormat   : array[ 1..2 ] of LongInt = ( AL_FORMAT_MONO16, AL_FORMAT_STEREO16 );
  sfBufCount : Integer = 4;
  sfSource   : LongWord;
  sfBuffers  : array[ 0..3 ] of LongWord;
  {$ELSE}
  sfBuffer  : IDirectSoundBuffer;
  sfLastPos : DWORD;
  {$ENDIF}

  {$IFDEF LINUX_OR_DARWIN}
  Thread : TThreadID;
  {$ENDIF}
  {$IFDEF WIN32}
  Thread   : DWORD;
  ThreadID : DWORD;
  {$ENDIF}

implementation
uses
  zgl_main,
  zgl_window,
  zgl_timers,
  zgl_log,
  zgl_utils;

function snd_Init;
begin
  Result := FALSE;
{$IFDEF USE_OPENAL}
  log_Add( 'OpenAL: load ' + libopenal  );
  if not InitOpenAL Then
    begin
      log_Add( 'Error while loading ' + libopenal );
      exit;
    end;

  log_Add( 'OpenAL: open device' );
  log_Add( 'OpenAL: Default device is "' + alcGetString( nil, ALC_DEFAULT_DEVICE_SPECIFIER ) + '"' );

  oal_Device := alcOpenDevice( 'Generic Software' );
  if not Assigned( oal_Device ) Then
    oal_Device := alcOpenDevice( nil );
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

  alGenSources( 1, @sfSource );
  alGenBuffers( sfBufCount, @sfBuffers );
{$ELSE}
  log_Add( 'DirectSound: load DSound.dll' );
  if not InitDSound Then
    log_Add( 'DirectSound: Error while loading libraries' );

  if DirectSoundCreate( nil, ds_Device, nil ) <> DS_OK Then
    begin
      FreeDSound;
      log_Add( 'DirectSound: Error while calling DirectSoundCreate' );
      exit;
    end;

  if ds_Device.SetCooperativeLevel( wnd_Handle, DSSCL_PRIORITY ) <> DS_OK Then
    log_Add( 'DirectSound: Can''t SetCooperativeLevel' );

  log_Add( 'DirectSound: sound system initialized successful' );
{$ENDIF}

  sndInitialized := TRUE;
  Result         := TRUE;
end;

procedure snd_Free;
begin
  if not sndInitialized Then exit;

  if Assigned( sfStream ) Then
    begin
      sfStream.CodecClose( sfStream );
      if Assigned( sfStream.Buffer ) Then
        FreeMemory( sfStream.Buffer );
    end;

{$IFDEF USE_OPENAL}
  alDeleteSources( 1, @sfSource );
  alDeleteBuffers( sfBufCount, @sfBuffers[ 0 ] );

  log_Add( 'OpenAL: destroy current sound context' );
  alcDestroyContext( oal_Context );
  log_Add( 'OpenAL: close sound device' );
  alcCloseDevice( oal_Device );
  log_Add( 'OpenAL: sound system finalized successful' );
  FreeOpenAL;
{$ELSE}
  sfBuffer  := nil;
  ds_Device := nil;

  FreeDSound;
  log_Add( 'DirectSound: sound system finalized successful' );
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
  while Assigned( Result.Next ) do
    Result := Result.Next;

  zgl_GetMem( Pointer( Result.Next ), SizeOf( zglTSound ) );
  Result.Next.Prev := Result;
  Result.Next.Next := nil;
  Result           := Result.Next;

{$IFDEF USE_OPENAL}
  if BufferCount > 0 Then
    alGenBuffers( BufferCount, @Result.Buffer );
  Result.sCount := SourceCount;
  SetLength( Result.Source, SourceCount );
  for i := 0 to SourceCount - 1 do
    alGenSources( 1, @Result.Source[ i ] );
{$ELSE}
  Result.sCount := SourceCount;
  SetLength( Result.Source, SourceCount );
{$ENDIF}

  INC( managerSound.Count.Items );
end;

procedure snd_Del;
  var
    i : Integer;
begin
  if not Assigned( Sound ) Then exit;

{$IFDEF USE_OPENAL}
  for i := 0 to Sound.sCount - 1 do
    alDeleteSources( 1, @Sound.Source[ i ] );
  alDeleteBuffers( 1, @Sound.Buffer );
{$ELSE}
  for i := 0 to Sound.sCount - 1 do
    Sound.Source[ i ] := nil;
{$ENDIF}
  SetLength( Sound.Source, 0 );
  FreeMemory( Sound.Data );

  if Assigned( Sound.Prev ) Then
    Sound.Prev.Next := Sound.Next;
  if Assigned( Sound.Next ) Then
    Sound.Next.Prev := Sound.Prev;

  FreeMemory( Sound );
  DEC( managerSound.Count.Items );
end;

function snd_LoadFromFile;
  var
    i   : Integer;
    f   : Integer;
    ext : String;
begin
  Result := nil;

  if not sndInitialized Then exit;

  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read ' + FileName );
      exit;
    end;
  Result := snd_Add( 1, SourceCount );

  for i := managerSound.Count.Formats - 1 downto 0 do
    begin
      file_GetExtension( FileName, ext );
      if u_StrUp( ext ) = managerSound.Formats[ i ].Extension Then
        managerSound.Formats[ i ].FileLoader( FileName, Result.Data, Result.Size, f, Result.Frequency );
    end;

  if not Assigned( Result.Data ) Then
    begin
      log_Add( 'Cannot load sound: ' + FileName );
      exit;
    end;

{$IFDEF USE_OPENAL}
  alBufferData( Result.Buffer, f, Result.Data, Result.Size, Result.Frequency );
{$ELSE}
  Result.Source[ 0 ] := dsu_CreateBuffer( Result.Size, Pointer( f ) );
  dsu_FillData( Result.Source[ 0 ], Result.Data, Result.Size );
  for i := 1 to Result.sCount - 1 do
    ds_Device.DuplicateSoundBuffer( Result.Source[ 0 ], Result.Source[ i ] );
{$ENDIF}

  log_Add( 'Successful loading of sound: ' + FileName );
end;

function snd_LoadFromMemory;
  var
    i : Integer;
    f : Integer;
begin
  Result := nil;

  if not sndInitialized Then exit;

  Result := snd_Add( 1, SourceCount );

  for i := managerSound.Count.Formats - 1 downto 0 do
    if u_StrUp( Extension ) = managerSound.Formats[ i ].Extension Then
      managerSound.Formats[ i ].MemLoader( Memory, Result.Data, Result.Size, f, Result.Frequency );

  if not Assigned( Result.Data ) Then
    begin
      log_Add( 'Cannot load sound: From Memory' );
      exit;
    end;

{$IFDEF USE_OPENAL}
  alBufferData( Result.Buffer, f, Result.Data, Result.Size, Result.Frequency );
{$ELSE}
  Result.Source[ 0 ] := dsu_CreateBuffer( Result.Size, Pointer( f ) );
  dsu_FillData( Result.Source[ 0 ], Result.Data, Result.Size );
  for i := 1 to Result.sCount - 1 do
    ds_Device.DuplicateSoundBuffer( Result.Source[ 0 ], Result.Source[ i ] );
{$ENDIF}

  log_Add( 'Successful loading of sound: From Memory' );
end;

function snd_Play;
  var
    i, j      : Integer;
    {$IFDEF USE_OPENAL}
    sourcePos : array[ 0..2 ] of Single;
    {$ELSE}
    DSERROR : HRESULT;
    Status  : DWORD;
    Vol     : Single;
    {$ENDIF}
begin
  Result := -1;

  if ( not Assigned( Sound ) ) or
     ( not sndInitialized ) or
     ( not sndCanPlay ) Then exit;

{$IFDEF USE_OPENAL}
  for i := 0 to Sound.sCount - 1 do
    begin
      alGetSourcei( Sound.Source[ i ], AL_SOURCE_STATE, j );
      if j <> AL_PLAYING Then
         begin
           Result := i;
           break;
         end;
    end;
  if Result = -1 Then exit;

  sourcePos[ 0 ] := X;
  sourcePos[ 1 ] := Y;
  sourcePos[ 2 ] := Z;

  alSourcei ( Sound.Source[ Result ], AL_BUFFER,    Sound.Buffer );
  alSourcefv( Sound.Source[ Result ], AL_POSITION,  @sourcePos );
  alSourcefv( Sound.Source[ Result ], AL_VELOCITY,  @oal_Velocity );
  alSourcef ( Sound.Source[ Result ], AL_GAIN,      sndVolume );
  alSourcei ( Sound.Source[ Result ], AL_FREQUENCY, Sound.Frequency );

  if Loop Then
    alSourcei( Sound.Source[ Result ], AL_LOOPING, AL_TRUE )
  else
    alSourcei( Sound.Source[ Result ], AL_LOOPING, AL_FALSE );

  alSourcePlay( Sound.Source[ Result ] );
{$ELSE}
  for i := 0 to Sound.sCount - 1 do
    begin
      DSERROR := Sound.Source[ i ].GetStatus( Status );
      if DSERROR <> DS_OK Then Status := 0;
      if ( Status and DSBSTATUS_PLAYING ) = 0 Then
        begin
          if ( Status and DSBSTATUS_BUFFERLOST ) <> 0 Then
            begin
              Sound.Source[ i ].Restore;
              dsu_FillData( Sound.Source[ i ], Sound.Data, Sound.Size );
            end;
          Result := i;
          break;
        end;
    end;
  if Result = -1 Then exit;

  Sound.Source[ Result ].SetPan      ( dsu_CalcPos( X, Y, Z, Vol )                 );
  Sound.Source[ Result ].SetVolume   ( dsu_CalcVolume( Vol )                       );
  Sound.Source[ Result ].SetFrequency( Sound.Frequency                             );
  Sound.Source[ Result ].Play        ( 0, 0, DSBPLAY_LOOPING * Byte( Loop = TRUE ) );
{$ENDIF}
end;

procedure snd_Stop;
  var
    i : Integer;
begin
  if ( not Assigned( Sound ) ) or
     ( not sndInitialized ) Then exit;

{$IFDEF USE_OPENAL}
  if source = -1 Then
    begin
      for i := 0 to Sound.sCount - 1 do alSourceStop( Sound.Source[ i ] );
      exit;
    end;
  alSourceStop( Sound.Source[ source ] );
{$ELSE}
  if source = -1 Then
    begin
      for i := 0 to Sound.sCount - 1 do Sound.Source[ i ].Stop;
      exit;
    end;
  Sound.Source[ source ].Stop;
{$ENDIF}
end;

procedure snd_SetVolume;
  var
    i, j : Integer;
    snd  : zglPSound;
begin
  if not sndInitialized Then exit;

  if ID = SND_STREAM Then
    sfVolume := Volume
  else
    if ( not Assigned( Sound ) ) and ( ID = SND_ALL ) Then
      sndVolume := Volume;
{$IFDEF USE_OPENAL}
  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.sCount - 1 do
            alSourcef( Sound.Source[ i ], AL_GAIN, Volume );
        end else
          if ID >= 0 Then
            alSourcef( Sound.Source[ ID ], AL_GAIN, Volume );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.Next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.sCount - 1 do
                alSourcef( snd.Source[ j ], AL_GAIN, Volume );
              snd := snd.Next;
            end;
        end else
          if ( ID = SND_STREAM ) and ( Assigned( sfStream ) ) Then
            alSourcef( sfSource, AL_GAIN, Volume );
{$ELSE}
  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.sCount - 1 do
            Sound.Source[ i ].SetVolume( dsu_CalcVolume( Volume ) );
        end else
          if ID >= 0 Then
            Sound.Source[ ID ].SetVolume( dsu_CalcVolume( Volume ) );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.Next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.sCount - 1 do
                snd.Source[ j ].SetVolume( dsu_CalcVolume( Volume ) );
              snd := snd.Next;
            end;
        end else
          if ( ID = SND_STREAM ) and ( Assigned( sfStream ) ) Then
           sfBuffer.SetVolume( dsu_CalcVolume( Volume ) );
{$ENDIF}
end;

procedure snd_SetFrequency;
  var
    i, j : Integer;
    snd  : zglPSound;
begin
  if not sndInitialized Then exit;

{$IFDEF USE_OPENAL}
  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.sCount - 1 do
            alSourcei( Sound.Source[ i ], AL_FREQUENCY, Frequency );
        end else
          if ID >= 0 Then
            alSourcei( Sound.Source[ ID ], AL_FREQUENCY, Frequency );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.Next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.sCount - 1 do
                alSourcei( snd.Source[ j ], AL_FREQUENCY, Frequency );
              snd := snd.Next;
            end;
        end else
          if ( ID = SND_STREAM ) and ( Assigned( sfStream ) ) Then
            alSourcef( sfSource, AL_FREQUENCY, Frequency );
{$ELSE}
  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.sCount - 1 do
            Sound.Source[ i ].SetFrequency( Frequency );
        end else
          if ID >= 0 Then
            Sound.Source[ ID ].SetFrequency( Frequency );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.Next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.sCount - 1 do
                snd.Source[ j ].SetFrequency( Frequency );
              snd := snd.Next;
            end;
        end else
          if ( ID = SND_STREAM ) and ( Assigned( sfStream ) ) Then
           sfBuffer.SetFrequency( Frequency );
{$ENDIF}
end;

procedure snd_SetFrequencyCoeff;
  var
    i, j : Integer;
    snd  : zglPSound;
begin
  if not sndInitialized Then exit;

{$IFDEF USE_OPENAL}
  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.sCount - 1 do
            alSourcei( Sound.Source[ i ], AL_FREQUENCY, Round( Sound.Frequency * Coefficient ) );
        end else
          if ID >= 0 Then
            alSourcei( Sound.Source[ ID ], AL_FREQUENCY, Round( Sound.Frequency * Coefficient ) );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.Next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.sCount - 1 do
                alSourcei( snd.Source[ j ], AL_FREQUENCY, Round( snd.Frequency * Coefficient ) );
              snd := snd.Next;
            end;
        end else
          if ( ID = SND_STREAM ) and ( Assigned( sfStream ) ) Then
            alSourcef( sfSource, AL_FREQUENCY, Round( snd.Frequency * Coefficient ) );
{$ELSE}
  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.sCount - 1 do
            Sound.Source[ i ].SetFrequency( Round( Sound.Frequency * Coefficient ) );
        end else
          if ID >= 0 Then
            Sound.Source[ ID ].SetFrequency( Round( Sound.Frequency * Coefficient ) );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.Next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.sCount - 1 do
                snd.Source[ j ].SetFrequency( Round( snd.Frequency * Coefficient ) );
              snd := snd.Next;
            end;
        end else
          if ( ID = SND_STREAM ) and ( Assigned( sfStream ) ) Then
           sfBuffer.SetFrequency( Round( snd.Frequency * Coefficient ) );
{$ENDIF}
end;

procedure snd_PlayFile;
  var
    i         : Integer;
    ext       : String;
    _End      : Boolean;
    {$IFDEF USE_OPENAL}
    BytesRead : Integer;
    {$ELSE}
    ap1, ap2 : Pointer;
    as1, as2 : DWORD;
    buffDesc : zglTBufferDesc;
    {$ENDIF}
begin
  if ( not sndInitialized ) or
     ( not sndCanPlayFile ) Then exit;

  if Assigned( sfStream ) and sfStream.Played Then
    begin
      snd_StopFile;
      sfStream.CodecClose( sfStream );
      if Assigned( sfStream.Buffer ) Then
        FreeMemory( sfStream.Buffer );
    end;

  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read ' + FileName );
      exit;
    end;

  for i := managerSound.Count.Formats - 1 downto 0 do
    begin
      file_GetExtension( FileName, ext );
      if u_StrUp( ext ) = managerSound.Formats[ i ].Extension Then
        sfStream := managerSound.Formats[ i ].Stream;
    end;

  if Assigned( sfStream ) Then
    sfStream.Loop := Loop;

  if ( not Assigned( sfStream ) ) or
     ( not sfStream.CodecOpen( FileName, sfStream ) ) Then
    begin
      sfStream := nil;
      log_Add( 'Cannot play: ' + FileName );
      exit;
    end;

{$IFDEF USE_OPENAL}
  alSourceStop( sfSource );
  alSourceRewind( sfSource );
  alSourcei( sfSource, AL_BUFFER, 0 );

  for i := 0 to sfBufCount - 1 do
    begin
      BytesRead := sfStream.CodecRead( sfStream.Buffer, sfStream.BufferSize, _End );
      if BytesRead <= 0 Then break;

      alBufferData( sfBuffers[ i ], sfFormat[ sfStream.Channels ], sfStream.Buffer, BytesRead, sfStream.Rate );
      alSourceQueueBuffers( sfSource, 1, @sfBuffers[ i ] );
    end;

  alSourcei( sfSource, AL_LOOPING, AL_FALSE );
  alSourcePlay( sfSource );
  alSourcef( sfSource, AL_GAIN, sfVolume );
  alSourcef( sfSource, AL_FREQUENCY, sfStream.Rate );
{$ELSE}
  with buffDesc do
    begin
      FormatCode     := 1;
      ChannelNumber  := sfStream.Channels;
      SampleRate     := sfStream.Rate;
      BitsPerSample  := 16;
      BytesPerSample := ( BitsPerSample div 8 ) * ChannelNumber;
      BytesPerSecond := SampleRate * BytesPerSample;
      cbSize         := SizeOf( buffDesc );
    end;
  if Assigned( sfBuffer ) Then sfBuffer := nil;
  sfBuffer := dsu_CreateBuffer( sfStream.BufferSize, @buffDesc.FormatCode );

  {sfBuffer.Lock( 0, sfStream.BufferSize, ap1, as1, ap2, as2, 0 );
  sfStream.CodecRead( ap1, as1, _End );
  sfBuffer.Unlock( ap1, as1, ap2, as2 );}

  sfBuffer.SetCurrentPosition( 0 );
  sfLastPos := 0;
  sfBuffer.Play( 0, 0, DSBPLAY_LOOPING );
  sfBuffer.SetVolume( dsu_CalcVolume( sfVolume ) );
  sfBuffer.SetFrequency( sfStream.Rate );
{$ENDIF}

  sfStream.Played := TRUE;
{$IFDEF LINUX_OR_DARWIN}
  Thread := BeginThread( @snd_ProcFile );
{$ENDIF}
{$IFDEF WIN32}
  Thread := CreateThread( nil, 0, @snd_ProcFile, nil, 0, ThreadID );
{$ENDIF}
end;

procedure snd_StopFile;
begin
  if ( not Assigned( sfStream ) ) or
     ( not sfStream.Played ) or
     ( not sndInitialized ) Then exit;

  sfStream.Played := FALSE;

{$IFDEF USE_OPENAL}
  sndStopFile := TRUE;
  while sndStopFile do;

  alSourceStop( sfSource );
  alSourceRewind( sfSource );
  alSourcei( sfSource, AL_BUFFER, 0 );
{$ELSE}
  TerminateThread( Thread, 0 );
  CloseHandle( Thread );
  sfBuffer.Stop;
{$ENDIF}
end;

function snd_ProcFile;
  var
    _End : Boolean;
  {$IFDEF USE_OPENAL}
    processed : LongInt;
    buffer    : LongWord;
    BytesRead : Integer;
  {$ELSE}
    as1, as2 : DWORD;
    ap1, ap2 : Pointer;
    Pos      : DWORD;
    NeedFill : DWORD;
  {$ENDIF}
begin
  try
    while not sndStopFile do
      begin
        if ( not Assigned( sfStream ) ) or
           ( not sndInitialized ) Then break;

        u_Sleep( 100 );
        {$IFDEF USE_OPENAL}
        alGetSourcei( sfSource, AL_BUFFERS_PROCESSED, processed );
        while ( processed > 0 ) and ( not sndStopFile ) do
          begin
            alSourceUnQueueBuffers( sfSource, 1, @buffer );

            BytesRead := sfStream.CodecRead( sfStream.Buffer, sfStream.BufferSize, _End );
            alBufferData( buffer, sfFormat[ sfStream.Channels ], sfStream.Buffer, BytesRead, sfStream.Rate );
            alSourceQueueBuffers( sfSource, 1, @buffer );

            DEC( processed );
          end;
        {$ELSE}
        while DWORD( sfBuffer.GetCurrentPosition( @Pos, @as2 ) ) = DSERR_BUFFERLOST do
          sfBuffer.Restore;

        NeedFill := ( sfStream.BufferSize + Pos - sfLastPos ) mod sfStream.BufferSize;

        ap1 := nil;
        ap2 := nil;
        as1 := 0;
        as2 := 0;

        sfBuffer.Lock( sfLastPos, NeedFill, ap1, as1, ap2, as2, 0 );
        sfLastPos := Pos;

        sfStream.CodecRead( ap1, as1, _End );
        if ( as2 <> 0 ) and ( not _End ) Then
          sfStream.CodecRead( ap2, as2, _End );

        sfBuffer.Unlock( ap1, as1, ap2, as2 );
        {$ENDIF}
        if _End then
          begin
            if sfStream.Loop Then
              sfStream.CodecLoop
            else
              begin
                sfStream^.Played := FALSE;
                break;
              end;
          end;
      end;
  except
  end;
{$IFDEF USE_OPENAL}
  alSourceQueueBuffers( sfSource, 1, @buffer );
{$ELSE}
  sfBuffer.Stop;
{$ENDIF}
  sndStopFile := FALSE;
end;

procedure snd_ResumeFile;
  var
    i    : Integer;
    _End : Boolean;
    {$IFDEF USE_OPENAL}
    BytesRead : Integer;
    {$ENDIF}
begin
  if ( not Assigned( sfStream ) ) or
     ( sfStream.Played ) or
     ( not sndInitialized ) Then exit;

{$IFDEF USE_OPENAL}
  alSourceStop( sfSource );
  alSourceRewind( sfSource );
  alSourcei( sfSource, AL_BUFFER, 0 );

  for i := 0 to sfBufCount - 1 do
    begin
      BytesRead := sfStream.CodecRead( sfStream.Buffer, sfStream.BufferSize, _End );
      //if BytesRead <= 0 Then break;

      alBufferData( sfBuffers[ i ], sfFormat[ sfStream.Channels ], sfStream.Buffer, BytesRead, sfStream.Rate );
      alSourceQueueBuffers( sfSource, 1, @sfBuffers[ i ] );
    end;

  alSourcei( sfSource, AL_LOOPING, AL_FALSE );
  alSourcePlay( sfSource );
  alSourcef( sfSource, AL_GAIN, sfVolume );
  alSourcef( sfSource, AL_FREQUENCY, sfStream.Rate );
{$ELSE}
  sfBuffer.Play( 0, 0, DSBPLAY_LOOPING );
  sfBuffer.SetVolume( dsu_CalcVolume( sfVolume ) );
  sfBuffer.SetFrequency( sfStream.Rate );
{$ENDIF}

  sfStream.Played := TRUE;
{$IFDEF LINUX_OR_DARWIN}
  Thread := BeginThread( @snd_ProcFile );
{$ENDIF}
{$IFDEF WIN32}
  Thread := CreateThread( nil, 0, @snd_ProcFile, nil, 0, ThreadID );
{$ENDIF}
end;

end.
