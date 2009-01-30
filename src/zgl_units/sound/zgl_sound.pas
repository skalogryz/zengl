{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru.2x4.ru
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_sound;

{$I define.inc}

interface

uses
  {$IFDEF LINUX}
  pthreads,
  openal,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  DirectSound,
  {$ENDIF}
  {$IFDEF DARWIN}
  cthreads,
  openal,
  {$ENDIF}
  zgl_types,
  zgl_global_var,
  zgl_log,
  zgl_file,
  zgl_memory,
  zgl_math,
  Utils
  ;

function  snd_Init : Boolean;
procedure snd_Free;
function  snd_Add( const BufferCount, SourceCount : Integer ) : zglPSound;
procedure snd_Del( Sound : zglPSound );
function  snd_LoadFromFile( const FileName : String; const SourceCount : Integer ) : zglPSound;
function  snd_LoadFromMemory( const Memory : zglTMemory; const Extension : String; const SourceCount : Integer ) : zglPSound;

function  snd_Play( const Sound : zglPSound; const X, Y, Z : Single; const Loop : Boolean ) : Integer;
procedure snd_Stop( const Sound : zglPSound; const Source : Integer );
procedure snd_SetVolume( const Volume : Byte; const ID : Integer );
procedure snd_SetFrequency( const Frequency, ID : Integer );
procedure snd_SetFrequencyCoeff( const Coefficient : Single; const ID : Integer );
procedure snd_PlayFile( const SoundFile : zglPSoundFile );
procedure snd_StopFile;
function  snd_ProcFile( data : Pointer ) : PtrInt;
procedure snd_RestoreFile;

var
  sndActive : Boolean;
  {$IFDEF LINUX}
  Thread     : TThreadID;
  ThreadAttr : ppthread_attr_t;
  {$ENDIF}
  {$IFDEF WIN32}
  Thread   : DWORD;
  ThreadID : DWORD;
  {$ENDIF}
  {$IFDEF DARWIN}
  Thread : TThreadID;
  {$ENDIF}

implementation
uses
  zgl_main, zgl_sound_wav;


// DirectSound Utils
{$IFDEF WIN32}
function dsu_CreateBuffer( BufferSize : DWORD; Format : Pointer ) : IDirectSoundBuffer;
  var
    DSoundBD : TDSBufferDesc;
begin
  fillChar( DSoundBD, SizeOf( TDSBUFFERDESC ), 0 );
  DSoundBD.dwSize  := sizeof( TDSBUFFERDESC );
  DSoundBD.dwFlags := DSBCAPS_STATIC        +
                      DSBCAPS_CTRLPAN       +
                      DSBCAPS_CTRLVOLUME    +
                      DSBCAPS_CTRLFREQUENCY +
                      DSBCAPS_GETCURRENTPOSITION2;
  DSoundBD.dwBufferBytes := BufferSize;
  DSoundBD.lpwfxFormat   := Format;

  ds_Device.CreateSoundBuffer( DSoundBD, Result, nil );
end;

procedure dsu_FillData( var Buffer : IDirectSoundBuffer; Data : Pointer; DataSize : DWORD; Pos : DWORD = 0 );
  var
    Block1, Block2 : Pointer;
    b1Size, b2Size : DWORD;
begin
  Buffer.Lock( Pos, DataSize, Block1, b1Size, Block2, b2Size, 0 );
  Move( Data^, Block1^, b1Size );
  if b2Size <> 0 Then Move( Pointer( Data + b1Size )^, Block2^, b2Size );
  Buffer.Unlock( Block1, b1Size, Block2, b2Size );
end;

function dsu_CalcPos( X, Y, Z : Single; var Volume : Integer ) : Integer;
  var
    i : Integer;
    dist, angle : Single;
begin
  ds_Plane.X := ds_Orientation[ 1 ] * ds_Orientation[ 5 ] - ds_Orientation[ 2 ] * ds_Orientation[ 4 ];
  ds_Plane.Y := ds_Orientation[ 2 ] * ds_Orientation[ 3 ] - ds_Orientation[ 0 ] * ds_Orientation[ 5 ];
  ds_Plane.Z := ds_Orientation[ 0 ] * ds_Orientation[ 4 ] - ds_Orientation[ 1 ] * ds_Orientation[ 3 ];

  dist := sqrt( sqr( X - ds_Position.X ) + sqr( Y - ds_Position.Y ) + sqr( Z - ds_Position.Z ) );
  if dist = 0 then
    angle := 0
  else
    angle := ( ds_Plane.X * ( X - ds_Position.X ) + ds_Plane.Y * ( Y - ds_Position.Y ) + ds_Plane.Z * ( Z - ds_Position.Z ) ) / dist;
  Result := trunc( 10000 * angle * 0.1 );
  if Result < -10000 Then Result := -10000;
  if Result > 10000  Then Result := 10000;

  i := Trunc( ( 1 - dist * 0.05 ) * 100 );
  if i < 0 Then i := 0;
  i := Trunc( i * sndVolume / 100 );
  Volume := -50 * ( 100 - i );
end;
{$ENDIF}

function snd_Init;
begin
  Result := FALSE;
  {$IFDEF LINUX_OR_DARWIN}
  log_Add( 'OpenAL: load ' + libopenal  );
  if not InitOpenAL( libopenal ) Then
    begin
      log_Add( 'Error while loading ' + libopenal );
      exit;
    end;

  log_Add( 'OpenAL: open device' );
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

  alcMakeContextCurrent( oal_Context );
  log_Add( 'OpenAL: sound system initialized successful' );

  alListenerfv( AL_POSITION,    @oal_Position );
  alListenerfv( AL_VELOCITY,    @oal_Velocity );
  alListenerfv( AL_ORIENTATION, @oal_Orientation );
  
  alGenSources( 1, @sfSource );
  alGenBuffers( sfBufCount, @sfBuffers );
  {$ENDIF}
  {$IFDEF WIN32}
  log_Add( 'DirectSound: load DSound.dll' );
  if not LoadDirectSound Then
    log_Add( 'DirectSound: Error while loading libraries' );

  if DirectSoundCreate( nil, ds_Device, nil ) <> DS_OK Then
    begin
      FreeDirectSound;
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

  {$IFDEF LINUX_OR_DARWIN}
  alDeleteBuffers( sfBufCount, @sfBuffers[ 0 ] );
  alDeleteSources( 1, @sfSource );

  log_Add( 'OpenAL: destroy current sound context' );
  alcDestroyContext( oal_Context );
  log_Add( 'OpenAL: close sound device' );
  alcCloseDevice( oal_Device );
  log_Add( 'OpenAL: sound system finalized successful' );
  {$ENDIF}
  {$IFDEF WIN32}
  sfBuffer  := nil;
  ds_Device := nil;

  FreeDirectSound;
  log_Add( 'DirectSound: sound system finalized successful' );
  {$ENDIF}
end;

function snd_Add;
  {$IFDEF LINUX_OR_DARWIN}
  var
    i : Integer;
  {$ENDIF}
begin
  Result := nil;

  if not sndInitialized Then exit;

  Result := @managerSound.First;
  while Assigned( Result.Next ) do
    Result := Result.Next;

  Result.Next := AllocMem( SizeOf( zglTSound ) );
  FillChar( Result.Next^, SizeOf( zglTSound ), 0 );
  Result.Next.Prev := Result;
  Result           := Result.Next;

  {$IFDEF LINUX_OR_DARWIN}
  if BufferCount > 0 Then
    alGenBuffers( BufferCount, @Result.Buffer );
  Result.sCount := SourceCount;
  SetLength( Result.Source, SourceCount );
  for i := 0 to SourceCount - 1 do
    alGenSources( 1, @Result.Source[ i ] );
  {$ENDIF}
  {$IFDEF WIN32}
  Result.sCount := SourceCount;
  SetLength( Result.Source, SourceCount );
  {$ENDIF}

  INC( managerSound.Count );
end;

procedure snd_Del;
  var
    i : Integer;
begin
  {$IFDEF LINUX_OR_DARWIN}
  alDeleteBuffers( 1, @Sound.Buffer );
  for i := 0 to Sound.sCount - 1 do
    alDeleteSources( 1, @Sound.Source[ i ] );
  {$ENDIF}
  {$IFDEF WIN32}
  for i := 0 to Sound.sCount - 1 do
    Sound.Source[ i ] := nil;
  {$ENDIF}

  SetLength( Sound.Source, 0 );
  Freemem( Sound.Data );

  if Assigned( Sound.Prev ) Then
    Sound.Prev.Next := Sound.Next;
  if Assigned( Sound.Next ) Then
    Sound.Next.Prev := Sound.Prev;

  Freememory( Sound );
  DEC( managerSound.Count );
end;

function snd_LoadFromFile;
  var
    i : Integer;
    f : DWORD;
begin
  Result := nil;

  if not sndInitialized Then exit;

  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read ' + FileName );
      exit;
    end;
  Result := snd_Add( 1, SourceCount );

  for i := sndNFCount - 1 downto 0 do
    if copy( StrUp( FileName ), length( FileName ) - 3, 4 ) = '.' + sndFormats[ i ].Extension Then
      sndFormats[ i ].FileLoader( FileName, Result.Data, Result.Size, f, Result.Frequency );

  {$IFDEF LINUX_OR_DARWIN}
  alBufferData( Result.Buffer, f, Result.Data, Result.Size, Result.Frequency );
  {$ENDIF}
  {$IFDEF WIN32}
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
    f : DWORD;
begin
  Result := nil;

  if not sndInitialized Then exit;

  Result := snd_Add( 1, SourceCount );

  for i := sndNFCount - 1 downto 0 do
    if StrUp( Extension ) = sndFormats[ i ].Extension Then
      sndFormats[ i ].MemLoader( Memory, Result.Data, Result.Size, f, Result.Frequency );

  {$IFDEF LINUX_OR_DARWIN}
  alBufferData( Result.Buffer, f, Result.Data, Result.Size, Result.Frequency );
  {$ENDIF}
  {$IFDEF WIN32}
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
    {$IFDEF LINUX_OR_DARWIN}
    sourcePos : array[ 0..2 ] of TALfloat;
    {$ENDIF}
    {$IFDEF WIN32}
    DSERROR : HRESULT;
    Status  : DWORD;
    Vol     : Integer;
    {$ENDIF}
begin
  Result := -1;

  if ( not Assigned( Sound ) ) or 
     ( not sndInitialized ) or 
     ( not sndCanPlay ) Then exit;

  {$IFDEF LINUX_OR_DARWIN}
  for i := 0 to Sound.sCount - 1 do
    begin
      alGetSourcei( Sound.Source[ i ], AL_SOURCE_STATE, @j );
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
  alSourcef ( Sound.Source[ Result ], AL_GAIN,      sndVolume / 100 );
  alSourcei ( Sound.Source[ Result ], AL_FREQUENCY, Sound.Frequency );
  
  if Loop Then
    alSourcei( Sound.Source[ Result ], AL_LOOPING, AL_TRUE )
  else
    alSourcei( Sound.Source[ Result ], AL_LOOPING, AL_FALSE );

  alSourcePlay( Sound.Source[ Result ] );
  {$ENDIF}
  {$IFDEF WIN32}
  for i := 0 to Sound.sCount - 1 do
    begin
      DSERROR := Sound.Source[ i ].GetStatus( Status );
      if DSERROR <> DS_OK Then Status := 0;
      if ( Status and DSBSTATUS_PLAYING ) = 0 Then
        begin
          if ( Status and DSBSTATUS_BUFFERLOST ) <> 0 Then
            begin
              Sound.Source[ i ].Restore;
              dsu_FillData( Sound.Source[ 0 ], Sound.Data, Sound.Size );
            end;
          Result := i;
          break;
        end;
    end;
  if Result = -1 Then exit;

  Sound.Source[ Result ].SetPan      ( dsu_CalcPos( X, Y, Z, Vol )                 );
  Sound.Source[ Result ].SetVolume   ( Vol                                         );
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

  {$IFDEF LINUX_OR_DARWIN}
  if source = -1 Then
    begin
      for i := 0 to Sound.sCount - 1 do alSourceStop( Sound.Source[ i ] );
      exit;
    end;
  alSourceStop( Sound.Source[ source ] );
  {$ENDIF}
  {$IFDEF WIN32}
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
    i, j  : Integer;
    Sound : zglPSound;
begin
  if Volume > 100 then
    sndVolume := 100
  else
    sndVolume := Volume;
  {$IFDEF LINUX_OR_DARWIN}
    if ID > -1 Then
      begin
        Sound := managerSound.First.Next;
        for i := 0 to managerSound.Count - 1 do
          begin
            for j := 0 to Sound.sCount - 1 do
              alSourcef( Sound.Source[ j ], AL_GAIN, sndVolume / 100 );
            Sound := Sound.Next;
          end;
      end else
        alSourcef( Sound.Source[ ID ], AL_GAIN, sndVolume / 100 );
  {$ENDIF}
  {$IFDEF WIN32}
    if ID > -1 Then
      begin
        Sound := managerSound.First.Next;
        for i := 0 to managerSound.Count - 1 do
          begin
            for j := 0 to Sound.sCount - 1 do
              Sound.Source[ j ].SetVolume( -50 * ( 100 - sndVolume ) );
            Sound := Sound.Next;
          end;
      end else
        Sound.Source[ ID ].SetVolume( -50 * ( 100 - sndVolume ) );
  {$ENDIF}
end;

procedure snd_SetFrequency;
  var
    i, j  : Integer;
    Sound : zglPSound;
begin
  {$IFDEF LINUX_OR_DARWIN}
    if ID > -1 Then
      begin
        Sound := managerSound.First.Next;
        for i := 0 to managerSound.Count - 1 do
          begin
            for j := 0 to Sound.sCount - 1 do
              alSourcei( Sound.Source[ j ], AL_FREQUENCY, Frequency );
            Sound := Sound.Next;
          end;
      end else
        alSourcei( Sound.Source[ ID ], AL_FREQUENCY, Frequency );
  {$ENDIF}
  {$IFDEF WIN32}
    if ID > -1 Then
      begin
        Sound := managerSound.First.Next;
        for i := 0 to managerSound.Count - 1 do
          begin
            for j := 0 to Sound.sCount - 1 do
              Sound.Source[ j ].SetFrequency( Frequency );
            Sound := Sound.Next;
          end;
      end else
        Sound.Source[ ID ].SetFrequency( Frequency );
  {$ENDIF}
end;

procedure snd_SetFrequencyCoeff;
  var
    i, j  : Integer;
    Sound : zglPSound;
begin
  {$IFDEF LINUX_OR_DARWIN}
    if ID > -1 Then
      begin
        Sound := managerSound.First.Next;
        for i := 0 to managerSound.Count - 1 do
          begin
            for j := 0 to Sound.sCount - 1 do
              alSourcei( Sound.Source[ j ], AL_FREQUENCY, Round( Sound.Frequency * Coefficient ) );
            Sound := Sound.Next;
          end;
      end else
        alSourcei( Sound.Source[ ID ], AL_FREQUENCY, Round( Sound.Frequency * Coefficient ) );
  {$ENDIF}
  {$IFDEF WIN32}
    if ID > -1 Then
      begin
        Sound := managerSound.First.Next;
        for i := 0 to managerSound.Count - 1 do
          begin
            for j := 0 to Sound.sCount - 1 do
              Sound.Source[ j ].SetFrequency( Round( Sound.Frequency * Coefficient ) );
            Sound := Sound.Next;
          end;
      end else
        Sound.Source[ ID ].SetFrequency( Round( Sound.Frequency * Coefficient ) );
  {$ENDIF}
end;

procedure snd_PlayFile;
  var
    i : Integer;
    {$IFDEF WIN32}
    wavHeader : zglTWAVHeader;
    {$ENDIF}
begin
  if ( not sndInitialized ) or
     ( not sndCanPlayFile ) Then exit;

  if Assigned( sfStream ) Then
    if sfStream.Played Then snd_StopFile;

  sfStream := SoundFile;

  {$IFDEF LINUX_OR_DARWIN}
  alSourceStop( sfSource );
  alSourceRewind( sfSource );
  alSourcei( sfSource, AL_BUFFER, 0 );

  for i := 0 to sfBufCount - 1 do
    begin
      if sfStream.CodecRead( sfStream.Buffer, sfStream.BufferSize ) = 0 Then break;

      alBufferData( sfBuffers[ i ], sfFormat[ sfStream.Channels ], sfStream.Buffer, sfStream.BufferSize, sfStream.Rate );
      alSourceQueueBuffers( sfSource, 1, @sfBuffers[ i ] );
    end;

  alSourcei( sfSource, AL_LOOPING, AL_FALSE );
  alSourcePlay( sfSource );
  sfStream^.Played := TRUE;
  {$ENDIF}

  {$IFDEF WIN32}
  with wavHeader do
    begin
      FormatCode     := 1;
      ChannelNumber  := sfStream.Channels;
      SampleRate     := sfStream.Rate;
      BitsPerSample  := 16;
      BytesPerSample := BitsPerSample div 8 * ChannelNumber;
      BytesPerSecond := SampleRate * BytesPerSample;
    end;
  if Assigned( sfBuffer ) Then sfBuffer := nil;
  sfBuffer := dsu_CreateBuffer( sfStream.BufferSize * 4, @wavHeader.FormatCode );
  sfStream.CodecRead( sfStream.Buffer, sfStream.BufferSize );
  dsu_FillData( sfBuffer, sfStream.Buffer, sfStream.BufferSize );
  sfBufferRead[ 0 ] := TRUE;
  sfBuffer.Play( 0, 0, DSBPLAY_LOOPING );
  sfBufferPos  := 0;
  sfBufferNext := 1;
  {$ENDIF}

  {$IFDEF LINUX}
  pthread_create( Thread, ThreadAttr^, @snd_ProcFile, @oal_Device );
  {$ENDIF}
  {$IFDEF WIN32}
  Thread := CreateThread( nil, 0, @snd_ProcFile, nil, 0, ThreadID );
  {$ENDIF}
  {$IFDEF DARWIN}
  ThreadID := BeginThread( @snd_ProcFile );
  {$ENDIF}
end;

procedure snd_StopFile;
  {$IFDEF LINUX_OR_DARWIN}
  var
    buffer : TALuint;
  {$ENDIF}
begin
  if ( not Assigned( sfStream ) ) or
     ( not sndInitialized ) Then exit;

  if sfStream.Played Then
    begin
      sndStopFile := TRUE;
      while sndStopFile do;
    end;
  sfStream^.Played := FALSE;

  {$IFDEF LINUX_OR_DARWIN}
  alSourceStop( sfSource );
  alSourceRewind( sfSource );
  alSourcei( sfSource, AL_BUFFER, 0 );
  {$ENDIF}

  {$IFDEF WIN32}
  TerminateThread( Thread, 0 );
  CloseHandle( Thread );
  {$ENDIF}

  {$IFDEF DARWIN}
  //KillThread( ThreadID );
  {$ENDIF}
end;

function snd_ProcFile;
  var
  {$IFDEF LINUX_OR_DARWIN}
    processed : TALint;
    buffer    : TALuint;
  {$ENDIF}
  {$IFDEF WIN32}
    Pos : DWORD;
  {$ENDIF}
begin
  try
    while not sndStopFile do
      begin
        if ( not Assigned( sfStream ) ) or
           ( not sndInitialized ) Then break;

        {$IFDEF LINUX_OR_DARWIN}
        alSourcef( sfSource, AL_GAIN, sndVolume / 100 );
        alGetSourcei( sfSource, AL_BUFFERS_PROCESSED, @processed );
        while ( processed > 0 ) and ( not sndStopFile ) do
          begin
            alSourceUnQueueBuffers( sfSource, 1, @buffer );

            if sfStream.CodecRead( sfStream.Buffer, sfStream.BufferSize ) = 0 Then
              begin
                if sfStream.Loop Then
                  sfStream.CodecLoop
                else
                  begin
                    sfStream^.Played := FALSE;
                    sfStream         := nil;
                    break;
                  end;
              end else sfStream^.Played := TRUE;

            alBufferData( buffer, sfFormat[ sfStream.Channels ], sfStream.Buffer, sfStream.BufferSize, sfStream.Rate );
            alSourceQueueBuffers( sfSource, 1, @buffer );

            DEC( processed );
          end;
        {$ENDIF}
        {$IFDEF WIN32}
        sfBuffer.SetVolume( -50 * ( 100 - sndVolume ) );
        sfBuffer.GetCurrentPosition( @Pos, @sfCurrPos );

        if not sfBufferRead[ sfBufferNext ] Then
          begin
            if sfStream.CodecRead( sfStream.Buffer, sfStream.BufferSize ) = 0 Then
              begin
                if sfStream.Loop Then
                  sfStream.CodecLoop
                else
                  begin
                    sfStream^.Played := FALSE;
                    sfStream         := nil;
                    break;
                  end;
              end else sfStream^.Played := TRUE;

            dsu_FillData( sfBuffer, sfStream.Buffer, sfStream.BufferSize, sfStream.Buffersize * sfBufferNext );
            sfBufferRead[ sfBufferNext ] := TRUE;
          end;

        if ( ( sfCurrPos > sfStream.BufferSize * sfBufferNext ) and ( sfBufferNext > 0 ) ) or
           ( ( sfCurrPos < sfStream.BufferSize ) and ( sfBufferNext = 0 ) ) Then
          begin
            sfBufferRead[ sfBufferPos ] := FALSE;
            INC( sfBufferPos );
            if sfBufferPos > 3 Then sfBufferPos := 0;
            INC( sfBufferNext );
            if sfBufferNext > 3 Then sfBufferNext := 0;
          end;
        {$ENDIF}
        {$IFNDEF DARWIN}
        sleep( 100 );
        {$ENDIF}
      end;
  except
  end;
  {$IFDEF LINUX_OR_DARWIN}
  alSourceQueueBuffers( sfSource, 1, @buffer );
  {$ENDIF}
  {$IFDEF WIN32}
  sfBuffer.Stop;
  {$ENDIF}
  sndStopFile := FALSE;
end;

procedure snd_RestoreFile;
  var
    i : Integer;
    {$IFDEF LINUX_OR_DARWIN}
    p : TALint;
    {$ENDIF}
begin
  if ( not Assigned( sfStream ) ) or
     ( not sndInitialized ) Then

  {$IFDEF LINUX_OR_DARWIN}
  for i := 0 to sfBufCount - 1 do
    begin
      if sfStream.CodecRead( sfStream.Buffer, sfStream.BufferSize ) = 0 Then break;

      alBufferData( sfBuffers[ i ], sfFormat[ sfStream.Channels ], sfStream.Buffer, sfStream.BufferSize, sfStream.Rate );
      alSourceQueueBuffers( sfSource, 1, @sfBuffers[ i ] );
    end;

  alSourcei( sfSource, AL_LOOPING, AL_FALSE );
  alSourceStop( sfSource );
  alSourceRewind( sfSource );
  alSourcePlay( sfSource );
  sfStream^.Played := TRUE;
  {$ENDIF}
  {$IFDEF WIN32}
  dsu_FillData( sfBuffer, sfStream.Buffer, sfStream.Buffersize, sfStream.Buffersize * sfBufferPos );
  sfBuffer.Play( 0, 0, DSBPLAY_LOOPING );
  sfBuffer.SetCurrentPosition( sfCurrPos );

  sfStream^.Played := TRUE;
  {$ENDIF}

  {$IFDEF LINUX}
  pthread_create( Thread, ThreadAttr^, @snd_ProcFile, nil );
  {$ENDIF}
  {$IFDEF WIN32}
  Thread := CreateThread( nil, 0, @snd_ProcFile, nil, 0, ThreadID );
  {$ENDIF}
  {$IFDEF DARWIN}
  ThreadID := BeginThread( @snd_ProcFile );
  {$ENDIF}
end;

end.
