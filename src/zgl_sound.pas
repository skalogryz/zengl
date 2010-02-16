{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.inf.ua
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
  {$LINKLIB pthread}
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
    Source     : DWORD;
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
    Buffer     : DWORD;
    sCount     : DWORD;
    Channel    : array of zglTSoundChannel;

    Data       : Pointer;
    Size       : DWORD;
    Length     : Double;
    Frequency  : DWORD;

    Prev, Next : zglPSound;
  end;

  zglTSoundStream = record
    _Data      : Pointer;
    _File      : zglTFile;
    _Decoder   : zglPSoundDecoder;
    _Playing   : Boolean;
    _Paused    : Boolean;
    _Complete  : Double;
    _LastTime  : Double;

    Buffer     : Pointer;
    BufferSize : DWORD;

    Frequency  : DWORD;
    Channels   : DWORD;
    Length     : Double;

    Loop       : Boolean;
  end;

  zglTSoundDecoder = record
    Ext   : String;
    Open  : function( var Stream : zglTSoundStream; const FileName : String ) : Boolean;
    Read  : function( var Stream : zglTSoundStream; const Buffer : Pointer; const Count : DWORD; var _End : Boolean ) : DWORD;
    Loop  : procedure( var Stream : zglTSoundStream );
    Close : procedure( var Stream : zglTSoundStream );
  end;

  zglTSoundFormat = record
    Extension  : String;
    Decoder    : zglPSoundDecoder;
    FileLoader : procedure( const FileName : String; var Data : Pointer; var Size, Format, Frequency : DWORD );
    MemLoader  : procedure( const Memory : zglTMemory; var Data : Pointer; var Size, Format, Frequency : DWORD );
  end;

  zglTSoundManager = record
    Count   : record
      Items   : DWORD;
      Formats : DWORD;
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
function  snd_ProcFile( data : Pointer ) : {$IFDEF WIN32} PInteger; stdcall; {$ELSE} LongInt; register; {$ENDIF}

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
  sfLastPos : array[ 1..SND_MAX ] of DWORD;
  {$ENDIF}

  {$IFDEF LINUX_OR_DARWIN}
  Thread  : array[ 1..SND_MAX ] of DWORD;
  {$ENDIF}
  {$IFDEF WIN32}
  Thread   : array[ 1..SND_MAX ] of DWORD;
  ThreadID : array[ 1..SND_MAX ] of DWORD;
  {$ENDIF}

implementation
uses
  zgl_application,
  zgl_main,
  zgl_window,
  zgl_timers,
  zgl_log,
  zgl_utils;

function GetStatusPlaying( Source : {$IFDEF USE_OPENAL} DWORD {$ELSE} IDirectSoundBuffer {$ENDIF} ) : Integer;
  var
    Status : {$IFDEF USE_OPENAL} LongInt {$ELSE} DWORD {$ENDIF};
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
        if timer_GetTicks - sfStream[ i ]._LastTime >= 10 Then
          begin
            sfStream[ i ]._Complete := timer_GetTicks - sfStream[ i ]._LastTime + sfStream[ i ]._Complete;
            if sfStream[ i ]._Complete > sfStream[ i ].Length Then
              sfStream[ i ]._Complete := sfStream[ i ].Length;
            sfStream[ i ]._LastTime := timer_GetTicks;
          end;
      end else
        begin
          if sfCanUse[ i ] < 100 Then
            INC( sfCanUse[ i ] );
          sfStream[ i ]._LastTime := timer_GetTicks;
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
            if sfStream[ i ]._Playing and ( not sfStream[ i ]._Paused ) Then
              begin
                sfStream[ i ]._Paused := TRUE;
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
            if sfStream[ i ]._Playing and ( not sfStream[ i ]._Paused ) Then
              begin
                snd_PauseFile( i );
                sfStream[ i ]._Paused := FALSE;
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

  log_Add( 'OpenAL: open device' );
  log_Add( 'OpenAL: Default device is "' + alcGetString( nil, ALC_DEFAULT_DEVICE_SPECIFIER ) + '"' );

  {$IFDEF WIN32}
  //oal_Device := alcOpenDevice( 'Generic Software' );
  //if not Assigned( oal_Device ) Then
  {$ENDIF}
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
      if ( i = 0 ) Then break;
      SetLength( oal_Sources, i );
      alGenSources( i, @oal_Sources[ 0 ] );
    end;
  SetLength( oal_SrcPtrs, i );
  SetLength( oal_SrcState, i );

  log_Add( 'OpenAL: generate ' + u_IntToStr( length( oal_Sources ) ) + ' source' );
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
    if Assigned( sfStream[ i ]._Decoder ) Then
      begin
        sfStream[ i ]._Decoder.Close( sfStream[ i ] );
        if Assigned( sfStream[ i ].Buffer ) Then
          FreeMemory( sfStream[ i ].Buffer );
        if Assigned( sfStream[ i ]._Data ) Then
          FreeMemory( sfStream[ i ]._Data );
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
  FreeOpenAL;
{$ELSE}
  for i := 1 to SND_MAX do
    sfSource[ i ]  := nil;
  ds_Device := nil;

  FreeDSound;
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
  while Assigned( Result.Next ) do
    Result := Result.Next;

  zgl_GetMem( Pointer( Result.Next ), SizeOf( zglTSound ) );
  Result.Next.Prev := Result;
  Result.Next.Next := nil;
  Result           := Result.Next;

  Result.sCount := SourceCount;
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
  for i := 0 to Sound.sCount - 1 do
    Sound.Channel[ i ].Source := nil;
{$ENDIF}
  SetLength( Sound.Channel, 0 );

  if Assigned( Sound.Prev ) Then
    Sound.Prev.Next := Sound.Next;
  if Assigned( Sound.Next ) Then
    Sound.Next.Prev := Sound.Prev;

  FreeMemory( Sound );
  DEC( managerSound.Count.Items );

  Sound := nil;
end;

function snd_LoadFromFile;
  var
    i   : Integer;
    f   : DWORD;
    ext : String;
begin
  Result := nil;

  if not sndInitialized Then exit;

  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Cannot read ' + FileName );
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
      log_Add( 'Cannot load sound: ' + FileName );
      snd_Del( Result );
      exit;
    end;

{$IFDEF USE_OPENAL}
  alBufferData( Result.Buffer, f, Result.Data, Result.Size, Result.Frequency );
  FreeMemory( Result.Data );
{$ELSE}
  dsu_CreateBuffer( Result.Channel[ 0 ].Source, Result.Size, Pointer( f ) );
  dsu_FillData( Result.Channel[ 0 ].Source, Result.Data, Result.Size );
  for i := 1 to Result.sCount - 1 do
    ds_Device.DuplicateSoundBuffer( Result.Channel[ 0 ].Source, Result.Channel[ i ].Source );
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
  for i := 1 to Result.sCount - 1 do
    ds_Device.DuplicateSoundBuffer( Result.Channel[ 0 ].Source, Result.Channel[ i ].Source );
{$ENDIF}
end;

function snd_Play;
  var
    i : Integer;
    {$IFNDEF USE_OPENAL}
    DSERROR : HRESULT;
    Status  : DWORD;
    Vol     : Single;
    {$ELSE}
    j       : Integer;
    {$ENDIF}
begin
  Result := -1;

  if ( not Assigned( Sound ) ) or
     ( not sndInitialized ) or
     ( not sndCanPlay ) Then exit;

{$IFDEF USE_OPENAL}
  for i := 0 to Sound.sCount - 1 do
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
  for i := 0 to Sound.sCount - 1 do
    begin
      DSERROR := Sound.Channel[ i ].Source.GetStatus( Status );
      if DSERROR <> DS_OK Then Status := 0;
      if ( Status and DSBSTATUS_PLAYING ) = 0 Then
        begin
          if ( Status and DSBSTATUS_BUFFERLOST ) <> 0 Then
            begin
              Sound.Channel[ i ].Source.Restore;
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

  Sound.Channel[ Result ].Source.SetPan( dsu_CalcPos( X, Y, Z, Vol ) );
  Sound.Channel[ Result ].Source.SetVolume( dsu_CalcVolume( Vol * sndVolume ) );
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
        alSourcei( Sound.Channel[ ID ].Source, AL_BUFFER, AL_NONE );
        {$ELSE}
        Sound.Channel[ ID ].Source.Stop;
        {$ENDIF}
      end;
  end;
begin
  if not sndInitialized Then exit;

  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.sCount - 1 do
            Stop( Sound, i );
        end else
          if ID >= 0 Then
            Stop( Sound, ID );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.Next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.sCount - 1 do
                Stop( snd, j );
              snd := snd.Next;
            end;
        end;
end;

procedure snd_SetPos;
  var
    i, j : Integer;
    snd  : zglPSound;
    {$IFNDEF USE_OPENAL}
    Vol  : Single;
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
        Sound.Channel[ ID ].Source.SetPan   ( dsu_CalcPos( X, Y, Z, Vol ) );
        Sound.Channel[ ID ].Source.SetVolume( dsu_CalcVolume( Vol * Sound.Channel[ ID ].Volume ) );
        {$ENDIF}
      end;
  end;
begin
  if not sndInitialized Then exit;

  if ( ID = SND_STREAM ) Then
    begin
      if ( Sound <> nil ) and ( sfStream[ DWORD( Sound ) ]._Playing ) Then
        begin
          sfPositions[ DWORD( Sound ), 0 ] := X;
          sfPositions[ DWORD( Sound ), 1 ] := Y;
          sfPositions[ DWORD( Sound ), 2 ] := Z;

          {$IFDEF USE_OPENAL}
          alSourcefv( DWORD( Sound ), AL_POSITION, @sfPositions[ DWORD( Sound ), 0 ] );
          {$ELSE}
          sfSource[ DWORD( Sound ) ].SetPan( dsu_CalcPos( sfPositions[ DWORD( Sound ), 0 ],
                                                          sfPositions[ DWORD( Sound ), 1 ],
                                                          sfPositions[ DWORD( Sound ), 2 ], Vol ) );
          sfSource[ DWORD( Sound ) ].SetVolume( dsu_CalcVolume( Vol * sfVolumes[ DWORD( Sound ) ] ) );
          {$ENDIF}
        end else
          for i := 1 to SND_MAX do
            if sfStream[ DWORD( Sound ) ]._Playing Then
              begin
                sfPositions[ i, 0 ] := X;
                sfPositions[ i, 1 ] := Y;
                sfPositions[ i, 2 ] := Z;

                {$IFDEF USE_OPENAL}
                alSourcefv( sfSource[ i ], AL_POSITION, @sfPositions[ i, 0 ] );
                {$ELSE}
                sfSource[ i ].SetPan( dsu_CalcPos( sfPositions[ i, 0 ],
                                                   sfPositions[ i, 1 ],
                                                   sfPositions[ i, 2 ], Vol ) );
                sfSource[ i ].SetVolume( dsu_CalcVolume( Vol * sfVolumes[ i ] ) );
                {$ENDIF}
              end;
      exit;
    end;

  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.sCount - 1 do
            SetPos( Sound, i, X, Y, Z );
        end else
          if ID >= 0 Then
            SetPos( Sound, ID, X, Y, Z );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.Next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.sCount - 1 do
                SetPos( snd, j, X, Y, Z );
              snd := snd.Next;
            end;
        end;
end;

procedure snd_SetVolume;
  var
    i, j : Integer;
    snd  : zglPSound;
    {$IFNDEF USE_OPENAL}
    Vol  : Single;
    {$ENDIF}
  procedure SetVolume( const Sound : zglPSound; const ID : Integer; const Volume : Single );
  begin
    Sound.Channel[ ID ].Volume := Volume;

    if Sound.Channel[ ID ].Source <> SND_ERROR Then
      begin
        {$IFDEF USE_OPENAL}
        alSourcef( Sound.Channel[ ID ].Source, AL_GAIN, Sound.Channel[ ID ].Volume );
        {$ELSE}
        Sound.Channel[ ID ].Source.SetPan( dsu_CalcPos( Sound.Channel[ ID ].Position.X,
                                                        Sound.Channel[ ID ].Position.Y,
                                                        Sound.Channel[ ID ].Position.Z, Vol ) );
        Sound.Channel[ ID ].Source.SetVolume( dsu_CalcVolume( Vol * Sound.Channel[ ID ].Volume ) );
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
      if ( Sound <> nil ) and ( sfStream[ DWORD( Sound ) ]._Playing ) Then
        begin
          sfVolumes[ DWORD( Sound ) ] := Volume;

          {$IFDEF USE_OPENAL}
          alSourcef( sfSource[ DWORD( Sound ) ], AL_GAIN, Volume );
          {$ELSE}
          sfSource[ DWORD( Sound ) ].SetPan( dsu_CalcPos( sfPositions[ DWORD( Sound ), 0 ],
                                                          sfPositions[ DWORD( Sound ), 1 ],
                                                          sfPositions[ DWORD( Sound ), 2 ], Vol ) );
          sfSource[ DWORD( Sound ) ].SetVolume( dsu_CalcVolume( Vol * sfVolumes[ DWORD( Sound ) ] ) );
          {$ENDIF}
        end else
          for i := 1 to SND_MAX do
            if sfStream[ DWORD( Sound ) ]._Playing Then
              begin
                sfVolumes[ i ] := Volume;

                {$IFDEF USE_OPENAL}
                alSourcef( sfSource[ i ], AL_GAIN, Volume );
                {$ELSE}
                sfSource[ i ].SetPan( dsu_CalcPos( sfPositions[ i, 0 ],
                                                   sfPositions[ i, 1 ],
                                                   sfPositions[ i, 2 ], Vol ) );
                sfSource[ i ].SetVolume( dsu_CalcVolume( Vol * sfVolumes[ i ] ) );
                {$ENDIF}
              end;
      exit;
    end;

  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.sCount - 1 do
            SetVolume( Sound, i, Volume );
        end else
          if ID >= 0 Then
            SetVolume( Sound, ID, Volume );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.Next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.sCount - 1 do
                SetVolume( snd, j, Volume );
              snd := snd.Next;
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

  if ( ID = SND_STREAM ) and ( Sound <> nil ) and ( sfStream[ DWORD( Sound ) ]._Playing ) Then
    begin
      {$IFDEF USE_OPENAL}
      alSourcef( sfSource[ DWORD( Sound ) ], AL_FREQUENCY, Frequency );
      {$ELSE}
      sfSource[ DWORD( Sound ) ].SetFrequency( Frequency );
      {$ENDIF}
      exit;
    end else
      if ID = SND_STREAM Then
        for i := 1 to SND_MAX do
          if sfStream[ i ]._Playing Then
            {$IFDEF USE_OPENAL}
            alSourcef( sfSource[ i ], AL_FREQUENCY, Frequency );
            {$ELSE}
            sfSource[ i ].SetFrequency( Frequency );
            {$ENDIF}

  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.sCount - 1 do
            SetFrequency( Sound, i, Frequency );
        end else
          if ID >= 0 Then
            SetFrequency( Sound, ID, Frequency );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.Next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.sCount - 1 do
                SetFrequency( snd, j, Frequency );
              snd := snd.Next;
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

  if ( ID = SND_STREAM ) and ( Sound <> nil ) and ( sfStream[ DWORD( Sound ) ]._Playing ) Then
    begin
      {$IFDEF USE_OPENAL}
      alSourcef( sfSource[ DWORD( Sound ) ], AL_FREQUENCY, Round( sfStream[ DWORD( Sound ) ].Frequency * Coefficient ) );
      {$ELSE}
      sfSource[ DWORD( Sound ) ].SetFrequency( Round( sfStream[ DWORD( Sound ) ].Frequency * Coefficient ) );
      {$ENDIF}
      exit;
    end else
      if ID = SND_STREAM Then
        for i := 1 to SND_MAX do
          if sfStream[ i ]._Playing Then
            {$IFDEF USE_OPENAL}
            alSourcef( sfSource[ i ], AL_FREQUENCY, Round( sfStream[ i ].Frequency * Coefficient ) );
            {$ELSE}
            sfSource[ i ].SetFrequency( Round( sfStream[ i ].Frequency * Coefficient ) );
            {$ENDIF}

  if Assigned( Sound ) Then
    begin
      if ID = SND_ALL Then
        begin
          for i := 0 to Sound.sCount - 1 do
            SetFrequency( Sound, i, Round( Sound.Frequency * Coefficient ) );
        end else
          if ID >= 0 Then
            SetFrequency( Sound, ID, Round( Sound.Frequency * Coefficient ) );
    end else
      if ID = SND_ALL Then
        begin
          snd := managerSound.First.Next;
          for i := 0 to managerSound.Count.Items - 1 do
            begin
              for j := 0 to snd.sCount - 1 do
                SetFrequency( snd, j, Round( snd.Frequency * Coefficient ) );
              snd := snd.Next;
            end;
        end;
end;

function snd_Get;
begin
  if not sndInitialized Then exit;

  if ID = SND_STREAM Then
    begin
      case What of
        SND_STATE_PLAYING: Result := GetStatusPlaying( sfSource[ DWORD( Sound ) ] );
        SND_STATE_TIME: Result := Round( sfStream[ DWORD( Sound ) ]._Complete );
        SND_STATE_PERCENT: Result := Round( 100 / sfStream[ DWORD( Sound ) ].Length * sfStream[ DWORD( Sound ) ]._Complete );
        SND_INFO_LENGTH: Result := Round( sfStream[ DWORD( Sound ) ].Length );
      end;
    end else
      begin
        if not Assigned( Sound ) Then
          begin
            Result := -1;
            exit;
          end;
        case What of
          SND_STATE_PLAYING: Result := GetStatusPlaying( Sound.Channel[ ID ].Source );
          SND_INFO_LENGTH: Result := Round( Sound.Length );
        end;
      end;
end;

function snd_GetStreamID;
  var
    i : Integer;
begin
  for i := 1 to SND_MAX do
    if ( not sfStream[ i ]._Playing ) and ( sfCanUse[ i ] = 100 ) Then
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
    _End      : Boolean;
    BytesRead : Integer;
    {$IFDEF USE_OPENAL}
    {$ELSE}
    buffDesc : zglTBufferDesc;
    {$ENDIF}
begin
  if ( not sndInitialized ) or
     ( not sndCanPlayFile ) Then exit;

  Result := snd_GetStreamID;
  if Result = -1 Then
    exit;

  if Assigned( sfStream[ Result ]._Decoder ) Then
    begin
      sfStream[ Result ]._Decoder.Close( sfStream[ Result ] );
      if Assigned( sfStream[ Result ].Buffer ) Then
        FreeMemory( sfStream[ Result ].Buffer );
      if Assigned( sfStream[ Result ]._Data ) Then
        FreeMemory( sfStream[ Result ]._Data );
      {$IFDEF WIN32}
      CloseHandle( Thread[ Result ] );
      {$ENDIF}
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
        sfStream[ Result ]._Decoder := managerSound.Formats[ i ].Decoder;
    end;

  if Assigned( sfStream[ Result ]._Decoder ) Then
    sfStream[ Result ].Loop := Loop;

  if ( not Assigned( sfStream[ Result ]._Decoder ) ) or
     ( not sfStream[ Result ]._Decoder.Open( sfStream[ Result ], FileName ) ) Then
    begin
      sfStream[ Result ]._Decoder := nil;
      log_Add( 'Cannot play: ' + FileName );
      exit;
    end;

{$IFDEF USE_OPENAL}
  alSourceStop( sfSource[ Result ] );
  alSourceRewind( sfSource[ Result ] );
  alSourcei( sfSource[ Result ], AL_BUFFER, AL_NONE );

  for i := 0 to sfBufCount - 1 do
    begin
      BytesRead := sfStream[ Result ]._Decoder.Read( sfStream[ Result ], sfStream[ Result ].Buffer, sfStream[ Result ].BufferSize, _End );
      if BytesRead <= 0 Then break;

      alBufferData( sfBuffers[ Result, i ], sfFormat[ sfStream[ Result ].Channels ], sfStream[ Result ].Buffer, BytesRead, sfStream[ Result ].Frequency );
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
  BytesRead := sfStream[ Result ]._Decoder.Read( sfStream[ Result ], sfStream[ Result ].Buffer, sfStream[ Result ].BufferSize, _End );
  dsu_FillData( sfSource[ Result ], sfStream[ Result ].Buffer, BytesRead );

  sfLastPos[ Result ] := 0;
  sfSource[ Result ].SetCurrentPosition( 0 );
  sfSource[ Result ].Play( 0, 0, DSBPLAY_LOOPING );
  sfSource[ Result ].SetVolume( dsu_CalcVolume( sfVolume ) );
  sfSource[ Result ].SetFrequency( sfStream[ Result ].Frequency );
{$ENDIF}

  sfStream[ Result ]._Playing  := TRUE;
  sfStream[ Result ]._Paused   := FALSE;
  sfStream[ Result ]._Complete := 0;
  sfStream[ Result ]._LastTime := timer_GetTicks;
{$IFDEF LINUX_OR_DARWIN}
  Thread[ Result ] := BeginThread( @snd_ProcFile, Pointer( Result ) );
{$ENDIF}
{$IFDEF WIN32}
  Thread[ Result ] := CreateThread( nil, 0, @snd_ProcFile, Pointer( Result ), 0, ThreadID[ Result ] );
{$ENDIF}
end;

procedure snd_PauseFile;
begin
  if ( not Assigned( sfStream[ ID ]._Decoder ) ) or
     ( not sfStream[ ID ]._Playing ) or
     ( sfStream[ ID ]._Paused ) or
     ( not sndInitialized ) Then exit;

  sfStream[ ID ]._Paused := TRUE;
{$IFDEF WIN32}
  SuspendThread( Thread[ ID ] );
{$ENDIF}

{$IFDEF USE_OPENAL}
  alSourcePause( sfSource[ ID ] );
{$ELSE}
  sfSource[ ID ].Stop;
{$ENDIF}
end;

procedure snd_StopFile;
begin
  if ( not Assigned( sfStream[ ID ]._Decoder ) ) or
     ( not sfStream[ ID ]._Playing ) or
     ( not sndInitialized ) Then exit;

  sfStream[ ID ]._Playing := FALSE;
{$IFDEF USE_OPENAL}
  alSourceStop( sfSource[ ID ] );
{$ELSE}
  sfSource[ ID ].Stop;
{$ENDIF}
end;

procedure snd_ResumeFile;
begin
  if ( not Assigned( sfStream[ ID ]._Decoder ) ) or
     ( not sfStream[ ID ]._Playing ) or
     ( not sfStream[ ID ]._Paused ) or
     ( not sndInitialized ) Then exit;

  sfStream[ ID ]._Paused := FALSE;
{$IFDEF USE_OPENAL}
  alSourcePlay( sfSource[ ID ] );
{$ELSE}
  sfSource[ ID ].Play( 0, 0, DSBPLAY_LOOPING );
{$ENDIF}

{$IFDEF WIN32}
  ResumeThread( Thread[ ID ] );
{$ENDIF}
end;

function snd_ProcFile;
  var
    ID   : Integer;
    _End : Boolean;
  {$IFDEF USE_OPENAL}
    BytesRead : Integer;
    processed : LongInt;
    buffer    : LongWord;
  {$ELSE}
    Block1, Block2 : Pointer;
    b1Size, b2Size : DWORD;
    Position       : DWORD;
    FillSize       : DWORD;
  {$ENDIF}
begin
  Result := 0;
  ID := DWORD( data );

  {$IFDEF USE_OPENAL}
  processed := 0;
  while ( processed < 1 ) and sfStream[ ID ]._Playing do
    alGetSourcei( sfSource[ ID ], AL_BUFFERS_PROCESSED, processed );
  {$ENDIF}
  while app_Work and sfStream[ ID ]._Playing do
    begin
      if not sndInitialized Then break;

      u_Sleep( 100 );
      if ( not app_Work ) or ( not sfStream[ ID ]._Playing ) Then break;
      {$IFDEF USE_OPENAL}
      alGetSourcei( sfSource[ ID ], AL_BUFFERS_PROCESSED, processed );
      while app_Work and ( processed > 0 ) and sfStream[ ID ]._Playing do
        begin
          alSourceUnQueueBuffers( sfSource[ ID ], 1, @buffer );

          BytesRead := sfStream[ ID ]._Decoder.Read( sfStream[ ID ], sfStream[ ID ].Buffer, sfStream[ ID ].BufferSize, _End );
          alBufferData( buffer, sfFormat[ sfStream[ ID ].Channels ], sfStream[ ID ].Buffer, BytesRead, sfStream[ ID ].Frequency );
          alSourceQueueBuffers( sfSource[ ID ], 1, @buffer );

          if _End Then break;

          DEC( processed );
        end;
      {$IFDEF LINUX_OR_DARWIN}
      while sfStream[ ID ]._Paused do u_Sleep( 10 );
      {$ENDIF}
      {$ELSE}
      Windows.EnterCriticalSection( sfCS );
      while DWORD( sfSource[ ID ].GetCurrentPosition( @Position, @b1Size ) ) = DSERR_BUFFERLOST do
        sfSource[ ID ].Restore;
      Windows.LeaveCriticalSection( sfCS );

      FillSize := ( sfStream[ ID ].BufferSize + Position - sfLastPos[ ID ] ) mod sfStream[ ID ].BufferSize;

      Block1 := nil;
      Block2 := nil;
      b1Size := 0;
      b2Size := 0;

      if sfSource[ ID ].Lock( sfLastPos[ ID ], FillSize, Block1, b1Size, Block2, b2Size, 0 ) <> DS_OK Then break;
      sfLastPos[ ID ] := Position;

      sfStream[ ID ]._Decoder.Read( sfStream[ ID ], Block1, b1Size, _End );
      if ( b2Size <> 0 ) and ( not _End ) Then
        sfStream[ ID ]._Decoder.Read( sfStream[ ID ], Block2, b2Size, _End );

      sfSource[ ID ].Unlock( Block1, b1Size, Block2, b2Size );
      {$ENDIF}
      if sfStream[ ID ]._Complete >= sfStream[ ID ].Length Then
        begin
          sfStream[ ID ]._Complete := 0;
          sfStream[ ID ]._LastTime := timer_GetTicks;
        end;
      if _End then
        begin
          if sfStream[ ID ].Loop Then
            begin
              sfStream[ ID ]._Decoder.Loop( sfStream[ ID ] );
            end else
              begin
                while sfStream[ ID ]._Complete < sfStream[ ID ].Length do
                  begin
                    sfStream[ ID ]._Complete := timer_GetTicks - sfStream[ ID ]._LastTime + sfStream[ ID ]._Complete;
                    sfStream[ ID ]._LastTime := timer_GetTicks;
                    u_Sleep( 10 );
                  end;
                if sfStream[ ID ]._Complete > sfStream[ ID ].Length Then
                  sfStream[ ID ]._Complete := sfStream[ ID ].Length;
                sfStream[ ID ]._Playing := FALSE;
              end;
        end;
    end;
  if not app_Work Then
    {$IFDEF LINUX_OR_DARWIN} EndThread( 0 ); {$ELSE} exit; {$ENDIF}

{$IFNDEF USE_OPENAL}
  sfSource[ ID ].Stop;
{$ENDIF}

{$IFDEF LINUX_OR_DARWIN}
  EndThread( 0 );
{$ENDIF}
end;

end.
