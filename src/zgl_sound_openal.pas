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
unit zgl_sound_openal;

{$I zgl_config.cfg}

interface

const
{$IFDEF LINUX}
  libopenal = 'libopenal.so';
{$ENDIF}
{$IFDEF WINDOWS}
  libopenal = 'openal32.dll';
{$ENDIF}
{$IFDEF DARWIN}
  libopenal = '/System/Library/Frameworks/OpenAL.framework/OpenAL';
{$ENDIF}

  ALC_DEFAULT_DEVICE_SPECIFIER              =$1004;
  ALC_DEVICE_SPECIFIER                      =$1005;

  AL_NONE                                   = 0;
  AL_FALSE                                  = 0;
  AL_TRUE                                   = 1;
  AL_NO_ERROR                               = 0;

  //Sound samples: format specifier.
  AL_FORMAT_MONO8                           =$1100;
  AL_FORMAT_MONO16                          =$1101;
  AL_FORMAT_STEREO8                         =$1102;
  AL_FORMAT_STEREO16                        =$1103;

  //Source state information.
  AL_SOURCE_STATE                           =$1010;
  AL_INITIAL                                =$1011;
  AL_PLAYING                                =$1012;
  AL_PAUSED                                 =$1013;
  AL_STOPPED                                =$1014;

  AL_BUFFER                                 =$1009;
  AL_BUFFERS_PROCESSED                      =$1016;

  AL_POSITION                               =$1004;
  AL_DIRECTION                              =$1005;
  AL_VELOCITY                               =$1006;
  AL_ORIENTATION                            =$100F;

  AL_PITCH                                  =$1003;
  AL_LOOPING                                =$1007;
  AL_GAIN                                   =$100A;
  AL_FREQUENCY                              =$2001;

function  InitOpenAL : Boolean;
procedure FreeOpenAL;

function oal_GetSource( const Source : Pointer ) : LongWord;

type
  PALCdevice = ^ALCdevice;
  ALCdevice  = record
end;

type
  PALCcontext = ^ALCcontext;
  ALCcontext  = record
end;

var
  oal_Library : {$IFDEF WINDOWS} LongWord {$ELSE} Pointer {$ENDIF};

  alcGetString           : function(device: PALCdevice; param: LongInt): PAnsiChar; cdecl;
  alGetError             : function(device: PALCdevice): LongInt; cdecl;
  // Device
  alcOpenDevice          : function(const devicename: PAnsiChar): PALCdevice; cdecl;
  alcCloseDevice         : function(device: PALCdevice): Boolean; cdecl;
  // Context
  alcCreateContext       : function(device: PALCdevice; const attrlist: PLongInt): PALCcontext; cdecl;
  alcMakeContextCurrent  : function(context: PALCcontext): Boolean; cdecl;
  alcDestroyContext      : procedure(context: PALCcontext); cdecl;
  // Listener
  alListenerfv           : procedure(param: LongInt; const values: PSingle); cdecl;
  // Sources
  alGenSources           : procedure(n: LongInt; sources: PLongWord); cdecl;
  alDeleteSources        : procedure(n: LongInt; const sources: PLongWord); cdecl;
  alSourcei              : procedure(sid: LongWord; param: LongInt; value: LongInt); cdecl;
  alSourcef              : procedure(sid: LongWord; param: LongInt; value: Single); cdecl;
  alSourcefv             : procedure(sid: LongWord; param: LongInt; const values: PSingle); cdecl;
  alGetSourcei           : procedure(sid: LongWord; param: LongInt; var value: LongInt); cdecl;
  alSourcePlay           : procedure(sid: LongWord); cdecl;
  alSourcePause          : procedure(sid: LongWord); cdecl;
  alSourceStop           : procedure(sid: LongWord); cdecl;
  alSourceRewind         : procedure(sid: LongWord); cdecl;
  //
  alSourceQueueBuffers   : procedure(sid: LongWord; numEntries: LongInt; const bids: PLongWord); cdecl;
  alSourceUnqueueBuffers : procedure(sid: LongWord; numEntries: LongInt; bids: PLongWord); cdecl;
  // Buffers
  alGenBuffers           : procedure(n: LongInt; buffers: PLongWord); cdecl;
  alDeleteBuffers        : procedure(n: LongInt; const buffers: PLongWord); cdecl;
  alBufferData           : procedure(bid: LongWord; format: LongInt; data: Pointer; size: LongInt; freq: LongInt); cdecl;

  oal_Device  : PALCdevice  = nil;
  oal_Context : PALCcontext = nil;
  oal_Sources : array of LongWord;
  oal_SrcPtrs : array of Pointer;
  oal_SrcState: array of LongWord;

  // Параметры слушателя
  oal_Position    : array[ 0..2 ] of Single = ( 0.0, 0.0, 0.0);  //позиция
  oal_Velocity    : array[ 0..2 ] of Single = ( 0.0, 0.0, 0.0 ); //движение
  oal_Orientation : array[ 0..5 ] of Single = ( 0.0, 0.0, -1.0, 0.0, 1.0, 0.0 ); //ориентация

  // Форматы звука для количества каналов
  oal_Format  : array[ 1..2 ] of LongInt = ( AL_FORMAT_MONO16, AL_FORMAT_STEREO16 );

implementation
uses
  zgl_utils;

function InitOpenAL : Boolean;
begin
  Result := FALSE;
  oal_Library := dlopen( libopenal {$IFDEF LINUX_OR_DARWIN}, $001 {$ENDIF} );
  {$IFDEF LINUX}
  // Для надежности...
  if oal_Library = nil Then oal_Library := dlopen( PChar( libopenal + '.1' ), $001 );
  if oal_Library = nil Then oal_Library := dlopen( PChar( libopenal + '.0' ), $001 );
  {$ENDIF}

  if oal_Library <> LIB_ERROR Then
    begin
      alcGetString           := dlsym( oal_Library, 'alcGetString' );
      alcOpenDevice          := dlsym( oal_Library, 'alcOpenDevice' );
      alcCloseDevice         := dlsym( oal_Library, 'alcCloseDevice' );
      alcCreateContext       := dlsym( oal_Library, 'alcCreateContext' );
      alcMakeContextCurrent  := dlsym( oal_Library, 'alcMakeContextCurrent' );
      alcDestroyContext      := dlsym( oal_Library, 'alcDestroyContext' );
      alGetError             := dlsym( oal_Library, 'alGetError' );
      alListenerfv           := dlsym( oal_Library, 'alListenerfv' );
      alGenSources           := dlsym( oal_Library, 'alGenSources' );
      alDeleteSources        := dlsym( oal_Library, 'alDeleteSources' );
      alSourcei              := dlsym( oal_Library, 'alSourcei' );
      alSourcef              := dlsym( oal_Library, 'alSourcef' );
      alSourcefv             := dlsym( oal_Library, 'alSourcefv' );
      alGetSourcei           := dlsym( oal_Library, 'alGetSourcei' );
      alSourcePlay           := dlsym( oal_Library, 'alSourcePlay' );
      alSourcePause          := dlsym( oal_Library, 'alSourcePause' );
      alSourceStop           := dlsym( oal_Library, 'alSourceStop' );
      alSourceRewind         := dlsym( oal_Library, 'alSourceRewind' );
      alSourceQueueBuffers   := dlsym( oal_Library, 'alSourceQueueBuffers' );
      alSourceUnqueueBuffers := dlsym( oal_Library, 'alSourceUnqueueBuffers' );
      alGenBuffers           := dlsym( oal_Library, 'alGenBuffers' );
      alDeleteBuffers        := dlsym( oal_Library, 'alDeleteBuffers' );
      alBufferData           := dlsym( oal_Library, 'alBufferData' );

      Result := TRUE;
    end else
      Result := FALSE;
end;

procedure FreeOpenAL;
begin
  dlclose( oal_Library );
end;

function oal_GetSource( const Source : Pointer ) : LongWord;
  var
    i, state : Integer;
begin
  Result := 0;
  for i := 0 to length( oal_Sources ) - 1 do
    begin
      alGetSourcei( oal_Sources[ i ], AL_SOURCE_STATE, state );
      if state <> AL_PLAYING Then
        begin
          if Assigned( oal_SrcPtrs[ i ] ) Then
            LongWord( oal_SrcPtrs[ i ]^ ) := 0;
          oal_SrcPtrs[ i ] := Source;
          Result := oal_Sources[ i ];
          break;
        end;
    end;
end;

end.
