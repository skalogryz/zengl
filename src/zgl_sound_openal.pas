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
unit zgl_sound_openal;

{$I zgl_config.cfg}

interface

const
{$IFDEF LINUX}
  libopenal = 'libopenal.so';
{$ENDIF}
{$IFDEF WIN32}
  libopenal = 'openal32.dll';
{$ENDIF}
{$IFDEF DARWIN}
  libopenal = '/System/Library/Frameworks/OpenAL.framework/OpenAL';
{$ENDIF}

  AL_NONE                                   = 0;
  AL_FALSE                                  = 0;
  AL_TRUE                                   = 1;

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

  AL_LOOPING                                =$1007;
  AL_GAIN                                   =$100A;
  AL_FREQUENCY                              =$2001;


type
  // OpenAL enumerations.
  TALenum = Integer;
  PALenum = ^TALenum;
  // OpenAL 32bit unsigned integer type.
  TALuint = Cardinal;
  PALuint = ^TALuint;
  // OpenAL 32bit signed integer type.
  TALint = Integer;
  PALint = ^TALint;
  // OpenAL 32bit floating point type.
  TALfloat = Single;
  PALfloat = ^TALfloat;
  // OpenAL 32bit type.
  TALsizei = Cardinal;
  PALsizei = ^TALsizei;

  // ALC enumerations.
  TALCenum = integer;
  PALCenum = ^TALCenum;
  // ALC 8bit unsigned byte.
  TALCubyte = Char;
  PALCubyte = PChar;
  // ALC void type
  TALCvoid = Pointer;
  PALCvoid = ^TALCvoid;
  // ALC device
  TALCdevice = TALCvoid;
  PALCdevice = ^TALCdevice;
  // ALC context
  TALCcontext = TALCvoid;
  PALCcontext = ^TALCcontext;
  // ALC 32bit signed integer type.
  TALCint = integer;
  PALCint = ^TALCint;
  // ALC 32bit floating point type.
  TALCfloat = single;
  PALCfloat = ^TALCfloat;

function  InitOpenAL : Boolean;
procedure FreeOpenAL;

var
  oal_Library : {$IFDEF WIN32} LongWord {$ELSE} Pointer {$ENDIF};

  // Device
  alcOpenDevice          : function(deviceName: PALCuByte): TALCdevice; cdecl;
  alcCloseDevice         : procedure(device: TALCdevice); cdecl;
  // Context
  alcCreateContext       : function(device: TALCdevice; attrlist: PALCint): TALCcontext; cdecl;
  alcMakeContextCurrent  : function(context: TALCcontext): TALCenum; cdecl;
  alcDestroyContext      : procedure(context: TALCcontext); cdecl;
  // Listener
  alListenerfv           : procedure(param: TALenum; values: PALfloat); cdecl;
  // Sources
  alGenSources           : procedure(n: TALsizei; sources: PALuint); cdecl;
  alDeleteSources        : procedure(n: TALsizei; sources: PALuint); cdecl;
  alSourcei              : procedure(source: TALuint; param: TALenum; value: TALint); cdecl;
  alSourcef              : procedure(source: TALuint; param: TALenum; value: TALfloat); cdecl;
  alSourcefv             : procedure(source: TALuint; param: TALenum; values: PALfloat); cdecl;
  alGetSourcei           : procedure(source: TALuint; param: TALenum; value: PALint); cdecl;
  alSourcePlay           : procedure(source: TALuint); cdecl;
  alSourceStop           : procedure(source: TALuint); cdecl;
  alSourceRewind         : procedure(source: TALuint); cdecl;
  //
  alSourceQueueBuffers   : procedure(source: TALuint; n: TALsizei; buffers: PALuint); cdecl;
  alSourceUnqueueBuffers : procedure(source: TALuint; n: TALsizei; buffers: PALuint); cdecl;
  // Buffers
  alGenBuffers           : procedure(n: TALsizei; buffers: PALuint); cdecl;
  alDeleteBuffers        : procedure(n: TALsizei; buffers: PALuint); cdecl;
  alBufferData           : procedure(buffer: TALuint; format: TALenum; data: Pointer; size, freq: TALsizei); cdecl;

  oal_Device  : TALCdevice  = nil;
  oal_Context : TALCcontext = nil;

  // Параметры слушателя
  oal_Position    : array[ 0..2 ] of TALfloat = ( 0.0, 0.0, 0.0);  //позиция
  oal_Velocity    : array[ 0..2 ] of TALfloat = ( 0.0, 0.0, 0.0 ); //движение
  oal_Orientation : array[ 0..5 ] of TALfloat = ( 0.0, 0.0, -1.0, 0.0, 1.0, 0.0 ); //ориентация

implementation
uses
  zgl_const,
  zgl_utils;

function InitOpenAL;
begin
  Result := FALSE;
  oal_Library := dlopen( libopenal {$IFDEF LINUX_OR_DARWIN}, $001 {$ENDIF} );
  {$IFDEF LINUX}
  // Для надежности...
  if oal_Library = nil Then oal_Library := dlopen( PChar( libopenal + '.0' ), $001 );
  if oal_Library = nil Then oal_Library := dlopen( PChar( libopenal + '.1' ), $001 );
  {$ENDIF}

  if oal_Library <> LIB_ERROR Then
    begin
      alcOpenDevice          := dlsym( oal_Library, 'alcOpenDevice' );
      alcCloseDevice         := dlsym( oal_Library, 'alcCloseDevice' );
      alcCreateContext       := dlsym( oal_Library, 'alcCreateContext' );
      alcMakeContextCurrent  := dlsym( oal_Library, 'alcMakeContextCurrent' );
      alcDestroyContext      := dlsym( oal_Library, 'alcDestroyContext' );
      alListenerfv           := dlsym( oal_Library, 'alListenerfv' );
      alGenSources           := dlsym( oal_Library, 'alGenSources' );
      alDeleteSources        := dlsym( oal_Library, 'alDeleteSources' );
      alSourcei              := dlsym( oal_Library, 'alSourcei' );
      alSourcef              := dlsym( oal_Library, 'alSourcef' );
      alSourcefv             := dlsym( oal_Library, 'alSourcefv' );
      alGetSourcei           := dlsym( oal_Library, 'alGetSourcei' );
      alSourcePlay           := dlsym( oal_Library, 'alSourcePlay' );
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

end.
