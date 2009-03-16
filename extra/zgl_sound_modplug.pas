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
unit zgl_sound_modplug;

interface
uses
  zgl_const,
  zgl_main,
  zgl_sound,
  zgl_log,
  zgl_file,
  zgl_memory,
  zgl_utils;

const
{$IFDEF WIN32}
  libmodplug = 'libmodplug.dll';
{$ENDIF}
{$IFDEF LINUX}
  libmodplug = 'libmodplug.so';
{$ENDIF}
{$IFDEF DARWIN}
  libmodplug = 'libmodplug.dylib';
{$ENDIF}
  MAX_FORMATS = 22;

const
  MODPLUG_ENABLE_OVERSAMPLING     = 1 shl 0;
  MODPLUG_ENABLE_NOISE_REDUCTION  = 1 shl 1;
  MODPLUG_ENABLE_REVERB           = 1 shl 2;
  MODPLUG_ENABLE_MEGABASS         = 1 shl 3;
  MODPLUG_ENABLE_SURROUND         = 1 shl 4;

  MODPLUG_RESAMPLE_NEAREST = 0;
  MODPLUG_RESAMPLE_LINEAR  = 1;
  MODPLUG_RESAMPLE_SPLINE  = 2;
  MODPLUG_RESAMPLE_FIR     = 3;

{***********************************************************************}
{                       POSIX TYPE DEFINITIONS                          }
{***********************************************************************}
type
  cint8   = shortint; pcint8   = ^cint8;
  cuint8  = byte;     pcuint8  = ^cuint8;
  cchar   = cint8;    pcchar   = ^cchar;
  cschar  = cint8;    pcschar  = ^cschar;
  cuchar  = cuint8;   pcuchar  = ^cuchar;
  cint32  = longint;  pcint32  = ^cint32;
  cuint32 = longword; pcuint32 = ^cuint32;
  cint    = cint32;   pcint    = ^cint;
  csint   = cint32;   pcsint   = ^csint;
  cuint   = cuint32;  pcuint   = ^cuint;
  cint64  = int64;    pcint64  = ^cint64;
  cbool   = longbool; pcbool   = ^cbool;
{$ifdef cpu64}
  clong   = int64;    pclong   = ^clong;
  cslong  = int64;    pcslong  = ^cslong;
  culong  = qword;    pculong  = ^culong;
{$else}
  clong   = longint;  pclong   = ^clong;
  cslong  = longint;  pcslong  = ^cslong;
  culong  = cardinal; pculong  = ^culong;
{$endif}
  cfloat  = single;   pcfloat  = ^cfloat;
  cdouble = double;   pcdouble = ^cdouble;

  csize_t      = culong;

type
  PModPlugFile = ^ModPlugFile;
  ModPlugFile = record
  end;

type
  PModPlug_Settings = ^ModPlug_Settings;
  ModPlug_Settings = record
    mFlags          : cint;
    mChannels       : cint;
    mBits           : cint;
    mFrequency      : cint;
    mResamplingMode : cint;
    mReverbDepth    : cint;
    mReverbDelay    : cint;
    mBassAmount     : cint;
    mBassRange      : cint;
    mSurroundDepth  : cint;
    mSurroundDelay  : cint;
    mLoopCount      : cint;
  end;

procedure mp_Init;
function  mp_CodecOpen( const FileName : String; var Stream : zglPSoundStream ) : Boolean;
function  mp_CodecRead( const Buffer : Pointer; const Count : DWORD; var _End : Boolean ) : DWORD;
procedure mp_CodecLoop;
procedure mp_CodecClose( var Stream : zglPSoundStream );

var
  Formats : array[ 0..MAX_FORMATS - 1 ] of zglTSoundStream;
  SUPPORT_FORMATS : array[ 0..MAX_FORMATS - 1 ] of String = ( 'MOD', 'IT',  'S3M', 'XM',  'IT',  '669', 'AMF', 'AMS', 'DBM', 'DMF', 'DSM', 'FAR',
                                                              'MDL', 'MED', 'MTM', 'OKT', 'PTM', 'STM', 'ULT', 'UMX', 'MT2', 'PSM' );

  mpLoad    : Boolean;
  mpInit    : Boolean;
  mpLibrary : {$IFDEF WIN32} LongWord {$ELSE} Pointer {$ENDIF};
  mpFile    : PModPlugFile;

  ModPlug_Load        : function(data: pointer; size: cint): PModPlugFile; cdecl;
  ModPlug_Unload      : procedure(_file: PModPlugFile); cdecl;
  ModPlug_Read        : function(_file: PModPlugFile; buffer: pointer; size: cint): cint; cdecl;
  ModPlug_GetName     : function(_file: PModPlugFile): pcchar; cdecl;
  ModPlug_GetLength   : function(_file: PModPlugFile): cint; cdecl;
  ModPlug_Seek        : procedure(_file: PModPlugFile; millisecond: cint); cdecl;
  ModPlug_GetSettings : procedure(settings: PModPlug_Settings); cdecl;
  ModPlug_SetSettings : procedure(const settings: PModPlug_Settings); cdecl;

implementation

procedure mp_Init;
begin
  mpLibrary := dlopen( libmodplug {$IFNDEF WIN32}, $001 {$ENDIF} );
  {$IFDEF LINUX}
  if mpLibrary = LIB_ERROR Then
    mpLibrary := dlopen( PChar( libmodplug + '.0' ), $001 );
  {$ENDIF}

  if mpLibrary <> LIB_ERROR Then
    begin
      ModPlug_Load        := dlsym( mpLibrary, 'ModPlug_Load' );
      ModPlug_Unload      := dlsym( mpLibrary, 'ModPlug_Unload' );
      ModPlug_Read        := dlsym( mpLibrary, 'ModPlug_Read' );
      ModPlug_GetName     := dlsym( mpLibrary, 'ModPlug_GetName' );
      ModPlug_GetLength   := dlsym( mpLibrary, 'ModPlug_GetLength' );
      ModPlug_Seek        := dlsym( mpLibrary, 'ModPlug_Seek' );
      ModPlug_GetSettings := dlsym( mpLibrary, 'ModPlug_GetSettings' );
      ModPlug_SetSettings := dlsym( mpLibrary, 'ModPlug_SetSettings' );

      log_Add( 'ModPlug: Successful initialized'  );
      mpInit := TRUE;
    end else
      begin
        log_Add( 'ModPlug: Error while loading ' + libmodplug  );
        mpInit := FALSE;
      end;

  mpLoad := TRUE;
end;

function mp_CodecOpen;
  var
    mem : zglTMemory;
begin
  if not mpLoad Then mp_Init;
  if not mpInit Then exit;

  mem_LoadFromFile( mem, FileName );
  mpFile := ModPlug_Load( mem.Memory, mem.Size );
  mem_Free( mem );

  if Assigned( mpFile ) Then
    begin
      Result := TRUE;

      Stream.Rate       := 44100;
      Stream.Channels   := 2;
      Stream.BufferSize := 20000 - ( 20000 mod ( 2 * Stream.Channels ) );
      zgl_GetMem( Pointer( Stream.Buffer ), Stream.BufferSize );
    end else
      Result := FALSE;
end;

function mp_CodecRead;
begin
  if not mpInit Then exit;

  Result := ModPlug_Read( mpFile, Buffer, Count );
  _End := Result = 0;
end;

procedure mp_CodecLoop;
begin
  if not mpInit Then exit;

  ModPlug_Seek( mpFile, 0 );
end;

procedure mp_CodecClose;
begin
  if not mpInit Then exit;

  ModPlug_Unload( mpFile );
end;

var
  i : Integer;
initialization
  for i := 0 to MAX_FORMATS - 1 do
    begin
      Formats[ i ].Extension  := SUPPORT_FORMATS[ i ];
      Formats[ i ].CodecOpen  := mp_CodecOpen;
      Formats[ i ].CodecRead  := mp_CodecRead;
      Formats[ i ].CodecLoop  := mp_CodecLoop;
      Formats[ i ].CodecClose := mp_CodecClose;
      zgl_Reg( SND_FORMAT_EXTENSION, PChar( SUPPORT_FORMATS[ i ] ) );
      zgl_Reg( SND_FORMAT_FILE_LOADER, nil );
      zgl_Reg( SND_FORMAT_MEM_LOADER,  nil );
      zgl_Reg( SND_FORMAT_STREAM, @Formats[ i ] );
    end;

finalization
  if mpInit Then
    dlclose( mpLibrary );

end.

