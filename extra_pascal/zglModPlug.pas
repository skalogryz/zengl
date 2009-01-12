{-------------------------------}
{-----------= ZenGL =-----------}
{-------- ModPlug Player -------}
{-------------------------------}
{ build: 3                      }
{ date:  07.08.08               }
{-------------------------------}
{ by:   Andru ( Kemka Andrey )  }
{ mail: dr.andru@gmail.com      }
{ ICQ:  496-929-849             }
{ site: http://andru.2x4.ru     }
{-------------------------------}
{                      (C) 2008 }
{-------------------------------}
{$IFDEF LINUX}
unit zglModPlug;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$MINENUMSIZE 4}
{$ENDIF}

interface
uses
  zglHeader;

const
{$IFDEF LINUX}
  libmodplug  = 'libmodplug.so';
  libmodplug0 = 'libmodplug.so.0';
{$ENDIF}
//{$IFDEF WIN32}
//  libmodplug = 'modplug.dll';
//{$ENDIF}

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

procedure modplug_Init;
procedure modplug_PlayFile( const FileName : PChar; const Looped : Boolean = FALSE );
procedure modplug_LoopFile;
procedure modplug_CloseFile;

function mp_ReadBuffer( const Buffer : Pointer; const Count : DWORD ) : DWORD;
function mp_Read(datasource: pointer; ptr: pointer; size: cuint): cuint; cdecl;
function mp_Seek(datasource: pointer; offset: clong; whence: cint): clong; cdecl;
function mp_Close( datasource : pointer ) : cint; cdecl;
function mp_GetPos( datasource : pointer ) : clong; cdecl;

var
  modplugInit : Boolean;
  mpFile      : zglTSoundFile;
  mpf         : PModPlugFile;
  fn          : PChar;

  lmodplug : Pointer;//{$IFDEF LINUX} Pointer {$ENDIF} {$IFDEF WIN32} HMODULE {$ENDIF};

  ModPlug_Load        : function(data: pointer; size: cint): PModPlugFile; cdecl;
  ModPlug_Unload      : procedure(_file: PModPlugFile); cdecl;
  ModPlug_Read        : function(_file: PModPlugFile; buffer: pointer; size: cint): cint; cdecl;
  ModPlug_GetName     : function(_file: PModPlugFile): pcchar; cdecl;
  ModPlug_GetLength   : function(_file: PModPlugFile): cint; cdecl;
  ModPlug_Seek        : procedure(_file: PModPlugFile; millisecond: cint); cdecl;
  ModPlug_GetSettings : procedure(settings: PModPlug_Settings); cdecl;
  ModPlug_SetSettings : procedure(const settings: PModPlug_Settings); cdecl;

implementation

function mp_ReadBuffer;
  var
    Res: cint;
begin
  Res := ModPlug_Read( mpf, Buffer, Count );
  if Res < 0 Then
    Result := 0
  else
    Result := Res;
end;

function mp_Read;
begin
  Result := file_Read( zglTFile( datasource^ ), ptr^, size );
end;

function mp_Seek;
begin
  case whence of
    0: Result := file_Seek( zglTFile( datasource^ ), offset, FSM_SET );
    1: Result := file_Seek( zglTFile( datasource^ ), offset, FSM_CUR );
    2: Result := file_Seek( zglTFile( datasource^ ), offset, FSM_END );
    else Result := 0;
  end;
end;

function mp_Close;
begin
  file_Close( zglTFile( datasource^ ) );
  Result := 0;
end;

function mp_GetPos;
begin
  Result := file_GetPos( zglTFile( datasource^ ) );
end;

procedure modplug_Init;
begin
  lmodplug := dlopen( libmodplug, $001 ); // {$IFDEF LINUX}, $001 {$ENDIF} );
  if lmodplug = nil Then
    lmodplug := dlopen( libmodplug0, $001 );

  if lmodplug <> nil then //{$IFDEF LINUX} nil {$ENDIF} {$IFDEF WIN32} 0 {$ENDIF} Then
    begin
      ModPlug_Load        := dlsym( lmodplug, 'ModPlug_Load' );
      ModPlug_Unload      := dlsym( lmodplug, 'ModPlug_Unload' );
      ModPlug_Read        := dlsym( lmodplug, 'ModPlug_Read' );
      ModPlug_GetName     := dlsym( lmodplug, 'ModPlug_GetName' );
      ModPlug_GetLength   := dlsym( lmodplug, 'ModPlug_GetLength' );
      ModPlug_Seek        := dlsym( lmodplug, 'ModPlug_Seek' );
      ModPlug_GetSettings := dlsym( lmodplug, 'ModPlug_GetSettings' );
      ModPlug_SetSettings := dlsym( lmodplug, 'ModPlug_SetSettings' );

      log_Add( 'ModPlug: Successful initialized'  );
      modplugInit := TRUE;
    end else
      begin
        log_Add( 'ModPlug: Error while loading ' + libmodplug  );
        modplugInit := FALSE;
      end;
end;

procedure modplug_PlayFile;
  var
    mem : zglTMemory;
begin
  if not modplugInit Then exit;

  if not file_Exists( FileName ) Then
    begin
      log_Add( 'ModPlug: Cannot read ' + FileName );
      exit;
    end;

  fn := FileName;
  mem_LoadFromFile( mem, fn );
  mpf := ModPlug_Load( mem.Memory, mem.Size );
  mem_Free( mem );

  mpFile.CodecRead  := @mp_ReadBuffer;
  mpFile.CodecLoop  := @modplug_LoopFile;
  mpFile.Rate       := 44100;
  mpFile.Channels   := 2;
  mpFile.BufferSize := 20000 - ( 20000 mod ( 2 * mpFile.Channels ) );
  mpFile.Buffer     := Allocmem( mpFile.BufferSize );
  mpFile.Loop       := Looped;
  snd_PlayFile( @mpFile );
end;

procedure modplug_LoopFile;
begin
  if not modplugInit Then exit;

  ModPlug_Seek( mpf, 0 );
end;

procedure modplug_CloseFile;
begin
  if not modplugInit Then exit;
  if not Assigned( mpf ) Then exit;

  if Assigned( mpFile.Buffer ) Then
    FreeMem( mpFile.Buffer );
  ModPlug_Unload( mpf );
end;

end.
{$ENDIF}
