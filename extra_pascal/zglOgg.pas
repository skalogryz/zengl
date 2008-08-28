{-------------------------------}
{-----------= ZenGL =-----------}
{---------- Ogg Player ---------}
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
unit zglOgg;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  zglHeader;

const
{$IFDEF LINUX}
  libogg        = 'libogg.so.0';
  libvorbis     = 'libvorbis.so.0';
  libvorbisfile = 'libvorbisfile.so.3';
{$ENDIF}
{$IFDEF WIN32}
  libogg        = 'ogg.dll';
  libvorbis     = 'vorbis.dll';
  libvorbisfile = 'vorbisfile.dll';
{$ENDIF}

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
  ppcfloat     = ^pcfloat;
  ogg_int64_t  = cint64;
  pogg_int64_t = ^ogg_int64_t;

  poggpack_buffer = ^oggpack_buffer;
  oggpack_buffer = record
    endbyte         : clong;
    endbit          : cint;
    buffer          : pcuchar;
    ptr             : pcuchar;
    storage         : clong;
  end;

  pogg_stream_state = ^ogg_stream_state;
  ogg_stream_state = record
    body_data       : pcuchar;
    body_storage    : clong;
    body_fill       : clong;
    body_returned   : clong;
    lacing_vals     : pcint;
    granule_vals    : pogg_int64_t;
    lacing_storage  : clong;
    lacing_fill     : clong;
    lacing_packet   : clong;
    lacing_returned : clong;
    header          : array[0..281] of cuchar;
    header_fill     : cint;
    e_o_s           : cint;
    b_o_s           : cint;
    serialno        : clong;
    pageno          : clong;
    packetno        : ogg_int64_t;
    granulepos      : ogg_int64_t;
  end;

  ogg_sync_state = record
    data            : pcuchar;
    storage         : cint;
    fill            : cint;
    returned        : cint;
    unsynced        : cint;
    headerbytes     : cint;
    bodybytes       : cint;
  end;

  pvorbis_info = ^vorbis_info;
  vorbis_info = record
    version         : cint;
    channels        : cint;
    rate            : clong;
    bitrate_upper   : clong;
    bitrate_nominal : clong;
    bitrate_lower   : clong;
    bitrate_window  : clong;
    codec_setup     : pointer;
  end;

  pvorbis_dsp_state = ^vorbis_dsp_state;
  vorbis_dsp_state = record
    analysisp       : cint;
    vi              : pvorbis_info;
    pcm             : ppcfloat;
    pcmret          : ppcfloat;
    pcm_storage     : cint;
    pcm_current     : cint;
    pcm_returned    : cint;
    preextrapolate  : cint;
    eofflag         : cint;
    lW              : clong;
    W               : clong;
    nW              : clong;
    centerW         : clong;
    granulepos      : ogg_int64_t;
    sequence        : ogg_int64_t;
    glue_bits       : ogg_int64_t;
    time_bits       : ogg_int64_t;
    floor_bits      : ogg_int64_t;
    res_bits        : ogg_int64_t;
    backend_state   : pointer;
  end;

  palloc_chain = ^alloc_chain;
  alloc_chain = record
    ptr             : pointer;
    next            : palloc_chain;
  end;

  pvorbis_block = ^vorbis_block;
  vorbis_block = record
    pcm             : ppcfloat;
    opb             : oggpack_buffer;
    lW              : clong;
    W               : clong;
    nW              : clong;
    pcmend          : cint;
    mode            : cint;
    eofflag         : cint;
    granulepos      : ogg_int64_t;
    sequence        : ogg_int64_t;
    vd              : pvorbis_dsp_state;
    localstore      : pointer;
    localtop        : clong;
    localalloc      : clong;
    totaluse        : clong;
    reap            : palloc_chain;
    glue_bits       : clong;
    time_bits       : clong;
    floor_bits      : clong;
    res_bits        : clong;
    internal        : pointer;
  end;

  pvorbis_comment = ^vorbis_comment;
  vorbis_comment = record
    user_comments   : ^pcchar;
    comment_lengths : pcint;
    comments        : cint;
    vendor          : pcchar;
  end;

  read_func  = function(ptr: pointer; size, nmemb: csize_t; datasource: pointer): csize_t; cdecl;
  seek_func  = function(datasource: pointer; offset: ogg_int64_t; whence: cint): cint; cdecl;
  close_func = function(datasource: pointer): cint; cdecl;
  tell_func  = function(datasource: pointer): clong; cdecl;

  pov_callbacks = ^ov_callbacks;
  ov_callbacks = record
    read            : read_func;
    seek            : seek_func;
    close           : close_func;
    tell            : tell_func;
  end;

  POggVorbis_File = ^OggVorbis_File;
  OggVorbis_File = record
    datasource      : pointer;
    seekable        : cint;
    offset          : ogg_int64_t;
    end_            : ogg_int64_t;
    oy              : ogg_sync_state;
    links           : cint;
    offsets         : pogg_int64_t;
    dataoffsets     : pogg_int64_t;
    serialnos       : pclong;
    pcmlengths      : pogg_int64_t;
    vi              : pvorbis_info;
    vc              : pvorbis_comment;
    pcm_offset      : ogg_int64_t;
    ready_state     : cint;
    current_serialno: clong;
    current_link    : cint;
    bittrack        : cdouble;
    samptrack       : cdouble;
    os              : ogg_stream_state;
    vd              : vorbis_dsp_state;
    vb              : vorbis_block;
    callbacks       : ov_callbacks;
  end;

procedure ogg_Init;
procedure ogg_PlayFile( FileName : PChar; Looped : Boolean = FALSE ); {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
procedure ogg_LoopFile; {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
procedure ogg_CloseFile; {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}

function ogg_ReadBuffer( Buffer : Pointer; Count : DWORD ) : DWORD; {$IFDEF LINUX} cdecl; {$ELSE} stdcall; {$ENDIF}
function ogg_Read( ptr : pointer; size, nmemb : csize_t; datasource : pointer) : csize_t; cdecl;
function ogg_Seek( datasource : pointer; offset : cint64; whence : cint) : cint; cdecl;
function ogg_Close( datasource : pointer ) : cint; cdecl;
function ogg_GetPos( datasource : pointer ) : clong; cdecl;

var
  oggInit : Boolean;
  oggFile : zglTSoundFile;

  fn : PChar;
  vi : pvorbis_info;
  vf : OggVorbis_File;
  vc : ov_callbacks;

  logg        : {$IFDEF LINUX} Pointer {$ENDIF} {$IFDEF WIN32} HMODULE {$ENDIF};
  lvorbis     : {$IFDEF LINUX} Pointer {$ENDIF} {$IFDEF WIN32} HMODULE {$ENDIF};
  lvorbisfile : {$IFDEF LINUX} Pointer {$ENDIF} {$IFDEF WIN32} HMODULE {$ENDIF};

  ov_clear          : function(var vf: OggVorbis_File): cint; cdecl;
  ov_open_callbacks : function(datasource: pointer; var vf: OggVorbis_File; initial: pointer; ibytes: clong; callbacks: ov_callbacks): cint; cdecl;
  ov_info           : function(var vf: OggVorbis_File; link: cint): pvorbis_info; cdecl;
  ov_read           : function(var vf: OggVorbis_File; buffer: pointer; length: cint; bigendianp: cbool; word: cint; sgned: cbool; bitstream: pcint): clong; cdecl;
  ov_time_seek      : function(var vf: OggVorbis_File; pos: cdouble): cint; cdecl;

implementation

function ogg_ReadBuffer;
  var
    BytesRead : Integer;
begin
  BytesRead := 0;
  repeat
    {$IFDEF FPC}
    Result := ov_read( vf, Buffer + BytesRead, Count - BytesRead, FALSE, 2, TRUE, nil );
    {$ELSE}
    Result := ov_read( vf, Pointer( Ptr( Buffer ) + BytesRead ), Count - BytesRead, FALSE, 2, TRUE, nil );
    {$ENDIF}

    if Result = -3  Then break;
    BytesRead := BytesRead + Result;
  until ( Result = 0 ) or ( BytesRead = Count );

  Result := BytesRead;
end;

function ogg_Read;
begin
  Result := file_Read( zglTFile( datasource^ ), ptr^, size * nmemb );
end;

function ogg_Seek;
begin
  case whence of
    0: file_Seek( zglTFile( datasource^ ), offset, FSM_SET );
    1: file_Seek( zglTFile( datasource^ ), offset, FSM_CUR );
    2: file_Seek( zglTFile( datasource^ ), offset, FSM_END );
  end;
  Result := 0;
end;

function ogg_Close;
begin
  file_Close( zglTFile( datasource^ ) );
  Result := 0;
end;

function ogg_GetPos;
begin
  Result := file_GetPos( zglTFile( datasource^ ) );
end;

procedure ogg_Init;
begin
  logg        := dlopen( libogg {$IFDEF LINUX}, $001 {$ENDIF} );
  lvorbis     := dlopen( libvorbis {$IFDEF LINUX}, $001 {$ENDIF} );
  lvorbisfile := dlopen( libvorbisfile {$IFDEF LINUX}, $001 {$ENDIF} );

  if ( logg <> {$IFDEF LINUX} nil {$ENDIF} {$IFDEF WIN32} 0 {$ENDIF} ) and
     ( lvorbis <> {$IFDEF LINUX} nil {$ENDIF} {$IFDEF WIN32} 0 {$ENDIF} ) and
     ( lvorbisfile <> {$IFDEF LINUX} nil {$ENDIF} {$IFDEF WIN32} 0 {$ENDIF} ) Then
    begin
      ov_clear          := dlsym( lvorbisfile, 'ov_clear' );
      ov_open_callbacks := dlsym( lvorbisfile, 'ov_open_callbacks' );
      ov_info           := dlsym( lvorbisfile, 'ov_info' );
      ov_read           := dlsym( lvorbisfile, 'ov_read' );
      ov_time_seek      := dlsym( lvorbisfile, 'ov_time_seek' );

      log_Add( 'Ogg: Successful initialized'  );
      oggInit := TRUE;
    end else
      begin
        log_Add( 'Ogg: Error while loading libraries: ' + libogg + ', ' + libvorbis + ', ' + libvorbisfile  );
        oggInit := FALSE;
      end;
end;

procedure ogg_PlayFile;
begin
  if not oggInit Then exit;

  if not file_Exists( FileName ) Then
    begin
      log_Add( 'Ogg: Cannot read ' + FileName );
      exit;
    end;
  if FileName = fn Then ogg_CloseFile;

  vc.read  := @ogg_Read;
  vc.seek  := @ogg_Seek;
  vc.close := @ogg_Close;
  vc.tell  := @ogg_GetPos;

  fn := FileName;
  file_Open( oggFile._File, FileName, FOM_OPENR );
  
  if ov_open_callbacks( @oggFile._File, vf, nil, 0, vc ) >= 0 Then
    begin
      vi                 := ov_info( vf, -1 );
      oggFile.CodecRead  := @ogg_ReadBuffer;
      oggFile.CodecLoop  := @ogg_LoopFile;
      oggFile.Rate       := vi^.rate;
      oggFile.Channels   := vi^.channels;
      oggFile.BufferSize := 20000 - ( 20000 mod ( 2 * vi^.channels ) );
      GetMem( oggFile.Buffer, oggFile.BufferSize );
      oggFile.Loop       := Looped;
      snd_PlayFile( @oggFile );
    end;
end;

procedure ogg_LoopFile;
begin
  if not oggInit Then exit;

  ov_time_seek( vf, 0 );
end;

procedure ogg_CloseFile;
begin
  if not oggInit Then exit;
  if not Assigned( vi ) Then exit;
  vi := nil;

  if Assigned( oggFile.Buffer ) Then
    FreeMem( oggFile.Buffer );
  ov_clear( vf );
end;

end.
