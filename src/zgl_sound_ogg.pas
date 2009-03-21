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
unit zgl_sound_ogg;

{$I zgl_config.cfg}

{$IFDEF FPC}
  {$IFDEF USE_OGG_STATIC}
    {$IFDEF LINUX}
      {$LINKLIB libogg.a}
      {$LINKLIB libvorbis.a}
      {$LINKLIB libvorbisfile.a}
    {$ENDIF}
    {$IFDEF WIN32}
      {$L libogg_win32/bitwise}
      {$L libogg_win32/framing}
      {$L libogg_win32/analysis}
      {$L libogg_win32/bitrate}
      {$L libogg_win32/block}
      {$L libogg_win32/codebook}
      {$L libogg_win32/envelope}
      {$L libogg_win32/floor0}
      {$L libogg_win32/floor1}
      {$L libogg_win32/info}
      {$L libogg_win32/lookup}
      {$L libogg_win32/lpc}
      {$L libogg_win32/lsp}
      {$L libogg_win32/mapping0}
      {$L libogg_win32/mdct}
      {$L libogg_win32/psy}
      {$L libogg_win32/registry}
      {$L libogg_win32/res0}
      {$L libogg_win32/sharedbook}
      {$L libogg_win32/smallft}
      {$L libogg_win32/synthesis}
      {$L libogg_win32/vorbisfile}
      {$L libogg_win32/window}
      {$LINKLIB libogg_win32/libgcc.a}
      {$LINKLIB libogg_win32/libmsvcrt.a}
    {$ENDIF}
    {$IFDEF DARWIN}
      {$L libogg_macos/bitwise}
      {$L libogg_macos/framing}
      {$L libogg_macos/analysis}
      {$L libogg_macos/bitrate}
      {$L libogg_macos/block}
      {$L libogg_macos/codebook}
      {$L libogg_macos/envelope}
      {$L libogg_macos/floor0}
      {$L libogg_macos/floor1}
      {$L libogg_macos/info}
      {$L libogg_macos/lookup}
      {$L libogg_macos/lpc}
      {$L libogg_macos/lsp}
      {$L libogg_macos/mapping0}
      {$L libogg_macos/mdct}
      {$L libogg_macos/psy}
      {$L libogg_macos/registry}
      {$L libogg_macos/res0}
      {$L libogg_macos/sharedbook}
      {$L libogg_macos/smallft}
      {$L libogg_macos/synthesis}
      {$L libogg_macos/vorbisfile}
      {$L libogg_macos/window}
      {$LINKLIB libgcc.a}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

interface
uses
  zgl_types,
  zgl_sound;

const
{$IFDEF LINUX}
  libogg        = 'libogg.so';
  libvorbis     = 'libvorbis.so';
  libvorbisfile = 'libvorbisfile.so';
{$ENDIF}
{$IFDEF WIN32}
  libogg        = 'ogg.dll';
  libvorbis     = 'vorbis.dll';
  libvorbisfile = 'vorbisfile.dll';
{$ENDIF}
{$IFDEF DARWIN}
  libogg        = 'libogg.0.5.3.dylib';
  libvorbis     = 'libvorbis.0.3.1.dylib';
  libvorbisfile = 'libvorbisfile.3.1.1.dylib';
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
function  ogg_CodecOpen( const FileName : String; var Stream : zglPSoundStream ) : Boolean;
function  ogg_CodecRead( const Buffer : Pointer; const Count : DWORD; var _End : Boolean ) : DWORD;
procedure ogg_CodecLoop;
procedure ogg_CodecClose( var Stream : zglPSoundStream );

function ogg_Read( ptr : pointer; size, nmemb : csize_t; datasource : pointer) : csize_t; cdecl;
function ogg_Seek( datasource : pointer; offset : cint64; whence : cint) : cint; cdecl;
function ogg_Close( datasource : pointer ) : cint; cdecl;
function ogg_GetPos( datasource : pointer ) : clong; cdecl;

{$IFDEF USE_OGG_STATIC}
function ov_clear(var vf: OggVorbis_File): cint; cdecl; external;
function ov_open_callbacks(datasource: pointer; var vf: OggVorbis_File; initial: pointer; ibytes: clong; callbacks: ov_callbacks): cint; cdecl; external;
function ov_info(var vf: OggVorbis_File; link: cint): pvorbis_info; cdecl; external;
function ov_read(var vf: OggVorbis_File; buffer: pointer; length: cint; bigendianp: cbool; word: cint; sgned: cbool; bitstream: pcint): clong; cdecl; external;
function ov_time_seek(var vf: OggVorbis_File; pos: cdouble): cint; cdecl; external;
{$ENDIF}

var
  oggLoad   : Boolean;
  oggInit   : Boolean;
  oggStream : zglTSoundStream;

  vi : pvorbis_info;
  vf : OggVorbis_File;
  vc : ov_callbacks;

  ogg_Library        : {$IFDEF LINUX_OR_DARWIN} Pointer {$ENDIF} {$IFDEF WIN32} HMODULE {$ENDIF};
  vorbis_Library     : {$IFDEF LINUX_OR_DARWIN} Pointer {$ENDIF} {$IFDEF WIN32} HMODULE {$ENDIF};
  vorbisfile_Library : {$IFDEF LINUX_OR_DARWIN} Pointer {$ENDIF} {$IFDEF WIN32} HMODULE {$ENDIF};

{$IFNDEF USE_OGG_STATIC}
  ov_clear          : function(var vf: OggVorbis_File): cint; cdecl;
  ov_open_callbacks : function(datasource: pointer; var vf: OggVorbis_File; initial: pointer; ibytes: clong; callbacks: ov_callbacks): cint; cdecl;
  ov_info           : function(var vf: OggVorbis_File; link: cint): pvorbis_info; cdecl;
  ov_read           : function(var vf: OggVorbis_File; buffer: pointer; length: cint; bigendianp: cbool; word: cint; sgned: cbool; bitstream: pcint): clong; cdecl;
  ov_time_seek      : function(var vf: OggVorbis_File; pos: cdouble): cint; cdecl;
{$ENDIF}

implementation
uses
  zgl_const,
  zgl_application,
  zgl_main,
  zgl_file,
  zgl_log,
  zgl_utils;

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
{$IFDEF USE_OGG_STATIC}
  oggInit := TRUE;
{$ELSE}
  ogg_Library        := dlopen( libogg {$IFDEF LINUX_OR_DARWIN}, $001 {$ENDIF} );
  vorbis_Library     := dlopen( libvorbis {$IFDEF LINUX_OR_DARWIN}, $001 {$ENDIF} );
  vorbisfile_Library := dlopen( libvorbisfile {$IFDEF LINUX_OR_DARWIN}, $001 {$ENDIF} );
  {$IFDEF DARWIN}
  if ( ogg_Library        = LIB_ERROR ) and
     ( vorbis_Library     = LIB_ERROR ) and
     ( vorbisfile_Library = LIB_ERROR ) Then
    begin
      ogg_Library        := dlopen( PChar( app_WorkDir + 'Contents/MacOS/' + libogg ), $001 );
      vorbis_Library     := dlopen( PChar( app_WorkDir + 'Contents/MacOS/' + libvorbis ), $001 );
      vorbisfile_Library := dlopen( PChar( app_WorkDir + 'Contents/MacOS/' + libvorbisfile ), $001 );
    end;
  {$ENDIF}

  if ( ogg_Library        <> LIB_ERROR ) and
     ( vorbis_Library     <> LIB_ERROR ) and
     ( vorbisfile_Library <> LIB_ERROR ) Then
    begin
      ov_clear          := dlsym( vorbisfile_Library, 'ov_clear' );
      ov_open_callbacks := dlsym( vorbisfile_Library, 'ov_open_callbacks' );
      ov_info           := dlsym( vorbisfile_Library, 'ov_info' );
      ov_read           := dlsym( vorbisfile_Library, 'ov_read' );
      ov_time_seek      := dlsym( vorbisfile_Library, 'ov_time_seek' );

      log_Add( 'Ogg: Successful initialized'  );
      oggInit := TRUE;
    end else
      begin
        log_Add( 'Ogg: Error while loading libraries: ' + libogg + ', ' + libvorbis + ', ' + libvorbisfile  );
        oggInit := FALSE;
      end;
{$ENDIF}

  vc.read  := @ogg_Read;
  vc.seek  := @ogg_Seek;
  vc.close := @ogg_Close;
  vc.tell  := @ogg_GetPos;

  oggLoad := TRUE;
end;

function ogg_CodecOpen;
begin
  if not oggLoad Then ogg_Init;
  if not oggInit Then exit;

  file_Open( Stream._File, FileName, FOM_OPENR );

  if ov_open_callbacks( @Stream._File, vf, nil, 0, vc ) >= 0 Then
    begin
      vi                := ov_info( vf, -1 );
      Stream.Rate       := vi.rate;
      Stream.Channels   := vi.channels;
      Stream.BufferSize := 64 * 1024;//{$IFDEF USE_OPENAL} 20000 - ( 20000 mod ( 2 * Stream.Channels ) ) {$ELSE} 64 * 1024  {$ENDIF};
      zgl_GetMem( Pointer( Stream.Buffer ), Stream.BufferSize );
      Result := TRUE;
    end else
      Result := FALSE;
  ov_time_seek( vf, 0 );
end;

function ogg_CodecRead;
  var
    BytesRead : Integer;
begin
  if not oggInit Then exit;

  BytesRead := 0;
  repeat
    Result := ov_read( vf, Pointer( Ptr( Buffer ) + BytesRead ), Count - BytesRead, FALSE, 2, TRUE, nil );

    if Result = -3  Then break;
    BytesRead := BytesRead + Result;
  until ( Result = 0 ) or ( BytesRead = Count );

  _End   := Result = 0;
  Result := BytesRead;
end;

procedure ogg_CodecLoop;
begin
  if not oggInit Then exit;

  ov_time_seek( vf, 0 );
end;

procedure ogg_CodecClose;
begin
  if not oggInit Then exit;
  if not Assigned( vi ) Then exit;
  vi:= nil;
  ov_clear( vf );
  {$IFDEF WIN32}
  //file_Close( Stream._File );
  {$ENDIF}
end;

initialization
  oggStream.Extension  := 'OGG';
  oggStream.CodecOpen  := ogg_CodecOpen;
  oggStream.CodecRead  := ogg_CodecRead;
  oggStream.CodecLoop  := ogg_CodecLoop;
  oggStream.CodecClose := ogg_CodecClose;
  zgl_Reg( SND_FORMAT_EXTENSION, PChar( 'OGG' ) );
  zgl_Reg( SND_FORMAT_FILE_LOADER, nil );
  zgl_Reg( SND_FORMAT_MEM_LOADER,  nil );
  zgl_Reg( SND_FORMAT_STREAM, @oggStream );

finalization
{$IFNDEF USE_OGG_STATIC}
  if oggInit Then
    begin
      dlclose( ogg_Library );
      dlclose( vorbis_Library );
      dlclose( vorbisfile_Library );
    end;
{$ENDIF}

end.
