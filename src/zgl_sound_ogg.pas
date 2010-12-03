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
unit zgl_sound_ogg;

{$I zgl_config.cfg}

{$IFDEF FPC}
  {$IFDEF USE_OGG_STATIC}
    {$IFDEF LINUX}
      {$IFDEF USE_OGG_FROM_SYSTEM}
        {$LINKLIB libogg.a}
        {$LINKLIB libvorbis.a}
        {$LINKLIB libvorbisfile.a}
      {$ELSE}
        {$IFDEF cpui386}
          {$LINKLIB libogg_linux_x86/libogg.a}
          {$LINKLIB libogg_linux_x86/libvorbis.a}
          {$LINKLIB libogg_linux_x86/libvorbisfile.a}
        {$ELSE}
          {$LINKLIB libogg_linux_x86_64/libogg.a}
          {$LINKLIB libogg_linux_x86_64/libvorbis.a}
          {$LINKLIB libogg_linux_x86_64/libvorbisfile.a}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
    {$IFDEF WINDOWS}
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
      {$IFDEF cpui386}
        {$L libogg_macos_i386/libogg-i386-master}
        {$L libogg_macos_i386/libvorbis-i386-master}
        {$L libogg_macos_i386/libvorbisfile-i386-master}
      {$ELSE}
        {$L libogg_macos_ppc/libogg-ppc-master}
        {$L libogg_macos_ppc/libvorbis-ppc-master}
        {$L libogg_macos_ppc/libvorbisfile-ppc-master}
      {$ENDIF}
      {$LINKLIB libgcc.a}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

interface
uses
  zgl_main,
  zgl_types,
  zgl_application,
  zgl_sound,
  {$IFDEF USE_OPENAL}
  zgl_sound_openal,
  {$ELSE}
  zgl_sound_dsound,
  {$ENDIF}
  zgl_file,
  zgl_memory,
  zgl_log,
  zgl_utils;

const
  OGG_EXTENSION : array[ 0..3 ] of Char = ( 'O', 'G', 'G', #0 );
{$IFDEF LINUX}
  libogg         = 'libogg.so';
  libvorbis      = 'libvorbis.so';
  libvorbisfile  = 'libvorbisfile.so';
  libogg0        = 'libogg.so.0';
  libvorbis0     = 'libvorbis.so.0';
  libvorbisfile3 = 'libvorbisfile.so.3';
{$ENDIF}
{$IFDEF WINDOWS}
  libogg        = 'libogg.dll';
  libvorbis     = 'libvorbis.dll';
  libvorbisfile = 'libvorbisfile.dll';
{$ENDIF}
{$IFDEF DARWIN}
  libogg        = 'libogg.0.5.3.dylib';
  libvorbis     = 'libvorbis.0.3.1.dylib';
  libvorbisfile = 'libvorbisfile.3.1.1.dylib';
{$ENDIF}
{$IFDEF ENDIAN_BIG}
  BIG_ENDIAN = TRUE;
{$ELSE}
  BIG_ENDIAN = FALSE;
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

  zglPOggStream = ^zglTOggStream;
  zglTOggStream = record
    vi : pvorbis_info;
    vf : OggVorbis_File;
    vc : ov_callbacks;
  end;

procedure ogg_Init;
function  ogg_DecoderOpen( var Stream : zglTSoundStream; const FileName : String ) : Boolean;
function  ogg_DecoderRead( var Stream : zglTSoundStream; Buffer : Pointer; Bytes : LongWord; var _End : Boolean ) : LongWord;
procedure ogg_DecoderLoop( var Stream : zglTSoundStream );
procedure ogg_DecoderClose( var Stream : zglTSoundStream );

procedure ogg_Load( var Data : Pointer; var Size, Format, Frequency : LongWord );
procedure ogg_LoadFromFile( const FileName : String; var Data : Pointer; var Size, Format, Frequency : LongWord );
procedure ogg_LoadFromMemory( const Memory : zglTMemory; var Data : Pointer; var Size, Format, Frequency : LongWord );

var
  oggLoad    : Boolean;
  oggInit    : Boolean;
  oggMemory  : zglTMemory;
  oggDecoder : zglTSoundDecoder;
  {$IFNDEF USE_OPENAL}
  oggBufferDesc : zglTBufferDesc;
  {$ENDIF}

  ogg_Library        : {$IFDEF LINUX_OR_DARWIN} Pointer {$ENDIF} {$IFDEF WINDOWS} HMODULE {$ENDIF};
  vorbis_Library     : {$IFDEF LINUX_OR_DARWIN} Pointer {$ENDIF} {$IFDEF WINDOWS} HMODULE {$ENDIF};
  vorbisfile_Library : {$IFDEF LINUX_OR_DARWIN} Pointer {$ENDIF} {$IFDEF WINDOWS} HMODULE {$ENDIF};

implementation

{$IFDEF USE_OGG_STATIC}
  function ov_clear(var vf: OggVorbis_File): cint; cdecl; external;
  function ov_open_callbacks(datasource: pointer; var vf: OggVorbis_File; initial: pointer; ibytes: clong; callbacks: ov_callbacks): cint; cdecl; external;
  function ov_info(var vf: OggVorbis_File; link: cint): pvorbis_info; cdecl; external;
  function ov_read(var vf: OggVorbis_File; buffer: pointer; length: cint; bigendianp: cbool; word: cint; sgned: cbool; bitstream: pcint): clong; cdecl; external;
  function ov_pcm_seek(var vf: OggVorbis_File; pos: cint64): cint; cdecl; external;
  function ov_pcm_total(var vf: OggVorbis_File; i: cint): ogg_int64_t; cdecl; external;
{$ELSE}
  var
    ov_clear          : function(var vf: OggVorbis_File): cint; cdecl;
    ov_open_callbacks : function(datasource: pointer; var vf: OggVorbis_File; initial: pointer; ibytes: clong; callbacks: ov_callbacks): cint; cdecl;
    ov_info           : function(var vf: OggVorbis_File; link: cint): pvorbis_info; cdecl;
    ov_read           : function(var vf: OggVorbis_File; buffer: pointer; length: cint; bigendianp: cbool; word: cint; sgned: cbool; bitstream: pcint): clong; cdecl;
    ov_pcm_seek       : function(var vf: OggVorbis_File; pos: cint64): cint; cdecl;
    ov_pcm_total      : function(var vf: OggVorbis_File; i: cint): ogg_int64_t; cdecl;
{$ENDIF}

function ogg_Read( ptr : pointer; size, nmemb : csize_t; datasource : pointer) : csize_t; cdecl;
begin
  Result := file_Read( zglTFile( datasource^ ), ptr^, size * nmemb );
end;

function ogg_Seek( datasource : pointer; offset : cint64; whence : cint) : cint; cdecl;
begin
  case whence of
    0: file_Seek( zglTFile( datasource^ ), offset, FSM_SET );
    1: file_Seek( zglTFile( datasource^ ), offset, FSM_CUR );
    2: file_Seek( zglTFile( datasource^ ), offset, FSM_END );
  end;
  Result := 0;
end;

function ogg_Close( datasource : pointer ) : cint; cdecl;
begin
  file_Close( zglTFile( datasource^ ) );
  Result := 0;
end;

function ogg_GetPos( datasource : pointer ) : clong; cdecl;
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
  {$IFDEF LINUX}
  if ( ogg_Library = LIB_ERROR ) and ( vorbis_Library = LIB_ERROR ) and ( vorbisfile_Library = LIB_ERROR ) Then
    begin
      ogg_Library        := dlopen( libogg0, $001 );
      vorbis_Library     := dlopen( libvorbis0, $001 );
      vorbisfile_Library := dlopen( libvorbisfile3, $001 );
    end;
  {$ENDIF}
  {$IFDEF DARWIN}
  if ( ogg_Library = LIB_ERROR ) and ( vorbis_Library = LIB_ERROR ) and ( vorbisfile_Library = LIB_ERROR ) Then
    begin
      ogg_Library        := dlopen( PChar( app_WorkDir + 'Contents/MacOS/' + libogg ), $001 );
      vorbis_Library     := dlopen( PChar( app_WorkDir + 'Contents/MacOS/' + libvorbis ), $001 );
      vorbisfile_Library := dlopen( PChar( app_WorkDir + 'Contents/MacOS/' + libvorbisfile ), $001 );
    end;
  {$ENDIF}

  if ( ogg_Library <> LIB_ERROR ) and ( vorbis_Library <> LIB_ERROR ) and ( vorbisfile_Library <> LIB_ERROR ) Then
    begin
      ov_clear          := dlsym( vorbisfile_Library, 'ov_clear' );
      ov_open_callbacks := dlsym( vorbisfile_Library, 'ov_open_callbacks' );
      ov_info           := dlsym( vorbisfile_Library, 'ov_info' );
      ov_read           := dlsym( vorbisfile_Library, 'ov_read' );
      ov_pcm_seek       := dlsym( vorbisfile_Library, 'ov_pcm_seek' );
      ov_pcm_total      := dlsym( vorbisfile_Library, 'ov_pcm_total' );

      log_Add( 'Ogg: Initialized'  );
      oggInit := TRUE;
    end else
      begin
        log_Add( 'Ogg: Error while loading libraries: ' + libogg + ', ' + libvorbis + ', ' + libvorbisfile  );
        oggInit := FALSE;
      end;
{$ENDIF}

  oggLoad := TRUE;
end;

function ogg_DecoderOpen( var Stream : zglTSoundStream; const FileName : String ) : Boolean;
begin
  Result := FALSE;
  if not oggLoad Then ogg_Init;
  if not oggInit Then exit;

  file_Open( Stream._file, FileName, FOM_OPENR );
  zgl_GetMem( Stream._data, SizeOf( zglTOggStream ) );

  with zglTOggStream( Stream._Data^ ) do
    begin
      vc.read  := @ogg_Read;
      vc.seek  := @ogg_Seek;
      vc.close := @ogg_Close;
      vc.tell  := @ogg_GetPos;
      if ov_open_callbacks( @Stream._file, vf, nil, 0, vc ) >= 0 Then
        begin
          vi                := ov_info( vf, -1 );
          Stream.Frequency  := vi.rate;
          Stream.Channels   := vi.channels;
          Stream.Length     := ov_pcm_total( vf, -1 ) / vi.rate * 1000;
          Stream.BufferSize := 64 * 1024;
          zgl_GetMem( Pointer( Stream.Buffer ), Stream.BufferSize );
          Result := TRUE;
        end;
        ov_pcm_seek( vf, 0 );
    end;
end;

function ogg_DecoderRead( var Stream : zglTSoundStream; Buffer : Pointer; Bytes : LongWord; var _End : Boolean ) : LongWord;
  var
    bytesRead : Integer;
begin
  if not oggInit Then exit;

  bytesRead := 0;
  repeat
    Result := ov_read( zglTOggStream( Stream._data^ ).vf, Pointer( Ptr( Buffer ) + bytesRead ), Bytes - bytesRead, BIG_ENDIAN, 2, TRUE, nil );
    bytesRead := bytesRead + Result;
  until ( Result = 0 ) or ( bytesRead = Bytes );

  _End   := Result = 0;
  Result := bytesRead;
end;

procedure ogg_DecoderLoop( var Stream : zglTSoundStream );
begin
  if not oggInit Then exit;

  ov_pcm_seek( zglTOggStream( Stream._data^ ).vf, 0 );
end;

procedure ogg_DecoderClose( var Stream : zglTSoundStream );
begin
  if not oggInit Then exit;
  if not Assigned( zglTOggStream( Stream._data^ ).vi ) Then exit;
  zglTOggStream( Stream._data^ ).vi := nil;
  ov_clear( zglTOggStream( Stream._data^ ).vf );
end;

function decoderRead( var VorbisFile : OggVorbis_File; const Buffer : Pointer; const Bytes : LongWord; var _End : Boolean ) : LongWord;
  var
    bytesRead : Integer;
begin
  bytesRead := 0;
  repeat
    Result := ov_read( VorbisFile, Pointer( Ptr( Buffer ) + bytesRead ), Bytes - bytesRead, BIG_ENDIAN, 2, TRUE, nil );
    bytesRead := bytesRead + Result;
  until ( Result = 0 ) or ( bytesRead = Bytes );

  _End   := Result = 0;
  Result := bytesRead;
end;

procedure ogg_Load( var Data : Pointer; var Size, Format, Frequency : LongWord );
  var
    bytesRead : Integer;
    buffer    : Pointer;
    _end      : Boolean;

    _vi : pvorbis_info;
    _vf : OggVorbis_File;
    _vc : ov_callbacks;
begin
  if not oggLoad Then ogg_Init;
  if not oggInit Then exit;

  FillChar( _vc, SizeOf( _vc ), 0 );
  if ov_open_callbacks( nil, _vf, oggMemory.Memory, oggMemory.Size, _vc ) >= 0 Then
    begin
      _vi       := ov_info( _vf, -1 );
      Frequency := _vi.rate;
      {$IFDEF USE_OPENAL}
      case _vi.channels of
        1: format := AL_FORMAT_MONO16;
        2: format := AL_FORMAT_STEREO16;
      end;
      {$ELSE}
      with oggBufferDesc do
        begin
          FormatCode     := $0001;
          ChannelNumber  := _vi.channels;
          SampleRate     := _vi.rate;
          BitsPerSample  := 16;
          BytesPerSample := ( BitsPerSample div 8 ) * ChannelNumber;
          BytesPerSecond := SampleRate * BytesPerSample;
          cbSize         := SizeOf( zglTBufferDesc );
        end;
      Format := Ptr( @oggBufferDesc.Formatcode );
      {$ENDIF}

      Size := 0;
      zgl_GetMem( Buffer, 64 * 1024 );
      repeat
        bytesRead := decoderRead( _vf, Buffer, 64 * 1024, _End );
        INC( Size, bytesRead );
      until _End;
      _vi := nil;
      ov_clear( _vf );

      if ov_open_callbacks( nil, _vf, oggMemory.Memory, oggMemory.Size, _vc ) >= 0 Then
        begin
          GetMem( Data, Size );
          decoderRead( _vf, Data, Size, _end );
          _vi := nil;
          ov_clear( _vf );
        end;
    end;
end;

procedure ogg_LoadFromFile( const FileName : String; var Data : Pointer; var Size, Format, Frequency : LongWord );
begin
  mem_LoadFromFile( oggMemory, FileName );
  ogg_Load( Data, Size, Format, Frequency );
  mem_Free( oggMemory );
end;

procedure ogg_LoadFromMemory( const Memory : zglTMemory; var Data : Pointer; var Size, Format, Frequency : LongWord );
begin
  oggMemory.Size     := Memory.Size;
  oggMemory.Memory   := Memory.Memory;
  oggMemory.Position := Memory.Position;
  ogg_Load( Data, Size, Format, Frequency );
end;

initialization
  oggDecoder.Ext   := OGG_EXTENSION;
  oggDecoder.Open  := ogg_DecoderOpen;
  oggDecoder.Read  := ogg_DecoderRead;
  oggDecoder.Loop  := ogg_DecoderLoop;
  oggDecoder.Close := ogg_DecoderClose;
  zgl_Reg( SND_FORMAT_EXTENSION,   @OGG_EXTENSION[ 0 ] );
  zgl_Reg( SND_FORMAT_FILE_LOADER, @ogg_LoadFromFile );
  zgl_Reg( SND_FORMAT_MEM_LOADER,  @ogg_LoadFromMemory );
  zgl_Reg( SND_FORMAT_DECODER,     @oggDecoder );

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
