{
 *  Copyright © Kemka Andrey aka Andru
 *  mail: dr.andru@gmail.com
 *  site: http://zengl.org
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

{$IFNDEF FPC}
  {$UNDEF USE_OGG_STATIC}
{$ENDIF}

//
{$IFDEF ANDROID}
  {$DEFINE USE_OGG_STATIC}
{$ENDIF}
// Developers from xiph.org didn't include target which builds dylib's, so...
{$IFDEF DARWIN}
  {$DEFINE USE_OGG_STATIC}
{$ENDIF}

{$IFDEF USE_OGG_STATIC}
  {$IFNDEF USE_TREMOLO}
    {$L bitwise}
    {$L framing}
    {$L analysis}
    {$L bitrate}
    {$L block}
    {$L codebook}
    {$L envelope}
    {$L floor0}
    {$L floor1}
    {$L info}
    {$L lookup}
    {$L lpc}
    {$L lsp}
    {$L mapping0}
    {$L mdct}
    {$L psy}
    {$L registry}
    {$L res0}
    {$L sharedbook}
    {$L smallft}
    {$L synthesis}
    {$L vorbisfile}
    {$L window}
  {$ELSE}
    {$LINKLIB libtremolo.a}
  {$ENDIF}
  {$IFDEF UNIX}
    {$LINKLIB m}
  {$ENDIF}
  {$IFDEF MACOSX}
    {$LINKLIB libgcc.a}
  {$ENDIF}
  {$IFDEF iOS}
    {$LINKLIB libgcc_s.1.dylib}
  {$ENDIF}
{$ENDIF}

interface
uses
  {$IFDEF WINDOWS}
  zgl_lib_msvcrt,
  {$ENDIF}
  zgl_memory;

const
  OGG_EXTENSION : array[ 0..3 ] of Char = ( 'O', 'G', 'G', #0 );

procedure ogg_LoadFromFile( const FileName : String; var Data : Pointer; var Size, Format, Frequency : LongWord );
procedure ogg_LoadFromMemory( const Memory : zglTMemory; var Data : Pointer; var Size, Format, Frequency : LongWord );

implementation
uses
  zgl_types,
  zgl_main,
  zgl_sound,
  zgl_file,
  zgl_log,
  zgl_utils;

const
{$IFDEF LINUX}
  libogg        = 'libogg.so.0';
  libvorbis     = 'libvorbis.so.0';
  libvorbisfile = 'libvorbisfile.so.3';
{$ENDIF}
{$IFDEF WINDOWS}
  libogg        = 'libogg-0.dll';
  libvorbis     = 'libvorbis-0.dll';
  libvorbisfile = 'libvorbisfile-3.dll';
{$ENDIF}
{$IFDEF MACOSX}
  libogg        = 'libogg.0.dylib';
  libvorbis     = 'libvorbis.0.dylib';
  libvorbisfile = 'libvorbisfile.3.dylib';
{$ENDIF}
{$IFDEF ENDIAN_BIG}
  BIG_ENDIAN = TRUE;
{$ELSE}
  BIG_ENDIAN = FALSE;
{$ENDIF}

type
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
    bittrack        : {$IFNDEF USE_TREMOLO} cdouble {$ELSE} ogg_int64_t {$ENDIF};
    samptrack       : {$IFNDEF USE_TREMOLO} cdouble {$ELSE} ogg_int64_t {$ENDIF};
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

{$IFDEF USE_OGG_STATIC}
  function ov_clear(var vf: OggVorbis_File): cint; cdecl; external;
  function ov_open_callbacks(datasource: pointer; var vf: OggVorbis_File; initial: pointer; ibytes: clong; callbacks: ov_callbacks): cint; cdecl; external;
  function ov_info(var vf: OggVorbis_File; link: cint): pvorbis_info; cdecl; external;
  function ov_read(var vf: OggVorbis_File; buffer: pointer; length: cint; {$IFNDEF USE_TREMOLO} bigendianp: cbool; word: cint; sgned: cbool; {$ENDIF} bitstream: pcint): clong; cdecl; external;
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

var
  oggLoad    : Boolean;
  oggInit    : Boolean;
  oggDecoder : zglTSoundDecoder;

{$IFNDEF USE_OGG_STATIC}
  oggLibrary        : {$IFDEF UNIX} Pointer {$ENDIF} {$IFDEF WINDOWS} HMODULE {$ENDIF};
  vorbisLibrary     : {$IFDEF UNIX} Pointer {$ENDIF} {$IFDEF WINDOWS} HMODULE {$ENDIF};
  vorbisfileLibrary : {$IFDEF UNIX} Pointer {$ENDIF} {$IFDEF WINDOWS} HMODULE {$ENDIF};
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


function ogg_ReadMem( ptr : pointer; size, nmemb : csize_t; datasource : pointer) : csize_t; cdecl;
begin
  Result := mem_Read( zglTMemory( datasource^ ), ptr^, size * nmemb );
end;

function ogg_SeekMem( datasource : pointer; offset : cint64; whence : cint) : cint; cdecl;
begin
  case whence of
    0: mem_Seek( zglTMemory( datasource^ ), offset, FSM_SET );
    1: mem_Seek( zglTMemory( datasource^ ), offset, FSM_CUR );
    2: mem_Seek( zglTMemory( datasource^ ), offset, FSM_END );
  end;
  Result := 0;
end;

function ogg_GetPosMem( datasource : pointer ) : clong; cdecl;
begin
  Result := zglTMemory( datasource^ ).Position;
end;

procedure ogg_Init;
begin
{$IFDEF USE_OGG_STATIC}
  oggInit := TRUE;
{$ELSE}
  {$IFDEF LINUX}
  oggLibrary        := dlopen( libogg, $001 );
  vorbisLibrary     := dlopen( libvorbis, $001 );
  vorbisfileLibrary := dlopen( libvorbisfile, $001 );
  {$ENDIF}
  {$IFDEF WINDOWS}
  oggLibrary        := dlopen( libogg );
  vorbisLibrary     := dlopen( libvorbis );
  vorbisfileLibrary := dlopen( libvorbisfile );
  {$ENDIF}
  {$IFDEF MACOSX}
  oggLibrary        := dlopen( PChar( app_WorkDir + 'Contents/Frameworks/' + libogg ), $001 );
  vorbisLibrary     := dlopen( PChar( app_WorkDir + 'Contents/Frameworks/' + libvorbis ), $001 );
  vorbisfileLibrary := dlopen( PChar( app_WorkDir + 'Contents/Frameworks/' + libvorbisfile ), $001 );
  {$ENDIF}

  if ( oggLibrary <> LIB_ERROR ) and ( vorbisLibrary <> LIB_ERROR ) and ( vorbisfileLibrary <> LIB_ERROR ) Then
    begin
      ov_clear          := dlsym( vorbisfileLibrary, 'ov_clear' );
      ov_open_callbacks := dlsym( vorbisfileLibrary, 'ov_open_callbacks' );
      ov_info           := dlsym( vorbisfileLibrary, 'ov_info' );
      ov_read           := dlsym( vorbisfileLibrary, 'ov_read' );
      ov_pcm_seek       := dlsym( vorbisfileLibrary, 'ov_pcm_seek' );
      ov_pcm_total      := dlsym( vorbisfileLibrary, 'ov_pcm_total' );

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

  with zglTOggStream( Stream._data^ ) do
    begin
      vc.read  := @ogg_Read;
      vc.seek  := @ogg_Seek;
      vc.close := @ogg_Close;
      vc.tell  := @ogg_GetPos;
      if ov_open_callbacks( @Stream._file, vf, nil, 0, vc ) >= 0 Then
        begin
          vi                := ov_info( vf, -1 );
          Stream.Bits       := 16;
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

function ogg_DecoderOpenMem( var Stream : zglTSoundStream; const Memory : zglTMemory ) : Boolean;
begin
  Result := FALSE;
  if not oggLoad Then ogg_Init;
  if not oggInit Then exit;

  zgl_GetMem( Stream._data, SizeOf( zglTOggStream ) );
  Stream._memory := Memory;

  with zglTOggStream( Stream._data^ ) do
    begin
      FillChar( vc, SizeOf( vc ), 0 );
      vc.read  := @ogg_ReadMem;
      vc.seek  := @ogg_SeekMem;
      vc.tell  := @ogg_GetPosMem;
      if ov_open_callbacks( @Stream._memory, vf, Pointer( Ptr( Memory.Memory ) + Memory.Position ), Memory.Size - Memory.Position, vc ) >= 0 Then
        begin
          vi                := ov_info( vf, -1 );
          Stream.Bits       := 16;
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
  Result := 0;
  if not oggInit Then exit;

  bytesRead := 0;
  repeat
    Result := ov_read( zglTOggStream( Stream._data^ ).vf, Pointer( Ptr( Buffer ) + bytesRead ), Bytes - bytesRead, {$IFNDEF USE_TREMOLO} BIG_ENDIAN, 2, TRUE, {$ENDIF} nil );
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
    Result := ov_read( VorbisFile, Pointer( Ptr( Buffer ) + bytesRead ), Bytes - bytesRead, {$IFNDEF USE_TREMOLO} BIG_ENDIAN, 2, TRUE, {$ENDIF} nil );
    bytesRead := bytesRead + Result;
  until ( Result = 0 ) or ( bytesRead = Bytes );

  _End   := Result = 0;
  Result := bytesRead;
end;

procedure ogg_LoadFromFile( const FileName : String; var Data : Pointer; var Size, Format, Frequency : LongWord );
  var
    oggMemory : zglTMemory;
begin
  mem_LoadFromFile( oggMemory, FileName );
  ogg_LoadFromMemory( oggMemory, Data, Size, Format, Frequency );
  mem_Free( oggMemory );
end;

procedure ogg_LoadFromMemory( const Memory : zglTMemory; var Data : Pointer; var Size, Format, Frequency : LongWord );
  var
    bytesRead : Integer;
    buffer    : Pointer;
    _end      : Boolean;

    _vi : pvorbis_info;
    _vf : OggVorbis_File;
    _vc : ov_callbacks;
begin
  if not oggLoad Then ogg_Init();
  if not oggInit Then exit;

  FillChar( _vc, SizeOf( _vc ), 0 );
  if ov_open_callbacks( nil, _vf, Pointer( Ptr( Memory.Memory ) + Memory.Position ), Memory.Size - Memory.Position, _vc ) >= 0 Then
    begin
      _vi       := ov_info( _vf, -1 );
      Frequency := _vi.rate;
      case _vi.channels of
        1: format := SND_FORMAT_MONO16;
        2: format := SND_FORMAT_STEREO16;
      end;

      Size := 0;
      zgl_GetMem( Buffer, 64 * 1024 );
      repeat
        bytesRead := decoderRead( _vf, Buffer, 64 * 1024, _End );
        INC( Size, bytesRead );
      until _End;
      FreeMem( Buffer );
      _vi := nil;
      ov_clear( _vf );

      if ov_open_callbacks( nil, _vf, Pointer( Ptr( Memory.Memory ) + Memory.Position ), Memory.Size - Memory.Position, _vc ) >= 0 Then
        begin
          GetMem( Data, Size );
          decoderRead( _vf, Data, Size, _end );
          _vi := nil;
          ov_clear( _vf );
        end;
    end;
end;

initialization
  oggDecoder.Ext     := OGG_EXTENSION;
  oggDecoder.Open    := ogg_DecoderOpen;
  oggDecoder.OpenMem := ogg_DecoderOpenMem;
  oggDecoder.Read    := ogg_DecoderRead;
  oggDecoder.Loop    := ogg_DecoderLoop;
  oggDecoder.Close   := ogg_DecoderClose;
  zgl_Reg( SND_FORMAT_EXTENSION,   @OGG_EXTENSION[ 0 ] );
  zgl_Reg( SND_FORMAT_FILE_LOADER, @ogg_LoadFromFile );
  zgl_Reg( SND_FORMAT_MEM_LOADER,  @ogg_LoadFromMemory );
  zgl_Reg( SND_FORMAT_DECODER,     @oggDecoder );

finalization
{$IFNDEF USE_OGG_STATIC}
  if oggInit Then
    begin
      dlclose( oggLibrary );
      dlclose( vorbisLibrary );
      dlclose( vorbisfileLibrary );
    end;
{$ENDIF}

end.
