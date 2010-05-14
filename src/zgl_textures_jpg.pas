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
unit zgl_textures_jpg;

{$I zgl_config.cfg}

interface

uses
  {$IFDEF USE_PASJPEG}
  jmorecfg,
  jpeglib,
  jerror,
  jdeferr,
  jdapimin,
  jdapistd,
  jdmarker,
  jdmaster,
  {$ELSE}
  Windows,
  {$ENDIF}

  zgl_memory;

const
  JPG_EXTENSION  : array[ 0..3 ] of Char = ( 'J', 'P', 'G', #0 );

{$IFDEF USE_PASJPEG}
const
  INPUT_BUF_SIZE = 4096;

type
  zglPJPGDecoder = ^zglTJPGDecoder;
  zglTJPGDecoder = record
    mgr    : jpeg_source_mgr;
    field  : JOCTET_FIELD_PTR;
end;

type
  zglPJPGData = ^zglTJPGData;
  zglTJPGData = record
    Buffer    : JSAMPARRAY;
    Data      : array of Byte;
    Width     : Word;
    Height    : Word;
    sWidth    : JDIMENSION; // Scanline width
    Grayscale : Boolean;
end;
{$ELSE}
type
  OLE_HANDLE = LongWord;
  OLE_XPOS_HIMETRIC  = Longint;
  OLE_YPOS_HIMETRIC  = Longint;
  OLE_XSIZE_HIMETRIC = Longint;
  OLE_YSIZE_HIMETRIC = Longint;

  PCLSID = PGUID;
  TCLSID = TGUID;

  POleStr  = PWideChar;
  Largeint = Int64;

  PStatStg = ^TStatStg;
  tagSTATSTG = record
    pwcsName: POleStr;
    dwType: Longint;
    cbSize: Largeint;
    mtime: TFileTime;
    ctime: TFileTime;
    atime: TFileTime;
    grfMode: Longint;
    grfLocksSupported: Longint;
    clsid: TCLSID;
    grfStateBits: Longint;
    reserved: Longint;
  end;
  TStatStg = tagSTATSTG;
  {$EXTERNALSYM STATSTG}
  STATSTG = TStatStg;

  ISequentialStream = interface(IUnknown)
    ['{0c733a30-2a1c-11ce-ade5-00aa0044773d}']
    function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult;
      stdcall;
    function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult;
      stdcall;
  end;

  IStream = interface(ISequentialStream)
    ['{0000000C-0000-0000-C000-000000000046}']
    function Seek(dlibMove: Largeint; dwOrigin: Longint;
      out libNewPosition: Largeint): HResult; stdcall;
    function SetSize(libNewSize: Largeint): HResult; stdcall;
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
      out cbWritten: Largeint): HResult; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function UnlockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
      stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
  end;

  IPicture = interface
    ['{7BF80980-BF32-101A-8BBB-00AA00300CAB}']
    function get_Handle(out handle: OLE_HANDLE): HResult;  stdcall;
    function get_hPal(out handle: OLE_HANDLE): HResult; stdcall;
    function get_Type(out typ: Smallint): HResult; stdcall;
    function get_Width(out width: OLE_XSIZE_HIMETRIC): HResult; stdcall;
    function get_Height(out height: OLE_YSIZE_HIMETRIC): HResult; stdcall;
    function Render(dc: HDC; x, y, cx, cy: Longint;
      xSrc: OLE_XPOS_HIMETRIC; ySrc: OLE_YPOS_HIMETRIC;
      cxSrc: OLE_XSIZE_HIMETRIC; cySrc: OLE_YSIZE_HIMETRIC;
      rcWBounds: Pointer): HResult; stdcall;
    function set_hPal(hpal: OLE_HANDLE): HResult; stdcall;
    function get_CurDC(out dcOut: HDC): HResult; stdcall;
    function SelectPicture(dcIn: HDC; out hdcOut: HDC;
      out bmpOut: OLE_HANDLE): HResult; stdcall;
    function get_KeepOriginalFormat(out fkeep: BOOL): HResult; stdcall;
    function put_KeepOriginalFormat(fkeep: BOOL): HResult; stdcall;
    function PictureChanged: HResult; stdcall;
    function SaveAsFile(const stream: IStream; fSaveMemCopy: BOOL;
      out cbSize: Longint): HResult; stdcall;
    function get_Attributes(out dwAttr: Longint): HResult; stdcall;
  end;

  function OleLoadPicture(stream: IStream; lSize: Longint; fRunmode: BOOL;
    const iid: TGUID; var vObject): HResult; stdcall external 'olepro32.dll' name 'OleLoadPicture';

  function CreateStreamOnHGlobal(hglob: HGlobal; fDeleteOnRelease: BOOL;
    var stm: IStream): HResult; stdcall external 'ole32.dll' name 'CreateStreamOnHGlobal';

type
  zglPJPGData = ^zglTJPGData;
  zglTJPGData = record
    Buffer    : IPicture;
    Stream    : IStream;
    Data      : array of Byte;
    Width     : Word;
    Height    : Word;
end;
{$ENDIF}

procedure jpg_Load( var Data : Pointer; var W, H : Word );
procedure jpg_LoadFromFile( const FileName : String; var Data : Pointer; var W, H : Word );
procedure jpg_LoadFromMemory( const Memory : zglTMemory; var Data : Pointer; var W, H : Word );
procedure jpg_FillData;

implementation
uses
  zgl_types,
  zgl_main,
  zgl_log;

var
  jpgMem     : zglTMemory;
  {$IFDEF USE_PASJPEG}
  jpgDecoder : zglPJPGDecoder;
  jpgCInfo   : jpeg_decompress_struct;
  jpgData    : zglTJPGData;
  {$ELSE}
  jpgData    : zglTJPGData;
  {$ENDIF}

{$IFDEF USE_PASJPEG}
procedure jpeg_output_message( cinfo : j_common_ptr ); register;
  var
    str : String;
begin
  cInfo.err.format_message( cinfo, str );
  log_Add( 'JPEG - ' + str );
end;

function jpeg_error( var err : jpeg_error_mgr ) : jpeg_error_mgr_ptr; register;
begin
  jpeg_std_error( err );
  err.output_message := jpeg_output_message;
  Result := @err;
end;

procedure Decoder_Init( cinfo : j_decompress_ptr ); register;
begin
end;

procedure Decoder_Term( cinfo : j_decompress_ptr ); register;
begin
end;

function Decoder_FillInputData( cinfo : j_decompress_ptr ) : Boolean; register;
  var
    decoder   : zglPJPGDecoder;
    bytesRead : Integer;
begin
  decoder := zglPJPGDecoder( cinfo.src );
  bytesRead := mem_Read( jpgMem, decoder.field^, INPUT_BUF_SIZE );
  if bytesRead <= 0 Then
    begin
      WARNMS( j_common_ptr( cinfo ), JWRN_JPEG_EOF );

      decoder.field[ 0 ] := JOCTET( $FF );
      decoder.field[ 1 ] := JOCTET( JPEG_EOI );
      bytesRead := 2;
    end;

  decoder.mgr.next_input_byte := JOCTETptr( decoder.field );
  decoder.mgr.bytes_in_buffer := bytesRead;
  Result := TRUE;
end;

procedure Decoder_SkipInputData( cinfo : j_decompress_ptr; BytesToSkip : Long ); register;
  var
    decoder : zglPJPGDecoder;
begin
  decoder := zglPJPGDecoder( cInfo.src );
  if BytesToSkip > 0 Then
    begin
      while BytesToSkip > Decoder.mgr.bytes_in_buffer do
        begin
          DEC( BytesToSkip, decoder.mgr.bytes_in_buffer );
          Decoder_FillInputData( cInfo );
        end;
      INC( decoder.mgr.next_input_byte, size_t( BytesToSkip ) );
      DEC( decoder.mgr.bytes_in_buffer, size_t( BytesToSkip ) );
    end;
end;
{$ENDIF}

procedure jpg_Load;
  label _exit;
  {$IFDEF USE_PASJPEG}
  var
    jerr : jpeg_error_mgr;
  {$ELSE}
  var
    m : Pointer;
    g : HGLOBAL;
  {$ENDIF}
begin
{$IFDEF USE_PASJPEG}
  jpgCInfo.err := jpeg_error( jerr );
  jpeg_create_decompress( @jpgCInfo );

  if not Assigned( jpgCInfo.src ) Then
    begin
      jpgCInfo.src := jpeg_source_mgr_ptr( jpgCInfo.mem.alloc_small( j_common_ptr( @jpgCInfo ), JPOOL_PERMANENT, SizeOf( zglTJPGDecoder ) ) );
      jpgDecoder := zglPJPGDecoder( jpgCInfo.src );
      jpgDecoder.field := JOCTET_FIELD_PTR( jpgCInfo.Mem.Alloc_small( j_common_ptr( @jpgCInfo ), JPOOL_PERMANENT, INPUT_BUF_SIZE * SizeOf( JOCTET ) ) );
    end;

  jpgDecoder                       := zglPJPGDecoder( jpgCInfo.src );
  jpgDecoder.mgr.init_source       := Decoder_Init;
  jpgDecoder.mgr.fill_input_buffer := Decoder_FillInputData;
  jpgDecoder.mgr.skip_input_data   := Decoder_SkipInputData;
  jpgDecoder.mgr.term_source       := Decoder_Term;
  jpgDecoder.mgr.resync_to_restart := jpeg_resync_to_restart;
  jpgDecoder.mgr.bytes_in_buffer   := 0;
  jpgDecoder.mgr.next_input_byte   := nil;

  jpgCInfo.dither_mode         := JDITHER_NONE;
  jpgCInfo.dct_method          := JDCT_FASTEST;
  jpgCInfo.two_pass_quantize   := FALSE;
  jpgCInfo.do_fancy_upsampling := FALSE;
  jpgCInfo.do_block_smoothing  := FALSE;

  jpeg_read_header( @jpgCInfo, TRUE );

  jpeg_calc_output_dimensions( @jpgCInfo );
  jpgData.sWidth := jpgCInfo.output_width * jpgCInfo.output_Components;
  SetLength( jpgData.Data, jpgCInfo.output_width * jpgCInfo.output_height * 4 );
  jpgData.Width  := jpgCInfo.output_width;
  jpgData.Height := jpgCInfo.output_height;

  if jpgCInfo.out_color_space = JCS_GRAYSCALE Then
    jpgData.Grayscale := TRUE
  else
    if jpgCInfo.out_color_space = JCS_RGB Then
      if jpgCInfo.quantize_colors Then
        jpgData.Grayscale := TRUE
      else
        jpgData.Grayscale := FALSE
    else
      begin
        log_Add( 'JPG - Unsupported ColorType' );
        goto _exit;
      end;
  jpgData.buffer := jpgCInfo.mem.alloc_sarray( j_common_ptr( @jpgCInfo ), JPOOL_IMAGE, jpgData.sWidth, 1 );

  jpeg_start_decompress( @jpgCInfo );

  while jpgCInfo.Output_Scanline < jpgCInfo.Output_Height do
    begin
      jpeg_read_scanlines( @jpgCInfo, jpgData.buffer, 1 );
      jpg_FillData;
    end;
{$ELSE}
  g := 0;
  try
    g := GlobalAlloc( GMEM_FIXED, jpgMem.Size );
    m := GlobalLock( g );
    mem_Read( jpgMem, m^, jpgMem.Size );
    GlobalUnlock( g );
    if CreateStreamOnHGlobal( Ptr( m ), FALSE, jpgData.Stream ) = S_OK Then
      if OleLoadPicture( jpgData.Stream, 0, FALSE, IPicture, jpgData.Buffer ) = S_OK Then jpg_FillData;
  finally
    if g <> 0 Then GlobalFree( g );
  end;
{$ENDIF}

  zgl_GetMem( Data, jpgData.Width * jpgData.Height * 4 );
  Move( Pointer( jpgData.Data )^, Data^, jpgData.Width * jpgData.Height * 4 );
  W := jpgData.Width;
  H := jpgData.Height;

_exit:
  begin
  {$IFDEF USE_PASJPEG}
    SetLength( jpgData.Data, 0 );
    jpeg_finish_decompress ( @jpgCInfo );
    jpeg_destroy_decompress( @jpgCInfo );
  {$ELSE}
    SetLength( jpgData.Data, 0 );
    jpgData.Buffer := nil;
    jpgData.Stream := nil;
  {$ENDIF}
    mem_Free( jpgMem );
  end;
end;

procedure jpg_LoadFromFile;
begin
  mem_LoadFromFile( jpgMem, FileName );
  jpg_Load( Data, W, H );
end;

procedure jpg_LoadFromMemory;
begin
  jpgMem.Size := Memory.Size;
  zgl_GetMem( jpgMem.Memory, Memory.Size );
  jpgMem.Position := Memory.Position;
  Move( Memory.Memory^, jpgMem.Memory^, Memory.Size );
  jpg_Load( Data, W, H );
end;

procedure jpg_FillData;
  {$IFDEF USE_PASJPEG}
  var
    i, j  : JDIMENSION;
    color : JSAMPLE_PTR;
  {$ELSE}
  var
    bi   : BITMAPINFO;
    bmp  : HBITMAP;
    DC   : HDC;
    p    : Pointer;
    W, H : Longint;
    i    : Integer;
    t    : Byte;
  {$ENDIF}
begin
{$IFDEF USE_PASJPEG}
  color := JSAMPLE_PTR( jpgData.Buffer[ 0 ] );
  if not jpgData.Grayscale Then
    begin
     for i := 0 to jpgData.Width - 1 do
        begin
          j := i * 4 + ( jpgData.Height - jpgCInfo.Output_Scanline ) * jpgData.Width * 4;
          jpgData.Data[ j + 0 ] := PByte( Ptr( color ) + i * 3 + 0 )^;
          jpgData.Data[ j + 1 ] := PByte( Ptr( color ) + i * 3 + 1 )^;
          jpgData.Data[ j + 2 ] := PByte( Ptr( color ) + i * 3 + 2 )^;
          jpgData.Data[ j + 3 ] := 255;
        end;
    end else
      begin
        for i := 0 to jpgData.Width - 1 do
          begin
            j := i * 4 + ( jpgData.Height - jpgCInfo.Output_Scanline ) * jpgData.Width * 4;
            jpgData.Data[ j + 0 ] := color^;
            jpgData.Data[ j + 1 ] := color^;
            jpgData.Data[ j + 2 ] := color^;
            jpgData.Data[ j + 3 ] := 255;

            INC( color );
          end;
      end;
{$ELSE}
  DC := CreateCompatibleDC( GetDC( 0 ) );
  jpgData.Buffer.get_Width ( W );
  jpgData.Buffer.get_Height( H );
  jpgData.Width  := MulDiv( W, GetDeviceCaps( DC, LOGPIXELSX ), 2540 );
  jpgData.Height := MulDiv( H, GetDeviceCaps( DC, LOGPIXELSY ), 2540 );

  FillChar( bi, SizeOf( bi ), 0 );
  bi.bmiHeader.biSize        := SizeOf( BITMAPINFOHEADER );
  bi.bmiHeader.biBitCount    := 32;
  bi.bmiHeader.biWidth       := jpgData.Width;
  bi.bmiHeader.biHeight      := jpgData.Height;
  bi.bmiHeader.biCompression := BI_RGB;
  bi.bmiHeader.biPlanes      := 1;
  bmp := CreateDIBSection( DC, bi, DIB_RGB_COLORS, p, 0, 0 );
  SelectObject( DC, bmp );
  jpgData.Buffer.Render( DC, 0, 0, jpgData.Width, jpgData.Height, 0, H, W, -H, nil );

  for i := 0 to jpgData.Width * jpgData.Height - 1 do
    begin
      t := PByte( Ptr( p ) + i * 4 + 2 )^;
      PByte( Ptr( p ) + i * 4 + 2 )^ := PByte( Ptr( p ) + i * 4 + 0 )^;
      PByte( Ptr( p ) + i * 4 + 0 )^ := t;
      PByte( Ptr( p ) + i * 4 + 3 )^ := 255;
    end;

  SetLength( jpgData.Data, jpgData.Width * jpgData.Height * 4 );
  Move( p^, Pointer( jpgData.Data )^, jpgData.Width * jpgData.Height * 4 );

  DeleteObject( bmp );
  DeleteDC    ( DC );
{$ENDIF}
end;

initialization
  zgl_Reg( TEX_FORMAT_EXTENSION,   @JPG_EXTENSION[ 0 ] );
  zgl_Reg( TEX_FORMAT_FILE_LOADER, @jpg_LoadFromFile );
  zgl_Reg( TEX_FORMAT_MEM_LOADER,  @jpg_LoadFromMemory );

end.
