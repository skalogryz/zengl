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

{$IFNDEF FPC}
  {$UNDEF USE_LIBJPEG}
{$ENDIF}

interface

uses
  {$IFNDEF USE_LIBJPEG}
  Windows,
  {$ENDIF}
  {$IFDEF WINDOWS}
  zgl_msvcrt,
  {$ENDIF}
  zgl_types,
  zgl_memory;

const
  JPG_EXTENSION  : array[ 0..3 ] of Char = ( 'J', 'P', 'G', #0 );
  JPEG_EXTENSION : array[ 0..4 ] of Char = ( 'J', 'P', 'E', 'G', #0 );

{$IFDEF USE_LIBJPEG}
  {$L jaricom}
  {$L jcomapi}
  {$L jdapimin}
  {$L jdapistd}
  {$L jdarith}
  {$L jdatasrc}
  {$L jdcoefct}
  {$L jdcolor}
  {$L jddctmgr}
  {$L jdhuff}
  {$L jdinput}
  {$L jdmainct}
  {$L jdmarker}
  {$L jdmaster}
  {$L jdmerge}
  {$L jdpostct}
  {$L jdsample}
  {$L jerror}
  {$L jidctflt}
  {$L jidctfst}
  {$L jidctint}
  {$L jmemmgr}
  {$L jmemnobs}
  {$L jquant1}
  {$L jquant2}
  {$L jutils}
  {$L wrapper}
  {$IFDEF DARWIN}
    {$LINKLIB libgcc.a}
  {$ENDIF}

type
  zglPJPGData = ^zglTJPGData;
  zglTJPGData = record
    Memory  : Pointer;
    MemSize : LongWord;
    Width   : Word;
    Height  : Word;
    GetMem  : function( Size : Integer ) : PByte; cdecl;
  end;

  procedure jpgturbo_Load( var jpgData : zglTJPGData; var Data : Pointer ); cdecl; external;
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
    Width     : Word;
    Height    : Word;
  end;
{$ENDIF}

procedure jpg_Load( var Data : Pointer; var W, H : Word );
procedure jpg_LoadFromFile( const FileName : String; var Data : Pointer; var W, H : Word );
procedure jpg_LoadFromMemory( const Memory : zglTMemory; var Data : Pointer; var W, H : Word );

implementation
uses
  zgl_main,
  zgl_log;

var
  jpgMem  : zglTMemory;
  jpgData : zglTJPGData;

{$IFNDEF USE_LIBJPEG}
procedure jpg_FillData( var Data : Pointer );
  var
    bi   : BITMAPINFO;
    bmp  : HBITMAP;
    DC   : HDC;
    p    : Pointer;
    W, H : Longint;
    i    : Integer;
begin
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

  GetMem( Data, jpgData.Width * jpgData.Height * 4 );

  for i := 0 to jpgData.Width * jpgData.Height - 1 do
    begin
      PByte( Ptr( Data ) + i * 4 + 0 )^ := PByte( Ptr( p ) + i * 4 + 2 )^;
      PByte( Ptr( Data ) + i * 4 + 1 )^ := PByte( Ptr( p ) + i * 4 + 1 )^;
      PByte( Ptr( Data ) + i * 4 + 2 )^ := PByte( Ptr( p ) + i * 4 + 0 )^;
      PByte( Ptr( Data ) + i * 4 + 3 )^ := 255;
    end;

  DeleteObject( bmp );
  DeleteDC    ( DC );
end;
{$ENDIF}

{$IFDEF USE_LIBJPEG}
function getmem_f( Size : Integer ) : PByte; cdecl;
begin
  GetMem( Pointer( Result ), Size );
end;
{$ENDIF}

procedure jpg_Load( var Data : Pointer; var W, H : Word );
  label _exit;
  {$IFNDEF USE_LIBJPEG}
  var
    m : Pointer;
    g : HGLOBAL;
  {$ENDIF}
begin
{$IFDEF USE_LIBJPEG}
  jpgData.Memory  := jpgMem.Memory;
  jpgData.MemSize := jpgMem.Size;
  jpgData.GetMem  := getmem_f;
  jpgturbo_Load( jpgData, Data );
{$ELSE}
  g := 0;
  try
    g := GlobalAlloc( GMEM_FIXED, jpgMem.Size );
    m := GlobalLock( g );
    mem_Read( jpgMem, m^, jpgMem.Size );
    GlobalUnlock( g );
    if CreateStreamOnHGlobal( Ptr( m ), FALSE, jpgData.Stream ) = S_OK Then
      if OleLoadPicture( jpgData.Stream, 0, FALSE, IPicture, jpgData.Buffer ) = S_OK Then jpg_FillData( Data );
  finally
    if g <> 0 Then GlobalFree( g );
  end;
{$ENDIF}

  W := jpgData.Width;
  H := jpgData.Height;

_exit:
  begin
  {$IFNDEF USE_LIBJPEG}
    jpgData.Buffer := nil;
    jpgData.Stream := nil;
  {$ENDIF}
  end;
end;

procedure jpg_LoadFromFile( const FileName : String; var Data : Pointer; var W, H : Word );
begin
  mem_LoadFromFile( jpgMem, FileName );
  jpg_Load( Data, W, H );
  mem_Free( jpgMem );
end;

procedure jpg_LoadFromMemory( const Memory : zglTMemory; var Data : Pointer; var W, H : Word );
begin
  jpgMem.Size     := Memory.Size;
  jpgMem.Memory   := Memory.Memory;
  jpgMem.Position := Memory.Position;
  jpg_Load( Data, W, H );
end;

initialization
  // jpg
  zgl_Reg( TEX_FORMAT_EXTENSION,   @JPG_EXTENSION[ 0 ] );
  zgl_Reg( TEX_FORMAT_FILE_LOADER, @jpg_LoadFromFile );
  zgl_Reg( TEX_FORMAT_MEM_LOADER,  @jpg_LoadFromMemory );
  // jpeg
  zgl_Reg( TEX_FORMAT_EXTENSION,   @JPEG_EXTENSION[ 0 ] );
  zgl_Reg( TEX_FORMAT_FILE_LOADER, @jpg_LoadFromFile );
  zgl_Reg( TEX_FORMAT_MEM_LOADER,  @jpg_LoadFromMemory );


end.
