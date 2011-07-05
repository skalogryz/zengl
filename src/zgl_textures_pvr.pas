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
unit zgl_textures_pvr;

{$I zgl_config.cfg}

interface

uses
  CFByteOrders,
  zgl_file,
  zgl_memory;

const
  PVR_EXTENSION : array[ 0..3 ] of Char = ( 'P', 'V', 'R', #0 );

  PVR_PVRTC2 = 24;
  PVR_PVRTC4 = 25;

type
  zglPPVRHeader = ^zglTPVRHeader;
  zglTPVRHeader = record
    HeaderLength : LongWord;
    Height       : LongWord;
    Width        : LongWord;
    NumMipmaps   : LongWord;
    Flags        : LongWord;
    DataLength   : LongWord;
    BPP          : LongWord;
    BitmaskRed   : LongWord;
    BitmaskGreen : LongWord;
    BitmaskBlue  : LongWord;
    BitmaskAlpha : LongWord;
    PVRTag       : LongWord;
    NumSurfs     : LongWord;
  end;


procedure pvr_LoadFromFile( const FileName : String; var Data : Pointer; var W, H, Format : Word );
procedure pvr_LoadFromMemory( const Memory : zglTMemory; var Data : Pointer; var W, H, Format : Word );

implementation
uses
  zgl_types,
  zgl_main,
  zgl_log,
  zgl_textures;

procedure pvr_LoadFromFile( const FileName : String; var Data : Pointer; var W, H, Format : Word );
  var
    pvrMem : zglTMemory;
begin
  mem_LoadFromFile( pvrMem, FileName );
  pvr_LoadFromMemory( pvrMem, Data, W, H, Format );
  mem_Free( pvrMem );
end;

procedure pvr_LoadFromMemory( const Memory : zglTMemory; var Data : Pointer; var W, H, Format : Word );
  var
    pvrMem    : zglTMemory;
    pvrHeader : zglTPVRHeader;
    size      : LongWord;
    flags     : LongWord;
begin
  pvrMem := Memory;
  mem_Read( pvrMem, pvrHeader, SizeOf( zglTPVRHeader ) );
  W     := CFSwapInt32LittleToHost( pvrHeader.Width );
  H     := CFSwapInt32LittleToHost( pvrHeader.Height );
  size  := CFSwapInt32LittleToHost( pvrHeader.DataLength );
	flags := CFSwapInt32LittleToHost( pvrHeader.Flags ) and $FF;
  if ( flags = PVR_PVRTC2 ) Then
    Format := TEX_FORMAT_RGBA_PVR2
  else
    Format := TEX_FORMAT_RGBA_PVR4;

  GetMem( Data, size );
  Move( Pointer( Ptr( pvrMem.Memory ) + pvrMem.Position )^, Data^, size );
end;

initialization
  zgl_Reg( TEX_FORMAT_EXTENSION,   @PVR_EXTENSION[ 0 ] );
  zgl_Reg( TEX_FORMAT_FILE_LOADER, @pvr_LoadFromFile );
  zgl_Reg( TEX_FORMAT_MEM_LOADER,  @pvr_LoadFromMemory );

end.
