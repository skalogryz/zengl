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
unit zgl_video;

{$I zgl_config.cfg}

interface

uses
  zgl_types,
  zgl_file,
  zgl_memory,
  zgl_textures;

type
  zglPVideoStream  = ^zglTVideoStream;
  zglPVideoDecoder = ^zglTVideoDecoder;
  zglPVideoManager = ^zglTVideoManager;

  zglTVideoStream = record
    _private   : record
      Data    : Pointer;
      File_   : zglTFile;
      Memory  : zglTMemory;
      Decoder : zglPVideoDecoder;
               end;

    Data       : Pointer;
    Texture    : zglPTexture;
    Frame      : Integer;
    Time       : Double;

    Info       : record
      Width     : Word;
      Height    : Word;
      FrameRate : Single;
      Duration  : Double;
                end;

    Loop       : Boolean;

    prev, next : zglPVideoStream;
  end;

  zglTVideoDecoder = record
    Extension : UTF8String;
    Open      : function( var Stream : zglTVideoStream; const FileName : UTF8String ) : Boolean;
    OpenMem   : function( var Stream : zglTVideoStream; const Memory : zglTMemory ) : Boolean;
    Update    : procedure( var Stream : zglTVideoStream; Time : Double; var Data : Pointer );
    Loop      : procedure( var Stream : zglTVideoStream );
    Close     : procedure( var Stream : zglTVideoStream );
  end;

  zglTVideoManager = record
    Count    : record
      Items    : Integer;
      Decoders : Integer;
              end;
    First    : zglTVideoStream;
    Decoders : array of zglPVideoDecoder;
  end;

function  video_Add : zglPVideoStream;
procedure video_Del( var Stream : zglPVideoStream );

function  video_OpenFile( const FileName : UTF8String ) : zglPVideoStream;
function  video_OpenMemory( const Memory : zglTMemory; const Extension : UTF8String ) : zglPVideoStream;
procedure video_Update( var Stream : zglPVideoStream; Time : Double );

var
  managerVideo : zglTVideoManager;

implementation
uses
  zgl_main,
  zgl_utils;

function video_Add : zglPVideoStream;
begin
  Result := @managerVideo.First;
  while Assigned( Result.next ) do
    Result := Result.next;

  zgl_GetMem( Pointer( Result.next ), SizeOf( zglTVideoStream ) );
  Result.Frame     := -1;
  Result.next.prev := Result;
  Result.next.next := nil;
  Result := Result.next;
  INC( managerVideo.Count.Items );
end;

procedure video_Del( var Stream : zglPVideoStream );
begin
  if not Assigned( Stream ) Then exit;

  if Assigned( Stream._private.Decoder ) Then
    Stream._private.Decoder.Close( Stream^ );

  if Assigned( Stream._private.Memory.Memory ) Then
    mem_Free( Stream._private.Memory );

  FreeMem( Stream.Data );

  if managerTexture.Count.Items > 0 Then
    tex_Del( Stream.Texture );

  if Assigned( Stream.prev ) Then
    Stream.prev.next := Stream.next;
  if Assigned( Stream.next ) Then
    Stream.next.prev := Stream.prev;
  FreeMem( Stream );
  Stream := nil;

  DEC( managerVideo.Count.Items );
end;

function video_OpenFile( const FileName : UTF8String ) : zglPVideoStream;
  var
    i   : Integer;
    ext : UTF8String;
begin
  Result := video_Add();

  ext := u_StrUp( file_GetExtension( FileName ) );
  for i := managerVideo.Count.Decoders - 1 downto 0 do
    if ext = managerVideo.Decoders[ i ].Extension Then
      Result._private.Decoder := managerVideo.Decoders[ i ];

  if not Assigned( Result._private.Decoder ) Then
    begin
      video_Del( Result );
      exit;
    end;

  if Result._private.Decoder.Open( Result^, FileName ) Then
    begin
      Result.Texture := tex_CreateZero( Result.Info.Width, Result.Info.Height, $FF000000 );
      GetMem( Result.Data, Result.Info.Width * Result.Info.Height * 4 );
      FillChar( Result.Data^, Result.Info.Width * Result.Info.Height * 4, 255 );
      video_Update( Result, 0 );
    end else
      video_Del( Result );
end;

function video_OpenMemory( const Memory : zglTMemory; const Extension : UTF8String ) : zglPVideoStream;
  var
    i   : Integer;
    ext : UTF8String;
begin
  Result := video_Add();

  ext := u_StrUp( Extension );
  for i := managerVideo.Count.Decoders - 1 downto 0 do
    if ext = managerVideo.Decoders[ i ].Extension Then
      Result._private.Decoder := managerVideo.Decoders[ i ];

  if not Assigned( Result._private.Decoder ) Then
    begin
      video_Del( Result );
      exit;
    end;

  if Result._private.Decoder.OpenMem( Result^, Memory ) Then
    begin
      Result.Texture := tex_CreateZero( Result.Info.Width, Result.Info.Height, $FF000000 );
      GetMem( Result.Data, Result.Info.Width * Result.Info.Height * 4 );
      FillChar( Result.Data^, Result.Info.Width * Result.Info.Height * 4, 255 );
      video_Update( Result, 0 );
    end else
      video_Del( Result );
end;

procedure video_Update( var Stream : zglPVideoStream; Time : Double );
  var
    frame : Integer;
begin
  if not Assigned( Stream ) Then exit;

  frame := Stream.Frame;
  Stream._private.Decoder.Update( Stream^, Time, Stream.Data );

  if Stream.Frame <> frame Then
    tex_SetData( Stream.Texture, Stream.Data, 0, 0, Stream.Info.Width, Stream.Info.Height );
end;

end.
