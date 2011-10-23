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
unit zgl_resources;

{$I zgl_config.cfg}

interface
uses
  zgl_memory,
  zgl_textures,
  {$IFDEF USE_SOUND}
  zgl_sound,
  {$ENDIF}
  zgl_types;

const
  RES_TEXTURE           = 1;
  RES_TEXTURE_FRAMESIZE = 2;
  RES_SOUND             = 3;

  QUEUE_STATE_STOP  = 0;
  QUEUE_STATE_START = 1;

type
  zglPResourceItem = ^zglTResourceItem;
  zglTResourceItem = record
    _type      : Integer;
    IsFromFile : Boolean;
    Ready      : Boolean;
    Resource   : Pointer;

    prev, next : zglPResourceItem;
  end;

type
  zglPTextureResource = ^zglTTextureResource;
  zglTTextureResource = record
    FileName         : String;
    Memory           : zglTMemory;
    Texture          : zglPTexture;
    FileLoader       : zglTTextureFileLoader;
    MemLoader        : zglTTextureMemLoader;
    pData            : Pointer;
    TransparentColor : LongWord;
    Flags            : LongWord;
    Format           : Word;
    Width, Height    : Word;
  end;

type
  zglPTextureFrameSizeResource = ^zglTTextureFrameSizeResource;
  zglTTextureFrameSizeResource = record
    Texture     : zglPTexture;
    FrameWidth  : Integer;
    FrameHeight : Integer;
  end;

{$IFDEF USE_SOUND}
type
  zglPSoundResource = ^zglTSoundResource;
  zglTSoundResource = record
    FileName   : String;
    Memory     : zglTMemory;
    Sound      : zglPSound;
    FileLoader : zglTSoundFileLoader;
    MemLoader  : zglTSoundMemLoader;
    Format     : LongWord;
  end;
{$ENDIF}

procedure res_Init;
procedure res_Free;
procedure res_Proc;
procedure res_AddToQueue( _type : Integer; FromFile : Boolean; Resource : Pointer );
function  res_ProcQueue( data : Pointer ) : LongInt;

procedure res_BeginQueue( QueueID : Byte );
procedure res_EndQueue;
function  res_GetPercentage( QueueID : Byte ) : Integer;

var
  resInitialized : Boolean;
  resBackground  : Boolean;
  resIsLoaded    : Boolean;
  resPercentage  : Integer;
  resThread      : LongWord;
  {$IFNDEF FPC}
  resThreadID    : LongWord;
  {$ENDIF}
  resQueueState  : Integer;
  resQueueSize   : Integer;
  resQueueMax    : Integer;
  resQueue       : zglTResourceItem;

implementation
uses
  zgl_main,
  zgl_window,
  zgl_screen,
  zgl_application,
  zgl_log,
  zgl_utils;

procedure res_Init;
begin
{$IFDEF FPC}
  resThread := LongWord( BeginThread( @res_ProcQueue, nil ) );
{$ELSE}
  resThread := BeginThread( nil, 0, @res_ProcQueue, nil, 0, resThreadID );
{$ENDIF}
  while not resInitialized do;
end;

procedure res_Free;
begin
  if resInitialized Then
    begin
      resQueueSize := 0;
      while resInitialized do;
    end;
end;

procedure res_Proc;
  var
    item : zglPResourceItem;
begin
  if resQueueSize <= 0 Then exit;

  item := resQueue.next;
  while Assigned( item ) do
    begin
      if ( item.Ready ) and Assigned( item.Resource ) Then
        case item._type of
          RES_TEXTURE:
            with zglPTextureResource( item.Resource )^ do
              begin
                tex_Create( Texture^, pData );
                FreeMem( pData );
                if item.IsFromFile Then
                  log_Add( 'Texture loaded: "' + FileName + '"' );

                FileName := '';
                FreeMem( item.Resource );
                item.Resource := nil;
                item.Ready := FALSE;
                DEC( resQueueSize );
                break;
              end;
        end;

      if resQueueSize <= 0 Then
        break
      else
        item := item.next;
    end;

  if resQueueSize = 0 Then
    begin
      resPercentage := 100;
      resQueueMax   := 0;
    end else
      resPercentage := Round( ( 1 - resQueueSize / resQueueMax ) * 100 );

  resIsLoaded := resQueueSize = 0;
end;

procedure res_AddToQueue( _type : Integer; FromFile : Boolean; Resource : Pointer );
  var
    item : ^zglPResourceItem;
    last : zglPResourceItem;
    new  : Boolean;
    tex  : zglPTextureResource;
    tfs  : zglPTextureFrameSizeResource;
    {$IFDEF USE_SOUND}
    snd  : zglPSoundResource;
    {$ENDIF}
begin
  new  := TRUE;
  item := @resQueue.next;
  last := @resQueue;
  while Assigned( item^ ) do
    begin
      if not Assigned( item^.Resource ) Then
        begin
          new := FALSE;
          break;
        end;

      last := item^;
      item := @item^.next;
    end;

  INC( resQueueSize );
  INC( resQueueMax );

  if new Then
    zgl_GetMem( Pointer( item^ ), SizeOf( zglTResourceItem ) );

  case _type of
    RES_TEXTURE:
      begin
        zgl_GetMem( Pointer( tex ), SizeOf( zglTTextureResource ) );
        with zglPTextureResource( Resource )^ do
          begin
            tex.FileName         := FileName;
            tex.Memory           := Memory;
            tex.Texture          := Texture;
            tex.FileLoader       := FileLoader;
            tex.MemLoader        := MemLoader;
            tex.TransparentColor := TransparentColor;
            tex.Flags            := Flags;
          end;
        item^.Resource := tex;
      end;
    RES_TEXTURE_FRAMESIZE:
      begin
        zgl_GetMem( Pointer( tfs ), SizeOf( zglTTextureFrameSizeResource ) );
        with zglPTextureFrameSizeResource( Resource )^ do
          begin
            tfs.Texture     := Texture;
            tfs.FrameWidth  := FrameWidth;
            tfs.FrameHeight := FrameHeight;
          end;
        item^.Resource := tfs;
      end;
    {$IFDEF USE_SOUND}
    RES_SOUND:
      begin
        zgl_GetMem( Pointer( snd ), SizeOf( zglTSoundResource ) );
        with zglPSoundResource( Resource )^ do
          begin
            snd.FileName   := FileName;
            snd.Memory     := Memory;
            snd.Sound      := Sound;
            snd.FileLoader := FileLoader;
            snd.MemLoader  := MemLoader;
          end;
        item^.Resource := snd;
      end;
    {$ENDIF}
  end;

  if new Then
    begin
      item^.prev      := last;
      item^.next      := nil;
      item^.prev.next := item^;
    end;
  item^.IsFromFile := FromFile;
  item^._type      := _type;
end;

function res_ProcQueue( data : Pointer ) : LongInt;
  var
    item : zglPResourceItem;
begin
  Result := 0;
  resInitialized := TRUE;
  item := nil;
  while appWork do
    begin
      item := resQueue.next;
      while Assigned( item ) do
        begin
          if ( not item.Ready ) and Assigned( item.Resource ) Then
            case item._type of
              RES_TEXTURE:
                with item^, zglPTextureResource( Resource )^ do
                  begin
                    if IsFromFile Then
                      FileLoader( FileName, pData, Width, Height, Format )
                    else
                      begin
                        FileName := 'From Memory';
                        MemLoader( Memory, pData, Width, Height, Format );
                      end;

                    if not Assigned( pData ) Then
                      begin
                        log_Add( 'Unable to load texture: "' + FileName + '"' );

                        FileName := '';
                        FreeMem( Resource );
                        Resource := nil;
                        DEC( resQueueSize );
                      end else
                        begin
                          Texture.Width  := Width;
                          Texture.Height := Height;
                          Texture.Flags  := Flags;
                          Texture.Format := Format;
                          if Texture.Format = TEX_FORMAT_RGBA Then
                            begin
                              if Texture.Flags and TEX_CALCULATE_ALPHA > 0 Then
                                begin
                                  tex_CalcTransparent( pData, TransparentColor, Width, Height );
                                  tex_CalcAlpha( pData, Width, Height );
                                end else
                                  tex_CalcTransparent( pData, TransparentColor, Width, Height );
                            end;
                          tex_CalcFlags( Texture^, pData );
                          tex_CalcTexCoords( Texture^ );
                          Ready := TRUE;
                        end;
                  end;
              RES_TEXTURE_FRAMESIZE:
                with item^, zglPTextureFrameSizeResource( Resource )^ do
                  begin
                    if Assigned( Texture ) Then
                      begin
                        Texture.FramesX := Round( Texture.Width ) div FrameWidth;
                        Texture.FramesY := Round( Texture.Height ) div FrameHeight;
                        if Texture.FramesX = 0 Then Texture.FramesX := 1;
                        if Texture.FramesY = 0 Then Texture.FramesY := 1;
                        tex_CalcTexCoords( Texture^ );
                      end;

                    FreeMem( Resource );
                    Resource := nil;
                    DEC( resQueueSize );
                  end;
              {$IFDEF USE_SOUND}
              RES_SOUND:
                with item^, zglPSoundResource( Resource )^ do
                  begin
                    if IsFromFile Then
                      FileLoader( FileName, Sound.Data, Sound.Size, Format, Sound.Frequency )
                    else
                      begin
                        FileName := 'From Memory';
                        MemLoader( Memory, Sound.Data, Sound.Size, Format, Sound.Frequency );
                      end;

                    if Assigned( Sound.Data ) Then
                      begin
                        snd_Create( Sound^, Format );
                        if IsFromFile Then
                          log_Add( 'Sound loaded: "' + FileName + '"' );
                      end else
                        log_Add( 'Unable to load sound: "' + FileName + '"' );

                    FileName := '';
                    FreeMem( Resource );
                    Resource := nil;
                    DEC( resQueueSize );
                  end;
              {$ENDIF}
            end;

          item := item.next;
        end;

      while ( appWork ) and ( resQueueSize = 0 ) do
        u_Sleep( 10 );
    end;

  resInitialized := FALSE;
  EndThread( 0 );
end;

procedure res_BeginQueue( QueueID : Byte );
begin
  resQueueState := QUEUE_STATE_START;
end;

procedure res_EndQueue;
begin
  resQueueState := QUEUE_STATE_STOP;
end;

function res_GetPercentage( QueueID : Byte ) : Integer;
begin
  Result := resPercentage;
end;

initialization
  resQueue.prev := @resQueue;
  resQueue.next := nil;

end.
