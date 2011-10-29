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
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  zgl_memory,
  zgl_textures,
  {$IFDEF USE_SOUND}
  zgl_sound,
  {$ENDIF}
  zgl_types;

const
  RES_TEXTURE           = $000001;
  RES_TEXTURE_FRAMESIZE = $000002;
  RES_TEXTURE_MASK      = $000003;
  RES_TEXTURE_DELETE    = $000004;
  RES_SOUND             = $000010;
  RES_SOUND_DELETE      = $000011;

type
  zglPResourceItem = ^zglTResourceItem;
  zglTResourceItem = record
    _type      : Integer;
    IsFromFile : Boolean;
    Ready      : Boolean;
    Prepared   : Boolean;
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

type
  zglPTextureMaskResource = ^zglTTextureMaskResource;
  zglTTextureMaskResource = record
    Texture : zglPTexture;
    Mask    : zglPTexture;
    tData   : Pointer;
    mData   : Pointer;
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
function  res_GetCompleted : Integer;

var
  resUseThreaded     : Boolean;
  resCompleted       : Integer;
  resThread          : array[ 0..255 ] of LongWord;
  {$IFNDEF FPC}
  resThreadID        : array[ 0..255 ] of LongWord;
  {$ENDIF}
  resQueueStackID    : array of Byte;
  resQueueID         : array[ 0..255 ] of Byte;
  resQueueCurrentID  : Byte;
  resQueueState      : array[ 0..255 ] of {$IFDEF FPC} PRTLEvent {$ELSE} THandle {$ENDIF};
  resQueueSize       : array[ 0..255 ] of Integer;
  resQueueMax        : array[ 0..255 ] of Integer;
  resQueuePercentage : array[ 0..255 ] of Integer;
  resQueueItems      : array[ 0..255 ] of zglTResourceItem;

implementation
uses
  zgl_main,
  zgl_window,
  zgl_screen,
  zgl_application,
  zgl_log,
  zgl_utils;

const
  EVENT_STATE_NULL = {$IFDEF FPC} nil {$ELSE} 0 {$ENDIF};

procedure res_Init;
begin
end;

procedure res_Free;
  var
    i : Integer;
begin
  for i := 0 to 255 do
    if resQueueState[ i ] <> EVENT_STATE_NULL Then
      begin
        {$IFDEF FPC}
        RTLEventSetEvent( resQueueState[ i ] );
        {$ELSE}
        SetEvent( resQueueState[ i ] );
        {$ENDIF}
        resQueueSize[ i ] := 0;
        while resQueueState[ i ] <> EVENT_STATE_NULL do;
      end;
end;

procedure res_Proc;
  var
    item : zglPResourceItem;
    id   : Integer;
    size : Integer;
    max  : Integer;
begin
  size := 0;
  max  := 0;
  for id := 0 to 255 do
    if resQueueState[ id ] <> EVENT_STATE_NULL Then
      begin
        if resQueueSize[ id ] <= 0 Then continue;

        item := resQueueItems[ id ].next;
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
                      DEC( resQueueSize[ id ] );
                      break;
                    end;
                RES_TEXTURE_MASK:
                  with zglPTextureMaskResource( item.Resource )^ do
                    begin
                      tex_SetData( Texture, tData, 0, 0, Texture.Width, Texture.Height );
                      FreeMem( tData );
                      FreeMem( mData );

                      FreeMem( item.Resource );
                      item.Resource := nil;
                      DEC( resQueueSize[ id ] );
                      break;
                    end;
              end;
            if ( not item.Prepared ) and Assigned( item.Resource ) Then
              case item._type of
                RES_TEXTURE_MASK:
                  with zglPTextureMaskResource( item.Resource )^ do
                    begin
                      tex_GetData( Texture, tData );
                      tex_GetData( Mask, mData );
                      item.Prepared := TRUE;

                      {$IFDEF FPC}
                      RTLEventSetEvent( resQueueState[ id ] );
                      {$ELSE}
                      SetEvent( resQueueState[ id ] );
                      {$ENDIF}

                      break;
                    end;
              end;

            if ( resQueueSize[ id ] = 0 ) or ( not item.Ready ) Then
              break
            else
              item := item.next;
          end;

        INC( size, resQueueSize[ id ] );
        INC( max, resQueueMax[ id ] );
        if resQueueSize[ id ] = 0 Then
          begin
            resQueuePercentage[ id ] := 100;
            resQueueMax[ id ]        := 0;
          end else
            resQueuePercentage[ id ] := Round( ( 1 - resQueueSize[ id ] / resQueueMax[ id ] ) * 100 );
      end;

  if size = 0 Then
    resCompleted := 100
  else
    resCompleted := Round( ( 1 - size / max ) * 100 );
end;

procedure res_AddToQueue( _type : Integer; FromFile : Boolean; Resource : Pointer );
  var
    item : ^zglPResourceItem;
    last : zglPResourceItem;
    new  : Boolean;
    tex  : zglPTextureResource;
    tfs  : zglPTextureFrameSizeResource;
    tm   : zglPTextureMaskResource;
    {$IFDEF USE_SOUND}
    snd  : zglPSoundResource;
    {$ENDIF}
begin
  new  := TRUE;
  item := @resQueueItems[ resQueueCurrentID ].next;
  last := @resQueueItems[ resQueueCurrentID ];
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

  INC( resQueueSize[ resQueueCurrentID ] );
  INC( resQueueMax[ resQueueCurrentID ] );

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
    RES_TEXTURE_MASK:
      begin
        zgl_GetMem( Pointer( tm ), SizeOf( zglTTextureMaskResource ) );
        with zglPTextureMaskResource( Resource )^ do
          begin
            tm.Texture := Texture;
            tm.Mask    := Mask;
          end;
        item^.Resource := tm;
      end;
    RES_TEXTURE_DELETE:
      begin
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
    RES_SOUND_DELETE:
      begin
      end;
    {$ENDIF}
  end;

  if new Then
    begin
      item^.prev      := last;
      item^.next      := nil;
      item^.prev.next := item^;
    end;
  item^.Prepared   := FALSE;
  item^.Ready      := FALSE;
  item^.IsFromFile := FromFile;
  item^._type      := _type;

  {$IFDEF FPC}
  RTLEventSetEvent( resQueueState[ resQueueCurrentID ] );
  {$ELSE}
  SetEvent( resQueueState[ resQueueCurrentID ] );
  {$ENDIF}
end;

function res_ProcQueue( data : Pointer ) : LongInt;
  var
    id   : Byte;
    item : zglPResourceItem;
    // mask
    i, j, mW, rW : Integer;
begin
  Result := 0;
  id     := PByte( data )^;
  item   := nil;
  while appWork do
    begin
      item := resQueueItems[ id ].next;
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
                        DEC( resQueueSize[ id ] );
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
                    Ready := TRUE;
                    DEC( resQueueSize[ id ] );
                  end;
              RES_TEXTURE_MASK:
                if item.Prepared Then
                  with item^, zglPTextureMaskResource( Resource )^ do
                    begin
                      if ( Texture.Width <> Mask.Width ) or ( Texture.Height <> Mask.Height ) or ( Texture.Format <> TEX_FORMAT_RGBA ) or ( Mask.Format <> TEX_FORMAT_RGBA ) Then
                        begin
                          FreeMem( Resource );
                          Resource := nil;
                          DEC( resQueueSize[ id ] );
                        end;

                      rW := Round( Texture.Width / Texture.U );
                      mW := Round( Mask.Width / Mask.U );

                      for j := 0 to Texture.Height - 1 do
                        begin
                          for i := 0 to Texture.Width - 1 do
                            PByte( Ptr( tData ) + i * 4 + 3 )^ := PByte( Ptr( mData ) + i * 4 )^;
                          INC( tData, rW * 4 );
                          INC( mData, mW * 4 );
                        end;
                      DEC( tData, rW * Texture.Height * 4 );
                      DEC( mData, mW * Mask.Height * 4 );

                      Ready := TRUE;
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
                    Ready := TRUE;
                    DEC( resQueueSize[ id ] );
                  end;
              {$ENDIF}
            end;

          item := item.next;
        end;

      {$IFDEF FPC}
      RTLEventWaitFor( resQueueState[ id ] );
      {$ELSE}
      WaitForSingleObject( resQueueState[ id ], INFINITE );
      {$ENDIF}
    end;

  {$IFDEF FPC}
  RTLEventDestroy( resQueueState[ id ] );
  {$ELSE}
  CloseHandle( resQueueState[ id ] );
  {$ENDIF}
  resQueueState[ id ] := EVENT_STATE_NULL;

  EndThread( 0 );
end;

procedure res_BeginQueue( QueueID : Byte );
begin
  if resQueueState[ QueueID ] = EVENT_STATE_NULL Then
    begin
      resQueueID[ QueueID ]         := QueueID;
      resQueueItems[ QueueID ].prev := @resQueueItems[ QueueID ];
      resQueueItems[ QueueID ].next := nil;
      {$IFDEF FPC}
      resQueueState[ QueueID ] := RTLEventCreate();
      resThread[ QueueID ]     := LongWord( BeginThread( @res_ProcQueue, @resQueueID[ QueueID ] ) );
      {$ELSE}
      resQueueState[ QueueID ] := CreateEvent( nil, TRUE, FALSE, nil );
      resThread[ QueueID ]     := BeginThread( nil, 0, @res_ProcQueue, @resQueueID[ QueueID ], 0, resThreadID[ QueueID ] );
      {$ENDIF}
    end;

  SetLength( resQueueStackID, Length( resQueueStackID ) + 1 );
  resQueueStackID[ Length( resQueueStackID ) - 1 ] := QueueID;

  resQueueCurrentID := QueueID;
  resUseThreaded := TRUE;
end;

procedure res_EndQueue;
begin
  if Length( resQueueStackID ) > 0 Then
    begin
      resQueueCurrentID := resQueueStackID[ Length( resQueueStackID ) - 1 ];
      SetLength( resQueueStackID, Length( resQueueStackID ) - 1 );
      if Length( resQueueStackID ) > 0 Then
        exit;
    end;

  resUseThreaded := FALSE;
end;

function res_GetPercentage( QueueID : Byte ) : Integer;
begin
  Result := resQueuePercentage[ QueueID ];
end;

function res_GetCompleted : Integer;
begin
  Result := resCompleted;
end;

end.
