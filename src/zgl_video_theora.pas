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
unit zgl_video_theora;

{$I zgl_config.cfg}

{$IFDEF USE_THEORA_STATIC}
  {$LINKLIB libtheoradec.a}
{$ENDIf}

interface

const
  THEORA_EXTENSIONS : array[ 0..0 ] of UTF8String = ( 'OGV' );

implementation
uses
  zgl_types,
  zgl_main,
  zgl_file,
  zgl_memory,
  zgl_lib_ogg,
  zgl_lib_theora,
  zgl_textures,
  zgl_video,
  zgl_log;

type
  zglPTheoraData = ^zglTTheoraData;
  zglTTheoraData = record
    File_       : zglTFile;
    Memory      : zglTMemory;
    SyncState   : ogg_sync_state;
    StreamState : ogg_stream_state;
    DecoderCtx  : pth_dec_ctx;
    Time        : Double;
  end;

var
  theoraDecoder : zglTVideoDecoder;

function buffer_data( var TheoraData : zglTTheoraData ) : cint;
  var
    buffer : pcchar;
begin
  buffer := ogg_sync_buffer( @TheoraData.SyncState, 4096 );
  if Assigned( TheoraData.Memory.Memory ) Then
    Result := mem_Read( TheoraData.Memory, buffer^, 4096 )
  else
    Result := file_Read( TheoraData.File_, buffer^, 4096 );
  ogg_sync_wrote( @TheoraData.SyncState, Result );
end;

function theora_Open( var TheoraData : zglTTheoraData; var Width, Height : Word; var FrameRate : Single ) : Boolean;
  var
    state     : Boolean;
    gotPacket : cint;
    test      : ogg_stream_state;
    ret       : cint;
    page      : ogg_page;
    packet    : ogg_packet;
    comment   : th_comment;
    info      : th_info;
    setupInfo : pth_setup_info;
    isTheora  : Boolean;
    headerin  : cint;
begin
  TheoraData.Time := 0;

  ogg_sync_init( @TheoraData.SyncState );
  th_comment_init( @comment );
  th_info_init( @info );
  setupInfo := nil;
  isTheora  := FALSE;
  headerin  := 0;

  state := FALSE;
  while not state do
    begin
      if buffer_data( TheoraData ) = 0 Then break;

      while ogg_sync_pageout( @TheoraData.SyncState, @page ) > 0 do
        begin
          if ogg_page_bos( @page ) = 0 Then
            begin
              ogg_stream_pagein( @TheoraData.StreamState, @page );
              state := TRUE;
              break;
            end;

          ogg_stream_init( @test, ogg_page_serialno( @page ) );
          ogg_stream_pagein( @test, @page );
          gotPacket := ogg_stream_packetpeek( @test, @packet );

          headerin := th_decode_headerin( @info, @comment, @setupInfo, @packet );
          if ( gotPacket = 1 ) and ( not isTheora ) and ( headerin >= 0 ) Then
            begin
              TheoraData.StreamState := test;
              isTheora               := TRUE;
              if headerin <> 0 Then
                ogg_stream_packetout( @TheoraData.StreamState, nil );
            end else
              ogg_stream_clear( @test );
        end;
    end;

  while isTheora and ( headerin <> 0 ) do
    begin
      if headerin <> 0 Then
        ret := ogg_stream_packetpeek( @TheoraData.StreamState, @packet );
      while ( headerin <> 0 ) and ( ret <> 0 ) do
        begin
          if ret < 0 Then
            begin
              ret := ogg_stream_packetpeek( @TheoraData.StreamState, @packet );
              continue;
            end;

          headerin := th_decode_headerin( @info, @comment, @setupInfo, @packet );
          if headerin < 0 Then
            begin
              log_Add( 'Theora: Error parsing Theora stream headers' );
              Result := FALSE;
              exit;
            end else
              if headerin > 0 Then
                ogg_stream_packetout( @TheoraData.StreamState, nil );

          ret := ogg_stream_packetpeek( @TheoraData.StreamState, @packet );
        end;

      if not ( isTheora and ( headerin <> 0 ) ) Then break;

      if ogg_sync_pageout( @TheoraData.SyncState, @page ) <= 0 Then
        begin
          if buffer_data( TheoraData ) = 0 Then
            begin
              log_Add( 'Theora: End of file while searching for codec headers' );
              Result := FALSE;
              exit;
            end;
        end else
          ogg_stream_pagein( @TheoraData.StreamState, @page );
    end;

  if isTheora Then
    begin
      TheoraData.DecoderCtx := th_decode_alloc( @info, setupInfo );

      Width     := info.frame_width;
      Height    := info.frame_height;
      FrameRate := info.fps_numerator / info.fps_denominator;

      while ogg_sync_pageout( @TheoraData.SyncState, @page ) > 0 do
        ogg_stream_pagein( @TheoraData.StreamState, @page );

      Result := TRUE;
    end else
      Result := FALSE;

  th_comment_clear( @comment );
  th_info_clear( @info );
  th_setup_free( setupInfo );
end;

function clamp( value : Integer ) : Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  if value > 2088960 Then
    Result := 2088960
  else
    if value < 0 Then
      Result := 0
    else
      Result := value;
end;

function theora_Update( var TheoraData : zglTTheoraData; Time : Double; Data : PByte ) : Integer;
  var
    ycbcr      : th_ycbcr_buffer;
    Y, Cb, Cr  : Integer;
    i, j       : Integer;
    videoReady : Boolean;
    granulePos : ogg_int64_t;
    page       : ogg_page;
    packet     : ogg_packet;
begin
  Result := 0;
  if TheoraData.Time > Time Then exit;

  videoReady := FALSE;
  granulePos := -1;

  while not videoReady do
    if ogg_stream_packetout( @TheoraData.StreamState, @packet ) > 0 Then
      begin
        if th_decode_packetin( TheoraData.DecoderCtx, @packet, @granulePos ) >= 0 Then
          begin
            TheoraData.Time := th_granule_time( TheoraData.DecoderCtx, granulePos );
            videoReady      := Time < TheoraData.Time;
            INC( Result );
          end;
      end else
        break;

  if not videoReady Then
    begin
      if buffer_data( TheoraData ) = 0 Then exit;

      while ogg_sync_pageout( @TheoraData.SyncState, @page ) > 0 do
        ogg_stream_pagein( @TheoraData.StreamState, @page );

      INC( Result, theora_Update( TheoraData, Time, Data ) );
      exit;
    end else
      begin
        th_decode_ycbcr_out( TheoraData.DecoderCtx, ycbcr );

        INC( Data, ( ycbcr[ 0 ].height - 1 ) * ycbcr[ 0 ].width * 4 );
        for j := 0 to ycbcr[ 0 ].height - 1 do
          begin
            for i := 0 to ycbcr[ 0 ].width - 1 do
              begin
                Y  := 9535 * ( PByte( Ptr( ycbcr[ 0 ].data ) + i )^ - 16 );
                Cb := PByte( Ptr( ycbcr[ 1 ].data ) + ( i shr 1 ) )^ - 128;
                Cr := PByte( Ptr( ycbcr[ 2 ].data ) + ( i shr 1 ) )^ - 128;

                PByte( Ptr( Data ) + 0 )^ := clamp( Y + 13074 * Cr ) shr 13;
                PByte( Ptr( Data ) + 1 )^ := clamp( Y - 6660 * Cr - 3203 * Cb ) shr 13;
                PByte( Ptr( Data ) + 2 )^ := clamp( Y + 16531 * Cb ) shr 13;
                INC( Data, 4 );
              end;

            DEC( Data, ycbcr[ 0 ].width * 8 );
            INC( ycbcr[ 0 ].data, ycbcr[ 0 ].stride );

            if j and 1 > 0 Then
              begin
                INC( ycbcr[ 1 ].data, ycbcr[ 1 ].stride );
                INC( ycbcr[ 2 ].data, ycbcr[ 2 ].stride );
              end;
          end;
      end;
end;

function theora_DecoderOpen( var Stream : zglTVideoStream; const FileName : UTF8String ) : Boolean;
begin
  if not Assigned( Stream._private.Data ) Then
    zgl_GetMem( Stream._private.Data, SizeOf( zglTTheoraData ) );

  file_Open( zglTTheoraData( Stream._private.Data^ ).File_, FileName, FOM_OPENR );
  Result := theora_Open( zglTTheoraData( Stream._private.Data^ ), Stream.Info.Width, Stream.Info.Height, Stream.Info.FrameRate );
end;

function theora_DecoderOpenMem( var Stream : zglTVideoStream; const Memory : zglTMemory ) : Boolean;
begin
  if not Assigned( Stream._private.Data ) Then
    zgl_GetMem( Stream._private.Data, SizeOf( zglTTheoraData ) );

  zglTTheoraData( Stream._private.Data^ ).Memory := Memory;
  Result := theora_Open( zglTTheoraData( Stream._private.Data^ ), Stream.Info.Width, Stream.Info.Height, Stream.Info.FrameRate );
end;

procedure theora_DecoderUpdate( var Stream : zglTVideoStream; Time : Double; var Data : Pointer );
begin
  Stream.Time := Stream.Time + Time / 1000;
  INC( Stream.Frame, theora_Update( zglTTheoraData( Stream._private.Data^ ), Stream.Time, Data ) );
end;

procedure theora_DecoderLoop( var Stream : zglTVideoStream );
begin
end;

procedure theora_DecoderClose( var Stream : zglTVideoStream );
begin
  with zglTTheoraData( Stream._private.Data^ ) do
    begin
      ogg_stream_clear( @StreamState );
      th_decode_free( DecoderCtx );
      ogg_sync_clear( @SyncState );

      file_Close( File_ );
    end;

  FreeMem( Stream._private.Data );
end;

initialization
{$IFDEF USE_THEORA}
  theoraDecoder.Extension := THEORA_EXTENSIONS[ 0 ];
  theoraDecoder.Open      := theora_DecoderOpen;
  theoraDecoder.OpenMem   := theora_DecoderOpenMem;
  theoraDecoder.Update    := theora_DecoderUpdate;
  theoraDecoder.Loop      := theora_DecoderLoop;
  theoraDecoder.Close     := theora_DecoderClose;
  zgl_Reg( VIDEO_FORMAT_DECODER, @theoraDecoder );
{$ENDIF}

end.
