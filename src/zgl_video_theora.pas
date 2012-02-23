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
  zglTTheoraData = record
    SyncState   : ogg_sync_state;
    StreamState : ogg_stream_state;
    DecoderCtx  : pth_dec_ctx;
    Time        : Double;
    YOffset     : cint;
  end;

var
  theoraDecoder : zglTVideoDecoder;

  oggFile        : zglTFile;
  oggSyncState   : ogg_sync_state;
  oggStreamState : ogg_stream_state;
  theoraDecCtx   : pth_dec_ctx;
  videobuf_time  : double;
  y_offset       : cint;

function buffer_data( var file_ : zglTFile; oy : pogg_sync_state ) : cint;
  var
    buffer : pcchar;
begin
  buffer := ogg_sync_buffer( oy, 4096 );
  Result := file_Read( file_, buffer^, 4096 );
  ogg_sync_wrote( oy, Result );
end;

function theora_DecoderOpen( var Stream : zglTVideoStream; const FileName : UTF8String ) : Boolean;
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
  file_Open( oggFile, FileName, FOM_OPENR );

  ogg_sync_init( @oggSyncState );
  th_comment_init( @comment );
  th_info_init( @info );
  setupInfo := nil;

  isTheora := FALSE;
  headerin := 0;
  videobuf_time := 0;

  state := FALSE;
  while not state do
    begin
      if buffer_data( oggFile, @oggSyncState ) = 0 Then break;

      while ogg_sync_pageout( @oggSyncState, @page ) > 0 do
        begin
          if ogg_page_bos( @page ) = 0 Then
            begin
              ogg_stream_pagein( @oggStreamState, @page );
              state := TRUE;
              break;
            end;

          ogg_stream_init( @test, ogg_page_serialno( @page ) );
          ogg_stream_pagein( @test, @page );
          gotPacket := ogg_stream_packetpeek( @test, @packet );

          headerin := th_decode_headerin( @info, @comment, @setupInfo, @packet );
          if ( gotPacket = 1 ) and ( not isTheora ) and ( headerin >= 0 ) Then
            begin
              oggStreamState := test;
              isTheora       := TRUE;
              if headerin <> 0 Then
                ogg_stream_packetout( @oggStreamState, nil );
            end else
              ogg_stream_clear( @test );
        end;
    end;

  while isTheora and ( headerin <> 0 ) do
    begin
      if headerin <> 0 Then
        ret := ogg_stream_packetpeek( @oggStreamState, @packet );
      while ( headerin <> 0 ) and ( ret <> 0 ) do
        begin
          if ret < 0 Then
            begin
              ret := ogg_stream_packetpeek( @oggStreamState, @packet );
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
                ogg_stream_packetout( @oggStreamState, nil );

          ret := ogg_stream_packetpeek( @oggStreamState, @packet );
        end;

      if not ( isTheora and ( headerin <> 0 ) ) Then break;

      if ogg_sync_pageout( @oggSyncState, @page ) <= 0 Then
        begin
          if buffer_data( oggFile, @oggSyncState ) = 0 Then
            begin
              log_Add( 'Theora: End of file while searching for codec headers' );
              Result := FALSE;
              exit;
            end;
        end else
          ogg_stream_pagein( @oggStreamState, @page );
    end;

  if isTheora Then
    begin
      theoraDecCtx          := th_decode_alloc( @info, setupInfo );
      Stream.Info.Width     := info.frame_width;
      Stream.Info.Height    := info.frame_height;
      Stream.Info.FrameRate := info.fps_numerator / info.fps_denominator;

      y_offset := info.pic_x + info.frame_width * info.pic_y;

      while ogg_sync_pageout( @oggSyncState, @page ) > 0 do
        ogg_stream_pagein( @oggStreamState, @page );

      Result := TRUE;
    end else
      Result := FALSE;

  th_comment_clear( @comment );
  th_info_clear( @info );
  th_setup_free( setupInfo );
end;

function theora_DecoderOpenMem( var Stream : zglTVideoStream; const Memory : zglTMemory ) : Boolean;
begin
  Result := FALSE;
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

function theora_Update( Time : Double; Data : PByte ) : Integer;
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
  if videobuf_time > Time Then exit;

  videoReady := FALSE;
  granulePos := -1;

  while not videoReady do
    if ogg_stream_packetout( @oggStreamState, @packet ) > 0 Then
      begin
        if th_decode_packetin( theoraDecCtx, @packet, @granulePos ) >= 0 Then
          begin
            videobuf_time := th_granule_time( theoraDecCtx, granulePos );
            videoReady    := Time < videobuf_time;
            INC( Result );
          end;
      end else
        break;

  if ( not videoReady ) and ( file_GetPos( oggFile ) = file_GetSize( oggFile ) ) Then exit;

  if not videoReady Then
    begin
      buffer_data( oggFile, @oggSyncState );
      while ogg_sync_pageout( @oggSyncState, @page ) > 0 do
        ogg_stream_pagein( @oggStreamState, @page );

      Result := theora_Update( Time, Data );
      exit;
    end else
      begin
        th_decode_ycbcr_out( theoraDecCtx, ycbcr );

        INC( Data, ( ycbcr[ 0 ].height - 1 ) * ycbcr[ 0 ].width * 4 );
        INC( ycbcr[ 0 ].data, y_offset );
        INC( ycbcr[ 0 ].data, y_offset div 2 );
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
        DEC( Data, ( ycbcr[ 0 ].height - 1 ) * ycbcr[ 0 ].width * 4 );
      end;
end;

procedure theora_DecoderUpdate( var Stream : zglTVideoStream; Time : Double; var Data : Pointer );
begin
  Stream.Time := Stream.Time + Time / 1000;
  INC( Stream.Frame, theora_Update( Stream.Time, Data ) );
end;

procedure theora_DecoderLoop( var Stream : zglTVideoStream );
begin
end;

procedure theora_DecoderClose( var Stream : zglTVideoStream );
begin
  ogg_stream_clear( @oggStreamState );
  th_decode_free( theoraDecCtx );
  theoraDecCtx := nil;
  ogg_sync_clear( @oggSyncState );

  file_Close( oggFile );
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
