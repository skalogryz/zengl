{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.ru
 *
 * This file is part of ZenGL
 *
 * ZenGL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * ZenGL is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_sound_dsound;

{$I zgl_config.cfg}

interface
uses
  Windows,
  math;

const
  DS_OK                       = $00000000;
  DSSCL_PRIORITY              = $00000002;

  DSBCAPS_PRIMARYBUFFER       = $00000001;
  DSBCAPS_STATIC              = $00000002;

  DSBCAPS_CTRLFREQUENCY       = $00000020;
  DSBCAPS_CTRLPAN             = $00000040;
  DSBCAPS_CTRLVOLUME          = $00000080;
  DSBCAPS_GETCURRENTPOSITION2 = $00010000;

  DSFX_LOCHARDWARE            = $00000001;
  DSFX_LOCSOFTWARE            = $00000002;

  DSBSTATUS_PLAYING           = $00000001;
  DSBSTATUS_BUFFERLOST        = $00000002;

  DSBPLAY_LOOPING             = $00000001;

  DSERR_BUFFERLOST            = $88780000 + 150;

type
  zglTBufferDesc = record
    FormatCode       : Word;
    ChannelNumber    : Word;
    SampleRate       : DWORD;
    BytesPerSecond   : DWORD;
    BytesPerSample   : Word;
    BitsPerSample    : Word;
    cbSize           : Word;
  end;


  IDirectSoundBuffer = interface;
  IDirectSound       = interface;

  TDSBUFFERDESC = packed record
    dwSize          : DWORD;
    dwFlags         : DWORD;
    dwBufferBytes   : DWORD;
    dwReserved      : DWORD;
    lpwfxFormat     : Pointer;
    guid3DAlgorithm : TGUID;
  end;

  IDirectSound = interface (IUnknown)
    ['{279AFA83-4981-11CE-A521-0020AF0BE560}']
    function CreateSoundBuffer(const lpDSBufferDesc: TDSBufferDesc;
        out lpIDirectSoundBuffer: IDirectSoundBuffer;
        pUnkOuter: IUnknown) : HResult; stdcall;
    function GetCaps(lpDSCaps: Pointer) : HResult; stdcall;
    function DuplicateSoundBuffer(lpDsbOriginal: IDirectSoundBuffer;
        out lpDsbDuplicate: IDirectSoundBuffer) : HResult; stdcall;
    function SetCooperativeLevel(hwnd: HWND; dwLevel: DWORD) : HResult; stdcall;
    function Compact: HResult; stdcall;
    function GetSpeakerConfig(var lpdwSpeakerConfig: DWORD) : HResult; stdcall;
    function SetSpeakerConfig(dwSpeakerConfig: DWORD) : HResult; stdcall;
    function Initialize(lpGuid: PGUID) : HResult; stdcall;
  end;

  IDirectSoundBuffer = interface (IUnknown)
    ['{279AFA85-4981-11CE-A521-0020AF0BE560}']
    function GetCaps(lpDSCaps: Pointer) : HResult; stdcall;
    function GetCurrentPosition
        (lpdwPlayPosition, lpdwReadPosition : PDWORD) : HResult; stdcall;
    function GetFormat(lpwfxFormat: Pointer; dwSizeAllocated: DWORD;
        lpdwSizeWritten: PDWORD) : HResult; stdcall;
    function GetVolume(var lplVolume: integer) : HResult; stdcall;
    function GetPan(var lplPan: integer) : HResult; stdcall;
    function GetFrequency(var lpdwFrequency: DWORD) : HResult; stdcall;
    function GetStatus(var lpdwStatus: DWORD) : HResult; stdcall;
    function Initialize(lpDirectSound: IDirectSound;
        const lpcDSBufferDesc: TDSBufferDesc) : HResult; stdcall;
    function Lock(dwWriteCursor, dwWriteBytes: DWORD;
        var lplpvAudioPtr1: Pointer; var lpdwAudioBytes1: DWORD;
        var lplpvAudioPtr2: Pointer; var lpdwAudioBytes2: DWORD;
        dwFlags: DWORD) : HResult; stdcall;
    function Play(dwReserved1,dwReserved2,dwFlags: DWORD) : HResult; stdcall;
    function SetCurrentPosition(dwPosition: DWORD) : HResult; stdcall;
    function SetFormat(lpcfxFormat: Pointer) : HResult; stdcall;
    function SetVolume(lVolume: integer) : HResult; stdcall;
    function SetPan(lPan: integer) : HResult; stdcall;
    function SetFrequency(dwFrequency: DWORD) : HResult; stdcall;
    function Stop: HResult; stdcall;
    function Unlock(lpvAudioPtr1: Pointer; dwAudioBytes1: DWORD;
        lpvAudioPtr2: Pointer; dwAudioBytes2: DWORD) : HResult; stdcall;
    function Restore: HResult; stdcall;
  end;

function  InitDSound : Boolean;
procedure FreeDSound;

function  dsu_CreateBuffer( BufferSize : DWORD; Format : Pointer ) : IDirectSoundBuffer;
procedure dsu_FillData( var Buffer : IDirectSoundBuffer; Data : Pointer; const DataSize : DWORD; const Pos : DWORD = 0 );
function  dsu_CalcPos( const X, Y, Z : Single; var Volume : Single ) : Integer;
function  dsu_CalcVolume( const Volume : Single ) : Integer;

var
  dsound_Library    : HMODULE;
  DirectSoundCreate : function (lpGuid: PGUID; out ppDS: IDirectSound; pUnkOuter: IUnknown): HResult; stdcall;

  ds_Device      : IDirectSound;
  ds_Position    : array[ 0..2 ] of Single;
  ds_Plane       : array[ 0..2 ] of Single;
  ds_Orientation : array[ 0..5 ] of Single = ( 0.0, 0.0, -1.0, 0.0, 1.0, 0.0 );

implementation
uses
  zgl_types,
  zgl_sound,
  zgl_log,
  zgl_utils;

function InitDSound;
begin
  Result            := FALSE;
  dsound_Library    := dlopen( 'DSound.dll' );
  DirectSoundCreate := dlsym( dsound_Library, 'DirectSoundCreate' );
  Result            := dsound_Library <> 0;
end;

procedure FreeDSound;
begin
  dlclose( dsound_Library );
end;

function dsu_CreateBuffer;
  var
    DSoundBD : TDSBufferDesc;
begin
  FillChar( DSoundBD, SizeOf( TDSBUFFERDESC ), 0 );
  DSoundBD.dwSize  := SizeOf( TDSBUFFERDESC );
  DSoundBD.dwFlags := DSBCAPS_STATIC        +
                      DSBCAPS_CTRLPAN       +
                      DSBCAPS_CTRLVOLUME    +
                      DSBCAPS_CTRLFREQUENCY +
                      DSBCAPS_GETCURRENTPOSITION2;
  DSoundBD.dwBufferBytes := BufferSize;
  DSoundBD.lpwfxFormat   := Format;

  ds_Device.CreateSoundBuffer( DSoundBD, Result, nil );
end;

procedure dsu_FillData;
  var
    Block1, Block2 : Pointer;
    b1Size, b2Size : DWORD;
begin
  Buffer.Lock( Pos, DataSize, Block1, b1Size, Block2, b2Size, 0 );
  Move( Data^, Block1^, b1Size );
  if b2Size <> 0 Then Move( Pointer( Ptr( Data ) + b1Size )^, Block2^, b2Size );
  Buffer.Unlock( Block1, b1Size, Block2, b2Size );
end;

function dsu_CalcPos;
  var
    dist, angle : Single;
begin
  ds_Plane[ 0 ] := ds_Orientation[ 1 ] * ds_Orientation[ 5 ] - ds_Orientation[ 2 ] * ds_Orientation[ 4 ];
  ds_Plane[ 1 ] := ds_Orientation[ 2 ] * ds_Orientation[ 3 ] - ds_Orientation[ 0 ] * ds_Orientation[ 5 ];
  ds_Plane[ 2 ] := ds_Orientation[ 0 ] * ds_Orientation[ 4 ] - ds_Orientation[ 1 ] * ds_Orientation[ 3 ];

  dist := sqrt( sqr( X - ds_Position[ 0 ] ) + sqr( Y - ds_Position[ 1 ] ) + sqr( Z - ds_Position[ 2 ] ) );
  if dist = 0 then
    angle := 0
  else
    angle := ( ds_Plane[ 0 ] * ( X - ds_Position[ 0 ] ) +
               ds_Plane[ 1 ] * ( Y - ds_Position[ 1 ] ) +
               ds_Plane[ 2 ] * ( Z - ds_Position[ 2 ] ) ) / dist;
  Result := Trunc( 10000 * angle * 0.1 );
  if Result < -10000 Then Result := -10000;
  if Result > 10000  Then Result := 10000;

  Volume := sndVolume / dist;
  if Volume < 0 Then Volume := 0;
end;

function dsu_CalcVolume;
begin
  if volume = 0 Then
    Result := -10000
  else
    Result := - Round( 1000 * ln( 1 / volume ) );
end;

end.
