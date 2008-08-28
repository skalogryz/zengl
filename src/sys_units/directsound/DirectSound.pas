{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  Files:      dsound.h                                                      *}
{*  Content:    DirectSound include file                                      *}
{*                                                                            *}
{******************************************************************************}
// Кастр^W урезанная версия хедера by Kemka Andrey aka Andru :)
unit DirectSound;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
uses
  Windows;

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

type
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


function  LoadDirectSound : Boolean;
procedure FreeDirectSound;

var
  DSound            : HMODULE;
  DirectSoundCreate : function (lpGuid: PGUID; out ppDS: IDirectSound; pUnkOuter: IUnknown): HResult; stdcall;

implementation

function LoadDirectSound;
begin
  Result            := FALSE;
  DSound            := LoadLibrary( 'DSound.dll' );
  DirectSoundCreate := GetProcAddress( DSound, 'DirectSoundCreate' );
  Result            := DSound <> 0;
end;

procedure FreeDirectSound;
begin
  FreeLibrary( DSound );
end;

end.
