{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru.2x4.ru
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
}
unit zgl_file;

{$I define.inc}

interface

uses
  {$IFDEF LINUX}
  libc
  {$ENDIF}
  {$IFDEF WIN32}
  Windows
  {$ENDIF}
  ;
  
{$IFDEF LINUX}
type zglTFile = PFILE;
{$ENDIF}
{$IFDEF WIN32}
type zglTFile = HANDLE;
{$ENDIF}

const
  // Open Mode
  FOM_CREATE = $01; // Create
  FOM_OPENR  = $02; // Read
  FOM_OPENRW = $03; // Read&Write
  
  // Seek Mode
  FSM_SET    = $01;
  FSM_CUR    = $02;
  FSM_END    = $03;
  
procedure file_Open( var FileHandle : zglTFile; FileName : PChar; Mode : Byte ); extdecl;
function  file_Exists( FileName : PChar ) : Boolean; extdecl;
function  file_Seek( FileHandle : zglTFile; Offset : DWORD; Mode : Byte ) : DWORD; extdecl;
function  file_GetPos( FileHandle : zglTFile ) : DWORD; extdecl;
function  file_Read( FileHandle : zglTFile; var buffer; count : DWORD ) : DWORD; extdecl;
function  file_Write( FileHandle : zglTFile; const buffer; count : DWORD ) : DWORD; extdecl;
procedure file_Trunc( FileHandle : zglTFile; count : DWORD ); extdecl;
function  file_GetSize( FileHandle : zglTFile ) : DWORD; extdecl;
procedure file_Flush( FileHandle : zglTFile ); extdecl;
procedure file_Close( FileHandle : zglTFile ); extdecl;

implementation

procedure file_Open;
begin
  {$IFDEF LINUX}
  case Mode of
    FOM_CREATE: FileHandle := fopen( FileName, 'w' );
    FOM_OPENR:  FileHandle := fopen( FileName, 'r' );
    FOM_OPENRW: FileHandle := fopen( FileName, 'r+' );
  end;
  {$ENDIF}
  {$IFDEF WIN32}
  case Mode of
    FOM_CREATE: FileHandle := CreateFile( FileName, GENERIC_ALL, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 );
    FOM_OPENR:  FileHandle := CreateFile( FileName, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );
    FOM_OPENRW: FileHandle := CreateFile( FileName, GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0 );
  end;
  {$ENDIF}
end;

function file_Exists;
  var
    FileHandle : zglTFile;
begin
  {$IFDEF LINUX}
  Result := not Boolean( access( FileName, F_OK ) );
  {$ENDIF}
  {$IFDEF WIN32}
  file_Open( FileHandle, FileName, FOM_OPENR );
  Result := FileHandle <> INVALID_HANDLE_VALUE;
  if Result Then
    file_Close( FileHandle );
  {$ENDIF}
end;

function file_Seek;
begin
  {$IFDEF LINUX}
  case Mode of
    FSM_SET: Result := fseek( FileHandle, Offset, SEEK_SET );
    FSM_CUR: Result := fseek( FileHandle, Offset, SEEK_CUR );
    FSM_END: Result := fseek( FileHandle, Offset, SEEK_END );
  end;
  {$ENDIF}
  {$IFDEF WIN32}
  case Mode of
    FSM_SET: Result := SetFilePointer( FileHandle, Offset, nil, FILE_BEGIN );
    FSM_CUR: Result := SetFilePointer( FileHandle, Offset, nil, FILE_CURRENT );
    FSM_END: Result := SetFilePointer( FileHandle, Offset, nil, FILE_END );
  end;
  {$ENDIF}
end;

function file_GetPos;
begin
  {$IFDEF LINUX}
  Result := ftell( FileHandle );
  {$ENDIF}
  {$IFDEF WIN32}
  Result := SetFilePointer( FileHandle, 0, nil, FILE_CURRENT );
  {$ENDIF}
end;

function file_Read;
begin
  {$IFDEF LINUX}
  Result := ftell( FileHandle );
  if Result + count > file_GetSize( FileHandle ) Then
    Result := file_GetSize( FileHandle ) - Result
  else
    Result := count;
  fread( @buffer, count, 1, FileHandle );
  {$ENDIF}
  {$IFDEF WIN32}
  ReadFile( FileHandle, buffer, count, Result, nil );
  {$ENDIF}
end;

function file_Write;
begin
  {$IFDEF LINUX}
  Result := ftell( FileHandle );
  if Result + count > file_GetSize( FileHandle ) Then
    Result := file_GetSize( FileHandle ) - Result
  else
    Result := count;
  fwrite( @buffer, count, 1, FileHandle );
  {$ENDIF}
  {$IFDEF WIN32}
  WriteFile( FileHandle, buffer, count, Result, nil );
  {$ENDIF}
end;

procedure file_Trunc;
begin
  {$IFDEF LINUX}
  ftruncate( DWORD( FileHandle ), count );
  {$ENDIF}
end;

function file_GetSize;
  var
    tmp : DWORD;
begin
  {$IFDEF LINUX}
  // Весьма безумная реализация 8)
  tmp := ftell( FileHandle );
  fseek( FileHandle, 0, SEEK_END );
  Result := ftell( FileHandle );
  fseek( FileHandle, tmp, SEEK_SET );
  {$ENDIF}
  {$IFDEF WIN32}
  Result := GetFileSize( FileHandle, nil );
  {$ENDIF}
end;

procedure file_Flush;
begin
  {$IFDEF LINUX}
  fflush( FileHandle );
  {$ENDIF}
  {$IFDEF WIN32}
  FlushFileBuffers( FileHandle );
  {$ENDIF}
end;

procedure file_Close;
begin
  {$IFDEF LINUX}
  fclose( FileHandle );
  {$ENDIF}
  {$IFDEF WIN32}
  CloseHandle( FileHandle );
  {$ENDIF}
end;

end.
