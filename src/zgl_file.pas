{
 * Copyright © Kemka Andrey aka Andru
 * mail: dr.andru@gmail.com
 * site: http://andru-kun.inf.ua
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
unit zgl_file;

{$I zgl_config.cfg}

interface

uses
  {$IFDEF LINUX_OR_DARWIN}
  baseunix,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  zgl_types;

{$IFDEF LINUX_OR_DARWIN}
type PFILE = Pointer;
type zglTFile = PFILE;
{$ENDIF}
{$IFDEF WIN32}
type zglTFile = THandle;
{$ENDIF}

type zglTFileList = zglTStringList;

const
  // Open Mode
  FOM_CREATE = $01; // Create
  FOM_OPENR  = $02; // Read
  FOM_OPENRW = $03; // Read&Write

  // Seek Mode
  FSM_SET    = $01;
  FSM_CUR    = $02;
  FSM_END    = $03;

procedure file_Open( var FileHandle : zglTFile; const FileName : String; const Mode : Byte );
function  file_MakeDir( const Directory : String ) : Boolean;
function  file_Exists( const FileName : String ) : Boolean;
function  file_Seek( const FileHandle : zglTFile; const Offset, Mode : DWORD ) : DWORD;
function  file_GetPos( const FileHandle : zglTFile ) : DWORD;
function  file_Read( const FileHandle : zglTFile; var buffer; const count : DWORD ) : DWORD;
function  file_Write( const FileHandle : zglTFile; const buffer; const count : DWORD ) : DWORD;
procedure file_Trunc( const FileHandle : zglTFile; const count : DWORD );
function  file_GetSize( const FileHandle : zglTFile ) : DWORD;
procedure file_Flush( const FileHandle : zglTFile );
procedure file_Close( const FileHandle : zglTFile );
procedure file_Find( const Directory : String; var List : zglTFileList; const FindDir : Boolean );
procedure file_GetName( const FileName : String; var Result : String );
procedure file_GetExtension( const FileName : String; var Result : String );
procedure file_SetPath( const Path : String );

{$IFDEF LINUX_OR_DARWIN}
// "Домо оригато" разработчикам FreePascal, которые принципиально
// не портировали модуль libc на платформу x86_64
const
  { POSIX file modes: group permission...  }
  S_IRWXG = $0038;
  S_IRGRP = $0020;
  S_IWGRP = $0010;
  S_IXGRP = $0008;
  { POSIX file modes: other permission...  }
  S_IRWXO = $0007;
  S_IROTH = $0004;
  S_IWOTH = $0002;
  S_IXOTH = $0001;
  { read/write search permission for everyone }
  MODE_MKDIR = S_IWUSR or S_IRUSR or
               S_IWGRP or S_IRGRP or
               S_IWOTH or S_IROTH or
               S_IXUSR or S_IXGRP or S_IXOTH;

type
  Pdirent = ^dirent;
  dirent  = record
    d_ino    : DWORD;
    d_off    : LongInt;
    d_reclen : WORD;
    d_type   : Byte;
    d_name   : array[ 0..255 ] of AnsiChar;
  end;

type
  TDirEnt       = dirent;
  PPDirEnt      = ^PDirEnt;
  PPPDirEnt     = ^PPDirEnt;
  TSelectorFunc = function(const p1: PDirEnt): Integer; cdecl;
  TCompareFunc  = function(const p1, p2: Pointer): Integer; cdecl;

function fopen(__filename:Pchar; __modes:Pchar):PFILE;cdecl;external 'libc' name 'fopen';
function fclose(__stream:PFILE):longint;cdecl;external 'libc' name 'fclose';
function fflush(__stream:PFILE):longint;cdecl;external 'libc' name 'fflush';
function fread(__ptr:pointer; __size:size_t; __n:size_t; __stream:PFILE):size_t;cdecl;external 'libc' name 'fread';
function fwrite(__ptr:pointer; __size:size_t; __n:size_t; __s:PFILE):size_t;cdecl;external 'libc' name 'fwrite';
function fseek(__stream:PFILE; __off:longint; __whence:longint):longint;cdecl;external 'libc' name 'fseek';
function ftell(__stream:PFILE):longint;cdecl;external 'libc' name 'ftell';
function ftruncate(__fd:longint; __length:LongInt):longint;cdecl;external 'libc' name 'ftruncate';
function access(__name:Pchar; __type:longint):longint;cdecl;external 'libc' name 'access';
function scandir(__dir:Pchar; __namelist:PPPdirent; __selector:TSelectorfunc; __cmp:TComparefunc):longint;cdecl;external 'libc' name 'scandir';
function free( ptr : Pointer ):longint;cdecl;external 'libc' name 'free';
function mkdir(pathname:Pchar; mode:mode_t):longint;cdecl;external 'libc' name 'mkdir';
{$ENDIF}

var
  filePath : String;

implementation

procedure file_Open;
begin
{$IFDEF LINUX_OR_DARWIN}
  case Mode of
    FOM_CREATE: FileHandle := fopen( PChar( filePath + FileName ), 'w' );
    FOM_OPENR:  FileHandle := fopen( PChar( filePath + FileName ), 'r' );
    FOM_OPENRW: FileHandle := fopen( PChar( filePath + FileName ), 'r+' );
  end;
{$ENDIF}
{$IFDEF WIN32}
  case Mode of
    FOM_CREATE: FileHandle := CreateFile( PChar( filePath + FileName ), GENERIC_ALL, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 );
    FOM_OPENR:  FileHandle := CreateFile( PChar( filePath + FileName ), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );
    FOM_OPENRW: FileHandle := CreateFile( PChar( filePath + FileName ), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0 );
  end;
{$ENDIF}
end;

function file_MakeDir;
begin
{$IFDEF LINUX_OR_DARWIN}
  Result := mkdir( PChar( Directory ), MODE_MKDIR ) = 0;
{$ENDIF}
{$IFDEF WIN32}
  Result := CreateDirectory( PChar( Directory ), nil );
{$ENDIF}
end;

function file_Exists;
  {$IFDEF WIN32}
  var
    FileHandle : zglTFile;
  {$ENDIF}
begin
{$IFDEF LINUX_OR_DARWIN}
  Result := not Boolean( access( PChar( filePath + FileName ), F_OK ) );
{$ENDIF}
{$IFDEF WIN32}
  file_Open( FileHandle, filePath + FileName, FOM_OPENR );
  Result := FileHandle <> INVALID_HANDLE_VALUE;
  if Result Then
    file_Close( FileHandle );
{$ENDIF}
end;

function file_Seek;
begin
{$IFDEF LINUX_OR_DARWIN}
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
{$IFDEF LINUX_OR_DARWIN}
  Result := ftell( FileHandle );
{$ENDIF}
{$IFDEF WIN32}
  Result := SetFilePointer( FileHandle, 0, nil, FILE_CURRENT );
{$ENDIF}
end;

function file_Read;
begin
{$IFDEF LINUX_OR_DARWIN}
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
{$IFDEF LINUX_OR_DARWIN}
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
{$IFDEF LINUX_OR_DARWIN}
  ftruncate( DWORD( FileHandle ), count );
{$ENDIF}
end;

function file_GetSize;
  {$IFDEF LINUX_OR_DARWIN}
  var
    tmp : DWORD;
  {$ENDIF}
begin
{$IFDEF LINUX_OR_DARWIN}
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
{$IFDEF LINUX_OR_DARWIN}
  fflush( FileHandle );
{$ENDIF}
{$IFDEF WIN32}
  FlushFileBuffers( FileHandle );
{$ENDIF}
end;

procedure file_Close;
begin
{$IFDEF LINUX_OR_DARWIN}
  fclose( FileHandle );
{$ENDIF}
{$IFDEF WIN32}
  CloseHandle( FileHandle );
{$ENDIF}
end;

{$IFDEF LINUX_OR_DARWIN}
function filter_file(const p1: PDirEnt): Integer; cdecl;
begin
  Result := Byte( p1.d_type = 8 );
end;
function filter_dir(const p1: PDirEnt): Integer; cdecl;
begin
  Result := Byte( p1.d_type = 4 );
end;
{$ENDIF}

procedure file_Find;
  {$IFDEF LINUX_OR_DARWIN}
  var
    FList : array of Pdirent;
    i     : Integer;
  {$ENDIF}
  {$IFDEF WIN32}
  var
    First : THandle;
    FList : {$IFDEF FPC} WIN32FINDDATAA {$ELSE} WIN32_FIND_DATA {$ENDIF};
  {$ENDIF}
begin
{$IFDEF LINUX_OR_DARWIN}
  if FindDir Then
    List.Count := scandir( PChar( Directory ), @FList, filter_dir, nil )
  else
    List.Count := scandir( PChar( Directory ), @FList, filter_file, nil );
  if List.Count <> -1 Then
    begin
      SetLength( List.Items, List.Count );
      for i := 0 to List.Count - 1 do
        begin
          List.Items[ i ] := String( FList[ i ].d_name );
          Free( FList[ i ] );
        end;
      SetLength( FList, 0 );
    end;
{$ENDIF}
{$IFDEF WIN32}
  First := FindFirstFile( PChar( Directory ), FList );
  repeat
    if FindDir Then
      begin
        if FList.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 Then continue;
      end else
        if FList.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY > 0 Then continue;
    SetLength( List.Items, List.Count + 1 );
    List.Items[ List.Count ] := FList.cFileName;
    INC( List.Count );
  until not FindNextFile( First, FList );
{$ENDIF}
end;

procedure GetStr( const Str : String; var Result : String; const d : Char );
  var
    i, pos, l : Integer;
begin
  pos := 0;
  l := length( Str );
  for i := l downto 1 do
    if Str[ i ] = d Then
      begin
        pos := i;
        break;
      end;
  Result := copy( Str, l - ( l - pos ) + 1, ( l - pos ) );
end;

procedure file_GetName;
  var
    tmp : String;
begin
  GetStr( FileName, Result, '/' );
  {$IFDEF WIN32}
  if Result = '' Then
    GetStr( FileName, Result, '\' );
  {$ENDIF}
  GetStr( Result, tmp, '.' );
  Result := copy( Result, 1, length( Result ) - length( tmp ) - 1 );
end;

procedure file_GetExtension;
begin
  GetStr( FileName, Result, '.' );
end;

procedure file_SetPath;
begin
  filePath := Path;
end;

end.
