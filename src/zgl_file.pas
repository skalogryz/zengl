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
unit zgl_file;

{$I zgl_config.cfg}
{$IFDEF iOS}
  {$modeswitch objectivec1}
{$ENDIF}

interface
uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF iOS}
  iPhoneAll, CFBase, CFString,
  {$ENDIF}
  {$IFDEF USE_ZIP}
  zgl_lib_zip,
  {$ENDIF}
  zgl_types;

{$IFDEF UNIX}
type zglTFile = Ptr;
{$ENDIF}
{$IFDEF WINDOWS}
type zglTFile = THandle;
{$ENDIF}

type zglTFileList = zglTStringList;

const
  FILE_ERROR = {$IFNDEF WINDOWS} 0 {$ELSE} Ptr( -1 ) {$ENDIF};

  // Open Mode
  FOM_CREATE = $01; // Create
  FOM_OPENR  = $02; // Read
  FOM_OPENRW = $03; // Read&Write

  // Seek Mode
  FSM_SET    = $01;
  FSM_CUR    = $02;
  FSM_END    = $03;

function  file_Open( var FileHandle : zglTFile; const FileName : String; Mode : Byte ) : Boolean;
function  file_MakeDir( const Directory : String ) : Boolean;
function  file_Remove( const Name : String ) : Boolean;
function  file_Exists( const Name : String ) : Boolean;
function  file_Seek( FileHandle : zglTFile; Offset, Mode : Integer ) : LongWord;
function  file_GetPos( FileHandle : zglTFile ) : LongWord;
function  file_Read( FileHandle : zglTFile; var Buffer; Bytes : LongWord ) : LongWord;
function  file_Write( FileHandle : zglTFile; const Buffer; Bytes : LongWord ) : LongWord;
function  file_GetSize( FileHandle : zglTFile ) : LongWord;
procedure file_Flush( FileHandle : zglTFile );
procedure file_Close( var FileHandle : zglTFile );
procedure file_Find( const Directory : String; var List : zglTFileList; FindDir : Boolean );
function  file_GetName( const FileName : String ) : String;
function  file_GetExtension( const FileName : String ) : String;
function  file_GetDirectory( const FileName : String ) : String;
procedure file_SetPath( const Path : String );

{$IFDEF USE_ZIP}
function  file_OpenArchive( const FileName : String; const Password : String = '' ) : Boolean;
procedure file_CloseArchive;
{$ENDIF}

function _file_GetName( const FileName : String ) : PChar;
function _file_GetExtension( const FileName : String ) : PChar;
function _file_GetDirectory( const FileName : String ) : PChar;

{$IF DEFINED(DARWIN) or DEFINED(WINCE)}
function platform_GetRes( const FileName : String ) : String;
{$IFEND}

{$IFDEF UNIX}
const
  { read/write search permission for everyone }
  MODE_MKDIR = S_IWUSR or S_IRUSR or
               S_IWGRP or S_IRGRP or
               S_IWOTH or S_IROTH or
               S_IXUSR or S_IXGRP or S_IXOTH;
{$ENDIF}

implementation
uses
  {$IF DEFINED(DARWIN) or DEFINED(WINCE)}
  zgl_application,
  {$IFEND}
  zgl_main,
  zgl_utils;

var
  filePath : String = '';
  {$IFDEF WINCE}
  wideStr : PWideChar;
  {$ENDIF}
  {$IFDEF iOS}
  iosFileManager : NSFileManager;
  {$ENDIF}
  {$IFDEF USE_ZIP}
  zipCurrent : Pzip;
  {$ENDIF}

function GetDir( const Path : String ) : String;
  var
    len : Integer;
begin
  len := length( Path );
  if ( len > 0 ) and ( Path[ len ] <> '/' ) {$IFDEF WINDOWS} and ( Path[ len ] <> '\' ) {$ENDIF} Then
    Result := Path + '/'
  else
    Result := u_CopyStr( Path );
end;

function file_Open( var FileHandle : zglTFile; const FileName : String; Mode : Byte ) : Boolean;
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      zgl_GetMem( Pointer( FileHandle ), SizeOf( zglZipFile ) );
      zglPZipFile( FileHandle ).file_ := zip_fopen( zipCurrent, PAnsiChar( filePath + FileName ), ZIP_FL_UNCHANGED );
      if not Assigned( zglPZipFile( FileHandle ).file_ ) Then
        zgl_FreeMem( Pointer( FileHandle ) )
      else
        zglPZipFile( FileHandle ).name := u_GetPAnsiChar( filePath + FileName );

      Result := FileHandle <> 0;
      if ( Mode = FOM_CREATE ) or ( Mode = FOM_OPENRW ) Then
        begin
          FileHandle := 0;
          Result := FALSE;
        end;
      exit;
    end;
  {$ENDIF}

{$IFDEF LINUX}
  case Mode of
    FOM_CREATE: FileHandle := FpOpen( filePath + FileName, O_Creat or O_Trunc or O_RdWr );
    FOM_OPENR:  FileHandle := FpOpen( filePath + FileName, O_RdOnly );
    FOM_OPENRW: FileHandle := FpOpen( filePath + FileName, O_RdWr );
  end;
{$ENDIF}
{$IFDEF WINDESKTOP}
  case Mode of
    FOM_CREATE: FileHandle := CreateFile( PChar( filePath + FileName ), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 );
    FOM_OPENR:  FileHandle := CreateFile( PChar( filePath + FileName ), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );
    FOM_OPENRW: FileHandle := CreateFile( PChar( filePath + FileName ), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0 );
  end;
{$ENDIF}
{$IFDEF WINCE}
  wideStr := u_GetPWideChar( platform_GetRes( filePath + FileName ) );
  case Mode of
    FOM_CREATE: FileHandle := CreateFile( wideStr, GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 );
    FOM_OPENR:  FileHandle := CreateFile( wideStr, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );
    FOM_OPENRW: FileHandle := CreateFile( wideStr, GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0 );
  end;
  FreeMem( wideStr );
{$ENDIF}
{$IFDEF DARWIN}
  case Mode of
    FOM_CREATE: FileHandle := FpOpen( platform_GetRes( filePath + FileName ), O_Creat or O_Trunc or O_RdWr );
    FOM_OPENR:  FileHandle := FpOpen( platform_GetRes( filePath + FileName ), O_RdOnly );
    FOM_OPENRW: FileHandle := FpOpen( platform_GetRes( filePath + FileName ), O_RdWr );
  end;
{$ENDIF}
  Result := FileHandle <> FILE_ERROR;
end;

function file_MakeDir( const Directory : String ) : Boolean;
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      Result := FALSE;
      exit;
    end;
  {$ENDIF}

{$IFDEF LINUX}
  Result := FpMkdir( filePath + Directory, MODE_MKDIR ) = FILE_ERROR;
{$ENDIF}
{$IFDEF WINDESKTOP}
  Result := CreateDirectory( PChar( filePath + Directory ), nil );
{$ENDIF}
{$IFDEF WINCE}
  wideStr := u_GetPWideChar( platform_GetRes( filePath + Directory ) );
  Result := CreateDirectory( wideStr, nil );
  FreeMem( wideStr );
{$ENDIF}
{$IFDEF DARWIN}
  Result := FpMkdir( platform_GetRes( filePath + Directory ), MODE_MKDIR ) = FILE_ERROR;
{$ENDIF}
end;

function file_Remove( const Name : String ) : Boolean;
  var
  {$IF DEFINED(LINUX) or DEFINED(MACOSX)}
    status : Stat;
  {$IFEND}
  {$IFDEF WINDOWS}
    attr   : LongWord;
  {$ENDIF}
  {$IFDEF iOS}
    error : NSErrorPointer;
  {$ENDIF}
    i    : Integer;
    dir  : Boolean;
    path : String;
    list : zglTFileList;
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      Result := FALSE;
      exit;
    end;
  {$ENDIF}

  if not file_Exists( Name ) Then
    begin
      Result := FALSE;
      exit;
    end;

{$IFDEF LINUX}
  FpStat( filePath + Name, status );
  dir := fpS_ISDIR( status.st_mode );
{$ENDIF}
{$IFDEF WINDESKTOP}
  {$IFNDEF WINCE}
  attr := GetFileAttributes( PChar( filePath + Name ) );
  {$ELSE}
  wideStr := u_GetPWideChar( platform_GetRes( filePath + Name ) );
  attr    := GetFileAttributes( wideStr );
  FreeMem( wideStr );
  {$ENDIF}
  dir  := attr and FILE_ATTRIBUTE_DIRECTORY > 0;
{$ENDIF}
{$IFDEF MACOSX}
  FpStat( platform_GetRes( filePath + Name ), status );
  dir := fpS_ISDIR( status.st_mode );
{$ENDIF}
{$IFDEF iOS}
  iosFileManager.fileExistsAtPath_isDirectory( u_GetNSString( platform_GetRes( filePath + Name ) ), @dir );
{$ENDIF}

  if dir Then
    begin
      path := GetDir( Name );

      file_Find( path, list, FALSE );
      for i := 0 to list.Count - 1 do
        file_Remove( path + list.Items[ i ] );

      file_Find( path, list, TRUE );
      for i := 2 to list.Count - 1 do
        file_Remove( path + list.Items[ i ] );

      {$IFDEF LINUX}
      Result := FpRmdir( filePath + Name ) = 0;
      {$ENDIF}
      {$IFDEF WINDESKTOP}
      Result := RemoveDirectory( PChar( filePath + Name ) );
      {$ENDIF}
      {$IFDEF MACOSX}
      Result := FpRmdir( platform_GetRes( filePath + Name ) ) = 0;
      {$ENDIF}
      {$IFDEF iOS}
      Result := iosFileManager.removeItemAtPath_error( u_GetNSString( platform_GetRes( filePath + Name ) ), error );
      {$ENDIF}
    end else
      {$IFDEF LINUX}
      Result := FpUnlink( filePath + Name ) = 0;
      {$ENDIF}
      {$IFDEF WINDESKTOP}
      Result := DeleteFile( PChar( filePath + Name ) );
      {$ENDIF}
      {$IFDEF WINCE}
      begin
        wideStr := u_GetPWideChar( platform_GetRes( filePath + Name ) );
        Result  := DeleteFile( wideStr );
        FreeMem( wideStr );
      end;
      {$ENDIF}
      {$IFDEF MACOSX}
      Result := FpUnlink( platform_GetRes( filePath + Name ) ) = 0;
      {$ENDIF}
      {$IFDEF iOS}
      Result := iosFileManager.removeItemAtPath_error( u_GetNSString( platform_GetRes( filePath + Name ) ), error );
      {$ENDIF}
end;

function file_Exists( const Name : String ) : Boolean;
  {$IFDEF UNIX}
  var
    status : Stat;
  {$ENDIF}
  {$IFDEF USE_ZIP}
  var
    zipStat : Tzip_stat;
  {$ENDIF}
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      Result := zip_stat( zipCurrent, PAnsiChar( Name ), 0, zipStat ) = 0;
      exit;
    end;
  {$ENDIF}

{$IFDEF LINUX}
  Result := FpStat( filePath + Name, status ) = 0;
{$ENDIF}
{$IFDEF WINDESKTOP}
  Result := GetFileAttributes( PChar( filePath + Name ) ) <> $FFFFFFFF;
{$ENDIF}
{$IFDEF WINCE}
  wideStr := u_GetPWideChar( platform_GetRes( filePath + Name ) );
  Result  := GetFileAttributes( wideStr ) <> $FFFFFFFF;
  FreeMem( wideStr );
{$ENDIF}
{$IFDEF MACOSX}
  Result := FpStat( platform_GetRes( filePath + Name ), status ) = 0;
{$ENDIF}
{$IFDEF iOS}
  Result := iosFileManager.fileExistsAtPath( u_GetNSString( platform_GetRes( filePath + Name ) ) );
{$ENDIF}
end;

function file_Seek( FileHandle : zglTFile; Offset, Mode : Integer ) : LongWord;
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      Result := 0;
      exit;
    end;
  {$ENDIF}

{$IFDEF UNIX}
  case Mode of
    FSM_SET: Result := FpLseek( FileHandle, Offset, SEEK_SET );
    FSM_CUR: Result := FpLseek( FileHandle, Offset, SEEK_CUR );
    FSM_END: Result := FpLseek( FileHandle, Offset, SEEK_END );
  end;
{$ENDIF}
{$IFDEF WINDOWS}
  case Mode of
    FSM_SET: Result := SetFilePointer( FileHandle, Offset, nil, FILE_BEGIN );
    FSM_CUR: Result := SetFilePointer( FileHandle, Offset, nil, FILE_CURRENT );
    FSM_END: Result := SetFilePointer( FileHandle, Offset, nil, FILE_END );
  end;
{$ENDIF}
end;

function file_GetPos( FileHandle : zglTFile ) : LongWord;
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      Result := 0;
      exit;
    end;
  {$ENDIF}

{$IFDEF UNIX}
  Result := FpLseek( FileHandle, 0, SEEK_CUR );
{$ENDIF}
{$IFDEF WINDOWS}
  Result := SetFilePointer( FileHandle, 0, nil, FILE_CURRENT );
{$ENDIF}
end;

function file_Read( FileHandle : zglTFile; var Buffer; Bytes : LongWord ) : LongWord;
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      Result := zip_fread( zglPZipFile( FileHandle ).file_, Buffer, Bytes );
      exit;
    end;
  {$ENDIF}

{$IFDEF UNIX}
  Result := FpLseek( FileHandle, 0, SEEK_CUR );
  if Result + Bytes > file_GetSize( FileHandle ) Then
    Result := file_GetSize( FileHandle ) - Result
  else
    Result := Bytes;
  FpRead( FileHandle, Buffer, Bytes );
{$ENDIF}
{$IFDEF WINDOWS}
  ReadFile( FileHandle, Buffer, Bytes, Result, nil );
{$ENDIF}
end;

function file_Write( FileHandle : zglTFile; const Buffer; Bytes : LongWord ) : LongWord;
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      Result := 0;
      exit;
    end;
  {$ENDIF}

{$IFDEF UNIX}
  Result := FpLseek( FileHandle, 0, SEEK_CUR );
  if Result + Bytes > file_GetSize( FileHandle ) Then
    Result := file_GetSize( FileHandle ) - Result
  else
    Result := Bytes;
  FpWrite( FileHandle, Buffer, Bytes );
{$ENDIF}
{$IFDEF WINDOWS}
  WriteFile( FileHandle, Buffer, Bytes, Result, nil );
{$ENDIF}
end;

function file_GetSize( FileHandle : zglTFile ) : LongWord;
  {$IFDEF UNIX}
  var
    tmp : LongWord;
  {$ENDIF}
  {$IFDEF USE_ZIP}
  var
    zipStat : Tzip_stat;
  {$ENDIF}
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      if zip_stat( zipCurrent, zglPZipFile( FileHandle ).name, 0, zipStat ) = 0 Then
        Result := zipStat.size
      else
        Result := 0;
      exit;
    end;
  {$ENDIF}

{$IFDEF UNIX}
  // Весьма безумная реализация 8)
  tmp    := FpLseek( FileHandle, 0, SEEK_CUR );
  Result := FpLseek( FileHandle, 0, SEEK_END );
  FpLseek( FileHandle, tmp, SEEK_SET );
{$ENDIF}
{$IFDEF WINDOWS}
  Result := GetFileSize( FileHandle, nil );
{$ENDIF}
end;

procedure file_Flush( FileHandle : zglTFile );
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then exit;
  {$ENDIF}

{$IFDEF UNIX}
  //fflush( FileHandle );
{$ENDIF}
{$IFDEF WINDOWS}
  FlushFileBuffers( FileHandle );
{$ENDIF}
end;

procedure file_Close( var FileHandle : zglTFile );
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      zip_fclose( zglPZipFile( FileHandle ).file_ );
      zgl_FreeMem( Pointer( zglPZipFile( FileHandle ).name ) );
      zgl_FreeMem( Pointer( FileHandle ) );
      FileHandle := 0;
      exit;
    end;
  {$ENDIF}

{$IFDEF UNIX}
  FpClose( FileHandle );
{$ENDIF}
{$IFDEF WINDOWS}
  CloseHandle( FileHandle );
{$ENDIF}
  FileHandle := FILE_ERROR;
end;

procedure file_Find( const Directory : String; var List : zglTFileList; FindDir : Boolean );
  var
  {$IF DEFINED(LINUX) or DEFINED(MACOSX)}
    dir    : PDir;
    dirent : PDirent;
    _type  : Integer;
  {$IFEND}
  {$IFDEF WINDOWS}
    First : THandle;
    FList : WIN32_FIND_DATA;
  {$ENDIF}
  {$IFDEF iOS}
    i           : Integer;
    dirContent  : NSArray;
    path        : NSString;
    fileName    : array[ 0..255 ] of Char;
    error       : NSErrorPointer;
    isDirectory : Boolean;
  {$ENDIF}
  {$IFDEF USE_ZIP}
    count : Integer;
    name  : PAnsiChar;
    len   : Integer;
  {$ENDIF}
begin
  List.Count := 0;

  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      for count := 0 to zip_get_num_entries( zipCurrent, ZIP_FL_UNCHANGED ) do
        begin
          name := zip_get_name( zipCurrent, count, ZIP_FL_UNCHANGED );
          len  := Length( name );
          if ( file_GetDirectory( name ) = Directory ) and ( ( FindDir and ( name[ len - 1 ] = '/' ) ) or ( ( not FindDir ) and ( name[ len - 1 ] <> '/' ) ) ) Then
            begin
              SetLength( List.Items, List.Count + 1 );
              List.Items[ List.Count ] := u_CopyStr( name );
              INC( List.Count );
            end;
        end;

      if List.Count > 2 Then
        u_SortList( List, 0, List.Count - 1 );
      exit;
    end;
  {$ENDIF}

{$IF DEFINED(LINUX) or DEFINED(MACOSX)}
  if FindDir Then
    _type := 4
  else
    _type := 8;

  {$IFDEF LINUX}
  dir := FpOpenDir( filePath + Directory );
  {$ELSE}
  dir := FpOpenDir( platform_GetRes( filePath + Directory ) );
  {$ENDIF}
  repeat
    dirent := FpReadDir( dir^ );
    if Assigned( dirent ) and ( dirent^.d_type = _type ) Then
      begin
        SetLength( List.Items, List.Count + 1 );
        List.Items[ List.Count ] := dirent^.d_name;
        INC( List.Count );
      end;
  until not Assigned( dirent );
  FpCloseDir( dir^ );
{$IFEND}
{$IFDEF WINDOWS}
  {$IFNDEF WINCE}
  First := FindFirstFile( PChar( GetDir( filePath + Directory ) + '*' ), FList );
  {$ELSE}
  wideStr := u_GetPWideChar( platform_GetRes( filePath + Directory ) + '*' );
  First   := FindFirstFile( wideStr, FList );
  FreeMem( wideStr );
  {$ENDIF}
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
  FindClose( First );
{$ENDIF}
{$IFDEF iOS}
  path       := u_GetNSString( platform_GetRes( filePath + Directory ) );
  dirContent := iosFileManager.contentsOfDirectoryAtPath_error( path, error );
  iosFileManager.changeCurrentDirectoryPath( path );
  for i := 0 to dirContent.count() - 1 do
    begin
      if FindDir Then
        begin
          if ( iosFileManager.fileExistsAtPath_isDirectory( dirContent.objectAtIndex( i ), @isDirectory ) ) and ( not isDirectory ) Then continue;
        end else
          if ( iosFileManager.fileExistsAtPath_isDirectory( dirContent.objectAtIndex( i ), @isDirectory ) ) and ( isDirectory ) Then continue;

      SetLength( List.Items, List.Count + 1 );
      FillChar( fileName[ 0 ], 256, 0 );
      CFStringGetCString( CFStringRef( dirContent.objectAtIndex( i ) ), @fileName[ 0 ], 255, kCFStringEncodingUTF8 );
      List.Items[ List.Count ] := PChar( @fileName[ 0 ] );
      INC( List.Count );
    end;
{$ENDIF}

  if List.Count > 2 Then
    u_SortList( List, 0, List.Count - 1 );
end;

procedure GetStr( const Str : String; var Result : String; const d : Char; const b : Boolean );
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
  if b Then
    Result := copy( Str, 1, pos )
  else
    Result := copy( Str, l - ( l - pos ) + 1, ( l - pos ) );
end;

function file_GetName( const FileName : String ) : String;
  var
    tmp : String;
begin
  GetStr( FileName, Result, '/', FALSE );
  {$IFDEF WINDOWS}
  if Result = FileName Then
    GetStr( FileName, Result, '\', FALSE );
  {$ENDIF}
  GetStr( Result, tmp, '.', FALSE );
  if Result <> tmp Then
    Result := copy( Result, 1, length( Result ) - length( tmp ) - 1 );
end;

function file_GetExtension( const FileName : String ) : String;
  var
    tmp : String;
begin
  GetStr( FileName, tmp, '/', FALSE );
  {$IFDEF WINDOWS}
  if tmp = FileName Then
    GetStr( FileName, tmp, '\', FALSE );
  {$ENDIF}
  GetStr( tmp, Result, '.', FALSE );
  if tmp = Result Then
    Result := '';
end;

function file_GetDirectory( const FileName : String ) : String;
begin
  GetStr( FileName, Result, '/', TRUE );
  {$IFDEF WINDOWS}
  if Result = '' Then
    GetStr( FileName, Result, '\', TRUE );
  {$ENDIF}
end;

procedure file_SetPath( const Path : String );
begin
  filePath := GetDir( Path );
end;

{$IFDEF MACOSX}
function platform_GetRes( const FileName : String ) : String;
  var
    len : Integer;
begin
  len := length( FileName );
  if ( len > 0 ) and ( FileName[ 1 ] <> '/' ) Then
    Result := appWorkDir + 'Contents/Resources/' + FileName
  else
    Result := FileName;
end;
{$ENDIF}
{$IFDEF WINCE}
function platform_GetRes( const FileName : String ) : String;
  var
    len : Integer;
begin
  len := length( FileName );
  if ( len > 0 ) and ( FileName[ 1 ] <> '/' ) and ( FileName[ 1 ] <> '\' ) Then
    Result := appWorkDir + FileName
  else
    Result := FileName;
end;
{$ENDIF}
{$IFDEF iOS}
function platform_GetRes( const FileName : String ) : String;
  var
    len : Integer;
begin
  len := length( FileName );
  if ( len > 0 ) and ( FileName[ 1 ] <> '/' ) Then
    Result := appWorkDir + FileName
  else
    Result := FileName;
end;
{$ENDIF}

{$IFDEF USE_ZIP}
function file_OpenArchive( const FileName : String; const Password : String = '' ) : Boolean;
  var
    error : Integer;
begin
  {$IF DEFINED(MACOSX) or DEFINED(iOS) or DEFINED(WINCE)}
  zipCurrent := zip_open( PAnsiChar( platform_GetRes( filePath + FileName ) ), 0, error );
  {$ELSE}
  zipCurrent := zip_open( PAnsiChar( FileName ), 0, error );
  {$IFEND}
  Result     := zipCurrent <> nil;
  if Password = '' Then
    zip_set_default_password( zipCurrent, nil )
  else
    zip_set_default_password( zipCurrent, PAnsiChar( Password ) );
end;

procedure file_CloseArchive;
begin
  zip_close( zipCurrent );
  zipCurrent := nil;
end;
{$ENDIF}

function _file_GetName( const FileName : String ) : PChar;
begin
  Result := u_GetPChar( file_GetName( FileName ) );
end;

function _file_GetExtension( const FileName : String ) : PChar;
begin
  Result := u_GetPChar( file_GetExtension( FileName ) );
end;

function _file_GetDirectory( const FileName : String ) : PChar;
begin
  Result := u_GetPChar( file_GetDirectory( FileName ) );
end;

{$IFDEF iOS}
initialization
  app_InitPool();
  iosFileManager := NSFileManager.alloc().init();

finalization
  iosFileManager.dealloc();
{$ENDIF}

end.
