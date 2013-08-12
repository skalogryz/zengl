{
 *  Copyright (c) 2012 Andrey Kemka
 *
 *  This software is provided 'as-is', without any express or
 *  implied warranty. In no event will the authors be held
 *  liable for any damages arising from the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute
 *  it freely, subject to the following restrictions:
 *
 *  1. The origin of this software must not be misrepresented;
 *     you must not claim that you wrote the original software.
 *     If you use this software in a product, an acknowledgment
 *     in the product documentation would be appreciated but
 *     is not required.
 *
 *  2. Altered source versions must be plainly marked as such,
 *     and must not be misrepresented as being the original software.
 *
 *  3. This notice may not be removed or altered from any
 *     source distribution.
}
unit zgl_file;

{$I zgl_config.cfg}

interface

uses
  Windows,
  {$IFDEF USE_ZIP}
  zgl_lib_zip,
  {$ENDIF}
  zgl_types;

type
  zglTFile     = Ptr;
  zglTFileList = zglTStringList;

const
  FILE_ERROR = Ptr( -1 );

  // Open Mode
  FOM_CREATE = $01; // Create
  FOM_OPENR  = $02; // Read
  FOM_OPENRW = $03; // Read&Write

  // Seek Mode
  FSM_SET    = $01;
  FSM_CUR    = $02;
  FSM_END    = $03;

function  file_Open( out FileHandle : zglTFile; const FileName : UTF8String; Mode : Byte ) : Boolean;
function  file_MakeDir( const Directory : UTF8String ) : Boolean;
function  file_Remove( const Name : UTF8String ) : Boolean;
function  file_Exists( const Name : UTF8String ) : Boolean;
function  file_Seek( FileHandle : zglTFile; Offset, Mode : Integer ) : LongWord;
function  file_GetPos( FileHandle : zglTFile ) : LongWord;
function  file_Read( FileHandle : zglTFile; var Buffer; Bytes : LongWord ) : LongWord;
function  file_Write( FileHandle : zglTFile; const Buffer; Bytes : LongWord ) : LongWord;
function  file_GetSize( FileHandle : zglTFile ) : LongWord;
procedure file_Flush( FileHandle : zglTFile );
procedure file_Close( var FileHandle : zglTFile );
procedure file_Find( const Directory : UTF8String; out List : zglTFileList; FindDir : Boolean );
function  file_GetName( const FileName : UTF8String ) : UTF8String;
function  file_GetExtension( const FileName : UTF8String ) : UTF8String;
function  file_GetDirectory( const FileName : UTF8String ) : UTF8String;
procedure file_SetPath( const Path : UTF8String );

{$IFDEF USE_ZIP}
function  file_OpenArchive( const FileName : UTF8String; const Password : UTF8String = '' ) : Boolean;
procedure file_CloseArchive;
{$ENDIF}

function _file_GetName( const FileName : UTF8String ) : PAnsiChar;
function _file_GetExtension( const FileName : UTF8String ) : PAnsiChar;
function _file_GetDirectory( const FileName : UTF8String ) : PAnsiChar;

implementation
uses
  zgl_main,
  zgl_resources,
  zgl_log,
  zgl_utils;

var
  filePath : UTF8String = '';

threadvar
  wideStr  : PWideChar;

function GetDir( const Path : UTF8String ) : UTF8String;
  var
    len : Integer;
begin
  len := Length( Path );
  if ( len > 0 ) and ( Path[ len ] <> '/' ) {$IFDEF WINDOWS} and ( Path[ len ] <> '\' ) {$ENDIF} Then
    Result := Path + '/'
  else
    Result := utf8_Copy( Path );
end;

function file_Open( out FileHandle : zglTFile; const FileName : UTF8String; Mode : Byte ) : Boolean;
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      if ( Mode = FOM_CREATE ) or ( Mode = FOM_OPENRW ) Then
        begin
          FileHandle := 0;
          Result := FALSE;
          exit;
        end;

      zgl_GetMem( Pointer( FileHandle ), SizeOf( zglZipFile ) );
      zglPZipFile( FileHandle ).file_ := zip_fopen( zipCurrent, PAnsiChar( filePath + FileName ), ZIP_FL_UNCHANGED );
      if not Assigned( zglPZipFile( FileHandle ).file_ ) Then
        zgl_FreeMem( Pointer( FileHandle ) )
      else
        zglPZipFile( FileHandle ).name := utf8_GetPAnsiChar( filePath + FileName );

      Result := FileHandle <> 0;
      exit;
    end;
  {$ENDIF}

  wideStr := utf8_GetPWideChar( filePath + FileName );
  case Mode of
    FOM_CREATE: FileHandle := CreateFileW( wideStr, GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 );
    FOM_OPENR:  FileHandle := CreateFileW( wideStr, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );
    FOM_OPENRW: FileHandle := CreateFileW( wideStr, GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0 );
  end;
  FreeMem( wideStr );
  Result := FileHandle <> FILE_ERROR;
end;

function file_MakeDir( const Directory : UTF8String ) : Boolean;
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      Result := FALSE;
      exit;
    end;
  {$ENDIF}

  wideStr := utf8_GetPWideChar( filePath + Directory );
  Result := CreateDirectoryW( wideStr, nil );
  FreeMem( wideStr );
end;

function file_Remove( const Name : UTF8String ) : Boolean;
  var
    i    : Integer;
    dir  : Boolean;
    path : UTF8String;
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

  wideStr := utf8_GetPWideChar( filePath + Name );
  dir := GetFileAttributesW( wideStr ) and FILE_ATTRIBUTE_DIRECTORY > 0;
  FreeMem( wideStr );

  if dir Then
    begin
      path := GetDir( Name );

      file_Find( path, list, FALSE );
      for i := 0 to list.Count - 1 do
        file_Remove( path + list.Items[ i ] );

      file_Find( path, list, TRUE );
      for i := 2 to list.Count - 1 do
        file_Remove( path + list.Items[ i ] );

      wideStr := utf8_GetPWideChar( filePath + Name );
      Result := RemoveDirectoryW( wideStr );
      FreeMem( wideStr );
    end else
      begin
        wideStr := utf8_GetPWideChar( filePath + Name );
        Result := DeleteFileW( wideStr );
        FreeMem( wideStr );
      end;
end;

function file_Exists( const Name : UTF8String ) : Boolean;
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
  wideStr := utf8_GetPWideChar( filePath + Name );
  Result  := GetFileAttributesW( wideStr ) <> $FFFFFFFF;
  FreeMem( wideStr );
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

  case Mode of
    FSM_SET: Result := SetFilePointer( FileHandle, Offset, nil, FILE_BEGIN );
    FSM_CUR: Result := SetFilePointer( FileHandle, Offset, nil, FILE_CURRENT );
    FSM_END: Result := SetFilePointer( FileHandle, Offset, nil, FILE_END );
  else
    Result := 0;
  end;
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

  Result := SetFilePointer( FileHandle, 0, nil, FILE_CURRENT );
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

  ReadFile( FileHandle, Buffer, Bytes, Result, nil );
end;

function file_Write( FileHandle : zglTFile; const Buffer; Bytes : LongWord ) : LongWord;
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) and ( FileHandle <> log {FIXME} ) Then
    begin
      Result := 0;
      exit;
    end;
  {$ENDIF}
  WriteFile( FileHandle, Buffer, Bytes, Result, nil );
end;

function file_GetSize( FileHandle : zglTFile ) : LongWord;
  {$IFDEF USE_ZIP}
  var
    zipStat : Tzip_stat;
  {$ENDIF}
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) and ( FileHandle <> log {FIXME} ) Then
    begin
      if zip_stat( zipCurrent, zglPZipFile( FileHandle ).name, 0, zipStat ) = 0 Then
        Result := zipStat.size
      else
        Result := 0;
      exit;
    end;
  {$ENDIF}

  Result := GetFileSize( FileHandle, nil );
end;

procedure file_Flush( FileHandle : zglTFile );
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) and ( FileHandle <> log {FIXME} ) Then exit;
  {$ENDIF}

  FlushFileBuffers( FileHandle );
end;

procedure file_Close( var FileHandle : zglTFile );
begin
  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) and ( FileHandle <> log {FIXME} ) Then
    begin
      zip_fclose( zglPZipFile( FileHandle ).file_ );
      zgl_FreeMem( Pointer( zglPZipFile( FileHandle ).name ) );
      zgl_FreeMem( Pointer( FileHandle ) );
      FileHandle := 0;
      exit;
    end;
  {$ENDIF}

  CloseHandle( FileHandle );
  FileHandle := FILE_ERROR;
end;

procedure file_Find( const Directory : UTF8String; out List : zglTFileList; FindDir : Boolean );
  var
    First : THandle;
    FList : WIN32_FIND_DATAW;
    len   : Integer;
  {$IFDEF USE_ZIP}
    count : Integer;
    name  : PAnsiChar;
    name2 : UTF8String;
  {$ENDIF}
begin
  List.Count := 0;

  {$IFDEF USE_ZIP}
  if Assigned( zipCurrent ) Then
    begin
      for count := 0 to zip_get_num_entries( zipCurrent, ZIP_FL_UNCHANGED ) - 1 do
        begin
          name := zip_get_name( zipCurrent, count, ZIP_FL_UNCHANGED );
          len  := Length( name );
          if FindDir Then
            begin
              if name[ len - 1 ] = '/' Then
                begin
                  name2 := name;
                  SetLength( name2, len - 1 );
                end else
                  continue;

              if file_GetDirectory( name2 ) = Directory Then
                begin
                  SetLength( List.Items, List.Count + 1 );
                  List.Items[ List.Count ] := copy( name2, Length( Directory ) + 1, len - 1 );
                  INC( List.Count );
                end;
            end else
              if ( name[ len - 1 ] <> '/' ) and ( file_GetDirectory( name ) = Directory ) Then
                begin
                  SetLength( List.Items, List.Count + 1 );
                  List.Items[ List.Count ] := utf8_Copy( file_GetName( name ) + '.' + file_GetExtension( name ) );
                  INC( List.Count );
                end;
        end;

      if List.Count > 2 Then
        u_SortList( List, 0, List.Count - 1 );
      exit;
    end;
  {$ENDIF}

  wideStr := utf8_GetPWideChar( filePath + Directory + '*' );
  First   := FindFirstFileW( wideStr, FList );
  FreeMem( wideStr );
  repeat
    if FindDir Then
      begin
        if FList.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 Then continue;
      end else
        if FList.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY > 0 Then continue;
    SetLength( List.Items, List.Count + 1 );
    List.Items[ List.Count ] := utf16_GetUTF8String( FList.cFileName );
    INC( List.Count );
  until not FindNextFileW( First, FList );
  FindClose( First );

  if List.Count > 2 Then
    u_SortList( List, 0, List.Count - 1 );
end;

function GetStr( const Str : UTF8String; const d : AnsiChar; const b : Boolean ) : UTF8String;
  var
    i, pos, l : Integer;
begin
  pos := 0;
  l := Length( Str );
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

function file_GetName( const FileName : UTF8String ) : UTF8String;
  var
    tmp : UTF8String;
begin
  Result := GetStr( FileName, '/', FALSE );
  if Result = FileName Then
    Result := GetStr( FileName, '\', FALSE );
  tmp := GetStr( Result, '.', FALSE );
  if Result <> tmp Then
    Result := copy( Result, 1, Length( Result ) - length( tmp ) - 1 );
end;

function file_GetExtension( const FileName : UTF8String ) : UTF8String;
  var
    tmp : UTF8String;
begin
  tmp := GetStr( FileName, '/', FALSE );
  if tmp = FileName Then
    tmp := GetStr( FileName, '\', FALSE );
  Result := GetStr( tmp, '.', FALSE );
  if tmp = Result Then
    Result := '';
end;

function file_GetDirectory( const FileName : UTF8String ) : UTF8String;
begin
  Result := GetStr( FileName, '/', TRUE );
  if Result = '' Then
    Result := GetStr( FileName, '\', TRUE );
end;

procedure file_SetPath( const Path : UTF8String );
begin
  filePath := GetDir( Path );
end;

{$IFDEF USE_ZIP}
function file_OpenArchive( const FileName : UTF8String; const Password : UTF8String = '' ) : Boolean;
  var
    error : Integer;
    res   : zglTZIPResource;
begin
  if resUseThreaded Then
    begin
      Result       := TRUE;
      res.FileName := FileName;
      res.Password := Password;
      res_AddToQueue( RES_ZIP_OPEN, TRUE, @res );
      exit;
    end;

  zipCurrent := zip_open( PAnsiChar( filePath + FileName ), 0, error );
  Result     := zipCurrent <> nil;

  if not Result Then
    begin
      log_Add( 'Unable to open archive: ' + FileName );
      exit;
    end;

  if Password = '' Then
    zip_set_default_password( zipCurrent, nil )
  else
    zip_set_default_password( zipCurrent, PAnsiChar( Password ) );
end;

procedure file_CloseArchive;
  var
    res : zglTZIPResource;
begin
  if resUseThreaded Then
    begin
      res.FileName := '';
      res.Password := '';
      res_AddToQueue( RES_ZIP_CLOSE, TRUE, @res );
      exit;
    end;

  zip_close( zipCurrent );
  zipCurrent := nil;
end;
{$ENDIF}

function _file_GetName( const FileName : UTF8String ) : PAnsiChar;
begin
  Result := utf8_GetPAnsiChar( file_GetName( FileName ) );
end;

function _file_GetExtension( const FileName : UTF8String ) : PAnsiChar;
begin
  Result := utf8_GetPAnsiChar( file_GetExtension( FileName ) );
end;

function _file_GetDirectory( const FileName : UTF8String ) : PAnsiChar;
begin
  Result := utf8_GetPAnsiChar( file_GetDirectory( FileName ) );
end;

end.
