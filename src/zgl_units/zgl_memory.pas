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
unit zgl_memory;

{$I define.inc}

interface

uses
  zgl_file;

type
  zglPMemory = ^zglTMemory;
  zglTMemory = record
    Memory   : Pointer;
    Size     : DWORD;
    Position : DWORD;
end;

procedure mem_LoadFromFile( var Memory : zglTMemory; const FileName : String );
procedure mem_SaveToFile( var Memory : zglTMemory; const FileName : String );
function  mem_Seek( var Memory : zglTMemory; const Offset, Mode : DWORD ) : DWORD;
function  mem_Read( var Memory : zglTMemory; var buffer; const count : DWORD ) : DWORD;
function  mem_ReadSwap( var Memory : zglTMemory; var buffer; const count : DWORD ) : DWORD;
function  mem_Write( var Memory : zglTMemory; const buffer; const count : DWORD ) : DWORD;
procedure mem_SetSize( var Memory : zglTMemory; const Size : DWORD );
procedure mem_Free( var Memory : zglTMemory );

implementation

procedure mem_LoadFromFile;
  var
    F : zglTFile;
begin
  if not file_Exists( FileName ) Then exit;

  file_Open( F, FileName, FOM_OPENR );
  Memory.Size     := file_GetSize( F );
  Memory.Position := 0;
  Memory.Memory   := AllocMem( Memory.Size );
  file_Read( F, Memory.Memory^, Memory.Size );
  file_Close( F );
end;

procedure mem_SaveToFile;
  var
    F : zglTFile;
begin
  file_Open( F, FileName, FOM_CREATE );
  file_Write( F, Memory.Memory^, Memory.Size );
  file_Close( F );
end;

function mem_Seek;
begin
  case Mode of
    FSM_SET: Memory.Position := Offset;
    FSM_CUR: Memory.Position := Memory.Position + Offset;
    FSM_END: Memory.Position := Memory.Size + Offset;
  end;
  Result := Memory.Position;
end;

function mem_Read;
begin
  if ( Memory.Position >= 0 ) and ( count > 0 ) Then
    begin
      Result := Memory.Size - Memory.Position;
      if Result > 0 Then
        begin
          if Result > count Then Result := count;
          Move( Pointer( Memory.Memory + Memory.Position )^, buffer, Result );
          INC( Memory.Position, Result );
          exit;
        end;
    end;
  Result := 0;
end;

function mem_ReadSwap;
  var
    i       : DWORD;
    pBuffer : array of Byte;
begin
  if ( Memory.Position >= 0 ) and ( count > 0 ) Then
    begin
      Result := Memory.Size - Memory.Position;
      if Result > 0 Then
        begin
          if Result > count Then Result := count;
          SetLength( pBuffer, Result );
          for i := 0 to Result - 1 do
            begin
              pbuffer[ Result - i - 1 ] := PByte( Memory.Memory + Memory.Position )^;
              INC( Memory.Position );
            end;
          Move( pBuffer[ 0 ], buffer, Result );
          SetLength( pBuffer, 0 );
          exit;
        end;
    end;
  Result := 0;
end;

function mem_Write;
begin
  if count = 0 Then
    begin
      Result := 0;
      exit;
    end;
  if Memory.Position + Count > Memory.Size Then
    mem_SetSize( Memory, Memory.Position + Count );
  Move( buffer, Pointer( Memory.Memory + Memory.Position )^, Count );
  INC( Memory.Position, Count );
  Result := Count;
end;

procedure mem_SetSize;
begin
  Memory.Memory := ReAllocMem( Memory.Memory, Size );
  Memory.Size   := Size;
end;

procedure mem_Free;
begin
  FreeMem( Memory.Memory );
  Memory.Size     := 0;
  Memory.Position := 0;
end;

end.
