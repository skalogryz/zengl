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
unit zgl_shader;

{$I define.inc}

interface

uses
  GL, GLext,
  zgl_opengl,
  zgl_file,
  zgl_memory,
  zgl_global_var,
  zgl_log,
  Utils;
  
const
  SHADER_ARB          = 0;
  SHADER_GLSL         = 1;
  SHADER_VERTEX_ARB   = $8620;
  SHADER_FRAGMENT_ARB = $8804;
  SHADER_VERTEX       = $8B31;
  SHADER_FRAGMENT     = $8B30;
  
function  shader_InitARB : Boolean;
function  shader_LoadFromFileARB( const FileName : PChar; const ShaderType : DWORD ) : DWORD;
procedure shader_BeginARB( const Shader, ShaderType : DWORD );
procedure shader_EndARB( const ShaderType : DWORD );
procedure shader_FreeARB( const Shader : DWORD );
  
function  shader_InitGLSL : Boolean;
function  shader_LoadFromFile( const FileName : PChar; const ShaderType : DWORD; const Link : Boolean ) : DWORD;
procedure shader_Attach( const Attach : DWORD );
procedure shader_BeginLink;
function  shader_EndLink : DWORD;
procedure shader_Begin( const Shader : DWORD );
procedure shader_End;
procedure shader_Free( const Shader : DWORD );
function  shader_GetUniform( const Shader : DWORD; const UniformName : PChar ) : Integer;
procedure shader_SetUniform1f( const Uniform : Integer; const v1 : Single );
procedure shader_SetUniform1i( const Uniform : Integer; const v1 : Integer );
procedure shader_SetUniform2f( const Uniform : Integer; const v1, v2 : Single );
procedure shader_SetUniform3f( const Uniform : Integer; const v1, v2, v3 : Single );
procedure shader_SetUniform4f( const Uniform : Integer; const v1, v2, v3, v4 : Single );
function  shader_GetAttrib( const Shader : DWORD; const AttribName : PChar ) : Integer;
procedure shader_SetAttrib1f( const Attrib : Integer; const v1 : Single );
procedure shader_SetAttrib2f( const Attrib : Integer; const v1, v2 : Single );
procedure shader_SetAttrib3f( const Attrib : Integer; const v1, v2, v3 : Single );
procedure shader_SetAttrib4f( const Attrib : Integer; const v1, v2, v3, v4 : Single );
procedure shader_SetAttribPf( const Attrib : Integer; const v : Pointer; const Normalized : Boolean );
procedure shader_SetParameter4f( const ShaderType : DWORD; const Parameter : Integer; const v1, v2, v3, v4 : Single; const Local : Boolean );

implementation

var
  ls : DWORD;

{------------------------------------------------------------------------------}
{-------------------------------- ARB SHADERS ---------------------------------}
{------------------------------------------------------------------------------}
function shader_InitARB;
begin
  Result := FALSE;
  glGenProgramsARB := gl_GetProc( 'glGenPrograms' );
  if Assigned( glGenProgramsARB ) Then
    begin
      ogl_CanARB                   := TRUE;
      glBindProgramARB             := gl_GetProc( 'glBindProgram' );
      glDeleteProgramsARB          := gl_GetProc( 'glDeletePrograms' );
      glProgramStringARB           := gl_GetProc( 'glProgramString' );
      glGetAttribLocationARB       := gl_GetProc( 'glGetAttribLocation' );
      glVertexAttrib1fARB          := gl_GetProc( 'glVertexAttrib1f' );
      glVertexAttrib2fARB          := gl_GetProc( 'glVertexAttrib2f' );
      glVertexAttrib3fARB          := gl_GetProc( 'glVertexAttrib3f' );
      glVertexAttrib4fARB          := gl_GetProc( 'glVertexAttrib4f' );
      glVertexAttribPointerARB     := gl_GetProc( 'glVertexAttribPointer' );
      glProgramLocalParameter4fARB := gl_GetProc( 'glProgramLocalParameter4f' );
      glProgramEnvParameter4fARB   := gl_GetProc( 'glProgramEnvParameter4f' );
      Result := TRUE;
    end else
      ogl_CanARB := FALSE;
  log_Add( 'Support ARB shaders: ' + u_BoolToStr( ogl_CanARB ) );
end;

function shader_LoadFromFileARB;
  var
    M : zglTMemory;
    i : Integer;
    s : string;
    c : Char;

    _string : PChar;
begin
  Result := 0;
  if not ogl_CanARB Then Exit;

  if not file_Exists( FileName ) Then exit;

  s := '';
  mem_LoadFromFile( M, FileName );
  for i := 0 to M.Size - 1 do
    begin
      mem_Read( M, c, 1 );
      s := s + c;
    end;

  _string := PChar( s );
  
  glGenProgramsARB( 1, @Result );
  glBindProgramARB( ShaderType, Result );
  glProgramStringARB( ShaderType, GL_PROGRAM_FORMAT_ASCII_ARB, M.Size, _string );
  mem_Free( M );
  s := glGetString( GL_PROGRAM_ERROR_STRING_ARB );
  if s <> '' Then
    begin
      log_Add( 'Shader load error: ' + #13#10 + s );
      Result := 0;
      exit;
    end;
end;

procedure shader_BeginARB;
begin
  if not ogl_CanARB Then exit;
  glEnable( ShaderType );
  glBindProgramARB( ShaderType, Shader );
end;

procedure shader_EndARB;
begin
  if not ogl_CanARB Then exit;
  glBindProgramARB( ShaderType, 0 );
  glDisable( ShaderType );
end;

procedure shader_FreeARB;
begin
  if not ogl_CanARB Then exit;
  glDeleteProgramsARB( 1, @Shader );
end;

{------------------------------------------------------------------------------}
{-------------------------------- GLSL SHADERS --------------------------------}
{------------------------------------------------------------------------------}

function shader_InitGLSL;
begin
  Result := FALSE;
  glCreateShaderObjectARB := gl_GetProc( 'glCreateShaderObject' );
  if Assigned( glCreateShaderObjectARB ) Then
    begin
      ogl_CanGLSL               := TRUE;
      glCreateShaderObjectARB   := gl_GetProc( 'glCreateShaderObject' );
      glCreateProgramObjectARB  := gl_GetProc( 'glCreateProgramObject' );
      glDeleteObjectARB         := gl_GetProc( 'glDeleteObject' );
      glShaderSourceARB         := gl_GetProc( 'glShaderSource' );
      glCompileShaderARB        := gl_GetProc( 'glCompileShader' );
      glAttachObjectARB         := gl_GetProc( 'glAttachObject' );
      glGetObjectParameterivARB := gl_GetProc( 'glGetObjectParameteriv' );
      glLinkProgramARB          := gl_GetProc( 'glLinkProgram' );
      glUseProgramObjectARB     := gl_GetProc( 'glUseProgramObject' );
      glGetUniformLocationARB   := gl_GetProc( 'glGetUniformLocation' );
      glUniform1fARB            := gl_GetProc( 'glUniform1f' );
      glUniform1iARB            := gl_GetProc( 'glUniform1i' );
      glUniform2fARB            := gl_GetProc( 'glUniform2f' );
      glUniform3fARB            := gl_GetProc( 'glUniform3f' );
      glUniform4fARB            := gl_GetProc( 'glUniform4f' );
      glGetAttribLocationARB    := gl_GetProc( 'glGetAttribLocation' );
      glVertexAttrib1fARB       := gl_GetProc( 'glVertexAttrib1f' );
      glVertexAttrib2fARB       := gl_GetProc( 'glVertexAttrib2f' );
      glVertexAttrib3fARB       := gl_GetProc( 'glVertexAttrib3f' );
      glVertexAttrib4fARB       := gl_GetProc( 'glVertexAttrib4f' );
      glVertexAttribPointerARB  := gl_GetProc( 'glVertexAttribPointer' );
      Result := TRUE;
    end else
      ogl_CanGLSL := FALSE;
  log_Add( 'Support GLSL shaders: ' + u_BoolToStr( ogl_CanGLSL ) );
end;

function shader_LoadFromFile;
  var
    M : zglTMemory;
    i : Integer;
    s : string;
    c : Char;

    _string : PChar;
    params  : DWORD;
begin
  Result := 0;
  if not ogl_CanGLSL Then Exit;

  if not file_Exists( FileName ) Then exit;

  s := '';
  mem_LoadFromFile( M, FileName );
  for i := 0 to M.Size - 1 do
    begin
      mem_Read( M, c, 1 );
      if ( c = #10 ) or ( c = #13 ) Then
        s := s + #13#10
      else
        s := s + c;
    end;
  mem_Free( M );

  _string := PChar( s );
  if Link Then
    begin
      Result := glCreateProgramObjectARB;
      ls     := glCreateShaderObjectARB( ShaderType );
    end else
      Result := glCreateShaderObjectARB( ShaderType );
  if Result = 0 Then
    begin
      log_Add( 'Shader create error.' );
      Exit;
    end;

  if Link Then
    begin
      glShaderSourceARB ( ls, 1, @_string, nil );
      glCompileShaderARB( ls );

      glGetObjectParameterivARB( ls, GL_OBJECT_COMPILE_STATUS_ARB, @params );
      if params <> GL_TRUE Then
        begin
          Result := 0;
          glDeleteObjectARB( Result );
          log_Add( 'Shader compile error.' );
          Exit;
        end;

      glAttachObjectARB( Result, ls );
      glLinkProgramARB ( Result );

      glGetObjectParameterivARB( Result, GL_OBJECT_LINK_STATUS_ARB, @params );
      if params <> GL_TRUE Then
        begin
          Result := 0;
          glDeleteObjectARB( Result );
          log_Add( 'Shader link error.' );
          Exit;
        end;
    end else
      begin
        glShaderSourceARB ( Result, 1, @_string, nil );
        glCompileShaderARB( Result );

        glGetObjectParameterivARB( Result, GL_OBJECT_COMPILE_STATUS_ARB, @params );

        if params <> GL_TRUE Then
          begin
            Result := 0;
            glDeleteObjectARB( Result );
            log_Add( 'Shader compile error.' );
            Exit;
          end;
      end;
end;

procedure shader_Attach;
begin
  if not ogl_CanGLSL Then Exit;
  glAttachObjectARB( ls, Attach );
end;

procedure shader_BeginLink;
begin
  ls := 0;
  if not ogl_CanGLSL Then Exit;
  ls := glCreateProgramObjectARB;
end;

function shader_EndLink;
  var
    status : Integer;
begin
  Result := 0;
  if not ogl_CanGLSL Then Exit;
  glLinkProgramARB( ls );
  glGetObjectParameterivARB( ls, GL_OBJECT_LINK_STATUS_ARB, @status );
  if status <> GL_TRUE then
    begin
      Result := 0;
      glDeleteObjectARB( ls );
      log_Add( 'Shader link error' );
      Exit;
    end else
      Result := ls;
end;

procedure shader_Begin;
begin
  if not ogl_CanGLSL Then Exit;
  glUseProgramObjectARB( Shader );
end;

procedure shader_End;
begin
  if not ogl_CanGLSL Then Exit;
  glUseProgramObjectARB( 0 );
end;

procedure shader_Free;
begin
  if not ogl_CanGLSL Then Exit;
  glDeleteObjectARB( Shader );
end;

function shader_GetUniform;
begin
  Result := 0;
  if not ogl_CanGLSL Then Exit;
  Result := glGetUniformLocationARB( Shader, UniformName );
end;

procedure shader_SetUniform1f;
begin
  if not ogl_CanGLSL Then Exit;
  glUniform1fARB( Uniform, v1 );
end;

procedure shader_SetUniform1i;
begin
  if not ogl_CanGLSL Then Exit;
  glUniform1iARB( Uniform, v1 );
end;

procedure shader_SetUniform2f;
begin
  if not ogl_CanGLSL Then Exit;
  glUniform2fARB( Uniform, v1, v2 );
end;

procedure shader_SetUniform3f;
begin
  if not ogl_CanGLSL Then Exit;
  glUniform3fARB( Uniform, v1, v2, v3 );
end;

procedure shader_SetUniform4f;
begin
  if not ogl_CanGLSL Then Exit;
  glUniform4fARB( Uniform, v1, v2, v3, v4 );
end;

function shader_GetAttrib;
begin
  Result := 0;
  if ( not ogl_CanGLSL ) and ( not ogl_CanARB ) Then Exit;
  Result := glGetAttribLocationARB( Shader, AttribName );
end;

procedure shader_SetAttrib1f;
begin
  if ( not ogl_CanGLSL ) and ( not ogl_CanARB ) Then exit;
  glVertexAttrib1fARB( Attrib, v1 );
end;

procedure shader_SetAttrib2f;
begin
  if ( not ogl_CanGLSL ) and ( not ogl_CanARB ) Then exit;
  glVertexAttrib2fARB( Attrib, v1, v2 );
end;

procedure shader_SetAttrib3f;
begin
  if ( not ogl_CanGLSL ) and ( not ogl_CanARB ) Then exit;
  glVertexAttrib3fARB( Attrib, v1, v2, v3 );
end;

procedure shader_SetAttrib4f;
begin
  if ( not ogl_CanGLSL ) and ( not ogl_CanARB ) Then exit;
  glVertexAttrib4fARB( Attrib, v1, v2, v3, v4 );
end;

procedure shader_SetAttribPf;
begin
  if ( not ogl_CanGLSL ) and ( not ogl_CanARB ) Then exit;
  glVertexAttribPointerARB( Attrib, 3, GL_FLOAT, Byte( Normalized ), 0, v );
end;

procedure shader_SetParameter4f;
begin
  if not ogl_CanARB Then exit;
  if Local Then
    glProgramLocalParameter4fARB( ShaderType, Parameter, v1, v2, v3, v4 )
  else
    glProgramEnvParameter4fARB( ShaderType, Parameter, v1, v2, v3, v4 );
end;

end.
