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
library zengl;

{$I define.inc}

uses
  {$IFDEF LINUX}
  X, XLib, XUtil,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}

  GL,
  GLext,
  {$IFDEF LINUX}
  GLX,
  {$ENDIF}
  
  zgl_file,
  zgl_memory,

  zgl_const,
  zgl_types,
  zgl_global_var,

  zgl_main,

  zgl_opengl,
  zgl_window,
  zgl_screen,

  zgl_keyboard,
  zgl_mouse,

  zgl_log,
  zgl_ini,

  zgl_timers,

  zgl_textures,
  zgl_textures_tga,
  zgl_textures_jpg,

  zgl_sound,
  zgl_sound_wav,

  zgl_fx,
  zgl_fx_2d,

  zgl_camera_2d,
  zgl_primitives_2d,
  zgl_sprite_2d,
  zgl_text,

  zgl_gui_main,
  zgl_gui_types,
  zgl_gui_render,
  zgl_gui_process,

  zgl_zbuffer,
  zgl_scissor,

  zgl_render_target,

  zgl_object_3d,
  zgl_primitives_3d,
  zgl_sprite_3d,

  zgl_camera_3d,

  zgl_mesh_file,
  zgl_simple_mesh,
  zgl_skinned_mesh,
  zgl_heightmap,

  zgl_vbo,

  zgl_frustum,
  zgl_octree,

  zgl_light,
  zgl_fog,
  zgl_sky,

  zgl_shadow_volume,

  zgl_shader,

  zgl_math,
  zgl_utils_3d,
  zgl_collision_2d,
  zgl_collision_3d,

  Utils;
  
exports
  // Main
  zgl_Init,
  {$IFDEF WIN32}
  zgl_InitToHandle,
  {$ENDIF}
  zgl_Exit,
  zgl_Reg,
  zgl_Get,
  zgl_GetMem,
  zgl_Enable,
  zgl_Disable,
  log_AddC name 'log_Add',
  
  // Window
  wnd_SetCaption,
  wnd_SetSize,
  wnd_SetPos,
  wnd_SetOnTop,
  wnd_ShowCursor,
  
  // Screen
  scr_Clear,
  scr_Flush,
  scr_SetVSync,
  scr_SetFSAA,
  scr_SetOptions,
  scr_CorrectResolution,
  
  // INI
  ini_LoadFromFile,
  ini_SaveToFile,
  ini_Add,
  ini_ReadKeyStr,
  ini_ReadKeyInt,
  ini_ReadKeyBool,
  ini_WriteKeyStr,
  ini_WriteKeyInt,
  ini_WriteKeyBool,
  
  // Timers
  timer_Add,
  timer_Del,
  timer_GetTicks,
  timer_Reset,
  
  // Keyboard
  key_Down,
  key_Up,
  key_Last,
  key_BeginReadText,
  key_EndReadText,
  key_ClearState,
  
  // Mouse
  mouse_X,
  mouse_Y,
  mouse_DX,
  mouse_DY,
  mouse_Down,
  mouse_Up,
  mouse_Click,
  mouse_Wheel,
  mouse_ClearState,
  mouse_Lock,
  
  // Textures
  tex_Add,
  tex_Del,
  tex_Create,
  tex_CreateZero,
  tex_LoadFromFile,
  tex_SetFrameSize,
  tex_SetMask,
  tex_GetData,
  tex_Filter,
  tex_SetAnisotropy,
  
  // OpenGL
  Set2DMode,
  Set3DMode,
  
  // Z Buffer
  zbuffer_SetDepth,
  zbuffer_Clear,
  
  // Scissor
  scissor_Begin,
  scissor_End,
  
  // Render Targets
  rtarget_Add,
  rtarget_Del,
  rtarget_Set,
  
  // FX
  fx_SetBlendMode,
  // FX 2D
  fx2d_SetColorMix,
  fx2d_SetVCA,
  fx2d_SetVertexes,
  fx2d_SetScale,
  
  // Camera 2D
  cam2d_Set,
  
  // Primitives 2D
  pr2d_Pixel,
  pr2d_Line,
  pr2d_Triangle,
  pr2d_Rect,
  pr2d_Circle,
  pr2d_Ellipse,
  
  // Sprite 2D
  ssprite2d_Draw,
  asprite2d_Draw,
  csprite2d_Draw,
  
  // Text
  font_Add,
  font_Del,
  font_LoadFromFile,
  text_Draw,
  text_GetWidth,

{$IFDEF USE_GUI}
  gui_Init,
  gui_Draw,
  gui_Proc,
  gui_AddWidget,
  gui_DelWidget,
{$ENDIF}

{$IFDEF USE_SOUND}
  // Sound
  snd_Init,
  snd_Free,
  snd_Add,
  snd_Del,
  snd_LoadFromFile,
  snd_Play,
  snd_Stop,
  snd_SetVolume,
  snd_SetFrequency,
  snd_SetFrequencyCoeff,
  snd_PlayFile,
  snd_StopFile,
  snd_RestoreFile,
{$ENDIF}
  
{$IFDEF USE_3D}
  // Object 3D
  obj3d_Begin,
  obj3d_End,
  obj3d_Enable,
  obj3d_Disable,
  // material
  obj3d_SetColor,
  obj3d_BindTexture,
  obj3d_SetMaterial,
  // transform
  obj3d_Rotate,
  obj3d_Scale,
  obj3d_Move,
  obj3d_SetMatrix,
  obj3d_MulMatrix,

  // Primitives 3D
  pr3d_Point,
  pr3d_Line,
  pr3d_Plane,
  pr3d_AABB,
  pr3d_Sphere,

  // Sprite 3D
  ssprite3d_Draw,
  asprite3d_Draw,
  
  // Camera 3D
  cam3d_Set,
  cam3d_Fly,
  cam3d_Strafe,
  
  // Static Mesh
  smesh_LoadFromFile,
  smesh_Animate,
  smesh_Draw,
  smesh_DrawGroup,
  smesh_Free,

  // Skinned Mesh
  skmesh_LoadFromFile,
  skmesh_Animate,
  skmesh_Draw,
  skmesh_DrawGroup,
  skmesh_DrawSkelet,
  skmesh_Free,

  // HeightMap
  heightmap_Build,
  heightmap_Draw,
  heightmap_Free,
  heightmap_GetPlane,
  heightmap_GetYOffset,

  // VBO
  vbo_Build,
  vbo_Free,
  
  // Frustum
  frustum_Calc,
  frustum_PointIn,
  frustum_PPointIn,
  frustum_TriangleIn,
  frustum_SphereIn,
  frustum_BoxIn,
  frustum_CubeIn,
  
  // Octree
  octree_Build,
  octree_Free,
  octree_Draw,
  octree_DrawDebug,
  octree_DrawNode,
  octree_DrawDFaces,
  
  // Light
  light_Enable,
  light_Disable,
  light_SetPosition,
  light_SetMaterial,
  
  // Fog
  fog_Enable,
  fog_Disable,
  fog_SetMode,
  fog_SetColor,
  fog_SetDensity,
  fog_SetBeginEnd,
  
  // SkyBox
  skybox_Init,
  skybox_Draw,

  shadow_InitVolume,
  shadow_CalcVolume,
  shadow_DrawVolume,
  shadow_DrawShadowVolumes,

  // Shaders
  // asm
  shader_InitARB,
  shader_LoadFromFileARB,
  shader_BeginARB,
  shader_EndARB,
  shader_FreeARB,
  // glsl
  shader_InitGLSL,
  shader_LoadFromFile,
  shader_Attach,
  shader_BeginLink,
  shader_EndLink,
  shader_Begin,
  shader_End,
  shader_Free,
  shader_GetUniform,
  shader_SetUniform1f,
  shader_SetUniform1i,
  shader_SetUniform2f,
  shader_SetUniform3f,
  shader_SetUniform4f,
  shader_GetAttrib,
  shader_SetAttrib1f,
  shader_SetAttrib2f,
  shader_SetAttrib3f,
  shader_SetAttrib4f,
  shader_SetAttribPf,
  shader_SetParameter4f,
{$ENDIF USE_3D}
  
  // Math
  //
  m_Cos,
  m_Sin,
  m_SinCos,
  m_Distance,
  m_FDistance,
  m_Angle,
{$IFDEF USE_3D}
  // vectors
  vector_Get,
  vector_Add,
  vector_Sub,
  vector_Mul,
  vector_Div,
  vector_AddV,
  vector_SubV,
  vector_MulV,
  vector_DivV,
  vector_MulM3f,
  vector_MulM4f,
  vector_MulInvM4f,
  vector_RotateX,
  vector_RotateY,
  vector_RotateZ,
  vector_RotateQ,
  vector_Negate,
  vector_Normalize,
  vector_Angle,
  vector_Cross,
  vector_Dot,
  vector_Distance,
  vector_FDistance,
  vector_Length,
  vector_Lerp,
  // matrix
  matrix3f_Get,
  matrix3f_OrthoNormalize,
  matrix3f_Transpose,
  matrix3f_Rotate,
  matrix3f_Add,
  matrix3f_Mul,
  matrix4f_Transpose,
  matrix4f_Translate,
  matrix4f_Determinant,
  matrix4f_Inverse,
  matrix4f_Rotate,
  matrix4f_Scale,
  matrix4f_Mul,
  // quaternions
  quater_Get,
  quater_Add,
  quater_Sub,
  quater_Mul,
  quater_Negate,
  quater_Normalize,
  quater_Dot,
  quater_Lerp,
  quater_FromRotation,
  quater_GetM4f,
  // line 3d
  line3d_ClosestPoint,
  // plane
  plane_Get,
  plane_Distance,
  // triangles
  tri_GetNormal,
{$ENDIF}
  
  // Collision 2D
  col2d_PointInRect,
  col2d_PointInCircle,
  col2d_PointInPolyLine,
  col2d_Line,
  col2d_LineVsRect,
  col2d_LineVsCircle,
  col2d_LineVsCircleXY,
  col2d_LineVsPolyLine,
  col2d_PolyLine,
  col2d_PolyLineVsRect,
  col2d_PolyLineVsCircle,
  col2d_PolyLineVsCircleXY,
  col2d_Rect,
  col2d_RectInRect,
  col2d_RectInCircle,
  col2d_RectVsCircle,
  col2d_Circle,
  col2d_CircleInCircle,
  col2d_CircleInRect,
  col2dEx_LastX,
  col2dEx_LastY,
  col2dEx_LastLineA,
  col2dEx_LastLineB,
  col2dEx_PolyRotate,
  col2dEx_PolyScale,
  col2dEx_PolyMove,
  col2dEx_PolyCenter,
  col2dEx_PolyRect,
  col2dEx_CalcLineCross,
  
{$IFDEF USE_3D}
  // Collision 3D
  col3d_PointInTri,
  col3d_PointInAABB,
  col3d_PointInOBB,
  col3d_PointInSphere,
  col3d_LineVsAABB,
  col3d_LineVsOBB,
  col3d_LineVsSphere,
  col3d_PlaneVsSphere,
  col3d_AABBVsAABB,
  col3d_AABBVsOBB,
  col3d_AABBVsSphere,
  col3d_OBBVsOBB,
  col3d_OBBVsSphere,
  col3d_SphereVsSphere,
  col3d_SphereVsNode,
{$ENDIF USE_3D}
  
  // Utils
  file_Open,
  file_Exists,
  file_Seek,
  file_GetPos,
  file_Read,
  file_Write,
  file_Trunc,
  file_GetSize,
  file_Flush,
  file_Close,

  mem_LoadFromFile,
  mem_SaveToFile,
  mem_Seek,
  mem_Read,
  mem_Write,
  mem_SetSize,
  mem_Free
  ;

{$IFDEF WIN32}
var
  SysInfo : _SYSTEM_INFO;
{$ENDIF}
begin
  {$IFDEF WIN32}
  // Багнутое MS-поделко требует патча :)
  // Вешаем движок на одно ядро
  GetSystemInfo( SysInfo );
  SetProcessAffinityMask( GetCurrentProcess, SysInfo.dwActiveProcessorMask );
  
  wnd_INST := hInstance;
  {$ENDIF}
end.
