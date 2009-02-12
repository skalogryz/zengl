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
  zgl_opengl_simple,
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

const
// Ненавижу Apple! :)
  {$IFDEF DARWIN}
  prefix = '_';
  {$ELSE}
  prefix = '';
  {$ENDIF}

exports
  // Main
  zgl_Init                 name prefix + 'zgl_Init',
  {$IFDEF WIN32}
  zgl_InitToHandle,
  {$ENDIF}
  zgl_Exit                 name prefix + 'zgl_Exit',
  zgl_Reg                  name prefix + 'zgl_Reg',
  zgl_Get                  name prefix + 'zgl_Get',
  zgl_GetMem               name prefix + 'zgl_GetMem',
  zgl_Enable               name prefix + 'zgl_Enable',
  zgl_Disable              name prefix + 'zgl_Disable',
  log_Add                  name prefix + 'log_Add',

  // Window
  wnd_SetCaption           name prefix + 'wnd_SetCaption',
  wnd_SetSize              name prefix + 'wnd_SetSize',
  wnd_SetPos               name prefix + 'wnd_SetPos',
  wnd_SetOnTop             name prefix + 'wnd_SetOnTop',
  wnd_ShowCursor           name prefix + 'wnd_ShowCursor',

  // Screen
  scr_Clear                name prefix + 'scr_Clear',
  scr_Flush                name prefix + 'scr_Flush',
  scr_SetVSync             name prefix + 'scr_SetVSync',
  scr_SetFSAA              name prefix + 'scr_SetFSAA',
  scr_SetOptions           name prefix + 'scr_SetOptions',
  scr_CorrectResolution    name prefix + 'scr_CorrectResolution',

  // INI
  ini_LoadFromFile         name prefix + 'ini_LoadFromFile',
  ini_SaveToFile           name prefix + 'ini_SaveToFile',
  ini_Add                  name prefix + 'ini_Add',
  ini_ReadKeyStr           name prefix + 'ini_ReadKeyStr',
  ini_ReadKeyInt           name prefix + 'ini_ReadKeyInt',
  ini_ReadKeyBool          name prefix + 'ini_ReadKeyBool',
  ini_WriteKeyStr          name prefix + 'ini_WriteKeyStr',
  ini_WriteKeyInt          name prefix + 'ini_WriteKeyInt',
  ini_WriteKeyBool         name prefix + 'ini_WriteKeyBool',

  // Timers
  timer_Add                name prefix + 'timer_Add',
  timer_Del                name prefix + 'timer_Del',
  timer_GetTicks           name prefix + 'timer_GetTicks',
  timer_Reset              name prefix + 'timer_Reset',

  // Keyboard
  key_Down                 name prefix + 'key_Down',
  key_Up                   name prefix + 'key_Up',
  key_Last                 name prefix + 'key_Last',
  key_BeginReadText        name prefix + 'key_BeginReadText',
  key_EndReadText          name prefix + 'key_EndReadText',
  key_ClearState           name prefix + 'key_ClearState',

  // Mouse
  mouse_X                  name prefix + 'mouse_X',
  mouse_Y                  name prefix + 'mouse_Y',
  mouse_DX                 name prefix + 'mouse_DX',
  mouse_DY                 name prefix + 'mouse_DY',
  mouse_Down               name prefix + 'mouse_Down',
  mouse_Up                 name prefix + 'mouse_Up',
  mouse_Click              name prefix + 'mouse_Click',
  mouse_Wheel              name prefix + 'mouse_Wheel',
  mouse_ClearState         name prefix + 'mouse_ClearState',
  mouse_Lock               name prefix + 'mouse_Lock',

  // Textures
  tex_Add                  name prefix + 'tex_Add',
  tex_Del                  name prefix + 'tex_Del',
  tex_Create               name prefix + 'tex_Create',
  tex_CreateZero           name prefix + 'tex_CreateZero',
  tex_LoadFromFile         name prefix + 'tex_LoadFromFile',
  tex_LoadFromMemory       name prefix + 'tex_LoadFromMemory',
  tex_SetFrameSize         name prefix + 'tex_SetFrameSize',
  tex_SetMask              name prefix + 'tex_SetMask',
  tex_GetData              name prefix + 'tex_GetData',
  tex_Filter               name prefix + 'tex_Filter',
  tex_SetAnisotropy        name prefix + 'tex_SetAnisotropy',

  // OpenGL
  Set2DMode                name prefix + 'Set2DMode',
  Set3DMode                name prefix + 'Set3DMode',

  // Z Buffer
  zbuffer_SetDepth         name prefix + 'zbuffer_SetDepth',
  zbuffer_Clear            name prefix + 'zbuffer_Clear',

  // Scissor
  scissor_Begin            name prefix + 'scissor_Begin',
  scissor_End              name prefix + 'scissor_End',

  // Render Targets
  rtarget_Add              name prefix + 'rtarget_Add',
  rtarget_Del              name prefix + 'rtarget_Del',
  rtarget_Set              name prefix + 'rtarget_Set',

  // FX
  fx_SetBlendMode          name prefix + 'fx_SetBlendMode',
  // FX 2D
  fx2d_SetColorMix         name prefix + 'fx2d_SetColorMix',
  fx2d_SetVCA              name prefix + 'fx2d_SetVCA',
  fx2d_SetVertexes         name prefix + 'fx2d_SetVertexes',
  fx2d_SetScale            name prefix + 'fx2d_SetScale',

  // Camera 2D
  cam2d_Set                name prefix + 'cam2d_Set',

  // Primitives 2D
  pr2d_Pixel               name prefix + 'pr2d_Pixel',
  pr2d_Line                name prefix + 'pr2d_Line',
  pr2d_Triangle            name prefix + 'pr2d_Triangle',
  pr2d_Rect                name prefix + 'pr2d_Rect',
  pr2d_Circle              name prefix + 'pr2d_Circle',
  pr2d_Ellipse             name prefix + 'pr2d_Ellipse',

  // Sprite 2D
  ssprite2d_Draw           name prefix + 'ssprite2d_Draw',
  asprite2d_Draw           name prefix + 'asprite2d_Draw',
  csprite2d_Draw           name prefix + 'csprite2d_Draw',

  // Text
  font_Add                 name prefix + 'font_Add',
  font_Del                 name prefix + 'font_Del',
  font_LoadFromFile        name prefix + 'font_LoadFromFile',
  text_Draw                name prefix + 'text_Draw',
  text_GetWidth            name prefix + 'text_GetWidth',

{$IFDEF USE_GUI}
  gui_Init                 name prefix + 'gui_Init',
  gui_Draw                 name prefix + 'gui_Draw',
  gui_Proc                 name prefix + 'gui_Proc',
  gui_AddWidget            name prefix + 'gui_AddWidget',
  gui_DelWidget            name prefix + 'gui_DelWidget',
{$ENDIF}

{$IFDEF USE_SOUND}
  // Sound
  snd_Init                 name prefix + 'snd_Init',
  snd_Free                 name prefix + 'snd_Free',
  snd_Add                  name prefix + 'snd_Add',
  snd_Del                  name prefix + 'snd_Del',
  snd_LoadFromFile         name prefix + 'snd_LoadFromFile',
  snd_LoadFromMemory       name prefix + 'snd_LoadFromMemory',
  snd_Play                 name prefix + 'snd_Play',
  snd_Stop                 name prefix + 'snd_Stop',
  snd_SetVolume            name prefix + 'snd_SetVolume',
  snd_SetFrequency         name prefix + 'snd_SetFrequency',
  snd_SetFrequencyCoeff    name prefix + 'snd_SetFrequencyCoeff',
  snd_PlayFile             name prefix + 'snd_PlayFile',
  snd_StopFile             name prefix + 'snd_StopFile',
  snd_RestoreFile          name prefix + 'snd_RestoreFile',
{$ENDIF}

{$IFDEF USE_3D}
  // Object 3D
  obj3d_Begin              name prefix + 'obj3d_Begin',
  obj3d_End                name prefix + 'obj3d_End',
  obj3d_Enable             name prefix + 'obj3d_Enable',
  obj3d_Disable            name prefix + 'obj3d_Disable',
  // material
  obj3d_SetColor           name prefix + 'obj3d_SetColor',
  obj3d_BindTexture        name prefix + 'obj3d_BindTexture',
  obj3d_SetMaterial        name prefix + 'obj3d_SetMaterial',
  // transform
  obj3d_Rotate             name prefix + 'obj3d_Rotate',
  obj3d_Scale              name prefix + 'obj3d_Scale',
  obj3d_Move               name prefix + 'obj3d_Move',
  obj3d_SetMatrix          name prefix + 'obj3d_SetMatrix',
  obj3d_MulMatrix          name prefix + 'obj3d_MulMatrix',

  // Primitives 3D
  pr3d_Point               name prefix + 'pr3d_Point',
  pr3d_Line                name prefix + 'pr3d_Line',
  pr3d_Plane               name prefix + 'pr3d_Plane',
  pr3d_AABB                name prefix + 'pr3d_AABB',
  pr3d_Sphere              name prefix + 'pr3d_Sphere',

  // Sprite 3D
  ssprite3d_Draw           name prefix + 'ssprite3d_Draw',
  asprite3d_Draw           name prefix + 'asprite3d_Draw',

  // Camera 3D
  cam3d_Set                name prefix + 'cam3d_Set',
  cam3d_Fly                name prefix + 'cam3d_Fly',
  cam3d_Strafe             name prefix + 'cam3d_Strafe',

  // Static Mesh
  smesh_LoadFromFile       name prefix + 'smesh_LoadFromFile',
  smesh_LoadFromMemory     name prefix + 'smesh_LoadFromMemory',
  smesh_Animate            name prefix + 'smesh_Animate',
  smesh_Draw               name prefix + 'smesh_Draw',
  smesh_DrawGroup          name prefix + 'smesh_DrawGroup',
  smesh_Free               name prefix + 'smesh_Free',

  // Skinned Mesh
  skmesh_LoadFromFile      name prefix + 'skmesh_LoadFromFile',
  skmesh_LoadFromMemory    name prefix + 'skmesh_LoadFromMemory',
  skmesh_Animate           name prefix + 'skmesh_Animate',
  skmesh_Draw              name prefix + 'skmesh_Draw',
  skmesh_DrawGroup         name prefix + 'skmesh_DrawGroup',
  skmesh_DrawSkelet        name prefix + 'skmesh_DrawSkelet',
  skmesh_Free              name prefix + 'skmesh_Free',

  // HeightMap
  heightmap_Build          name prefix + 'heightmap_Build',
  heightmap_Draw           name prefix + 'heightmap_Draw',
  heightmap_Free           name prefix + 'heightmap_Free',
  heightmap_GetPlane       name prefix + 'heightmap_GetPlane',
  heightmap_GetYOffset     name prefix + 'heightmap_GetYOffset',

  // VBO
  vbo_Build                name prefix + 'vbo_Build',
  vbo_Free                 name prefix + 'vbo_Free',

  // Frustum
  frustum_Calc             name prefix + 'frustum_Calc',
  frustum_PointIn          name prefix + 'frustum_PointIn',
  frustum_PPointIn         name prefix + 'frustum_PPointIn',
  frustum_TriangleIn       name prefix + 'frustum_TriangleIn',
  frustum_SphereIn         name prefix + 'frustum_SphereIn',
  frustum_BoxIn            name prefix + 'frustum_BoxIn',
  frustum_CubeIn           name prefix + 'frustum_CubeIn',

  // Octree
  octree_Build             name prefix + 'octree_Build',
  octree_Free              name prefix + 'octree_Free',
  octree_Draw              name prefix + 'octree_Draw',
  octree_DrawDebug         name prefix + 'octree_DrawDebug',
  octree_DrawNode          name prefix + 'octree_DrawNode',
  octree_DrawDFaces        name prefix + 'octree_DrawDFaces',

  // Light
  light_Enable             name prefix + 'light_Enable',
  light_Disable            name prefix + 'light_Disable',
  light_SetPosition        name prefix + 'light_SetPosition',
  light_SetMaterial        name prefix + 'light_SetMaterial',

  // Fog
  fog_Enable               name prefix + 'fog_Enable',
  fog_Disable              name prefix + 'fog_Disable',
  fog_SetMode              name prefix + 'fog_SetMode',
  fog_SetColor             name prefix + 'fog_SetColor',
  fog_SetDensity           name prefix + 'fog_SetDensity',
  fog_SetBeginEnd          name prefix + 'fog_SetBeginEnd',

  // SkyBox
  skybox_Init              name prefix + 'skybox_Init',
  skybox_Draw              name prefix + 'skybox_Draw',

  shadow_InitVolume        name prefix + 'shadow_InitVolume',
  shadow_CalcVolume        name prefix + 'shadow_CalcVolume',
  shadow_DrawVolume        name prefix + 'shadow_DrawVolume',
  shadow_DrawShadowVolumes name prefix + 'shadow_DrawShadowVolumes',

  // Shaders
  // asm
  shader_InitARB           name prefix + 'shader_InitARB',
  shader_LoadFromFileARB   name prefix + 'shader_LoadFromFileARB',
  shader_BeginARB          name prefix + 'shader_BeginARB',
  shader_EndARB            name prefix + 'shader_EndARB',
  shader_FreeARB           name prefix + 'shader_FreeARB',
  // glsl
  shader_InitGLSL          name prefix + 'shader_InitGLSL',
  shader_LoadFromFile      name prefix + 'shader_LoadFromFile',
  shader_Attach            name prefix + 'shader_Attach',
  shader_BeginLink         name prefix + 'shader_BeginLink',
  shader_EndLink           name prefix + 'shader_EndLink',
  shader_Begin             name prefix + 'shader_Begin',
  shader_End               name prefix + 'shader_End',
  shader_Free              name prefix + 'shader_Free',
  shader_GetUniform        name prefix + 'shader_GetUniform',
  shader_SetUniform1f      name prefix + 'shader_SetUniform1f',
  shader_SetUniform1i      name prefix + 'shader_SetUniform1i',
  shader_SetUniform2f      name prefix + 'shader_SetUniform2f',
  shader_SetUniform3f      name prefix + 'shader_SetUniform3f',
  shader_SetUniform4f      name prefix + 'shader_SetUniform4f',
  shader_GetAttrib         name prefix + 'shader_GetAttrib',
  shader_SetAttrib1f       name prefix + 'shader_SetAttrib1f',
  shader_SetAttrib2f       name prefix + 'shader_SetAttrib2f',
  shader_SetAttrib3f       name prefix + 'shader_SetAttrib3f',
  shader_SetAttrib4f       name prefix + 'shader_SetAttrib4f',
  shader_SetAttribPf       name prefix + 'shader_SetAttribPf',
  shader_SetParameter4f    name prefix + 'shader_SetParameter4f',
{$ENDIF USE_3D}

  // Math
  //
  m_Cos                    name prefix + 'm_Cos',
  m_Sin                    name prefix + 'm_Sin',
  m_SinCos                 name prefix + 'm_SinCos',
  m_Distance               name prefix + 'm_Distance',
  m_FDistance              name prefix + 'm_FDistance',
  m_Angle                  name prefix + 'm_Angle',
{$IFDEF USE_3D}
  // vectors
  vector_Get               name prefix + 'vector_Get',
  vector_Add               name prefix + 'vector_Add',
  vector_Sub               name prefix + 'vector_Sub',
  vector_Mul               name prefix + 'vector_Mul',
  vector_Div               name prefix + 'vector_Div',
  vector_AddV              name prefix + 'vector_AddV',
  vector_SubV              name prefix + 'vector_SubV',
  vector_MulV              name prefix + 'vector_MulV',
  vector_DivV              name prefix + 'vector_DivV',
  vector_MulM3f            name prefix + 'vector_MulM3f',
  vector_MulM4f            name prefix + 'vector_MulM4f',
  vector_MulInvM4f         name prefix + 'vector_MulInvM4f',
  vector_RotateX           name prefix + 'vector_RotateX',
  vector_RotateY           name prefix + 'vector_RotateY',
  vector_RotateZ           name prefix + 'vector_RotateZ',
  vector_RotateQ           name prefix + 'vector_RotateQ',
  vector_Negate            name prefix + 'vector_Negate',
  vector_Normalize         name prefix + 'vector_Normalize',
  vector_Angle             name prefix + 'vector_Angle',
  vector_Cross             name prefix + 'vector_Cross',
  vector_Dot               name prefix + 'vector_Dot',
  vector_Distance          name prefix + 'vector_Distance',
  vector_FDistance         name prefix + 'vector_FDistance',
  vector_Length            name prefix + 'vector_Length',
  vector_Lerp              name prefix + 'vector_Lerp',
  // matrix
  matrix3f_Get             name prefix + 'matrix3f_Get',
  matrix3f_OrthoNormalize  name prefix + 'matrix3f_OrthoNormalize',
  matrix3f_Transpose       name prefix + 'matrix3f_Transpose',
  matrix3f_SetRot          name prefix + 'matrix3f_SetRot',
  matrix3f_Add             name prefix + 'matrix3f_Add',
  matrix3f_Mul             name prefix + 'matrix3f_Mul',
  matrix4f_Transpose       name prefix + 'matrix4f_Transpose',
  matrix4f_Translate       name prefix + 'matrix4f_Translate',
  matrix4f_Determinant     name prefix + 'matrix4f_Determinant',
  matrix4f_Inverse         name prefix + 'matrix4f_Inverse',
  matrix4f_SetPos          name prefix + 'matrix4f_SetPos',
  matrix4f_SetRot          name prefix + 'matrix4f_SetRot',
  matrix4f_Scale           name prefix + 'matrix4f_Scale',
  matrix4f_Mul             name prefix + 'matrix4f_Mul',
  // quaternions
  quater_Get               name prefix + 'quater_Get',
  quater_Add               name prefix + 'quater_Add',
  quater_Sub               name prefix + 'quater_Sub',
  quater_Mul               name prefix + 'quater_Mul',
  quater_Negate            name prefix + 'quater_Negate',
  quater_Normalize         name prefix + 'quater_Normalize',
  quater_Dot               name prefix + 'quater_Dot',
  quater_Lerp              name prefix + 'quater_Lerp',
  quater_FromRotation      name prefix + 'quater_FromRotation',
  quater_GetM4f            name prefix + 'quater_GetM4f',
  // line 3d
  line3d_ClosestPoint      name prefix + 'line3d_ClosestPoint',
  // plane
  plane_Get                name prefix + 'plane_Get',
  plane_Distance           name prefix + 'plane_Distance',
  // triangles
  tri_GetNormal            name prefix + 'tri_GetNormal',
{$ENDIF}

  // Collision 2D
  col2d_PointInRect        name prefix + 'col2d_PointInRect',
  col2d_PointInCircle      name prefix + 'col2d_PointInCircle',
  col2d_PointInPolyLine    name prefix + 'col2d_PointInPolyLine',
  col2d_Line               name prefix + 'col2d_Line',
  col2d_LineVsRect         name prefix + 'col2d_LineVsRect',
  col2d_LineVsCircle       name prefix + 'col2d_LineVsCircle',
  col2d_LineVsCircleXY     name prefix + 'col2d_LineVsCircleXY',
  col2d_LineVsPolyLine     name prefix + 'col2d_LineVsPolyLine',
  col2d_PolyLine           name prefix + 'col2d_PolyLine',
  col2d_PolyLineVsRect     name prefix + 'col2d_PolyLineVsRect',
  col2d_PolyLineVsCircle   name prefix + 'col2d_PolyLineVsCircle',
  col2d_PolyLineVsCircleXY name prefix + 'col2d_PolyLineVsCircleXY',
  col2d_Rect               name prefix + 'col2d_Rect',
  col2d_RectInRect         name prefix + 'col2d_RectInRect',
  col2d_RectInCircle       name prefix + 'col2d_RectInCircle',
  col2d_RectVsCircle       name prefix + 'col2d_RectVsCircle',
  col2d_Circle             name prefix + 'col2d_Circle',
  col2d_CircleInCircle     name prefix + 'col2d_CircleInCircle',
  col2d_CircleInRect       name prefix + 'col2d_CircleInRect',
  col2dEx_LastX            name prefix + 'col2dEx_LastX',
  col2dEx_LastY            name prefix + 'col2dEx_LastY',
  col2dEx_LastLineA        name prefix + 'col2dEx_LastLineA',
  col2dEx_LastLineB        name prefix + 'col2dEx_LastLineB',
  col2dEx_PolyRotate       name prefix + 'col2dEx_PolyRotate',
  col2dEx_PolyScale        name prefix + 'col2dEx_PolyScale',
  col2dEx_PolyMove         name prefix + 'col2dEx_PolyMove',
  col2dEx_PolyCenter       name prefix + 'col2dEx_PolyCenter',
  col2dEx_PolyRect         name prefix + 'col2dEx_PolyRect',
  col2dEx_CalcLineCross    name prefix + 'col2dEx_CalcLineCross',

{$IFDEF USE_3D}
  // Collision 3D
  col3d_PointInTri         name prefix + 'col3d_PointInTri',
  col3d_PointInAABB        name prefix + 'col3d_PointInAABB',
  col3d_PointInOBB         name prefix + 'col3d_PointInOBB',
  col3d_PointInSphere      name prefix + 'col3d_PointInSphere',
  col3d_LineVsAABB         name prefix + 'col3d_LineVsAABB',
  col3d_LineVsOBB          name prefix + 'col3d_LineVsOBB',
  col3d_LineVsSphere       name prefix + 'col3d_LineVsSphere',
  col3d_PlaneVsSphere      name prefix + 'col3d_PlaneVsSphere',
  col3d_AABBVsAABB         name prefix + 'col3d_AABBVsAABB',
  col3d_AABBVsOBB          name prefix + 'col3d_AABBVsOBB',
  col3d_AABBVsSphere       name prefix + 'col3d_AABBVsSphere',
  col3d_OBBVsOBB           name prefix + 'col3d_OBBVsOBB',
  col3d_OBBVsSphere        name prefix + 'col3d_OBBVsSphere',
  col3d_SphereVsSphere     name prefix + 'col3d_SphereVsSphere',
  col3d_SphereVsNode       name prefix + 'col3d_SphereVsNode',
{$ENDIF USE_3D}

  // Utils
  file_Open                name prefix + 'file_Open',
  file_Exists              name prefix + 'file_Exists',
  file_Seek                name prefix + 'file_Seek',
  file_GetPos              name prefix + 'file_GetPos',
  file_Read                name prefix + 'file_Read',
  file_Write               name prefix + 'file_Write',
  file_Trunc               name prefix + 'file_Trunc',
  file_GetSize             name prefix + 'file_GetSize',
  file_Flush               name prefix + 'file_Flush',
  file_Close               name prefix + 'file_Close',
  file_Find                name prefix + 'file_Find',

  mem_LoadFromFile         name prefix + 'mem_LoadFromFile',
  mem_SaveToFile           name prefix + 'mem_SaveToFile',
  mem_Seek                 name prefix + 'mem_Seek',
  mem_Read                 name prefix + 'mem_Read',
  mem_Write                name prefix + 'mem_Write',
  mem_SetSize              name prefix + 'mem_SetSize',
  mem_Free                 name prefix + 'mem_Free'
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
