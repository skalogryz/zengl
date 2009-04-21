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
library ZenGL;

{$I zgl_config.cfg}

uses
  zgl_const,
  zgl_types,

  zgl_main,
  zgl_application,

  zgl_screen,
  zgl_window,
  zgl_opengl,
  zgl_opengl_all,
  zgl_opengl_simple,

  zgl_timers,

  zgl_ini,
  zgl_log,

  zgl_mouse,
  zgl_keyboard,

  zgl_textures,
  zgl_textures_jpg,
  zgl_textures_png,
  zgl_textures_tga,

  zgl_render_target,

  zgl_sound,
  zgl_sound_dsound,
  zgl_sound_openal,
  zgl_sound_wav,
  zgl_sound_ogg,

  zgl_fx,
  zgl_camera_2d,

  zgl_font,
  zgl_text,

  zgl_gui_main,
  zgl_gui_types,
  zgl_gui_process,
  zgl_gui_render,
  zgl_gui_utils,

  zgl_primitives_2d,
  zgl_sprite_2d,

  zgl_file,
  zgl_memory,

  zgl_math_2d,
  zgl_math_3d,

  zgl_collision_2d,

  zgl_frustum,

  zgl_utils;

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
  zgl_GetSysDir            name prefix + 'zgl_GetSysDir',
  zgl_GetMem               name prefix + 'zgl_GetMem',
  zgl_Enable               name prefix + 'zgl_Enable',
  zgl_Disable              name prefix + 'zgl_Disable',
  log_Add                  name prefix + 'log_Add',

  // Window
  wnd_SetCaption           name prefix + 'wnd_SetCaption',
  wnd_SetSize              name prefix + 'wnd_SetSize',
  wnd_SetPos               name prefix + 'wnd_SetPos',
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
  ini_IsKey                name prefix + 'ini_IsKey',
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

  // Keyboard
  key_Down                 name prefix + 'key_Down',
  key_Up                   name prefix + 'key_Up',
  key_Last                 name prefix + 'key_Last',
  key_BeginReadText        name prefix + 'key_BeginReadText',
  key_EndReadText          name prefix + 'key_EndReadText',
  key_ClearState           name prefix + 'key_ClearState',

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
  font_LoadFromMemory      name prefix + 'font_LoadFromMemory',
  text_Draw                name prefix + 'text_Draw',
  text_DrawInRect          name prefix + 'text_DrawInRect',
  text_GetWidth            name prefix + 'text_GetWidth',

  gui_Init                 name prefix + 'gui_Init',
  gui_Draw                 name prefix + 'gui_Draw',
  gui_Proc                 name prefix + 'gui_Proc',
  gui_AddWidget            name prefix + 'gui_AddWidget',
  gui_DelWidget            name prefix + 'gui_DelWidget',

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
  snd_ResumeFile           name prefix + 'snd_ResumeFile',

  // Frustum
  frustum_Calc             name prefix + 'frustum_Calc',
  frustum_PointIn          name prefix + 'frustum_PointIn',
  frustum_PPointIn         name prefix + 'frustum_PPointIn',
  frustum_TriangleIn       name prefix + 'frustum_TriangleIn',
  frustum_SphereIn         name prefix + 'frustum_SphereIn',
  frustum_BoxIn            name prefix + 'frustum_BoxIn',
  frustum_CubeIn           name prefix + 'frustum_CubeIn',

  // Math
  //
  m_Cos                    name prefix + 'm_Cos',
  m_Sin                    name prefix + 'm_Sin',
  m_Distance               name prefix + 'm_Distance',
  m_FDistance              name prefix + 'm_FDistance',
  m_Angle                  name prefix + 'm_Angle',

  // Collision 2D
  col2d_PointInRect        name prefix + 'col2d_PointInRect',
  col2d_PointInCircle      name prefix + 'col2d_PointInCircle',
  col2d_Line               name prefix + 'col2d_Line',
  col2d_LineVsRect         name prefix + 'col2d_LineVsRect',
  col2d_LineVsCircle       name prefix + 'col2d_LineVsCircle',
  col2d_LineVsCircleXY     name prefix + 'col2d_LineVsCircleXY',
  col2d_Rect               name prefix + 'col2d_Rect',
  col2d_ClipRect           name prefix + 'col2d_ClipRect',
  col2d_RectInRect         name prefix + 'col2d_RectInRect',
  col2d_RectInCircle       name prefix + 'col2d_RectInCircle',
  col2d_RectVsCircle       name prefix + 'col2d_RectVsCircle',
  col2d_Circle             name prefix + 'col2d_Circle',
  col2d_CircleInCircle     name prefix + 'col2d_CircleInCircle',
  col2d_CircleInRect       name prefix + 'col2d_CircleInRect',

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
  file_GetName             name prefix + 'file_GetName',
  file_GetExtension        name prefix + 'file_GetExtension',
  file_Find                name prefix + 'file_Find',

  mem_LoadFromFile         name prefix + 'mem_LoadFromFile',
  mem_SaveToFile           name prefix + 'mem_SaveToFile',
  mem_Seek                 name prefix + 'mem_Seek',
  mem_Read                 name prefix + 'mem_Read',
  mem_ReadSwap             name prefix + 'mem_ReadSwap',
  mem_Write                name prefix + 'mem_Write',
  mem_SetSize              name prefix + 'mem_SetSize',
  mem_Free                 name prefix + 'mem_Free'
  ;

begin
  {$IFDEF WIN32}
  wnd_INST := hInstance;
  {$ENDIF}
end.
