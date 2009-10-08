program demo05;

uses
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_camera_2d,
  zgl_render_2d,
  zgl_fx,
  zgl_textures,
  zgl_textures_png, // Важный момент, обязательно один раз подключить модуль с поддержкой нужного формата данных
  zgl_textures_jpg,
  zgl_sprite_2d,
  zgl_primitives_2d,
  zgl_font,
  zgl_text,
  zgl_math_2d,
  zgl_utils;

type
  TTux = record
    Texture : zglPTexture;
    Frame   : Integer;
    Pos     : zglTPoint2D;
end;

var
  fntMain     : zglPFont;
  texLogo     : zglPTexture;
  texBack     : zglPTexture;
  texGround   : zglPTexture;
  texTuxWalk  : zglPTexture;
  texTuxStand : zglPTexture;
  tux         : array[ 0..20 ] of TTux;
  time        : Integer;
  camMain     : zglTCamera2D;

procedure Init;
  var
    i : Integer;
begin
  // Т.к. по умолчанию вся структура камеры заполняется нулями, следует для масштаба установить 1
  camMain.Zoom.X := 1;
  camMain.Zoom.Y := 1;

  // Загружаем текстуру
  // $FF000000 - указывает на то, что бы использовать альфа-канал из изображения
  // TEX_DEFAULT_2D - комплекс флагов, необходимых для 2D-спрайтов. Описание есть в справке
  texLogo := tex_LoadFromFile( '../res/zengl.png', $FF000000, TEX_DEFAULT_2D );

  texBack := tex_LoadFromFile( '../res/back01.jpg', $FF000000, TEX_DEFAULT_2D );

  texGround := tex_LoadFromFile( '../res/ground.png', $FF000000, TEX_DEFAULT_2D );
  // Указываем размер кадра в текстуре
  tex_SetFrameSize( texGround, 32, 32 );

  texTuxWalk := tex_LoadFromFile( '../res/tux_walking.png', $FF000000, TEX_DEFAULT_2D );
  tex_SetFrameSize( texTuxWalk, 64, 64 );
  texTuxStand := tex_LoadFromFile( '../res/tux_stand.png', $FF000000, TEX_DEFAULT_2D );
  tex_SetFrameSize( texTuxStand, 64, 64 );

  for i := 0 to 9 do
    begin
      tux[ i ].Texture := texTuxWalk;
      tux[ i ].Frame   := random( 19 ) + 2;
      tux[ i ].Pos.X   := i * 96;
      tux[ i ].Pos.Y   := 32;
    end;
  for i := 10 to 19 do
    begin
      tux[ i ].Texture := texTuxWalk;
      tux[ i ].Frame   := random( 19 ) + 2;
      tux[ i ].Pos.X   := ( i - 9 ) * 96;
      tux[ i ].Pos.Y   := 600 - 96;
    end;
  tux[ 20 ].Texture := texTuxStand;
  tux[ 20 ].Frame   := random( 19 ) + 2;
  tux[ 20 ].Pos.X   := 400 - 32;
  tux[ 20 ].Pos.Y   := 300 - 64 - 4;

  // Загружаем шрифт
  fntMain := font_LoadFromFile( '../res/font.zfi' );
  for i := 0 to fntMain.Count.Pages - 1 do
    fntMain.Pages[ i ] := tex_LoadFromFile( '../res/font_' + u_IntToStr( i ) + '.png', $FF000000, TEX_DEFAULT_2D );
end;

procedure Draw;
  var
    i : Integer;
    t : Single;
begin
  batch2d_Begin;
  if time > 255 Then
    begin
      // Для увеличения быстродействия можно отключить очистку буфера цвета,
      // учитывая что экран полностью заполнен
      zgl_Disable( COLOR_BUFFER_CLEAR );

      // Рисуем задний фон с размерами 800х600 используя текстуру back
      ssprite2d_Draw( texBack, 0, 0, 800, 600, 0 );

      // "Применить" текущие параметры камеры
      cam2d_Apply( @camMain );
      // Код ниже является более гибким вариантом. Он позволяет единажды вызвать установку камеры,
      // и любое изменение параметров camMain тут же будет влиять на вывод чего-либо, но этот вариант
      // уступает по быстродействию
      // cam2d_Set( @camMain );

      // Рисуем землю
      for i := -2 to 800 div 32 + 1 do
        asprite2d_Draw( texGround, i * 32, 96 - 12, 32, 32, 0, 2 );
      for i := -2 to 800 div 32 + 1 do
        asprite2d_Draw( texGround, i * 32, 600 - 32 - 12, 32, 32, 0, 2 );

      // Рисуем шагающих пингвинов
      for i := 0 to 9 do
        if i = 2 Then
          begin
            // Рисуем надпись в "рамочке" над пингвином
            t := text_GetWidth( fntMain, 'I''m so red...' ) * 0.75 + 4;
            pr2d_Rect( tux[ i ].Pos.X - 2, tux[ i ].Pos.Y - fntMain.MaxHeight + 4, t, fntMain.MaxHeight, $000000, 200, PR2D_FILL );
            pr2d_Rect( tux[ i ].Pos.X - 2, tux[ i ].Pos.Y - fntMain.MaxHeight + 4, t, fntMain.MaxHeight, $FFFFFF );
            text_DrawEx( fntMain, tux[ i ].Pos.X, tux[ i ].Pos.Y - fntMain.MaxHeight + 8, 0.75, 0, 'I''m so red...' );
            // Рисуем красного пингвина используя fx2d-функцию и флаг FX2D_COLORMIX
            fx2d_SetColorMix( $FF0000 );
            asprite2d_Draw( tux[ i ].Texture, tux[ i ].Pos.X, tux[ i ].Pos.Y, 64, 64, 0, tux[ i ].Frame div 2, 255, FX_BLEND or FX2D_COLORMIX );
          end else
            if i = 7 Then
              begin
                t := text_GetWidth( fntMain, '???' ) * 0.75 + 4;
                pr2d_Rect( tux[ i ].Pos.X + 32 - t / 2, tux[ i ].Pos.Y - fntMain.MaxHeight + 4, t, fntMain.MaxHeight, $000000, 200, PR2D_FILL );
                pr2d_Rect( tux[ i ].Pos.X + 32 - t / 2, tux[ i ].Pos.Y - fntMain.MaxHeight + 4, t, fntMain.MaxHeight, $FFFFFF );
                text_DrawEx( fntMain, tux[ i ].Pos.X + 32, tux[ i ].Pos.Y - fntMain.MaxHeight + 8, 0.75, 0, '???', 255, $FFFFFF, TEXT_HALIGN_CENTER );
                // Рисуем пингвина приведение используя флаг FX2D_COLORSET :)
                fx2d_SetColorMix( $FFFFFF );
                asprite2d_Draw( tux[ i ].Texture, tux[ i ].Pos.X, tux[ i ].Pos.Y, 64, 64, 0, tux[ i ].Frame div 2, 155, FX_BLEND or FX2D_COLORSET );
              end else
                asprite2d_Draw( tux[ i ].Texture, tux[ i ].Pos.X, tux[ i ].Pos.Y, 64, 64, 0, tux[ i ].Frame div 2 );

      // Рисуем пингвинов шагающих в обратную сторону используя флаг отражения текстуры FX2D_FLIPX
      for i := 10 to 19 do
        if i = 13 Then
          begin
            t := text_GetWidth( fntMain, 'I''m so big...' ) * 0.75 + 4;
            pr2d_Rect( tux[ i ].Pos.X - 2, tux[ i ].Pos.Y - fntMain.MaxHeight - 10, t, fntMain.MaxHeight, $000000, 200, PR2D_FILL );
            pr2d_Rect( tux[ i ].Pos.X - 2, tux[ i ].Pos.Y - fntMain.MaxHeight - 10, t, fntMain.MaxHeight, $FFFFFF );
            text_DrawEx( fntMain, tux[ i ].Pos.X, tux[ i ].Pos.Y - fntMain.MaxHeight - 4, 0.75, 0, 'I''m so big...' );
            // Рисуем "большего" пингвина. Т.к. FX2D_SCALE увеличивает спрайт относительно центра, то пингвина следует немного "поднять"
            fx2d_SetScale( 1.25, 1.25 );
            asprite2d_Draw( tux[ i ].Texture, tux[ i ].Pos.X, tux[ i ].Pos.Y - 8, 64, 64, 0, tux[ i ].Frame div 2, 255, FX_BLEND or FX2D_FLIPX or FX2D_SCALE );
          end else
            if i = 17 Then
              begin
                // Рисуем "высокого" пингвина используя вместое флага FX2D_SCALE флаг FX2D_VCHANGE и функцию fx2d_SetVertexes
                // для смещения координат двух верхних вершин спрайта
                fx2d_SetVertexes( 0, -16, 0, -16, 0, 0, 0, 0 );
                asprite2d_Draw( tux[ i ].Texture, tux[ i ].Pos.X, tux[ i ].Pos.Y, 64, 64, 0, tux[ i ].Frame div 2, 255, FX_BLEND or FX2D_FLIPX or FX2D_VCHANGE );
              end else
                asprite2d_Draw( tux[ i ].Texture, tux[ i ].Pos.X, tux[ i ].Pos.Y, 64, 64, 0, tux[ i ].Frame div 2, 255, FX_BLEND or FX2D_FLIPX );

      // Применить стандартное положение
      cam2d_Apply( nil );
      // Если же использовался вариант с cam2d_Set, то и сбрасывать следует этой же командой
      // cam2d_Set( nil );

      // Рисуем учатоск земли по центру экрана
      asprite2d_Draw( texGround, 11 * 32, 300 - 16, 32, 32, 0, 1 );
      asprite2d_Draw( texGround, 12 * 32, 300 - 16, 32, 32, 0, 2 );
      asprite2d_Draw( texGround, 13 * 32, 300 - 16, 32, 32, 0, 3 );

      t := text_GetWidth( fntMain, 'o_O' ) * 0.75 + 4;
      pr2d_Rect( tux[ 20 ].Pos.X + 32 - t / 2, tux[ 20 ].Pos.Y - fntMain.MaxHeight + 4, t, fntMain.MaxHeight, $000000, 200, PR2D_FILL );
      pr2d_Rect( tux[ 20 ].Pos.X + 32 - t / 2, tux[ 20 ].Pos.Y - fntMain.MaxHeight + 4, t, fntMain.MaxHeight, $FFFFFF );
      text_DrawEx( fntMain, tux[ 20 ].Pos.X + 32, tux[ 20 ].Pos.Y - fntMain.MaxHeight + 8, 0.75, 0, 'o_O', 255, $FFFFFF, TEXT_HALIGN_CENTER );
      asprite2d_Draw( tux[ 20 ].Texture, tux[ 20 ].Pos.X, tux[ 20 ].Pos.Y, 64, 64, 0, tux[ 20 ].Frame div 2 );
    end;

  if time <= 255 Then
    ssprite2d_Draw( texLogo, 400 - 256, 300 - 128, 512, 256, 0, time )
  else
    if time < 510 Then
      begin
        pr2d_Rect( 0, 0, 800, 600, $000000, 510 - time, PR2D_FILL );
        ssprite2d_Draw( texLogo, 400 - 256, 300 - 128, 512, 256, 0, 510 - time );
      end;

  if time > 255 Then
    text_Draw( fntMain, 0, 0, 'FPS: ' + u_IntToStr( zgl_Get( SYS_FPS ) ) );
  batch2d_End;
end;

procedure Timer;
  var
    i : Integer;
begin
  INC( time, 2 );

  camMain.Angle := camMain.Angle + cos( time / 1000 ) / 10;

  for i := 0 to 20 do
    begin
      INC( tux[ i ].Frame );
      if tux[ i ].Frame > 20 Then
        tux[ i ].Frame := 2;
    end;
  for i := 0 to 9 do
    begin
      tux[ i ].Pos.X := tux[ i ].Pos.X + 1.5;
      if tux[ i ].Pos.X > 864 Then
        tux[ i ].Pos.X := -96;
    end;
  for i := 10 to 19 do
    begin
      tux[ i ].Pos.X := tux[ i ].Pos.X - 1.5;
      if tux[ i ].Pos.X < -96 Then
        tux[ i ].Pos.X := 864;
    end;

  if key_Press( K_ESCAPE ) Then zgl_Exit;
  key_ClearState;
end;

Begin
  randomize;

  timer_Add( @Timer, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  wnd_SetCaption( '05 - Sprites 2D' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, 32, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init;
End.
