program demo13;

{$I zglCustomConfig.cfg}

uses
  zgl_application,
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_touch,
  zgl_render_2d,
  zgl_fx,
  zgl_textures,
  zgl_textures_png,
  zgl_textures_jpg,
  zgl_sprite_2d,
  zgl_particles_2d,
  zgl_primitives_2d,
  zgl_font,
  zgl_text,
  zgl_math_2d,
  zgl_utils
  ;

var
  dirRes      : UTF8String = 'data/';
  fntMain     : zglPFont;
  texBack     : zglPTexture;
  texParticle : zglPTexture;
  debug       : Boolean;
  eFire       : array[ 0..6 ] of zglTEmitter2D;
  eDiamond    : zglTEmitter2D;
  eRain       : zglTEmitter2D;
  count       : Integer;

procedure Init;
  var
    i : Integer;
begin
  zgl_Enable( CORRECT_RESOLUTION );
  scr_CorrectResolution( 800, 600 );

  texBack := tex_LoadFromFile( dirRes + 'back02.png' );

  fntMain := font_LoadFromFile( dirRes + 'font.zfi' );

  texParticle := tex_LoadFromFile( dirRes + 'particle.png', $FF000000, TEX_DEFAULT_2D or TEX_MIPMAP );
  tex_SetFrameSize( texParticle, 32, 32 );

  for i := 0 to 6 do
  with eFire[ i ] do
    begin
      emitter2d_Init( @eFire[ i ] );

      if i < 3 Then
        begin
          Type_             := EMITTER_POINT;
          Params.Loop       := TRUE;
          Params.LifeTime   := 1000;
          Params.Emission   := 10;
          AsPoint.Direction := 270 * deg2rad;
          AsPoint.Spread    := 15 * deg2rad;

          ParParams.LifeTimeS := 1200;
          ParParams.LifeTimeV := 0;
        end else
          if i < 6 Then
            begin
              Type_             := EMITTER_POINT;
              Params.Loop       := TRUE;
              Params.LifeTime   := 1000;
              Params.Emission   := 10;
              AsPoint.Direction := 270 * deg2rad;
              AsPoint.Spread    := 15 * deg2rad;

              ParParams.LifeTimeS := 1500;
              ParParams.LifeTimeV := 0;
            end else
              begin
                Type_            := EMITTER_LINE;
                Params.Loop      := TRUE;
                Params.LifeTime  := 1000;
                Params.Emission  := 100;
                AsLine.Direction := 270 * deg2rad;
                AsLine.Spread    := 15 * deg2rad;
                AsLine.Size      := 115;

                ParParams.LifeTimeS := 1200;
                ParParams.LifeTimeV := 0;
              end;
      case i of
        0:
          begin
            Params.Position.X := 642;
            Params.Position.Y := 190;
          end;
        1:
          begin
            Params.Position.X := 40;
            Params.Position.Y := 368;
          end;
        2:
          begin
            Params.Position.X := 246;
            Params.Position.Y := 368;
          end;
        3:
          begin
            Params.Position.X := 532;
            Params.Position.Y := 244;
          end;
        4:
          begin
            Params.Position.X := 318;
            Params.Position.Y := 422;
          end;
        5:
          begin
            Params.Position.X := 583;
            Params.Position.Y := 420;
          end;
        6:
          begin
            Params.Position.X := 740;
            Params.Position.Y := 525;
          end;
      end;

      ParParams.Texture    := texParticle;
      ParParams.BlendMode  := FX_BLEND_ADD;
      ParParams.ColorMode  := FX_COLOR_MIX;
      ParParams.Frame[ 0 ] := 4;
      ParParams.Frame[ 1 ] := 4;

      // Color
      SetLength( ParParams.Color, 5 );
      ParParams.Color[ 0 ].Life  := 0;
      ParParams.Color[ 0 ].Value := $FF2222;
      ParParams.Color[ 1 ].Life  := 0.1;
      ParParams.Color[ 1 ].Value := $FF2222;
      ParParams.Color[ 2 ].Life  := 0.2;
      ParParams.Color[ 2 ].Value := $FFFF00;
      ParParams.Color[ 3 ].Life  := 0.8;
      ParParams.Color[ 3 ].Value := $FF2200;
      ParParams.Color[ 4 ].Life  := 1;
      ParParams.Color[ 4 ].Value := $FF0000;
      // Alpha
      SetLength( ParParams.Alpha, 5 );
      ParParams.Alpha[ 0 ].Life  := 0;
      ParParams.Alpha[ 1 ].Life  := 0.1;
      ParParams.Alpha[ 2 ].Life  := 0.3;
      ParParams.Alpha[ 3 ].Life  := 0.8;
      ParParams.Alpha[ 4 ].Life  := 1;
      ParParams.Alpha[ 0 ].Value := 0;
      ParParams.Alpha[ 1 ].Value := 55;
      ParParams.Alpha[ 2 ].Value := 255;
      ParParams.Alpha[ 3 ].Value := 55;
      ParParams.Alpha[ 4 ].Value := 0;
      // Size
      ParParams.SizeXS := 8 + Integer( ( i > 2 ) and ( i < 6 ) ) * 2;
      ParParams.SizeYS := 8 + Integer( ( i > 2 ) and ( i < 6 ) ) * 2;
      ParParams.SizeXV := 0;
      ParParams.SizeYV := 0;
      SetLength( ParParams.SizeXD, 3 );
      SetLength( ParParams.SizeYD, 3 );
      ParParams.SizeXD[ 0 ].Life  := 0;
      ParParams.SizeXD[ 1 ].Life  := 0.6;
      ParParams.SizeXD[ 2 ].Life  := 1;
      ParParams.SizeXD[ 0 ].Value := 1;
      ParParams.SizeXD[ 1 ].Value := 4;
      ParParams.SizeXD[ 2 ].Value := 2;

      ParParams.SizeYD[ 0 ].Life  := 0;
      ParParams.SizeYD[ 1 ].Life  := 0.6;
      ParParams.SizeYD[ 2 ].Life  := 1;
      ParParams.SizeYD[ 0 ].Value := 1;
      ParParams.SizeYD[ 1 ].Value := 4;
      ParParams.SizeYD[ 2 ].Value := 2;
      // Angle
      ParParams.AngleS := 0;
      ParParams.AngleV := 360;
      // Velocity
      ParParams.VelocityS := 64;
      ParParams.VelocityV := 0;
      SetLength( ParParams.VelocityD, 2 );
      ParParams.VelocityD[ 0 ].Life  := 0;
      ParParams.VelocityD[ 0 ].Value := 0;
      ParParams.VelocityD[ 1 ].Life  := 1;
      ParParams.VelocityD[ 1 ].Value := 1;
      // Angular Velocity
      ParParams.aVelocityS := 0;
      ParParams.aVelocityV := 0;
      SetLength( ParParams.aVelocityD, 2 );
      ParParams.aVelocityD[ 0 ].Life  := 0;
      ParParams.aVelocityD[ 0 ].Value := 0;
      ParParams.aVelocityD[ 1 ].Life  := 1;
      ParParams.aVelocityD[ 1 ].Value := 0;
      // Spin
      ParParams.SpinS := 0;
      ParParams.SpinV := 90 * deg2rad;
      SetLength( ParParams.SpinD, 2 );
      ParParams.SpinD[ 0 ].Life  := 0;
      ParParams.SpinD[ 0 ].Value := 1;
      ParParams.SpinD[ 1 ].Life  := 1;
      ParParams.SpinD[ 1 ].Value := 2;
    end;

    with eDiamond do
      begin
        emitter2d_Init( @eDiamond );

        Type_             := EMITTER_RECTANGLE;
        Params.Loop       := TRUE;
        Params.LifeTime   := 1000;
        Params.Emission   := 2;
        AsRect.Rect.X     := 0;
        AsRect.Rect.Y     := 0;
        AsRect.Rect.W     := 30;
        AsRect.Rect.H     := 35;

        Params.Position.X := 4;
        Params.Position.Y := 420;

        ParParams.LifeTimeS := 1000;
        ParParams.LifeTimeV := 1000;

        ParParams.Texture    := texParticle;
        ParParams.BlendMode  := FX_BLEND_ADD;
        ParParams.ColorMode  := FX_COLOR_MIX;
        ParParams.Frame[ 0 ] := 2;
        ParParams.Frame[ 1 ] := 2;

        // Color
        SetLength( ParParams.Color, 2 );
        ParParams.Color[ 0 ].Life  := 0;
        ParParams.Color[ 0 ].Value := $FFFF00;
        ParParams.Color[ 1 ].Life  := 1;
        ParParams.Color[ 1 ].Value := $FFAAAA;
        // Alpha
        SetLength( ParParams.Alpha, 3 );
        ParParams.Alpha[ 0 ].Life  := 0;
        ParParams.Alpha[ 0 ].Value := 0;
        ParParams.Alpha[ 1 ].Life  := 0.4;
        ParParams.Alpha[ 1 ].Value := 255;
        ParParams.Alpha[ 2 ].Life  := 1;
        ParParams.Alpha[ 2 ].Value := 0;
        // Size
        ParParams.SizeXS := 16;
        ParParams.SizeXV := 0;
        ParParams.SizeYS := 16;
        ParParams.SizeYV := 0;
        SetLength( ParParams.SizeXD, 2 );
        SetLength( ParParams.SizeYD, 2 );
        ParParams.SizeXD[ 0 ].Life  := 0;
        ParParams.SizeXD[ 1 ].Life  := 1;
        ParParams.SizeXD[ 0 ].Value := 1;
        ParParams.SizeXD[ 1 ].Value := 2;

        ParParams.SizeYD[ 0 ].Life  := 0;
        ParParams.SizeYD[ 1 ].Life  := 1;
        ParParams.SizeYD[ 0 ].Value := 1;
        ParParams.SizeYD[ 1 ].Value := 2;
        // Velocity
        ParParams.VelocityS := 0;
        ParParams.VelocityV := 0;
        SetLength( ParParams.VelocityD, 2 );
        ParParams.VelocityD[ 0 ].Life  := 0;
        ParParams.VelocityD[ 0 ].Value := 0;
        ParParams.VelocityD[ 1 ].Life  := 1;
        ParParams.VelocityD[ 1 ].Value := 0;
        // Angular Velocity
        ParParams.aVelocityS := 0;
        ParParams.aVelocityV := 0;
        SetLength( ParParams.aVelocityD, 2 );
        ParParams.aVelocityD[ 0 ].Life  := 0;
        ParParams.aVelocityD[ 0 ].Value := 0;
        ParParams.aVelocityD[ 1 ].Life  := 1;
        ParParams.aVelocityD[ 1 ].Value := 0;
        // Spin
        ParParams.SpinS := 45 * deg2rad;
        ParParams.SpinV := 0;
        SetLength( ParParams.SpinD, 2 );
        ParParams.SpinD[ 0 ].Life  := 0;
        ParParams.SpinD[ 0 ].Value := 1;
        ParParams.SpinD[ 1 ].Life  := 1;
        ParParams.SpinD[ 1 ].Value := 1;
      end;

    with eRain do
      begin
        emitter2d_Init( @eRain );

        Type_             := EMITTER_LINE;
        Params.Loop       := TRUE;
        Params.LifeTime   := 1000;
        Params.Emission   := 250;
        AsLine.Direction  := 80 * deg2rad;
        AsLine.Spread     := 5 * deg2rad;
        AsLine.Size       := 800;

        Params.Position.X := 400;
        Params.Position.Y := 0;

        ParParams.Texture    := texParticle;
        ParParams.BlendMode  := FX_BLEND_NORMAL;
        ParParams.ColorMode  := FX_COLOR_MIX;
        ParParams.LifeTimeS  := 1000;
        ParParams.LifeTimeV  := 100;
        ParParams.Frame[ 0 ] := 3;
        ParParams.Frame[ 1 ] := 3;

        // Alpha
        SetLength( ParParams.Alpha, 4 );
        ParParams.Alpha[ 0 ].Life  := 0;
        ParParams.Alpha[ 0 ].Value := 0;
        ParParams.Alpha[ 1 ].Life  := 0.1;
        ParParams.Alpha[ 1 ].Value := 255;
        ParParams.Alpha[ 2 ].Life  := 0.9;
        ParParams.Alpha[ 2 ].Value := 200;
        ParParams.Alpha[ 3 ].Life  := 1;
        ParParams.Alpha[ 3 ].Value := 0;
        // Size
        ParParams.SizeXS := 16;
        ParParams.SizeXV := 0;
        ParParams.SizeYS := 16;
        ParParams.SizeYV := 0;
        SetLength( ParParams.SizeXD, 2 );
        SetLength( ParParams.SizeYD, 2 );
        ParParams.SizeXD[ 0 ].Life  := 0;
        ParParams.SizeXD[ 0 ].Value := 1;
        ParParams.SizeXD[ 1 ].Life  := 1;
        ParParams.SizeXD[ 1 ].Value := 1;
        ParParams.SizeYD[ 0 ].Life  := 0;
        ParParams.SizeYD[ 0 ].Value := 1;
        ParParams.SizeYD[ 1 ].Life  := 1;
        ParParams.SizeYD[ 1 ].Value := 1;
        // Velocity
        ParParams.VelocityS := 450;
        ParParams.VelocityV := 0;
        SetLength( ParParams.VelocityD, 2 );
        ParParams.VelocityD[ 0 ].Life  := 0;
        ParParams.VelocityD[ 0 ].Value := 1;
        ParParams.VelocityD[ 1 ].Life  := 1;
        ParParams.VelocityD[ 1 ].Value := 1;
        // Angular Velocity
        ParParams.aVelocityS := 0;
        ParParams.aVelocityV := 0;
        SetLength( ParParams.aVelocityD, 2 );
        ParParams.aVelocityD[ 0 ].Life  := 0;
        ParParams.aVelocityD[ 0 ].Value := 0;
        ParParams.aVelocityD[ 1 ].Life  := 1;
        ParParams.aVelocityD[ 1 ].Value := 0;
        // Spin
        ParParams.SpinS := 0;
        ParParams.SpinV := 0;
        SetLength( ParParams.SpinD, 2 );
        ParParams.SpinD[ 0 ].Life  := 0;
        ParParams.SpinD[ 0 ].Value := 0;
        ParParams.SpinD[ 1 ].Life  := 1;
        ParParams.SpinD[ 1 ].Value := 0;
      end;
end;

procedure Draw;
  var
    i : Integer;
begin
  batch2d_Begin;

  ssprite2d_Draw( texBack, 0, 0, 800, 600, 0 );

  for i := 0 to 6 do
    emitter2d_Draw( @eFire[ i ] );
  emitter2d_Draw( @eDiamond );
  emitter2d_Draw( @eRain );

  if debug Then
    begin
      for i := 0 to 6 do
        with eFire[ i ].BBox do
          pr2d_Rect( MinX, MinY, MaxX - MinX, MaxY - MinY, $FF0000, 255 );
      with eDiamond.BBox do
        pr2d_Rect( MinX, MinY, MaxX - MinX, MaxY - MinY, $FF0000, 255 );
      with eRain.BBox do
        pr2d_Rect( MinX, MinY, MaxX - MinX, MaxY - MinY, $FF0000, 255 );
    end;

  fx_SetBlendMode( FX_BLEND_NORMAL );
  fx_SetColorMode( FX_COLOR_MIX );

  text_Draw( fntMain, 0, 0, 'FPS: ' + u_IntToStr( zgl_Get( RENDER_FPS ) ) );
  text_Draw( fntMain, 0, 20, 'Particles: ' + u_IntToStr( count ) );
  text_Draw( fntMain, 0, 40, 'Debug(tap): ' + u_BoolToStr( debug ) );
  batch2d_End;
end;

procedure Timer;
begin
  if touch_Tap( 0 ) Then debug := not debug;

  touch_ClearState();
end;

procedure Update( dt : Double );
  var
    i : Integer;
begin
  for i := 0 to 6 do
    emitter2d_Proc( @eFire[ i ], dt );
  emitter2d_Proc( @eDiamond, dt );
  emitter2d_Proc( @eRain, dt );

  count := 0;
  for i := 0 to 6 do
    count := count + eFire[ i ].Particles;
  count := count + eDiamond.Particles;
  count := count + eRain.Particles;
end;

Begin
  randomize;

  timer_Add( @Timer, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );
  zgl_Reg( SYS_UPDATE, @Update );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, TRUE, TRUE );

  zgl_Init;
End.
