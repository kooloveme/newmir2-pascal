program demo07;

{$I zglCustomConfig.cfg}

{$R *.res}

uses
  {$IFDEF USE_ZENGL_STATIC}
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_camera_2d,
  zgl_render_2d,
  zgl_fx,
  zgl_textures,
  zgl_textures_png,
  zgl_textures_jpg,
  zgl_sprite_2d,
  zgl_primitives_2d,
  zgl_font,
  zgl_text,
  zgl_math_2d,
  zgl_utils
  {$ELSE}
  zglHeader
  {$ENDIF}
  ;

type
  TTux = record
    Texture : zglPTexture;
    Frame   : Integer;
    Pos     : zglTPoint2D;
end;

var
  dirRes      : UTF8String {$IFNDEF MACOSX} = '../data/' {$ENDIF};
  fntMain     : zglPFont;
  texLogo     : zglPTexture;
  texBack     : zglPTexture;
  texGround   : zglPTexture;
  texTuxWalk  : zglPTexture;
  texTuxStand : zglPTexture;
  tux         : array[ 0..20 ] of TTux;
  time        : Integer;
  camMain     : zglTCamera2D;
  Title       : UTF8String='企鹅测试';
procedure Init;
  var
    i : Integer;
begin
  // CN:Camera 必须初始化。因为camera结构体内部值默认是0
  // EN: Camera must be initialized, because camera structure is zero-filled by default.
  cam2d_Init( camMain );

  //CN: 载入纹理。
  // $FF000000 -意味着透明通道是是来自文件的。并且没有透明色！
  // TEX_DEFAULT_2D  -一个比较复杂的标志类型,2D 精灵需要使用它。 详细信息可以在帮助文档中找到
  // EN: Load the texture.
  //     $FF000000 - means that alpha channel must be used from file, without colorkey.
  //     TEX_DEFAULT_2D - complex of flags that needed for 2D sprites. Description can be found in help.
  texLogo := tex_LoadFromFile( dirRes + 'zengl.png', $FF000000, TEX_DEFAULT_2D );

  texBack := tex_LoadFromFile( dirRes + 'back01.jpg' );

  texGround := tex_LoadFromFile( dirRes + 'ground.png' );
  // CN:设置单个帧的大小
  // EN: Set the size of single frame for texture.
  tex_SetFrameSize( texGround, 32, 32 );

  texTuxWalk := tex_LoadFromFile( dirRes + 'tux_walking.png' );
  tex_SetFrameSize( texTuxWalk, 64, 64 );
  texTuxStand := tex_LoadFromFile( dirRes + 'tux_stand.png' );
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

  // 载入字体
  // EN: Load the font.
  fntMain := font_LoadFromFile( dirRes + 'font.zfi' );
end;

procedure Draw;
  var
    i : Integer;
    t : Single;
begin
  batch2d_Begin();
  if time > 255 Then
    begin
      // CN:关闭颜色缓存是一个提升渲染性能的好办法。因为屏幕上通常是充满了各种对象的
      // EN: Rendering perfomance can be increased by disabling clearing the color buffer. This is a good idea because screen is full of objects.
      zgl_Disable( COLOR_BUFFER_CLEAR );

      // CN:绘制纹理到屏幕上并且缩放到800*600
      // EN: Render the background with size 800x600 and using texture "back".
      ssprite2d_Draw( texBack, 0, 0, 800, 600, 0 );

      // CN:设置当前camera
      // EN: Set the current camera.
      cam2d_Set( @camMain );

      // CN: 绘制广场
      // EN: Render the ground.
      for i := -2 to 800 div 32 + 1 do
        asprite2d_Draw( texGround, i * 32, 96 - 12, 32, 32, 0, 2 );
      for i := -2 to 800 div 32 + 1 do
        asprite2d_Draw( texGround, i * 32, 600 - 32 - 12, 32, 32, 0, 2 );

      // CN:绘制企鹅
      // EN: Render penguins
      for i := 0 to 9 do
        if i = 2 Then
          begin
            // CN: 在企鹅上绘制框框
            // EN: Render the text in frame over penguins.
            t := text_GetWidth( fntMain, 'I''m so red...' ) * 0.75 + 4;
            pr2d_Rect( tux[ i ].Pos.X - 2, tux[ i ].Pos.Y - fntMain.MaxHeight + 4, t, fntMain.MaxHeight, $000000, 200, PR2D_FILL );
            pr2d_Rect( tux[ i ].Pos.X - 2, tux[ i ].Pos.Y - fntMain.MaxHeight + 4, t, fntMain.MaxHeight, $FFFFFF );
            text_DrawEx( fntMain, tux[ i ].Pos.X, tux[ i ].Pos.Y - fntMain.MaxHeight + 8, 0.75, 0, 'I''m so red...' );
            // CN:使用fx2d函数与FX_Color标志 绘制红色企鹅
            // EN: Render red penguin using fx2d-function and flag FX_COLOR.
            fx2d_SetColor( $FF0000 );
            asprite2d_Draw( tux[ i ].Texture, tux[ i ].Pos.X, tux[ i ].Pos.Y, 64, 64, 0, tux[ i ].Frame div 2, 255, FX_BLEND or FX_COLOR );
          end else
            if i = 7 Then
              begin
                t := text_GetWidth( fntMain, '???' ) * 0.75 + 4;
                pr2d_Rect( tux[ i ].Pos.X + 32 - t / 2, tux[ i ].Pos.Y - fntMain.MaxHeight + 4, t, fntMain.MaxHeight, $000000, 200, PR2D_FILL );
                pr2d_Rect( tux[ i ].Pos.X + 32 - t / 2, tux[ i ].Pos.Y - fntMain.MaxHeight + 4, t, fntMain.MaxHeight, $FFFFFF );
                text_DrawEx( fntMain, tux[ i ].Pos.X + 32, tux[ i ].Pos.Y - fntMain.MaxHeight + 8, 0.75, 0, '???', 255, $FFFFFF, TEXT_HALIGN_CENTER );
                // CN: 绘制幽灵企鹅 使用FX_COLOR_SET 和 FX_COLOR;
                // EN: Render penguin ghost using flag FX_COLOR and mode FX_COLOR_SET :)
                fx_SetColorMode( FX_COLOR_SET );
                fx2d_SetColor( $FFFFFF );
                asprite2d_Draw( tux[ i ].Texture, tux[ i ].Pos.X, tux[ i ].Pos.Y, 64, 64, 0, tux[ i ].Frame div 2, 155, FX_BLEND or FX_COLOR );
                // CN: 回到默认的绘制模式
                // EN: Return default mode.
                fx_SetColorMode( FX_COLOR_MIX );
              end else
                asprite2d_Draw( tux[ i ].Texture, tux[ i ].Pos.X, tux[ i ].Pos.Y, 64, 64, 0, tux[ i ].Frame div 2 );

      // CN: 使用另一种方法绘制企鹅使用FX2D_FLIPX
      // EN: Render penguins, that go another way using special flag for flipping texture - FX2D_FLIPX.
      for i := 10 to 19 do
        if i = 13 Then
          begin
            t := text_GetWidth( fntMain, 'I''m so big...' ) * 0.75 + 4;
            pr2d_Rect( tux[ i ].Pos.X - 2, tux[ i ].Pos.Y - fntMain.MaxHeight - 10, t, fntMain.MaxHeight, $000000, 200, PR2D_FILL );
            pr2d_Rect( tux[ i ].Pos.X - 2, tux[ i ].Pos.Y - fntMain.MaxHeight - 10, t, fntMain.MaxHeight, $FFFFFF );
            text_DrawEx( fntMain, tux[ i ].Pos.X, tux[ i ].Pos.Y - fntMain.MaxHeight - 4, 0.75, 0, 'I''m so big...' );
            // CN: 绘制大企鹅。.它必须移动位置。因为FX2D_SCALE 缩放精灵是相对于中央的。
            // EN: Render "big" penguin. It must be shifted up, because FX2D_SCALE scale sprite relative to the center.
            fx2d_SetScale( 1.25, 1.25 );
            asprite2d_Draw( tux[ i ].Texture, tux[ i ].Pos.X, tux[ i ].Pos.Y - 8, 64, 64, 0, tux[ i ].Frame div 2, 255, FX_BLEND or FX2D_FLIPX or FX2D_SCALE );
          end else
            if i = 17 Then
              begin
                // CN: 渲染"高个子企鹅" 使用 FX2D_VCHAGE 代替FX2D_SCALE,  并且fx2d_SetVertexes 来向上移动精灵顶点！
                // EN: Render "tall" penguin using flag FX2D_VCHANGE instead of FX2D_SCALE, and function fx2d_SetVertexes for shifting upper vertexes of sprite.
                fx2d_SetVertexes( 0, -16, 0, -16, 0, 0, 0, 0 );
                asprite2d_Draw( tux[ i ].Texture, tux[ i ].Pos.X, tux[ i ].Pos.Y, 64, 64, 0, tux[ i ].Frame div 2, 255, FX_BLEND or FX2D_FLIPX or FX2D_VCHANGE );
              end else
                asprite2d_Draw( tux[ i ].Texture, tux[ i ].Pos.X, tux[ i ].Pos.Y, 64, 64, 0, tux[ i ].Frame div 2, 255, FX_BLEND or FX2D_FLIPX );

      // CN: 重置camera
      // EN: Reset the camera.
      cam2d_Set( nil );

      // CN: 在屏幕中间绘制广场
      // EN: Render piece of ground in the center of screen.
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
    text_Draw( fntMain, 0, 0, 'FPS: ' + u_IntToStr( zgl_Get( RENDER_FPS ) ) );
  batch2d_End();
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

  if key_Press( K_ESCAPE ) Then zgl_Exit();
  key_ClearState();
end;

Begin

  {$IFNDEF USE_ZENGL_STATIC}
  if not zglLoad( libZenGL ) Then exit;
  {$ENDIF}

  randomize();

  timer_Add( @Timer, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  wnd_SetCaption( '07 - Sprites' );
  //wnd_SetCaption( Title );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
