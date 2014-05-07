program demo04;

{$I zglCustomConfig.cfg}

{$R *.res}

uses
  {$IFDEF USE_ZENGL_STATIC}
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_keyboard,
  zgl_font,
  zgl_text,
  zgl_sprite_2d,
  zgl_textures,
  zgl_textures_png,
  zgl_textures_jpg,
  zgl_utils
  {$ELSE}
  zglHeader
  {$ENDIF}
  ;

var
  dirRes  : UTF8String {$IFNDEF MACOSX} = '../data/' {$ENDIF};

  fntMain : zglPFont;
  texBack : zglPTexture;

procedure Init;
begin
  fntMain := font_LoadFromFile( dirRes + 'font.zfi' );
  texBack := tex_LoadFromFile( dirRes + 'back03.jpg' );
end;

procedure Draw;
begin
  ssprite2d_Draw( texBack, 0, 0, 800, 600, 0 );

  text_Draw( fntMain, 0, 0, 'Escape - Exit' );
  text_Draw( fntMain, 0, fntMain.MaxHeight * 1, 'F1 - Fullscreen with desktop resolution and correction of aspect' );
  text_Draw( fntMain, 0, fntMain.MaxHeight * 2, 'F2 - Fullscreen with desktop resolution and simple scaling' );
  text_Draw( fntMain, 0, fntMain.MaxHeight * 3, 'F3 - Fullscreen with resolution 800x600' );
  text_Draw( fntMain, 0, fntMain.MaxHeight * 4, 'F4 - Windowed mode' );
end;

procedure Timer;
begin
  // CN:推荐使用此种全屏模式。主要思想是切换到全屏显示而分辨率以桌面的分辨率宽高度并且保持宽高比。在一些LCD显示器上可能有些问题
  //
  // EN: Recommended fullscreen mode for using. Main idea is switching to fullscreen mode using current desktop resolution of user and saving the aspect.
  //This will avoid some problems
  //     with LCD's.
  if key_Press( K_F1 ) Then
    begin
      // CN:保持宽高比
      // EN: Enable aspect correction.
      zgl_Enable( CORRECT_RESOLUTION );
      // CN:设置游戏程序画面的分辨率.
      // EN: Set resolution for what application was wrote.
      scr_CorrectResolution( 800, 600 );
      //设置ZENGL覆盖全显示器
      scr_SetOptions( zgl_Get( DESKTOP_WIDTH ), zgl_Get( DESKTOP_HEIGHT ), REFRESH_MAXIMUM, TRUE, FALSE );
    end;

  // CN:类似前面的的模式，不保持画面的宽高比。例如对于5:4的分辨率(1280*1024)也有很好的效果，因为屏幕会扩展黑色区域
  //
  // EN: Similar mode to previous one with one exception - disabled correction for width and height. E.g. this can be useful for aspect 5:4(resolution 1280x1024),
  //     because screen can be filled without significant distortion.
  if key_Press( K_F2 ) Then
    begin
      zgl_Enable( CORRECT_RESOLUTION );
      zgl_Disable( CORRECT_WIDTH );
      zgl_Disable( CORRECT_HEIGHT );
      scr_CorrectResolution( 800, 600 );
      scr_SetOptions( zgl_Get( DESKTOP_WIDTH ), zgl_Get( DESKTOP_HEIGHT ), REFRESH_MAXIMUM, TRUE, FALSE );
    end;

  // CN:使用设置值开启全屏模式，现在这个方法在LCD显示器上有两个问题。
  // 1.  如果使用的分辨率不是LCD的主要分辨率。并且驱动也没有特殊的设置的话 会看到马赛克。
  // 2. 4:3分辨率会被缩放为宽屏显示器
  // EN: Switching to fullscreen mode using set values. Nowadays this method two main problems with LCD:
  //     - if used resolution is not main for LCD, then without special options in drivers user will see pixelization
  //     - picture with aspect 4:3 will be stretched on widescreen monitors
  if key_Press( K_F3 ) Then
    begin
      zgl_Disable( CORRECT_RESOLUTION );
      scr_SetOptions( 800, 600, REFRESH_MAXIMUM, TRUE, FALSE );
    end;

  // CN:窗口模式
  // EN: Windowed mode.
  if key_Press( K_F4 ) Then
    begin
      zgl_Disable( CORRECT_RESOLUTION );
      scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );
    end;

  if key_Press( K_ESCAPE ) Then zgl_Exit();

  key_ClearState();
end;

Begin
  {$IFNDEF USE_ZENGL_STATIC}
  if not zglLoad( libZenGL ) Then exit;
  {$ENDIF}

  timer_Add( @Timer, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  wnd_SetCaption( '04 - Screen Settings' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
