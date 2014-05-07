program demo01;
// CN:Õâ¸öÎÄ¼ş°üÀ¨Ò»Ğ©ÉßÕâ(ÀıÈçÊ¹ÓÃ²»Ê¹ÓÃ¾²Ì¬Á´½Ó) Óë ¶¨ÒåÊ¹ÓÃÁ´½ÓµÄ²Ù×÷ÏµÍ³
// EN: This file contains some options(e.g. whether to use static compilation) and defines of OS for which is compilation going.
{$I zglCustomConfig.cfg}

{$R *.res}

uses
  {$IFDEF USE_ZENGL_STATIC}
  //CN:Ê¹ÓÃ¾²Ì¬Á´½ÓĞèÒªuse ZenGLµÄ¹¦ÄÜµÄµ¥Ôª¡£
  // EN: Using static compilation needs to use ZenGL units with needed functionality.
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_utils
  {$ELSE}
  // RU:Ê¹ÓÃ¶¯Ì¬Á´½Ó¿â(so,dll,dylib) ½ö½öĞèÒªÒ»¸öÍ·ÎÄ¼ş
  // EN: Using ZenGL as a shared library(so, dll or dylib) needs only one header.
  zglHeader
  {$ENDIF}
  ;

var
  DirApp  : UTF8String;
  DirHome : UTF8String;
  UpdateDt: Double;

procedure Init;
begin
  // CN: ¿ÉÒÔÔÚÕâÀïÔØÈë×ÊÔ´ÎÄ¼ş
  // EN: Here can be loading of main resources.
end;

procedure Draw;
begin
  // CN: ÔÚÕâÀï»æÖÆËùÓĞ¶«Î÷
  // EN: Here "draw" anything :)
end;

procedure Update( dt : Double );
begin
  // CN:ÔÚ´Ëº¯ÊıÊµÏÖÆ½»¬µÄÒÆ¶¯ÊÇ×îÀíÏëµÄ¡£ÒòÎª¶¨Ê±Æ÷µÄ¾«È·¶ÈÊÜÏŞÖÆÓÚfps
  // ´Ëº¯Êı½«ÔÚÃ¿´ÎäÖÈ¾Ö®Ç°Ö´ĞĞ¡£Æä´«µİ½øÀ´µÄdt²ÎÊıÊÇµ±Ç°Ö¡ºÍÉÏ´ÎÖ¡µÄ¼ä¸ô¡£
  UpdateDt:=dt;

  // EN: This function is the best way to implement smooth moving of something, because accuracy of timers are restricted by FPS.
end;

procedure Timer;
begin
  // CN: Caption ½«»áÏÔÊ¾Ã¿ÃëµÄÖ¡ÂÊ
  // EN: Caption will show the frames per second.
  //wnd_SetCaption( '01 - Initialization[ FPS: ' + u_IntToStr( zgl_Get( RENDER_FPS ) ) + ' ]' );
  wnd_SetCaption(AnsiToUtf8('01 - ³õÊ¼»¯ZenGl [ FPS: ' + u_IntToStr(zgl_Get(RENDER_FPS)))+']'+u_floatToStr(UpdateDt));
end;

procedure Quit;
begin

end;

Begin
  // CN: Èç¹ûÃ»ÓĞÊ¹ÓÃ¾²Ì¬Á¬½Ó ÏÂÃæµÄ´úÂë½«»áÔØÈë¶¯Ì¬¿â
  // EN: Code below loads a library if static compilation is not used.
  {$IFNDEF USE_ZENGL_STATIC}
  if not zglLoad( libZenGL ) Then exit;
  {$ENDIF}

  // CN:ÎªÁËÔØÈë»òÕß/´´½¨ÉèÖÃÎÄ¼ş»òÕßÆäËûµÄ£¬¿ÉÒÔ»ñÈ¡ÓÃ»§µÄ¸ùÄ¿Â¼¡£»òÕß¿ÉÖ´ĞĞÎÄ¼şÄ¿Â¼(²»Ö§³ÖGNU/Linux)ÿ øå
  // EN: For loading/creating your own options/profiles/etc. you can get path to user home directory, or to executable file(not works for GNU/Linux).
  DirApp  := utf8_Copy( PAnsiChar( zgl_Get( DIRECTORY_APPLICATION ) ) );
  DirHome := utf8_Copy( PAnsiChar( zgl_Get( DIRECTORY_HOME ) ) );

  // CN:´´½¨Ò»¸ö¼ä¸ôÎª1000msµÄ¶¨Ê±Æ÷
  // EN: Create a timer with interval 1000ms.
  timer_Add( @Timer, 1000 );

  // CN:×¢²áÒ»¸ö¹ı³Ì£¬Õâ¸ö¹ı³Ì½«ÔÚZenGL³õÊ¼»¯ºóÍê³É
  // EN: Register the procedure, that will be executed after ZenGL initialization.
  zgl_Reg( SYS_LOAD, @Init );

  // CN:×¢²áäÖÈ¾¹ı³Ì
  // EN: Register the render procedure.
  zgl_Reg( SYS_DRAW, @Draw );
  
  // CN£º×¢²áÒ»¸ö¹ı³Ì £¬Õâ¸ö¹ı³Ì½«»á»ñÈ¡Á½´ÎÖ¡µÄ¼ä¸ô
  // EN: Register the procedure, that will get delta time between the frames.
  zgl_Reg( SYS_UPDATE, @Update );

  // CN:×¢²áÒ»¸ö¹ı³Ì£¬´Ë¹ı³Ì½«ÔÚZenGl¹Ø±ÕºóÖ´ĞĞ
  // EN: Register the procedure, that will be executed after ZenGL shutdown.
  zgl_Reg( SYS_EXIT, @Quit );

  // CN:ÉèÖÃ´°¿ÚµÄ±êÌâÊôĞÔ
  // EN: Set the caption of the window.
  wnd_SetCaption( AnsiToUtf8('ÈÈÑª´«Ææ') );

  // CN:ÔÊĞíÏÔÊ¾Êó±ê¹â±ê
  // EN: Allow to show the mouse cursor.
  wnd_ShowCursor( TRUE );

  // CN: ÉèÖÃÆÁÄ»
  // EN: Set screen options.
  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  // CN: ³õÊ¼»¯ZenGL.
  // EN: Initialize ZenGL.
  zgl_Init();
End.
