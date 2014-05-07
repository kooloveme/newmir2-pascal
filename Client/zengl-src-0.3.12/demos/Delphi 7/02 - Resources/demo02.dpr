program demo02;

{$I zglCustomConfig.cfg}

{$R *.res}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,

  {$IFDEF USE_ZENGL_STATIC}
  zgl_types,
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  // CN:文件调用单元。zip压缩文件和内存文件
  // EN: Units for using files, files in memory and zip archives.
  zgl_file,
  zgl_memory,
  // CN:多线程资源载入单元。
  // EN: Unit for multithreaded resource loading.
  zgl_resources,
  // CN:字体单元
  // EN: Unit for using fonts.
  zgl_font,
  // CN:纹理单元，zgl_textures 是一个重要的单元,下面的单元是提供以不同格式的文件支持。
  // EN: Units for using textures. zgl_textures is a main unit, next units provide support of different formats.
  zgl_textures,
  zgl_textures_tga, // TGA
  zgl_textures_jpg, // JPG
  zgl_textures_png, // PNG
  // CN：声音实现子系统在此单元下面。思想是和纹理一样的，这是一个主要的单元。下面是需要支持的不同格式的单元。
  // EN: Sound subsystem implemented in units below. Idea the same as for textures - there is a main unit and units for support different formats.
  zgl_sound,
  zgl_sound_wav, // WAV
  zgl_sound_ogg, // OGG

  zgl_primitives_2d,
  zgl_text,
  zgl_sprite_2d,
  zgl_utils
  {$ELSE}
  zglHeader
  {$ENDIF}
  ;

var
  dirRes : UTF8String {$IFNDEF MACOSX} = '../data/' {$ENDIF};

  memory : zglTMemory;

  // CN:每个资源都有一个自身的类型，此类型是一个指向结构体的指针
  // EN: Every resource has its own typem which is just a pointer to structure.
  fntMain  : zglPFont;
  //
  texLogo  : zglPTexture;
  texTest  : zglPTexture;
  //
  sndClick : zglPSound;
  sndMusic : zglPSound;

procedure TextureCalcEffect( pData : PByteArray; Width, Height : Word );
begin
  u_Sleep( 1000 );
end;

procedure Init;
  var
    i         : Integer;
    memStream : TMemoryStream;
begin
  // CN: 更多关于这些函数的参数和用法可以在其他DEMO中找到。这里只使用了一般的用法。
  // EN: Description with more details about parameters of functions can be found in other demos, here is only main idea shown.

  snd_Init();

  // CN:载入资源的函数使用  "类型_LoadFrom位置" 格式来声明。 "类型"可以是tex snd font 等等 "位置"可以是在文件 或者内存中
  // EN: Functions for loading resources named in format "$(prefix)_LoadFrom$(where)", where "$(prefix)" can be tex, snd, font and so on, and $(where) - File and Memory.
  fntMain  := font_LoadFromFile( dirRes + 'font.zfi' );
  texLogo  := tex_LoadFromFile( dirRes + 'zengl.png' );
  sndClick := snd_LoadFromFile( dirRes + 'click.wav' );

  // CN:多线程资源读取允许在读取的时候创建一个队列以及做其他的事情，例如渲染一些动画
  // 多线程资源读取与一般读取没有不同之处。除非使用函数操作开始和结束队列。
  // EN: Multithreaded resource loading allows to make queue and do something while loading, e.g. rendering some animation.
  //     Loading resources in multithreaded mode has almost no difference with standard mode, except using functions for beginning and ending queues.
  res_BeginQueue( 0 );
  // CN:为了保持屏幕在载入界面，基本上所有的资源读取函数都需要再res_BeginQueue 和res_EndQueque中调用。并且纹理将会在在延迟后被提交处理！
  //
  // EN: All standard functions for loading resources can be used between res_BeginQueue and res_EndQueue.
  //     Just for holding loading screen resources will be loaded multiple times, and texture will be post-processed with delay.
  zgl_Reg( TEX_CURRENT_EFFECT, @TextureCalcEffect );//在纹理载入时对纹理进行处理
  for i := 0 to 3 do
    begin
      texTest  := tex_LoadFromFile( dirRes + 'back01.jpg', TEX_NO_COLORKEY, TEX_DEFAULT_2D or TEX_CUSTOM_EFFECT );
      sndMusic := snd_LoadFromFile( dirRes + 'music.ogg' );
    end;
  res_EndQueue();

  // CN:从内存中加载资源文件需要额外指定其后缀。
  // 以下是一个使用TMemoryStream替代 mem_LoadFromFile/mem_Free 函数的示例。主要是为了展示一下ZglTmemory是如何工作的
  // EN: Loading resources from files in memory need additional set their extension.
  //     As an example TMemoryStream will be used instead of mem_LoadFromFile/mem_Free, just for showing how zglTMemory works.
  memStream := TMemoryStream.Create();
  {$IFNDEF MACOSX}
  memStream.LoadFromFile( dirRes + 'back01.jpg' );
  {$ELSE}
  memStream.LoadFromFile( PAnsiChar( zgl_Get( DIRECTORY_APPLICATION ) ) + 'Contents/Resources/back01.jpg' );
  {$ENDIF}
  memory.Position := memStream.Position;
  memory.Memory   := memStream.Memory;
  memory.Size     := memStream.Size;
  texTest := tex_LoadFromMemory( memory, 'JPG' );
  memStream.Free();

  // CN: 从文件或者压缩文件中载入资源，首先需要opened 然后再closed。使用file_OpenArchive 和File_CloseArchive来操作。
  // EN: For loading resources from zip-archive this archive should be "opened" first and then "closed" :) There are functions file_OpenArchive and file_CloseArchive for this.
  file_OpenArchive( dirRes + 'zengl.zip' );
  texLogo := tex_LoadFromFile( 'zengl.png' );
  file_CloseArchive();
end;

procedure Draw;
begin
  // CN：多线程资源只有被在读取过程被完成后才可以读取。下面的代码是如果资源没有载入完毕则显示载入画面
  // EN: Resources which are loading in multithreaded mode can be used only after finishing the loading process. Code below renders loading screen if resources are not loaded yet.
  if res_GetCompleted() < 100 Then
    begin
      ssprite2d_Draw( texLogo, ( 800 - texLogo.Width ) / 2, ( 600 - texLogo.Height ) / 2, texLogo.Width, texLogo.Height, 0 );
      text_Draw( fntMain, 400, 300 + texLogo.Height / 4, 'Loading... ' + u_IntToStr( res_GetCompleted() ) + '%', TEXT_HALIGN_CENTER );
      exit;
    end;

  ssprite2d_Draw( texTest, 0, 0, 800, 600, 0 );
  text_Draw( fntMain, 0, 0, 'FPS: ' + u_IntToStr( zgl_Get( RENDER_FPS ) ) );
  text_Draw( fntMain, 0, 16, 'VRAM Used: ' + u_FloatToStr( zgl_Get( RENDER_VRAM_USED ) / 1024 / 1024 ) + 'Mb' );
end;

Begin
  {$IFNDEF USE_ZENGL_STATIC}
  if not zglLoad( libZenGL ) Then exit;
  {$ENDIF}

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  wnd_SetCaption( '02 - Resources' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );
  scr_SetVSync(True);

  zgl_Init();
End.
