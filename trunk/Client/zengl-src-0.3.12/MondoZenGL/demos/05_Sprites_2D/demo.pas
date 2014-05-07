unit Demo;

{$INCLUDE '../../src/mz_config.cfg'}

interface

procedure RunDemo;

implementation

uses
  SysUtils,
  { You don't necessarily have to use these units. However, Delphi will not
    inline some methods if these units are not in the uses clause. }
  {$IFNDEF FPC}
  {$IFDEF USE_ZENGL_STATIC}
  zgl_sprite_2d,
  zgl_text,
  zgl_primitives_2d,
  zgl_utils,
  zgl_camera_2d,
  zgl_textures,
  zgl_font,
  zgl_keyboard,
  zgl_main,
  zgl_fx,
  zgl_render_2d,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  {$ENDIF}
  MondoZenGL;

const
  {$IFDEF DARWIN}
  RESOURCE_DIRECTORY = '';
  {$ELSE}
  RESOURCE_DIRECTORY = '../data/';
  {$ENDIF}

type
  TTux = class
  private
    FTexture: TMZTexture;
    FFrame: Integer;
    FPos: TMZPoint;
    FStepX: Single;
  public
    constructor Create(const Texture: TMZTexture; const Frame: Integer;
      const X, Y, StepX: Single);
    procedure Step;

    property Texture: TMZTexture read FTexture;
    property Frame: Integer read FFrame;
    property Pos: TMZPoint read FPos;
  end;

type
  TDemoScene = class(TMZScene)
  private
    FFont: TMZFont;
    FTimer: TMZTimer;
    FTexLogo: TMZTexture;
    FTexBack: TMZTexture;
    FTexGround: TMZTexture;
    FTexTuxWalk: TMZTexture;
    FTexTuxStand: TMZTexture;
    FTux: array [0..20] of TTux;
    FCamera: TMZCamera;
    FCounter: Integer;
  private
    procedure TimerExpired(Sender: TObject);
  protected
    { Summary:
        Is called before the scene is executed. You can override this method
        to initialize scene specific resources. }
    procedure Startup; override;

    { Summary:
        Is called just before the scene is terminated. You can override this
        method to cleanup scene specific resources. }
    procedure Shutdown; override;

    { Summary:
        Is called during each iteration of the main loop to render the current
        frame. }
    procedure RenderFrame; override;
  end;

procedure RunDemo;
var
  Application: TMZApplication;
begin
  Application := TMZApplication.Create;
  Application.Options := Application.Options + [aoShowCursor] - [aoAllowPortraitOrientation];
  Application.Caption := '05 - Sprites 2D';
  Application.ScreenWidth := 1024;
  Application.ScreenHeight := 600;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TTux }

constructor TTux.Create(const Texture: TMZTexture; const Frame: Integer;
  const X, Y, StepX: Single);
begin
  inherited Create;
  FTexture := Texture;
  FFrame := Frame;
  FPos := TMZPoint.Create(X, Y);
  FStepX := StepX;
end;

procedure TTux.Step;
begin
  Inc(FFrame);
  if (FFrame > 20) then
    FFrame := 2;

  FPos.X := FPos.X + FStepX;
  if (FStepX > 0) then
  begin
    if (FPos.X > 1080) then
      FPos.X := -120;
  end
  else
  begin
    if (FPos.X < -120) then
      FPos.X := 1080;
  end;
end;

{ TDemoScene }

procedure TDemoScene.TimerExpired(Sender: TObject);
var
  Tux: TTux;
begin
  Inc(FCounter, 2);

  FCamera.RotationAngle := FCamera.RotationAngle + Cos(FCounter / 1000) / 10;

  for Tux in FTux do
    Tux.Step;

  if TMZKeyboard.IsKeyPressed(kcEscape) then
    Application.Quit;
  TMZKeyboard.ClearState;
end;

procedure TDemoScene.Startup;
var
  I: Integer;
begin
  inherited Startup;
  // RU: Загружаем текстуру.
  // $FF000000 - указывает на то, что бы использовать альфа-канал из изображения.
  // TEX_DEFAULT_2D - комплекс флагов, необходимых для 2D-спрайтов. Описание есть в справке.
  //
  // EN: Load the texture.
  // $FF000000 - means that alpha channel must be used from file, without colorkey.
  // TEX_DEFAULT_2D - complex of flags that needed for 2D sprites. Description can be found in help.
  FTexLogo := TMZTexture.Create(RESOURCE_DIRECTORY + 'zengl.png');
  FTexBack := TMZTexture.Create(RESOURCE_DIRECTORY + 'back01.jpg');
  FTexGround := TMZTexture.Create(RESOURCE_DIRECTORY + 'ground.png');

  // RU: Указываем размер кадра в текстуре.
  // EN: Set the size of single frame (or tile) for texture.
  FTexGround.SetFrameSize(32, 32);

  FTexTuxWalk := TMZTexture.Create(RESOURCE_DIRECTORY + 'tux_walking.png');
  FTexTuxWalk.SetFrameSize(64, 64);

  FTexTuxStand := TMZTexture.Create(RESOURCE_DIRECTORY + 'tux_stand.png');
  FTexTuxStand.SetFrameSize(64, 64);

  for I := 0 to 9 do
  begin
    FTux[I] := TTux.Create(FTexTuxWalk, Random(19) + 2, I * 120, 32, 1.5);
    FTux[I + 10] := TTux.Create(FTexTuxWalk, Random(19) + 2, I * 129, 768 - 96, -1.5);
  end;
  FTux[20] := TTux.Create(FTexTuxStand, Random(19) + 2, 512 - 32, 384 - 64 - 4, 0);

  // RU: Загружаем шрифт.
  // EN: Load the font.
  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'font.zfi' );

  FCamera := TMZCamera.Create;
  FTimer := TMZTimer.Create(TimerExpired, 16)
end;

procedure TDemoScene.Shutdown;
var
  I: Integer;
begin
  FTexLogo.Free;
  FTexBack.Free;
  FTexGround.Free;
  FTexTuxWalk.Free;
  FTexTuxStand.Free;
  for I := 0 to 20 do
    FTux[I].Free;
  FFont.Free;
  FCamera.Free;
  FTimer.Free;
  inherited Shutdown;
end;

procedure TDemoScene.RenderFrame;
var
  I: Integer;
  W: Single;
  Tux: TTux;
  R: TMZRect;
begin
  inherited RenderFrame;
  Canvas.BeginBatch;
  try
    { Possible FCounter values:
      0..255: "ZenGL" logo fade-in animation.
      255..510: "ZenGL" logo fade-out animation, as well as main loop.
      >510: Main loop }
    if (FCounter > 255) then
    begin
      // RU: Для увеличения быстродействия можно отключить очистку буфера цвета,
      // учитывая что экран полностью заполнен.
      // EN: Rendering perfomance can be increased by disabling clearing the color buffer.
      // This is a good idea because screen is full of objects.
      Canvas.DisableOption(coClearColorBuffer);

      // RU: Рисуем задний фон с размерами 800х600 используя текстуру back.
      // EN: Render the background with size 800x600 and using texture "back".
      Canvas.DrawSprite(FTexBack, 0, 0, 1024, 768);

      // RU: Установить текущую камеру.
      // EN: Set the current camera.
      Canvas.Camera := FCamera;

      // RU: Рисуем землю.
      // EN: Render the ground.
      for I := -2 to (1024 div 32) + 1 do
      begin
        Canvas.DrawSpriteFrame(FTexGround, 2, I * 32, 96 - 12, 32, 32);
        Canvas.DrawSpriteFrame(FTexGround, 2, I * 32, 768 - 32 - 12, 32, 32);
      end;

      // RU: Рисуем шагающих пингвинов.
      // EN: Render penguins
      for I := 0 to 9 do
      begin
        Tux := FTux[I];
        if (I = 2) then
        begin
          // RU: Рисуем надпись в "рамочке" над пингвином.
          // EN: Render the text in frame over penguins.
          W := Canvas.CalculateTextWidth(FFont, 'I''m so red...') * 0.75 + 4;
          R := TMZRect.Create(Tux.Pos.X - 2, Tux.Pos.Y - FFont.MaxHeight + 4, W, FFont.MaxHeight);
          Canvas.FillRect(R, $000000, 200);
          Canvas.DrawRect(R, $FFFFFF);
          Canvas.DrawText(FFont, Tux.Pos.X, Tux.Pos.Y - FFont.MaxHeight + 8,
            0.75, 0, 'I''m so red...');

          // RU: Рисуем красного пингвина используя fx2d-функцию и флаг FX_COLOR.
          // EN: Render red penguin using fx2d-function and flag FX_COLOR.
          Canvas.TextureColor := $FF0000;
          Canvas.DrawSpriteFrame(Tux.Texture, Tux.Frame div 2, Tux.Pos.X,
            Tux.Pos.Y, 64, 64, 0, $FF, [efBlend, efColor]);
        end
        else if (I = 7) then
        begin
          W := Canvas.CalculateTextWidth(FFont, '???') * 0.75 + 4;
          R := TMZRect.Create(Tux.Pos.X + 32 - W / 2, Tux.Pos.Y - FFont.MaxHeight + 4, W, FFont.MaxHeight);
          Canvas.FillRect(R, $000000, 200);
          Canvas.DrawRect(R, $FFFFFF);
          Canvas.DrawText(FFont, Tux.Pos.X + 32, Tux.Pos.Y - FFont.MaxHeight + 8,
            0.75, 0, '???', $FF, $FFFFFF, [tfHAlignCenter]);

          // RU: Рисуем пингвина приведение используя флаг FX_COLOR установив режим в FX_COLOR_SET :)
          // EN: Render penguin ghost using flag FX_COLOR and mode FX_COLOR_SET :)
          Canvas.ColorMode := cmReplace;
          Canvas.TextureColor := $FFFFFF;
          Canvas.DrawSpriteFrame(Tux.Texture, Tux.Frame div 2, Tux.Pos.X,
            Tux.Pos.Y, 64, 64, 0, 155, [efBlend, efColor]);

          // RU: Возвращаем обычный режим.
          // EN: Return default mode.
          Canvas.ColorMode := cmMix;
        end
        else
          Canvas.DrawSpriteFrame(Tux.Texture, Tux.Frame div 2, Tux.Pos.X,
            Tux.Pos.Y, 64, 64);
      end;

      // RU: Рисуем пингвинов шагающих в обратную сторону используя флаг отражения текстуры FX2D_FLIPX.
      // EN: Render penguins, that go another way using special flag for flipping texture - FX2D_FLIPX.
      for I := 10 to 19 do
      begin
        Tux := FTux[I];
        if (I = 13) then
        begin
          W := Canvas.CalculateTextWidth(FFont, 'I''m so big...') * 0.75 + 4;
          R := TMZRect.Create(Tux.Pos.X - 2, Tux.Pos.Y - FFont.MaxHeight - 10, W, FFont.MaxHeight);
          Canvas.FillRect(R, $000000, 200);
          Canvas.DrawRect(R, $FFFFFF);
          Canvas.DrawText(FFont, Tux.Pos.X, Tux.Pos.Y - FFont.MaxHeight - 4,
            0.75, 0, 'I''m so big...');

          // RU: Рисуем "большего" пингвина. Т.к. FX2D_SCALE увеличивает спрайт относительно центра, то пингвина следует немного "поднять".
          // EN: Render "big" penguin. It must be shifted up, because FX2D_SCALE scale sprite relative to the center.
          Canvas.SetScale(1.25);
          Canvas.DrawSpriteFrame(Tux.Texture, Tux.Frame div 2, Tux.Pos.X,
            Tux.Pos.Y - 8, 64, 64, 0, $FF, [efBlend, efFlipX, efScale]);
        end
        else if (I = 17) then
        begin
          // RU: Рисуем "высокого" пингвина используя вместо флага FX2D_SCALE флаг FX2D_VCHANGE и функцию fx2d_SetVertexes
          // для смещения координат двух верхних вершин спрайта.
          // EN: Render "tall" penguin using flag FX2D_VCHANGE instead of FX2D_SCALE, and function fx2d_SetVertexes for
          // shifting upper vertexes of sprite.
          Canvas.SetVertexOffsets(0, -16, 0, -16, 0, 0, 0, 0);
          Canvas.DrawSpriteFrame(Tux.Texture, Tux.Frame div 2, Tux.Pos.X,
            Tux.Pos.Y, 64, 64, 0, $FF, [efBlend, efFlipX, efOffsetVertices]);
        end
        else
          Canvas.DrawSpriteFrame(Tux.Texture, Tux.Frame div 2, Tux.Pos.X,
            Tux.Pos.Y, 64, 64, 0, $FF, [efBlend, efFlipX]);
      end;

      // RU: Сбросить камеру.
      // EN: Reset the camera.
      Canvas.Camera := nil;

      // RU: Рисуем учатоск земли по центру экрана.
      // EN: Render piece of ground in the center of screen.
      Canvas.DrawSpriteFrame(FTexGround, 1, 464, 384 - 16, 32, 32);
      Canvas.DrawSpriteFrame(FTexGround, 2, 496, 384 - 16, 32, 32);
      Canvas.DrawSpriteFrame(FTexGround, 3, 528, 384 - 16, 32, 32);

      Tux := FTux[20];
      W := Canvas.CalculateTextWidth(FFont, 'o_O') * 0.75 + 4;
      R := TMZRect.Create(Tux.Pos.X + 32 - W / 2, Tux.Pos.Y - FFont.MaxHeight + 4, W, FFont.MaxHeight);
      Canvas.FillRect(R, $000000, 200);
      Canvas.DrawRect(R, $FFFFFF);
      Canvas.DrawText(FFont, Tux.Pos.X + 32, Tux.Pos.Y - FFont.MaxHeight + 8,
        0.75, 0, 'o_O', $FF, $FFFFFF, [tfHAlignCenter]);
      Canvas.DrawSpriteFrame(Tux.Texture, Tux.Frame div 2, Tux.Pos.X, Tux.Pos.Y, 64, 64);

      Canvas.DrawText(FFont, 0, 0, 'FPS: ' + TMZUtils.IntToStr(Application.CurrentRenderFrameRate));
    end;

    { "ZenGL" logo fade-in/fade-out }
    if (FCounter <= 255) then
      Canvas.DrawSprite(FTexLogo, 512 - 256, 384 - 128, 512, 256, 0, FCounter)
    else if (FCounter < 510) then
    begin
      Canvas.FillRect(0, 0, 1024, 768, $000000, 510 - FCounter);
      Canvas.DrawSprite(FTexLogo, 512 - 256, 384 - 128, 512, 256, 0, 510 - FCounter)
    end;
  finally
    Canvas.EndBatch;
  end;
end;

end.
