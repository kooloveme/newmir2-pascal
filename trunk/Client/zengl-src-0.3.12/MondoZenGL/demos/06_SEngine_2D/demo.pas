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
  zgl_render_2d,
  zgl_sprite_2d,
  zgl_primitives_2d,
  zgl_text,
  zgl_utils,
  zgl_textures,
  zgl_font,
  zgl_keyboard,
  zgl_main,
  zgl_mouse,
  zgl_collision_2d,
  zgl_math_2d,
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
  TSimpleButton = class
  private
    FCanvas: TMZCanvas;
    FFont: TMZFont;
    FCaption: UTF8String;
    FBounds: TMZRect;
  public
    constructor Create(const Canvas: TMZCanvas; const Font: TMZFont;
      const Caption: UTF8String; const X, Y: Single);
    procedure Render;
    function IsClicked: Boolean;

    property Bounds: TMZRect read FBounds;
  end;

type
  TDemoScene = class(TMZScene)
  private
    FFont: TMZFont;
    FTimerUpdate: TMZTimer;
    FTimerAddMiku: TMZTimer;
    FTexLogo: TMZTexture;
    FTexMiku: TMZTexture;
    FSpriteEngine: TMZSpriteEngine;
    FCounter: Integer;
    FButtonAdd: TSimpleButton;
    FButtonDelete: TSimpleButton;
    FButtonClear: TSimpleButton;
  private
    procedure TimerUpdateExpired(Sender: TObject);
    procedure TimerAddMikuExpired(Sender: TObject);
    procedure AddMiku;
    procedure DelMiku;
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

    { Summary:
        Is called during each iteration of the main loop to update the game
        state. The DeltaTimeMs is the number of milliseconds (1/1000th of a
        second) that has passed since the last call to Update.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update. }
    procedure Update(const DeltaTimeMs: Double); override;
  end;

type
  TMiku = class(TMZSprite)
  private
    { Movement speed in pixels per millisecond }
    FSpeedX: Single;
    FSpeedY: Single;
  public
    { Summary:
        Is called once just after construction to initialize the sprite.
        You can override this method to initialize some sprite properties or
        perform other custom initialization. }
    procedure Initialize; override;

    { Summary:
        Is called during each iteration of the main loop.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update. }
    procedure Update(const DeltaTimeMs: Double); override;
  end;

procedure RunDemo;
var
  Application: TMZApplication;
begin
  Application := TMZApplication.Create;
  Application.Options := Application.Options + [aoShowCursor] - [aoAllowPortraitOrientation];
  Application.Caption := '06 - SEngine';
  Application.ScreenWidth := 800;
  Application.ScreenHeight := 600;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TMiku }

procedure TMiku.Initialize;
begin
  inherited Initialize;
  X := 1024 + Random(1024);
  Y := Random(768 - 128);
  FSpeedX := -Random(10) / 5 - 0.5;
  FSpeedY := (Random(10) - 5) / 5;

  { In the original demo, FSpeed.X ranges from -2.5 .. -0.5 and FSpeed.Y ranges
    from -1.0 .. 1.0. In that demo, the position is updated in a timer that
    fires every 16ms.

    In this demo, we update the position in the TMZScene.Update method, which
    receives a DeltaTimeMs parameter with the time in milliseconds passes since
    the last update. This ensures a more consistent speed. However, this means
    that we need the set the speed variables in number of pixels per millisecond
    (instead of number of pixels per 16ms as in the original demo). }
  FSpeedX := FSpeedX / 16;
  FSpeedY := FSpeedY / 16;
end;

procedure TMiku.Update(const DeltaTimeMs: Double);
begin
  inherited Update(DeltaTimeMs);
  X := X + (FSpeedX * DeltaTimeMs);
  Y := Y + (FSpeedY * DeltaTimeMs);
  FrameNumber := FrameNumber + ((Abs(FSpeedX) + Abs(FSpeedY)) * DeltaTimeMs) / 25;
  if (FrameNumber > 8) then
    FrameNumber := 1;

  // RU: Если спрайт выходит за пределы по X/Y, сразу же удаляем его.
  // EN: Delete the sprite if it goes beyond X/Y.
  if (X < -128) or (Y < -128) or (Y > 768) then
    Free;
end;

{ TDemoScene }

procedure TDemoScene.TimerUpdateExpired(Sender: TObject);
begin
  Inc(FCounter, 2);

  // RU: По нажатию пробела очистить все спрайты.
  // EN: Delete all sprites if space was pressed.
  if FButtonClear.IsClicked then
    FSpriteEngine.Clear;

  if FButtonAdd.IsClicked then
    AddMiku;

  if FButtonDelete.IsClicked then
    DelMiku;

  if TMZKeyboard.IsKeyPressed(kcEscape) then
    Application.Quit;
  TMZKeyboard.ClearState;

  TMZMouse.ClearState;
end;

procedure TDemoScene.TimerAddMikuExpired(Sender: TObject);
begin
  AddMiku;
end;

procedure TDemoScene.AddMiku;
var
  I: Integer;
begin
  // RU: Добавить 100 спрайтов.
  // EN: Add 100 sprites.
  for I := 0 to 99 do
    TMiku.Create(FSpriteEngine, FTexMiku, Random(10));
end;

procedure TDemoScene.DelMiku;
var
  I, Count: Integer;
begin
  // RU: Удалим 100 спрайтов со случайным ID.
  // EN: Delete 100 sprites with random ID.
  for I := 0 to 99 do
  begin
    Count := FSpriteEngine.Count;
    if (Count = 0) then
      Break;
    FSpriteEngine[Random(Count)].Free;
  end;
end;

procedure TDemoScene.Startup;
var
  I: Integer;
begin
  inherited Startup;
  Randomize;
  FTexLogo := TMZTexture.Create(RESOURCE_DIRECTORY + 'zengl.png');
  FTexMiku := TMZTexture.Create(RESOURCE_DIRECTORY + 'miku.png');
  FTexMiku.SetFrameSize(128, 128);
  FSpriteEngine := TMZSpriteEngine.Create;

  // RU: Создадим 1000 спрайтов Miku-chan :)
  // EN: Create 1000 sprites of Miku-chan :)
  for I := 0 to 9 do
    AddMiku;

  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'font.zfi' );
  FTimerUpdate := TMZTimer.Create(TimerUpdateExpired, 16);
  FTimerAddMiku := TMZTimer.Create(TimerAddMikuExpired, 1000);

  FButtonAdd := TSimpleButton.Create(Canvas, FFont, 'Add Miku', 130, 4);
  FButtonDelete := TSimpleButton.Create(Canvas, FFont, 'Delete Miku',
    FButtonAdd.FBounds.X + FButtonAdd.FBounds.W + 4, 4);
  FButtonClear := TSimpleButton.Create(Canvas, FFont, 'Clear',
    FButtonDelete.FBounds.X + FButtonDelete.FBounds.W + 4, 4);
end;

procedure TDemoScene.Shutdown;
begin
  FTexLogo.Free;
  FTexMiku.Free;
  FSpriteEngine.Free;
  FFont.Free;
  FTimerUpdate.Free;
  FTimerAddMiku.Free;
  FButtonAdd.Free;
  FButtonDelete.Free;
  FButtonClear.Free;
  inherited Shutdown;
end;

procedure TDemoScene.RenderFrame;
begin
  inherited RenderFrame;
  Canvas.BeginBatch;
  try
    // RU: Рисуем все спрайты находящиеся в текущем спрайтовом менеджере.
    // EN: Render all sprites contained in current sprite engine.
    if (FCounter > 255) then
      FSpriteEngine.Render;

    if (FCounter <= 255) then
      Canvas.DrawSprite(FTexLogo, 512 - 256, 384 - 128, 512, 256, 0, FCounter)
    else if (FCounter < 510) then
    begin
      Canvas.FillRect(0, 0, 1024, 768, $000000, 510 - FCounter);
      Canvas.DrawSprite(FTexLogo, 512 - 256, 384 - 128, 512, 256, 0, 510 - FCounter)
    end;

    if (FCounter > 255) then
    begin
      Canvas.FillRect(0, 0, 120, 44, $000000, 200);
      Canvas.DrawText(FFont, 0, 0, 'FPS: ' + TMZUtils.IntToStr(Application.CurrentRenderFrameRate));
      Canvas.DrawText(FFont, 0, 20, 'Sprites: ' + TMZUtils.IntToStr(FSpriteEngine.Count));

      FButtonAdd.Render;
      FButtonDelete.Render;
      FButtonClear.Render;
    end;
  finally
    Canvas.EndBatch;
  end;
end;

procedure TDemoScene.Update(const DeltaTimeMs: Double);
begin
  inherited Update(DeltaTimeMs);
  // RU: Выполняем обработку всех спрайтов в текущем спрайтовом менеджере.
  // EN: Process all sprites contained in current sprite engine.
  FSpriteEngine.Update(DeltaTimeMs);
end;

{ TSimpleButton }

constructor TSimpleButton.Create(const Canvas: TMZCanvas; const Font: TMZFont;
  const Caption: UTF8String; const X, Y: Single);
begin
  inherited Create;
  FCanvas := Canvas;
  FFont := Font;
  FCaption := Caption;
  FBounds.X := X;
  FBounds.Y := Y;
  FBounds.W := FCanvas.CalculateTextWidth(FFont, FCaption) + 8;
  FBounds.H := FCanvas.CalculateTextHeight(FFont, FBounds.W, FCaption) + 8;
end;

function TSimpleButton.IsClicked: Boolean;
var
  P: TMZPoint;
begin
  P := TMZMouse.Position;
  Result := TMZMouse.IsButtonClicked(mbLeft);

  if (Result) then
    Result := FBounds.ContainsPoint(P);
end;

procedure TSimpleButton.Render;
begin
  FCanvas.FillRect(FBounds, $CBDEF4);
  FCanvas.DrawRect(FBounds, $15428B);
  FCanvas.DrawText(FFont, FBounds, 1, 0, FCaption, $FF, $15428B, [tfHAlignCenter, tfVAlignCenter]);
end;

end.
