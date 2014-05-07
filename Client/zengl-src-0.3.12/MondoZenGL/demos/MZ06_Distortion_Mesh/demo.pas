unit Demo;

{$INCLUDE '../../src/mz_config.cfg'}

interface

procedure RunDemo;

implementation

uses
  SysUtils,
  Math,
  { You don't necessarily have to use these units. However, Delphi will not
    inline some methods if these units are not in the uses clause. }
  {$IFNDEF FPC}
  {$IFDEF USE_ZENGL_STATIC}
  zgl_text,
  zgl_font,
  zgl_main,
  zgl_math_2d,
  zgl_keyboard,
  zgl_utils,
  zgl_textures,
  zgl_grid_2d,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  {$ENDIF}
  MondoZenGL;

const
  SCREEN_WIDTH = 1024;
  SCREEN_HEIGHT = 768;
  {$IFDEF DARWIN}
  RESOURCE_DIRECTORY = '';
  {$ELSE}
  RESOURCE_DIRECTORY = '../data/';
  {$ENDIF}

type
  TDemoScene = class(TMZScene)
  private
    FFont: TMZFont;
    FTexture: TMZTexture;
    FDistortionMesh: TMZDistortionMesh;
    FTime: Single;
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

procedure RunDemo;
var
  Application: TMZApplication;
begin
  Randomize;
  Application := TMZApplication.Create;
  Application.Options := Application.Options + [aoShowCursor, aoUseSound] - [aoAllowPortraitOrientation];
  Application.Caption := 'MZ06 - Distortion Mesh';
  Application.ScreenWidth := SCREEN_WIDTH;
  Application.ScreenHeight := SCREEN_HEIGHT;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TDemoScene }

const
  ROWS = 16;
  COLS = 20;

procedure TDemoScene.Startup;
begin
  inherited Startup;
  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'font.zfi');
  FTexture := TMZTexture.Create(RESOURCE_DIRECTORY + 'back01.jpg');
  FDistortionMesh := TMZDistortionMesh.Create(ROWS, COLS);
end;

procedure TDemoScene.Update(const DeltaTimeMs: Double);
begin
  inherited;
  FTime := FTime + 0.001 * DeltaTimeMs;
  if TMZKeyboard.IsKeyPressed(kcEscape) then
    Application.Quit;
  TMZKeyboard.ClearState;
end;

procedure TDemoScene.Shutdown;
begin
  FDistortionMesh.Free;
  FTexture.Free;
  FFont.Free;
  inherited Shutdown;
end;

procedure TDemoScene.RenderFrame;
const
  CELL_WIDTH = 800 div COLS;
  CELL_HEIGHT = 600 div ROWS;
var
  X, Y: Integer;
  R, A: Single;
begin
  inherited RenderFrame;
  for X := 0 to COLS do
    for Y := 0 to ROWS do
    begin
      R := Sqrt(Power(X - COLS / 2, 2) + Power(Y - ROWS / 2, 2));
      A := R * Cos(FTime * 2) * 0.1;
      FDistortionMesh[Y, X] := TMZPoint.Create(
        Sin(A) * (Y * CELL_HEIGHT - 300) + Cos(A) * (X * CELL_WIDTH - 400),
        Cos(A) * (Y * CELL_HEIGHT - 300) - Sin(A) * (X * CELL_WIDTH - 400));
    end;

  Canvas.DrawText(FFont, 2, 2, 'FPS: ' + TMZUtils.IntToStr(Application.CurrentRenderFrameRate));
  FDistortionMesh.Render(FTexture, SCREEN_WIDTH / 2, SCREEN_HEIGHT / 2);
end;

end.
