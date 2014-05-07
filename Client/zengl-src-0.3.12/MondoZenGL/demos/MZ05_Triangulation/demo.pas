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
  zgl_text,
  zgl_font,
  zgl_main,
  zgl_math_2d,
  zgl_keyboard,
  zgl_utils,
  zgl_primitives_2d,
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
    FRotationAngle: Double;
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
  Application.Caption := 'MZ05 - Triangulation';
  Application.ScreenWidth := SCREEN_WIDTH;
  Application.ScreenHeight := SCREEN_HEIGHT;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TDemoScene }

procedure TDemoScene.Startup;
begin
  inherited Startup;
  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'font.zfi');
end;

procedure TDemoScene.Update(const DeltaTimeMs: Double);
begin
  inherited;
  if TMZKeyboard.IsKeyPressed(kcEscape) then
    Application.Quit;
  TMZKeyboard.ClearState;

  FRotationAngle := FRotationAngle + 0.0005 * DeltaTimeMs;
end;

procedure TDemoScene.Shutdown;
begin
  FFont.Free;
  inherited Shutdown;
end;

procedure TDemoScene.RenderFrame;
var
  Pentagon: array [0..4] of TMZPoint;
  Square: array [0..3] of TMZPoint;
  Triangles: TMZTriangleArray;
  I: Integer;
  Angle: Double;
begin
  inherited RenderFrame;
  Canvas.DrawText(FFont, 2, 2, 'This demo shows how to triangulate a pentagon with a square hole');
  Canvas.DrawText(FFont, 2, 22, 'FPS: ' + TMZUtils.IntToStr(Application.CurrentRenderFrameRate));

  { Start triangulation with a pentagon shape }
  for I := 0 to 4 do
  begin
    Angle := FRotationAngle + 0.2 * (2 * Pi * I);
    Pentagon[I] := TMZPoint.Create(512 + 350 * Cos(Angle), 384 + 350 * Sin(Angle));
  end;
  TMZTriangulator.BeginTriangulation(Pentagon);

  { Add a square hole }
  for I := 0 to 3 do
  begin
    Angle := 0.25 * (2 * Pi * I) - (0.3 * FRotationAngle);
    Square[I] := TMZPoint.Create(512 + 150 * Cos(Angle), 384 + 150 * Sin(Angle));
  end;
  TMZTriangulator.AddHole(Square);

  { Get the result of the triangulation }
  Triangles := TMZTriangulator.EndTriangulation;

  { Draw the triangles }
  Canvas.AntiAlias := True;
  Canvas.FillTriangleList(Triangles, $FF0000);
  Canvas.DrawTriangleList(Triangles, $FFFFFF);
end;

end.
