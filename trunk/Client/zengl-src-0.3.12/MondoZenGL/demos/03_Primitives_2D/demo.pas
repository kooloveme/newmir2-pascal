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
  zgl_main,
  zgl_math_2d,
  zgl_primitives_2d,
  zgl_fx,
  zgl_keyboard,
  zgl_render_2d,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  {$ENDIF}
  MondoZenGL;

type
  TDemoScene = class(TMZScene)
  private
    FTimer: TMZTimer;
    FCenter: TMZPoint;
    FPoints: array [0..359] of TMZPoint;
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
  Application := TMZApplication.Create;
  Application.Options := Application.Options + [aoShowCursor] - [aoAllowPortraitOrientation];
  Application.Caption := '03 - Primitives 2D';
  Application.ScreenWidth := 1024;
  Application.ScreenHeight := 768;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TDemoScene }

procedure TDemoScene.TimerExpired(Sender: TObject);
begin
  Application.Caption := TMZUtils.Format('03 - Primitives 2D (%d fps)',
    [Application.CurrentRenderFrameRate]);
end;

procedure TDemoScene.Startup;
var
  I: Integer;
begin
  inherited Startup;
  Randomize;
  FTimer := TMZTimer.Create(TimerExpired, 1000);
  FCenter := TMZPoint.Create(512, 384);
  for I := 0 to 359 do
    FPoints[I] := TMZPoint.Create(
      512 + TMZMath.Cos(I) * (96 + Random(32)),
      384 + TMZMath.Sin(I) * (96 + Random(32)));
end;

procedure TDemoScene.Shutdown;
begin
  FTimer.Free;
  inherited Shutdown;
end;

procedure TDemoScene.RenderFrame;
var
  I: Integer;
begin
  inherited RenderFrame;
  Canvas.AntiAlias := False;

  // RU: Устанавливаем цвет и альфу для каждой вершины.
  // EN: Set color and alpha for each vertex.
  Canvas.SetPerVertexColors($FF0000, $00FF00, $0000FF, $FFFFFF, 255, 255, 255, 255);

  // RU: Рисуем прямоугольник с заливкой(флаг PR2D_FILL) с использованием отдельных цветов для каждой вершины(флаг FX2D_VCA).
  // EN: Render filled rectangle(flag PR2D_FILL) and use different colors for each vertex(flag FX2D_VCA).
  Canvas.FillRect(0, 0, 1024, 768, $000000, 255, [efPerVertexColors]);

  // RU: Рисуем в центре экрана круг с радиусом 128 пиксела.
  // EN: Render circle in center of screen with radius 128 pixels.
  Canvas.FillCircle(FCenter, 128, $000000, 155, 32);

  // RU: Т.к. далее идет вывод однотипных примитивов(линий) следует воспользоваться оптимизацией по количеству DIP'ов.
  // Не обязательно заключать в batch2d_Begin/batch2d_End только подобные участки рендера, вполне можно заключить в них всю Draw функцию.
  // Учитывая, что данный пример довольно простой в плане нагрузки на видеокарту, вполне возможно что с batch2d функциями FPS будет ниже :)
  // Дело в том, что при высоких FPS'ах ограничителем становится процессор, время которого тратится на проверки и пр.
  //
  // EN: Because code below is render one type of primitive(lines) it's better to enable DIP-optiomization.
  // No need to conclude only this code in batch2d_Begin/batch2d_End. Whole Draw function can be concluded.
  // Because this example is too easy for videocard, maybe FPS will be smaller with batch2d functions :)
  // The fact is that with high FPS's - bottleneck is CPU.
  Canvas.BeginBatch;
  Inc(FCounter);
  if (FCounter > 359) then
    FCounter := 0;
  FPoints[FCounter] := TMZPoint.Create(
    512 + TMZMath.Cos(FCounter) * (96 + Random(32)),
    384 + TMZMath.Sin(FCounter) * (96 + Random(32)));

  // RU: Рисуем линии внутри круга.
  // EN: Render lines inside the cricle.
  for I := 0 to 359 do
    Canvas.DrawLine(FCenter, FPoints[I], $FFFFFF);
  Canvas.EndBatch;

  Canvas.AntiAlias := True;
  Canvas.FillEllipse(512 + 300, 384, 64, 256, $FFFFFF, 55);
  Canvas.DrawEllipse(512 + 300, 384, 64, 256, $000000);

  Canvas.FillEllipse(512 - 300, 384, 64, 256, $FFFFFF, 55);
  Canvas.DrawEllipse(512 - 300, 384, 64, 256, $000000);
end;

procedure TDemoScene.Update(const DeltaTimeMs: Double);
begin
  inherited Update(DeltaTimeMs);
  if TMZKeyboard.IsKeyPressed(kcEscape) then
    Application.Quit;
  TMZKeyboard.ClearState;
end;

end.
