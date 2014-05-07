unit Demo;

{$INCLUDE '../../src/mz_config.cfg'}

interface

procedure RunDemo;

implementation

uses
  Contnrs,
  Classes,
  SysUtils,
  { You don't necessarily have to use these units. However, Delphi will not
    inline some methods if these units are not in the uses clause. }
  {$IFNDEF FPC}
  {$IFDEF USE_ZENGL_STATIC}
  zgl_keyboard,
  zgl_utils,
  zgl_text,
  zgl_render_2d,
  zgl_main,
  zgl_font,
  zgl_primitives_2d,
  zgl_mouse,
  zgl_collision_2d,
  zgl_math_2d,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  zglChipmunk,
  {$ENDIF}
  MondoZenGL,
  mzChipmunk;

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
    FChecked: Boolean;
  public
    constructor Create(const Canvas: TMZCanvas; const Font: TMZFont;
      const Caption: UTF8String; const X, Y: Single);
    procedure Render;
    function IsClicked: Boolean;

    property Bounds: TMZRect read FBounds;
    property Checked: Boolean read FChecked write FChecked;
  end;

type
  TDemoScene = class(TMZScene)
  private
    FFont: TMZFont;
    FSpace: TCPSpace;
    FChildren: TObjectList;
    FButtonBox: TSimpleButton;
    FButtonBall: TSimpleButton;
    FUseBall: Boolean;
  private
    procedure RenderShape(const Space: TCPSpace;
      const Shape: TCPShape; const Data: Pointer);
    procedure RenderArbiter(const Space: TCPSpace;
      const Arbiter: PCPArbiter; const Data: Pointer);
    procedure AddBox(const X, Y: Integer);
    procedure AddBall(const X, Y: Integer);
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
  Application.Options := Application.Options + [aoShowCursor] - [aoAllowPortraitOrientation];
  Application.Caption := '11 - Physics 2D';
  Application.ScreenWidth := 1024;
  Application.ScreenHeight := 768;
  Application.SetScene(TDemoScene.Create);
  { The application and scene will automatically be freed on shutdown }
end;

{ TDemoScene }

procedure TDemoScene.AddBall(const X, Y: Integer);
const
  MASS       = 1;
  RADIUS     = 16;
  ELASTICITY = 0.5;
  FRICTION   = 0.9;
var
  Body: TCPBody;
  Shape: TCPShape;
begin
  Body := TCPBody.Create(MASS, TCPBody.MomentForCircle(MASS, 0, RADIUS, CPVZero));
  Body.Position := CPV(X, Y);
  FChildren.Add(Body);
  FSpace.AddBody(Body);

  Shape := TCPCircleShape.Create(Body, RADIUS, CPVZero);
  Shape.Elasticity := ELASTICITY;
  Shape.Friction := FRICTION;
  FChildren.Add(Shape);
  FSpace.AddShape(Shape);
end;

procedure TDemoScene.AddBox(const X, Y: Integer);
const
  WIDTH      = 48;
  HEIGHT     = 32;
  MASS       = 1;
  ELASTICITY = 0.5;
  FRICTION   = 0.5;
  VERTS: array [0..3] of TCPVect = (
    (X: -WIDTH / 2; Y: -HEIGHT / 2),
    (X: -WIDTH / 2; Y:  HEIGHT / 2),
    (X:  WIDTH / 2; Y:  HEIGHT / 2),
    (X:  WIDTH / 2; Y: -HEIGHT / 2));
var
  Body: TCPBody;
  Shape: TCPShape;
  Moment: TCPFloat;
begin
  Moment := TCPBody.MomentForPolygon(MASS, VERTS, CPVZero);

  Body := TCPBody.Create(MASS, Moment);
  Body.Position := CPV((X - 10) + (WIDTH / 2), (Y - 10) + (HEIGHT / 2));
  FChildren.Add(Body);
  FSpace.AddBody(Body);

  Shape := TCPPolyShape.Create(Body, VERTS, CPVZero);
  Shape.Elasticity := ELASTICITY;
  Shape.Friction := FRICTION;
  FChildren.Add(Shape);
  FSpace.AddShape(Shape);
end;

procedure TDemoScene.Startup;
const
  ELASTICITY = 1;
  FRICTION   = 0.9;
var
  StaticBody: TCPBody;
  Ground: TCPShape;
begin
  inherited Startup;
  FFont := TMZFont.Create(RESOURCE_DIRECTORY + 'font.zfi' );
  FChildren := TObjectList.Create;
  TCPChipmunk.Initialize;

  FButtonBox := TSimpleButton.Create(Canvas, FFont, 'Box', 10, 50);
  FButtonBox.Checked := True;
  FButtonBall := TSimpleButton.Create(Canvas, FFont, 'Ball',
    FButtonBox.Bounds.X + FButtonBox.Bounds.W + 4, 50);
  // RU: Создаем новый "мир".
  // EN: Create new world.
  FSpace := TCPSpace.Create;

  // RU: Задаем количество итераций обработки(рекомендуется 10).
  // EN: Set count of iterations of processing(recommended is 10).
  FSpace.Iterations := 10;
  FSpace.ElasticIterations := 10;

  // RU: Задаем силу гравитации.
  // EN: Set the gravity.
  FSpace.Gravity := CPV(0, 256);

  // RU: Задаем коэффициент "затухания" движения объектов.
  // EN: Set the damping for moving of objects.
  FSpace.Damping := 0.9;

  // RU: Создадим статичное "тело".
  // EN: Create a static "body".
  StaticBody := TCPBody.Create(INFINITY, INFINITY);
  FChildren.Add(StaticBody);

  // RU: Добавим три отрезка для ограничения мира.
  // Первый параметр - указатель на созданное тело, два последующих - координаты точек отрезка, последний - толщина отрезка.
  //
  // EN: Add three segments for restriction of world.
  // First parameter - pointer of created body, next two - coordinates of segment points, the last one - width of segment.
  Ground := TCPSegmentShape.Create(StaticBody, CPV(5, 0), CPV(5, 758), 1);
  FChildren.Add(Ground);
  Ground.Elasticity := ELASTICITY;
  Ground.Friction := FRICTION;
  FSpace.AddStaticShape(Ground);

  Ground := TCPSegmentShape.Create(StaticBody, CPV(1019, 0), CPV(1019, 758), 1);
  FChildren.Add(Ground);
  Ground.Elasticity := ELASTICITY;
  Ground.Friction := FRICTION;
  FSpace.AddStaticShape(Ground);

  Ground := TCPSegmentShape.Create(StaticBody, CPV(0, 758), CPV(1024, 758), 1);
  FChildren.Add(Ground);
  Ground.Elasticity := ELASTICITY;
  Ground.Friction := FRICTION;
  FSpace.AddStaticShape(Ground);

  // RU: Добавим треугольник.
  // EN: Add the triangle.
  StaticBody := TCPBody.Create(INFINITY, INFINITY);
  FChildren.Add(StaticBody);

  Ground := TCPSegmentShape.Create(StaticBody, CPV(512, 384), CPV(312, 434), 1);
  FChildren.Add(Ground);
  Ground.Elasticity := ELASTICITY;
  Ground.Friction := FRICTION;
  FSpace.AddStaticShape(Ground);

  Ground := TCPSegmentShape.Create(StaticBody, CPV(312, 434), CPV(812, 434), 1);
  FChildren.Add(Ground);
  Ground.Elasticity := ELASTICITY;
  Ground.Friction := FRICTION;
  FSpace.AddStaticShape(Ground);

  Ground := TCPSegmentShape.Create(StaticBody, CPV(812, 434), CPV(512, 384), 1);
  FChildren.Add(Ground);
  Ground.Elasticity := ELASTICITY;
  Ground.Friction := FRICTION;
  FSpace.AddStaticShape(Ground);
end;

procedure TDemoScene.RenderShape(const Space: TCPSpace; const Shape: TCPShape;
  const Data: Pointer);
var
  Color: Cardinal absolute Data;
  Circle: TCPCircleShape absolute Shape;
  Segment: TCPSegmentShape absolute Shape;
  Poly: TCPPolyShape absolute Shape;
  I: Integer;
  V1, V2: TCPVect;
begin
  case Shape.ShapeType of
    stCircle:
      begin
        Canvas.DrawCircle(Circle.TC.X, Circle.TC.Y, Circle.Radius, Color);
        Canvas.DrawLine(Circle.TC.X, Circle.TC.Y,
          Circle.TC.X + Circle.Body.Rotation.X * Circle.Radius,
          Circle.TC.Y + Circle.Body.Rotation.Y * Circle.Radius, Color);
      end;

    stSegment:
      begin
        Canvas.DrawLine(Segment.T1.X, Segment.T1.Y, Segment.T2.X, Segment.T2.Y,
          Color);
      end;

    stPoly:
      begin
        for I := 0 to Poly.VertexCount - 2 do
        begin
          V1 := Poly.TV[I];
          V2 := Poly.TV[I + 1];
          Canvas.DrawLine(V1.X, V1.Y, V2.X, V2.Y, Color);
        end;
        V1 := Poly.TV[Poly.VertexCount - 1];
        V2 := Poly.TV[0];
        Canvas.DrawLine(V1.X, V1.Y, V2.X, V2.Y, Color);
      end;
  end;
end;

procedure TDemoScene.RenderArbiter(const Space: TCPSpace;
  const Arbiter: PCPArbiter; const Data: Pointer);
var
  I: Integer;
  V: TCPVect;
begin
  for I := 0 to Arbiter.ContactCount - 1 do
  begin
    V := Arbiter.ContactPoints[I].Point;
    Canvas.FillCircle(V.X, V.Y, 4, $FF0000, 255, 8);
  end;
end;

procedure TDemoScene.RenderFrame;
begin
  Canvas.BeginBatch;
  try
    Canvas.AntiAlias := True;
    FSpace.ForEachStaticShape(RenderShape, Pointer($00FF00));
    FSpace.ForEachShape(RenderShape, Pointer($0000FF));
    FSpace.ForEachArbiter(RenderArbiter, nil);

    Canvas.DrawText(FFont, 10, 5, 'FPS: ' + TMZUtils.IntToStr(Application.CurrentRenderFrameRate));
    {$IFDEF iOS}
    Canvas.DrawText(FFont, 10, 25, 'Use your fingers: tap to add a box or ball');
    {$ELSE}
    Canvas.DrawText(FFont, 10, 25, 'Use your mouse: click to add a box or ball');
    {$ENDIF}

    FButtonBox.Render;
    FButtonBall.Render;
  finally
    Canvas.EndBatch;
  end;
end;

procedure TDemoScene.Shutdown;
begin
  FChildren.Free;
  FSpace.Free;
  FFont.Free;
  FButtonBox.Free;
  FButtonBall.Free;
  inherited Shutdown;
end;

procedure TDemoScene.Update(const DeltaTimeMs: Double);
begin
  inherited Update(DeltaTimeMs);
  FSpace.Step(DeltaTimeMs / 1000);

  if FButtonBox.IsClicked then
    FUseBall := False
  else if FButtonBall.IsClicked then
    FUseBall := True
  else if TMZMouse.IsButtonClicked(mbLeft) then
  begin
    if FUseBall then
      AddBall(TMZMouse.X, TMZMouse.Y)
    else
    AddBox(TMZMouse.X, TMZMouse.Y);
  end;

  FButtonBox.Checked := (not FUseBall);
  FButtonBall.Checked := FUseBall;

  if TMZKeyboard.IsKeyPressed(kcEscape) then
    Application.Quit;

  TMZKeyboard.ClearState;
  TMZMouse.ClearState;
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
  if (FChecked) then
  begin
    FCanvas.FillRect(FBounds, $15428B);
    FCanvas.DrawText(FFont, FBounds, FCaption, [tfHAlignCenter, tfVAlignCenter]);
  end
  else
  begin
    FCanvas.FillRect(FBounds, $CBDEF4);
    FCanvas.DrawRect(FBounds, $15428B);
    FCanvas.DrawText(FFont, FBounds, 1, 0, FCaption, $FF, $15428B, [tfHAlignCenter, tfVAlignCenter]);
  end;
end;

end.
