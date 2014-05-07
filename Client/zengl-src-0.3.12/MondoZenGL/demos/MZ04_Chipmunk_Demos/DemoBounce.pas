unit DemoBounce;

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoBounce = class(TDemoScene)
  private
    procedure AddBox;
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

{ TDemoBounce }

procedure TDemoBounce.AddBox;
const
  SIZE = 10;
  MASS = 1;
  VERTS: array [0..3] of TCPVect = (
    (X: -SIZE; Y: -SIZE),
    (X: -SIZE; Y:  SIZE),
    (X:  SIZE; Y:  SIZE),
    (X:  SIZE; Y: -SIZE));
var
  Radius: TCPFloat;
  Body: TCPBody;
  Shape: TCPShape;
begin
  Radius := CPV(SIZE, SIZE).Length;

  Body := Space.AddBody(TCPBody.Create(MASS, TCPBody.MomentForPolygon(MASS, VERTS, CPVZero)));
  AutoFree(Body);
  Body.Position := CPV(
    Random(640 - Round(2 * Radius)) - (320 - Radius),
    Random(480 - Round(2 * Radius)) - (240 - Radius));
  Body.Velocity := CPV(2 * Random - 1, 2 * Random - 1) * 200;

  Shape := Space.AddShape(TCPPolyShape.Create(Body, VERTS, CPVZero));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 0;
end;

procedure TDemoBounce.Startup;
var
  Body, StaticBody: TCPBody;
  Shape: TCPShape;
  Constraint: TCPConstraint;
  I: Integer;
begin
  inherited;
  TCPShape.ResetIdCounter;
  Space.ResizeActiveHash(30, 1000);
  Space.Iterations := 10;

  StaticBody := Space.StaticBody;

  { Create segments around the edge of the screen. }
  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody,
    CPV(-320, -240), CPV(-320, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody,
    CPV(320, -240), CPV(320, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody,
    CPV(-320, -240), CPV(320, -240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody,
    CPV(-320, 240), CPV(320, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  for I := 0 to 9 do
    AddBox;

  Body := Space.AddBody(TCPBody.Create(100, 10000));
  AutoFree(Body);

  Shape := Space.AddShape(TCPSegmentShape.Create(Body, CPV(-75, 0), CPV(75, 0), 5));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;

  Constraint := Space.AddConstraint(TCPPivotJoint.Create(Body, StaticBody,
    CPVZero, CPVZero));
  AutoFree(Constraint);
end;

class function TDemoBounce.Title: UTF8String;
begin
  Result := 'Bounce';
end;

procedure TDemoBounce.UpdateFrame(const Ticks: Integer);
const
  STEPS = 3;
  DT = 1 / FRAMES_PER_SECOND / STEPS;
var
  I: Integer;
begin
  inherited;
  for I := 0 to STEPS - 1 do
    Space.Step(DT);
end;

end.