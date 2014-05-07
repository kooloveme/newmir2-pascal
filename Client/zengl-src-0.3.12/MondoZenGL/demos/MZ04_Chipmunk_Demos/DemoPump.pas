unit DemoPump;

interface

{$INCLUDE '..\..\src\mz_config.cfg'}
{$HINTS OFF}

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

const
  NUM_BALLS = 5;

type
  TDemoPump = class(TDemoScene)
  private
    FBalls: array [0..NUM_BALLS - 1] of TCPBody;
    FMotor: TCPSimpleMotor;
  private
    function AddBall(const Pos: TCPVect): TCPBody;
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

{ TDemoPump }

function TDemoPump.AddBall(const Pos: TCPVect): TCPBody;
var
  Shape: TCPShape;
begin
  Result := Space.AddBody(TCPBody.Create(1, TCPBody.MomentForCircle(1, 30, 0, CPVZero)));
  AutoFree(Result);
  Result.Position := Pos;

  Shape := Space.AddShape(TCPCircleShape.Create(Result, 30, CPVZero));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.5;
end;

procedure TDemoPump.Startup;
const
  BOTTOM = -300;
  TOP = 32;
  LEN = TOP - BOTTOM;
  VERTS: array [0..3] of TCPVect = (
    (X: -30; Y: -80),
    (X: -30; Y:  80),
    (X:  30; Y:  64),
    (X:  30; Y: -80));
var
  StaticBody, Plunger, SmallGear, BigGear, Feeder: TCPBody;
  Shape: TCPShape;
  Constraint: TCPConstraint;
  I: Integer;
  Anchor: TCPVect;
begin
  ShowArrowButtons := True;
  inherited;
  {$IFDEF iOS}
  Message := 'Keep "<--" or "-->" button down to control the pump';
  {$ELSE}
  Message := 'Keep "<--" or "-->" button down, or use the cursor keys, to control the pump';
  {$ENDIF}
  Space.Gravity := CPV(0, -600);
  StaticBody := Space.StaticBody;

  { beveling all of the line segments slightly helps prevent things from getting
    stuck on cracks }
  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-256, 16), CPV(-256, 300), 2));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.5;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-256, 16), CPV(-192, 0), 2));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.5;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-192, 0), CPV(-192, -64), 2));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.5;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-128, -64), CPV(-128, 144), 2));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.5;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-192, 80), CPV(-192, 176), 2));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.5;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-192, 176), CPV(-128, 240), 2));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-128, 144), CPV(192, 64), 2));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.5;
  Shape.Layers := NOT_GRABABLE_MASK;

  Plunger := Space.AddBody(TCPBody.Create(1, INFINITY));
  AutoFree(Plunger);
  Plunger.Position := CPV(-160, -80);

  Shape := Space.AddShape(TCPPolyShape.Create(Plunger, VERTS, CPVZero));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 0.5;
  Shape.Layers := 1;

  { add balls to hopper }
  for I := 0 to NUM_BALLS - 1 do
    FBalls[I] := AddBall(CPV(-224 + I, 80 + 64 * I));

  { add small gear }
  SmallGear := Space.AddBody(TCPBody.Create(10, TCPBody.MomentForCircle(10, 80, 0, CPVZero)));
  AutoFree(SmallGear);
  SmallGear.Position := CPV(-160, -160);
  SmallGear.Angle := -90;

  Shape := Space.AddShape(TCPCircleShape.Create(SmallGear, 80, CPVZero));
  AutoFree(Shape);
  Shape.Layers := 0;

  Constraint := Space.AddConstraint(TCPPivotJoint.Create(StaticBody, SmallGear, CPV(-160, -160), CPVZero));
  AutoFree(Constraint);

  { add big gear }
  BigGear := Space.AddBody(TCPBody.Create(40, TCPBody.MomentForCircle(40, 160, 0, CPVZero)));
  AutoFree(BigGear);
  BigGear.Position := CPV(80, -160);
  BigGear.Angle := 90;

  Shape := Space.AddShape(TCPCircleShape.Create(BigGear, 160, CPVZero));
  AutoFree(Shape);
  Shape.Layers := 0;

  Constraint := Space.AddConstraint(TCPPivotJoint.Create(StaticBody, BigGear, CPV(80, -160), CPVZero));
  AutoFree(Constraint);

  { connect the plunger to the small gear. }
  Constraint := Space.AddConstraint(TCPPinJoint.Create(SmallGear, Plunger, CPV(80, 0), CPV(0, 0)));
  AutoFree(Constraint);

  { connect the gears }
  Constraint := Space.AddConstraint(TCPGearJoint.Create(SmallGear, BigGear, -90, -2));
  AutoFree(Constraint);

  { feeder mechanism }
  Feeder := Space.AddBody(TCPBody.Create(1,
    TCPBody.MomentForSegment(1, CPV(-224, BOTTOM), CPV(-224, TOP))));
  AutoFree(Feeder);
  Feeder.Position := CPV(-224, (BOTTOM + TOP) / 2);

  Shape := Space.AddShape(TCPSegmentShape.Create(Feeder, CPV(0, LEN / 2), CPV(0, -LEN / 2), 20));
  AutoFree(Shape);

  Constraint := Space.AddConstraint(TCPPivotJoint.Create(StaticBody, Feeder,
    CPV(-224, BOTTOM), CPV(0, -LEN / 2)));
  AutoFree(Constraint);

  Anchor := Feeder.WorldToLocal(CPV(-224, -160));
  Constraint := Space.AddConstraint(TCPPinJoint.Create(Feeder, SmallGear,
    Anchor, CPV(0, 80)));
  AutoFree(Constraint);

  { motorize the second gear }
  FMotor := TCPSimpleMotor.Create(StaticBody, BigGear, 3);
  AutoFree(FMotor);
  Space.AddConstraint(FMotor);
end;

class function TDemoPump.Title: UTF8String;
begin
  Result := 'Pump';
end;

procedure TDemoPump.UpdateFrame(const Ticks: Integer);
const
  STEPS = 2;
  DT = 1 / FRAMES_PER_SECOND / STEPS;
var
  Coef, Rate: TCPFloat;
  I, J: Integer;
  Ball: TCPBody;
begin
  inherited;
  Coef := (2 + ArrowDirection.Y) / 3;
  Rate := ArrowDirection.X * 30 * Coef;

  FMotor.Rate := Rate * RAD2DEG;
  if (Rate <> 0) then
    FMotor.MaxForce := 1000000
  else
    FMotor.MaxForce := 0;

  for I := 0 to STEPS - 1 do
  begin
    Space.Step(DT);

    for J := 0 to NUM_BALLS - 1 do
    begin
      Ball := FBalls[J];
      if (Ball.Position.X > 320) then
      begin
        Ball.Velocity := CPVZero;
        Ball.Position := CPV(-224, 200);
      end;
    end;
  end;
end;

end.
