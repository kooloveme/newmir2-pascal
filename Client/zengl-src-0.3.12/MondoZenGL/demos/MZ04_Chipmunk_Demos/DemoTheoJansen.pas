unit DemoTheoJansen;

interface

{$INCLUDE '..\..\src\mz_config.cfg'}
{$HINTS OFF}

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoTheoJansen = class(TDemoScene)
  private
    FMotor: TCPSimpleMotor;
  private
    procedure MakeLeg(const Side, Offset: TCPFloat; const Chassis,
      Crank: TCPBody; const Anchor: TCPVect);
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

const
  SEG_RADIUS = 3;

{ TDemoTheoJansen }

procedure TDemoTheoJansen.MakeLeg(const Side, Offset: TCPFloat; const Chassis,
  Crank: TCPBody; const Anchor: TCPVect);
const
  LEG_MASS = 1;
var
  A, B: TCPVect;
  Shape: TCPShape;
  UpperLeg, LowerLeg: TCPBody;
  Constraint: TCPConstraint;
  PinJoint: TCPPinJoint;
  Diag: TCPFloat;
begin
  { Make leg }
  A := CPVZero;
  B := CPV(0, Side);
  UpperLeg := TCPBody.Create(LEG_MASS, TCPBody.MomentForSegment(LEG_MASS, A, B));
  AutoFree(UpperLeg);
  UpperLeg.Position := CPV(Offset, 0);
  Space.AddBody(UpperLeg);
  Shape := Space.AddShape(TCPSegmentShape.Create(UpperLeg, A, B, SEG_RADIUS));
  AutoFree(Shape);
  Constraint := Space.AddConstraint(TCPPivotJoint.Create(Chassis, UpperLeg,
    CPV(Offset, 0), CPVZero));
  AutoFree(Constraint);

  { Lower leg }
  A := CPVZero;
  B := CPV(0, -Side);
  LowerLeg := TCPBody.Create(LEG_MASS, TCPBody.MomentForSegment(LEG_MASS, A, B));
  AutoFree(LowerLeg);
  LowerLeg.Position := CPV(Offset, -Side);
  Space.AddBody(LowerLeg);
  Shape := Space.AddShape(TCPSegmentShape.Create(LowerLeg, A, B, SEG_RADIUS));
  AutoFree(Shape);
  Shape.Group := 1;
  Shape := Space.AddShape(TCPCircleShape.Create(LowerLeg, SEG_RADIUS * 2, B));
  AutoFree(Shape);
  Shape.Group := 1;
  Shape.Elasticity := 0;
  Shape.Friction := 1;
  Constraint := Space.AddConstraint(TCPPinJoint.Create(Chassis, LowerLeg, CPV(Offset, 0), CPVZero));
  AutoFree(Constraint);
  Constraint := Space.AddConstraint(TCPGearJoint.Create(UpperLeg, LowerLeg, 0, 1));
  AutoFree(Constraint);

  Diag := Sqrt(SIDE * SIDE + Offset * Offset);

  PinJoint := TCPPinJoint.Create(Crank, UpperLeg, Anchor, CPV(0, SIDE));
  AutoFree(PinJoint);
  PinJoint.Distance := Diag;
  Space.AddConstraint(PinJoint);

  PinJoint := TCPPinJoint.Create(Crank, LowerLeg, Anchor, CPVZero);
  AutoFree(PinJoint);
  PinJoint.Distance := Diag;
  Space.AddConstraint(PinJoint);
end;

procedure TDemoTheoJansen.Startup;
const
  OFFSET       = 30;
  CHASSIS_MASS = 2;
  CRANK_MASS   = 1;
  CRANK_RADIUS = 13;
  SIDE         = 30;
  NUM_LEGS     = 2;
var
  StaticBody, Chassis, Crank: TCPBody;
  Shape: TCPShape;
  Constraint: TCPConstraint;
  A, B: TCPVect;
  I: Integer;
begin
  ShowArrowButtons := True;
  inherited;
  {$IFDEF iOS}
  Message := 'Use "<--" and "-->" buttons to control the machine';
  {$ELSE}
  Message := 'Use "<--" and "-->" buttons or the cursor keys to control the machine';
  {$ENDIF}
  TCPShape.ResetIdCounter;
  Space.Iterations := 20;
  Space.Gravity := CPV(0, -500);
  StaticBody := Space.StaticBody;

  { Create segments around the edge of the screen. }
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

  { Make chassis }
  A := CPV(-OFFSET, 0);
  B := CPV(OFFSET, 0);
  Chassis := TCPBody.Create(CHASSIS_MASS, TCPBody.MomentForSegment(CHASSIS_MASS, A, B));
  AutoFree(Chassis);
  Space.AddBody(Chassis);

  Shape := TCPSegmentShape.Create(Chassis, A, B, SEG_RADIUS);
  AutoFree(Shape);
  Shape.Group := 1;
  Space.AddShape(Shape);

  { Make crank }
  Crank := TCPBody.Create(CRANK_MASS, TCPBody.MomentForCircle(CRANK_MASS, CRANK_RADIUS, 0, CPVZero));
  AutoFree(Crank);
  Space.AddBody(Crank);

  Shape := TCPCircleShape.Create(Crank, CRANK_RADIUS, CPVZero);
  AutoFree(Shape);
  Shape.Group := 1;
  Space.AddShape(Shape);

  Constraint := Space.AddConstraint(TCPPivotJoint.Create(
    Chassis, Crank, CPVZero, CPVZero));
  AutoFree(Constraint);

  for I := 0 to NUM_LEGS - 1 do
  begin
    MakeLeg(SIDE,  OFFSET, Chassis, Crank,
      TCPVect.ForAngle((2 * I + 0) / NUM_LEGS * 180) * CRANK_RADIUS);
    MakeLeg(SIDE, -OFFSET, Chassis, Crank,
      TCPVect.ForAngle((2 * I + 1) / NUM_LEGS * 180) * CRANK_RADIUS);
  end;

  FMotor := TCPSimpleMotor.Create(Chassis, Crank, 6);
  AutoFree(FMotor);
  Space.AddConstraint(FMotor)
end;

class function TDemoTheoJansen.Title: UTF8String;
begin
  Result := 'Theo Jansen Machine';
end;

procedure TDemoTheoJansen.UpdateFrame(const Ticks: Integer);
const
  STEPS = 3;
  DT = 1 / FRAMES_PER_SECOND / STEPS;
var
  Coef, Rate: TCPFloat;
  I: INteger;
begin
  inherited;
  Coef := (2 + ArrowDirection.Y) / 3;
  Rate := ArrowDirection.X * 10 * Coef;
  FMotor.Rate := Rate * RAD2DEG;
  if (Rate <> 0) then
    FMotor.MaxForce := 100000
  else
    FMotor.MaxForce := 0;

  for I := 0 to STEPS - 1 do
    Space.Step(DT);
end;

end.
