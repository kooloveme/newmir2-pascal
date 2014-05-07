unit DemoJoints;
{$HINTS OFF}

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoJoints = class(TDemoScene)
  private
    function AddBall(const Pos, BoxOffset: TCPVect): TCPBody;
    function AddBar(const Pos, BoxOffset: TCPVect): TCPBody;
    function AddLever(const Pos, BoxOffset: TCPVect): TCPBody;
    function AddWheel(const Pos, BoxOffset: TCPVect): TCPBody;
    function AddChassis(const Pos, BoxOffset: TCPVect): TCPBody;
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

{ TDemoJoints }

function TDemoJoints.AddBall(const Pos, BoxOffset: TCPVect): TCPBody;
const
  RADIUS = 15;
  MASS   = 1;
var
  Shape: TCPShape;
begin
  Result := Space.AddBody(TCPBody.Create(MASS,
    TCPBody.MomentForCircle(MASS, 0, RADIUS, CPVZero)));
  AutoFree(Result);
  Result.Position := Pos + BoxOffset;

  Shape := Space.AddShape(TCPCircleShape.Create(Result, RADIUS, CPVZero));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.7;
end;

function TDemoJoints.AddBar(const Pos, BoxOffset: TCPVect): TCPBody;
const
  MASS = 2;
var
  A, B: TCPVect;
  Shape: TCPShape;
begin
  A := CPV(0, 30);
  B := CPV(0, -30);
  Result := Space.AddBody(TCPBody.Create(MASS,
    TCPBody.MomentForSegment(MASS, A, B)));
  AutoFree(Result);
  Result.Position := Pos + BoxOffset;

  Shape := Space.AddShape(TCPSegmentShape.Create(Result, A, B, 5));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.7;
end;

function TDemoJoints.AddChassis(const Pos, BoxOffset: TCPVect): TCPBody;
const
  MASS = 5;
  NUM  = 4;
  VERTS: array [0..NUM - 1] of TCPVect = (
    (X: -40; Y: -15),
    (X: -40; Y:  15),
    (X:  40; Y:  15),
    (X:  40; Y: -15));
var
  Shape: TCPShape;
begin
  Result := Space.AddBody(TCPBody.Create(MASS,
    TCPBody.MomentForPolygon(MASS, VERTS, CPVZero)));
  AutoFree(Result);
  Result.Position := Pos + BoxOffset;

  Shape := Space.AddShape(TCPPolyShape.Create(Result, VERTS, CPVZero));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.7;
  Shape.Group := 1; // use a group to keep the car parts from colliding
end;

function TDemoJoints.AddLever(const Pos, BoxOffset: TCPVect): TCPBody;
const
  MASS = 1;
var
  A, B: TCPVect;
  Shape: TCPShape;
begin
  A := CPV(0, 15);
  B := CPV(0, -15);
  Result := Space.AddBody(TCPBody.Create(MASS,
    TCPBody.MomentForSegment(MASS, A, B)));
  AutoFree(Result);
  Result.Position := Pos + (BoxOffset + CPV(0, -15));

  Shape := Space.AddShape(TCPSegmentShape.Create(Result, A, B, 5));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.7;
end;

function TDemoJoints.AddWheel(const Pos, BoxOffset: TCPVect): TCPBody;
const
  RADIUS = 15;
  MASS   = 1;
var
  Shape: TCPShape;
begin
  Result := Space.AddBody(TCPBody.Create(MASS,
    TCPBody.MomentForCircle(MASS, 0, RADIUS, CPVZero)));
  AutoFree(Result);
  Result.Position := Pos + BoxOffset;

  Shape := Space.AddShape(TCPCircleShape.Create(Result, RADIUS, CPVZero));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.7;
  Shape.Group := 1; // use a group to keep the car parts from colliding
end;

procedure TDemoJoints.Startup;
var
  StaticBody, Body1, Body2, Wheel1, Wheel2, Chassis: TCPBody;
  Shape: TCPShape;
  BoxOffset, PosA, PosB: TCPVect;
begin
  inherited;
  Space.Iterations := 10;
  Space.Gravity := CPV(0, -100);
  Space.SleepTimeThreshold := 0.5;
  StaticBody := Space.StaticBody;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-320, 240), CPV(320, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-320, 120), CPV(320, 120), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-320, 0), CPV(320, 0), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-320, -120), CPV(320, -120), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-320, -240), CPV(320, -240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;


  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-320, -240), CPV(-320, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-160, -240), CPV(-160, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(0, -240), CPV(0, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(160, -240), CPV(160, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(320, -240), CPV(320, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  PosA := CPV(50, 60);
  PosB := CPV(110, 60);

  // Pin Joints - Link shapes with a solid bar or pin.
  // Keeps the anchor points the same distance apart from when the joint was created.
  BoxOffset := CPV(-320, -240);
  Body1 := AddBall(PosA, BoxOffset);
  Body2 := AddBall(PosB, BoxOffset);
  AutoFree(Space.AddConstraint(TCPPinJoint.Create(Body1, Body2, CPV(15, 0),
    CPV(-15, 0))));

  // Slide Joints - Like pin joints but with a min/max distance.
  // Can be used for a cheap approximation of a rope.
  BoxOffset := CPV(-160, -240);
  Body1 := AddBall(PosA, BoxOffset);
  Body2 := AddBall(PosB, BoxOffset);
  AutoFree(Space.AddConstraint(TCPSlideJoint.Create(Body1, Body2, CPV(15, 0),
    CPV(-15, 0), 20, 40)));

  // Pivot Joints - Holds the two anchor points together. Like a swivel.
  BoxOffset := CPV(0, -240);
  Body1 := AddBall(PosA, BoxOffset);
  Body2 := AddBall(PosB, BoxOffset);
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body1, Body2,
    BoxOffset + CPV(80, 60))));

  // Groove Joints - Like a pivot joint, but one of the anchors is a line segment
  // that the pivot can slide in
  BoxOffset := CPV(160, -240);
  Body1 := AddBall(PosA, BoxOffset);
  Body2 := AddBall(PosB, BoxOffset);
  AutoFree(Space.AddConstraint(TCPGrooveJoint.Create(Body1, Body2, CPV(30, 30),
    CPV(30, -30), CPV(-30, 0))));

  // Damped Springs
  BoxOffset := CPV(-320, -120);
  Body1 := AddBall(PosA, BoxOffset);
  Body2 := AddBall(PosB, BoxOffset);
  AutoFree(Space.AddConstraint(TCPDampedSpring.Create(Body1, Body2, CPV(15, 0),
    CPV(-15, 0), 20, 5, 0.3)));

  // Damped Rotary Springs
  BoxOffset := CPV(-160, -120);
  Body1 := AddBar(PosA, BoxOffset);
  Body2 := AddBar(PosB, BoxOffset);
  // Add some pin joints to hold the circles in place.
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body1, StaticBody, PosA + BoxOffset)));
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body2, StaticBody, PosB + BoxOffset)));
  AutoFree(Space.AddConstraint(TCPDampedRotarySpring.Create(Body1, Body2,
    0, 3000, 60)));

  // Rotary Limit Joint
  BoxOffset := CPV(0, -120);
  Body1 := AddLever(PosA, BoxOffset);
  Body2 := AddLever(PosB, BoxOffset);
  // Add some pin joints to hold the circles in place.
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body1, StaticBody, PosA + BoxOffset)));
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body2, StaticBody, PosB + BoxOffset)));
  // Hold their rotation within 90 degrees of each other.
  AutoFree(Space.AddConstraint(TCPRotaryLimitJoint.Create(Body1, Body2, -90, 90)));

  // Ratchet Joint - A rotary ratchet, like a socket wrench
  BoxOffset := CPV(160, -120);
  Body1 := AddLever(PosA, BoxOffset);
  Body2 := AddLever(PosB, BoxOffset);
  // Add some pin joints to hold the circles in place.
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body1, StaticBody, PosA + BoxOffset)));
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body2, StaticBody, PosB + BoxOffset)));
  // Ratchet every 90 degrees
  AutoFree(Space.AddConstraint(TCPRatchetJoint.Create(Body1, Body2, 0, 90)));

  // Gear Joint - Maintain a specific angular velocity ratio
  BoxOffset := CPV(-320, 0);
  Body1 := AddBar(PosA, BoxOffset);
  Body2 := AddBar(PosB, BoxOffset);
  // Add some pin joints to hold the circles in place.
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body1, StaticBody, PosA + BoxOffset)));
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body2, StaticBody, PosB + BoxOffset)));
  // Force one to sping 2x as fast as the other
  AutoFree(Space.AddConstraint(TCPGearJoint.Create(Body1, Body2, 0, 2)));

  // Simple Motor - Maintain a specific angular relative velocity
  BoxOffset := CPV(-160, 0);
  Body1 := AddBar(PosA, BoxOffset);
  Body2 := AddBar(PosB, BoxOffset);
  // Add some pin joints to hold the circles in place.
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body1, StaticBody, PosA + BoxOffset)));
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body2, StaticBody, PosB + BoxOffset)));
  // Make them spin at 1/2 revolution per second in relation to each other.
  AutoFree(Space.AddConstraint(TCPSimpleMotor.Create(Body1, Body2, 180)));

  // Make a car with some nice soft suspension
  BoxOffset := CPV(0, 0);
  Wheel1 := AddWheel(PosA, BoxOffset);
  Wheel2 := AddWheel(PosB, BoxOffset);
  Chassis := AddChassis(CPV(80, 100), BoxOffset);

  AutoFree(Space.AddConstraint(TCPGrooveJoint.Create(Chassis, Wheel1,
    CPV(-30, -10), CPV(-30, -40), CPVZero)));
  AutoFree(Space.AddConstraint(TCPGrooveJoint.Create(Chassis, Wheel2,
    CPV( 30, -10), CPV( 30, -40), CPVZero)));

  AutoFree(Space.AddConstraint(TCPDampedSpring.Create(Chassis, Wheel1,
    CPV(-30, 0), CPVZero, 50, 20, 1.5)));
  AutoFree(Space.AddConstraint(TCPDampedSpring.Create(Chassis, Wheel2,
    CPV( 30, 0), CPVZero, 50, 20, 1.5)));
end;

class function TDemoJoints.Title: UTF8String;
begin
  Result := 'Joints and Constraints';
end;

procedure TDemoJoints.UpdateFrame(const Ticks: Integer);
begin
  inherited;
  Space.Step(1 / FRAMES_PER_SECOND);
end;

end.
