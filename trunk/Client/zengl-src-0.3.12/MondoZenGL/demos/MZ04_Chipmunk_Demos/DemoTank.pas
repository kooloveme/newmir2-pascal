unit DemoTank;
{$HINTS OFF}

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoTank = class(TDemoScene)
  private
    FTankBody: TCPBody;
    FTankControlBody: TCPBody;
  private
    function AddBox(const Size, Mass: TCPFloat): TCPBody;
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

{ TDemoTank }

function TDemoTank.AddBox(const Size, Mass: TCPFloat): TCPBody;
var
  Verts: array [0..3] of TCPVect;
  Radius: TCPFloat;
  Shape: TCPShape;
begin
  Verts[0] := CPV(-Size, -Size);
  Verts[1] := CPV(-Size,  Size);
  Verts[2] := CPV( Size,  Size);
  Verts[3] := CPV( Size, -Size);

  Radius := CPV(Size, Size).Length;

  Result := Space.AddBody(TCPBody.Create(Mass, TCPBody.MomentForPolygon(Mass, Verts, CPVZero)));
  AutoFree(Result);
  Result.Position := CPV(Random(640 - Trunc(2 * Radius)) - (320 - Radius),
                         Random(480 - Trunc(2 * Radius)) - (240 - Radius));

  Shape := Space.AddShape(TCPPolyShape.Create(Result, Verts, CPVZero));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.7;
end;

procedure TDemoTank.Startup;
var
  StaticBody, Body: TCPBody;
  Shape: TCPShape;
  I: Integer;
  Pivot: TCPPivotJoint;
  Gear: TCPGearJoint;
begin
  inherited;
  UseBoxTexture := True;
  TCPShape.ResetIdCounter;
  Space.ResizeActiveHash(30, 1000);
  Space.Iterations := 10;
  Space.SleepTimeThreshold := 0.5;
  StaticBody := Space.StaticBody;

  // Create segments around the edge of the screen.
  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-320, -240), CPV(-320, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(320, -240), CPV(320, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-320, -240), CPV(320, -240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-320, 240), CPV(320, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  for I := 0 to 49 do
  begin
    Body := AddBox(10, 1);

    Pivot := TCPPivotJoint.Create(StaticBody, Body, CPVZero, CPVZero);
    AutoFree(Pivot);
    Space.AddConstraint(Pivot);
    Pivot.BiasCoef := 0; // disable joint correction
    Pivot.MaxForce := 1000; // emulate linear friction

    Gear := TCPGearJoint.Create(StaticBody, Body, 0, 1);
    AutoFree(Gear);
    Space.AddConstraint(Gear);
    Gear.BiasCoef := 0; // disable joint correction
    Gear.MaxForce := 5000; // emulate angular friction
  end;

  // We joint the tank to the control body and control the tank indirectly by
  // modifying the control body.
  FTankControlBody := TCPBody.Create(INFINITY, INFINITY);
  AutoFree(FTankControlBody);
  FTankBody := AddBox(15, 10);

  Pivot := TCPPivotJoint.Create(FTankControlBody, FTankBody, CPVZero, CPVZero);
  AutoFree(Pivot);
  Space.AddConstraint(Pivot);
  Pivot.BiasCoef := 0; // disable joint correction
  Pivot.MaxForce := 10000; // emulate linear friction

  Gear := TCPGearJoint.Create(FTankControlBody, FTankBody, 0, 1);
  AutoFree(Gear);
  Space.AddConstraint(Gear);
  Gear.BiasCoef := 1; // limit angular correction rate
  Gear.MaxBias := 1; // limit angular correction rate
  Gear.MaxForce := 500000; // emulate angular friction
end;

class function TDemoTank.Title: UTF8String;
begin
  Result := 'Tank';
end;

procedure TDemoTank.UpdateFrame(const Ticks: Integer);
var
  MouseDelta: TCPVect;
  Turn, Direction: TCPFloat;
begin
  inherited;
  // turn the control body based on the angle relative to the actual body
  MouseDelta := MousePoint - FTankBody.Position;
  Turn := FTankBody.Rotation.Unrotate(MouseDelta).ToAngle;
  FTankControlBody.Angle := FTankBody.Angle - Turn;

  // drive the tank towards the mouse
  if MousePoint.Near(FTankBody.Position, 30) then
    FTankControlBody.Velocity := CPVZero
  else
  begin
    if ((MouseDelta * FTankBody.Rotation) > 0) then
      Direction := 1
    else
      Direction := -1;
    FTankControlBody.Velocity := FTankBody.Rotation.Rotate(CPV(30 * Direction, 0));
  end;

  Space.Step(1 / FRAMES_PER_SECOND);
end;

end.
