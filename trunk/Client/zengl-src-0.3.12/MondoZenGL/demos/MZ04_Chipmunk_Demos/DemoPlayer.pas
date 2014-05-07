unit DemoPlayer;
{$HINTS OFF}

interface

uses
  Classes,
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TPlayer = record
    Friction: TCPFloat;
    Shape: TCPShape;
    GroundNormal: TCPVect;
    GroundShapes: TList;
  end;

type
  TDemoPlayer = class(TDemoScene)
  private
    FPlayerInstance: TPlayer;
    FLastJumpState: Boolean;
  private
    procedure PlayerUpdateVelocity(const Body: TCPBody; const Gravity: TCPVect;
      const Damping: TCPFloat; const DT: TCPFloat);
    function CollisionBegin(const Space: TCPSpace;
      const Arbiter: PCPArbiter; const Data: Pointer): TCPBool;
    function CollisionPreSolve(const Space: TCPSpace;
      const Arbiter: PCPArbiter; const Data: Pointer): TCPBool;
    procedure CollisionSeparate(const Space: TCPSpace;
      const Arbiter: PCPArbiter; const Data: Pointer);
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure Shutdown; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

uses
  Math;

{ TDemoPlayer }

function TDemoPlayer.CollisionBegin(const Space: TCPSpace;
  const Arbiter: PCPArbiter; const Data: Pointer): TCPBool;
var
  A, B: TCPShape;
  N: TCPVect;
begin
  Arbiter.GetShapes(A, B);
  N := -Arbiter.GetNormal(0);
  if (N.Y > 0) then
    FPlayerInstance.GroundShapes.Add(B);
  Result := True;
end;

function TDemoPlayer.CollisionPreSolve(const Space: TCPSpace;
  const Arbiter: PCPArbiter; const Data: Pointer): TCPBool;
var
  A, B: TCPShape;
  N: TCPVect;
begin
  Arbiter.GetShapes(A, B);
  if (Arbiter.IsFirstContact) then
  begin
    A.Friction := FPlayerInstance.Friction;

    // pick the most upright jump normal each frame
    N := -Arbiter.GetNormal(0);
    if (N.Y >= FPlayerInstance.GroundNormal.Y) then
      FPlayerInstance.GroundNormal := N;
  end;
  Result := True;
end;

procedure TDemoPlayer.CollisionSeparate(const Space: TCPSpace;
  const Arbiter: PCPArbiter; const Data: Pointer);
var
  A, B: TCPShape;
begin
  Arbiter.GetShapes(A, B);
  FPlayerInstance.GroundShapes.Remove(B);
  if (FPlayerInstance.GroundShapes.Count = 0) then
  begin
    A.Friction := 0;
    FPlayerInstance.GroundNormal := CPVZero;
  end;
end;

procedure TDemoPlayer.PlayerUpdateVelocity(const Body: TCPBody;
  const Gravity: TCPVect; const Damping, DT: TCPFloat);
begin
  Body.UpdateVelocity(Gravity, Damping, DT);
  Body.Velocity := CPV(EnsureRange(Body.Velocity.X, -400, 400),
                       Max(Body.Velocity.Y, -700));
end;

procedure TDemoPlayer.Shutdown;
begin
  inherited;
  FPlayerInstance.GroundShapes.Free;
end;

procedure TDemoPlayer.Startup;
const
  RADIUS = 15;
var
  Body, StaticBody: TCPBody;
  Shape: TCPShape;
begin
  inherited;
  UseBallTexture := True;
  TCPShape.ResetIdCounter;
  Space.Iterations := 10;
  Space.Gravity := CPV(0, -1500);
  StaticBody := Space.StaticBody;

  { Create segments around the edge of the screen. }
  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-320, -240), CPV(-320, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;
  Shape.CollisionType := 2;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(320, -240), CPV(320, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;
  Shape.CollisionType := 2;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-320, -240), CPV(320, -240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;
  Shape.CollisionType := 2;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-320, 240), CPV(320, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;
  Shape.CollisionType := 2;

  // add some other segments to play with
  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-220, -200), CPV(-220, 240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;
  Shape.CollisionType := 2;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(0, -240), CPV(320, -200), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;
  Shape.CollisionType := 2;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(200, -240), CPV(320, -100), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;
  Shape.CollisionType := 2;

  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-220, -80), CPV(200, -80), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;
  Shape.CollisionType := 2;

  // Set up the player
  Body := Space.AddBody(TCPBody.Create(10, INFINITY));
  AutoFree(Body);
  Body.Position := CPV(0, -220);
  Body.OnUpdateVelocity := PlayerUpdateVelocity;

  Shape := Space.AddShape(TCPCircleShape.Create(Body, RADIUS, CPVZero));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 2;
  Shape.CollisionType := 1;

  FPlayerInstance.Friction := Shape.Friction;
  FPlayerInstance.Shape := Shape;
  FPlayerInstance.GroundShapes := TList.Create;

  Space.AddCollisionHandler(1, 2, CollisionBegin, CollisionPreSolve, nil, CollisionSeparate, nil);
end;

class function TDemoPlayer.Title: UTF8String;
begin
  Result := 'Player';
end;

procedure TDemoPlayer.UpdateFrame(const Ticks: Integer);
const
  STEPS = 3;
  DT    = 1 / FRAMES_PER_SECOND / STEPS;
var
  JumpState: Boolean;
  Body: TCPBody;
  GroundNormal: TCPVect;
  AirAccel: TCPFloat;
  I: Integer;
begin
  inherited;
  JumpState := (ArrowDirection.Y > 0);
  Body := FPlayerInstance.Shape.Body;
  GroundNormal := FPlayerInstance.GroundNormal;
  if (GroundNormal.Y > 0) then
  begin
    FPlayerInstance.Shape.SurfaceVelocity := GroundNormal.Perp * (400 * ArrowDirection.X);
    if (ArrowDirection.X <> 0) then
      Body.Activate;
  end
  else
    FPlayerInstance.Shape.SurfaceVelocity := CPVZero;

  // Apply jump
  if JumpState and (not FLastJumpState) and (GroundNormal.LengthSq > 0) then
  begin
    Body.Velocity := Body.Velocity
      + (GroundNormal.SphericalLinearInterpolate(CPV(0, 1), 0.75) * 500);
    Body.Activate;
  end;

  if (FPlayerInstance.GroundShapes.Count = 0) then
  begin
    AirAccel := Body.Velocity.X + ArrowDirection.X * 2000;
    Body.Force := CPV(Body.Mass * AirAccel, Body.Force.Y);
  end;

  for I := 0 to STEPS - 1 do
    Space.Step(DT);

  FLastJumpState := JumpState;
end;

end.
