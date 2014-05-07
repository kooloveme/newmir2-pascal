unit DemoOneWay;
{$HINTS OFF}

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoOneWay = class(TDemoScene)
  private
    FPlatformDirection: TCPVect; // direction objects may pass through
  private
    function PreSolve(const Space: TCPSpace; const Arbiter: PCPArbiter;
      const Data: Pointer): TCPBool;
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

{ TDemoOneWay }

function TDemoOneWay.PreSolve(const Space: TCPSpace; const Arbiter: PCPArbiter;
  const Data: Pointer): TCPBool;
var
  A, B: TCPShape;
begin
  Arbiter.GetShapes(A, B);
  if ((Arbiter.GetNormal(0) * FPlatformDirection) < 0) then
  begin
    Arbiter.Ignore;
    Result := False;
  end
  else
    Result := True;
end;

procedure TDemoOneWay.Startup;
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
  Space.Gravity := CPV(0, -100);
  StaticBody := Space.StaticBody;

  { Create segments around the edge of the screen. }
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

  // Add our one way segment
  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-160, -100), CPV(160, -100), 10));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.CollisionType := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  FPlatformDirection := CPV(0, 1); // let objects pass upwards

  // Add a ball to make things more interesting
  Body := Space.AddBody(TCPBody.Create(10, TCPBody.MomentForCircle(10, 0, RADIUS, CPVZero)));
  AutoFree(Body);
  Body.Position := CPV(0, -200);
  Body.Velocity := CPV(0, 170);

  Shape := Space.AddShape(TCPCircleShape.Create(Body, RADIUS, CPVZero));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.9;
  Shape.CollisionType := 2;

  Space.AddCollisionHandler(1, 2, nil, PreSolve, nil, nil, nil);
end;

class function TDemoOneWay.Title: UTF8String;
begin
  Result := 'One Way Platforms';
end;

procedure TDemoOneWay.UpdateFrame(const Ticks: Integer);
begin
  inherited;
  Space.Step(1 / FRAMES_PER_SECOND);
end;

end.
