unit DemoSensors;
{$HINTS OFF}

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

const
  BALL_TYPE            = 0;
  BLOCKING_SENSOR_TYPE = 1;
  CATCH_SENSOR_TYPE    = 2;

type
  TEmitter = record
    Queue: Integer;
    Blocked: Integer;
    Position: TCPVect;
  end;

type
  TDemoSensors = class(TDemoScene)
  private
    FEmitterInstance: TEmitter;
  private
    function BlockerBegin(const Space: TCPSpace;
      const Arbiter: PCPArbiter; const Data: Pointer): TCPBool;
    procedure BlockerSeparate(const Space: TCPSpace;
      const Arbiter: PCPArbiter; const Data: Pointer);
    function CatcherBegin(const Space: TCPSpace;
      const Arbiter: PCPArbiter; const Data: Pointer): TCPBool;
    procedure PostStepRemove(const Space: TCPSpace; const Obj, Data: Pointer);
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

function FRandUnit: TCPFloat;
begin
  Result := (2 * Random) - 1;
end;

{ TDemoSensors }

function TDemoSensors.BlockerBegin(const Space: TCPSpace;
  const Arbiter: PCPArbiter; const Data: Pointer): TCPBool;
begin
  Inc(FEmitterInstance.Blocked);
  Result := False; // Return values from sensors callbacks are ignored
end;

procedure TDemoSensors.BlockerSeparate(const Space: TCPSpace;
  const Arbiter: PCPArbiter; const Data: Pointer);
begin
  Dec(FEmitterInstance.Blocked);
end;

function TDemoSensors.CatcherBegin(const Space: TCPSpace;
  const Arbiter: PCPArbiter; const Data: Pointer): TCPBool;
var
  A, B: TCPShape;
begin
  Arbiter.GetShapes(A, B);
  Inc(FEmitterInstance.Queue);
  Space.AddPostStepCallback(PostStepRemove, B, nil);
  Result := False;
end;

procedure TDemoSensors.PostStepRemove(const Space: TCPSpace; const Obj,
  Data: Pointer);
var
  Shape: TCPShape;
begin
  Shape := TCPShape(Obj);
  Space.RemoveBody(Shape.Body);
  Space.RemoveShape(Shape);

  RemoveAutoFree(Shape.Body);
  RemoveAutoFree(Shape);
end;

procedure TDemoSensors.Startup;
var
  StaticBody: TCPBody;
  Shape: TCPShape;
begin
  inherited;
  UseBallTexture := True;
  TCPShape.ResetIdCounter;
  Space.Iterations := 10;
  Space.Gravity := CPV(0, -100);
  StaticBody := Space.StaticBody;

  // Data structure for our ball emitter
  // We'll use two sensors for it, one to see if the emitter is blocked
  // a second to catch the balls and add them back to the emitter
  FEmitterInstance.Queue := 5;
  FEmitterInstance.Blocked := 0;
  FEmitterInstance.Position := CPV(0, 150);

  // Create our blocking sensor, so we know when the emitter is clear to emit
  // another ball
  Shape := Space.AddShape(TCPCircleShape.Create(StaticBody, 15, FEmitterInstance.Position));
  AutoFree(Shape);
  Shape.Sensor := True;
  Shape.CollisionType := BLOCKING_SENSOR_TYPE;

  // Create our catch sensor to requeue the balls when they reach the bottom of
  // the screen
  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(-2000, -200),
    CPV(2000, -200), 15));
  AutoFree(Shape);
  Shape.Sensor := True;
  Shape.CollisionType := CATCH_SENSOR_TYPE;

  Space.AddCollisionHandler(BLOCKING_SENSOR_TYPE, BALL_TYPE, BlockerBegin, nil,
    nil, BlockerSeparate, nil);

  Space.AddCollisionHandler(CATCH_SENSOR_TYPE, BALL_TYPE, CatcherBegin, nil,
    nil, nil, nil);
end;

class function TDemoSensors.Title: UTF8String;
begin
  Result := 'Sensors';
end;

procedure TDemoSensors.UpdateFrame(const Ticks: Integer);
var
  Body: TCPBody;
  Shape: TCPShape;
begin
  inherited;
  if (FEmitterInstance.Blocked = 0) and (FEmitterInstance.Queue > 0) then
  begin
    Dec(FEmitterInstance.Queue);
    Body := Space.AddBody(TCPBody.Create(1, TCPBody.MomentForCircle(1, 15, 0, CPVZero)));
    AutoFree(Body);
    Body.Position := FEmitterInstance.Position;
    Body.Velocity := CPV(FRandUnit, FRandUnit) * 100;

    Shape := Space.AddShape(TCPCircleShape.Create(Body, 15, CPVZero));
    AutoFree(Shape);
    Shape.CollisionType := BALL_TYPE;
  end;

  Space.Step(1 / FRAMES_PER_SECOND);
end;

end.
