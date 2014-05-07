unit DemoPyramidStack;
{$HINTS OFF}

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoPyramidStack = class(TDemoScene)
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

{ TDemoPyramidStack }

procedure TDemoPyramidStack.Startup;
const
  RADIUS = 15;
var
  Body, StaticBody: TCPBody;
  Shape: TCPShape;
  I, J: Integer;
begin
  inherited;
  UseBallTexture := True;
  UseBoxTexture := True;
  TCPShape.ResetIdCounter;
  Space.Iterations := 30;
  Space.ResizeStaticHash(40, 1000);
  Space.ResizeActiveHash(40, 1000);
  Space.Gravity := CPV(0, -100);
  Space.SleepTimeThreshold := 0.5;

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

  { Add lots of boxes. }
  for I := 0 to 13 do
  begin
    for J := 0 to I do
    begin
      Body := Space.AddBody(TCPBody.Create(1, TCPBody.MomentForBox(1, 30, 30)));
      AutoFree(Body);
      Body.Position := CPV(J * 32 - I * 16, 300 - I * 32);

      Shape := Space.AddShape(TCPPolyShape.CreateBox(Body, 30, 30));
      AutoFree(Shape);
      Shape.Elasticity := 0;
      Shape.Friction := 0.8;
    end;
  end;

  { Add a ball to make things more interesting }
  Body := Space.AddBody(TCPBody.Create(10, TCPBody.MomentForCircle(10, 0, RADIUS, CPVZero)));
  AutoFree(Body);
  Body.Position := CPV(0, -240 + RADIUS + 5);

  Shape := Space.AddShape(TCPCircleShape.Create(Body, RADIUS, CPVZero));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.9;
end;

class function TDemoPyramidStack.Title: UTF8String;
begin
  Result := 'Pyramid Stack';
end;

procedure TDemoPyramidStack.UpdateFrame(const Ticks: Integer);
const
  STEPS = 3;
  DT = 1 / FRAMES_PER_SECOND / STEPS;
var
  I: Integer;
begin
  for I := 0 to STEPS - 1 do
    Space.Step(DT);
end;

end.
