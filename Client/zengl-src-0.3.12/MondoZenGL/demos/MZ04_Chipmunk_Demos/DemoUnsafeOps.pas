unit DemoUnsafeOps;

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
  NUM_CIRCLES = 30;

type
  TDemoUnsafeOps = class(TDemoScene)
  private
    FCircleRadius: TCPFloat;
    FCircles: array [0..NUM_CIRCLES - 1] of TCPCircleShape;
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

{ TDemoUnsafeOps }

procedure TDemoUnsafeOps.Startup;
var
  Body, StaticBody: TCPBody;
  Shape: TCPShape;
  I: Integer;
begin
  ShowArrowButtons := True;
  inherited;
  {$IFDEF iOS}
  Message := 'Use "<--" and "-->" buttons to shrink/grow the balls';
  {$ELSE}
  Message := 'Use "<--" and "-->" buttons or cursor keys to shrink/grow the balls';
  {$ENDIF}
  UseBallTexture := True;
  FCircleRadius := 30;
  Space.Iterations := 5;
  Space.Gravity := CPV(0, -100);
  Space.ResizeStaticHash(40, 999);
  Space.ResizeActiveHash(30, 2999);

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

  for I := 0 to NUM_CIRCLES - 1 do
  begin
    Body := Space.AddBody(TCPBody.Create(1, TCPBody.MomentForCircle(1, 0, FCircleRadius, CPVZero)));
    AutoFree(Body);
    Body.Position := CPV(Random * 2 - 1, Random * 2 -1) * (FCircleRadius * 5);

    Shape := Space.AddShape(TCPCircleShape.Create(Body, FCircleRadius, CPVZero));
    AutoFree(Shape);
    FCircles[I] := Shape as TCPCircleShape;
    Shape.Elasticity := 0;
    Shape.Friction := 1;
  end;

  Message := 'Methods that start with a "Unsafe" prefix are functions for changing shapes, ' +
    'but they can cause severe stability problems if used'#10 +
    'incorrectly. Shape changes occur as instantaneous changes to position without an accompanying velocity change. '#10 +
    'USE WITH CAUTION!';
end;

class function TDemoUnsafeOps.Title: UTF8String;
begin
  Result := 'Unsafe Operations';
end;

procedure TDemoUnsafeOps.UpdateFrame(const Ticks: Integer);
const
  DT = 1 / FRAMES_PER_SECOND;
var
  I: Integer;
begin
  inherited;
  if (ArrowDirection.X <> 0) then
  begin
    FCircleRadius := FCircleRadius + ArrowDirection.X;
    if (FCircleRadius < 10) then
      FCircleRadius := 10;

    for I := 0 to NUM_CIRCLES - 1 do
    begin
      FCircles[I].Body.Mass := TCPBody.MomentForCircle(1, 0, FCircleRadius, CPVZero);
      FCircles[I].UnsafeSetRadius(FCircleRadius);
    end;
  end;

  Space.Step(DT);
end;

end.
