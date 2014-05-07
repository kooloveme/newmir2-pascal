unit DemoTumble;

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoTumble = class(TDemoScene)
  private
    FStaticBody: TCPBody;
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

{ TDemoTumble }

procedure TDemoTumble.Startup;
const
  { Vertexes for the bricks }
  NUM = 4;
  VERTS: array [0..NUM - 1] of TCPVect = (
    (X: -30; Y: -15),
    (X: -30; Y:  15),
    (X:  30; Y:  15),
    (X:  30; Y: -15));
var
  Body: TCPBody;
  Shape: TCPShape;
  A, B, C, D: TCPVect;
  I, J: Integer;
begin
  inherited;
  FStaticBody := TCPBody.Create(INFINITY, INFINITY);
  AutoFree(FStaticBody);

  TCPShape.ResetIdCounter;
  Space.ResizeActiveHash(40, 999);
  Space.ResizeStaticHash(40, 99);
  Space.Gravity := CPV(0, -600);

  { Set up the static box. }
  A := CPV(-200, -200);
  B := CPV(-200,  200);
  C := CPV( 200,  200);
  D := CPV( 200, -200);

  Shape := Space.AddShape(TCPSegmentShape.Create(FStaticBody, A, B, 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(FStaticBody, B, C, 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(FStaticBody, C, D, 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  Shape := Space.AddShape(TCPSegmentShape.Create(FStaticBody, D, A, 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  { Give the box a little spin.
    Because staticBody is never added to the space, we will need to update it
    ourselves (see UpdateFrame).
    NOTE: Normally you would want to add the segments as normal and not static
    shapes. I'm just doing it to demonstrate the TCPSpace.RehashStatic
    function. }
  FStaticBody.RotationalVelocity := 23;

  { Add the bricks. }
  for I := 0 to 2 do
  begin
    for J := 0 to 7 do
    begin
      Body := Space.AddBody(TCPBody.Create(1, TCPBody.MomentForPolygon(1, VERTS, CPVZero)));
      AutoFree(Body);
      Body.Position := CPV(I * 60 - 150, J * 30 - 150);

      Shape := Space.AddShape(TCPPolyShape.Create(Body, VERTS, CPVZero));
      AutoFree(Shape);
      Shape.Elasticity := 0;
      Shape.Friction := 0.7;
    end;
  end;
end;

class function TDemoTumble.Title: UTF8String;
begin
  Result := 'Tumble';
end;

procedure TDemoTumble.UpdateFrame(const Ticks: Integer);
const
  STEPS = 3;
  DT = 1 / FRAMES_PER_SECOND / STEPS;
var
  I: Integer;
begin
  inherited;
  for I := 0 to STEPS - 1 do
  begin
    Space.Step(DT);

    { Manually update the position of the static shape so that the box rotates }
    FStaticBody.UpdatePosition(DT);

    { Because the box was added as a static shape and we moved it
      we need to manually rehash the static spatial hash. }
    Space.RehashStatic;
  end;
end;

end.