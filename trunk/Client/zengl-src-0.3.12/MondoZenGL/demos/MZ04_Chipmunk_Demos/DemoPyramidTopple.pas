unit DemoPyramidTopple;

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoPyramidTopple = class(TDemoScene)
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

{ TDemoPyramidTopple }

procedure TDemoPyramidTopple.Startup;
const
  N = 9;

  { Shared friction constant. }
  FRICTION = 0.6;

  { Vertexes for the dominos. }
  NUM = 4;
  VERTS: array [0..NUM - 1] of TCPVect = (
    (X: -3; Y: -20),
    (X: -3; Y:  20),
    (X:  3; Y:  20),
    (X:  3; Y: -20));
var
  Body: TCPBody;
  Shape: TCPShape;
  I, J: Integer;
  Offset: TCPVect;
begin
  inherited;
  TCPShape.ResetIdCounter;
  Space.Iterations := 30;
  Space.ResizeActiveHash(30, 2999);
  Space.ResizeStaticHash(30, 999);
  Space.Gravity := CPV(0, -300);
  Space.SleepTimeThreshold := 0.5;

  { Add a floor.}
  Shape := Space.AddShape(TCPSegmentShape.Create(Space.StaticBody,
    CPV(-600, -240), CPV(600, -240), 0));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;

  { Add the dominoes. Skim over this. It doesn't do anything fancy,
    and it's hard to follow. }
  for I := 1 to N do
  begin
    Offset := CPV(-I * 60 / 2, (N - I) * 52);

    for J := 0 to I - 1 do
    begin
      Body := Space.AddBody(TCPBody.Create(1, TCPBody.MomentForPolygon(1, VERTS, CPVZero)));
      AutoFree(Body);
      Body.Position := CPV(J * 60, -220) + Offset;

      Shape := Space.AddShape(TCPPolyShape.Create(Body, VERTS, CPVZero));
      AutoFree(Shape);
      Shape.Elasticity := 0;
      Shape.Friction := FRICTION;

      Body := Space.AddBody(TCPBody.Create(1, TCPBody.MomentForPolygon(1, VERTS, CPVZero)));
      AutoFree(Body);
      Body.Position := CPV(J * 60, -197) + Offset;
      Body.Angle := 90;

      Shape := Space.AddShape(TCPPolyShape.Create(Body, VERTS, CPVZero));
      AutoFree(Shape);
      Shape.Elasticity := 0;
      Shape.Friction := FRICTION;

      if (J <> (I - 1)) then
      begin
        Body := Space.AddBody(TCPBody.Create(1, TCPBody.MomentForPolygon(1, VERTS, CPVZero)));
        AutoFree(Body);
        Body.Position := CPV(J * 60 + 30, -191) + Offset;
        Body.Angle := 90;

        Shape := Space.AddShape(TCPPolyShape.Create(Body, VERTS, CPVZero));
        AutoFree(Shape);
        Shape.Elasticity := 0;
        Shape.Friction := FRICTION;
      end;
    end;

    Body := Space.AddBody(TCPBody.Create(1, TCPBody.MomentForPolygon(1, VERTS, CPVZero)));
    AutoFree(Body);
    Body.Position := CPV(-17, -174) + Offset;

    Shape := Space.AddShape(TCPPolyShape.Create(Body, VERTS, CPVZero));
    AutoFree(Shape);
    Shape.Elasticity := 0;
    Shape.Friction := FRICTION;

    Body := Space.AddBody(TCPBody.Create(1, TCPBody.MomentForPolygon(1, VERTS, CPVZero)));
    AutoFree(Body);
    Body.Position := CPV((I - 1) * 60 + 17, - 174) + Offset;

    Shape := Space.AddShape(TCPPolyShape.Create(Body, VERTS, CPVZero));
    AutoFree(Shape);
    Shape.Elasticity := 0;
    Shape.Friction := FRICTION;
  end;
end;

class function TDemoPyramidTopple.Title: UTF8String;
begin
  Result := 'Pyramid Topple';
end;

procedure TDemoPyramidTopple.UpdateFrame(const Ticks: Integer);
const
  STEPS = 3;
  DT = 1 / FRAMES_PER_SECOND / STEPS;
var
  I: Integer;
begin
  inherited;
  for I := 0 to STEPS - 1 do
    Space.Step(DT);
end;

end.