unit DemoQuery;

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoQuery = class(TDemoScene)
  private
    FQuerySeg: TCPSegmentShape;
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

uses
  SysUtils,
  MondoZenGL;

{ TDemoQuery }

procedure TDemoQuery.Startup;
const
  MASS      = 1;
  LENGTH    = 100;
  NUM_VERTS = 5;
  R         = 20;
var
  StaticBody, Body: TCPBody;
  Shape: TCPShape;
  A, B: TCPVect;
  Verts: array [0..NUM_VERTS - 1] of TCPVect;
  I: Integer;
  Angle: TCPFloat;
begin
  inherited;
  TCPShape.ResetIdCounter;
  Space.ElasticIterations := 0;
  Space.Iterations := 5;
  Space.ResizeStaticHash(40, 999);
  Space.ResizeActiveHash(30, 2999);
  StaticBody := Space.StaticBody;

  // add a non-collidable segment as a quick and dirty way to draw the query line
  Shape := Space.AddShape(TCPSegmentShape.Create(StaticBody, CPVZero,
    CPV(100, 0), 4));
  AutoFree(Shape);
  Shape.Layers := 0;
  FQuerySeg := Shape as TCPSegmentShape;

  // add a fat segment
  A := CPV(-LENGTH / 2, 0);
  B := CPV(LENGTH / 2, 0);
  Body := Space.AddBody(TCPBody.Create(MASS, TCPBody.MomentForSegment(MASS, A, B)));
  AutoFree(Body);
  Body.Position := CPV(0, 100);
  AutoFree(Space.AddShape(TCPSegmentShape.Create(Body, A, B, 20)));

  // add a static segment
  AutoFree(Space.AddShape(TCPSegmentShape.Create(StaticBody, CPV(0, 300),
    CPV(300, 0), 0)));

  // add a pentagon
  for I := 0 to NUM_VERTS - 1 do
  begin
    Angle := -2 * Pi * I / NUM_VERTS;
    Verts[I] := CPV(30 * Cos(Angle), 30 * Sin(Angle));
  end;
  Body := Space.AddBody(TCPBody.Create(MASS, TCPBody.MomentForPolygon(MASS, Verts, CPVZero)));
  AutoFree(Body);
  Body.Position := CPV(50, 50);
  AutoFree(Space.AddShape(TCPPolyShape.Create(Body, Verts, CPVZero)));

  // add a circle
  Body := Space.AddBody(TCPBody.Create(MASS, TCPBody.MomentForCircle(MASS, 0, R, CPVZero)));
  AutoFree(Body);
  Body.Position := CPV(100, 100);
  AutoFree(Space.AddShape(TCPCircleShape.Create(Body, R, CPVZero)));
end;

class function TDemoQuery.Title: UTF8String;
begin
  Result := 'Segment Query';
end;

procedure TDemoQuery.UpdateFrame(const Ticks: Integer);
const
  DT = 1 / FRAMES_PER_SECOND;
var
  Start, Stop, LineEnd, Point: TCPVect;
  Info: TCPSegmentQueryInfo;
begin
  Start := CPVZero;
  Stop := MousePoint;
  LineEnd := Stop;

  Message := TMZUtils.Format('Query: Dist(%f), Points%s, ',
    [Start.Distance(Stop), Stop.ToString]);

  if (Space.SegmentQuery(Start, Stop, CP_ALL_LAYERS, CP_NO_GROUP, Info) <> nil) then
  begin
    Point := Info.HitPoint(Start, Stop);
    LineEnd := Point + CPVZero;
    Message := Message + TMZUtils.Format('Segment Query: Dist(%f) Normal%s',
      [Info.HitDist(Start, Stop), Info.Normal.ToString]);
  end
  else
    Message := Message + 'Segment Query (None)';

  FQuerySeg.UnsafeSetEndPoints(Start, LineEnd);
  FQuerySeg.CacheBoundingBox; // force it to update it's collision detection data so it will draw

  Space.Step(DT);
end;

end.