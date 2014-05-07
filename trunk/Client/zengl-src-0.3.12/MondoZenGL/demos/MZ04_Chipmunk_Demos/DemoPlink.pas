unit DemoPlink;

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoPlink = class(TDemoScene)
  private
    procedure EachBody(const Space: TCPSpace; const Body: TCPBody;
      const Data: Pointer);
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

{ TDemoPlink }

procedure TDemoPlink.EachBody(const Space: TCPSpace; const Body: TCPBody;
  const Data: Pointer);
var
  X: TCPFloat;
begin
  { Iterate over all of the bodies and reset the ones that have fallen
    offscreen. }
  if (Body.Position.Y < -260) or (Abs(Body.Position.X) > 340) then
  begin
    X := Random(640) - 320;
    Body.Position := CPV(X, 260);
  end;
end;

procedure TDemoPlink.Startup;
const
  NUM_VERTS = 5;
  { Vertexes for a triangle shape. }
  TRIS: array [0..2] of TCPVect = (
    (X: -15; Y: -15),
    (X:   0; Y:  10),
    (X:  15; Y: -15));
var
  Body, StaticBody: TCPBody;
  Shape: TCPShape;
  Verts: array [0..NUM_VERTS - 1] of TCPVect;
  I, J: Integer;
  Angle, Stagger, X: TCPFloat;
  Offset: TCPVect;
begin
  inherited;
  UseTriangleTexture := True;
  UsePentagonTexture := True;
  TCPShape.ResetIdCounter;
  Space.Iterations := 5;
  Space.Gravity := CPV(0, -100);
  Space.ResizeStaticHash(40, 999);
  Space.ResizeActiveHash(30, 2999);

  StaticBody := Space.StaticBody;

  { Create vertexes for a pentagon shape. }
  for I := 0 to NUM_VERTS - 1 do
  begin
    Angle := (-2 * Pi * I) / NUM_VERTS;
    Verts[I] := CPV(10 * Cos(Angle), 10 * Sin(Angle));
  end;

  { Create the static triangles. }
  for I := 0 to 9 do
  begin
    for J := 0 to 5 do
    begin
      Stagger := (J and 1) * 40;
      Offset := CPV(I * 80 + - 320 + Stagger, J * 70 - 240);
      Shape := Space.AddShape(TCPPolyShape.Create(StaticBody, TRIS, Offset));
      AutoFree(Shape);
      Shape.Elasticity := 1;
      Shape.Friction := 1;
      Shape.Layers := NOT_GRABABLE_MASK;
    end;
  end;

  { Add lots of pentagons. }
  for I := 0 to 299 do
  begin
    Body := Space.AddBody(TCPBody.Create(1, TCPBody.MomentForPolygon(1, Verts, CPVZero)));
    AutoFree(Body);
    X := Random(640) - 320;
    Body.Position := CPV(X, 350);

    Shape := Space.AddShape(TCPPolyShape.Create(Body, Verts, CPVZero));
    AutoFree(Shape);
    Shape.Elasticity := 0;
    Shape.Friction := 0.4;
  end;
end;

class function TDemoPlink.Title: UTF8String;
begin
  Result := 'Plink';
end;

procedure TDemoPlink.UpdateFrame(const Ticks: Integer);
begin
  inherited;
  Space.Step(1 / FRAMES_PER_SECOND);
  Space.ForEachBody(EachBody, nil);
end;

end.