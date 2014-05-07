unit DemoPlanet;

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoPlanet = class(TDemoScene)
  private
    FPlanetBody: TCPBody;
    procedure AddBox;
    procedure PlanetGravityUpdateVelocity(const Body: TCPBody;
      const Gravity: TCPVect; const Damping: TCPFloat; const DT: TCPFloat);
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

uses
  Math;

const
  GRAVITY_STRENGTH = 5.0e6;

function RandomPosition(const Radius: TCPFloat): TCPVect;
begin
  repeat
    Result := CPV(Random(640 - Round(2 * Radius)) - (320 - Radius),
                  Random(480 - Round(2 * Radius)) - (240 - Radius));
  until (Result.Length > 85);
end;

{ TDemoPlanet }

procedure TDemoPlanet.AddBox;
const
  SIZE = 10;
  MASS = 1;
  VERTS: array [0..3] of TCPVect = (
    (X: -SIZE; Y: -SIZE),
    (X: -SIZE; Y:  SIZE),
    (X:  SIZE; Y:  SIZE),
    (X:  SIZE; Y: -SIZE));
var
  Radius, R, V: TCPFloat;
  Body: TCPBody;
  Shape: TCPShape;
  P: TCPVect;
begin
  Radius := CPV(SIZE, SIZE).Length;

  Body := Space.AddBody(TCPBody.Create(MASS, TCPBody.MomentForPolygon(MASS, VERTS, CPVZero)));
  AutoFree(Body);
  Body.OnUpdateVelocity := PlanetGravityUpdateVelocity;
  Body.Position := RandomPosition(Radius);

  { Set the box's velocity to put it into a circular orbit from its
    starting position. }
  P := Body.Position;
  R := P.Length;
  V := Sqrt(GRAVITY_STRENGTH / R) / R;
  Body.Velocity := P.Perp * V;

  { Set the box's angular velocity to match its orbital period and
    align its initial angle with its position. }
  Body.RotationalVelocity := V * RAD2DEG;
  Body.Angle := ArcTan2(P.Y, P.X) * RAD2DEG;

  Shape := Space.AddShape(TCPPolyShape.Create(Body, VERTS, CPVZero));
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.7;
end;

procedure TDemoPlanet.PlanetGravityUpdateVelocity(const Body: TCPBody;
  const Gravity: TCPVect; const Damping, DT: TCPFloat);
var
  P, G: TCPVect;
  SqDist: TCPFloat;
begin
  { Gravitational acceleration is proportional to the inverse square of
    distance, and directed toward the origin. The central planet is assumed
    to be massive enough that it affects the satellites but not vice versa. }
  P := Body.Position;
  SqDist := P.LengthSq;
  G := P * (-GRAVITY_STRENGTH / (SqDist * Sqrt(SqDist)));
  Body.UpdateVelocity(G, Damping, DT);
end;

procedure TDemoPlanet.Startup;
var
  Shape: TCPShape;
  I: Integer;
begin
  inherited;
  UseBoxTexture := True;
  FPlanetBody := TCPBody.Create(INFINITY, INFINITY);
  AutoFree(FPlanetBody);
  FPlanetBody.RotationalVelocity := -11;

  TCPShape.ResetIdCounter;
  Space.ResizeActiveHash(30, 10000);
  Space.Iterations := 20;

  for I := 0 to 29 do
    AddBox;

  Shape := Space.AddShape(TCPCircleShape.Create(FPlanetBody, 70, CPVZero));
  AutoFree(Shape);
  Shape.Elasticity := 1;
  Shape.Friction := 1;
  Shape.Layers := NOT_GRABABLE_MASK;
end;

class function TDemoPlanet.Title: UTF8String;
begin
  Result := 'Planet';
end;

procedure TDemoPlanet.UpdateFrame(const Ticks: Integer);
begin
  inherited;
  Space.Step(1 / FRAMES_PER_SECOND);

  { Update the static body spin so that it looks like it's rotating. }
  FPlanetBody.UpdatePosition(1 / FRAMES_PER_SECOND);
end;

end.