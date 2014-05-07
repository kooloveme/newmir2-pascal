unit DemoLogoSmash;

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoLogoSmash = class(TDemoScene)
  private
    function MakeBall(const X, Y: TCPFloat): TCPShape;
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

const
  IMAGE_WIDTH      = 188;
  IMAGE_HEIGHT     = 35;
  IMAGE_ROW_LENGTH = 24;

const
  IMAGE_BITMAP: array [0..IMAGE_HEIGHT * IMAGE_ROW_LENGTH - 1] of ShortInt = (
    15,-16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,-64,15,63,-32,-2,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,31,-64,15,127,-125,-1,-128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,127,-64,15,127,15,-1,-64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,-1,-64,15,-2,
    31,-1,-64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,-1,-64,0,-4,63,-1,-32,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,1,-1,-64,15,-8,127,-1,-32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,-1,-64,0,-8,-15,-1,-32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,-31,-1,-64,15,-8,-32,
    -1,-32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,-15,-1,-64,9,-15,-32,-1,-32,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,31,-15,-1,-64,0,-15,-32,-1,-32,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,63,-7,-1,-64,9,-29,-32,127,-61,-16,63,15,-61,-1,-8,31,-16,15,-8,126,7,-31,
    -8,31,-65,-7,-1,-64,9,-29,-32,0,7,-8,127,-97,-25,-1,-2,63,-8,31,-4,-1,15,-13,
    -4,63,-1,-3,-1,-64,9,-29,-32,0,7,-8,127,-97,-25,-1,-2,63,-8,31,-4,-1,15,-13,
    -2,63,-1,-3,-1,-64,9,-29,-32,0,7,-8,127,-97,-25,-1,-1,63,-4,63,-4,-1,15,-13,
    -2,63,-33,-1,-1,-32,9,-25,-32,0,7,-8,127,-97,-25,-1,-1,63,-4,63,-4,-1,15,-13,
    -1,63,-33,-1,-1,-16,9,-25,-32,0,7,-8,127,-97,-25,-1,-1,63,-4,63,-4,-1,15,-13,
    -1,63,-49,-1,-1,-8,9,-57,-32,0,7,-8,127,-97,-25,-8,-1,63,-2,127,-4,-1,15,-13,
    -1,-65,-49,-1,-1,-4,9,-57,-32,0,7,-8,127,-97,-25,-8,-1,63,-2,127,-4,-1,15,-13,
    -1,-65,-57,-1,-1,-2,9,-57,-32,0,7,-8,127,-97,-25,-8,-1,63,-2,127,-4,-1,15,-13,
    -1,-1,-57,-1,-1,-1,9,-57,-32,0,7,-1,-1,-97,-25,-8,-1,63,-1,-1,-4,-1,15,-13,-1,
    -1,-61,-1,-1,-1,-119,-57,-32,0,7,-1,-1,-97,-25,-8,-1,63,-1,-1,-4,-1,15,-13,-1,
    -1,-61,-1,-1,-1,-55,-49,-32,0,7,-1,-1,-97,-25,-8,-1,63,-1,-1,-4,-1,15,-13,-1,
    -1,-63,-1,-1,-1,-23,-49,-32,127,-57,-1,-1,-97,-25,-1,-1,63,-1,-1,-4,-1,15,-13,
    -1,-1,-63,-1,-1,-1,-16,-49,-32,-1,-25,-1,-1,-97,-25,-1,-1,63,-33,-5,-4,-1,15,
    -13,-1,-1,-64,-1,-9,-1,-7,-49,-32,-1,-25,-8,127,-97,-25,-1,-1,63,-33,-5,-4,-1,
    15,-13,-1,-1,-64,-1,-13,-1,-32,-49,-32,-1,-25,-8,127,-97,-25,-1,-2,63,-49,-13,
    -4,-1,15,-13,-1,-1,-64,127,-7,-1,-119,-17,-15,-1,-25,-8,127,-97,-25,-1,-2,63,
    -49,-13,-4,-1,15,-13,-3,-1,-64,127,-8,-2,15,-17,-1,-1,-25,-8,127,-97,-25,-1,
    -8,63,-49,-13,-4,-1,15,-13,-3,-1,-64,63,-4,120,0,-17,-1,-1,-25,-8,127,-97,-25,
    -8,0,63,-57,-29,-4,-1,15,-13,-4,-1,-64,63,-4,0,15,-17,-1,-1,-25,-8,127,-97,
    -25,-8,0,63,-57,-29,-4,-1,-1,-13,-4,-1,-64,31,-2,0,0,103,-1,-1,-57,-8,127,-97,
    -25,-8,0,63,-57,-29,-4,-1,-1,-13,-4,127,-64,31,-2,0,15,103,-1,-1,-57,-8,127,
    -97,-25,-8,0,63,-61,-61,-4,127,-1,-29,-4,127,-64,15,-8,0,0,55,-1,-1,-121,-8,
    127,-97,-25,-8,0,63,-61,-61,-4,127,-1,-29,-4,63,-64,15,-32,0,0,23,-1,-2,3,-16,
    63,15,-61,-16,0,31,-127,-127,-8,31,-1,-127,-8,31,-128,7,-128,0,0);

function GetPixel(const X, Y: Integer): Boolean; inline;
var
  B: Byte;
begin
  B := Byte(IMAGE_BITMAP[(X shr 3) + (Y * IMAGE_ROW_LENGTH)]);
  B := B shr (not X and 7);
  Result := ((B and 1) <> 0);
end;

{ TDemoLogoSmash }

function TDemoLogoSmash.MakeBall(const X, Y: TCPFloat): TCPShape;
var
  Body: TCPBody;
begin
  Body := TCPBody.Create(1, INFINITY);
  Body.Position := CPV(X, Y);
  AutoFree(Body);

  Result := TCPCircleShape.Create(Body, 0.95, CPVZero);
  Result.Elasticity := 0;
  Result.Friction := 0;
  AutoFree(Result);
end;

procedure TDemoLogoSmash.Startup;
var
  Body: TCPBody;
  Shape: TCPShape;
  X, Y: Integer;
  XJitter, YJitter: TCPFloat;
begin
  inherited;
  DrawShapes := False;
  DrawOutlines := False;
  CollisionPointSize := 2;
  BodyPointSize := 3;

  Space.ResizeActiveHash(2, 10000);
  Space.ResizeStaticHash(2, 10000);
  Space.Iterations := 1;

  for Y := 0 to IMAGE_HEIGHT - 1 do
  begin
    for X := 0 to IMAGE_WIDTH - 1 do
    begin
      if GetPixel(X, Y) then
      begin
        XJitter := 0; //0.05 * Random();
        YJitter := 0; //0.05 * Random();
        Shape := MakeBall(2 * (X - IMAGE_WIDTH / 2 + XJitter),
                          2 * (IMAGE_HEIGHT / 2 - Y + YJitter));
        Space.AddBody(Shape.Body);
        Space.AddShape(Shape);
      end;
    end;
  end;

  Body := Space.AddBody(TCPBody.Create(INFINITY, INFINITY));
  Body.Position := CPV(-1000, -10);
  Body.Velocity := CPV(400, 0);
  AutoFree(Body);

  Shape := Space.AddShape(TCPCircleShape.Create(Body, 8, CPVZero));
  Shape.Elasticity := 0;
  Shape.Friction := 0;
  Shape.Layers := NOT_GRABABLE_MASK;
  AutoFree(Shape);
end;

class function TDemoLogoSmash.Title: UTF8String;
begin
  Result := 'Logo Smash';
end;

procedure TDemoLogoSmash.UpdateFrame(const Ticks: Integer);
begin
  Space.Step(1 / FRAMES_PER_SECOND);
end;

end.