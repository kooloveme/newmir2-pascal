unit DemoSpringies;

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoSpringies = class(TDemoScene)
  private
    function AddBar(const A, B: TCPVect; const Group: Integer): TCPBody;
    function NewSpring(const A, B: TCPBody; const Anchor1, Anchor2: TCPVect;
      const RestLength, Stiff, Damp: TCPFloat): TCPDampedSpring;
    function SpringForce(const Spring: TCPDampedSpring;
      const Dist: TCPFloat): TCPFloat;
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

uses
  Math;

{ TDemoSpringies }

function TDemoSpringies.AddBar(const A, B: TCPVect;
  const Group: Integer): TCPBody;
var
  Center: TCPVect;
  Length, Mass: TCPFloat;
  Shape: TCPShape;
begin
  Center := (A + B) * (1 / 2);
  Length := (B - A).Length;
  Mass := Length / 160;

  Result := Space.AddBody(TCPBody.Create(Mass, Mass * Length * Length / 12));
  AutoFree(Result);
  Result.Position := Center;

  Shape := Space.AddShape(TCPSegmentShape.Create(Result, A - Center, B - Center, 10));
  AutoFree(Shape);
  Shape.Group := Group;
end;

function TDemoSpringies.NewSpring(const A, B: TCPBody; const Anchor1,
  Anchor2: TCPVect; const RestLength, Stiff, Damp: TCPFloat): TCPDampedSpring;
begin
  Result := TCPDampedSpring.Create(A, B, Anchor1, Anchor2, RestLength, Stiff, Damp);
  AutoFree(Result);
  Result.OnForce := SpringForce;
end;

function TDemoSpringies.SpringForce(const Spring: TCPDampedSpring;
  const Dist: TCPFloat): TCPFloat;
const
  CLAMP = 20;
begin
  Result := EnsureRange(Spring.RestLength - Dist, -CLAMP, CLAMP) * Spring.Stiffness;
end;

procedure TDemoSpringies.Startup;
const
  STIFF = 100;
  DAMP  = 0.5;
var
  StaticBody, Body1, Body2, Body3, Body4, Body5, Body6, Body7, Body8: TCPBody;
  Body9, Body10, Body11, Body12, Body13, Body14: TCPBody;
begin
  inherited;
  StaticBody := Space.StaticBody;

  Body1  := AddBar(CPV(-240,  160), CPV(-160,   80), 1);
  Body2  := AddBar(CPV(-160,   80), CPV( -80,  160), 1);
  Body3  := AddBar(CPV(   0,  160), CPV(  80,    0), 0);
  Body4  := AddBar(CPV( 160,  160), CPV( 240,  160), 0);
  Body5  := AddBar(CPV(-240,    0), CPV(-160,  -80), 2);
  Body6  := AddBar(CPV(-160,  -80), CPV( -80,    0), 2);
  Body7  := AddBar(CPV( -80,    0), CPV(   0,    0), 2);
  Body8  := AddBar(CPV(   0,  -80), CPV(  80,  -80), 0);
  Body9  := AddBar(CPV( 240,   80), CPV( 160,    0), 3);
  Body10 := AddBar(CPV( 160,    0), CPV( 240,  -80), 3);
  Body11 := AddBar(CPV(-240,  -80), CPV(-160, -160), 4);
  Body12 := AddBar(CPV(-160, -160), CPV( -80, -160), 0);
  Body13 := AddBar(CPV(   0, -160), CPV(  80, -160), 0);
  Body14 := AddBar(CPV( 160, -160), CPV( 240, -160), 0);

  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body1,  Body2,  CPV( 40, -40), CPV(-40, -40))));
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body5,  Body6,  CPV( 40, -40), CPV(-40, -40))));
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body6,  Body7,  CPV( 40,  40), CPV(-40,   0))));
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body9,  Body10, CPV(-40, -40), CPV(-40,  40))));
  AutoFree(Space.AddConstraint(TCPPivotJoint.Create(Body11, Body12, CPV( 40, -40), CPV(-40,   0))));

  Space.AddConstraint(NewSpring(StaticBody,  Body1, CPV(-320,  240), CPV(-40, 40), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring(StaticBody,  Body1, CPV(-320,   80), CPV(-40, 40), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring(StaticBody,  Body1, CPV(-160,  240), CPV(-40, 40), 0.0, STIFF, DAMP));

  Space.AddConstraint(NewSpring(StaticBody,  Body2, CPV(-160,  240), CPV( 40, 40), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring(StaticBody,  Body2, CPV(   0,  240), CPV( 40, 40), 0.0, STIFF, DAMP));

  Space.AddConstraint(NewSpring(StaticBody,  Body3, CPV(  80,  240), CPV(-40, 80), 0.0, STIFF, DAMP));

  Space.AddConstraint(NewSpring(StaticBody,  Body4, CPV(  80,  240), CPV(-40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring(StaticBody,  Body4, CPV( 320,  240), CPV( 40,  0), 0.0, STIFF, DAMP));

  Space.AddConstraint(NewSpring(StaticBody,  Body5, CPV(-320,   80), CPV(-40, 40), 0.0, STIFF, DAMP));

  Space.AddConstraint(NewSpring(StaticBody,  Body9, CPV( 320,  80), CPV( 40, 40), 0.0, STIFF, DAMP));

  Space.AddConstraint(NewSpring(StaticBody, Body10, CPV( 320,   0), CPV( 40,-40), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring(StaticBody, Body10, CPV( 320,-160), CPV( 40,-40), 0.0, STIFF, DAMP));

  Space.AddConstraint(NewSpring(StaticBody, Body11, CPV(-320,-160), CPV(-40, 40), 0.0, STIFF, DAMP));

  Space.AddConstraint(NewSpring(StaticBody, Body12, CPV(-240,-240), CPV(-40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring(StaticBody, Body12, CPV(   0,-240), CPV( 40,  0), 0.0, STIFF, DAMP));

  Space.AddConstraint(NewSpring(StaticBody, Body13, CPV(   0,-240), CPV(-40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring(StaticBody, Body13, CPV(  80,-240), CPV( 40,  0), 0.0, STIFF, DAMP));

  Space.AddConstraint(NewSpring(StaticBody, Body14, CPV(  80,-240), CPV(-40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring(StaticBody, Body14, CPV( 240,-240), CPV( 40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring(StaticBody, Body14, CPV( 320,-160), CPV( 40,  0), 0.0, STIFF, DAMP));

  Space.AddConstraint(NewSpring( Body1,  Body5, CPV( 40,-40), CPV(-40, 40), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body1,  body6, CPV( 40,-40), CPV( 40, 40), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body2,  Body3, CPV( 40, 40), CPV(-40, 80), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body3,  Body4, CPV(-40, 80), CPV(-40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body3,  Body4, CPV( 40,-80), CPV(-40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body3,  Body7, CPV( 40,-80), CPV( 40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body3,  Body7, CPV(-40, 80), CPV(-40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body3,  Body8, CPV( 40,-80), CPV( 40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body3,  Body9, CPV( 40,-80), CPV(-40,-40), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body4,  Body9, CPV( 40,  0), CPV( 40, 40), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body5, Body11, CPV(-40, 40), CPV(-40, 40), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body5, Body11, CPV( 40,-40), CPV( 40,-40), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body7,  Body8, CPV( 40,  0), CPV(-40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body8, Body12, CPV(-40,  0), CPV( 40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body8, Body13, CPV(-40,  0), CPV(-40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body8, Body13, CPV( 40,  0), CPV( 40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring( Body8, Body14, CPV( 40,  0), CPV(-40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring(Body10, Body14, CPV( 40,-40), CPV(-40,  0), 0.0, STIFF, DAMP));
  Space.AddConstraint(NewSpring(Body10, Body14, CPV( 40,-40), CPV(-40,  0), 0.0, STIFF, DAMP));
end;

class function TDemoSpringies.Title: UTF8String;
begin
  Result := 'Springies';
end;

procedure TDemoSpringies.UpdateFrame(const Ticks: Integer);
begin
  inherited;
  Space.Step(1 / FRAMES_PER_SECOND);
end;

end.
