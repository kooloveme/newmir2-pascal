unit DemoMagnetsElectric;
{$HINTS OFF}

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

const
  WIDTH   = 600;
  HEIGHT  = 400;
  SINGMAX = 10; // Maximum number of singularities per body
  NMAG    = 10; // Number of magnets
  NCHG    = 10; // Number of charged bodies
  NMIX    = 10; // Number of charged magnets
  COU_MKS = 8.987551787e9; // Some physical constants
  MAG_MKS = 1e-7;

type
  PForceData = ^TForceData;
  TSingForceFunc = procedure (const Data: PForceData);

  { Singularities }
  TSing = record
    // Number of singularities
    NSing: Integer;
    // Value of the singularities
    Value: array [0..SINGMAX - 1] of TCPFloat;
    // Type of the singularities
    Typ: array [0..SINGMAX-1] of (stMagdipole, stElectrical);
    // Global position of the singularities
    GPos: array [0..SINGMAX-1] of TCPVect;
    // Local position of the singularities
    Position: array [0..SINGMAX-1] of TCPVect;
    // Angle of the singularities measured in the body axes
    Angle: array [0..SINGMAX-1] of TCPFloat;
    // Angle of the singularities measured from x
    GAngle: array [0..SINGMAX-1] of TCPFloat;
    // Force function
    ForceFunc: array [0..SINGMAX-1] of TSingForceFunc;
    // Force function
    TorqueFunc: array [0..SINGMAX-1] of TSingForceFunc;
  end;
  PSing = ^TSing;

  { Data for the force functions }
  TForceData = record
    // Everything in global coordinates
    // Position of the source
    P0: TCPVect;
    // Observed position
    P: TCPVect;
    // Relative position source-observed
    RelP: TCPVect;
    // distance, disntace^2, ditance ^3
    R: array [0..2] of TCPFloat;
    // angle of the source
    Ang0: TCPFloat;
    // angle of the observed singularity
    Ang: TCPFloat;
    // Foce value
    F: TCPVect;
    // Torque value
    T: TCPFloat;
  end;

type
  TDemoMagnetsElectric = class(TDemoScene)
  private
    procedure MakeMag(const P: TCPVect; const Ang, Mag: TCPFloat);
    procedure MakeCharged(const P: TCPVect; const Chg: TCPFloat);
    procedure MakeMix(const P: TCPVect; const Ang, Mag, Chg: TCPFloat);
    procedure ChargedBodyUpdatePositionVerlet(const Body: TCPBody;
      const DT: TCPFloat);
    procedure ChargedBodyUpdateVelocityVerlet(const Body: TCPBody;
      const Gravity: TCPVect; const Damping, DT: TCPFloat);
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure Shutdown; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

uses
  Math;

// **** Forces ****** //
// Calculate the forces between two bodies. all this functions requieres
// a pointer to an structure with the necessary fields.

// forces between charges
procedure CoulombForce(const Data: PForceData);
begin
  Data.F := Data.RelP.Normalize * (COU_MKS / Data.R[1]);
end;

// forces between magnets
procedure MagDipoleForce(const Data: PForceData);
var
  Phi, Alpha, Beta, Fr, FPhi: TCPFloat;
begin
  // Angle of the relative position vector
  Phi := Data.RelP.ToAngle;
  Alpha := Data.Ang0;
  Beta := Data.Ang;

  Alpha := (Phi - Alpha) * DEG2RAD;
  Beta := (Phi - Beta) * DEG2RAD;
  Phi := Phi * DEG2RAD;

  // Components in polar coordinates
  Fr := (2.0e0 * Cos(Alpha) * Cos(Beta) - Sin(Alpha) * Sin(Beta));
  FPhi := Sin(Alpha + Beta);

  // Cartesian coordinates
  Data.F := CPV(Fr * Cos(Phi) - FPhi * Sin(Phi),
                Fr * Sin(Phi) + FPhi * Cos(Phi));
  Data.F := Data.F * (-3.e0 * MAG_MKS / (Data.R[1] * Data.R[1]));
end;

procedure MagDipoleTorque(const Data: PForceData);
var
  Phi, Alpha, Beta: TCPFloat;
begin
  Phi := Data.RelP.ToAngle;
  Alpha := Data.Ang0;
  Beta := Data.Ang;

  Alpha := (Phi - Alpha) * DEG2RAD;
  Beta := (Phi - Beta) * DEG2RAD;

  // Torque. Though we could use a component of F to save some space,
  // we use another variables for the sake of clarity.
  Data.T := (MAG_MKS / Data.R[2]) * (3.0e0 * Cos(Alpha) * Sin(Beta) + Sin(Alpha - Beta));
end;

// This function fills the data structure for the force functions
// The structure Sing has the information about the singularity (charge or magnet)
procedure FillForceData(const Source: TSing; const IndS: Integer;
  const Obs: TSing; const IndO: Integer; var Data: TForceData);
begin
  // Global Position and orientation of the source singularity
  Data.P0 := Source.GPos[IndS];
  Data.Ang0 := Source.GAngle[IndS];

  // Global Position and orientation of the observed singularity
  Data.P := Obs.GPos[IndO];
  Data.Ang := Obs.GAngle[IndO];

  // Derived magnitudes
  Data.RelP := Data.P - Data.P0; // Relative position
  Data.R[0] := Data.RelP.Length; // Distance
  Data.R[1] := Data.RelP.LengthSq; // Square Distance
  Data.R[2] := Data.R[0] * Data.R[1]; // Cubic distance

  Source.ForceFunc[IndS](@Data); // The value of the force
  Data.F := Data.F * (Source.Value[IndS] * Obs.Value[IndO]);
end;

// Calculation of the interaction
procedure LRangeForceApply(const A, B: TCPBody);
var
  Aux, Aux2: PSing;
  Delta: TCPVect;
  FData: TForceData;
  I, J: Integer;
begin
  Aux := A.UserData;
  Aux2 := B.UserData;

  // General data needed to calculate interaction
  FData.F := CPVZero;
  // Calculate the forces between the charges of different bodies
  for I := 0 to Aux.NSing - 1 do
  begin
    for J := 0 to Aux2.NSing - 1 do
    begin
      if (Aux.Typ[I] = Aux2.Typ[J]) then
      begin
        FillForceData(Aux2^, J, Aux^, I, FData);

        // Force applied to body A
        Delta := Aux.GPos[I] - A.Position;
        A.ApplyForce(FData.F, Delta);

        if Assigned(Aux.TorqueFunc[I]) then
        begin
          // Torque on A
          Aux.TorqueFunc[I](@FData);
          A.Torque := A.Torque + (Aux.Value[I] * Aux2.Value[J] * FData.T);
        end;
      end;
    end;
  end;
end;

{ TDemoMagnetsElectric }

procedure TDemoMagnetsElectric.ChargedBodyUpdatePositionVerlet(
  const Body: TCPBody; const DT: TCPFloat);
// function for the integration of the positions
// The following functions are variations to the starndrd integration in Chipmunk
// you can go ack to the standard ones by doing the appropiate changes.
var
  B: TCPBody;
  Aux: PSing;
  I: Integer;
  DP: TCPVect;
begin
  Aux := Body.UserData;

  for I := 0 to Space.BodyCount - 1 do
  begin
    B := Space.Bodies[I];
    if (B <> Body) then
    begin
      // Calculate the forces between the singularities of different bodies
      LRangeForceApply(Body, B);
    end;
  end;

  DP := (Body.Velocity + Body.VelocityBias) * DT;
  DP := DP + ((Body.Force * Body.MassInv) * 0.5e0 * DT * DT);
  Body.Position := Body.Position + DP;

  Body.Angle := Body.Angle + (Body.RotationalVelocity + Body.RotationalVelocityBias) * DT
    + 0.5 * Body.Torque * Body.MomentInv * DT * DT;

  // Update position of the singularities
  for I := 0 to Aux.NSing - 1 do
  begin
    Aux.GPos[I] := Body.Position + Aux.Position[I].Rotate(Body.Rotation);
    Aux.GAngle[I] := Aux.Angle[I] + Body.Angle;
  end;

  Body.VelocityBias := CPVZero;
  Body.RotationalVelocityBias := 0;
end;

procedure TDemoMagnetsElectric.ChargedBodyUpdateVelocityVerlet(
  const Body: TCPBody; const Gravity: TCPVect; const Damping, DT: TCPFloat);
var
  B: TCPBody;
  I: Integer;
begin
  Body.Velocity := Body.Velocity
    + ((Gravity + (Body.Force * Body.MassInv)) * 0.5e0 * DT);
  Body.RotationalVelocity := Body.RotationalVelocity
    + Body.Torque * Body.MomentInv * 0.5e0 * DT;

  Body.Force := CPVZero;
  Body.Torque := 0.0e0;

  for I := 0 to Space.BodyCount - 1 do
  begin
    B := Space.Bodies[I];
    if (B <> Body) then
      // Calculate the forces between the singularities of different bodies
      LRangeForceApply(Body, B);
  end;

  Body.Velocity := (Body.Velocity * Damping)
    + ((Gravity + (Body.Force * Body.MassInv)) * 0.5e0 * DT);
  Body.RotationalVelocity := Body.RotationalVelocity * Damping
    + Body.Torque * Body.MomentInv * 0.5e0 * DT;
end;

procedure TDemoMagnetsElectric.MakeCharged(const P: TCPVect;
  const Chg: TCPFloat);
const
  NVERTS = 4;
  VERTS: array [0..NVERTS - 1] of TCPVect = (
    (X: -10; Y: -10),
    (X: -10; Y:  10),
    (X:  10; Y:  10),
    (X:  10; Y: -10));
var
  Body: TCPBody;
  Charge: PSing;
  Shape: TCPShape;
begin
  Body := TCPBody.Create(1, TCPBody.MomentForPolygon(1, VERTS, CPVZero));
  AutoFree(Body);
  Body.Position := P;
  Body.Velocity := CPVZero;
  Body.Angle := 0;
  Body.RotationalVelocity := 0.0e0;

  // Load the singularities
  GetMem(Charge, SizeOf(TSing));
  FillChar(Charge^, SizeOf(TSing), 0);
  Body.UserData := Charge;
  Charge.NSing := 1;
  Charge.Value[0] := Chg;
  Charge.Typ[0] := stElectrical;

  // The position and angle could be different form the one of the body
  Charge.Position[0] := CPVZero;
  Charge.GPos[0] := P + Charge.Position[0];
  Charge.GAngle[0] := 0;

  Charge.ForceFunc[0] := CoulombForce;
  Charge.TorqueFunc[0] := nil;

  Body.OnUpdatePosition := ChargedBodyUpdatePositionVerlet;
  Body.OnUpdateVelocity := ChargedBodyUpdateVelocityVerlet;
  Space.AddBody(Body);

  Shape := TCPPolyShape.Create(Body, VERTS, CPVZero);
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.7;
  Space.AddShape(Shape);
end;

procedure TDemoMagnetsElectric.MakeMag(const P: TCPVect; const Ang,
  Mag: TCPFloat);
const
  NVERTS = 6;
  VERTS: array [0..NVERTS - 1] of TCPVect = (
    (X: -10; Y: -10),
    (X: -10; Y:  10),
    (X:  10; Y:  10),
    (X:  15; Y:  5),
    (X:  15; Y: -5),
    (X:  10; Y: -10));
var
  Body: TCPBody;
  Magnet: PSing;
  Shape: TCPShape;
begin
  Body := TCPBody.Create(1, TCPBody.MomentForPolygon(1, VERTS, CPVZero));
  AutoFree(Body);
  Body.Position := P;
  Body.Velocity := CPVZero;
  Body.Angle := Ang;
  Body.RotationalVelocity := 0.0e0;

  // Load the singularities
  GetMem(Magnet, SizeOf(TSing));
  FillChar(Magnet^, SizeOf(TSing), 0);
  Body.UserData := Magnet;
  Magnet.NSing := 1;
  Magnet.Value[0] := Mag;
  Magnet.Typ[0] := stMagdipole;

  // The position and angle could be different form the one of the body
  Magnet.Position[0] := CPVZero;
  Magnet.GPos[0] := P + Magnet.Position[0];
  Magnet.Angle[0] := 0;
  Magnet.GAngle[0] := Ang;

  Magnet.ForceFunc[0] := MagDipoleForce;
  Magnet.TorqueFunc[0] := MagDipoleTorque;

  Body.OnUpdatePosition := ChargedBodyUpdatePositionVerlet;
  Body.OnUpdateVelocity := ChargedBodyUpdateVelocityVerlet;
  Space.AddBody(Body);

  Shape := TCPPolyShape.Create(Body, VERTS, CPVZero);
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.7;
  Space.AddShape(Shape);
end;

procedure TDemoMagnetsElectric.MakeMix(const P: TCPVect; const Ang, Mag,
  Chg: TCPFloat);
const
  NVERTS = 5;
  VERTS: array [0..NVERTS - 1] of TCPVect = (
    (X: -10; Y: -10),
    (X: -10; Y:  10),
    (X:  10; Y:  10),
    (X:  20; Y:  0),
    (X:  10; Y: -10));
var
  Body: TCPBody;
  Mix: PSing;
  Shape: TCPShape;
begin
  Body := TCPBody.Create(1, TCPBody.MomentForPolygon(1, VERTS, CPVZero));
  AutoFree(Body);
  Body.Position := P;
  Body.Velocity := CPVZero;
  Body.Angle := Ang;
  Body.RotationalVelocity := 0.0e0;

  // Load the singularities
  GetMem(Mix, SizeOf(TSing));
  FillChar(Mix^, SizeOf(TSing), 0);
  Body.UserData := Mix;
  Mix.NSing := 2;
  Mix.Value[0] := Mag;
  Mix.Value[1] := Chg;
  Mix.Typ[0] := stMagdipole;
  Mix.Typ[1] := stElectrical;

  // The position and angle could be different form the one of the body
  Mix.Position[0] := CPVZero;
  Mix.GPos[0] := P + Mix.Position[0];
  Mix.Position[1] := CPVZero;
  Mix.GPos[1] := P + Mix.Position[1];
  Mix.GAngle[0] := Ang;
  Mix.GAngle[1] := Ang;

  Mix.ForceFunc[0] := MagDipoleForce;
  Mix.ForceFunc[1] := CoulombForce;
  Mix.TorqueFunc[0] := MagDipoleTorque;
  Mix.TorqueFunc[1] := nil;

  Body.OnUpdatePosition := ChargedBodyUpdatePositionVerlet;
  Body.OnUpdateVelocity := ChargedBodyUpdateVelocityVerlet;
  Space.AddBody(Body);

  Shape := TCPPolyShape.Create(Body, VERTS, CPVZero);
  AutoFree(Shape);
  Shape.Elasticity := 0;
  Shape.Friction := 0.7;
  Space.AddShape(Shape);
end;

procedure TDemoMagnetsElectric.Shutdown;
var
  I: Integer;
begin
  for I := 0 to Space.BodyCount - 1 do
    FreeMem(Space.Bodies[I].UserData);
  inherited;
end;

procedure TDemoMagnetsElectric.Startup;
var
  Ang: TCPFloat;
  I: Integer;
  P: TCPVect;
begin
  inherited;
  TCPShape.ResetIdCounter;
  Space.Iterations := 5;
  Space.Gravity := CPVZero;

  Space.ResizeActiveHash(30, 2999);
  Randomize;

  // Create magnets
  for I := 0 to NMAG - 1 do
  begin
    P.X := ((2.0e0 * Random) - 1.0e0) * WIDTH / 2.0;
    P.Y := ((2.0e0 * Random) - 1.0e0) * HEIGHT / 2.0;
    Ang := ((2.0e0 * Random) - 1.0e0) * 180;
    MakeMag(P, Ang, 1.0e7);
  end;

  // Create charged objects
  for I := 0 to NCHG - 1 do
  begin
    P.X := ((2.0e0 * Random) - 1.0e0) * WIDTH / 2.0;
    P.Y := ((2.0e0 * Random) - 1.0e0) * HEIGHT / 2.0;
    MakeCharged(P, 1.0e-3 * Power(-1.0, I and 1));
  end;

  // Create charged magnets objects
  for I := 0 to NMIX - 1 do
  begin
    P.X := ((2.0e0 * Random) - 1.0e0) * WIDTH / 2.0;
    P.Y := ((2.0e0 * Random) - 1.0e0) * HEIGHT / 2.0;
    Ang := ((2.0e0 * Random) - 1.0e0) * 180;
    MakeMix(P, Ang, 1.0e7 * Power(-1.0, I and 1), 1.0e-3 * Power(-1.0, I and 1));
  end;
end;

class function TDemoMagnetsElectric.Title: UTF8String;
begin
  Result := 'Magnets and Electric Charges (By: Juan Pablo Carbajal)';
end;

procedure TDemoMagnetsElectric.UpdateFrame(const Ticks: Integer);
const
  STEPS = 10;
  DT = 1 / FRAMES_PER_SECOND / STEPS;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Space.BodyCount - 1 do
    Space.Bodies[I].ResetForces;

  for I := 0 to STEPS - 1 do
    Space.Step(DT);
end;

end.
