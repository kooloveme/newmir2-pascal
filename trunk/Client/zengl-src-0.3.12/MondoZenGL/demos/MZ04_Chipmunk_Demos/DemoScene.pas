unit DemoScene;

interface

{$INCLUDE '..\..\src\mz_config.cfg'}

{$IFNDEF USE_ZENGL_STATIC}
  {$MESSAGE Error 'This demo only works with static linking'}
{$ENDIF}

uses
  Classes,
  Contnrs,
  {$IFNDEF FPC}
  {$IFDEF USE_ZENGL_STATIC}
  zgl_mouse,
  zgl_text,
  zgl_font,
  zgl_primitives_2d,
  zgl_collision_2d,
  zgl_fx,
  zgl_math_2d,
  zgl_textures,
  zgl_sprite_2d,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  {$ENDIF}
  zglChipmunk,
  zgl_render_2d,
  MondoZenGL,
  mzChipmunk;

const
  SCREEN_WIDTH  = 1024;
  SCREEN_HEIGHT = 768;
  X_OFF         = SCREEN_WIDTH div 2;
  Y_OFF         = SCREEN_HEIGHT div 2;

const
  GRABABLE_MASK_BIT = Cardinal(1 shl 31);
  NOT_GRABABLE_MASK = not GRABABLE_MASK_BIT;

const
  { We try to maintain a fixed framerate of 60 fps }
  FRAMES_PER_SECOND = 60;
  SECONDS_PER_FRAME = 1 / FRAMES_PER_SECOND;
  MSEC_PER_FRAME = 1000 / FRAMES_PER_SECOND;

type
  TSimpleButton = class
  private
    FCanvas: TMZCanvas;
    FFont: TMZFont;
    FCaption: UTF8String;
    FBounds: TMZRect;
    FChecked: Boolean;
  public
    constructor Create(const Canvas: TMZCanvas; const Font: TMZFont;
      const Caption: UTF8String; const X, Y: Single);
    procedure Render;
    function ContainsPoint(const P: TMZPoint): Boolean;

    property Bounds: TMZRect read FBounds;
    property Checked: Boolean read FChecked write FChecked;
  end;

type
  { Base class for Chipmunk demos }
  TDemoScene = class abstract(TMZScene)
  private
    FLastTime: Double;
    FSpace: TCPSpace;
    FObjects: TObjectList;
    FTicks: Integer;
    FFont: TMZFont;
    FTextures: TMZTexture;
    FMouseBody: TCPBody;
    FMouseJoint: TCPConstraint;
    FMousePoint: TCPVect;
    FMousePointLast: TCPVect;
    FArrowDirection: TCPVect;
    FDrawShapes: Boolean;
    FDrawOutlines: Boolean;
    FCollisionPointSize: Single;
    FBodyPointSize: Single;
    FMaxArbiters: Integer;
    FMaxPoints: Integer;
    FMaxConstraints: Integer;
    FUseBallTexture: Boolean;
    FUseBoxTexture: Boolean;
    FUseTriangleTexture: Boolean;
    FUsePentagonTexture: Boolean;
    FShowArrowButtons: Boolean;
    FMessage: UTF8String;
    FButtonNext: TSimpleButton;
    FButtonRestart: TSimpleButton;
    FButtonPrev: TSimpleButton;
    {$IFNDEF iOS}
    FButtonAntiAlias: TSimpleButton;
    {$ENDIF}
    FButtonSpatialHash: TSimpleButton;
    FButtonBoundingBox: TSimpleButton;
    FButtonTextures: TSimpleButton;
    FButtonLeft: TSimpleButton;
    FButtonRight: TSimpleButton;
    function MouseToSpace: TCPVect;
    procedure DrawSpace;
    procedure DrawInstructions;
    procedure DrawInfo;
    procedure DrawSpatialHash(const Hash: PcpSpaceHash);
    procedure DrawObject(const Space: TCPSpace;
      const Shape: TCPShape; const Data: Pointer);
    procedure DrawBoundingBox(const Space: TCPSpace;
      const Shape: TCPShape; const Data: Pointer);
    procedure DrawCircleShape(const Body: TCPBody; const Shape: TCPCircleShape);
    procedure DrawSegmentShape(const Body: TCPBody; const Shape: TCPSegmentShape);
    procedure DrawPolyShape(const Body: TCPBody; const Shape: TCPPolyShape);
    procedure DrawConstraint(const Constraint: TCPConstraint);
    procedure DrawPivotJoint(const Joint: TCPPivotJoint; const BodyA, BodyB: TCPBody);
    procedure DrawPinJoint(const Joint: TCPPinJoint; const BodyA, BodyB: TCPBody);
    procedure DrawSlideJoint(const Joint: TCPSlideJoint; const BodyA, BodyB: TCPBody);
    procedure DrawGrooveJoint(const Joint: TCPGrooveJoint; const BodyA, BodyB: TCPBody);
    procedure DrawSpring(const Spring: TCPDampedSpring; const BodyA, BodyB: TCPBody);
    function ColorForShape(const Shape: TCPShape): Cardinal;
    class function ColorFromPointer(const Ptr: Pointer): Cardinal; static;
    function GetDrawBoundingBoxes: Boolean;
    function GetDrawHash: Boolean;
    procedure SetDrawBoundingBoxes(const Value: Boolean);
    procedure SetDrawHash(const Value: Boolean);
  protected
    class function Title: UTF8String; virtual; abstract;
    procedure UpdateFrame(const Ticks: Integer); virtual; abstract;
  protected
    { Summary:
        Is called before the scene is executed. You can override this method
        to initialize scene specific resources. }
    procedure Startup; override;

    { Summary:
        Is called just before the scene is terminated. You can override this
        method to cleanup scene specific resources. }
    procedure Shutdown; override;

    { Summary:
        Is called during each iteration of the main loop to render the current
        frame. }
    procedure RenderFrame; override;

    { Summary:
        Is called during each iteration of the main loop to update the game
        state. The DeltaTimeMs is the number of milliseconds (1/1000th of a
        second) that has passed since the last call to Update.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update. }
    procedure Update(const DeltaTimeMs: Double); override;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a keyboard key is pressed.
        Does nothing by default.
      Parameters:
        KeyCode: the key that is pressed. }
    procedure KeyDown(const KeyCode: TMZKeyCode); override;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a keyboard key is released.
        Does nothing by default.
      Parameters:
        KeyCode: the key that is released. }
    procedure KeyUp(const KeyCode: TMZKeyCode); override;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a mouse button is pressed.
        Does nothing by default.
      Parameters:
        Button: The mouse button that is pressed. }
    procedure MouseDown(const Button: TMZMouseButton); override;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a mouse button is released.
        Does nothing by default.
      Parameters:
        Button: The mouse button that is release. }
    procedure MouseUp(const Button: TMZMouseButton); override;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a finger is pressed onto the touch screen.
        Does nothing by default.
      Parameters:
        Finger: The finger ID.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure TouchDown(const Finger: Integer); override;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a finger is released from the touch screen.
        Does nothing by default.
      Parameters:
        Finger: The finger ID.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure TouchUp(const Finger: Integer); override;

    procedure AutoFree(const Obj: TObject);
    procedure RemoveAutoFree(const Obj: TObject);
    procedure MouseOrTouchDown(const Position: TMZPoint;
      const ButtonOrFinger: Integer);
    procedure MouseOrTouchUp(const Position: TMZPoint;
      const ButtonOrFinger: Integer);

    property Space: TCPSpace read FSpace;
    property DrawHash: Boolean read GetDrawHash write SetDrawHash;
    property DrawBoundingBoxes: Boolean read GetDrawBoundingBoxes write SetDrawBoundingBoxes;
    property DrawShapes: Boolean read FDrawShapes write FDrawShapes;
    property DrawOutlines: Boolean read FDrawOutlines write FDrawOutlines;
    property CollisionPointSize: Single read FCollisionPointSize write FCollisionPointSize;
    property BodyPointSize: Single read FBodyPointSize write FBodyPointSize;
    property ArrowDirection: TCPVect read FArrowDirection;
    property UseBallTexture: Boolean read FUseBallTexture write FUseBallTexture;
    property UseBoxTexture: Boolean read FUseBoxTexture write FUseBoxTexture;
    property UseTriangleTexture: Boolean read FUseTriangleTexture write FUseTriangleTexture;
    property UsePentagonTexture: Boolean read FUsePentagonTexture write FUsePentagonTexture;
    property ShowArrowButtons: Boolean read FShowArrowButtons write FShowArrowButtons;
    property Message: UTF8String read FMessage write FMessage;
    property MousePoint: TCPVect read FMousePoint;
  end;

implementation

uses
  SysUtils,
  Math,
  DemoApplication;

const
  LINE_COLOR       = $000000;
  COLLISION_COLOR  = $FF0000;
  CONSTRAINT_COLOR = $80FF80;
  BB_COLOR         = $4C804C;

const
  FRAME_BALL     = 1;
  FRAME_BOX      = 2;
  FRAME_TRIANGLE = 3;
  FRAME_PENTAGON = 4;

var
  CurrentSceneIndex: Integer = 0;

{ Copied from cpSpaceHash.c }
function HashFunc(const X, Y, N: cpHashValue): cpHashValue; inline;
begin
  Result := (X * 1640531513 xor Y * 2654435789) mod N;
end;

{ TDemoScene }

procedure TDemoScene.AutoFree(const Obj: TObject);
begin
  FObjects.Add(Obj);
end;

function TDemoScene.ColorForShape(const Shape: TCPShape): Cardinal;
var
  Body: TCPBody;
begin
  Body := Shape.Body;
  if Assigned(Body) then
  begin
    if Assigned(Body.Handle.node.next) then
    begin
      Result := $404040;
      Exit;
    end
    else if (Body.Handle.node.idleTime > Space.SleepTimeThreshold) then
    begin
      Result := $E6E6E6;
      Exit;
    end;
  end;
  Result := ColorFromPointer(Shape);
end;

class function TDemoScene.ColorFromPointer(const Ptr: Pointer): Cardinal;
const
  MULT = 127;
  ADD  = 63;
var
  R, G, B, Max: Byte;
begin
  Result := Cardinal(Ptr);

  { hash the pointer up nicely }
  Result := (Result  +  $7ed55d16)  +  (Result shl 12);
  Result := (Result xor $c761c23c) xor (Result shr 19);
  Result := (Result  +  $165667b1)  +  (Result shl 5);
  Result := (Result  +  $d3a2646c) xor (Result shl 9);
  Result := (Result  +  $fd7046c5)  +  (Result shl 3);
  Result := (Result xor $b55a4f09) xor (Result shr 16);

  R := (Result shr 16) and $FF;
  G := (Result shr 8) and $FF;
  B := Result and $FF;

  if (R > G) then
    if (R > B) then
      Max := R
    else
      Max := B
  else
    if (G > B) then
      Max := G
    else
      Max := B;
  if (Max = 0) then
    Max := 1;

  R := (R * MULT) div Max + ADD;
  G := (G * MULT) div Max + ADD;
  B := (B * MULT) div Max + ADD;

  Result := (R shl 16) or (G shl 8) or B;
end;

procedure TDemoScene.DrawBoundingBox(const Space: TCPSpace;
  const Shape: TCPShape; const Data: Pointer);
var
  BB: TCPBB;
begin
  BB := Shape.BoundingBox;
  Canvas.DrawRect(X_OFF + BB.L, Y_OFF - BB.T, BB.Width, BB.Height, BB_COLOR);
end;

procedure TDemoScene.DrawCircleShape(const Body: TCPBody;
  const Shape: TCPCircleShape);
var
  Quality: Word;
  R: TCPFloat;
  P: TMZPoint;
begin
  P := TMZPoint.Create(X_OFF + Shape.TC.X, Y_OFF - Shape.TC.Y);
  R := Shape.Radius;
  if FButtonTextures.Checked and FUseBallTexture and (not Shape.Sensor) then
  begin
    Canvas.TextureColor := ColorForShape(Shape);
    Canvas.DrawSpriteFrame(FTextures, FRAME_BALL, P.X - R, P.Y - R,
      R * 2, R * 2, -Shape.Body.Angle, $FF, [efBlend, efColor])
  end
  else
  begin
    Quality := Trunc(Shape.Radius);
    if (Quality < 8) then
      Quality := 8;
    if (not Shape.Sensor) then
      Canvas.FillCircle(P.X, P.Y, R, ColorForShape(Shape), $FF, Quality);

    Canvas.DrawCircle(P.X, P.Y, R, LINE_COLOR, $FF, Quality);

    Canvas.DrawLine(P.X, P.Y,
      P.X - Shape.Body.Rotation.Y * R,
      P.Y + Shape.Body.Rotation.X * R, LINE_COLOR);
  end;
end;

procedure TDemoScene.DrawConstraint(const Constraint: TCPConstraint);
var
  A, B: TCPBody;
begin
  A := Constraint.A;
  B := Constraint.B;

  case Constraint.ConstraintType of
    ctPinJoint:
      DrawPinJoint(TCPPinJoint(Constraint), A, B);

    ctSlideJoint:
      DrawSlideJoint(TCPSlideJoint(Constraint), A, B);

    ctPivotJoint:
      DrawPivotJoint(TCPPivotJoint(Constraint), A, B);

    ctGrooveJoint:
      DrawGrooveJoint(TCPGrooveJoint(Constraint), A, B);

    ctDampedSpring:
      DrawSpring(TCPDampedSpring(Constraint), A, B);
  end;
end;

procedure TDemoScene.DrawGrooveJoint(const Joint: TCPGrooveJoint; const BodyA,
  BodyB: TCPBody);
var
  A, B, C: TCPVect;
begin
  A := BodyA.Position + Joint.GrooveA.Rotate(BodyA.Rotation);
  B := BodyA.Position + Joint.GrooveB.Rotate(BodyA.Rotation);
  C := BodyB.Position + Joint.Anchor2.Rotate(BodyB.Rotation);
  A.X := X_OFF + A.X;
  A.Y := Y_OFF - A.Y;
  B.X := X_OFF + B.X;
  B.Y := Y_OFF - B.Y;
  C.X := X_OFF + C.X;
  C.Y := Y_OFF - C.Y;
  Canvas.FillRect(C.X - 2.5, C.Y - 2.5, 5, 5, CONSTRAINT_COLOR);
  Canvas.DrawLine(A.X, A.Y, B.X, B.Y, CONSTRAINT_COLOR);
end;

procedure TDemoScene.DrawInfo;
var
  Arbiters, Points, Constraints, I: Integer;
  KE, W: TCPFloat;
  Body: TCPBody;
  S: UTF8String;
begin
  Arbiters := FSpace.ArbiterCount;
  Points := 0;
  for I := 0 to Arbiters - 1 do
    Inc(Points, FSpace.Arbiters[I].ContactCount);

  Constraints := (FSpace.ConstraintCount + Points) * (FSpace.Iterations + FSpace.ElasticIterations);

  if (Arbiters > FMaxArbiters) then
    FMaxArbiters := Arbiters;
  if (Points > FMaxPoints) then
    FMaxPoints := Points;
  if (Constraints > FMaxConstraints) then
    FMaxConstraints := Constraints;

  KE := 0;
  for I := 0 to FSpace.BodyCount - 1 do
  begin
    Body := FSpace.Bodies[I];
    if (Body.Mass < INFINITY) and (Body.Moment < INFINITY) then
    begin
      W := Body.RotationalVelocity * DEG2RAD;
      KE := KE + (Body.Mass * (Body.Velocity * Body.Velocity))
               + (Body.Moment * W * W);
    end;
  end;
  if (KE < 1e-10) then
    KE := 0;

  S := TMZUtils.Format(
    'Arbiters: %d (%d) - Contact Points: %d (%d)'#10+
    'Other Constraints: %d, Iterations: %d'#10+
    'Constraints x Iterations: %d (%d)'#10+
    'KE: %1.4e',
    [Arbiters, FMaxArbiters, Points, FMaxPoints,
     FSpace.ConstraintCount, FSpace.Iterations + FSpace.ElasticIterations,
      Constraints, FMaxConstraints, KE]);

  Canvas.DrawText(FFont, 4, 50, 1, 0, S, 255, 0);
end;

procedure TDemoScene.DrawInstructions;
begin
  {$IFDEF iOS}
  Canvas.DrawText(FFont, 4, 30, 1, 0, 'Use your finger to grab objects', 255, 0);
  {$ELSE}
  Canvas.DrawText(FFont, 4, 30, 1, 0, 'Use the mouse to grab objects', 255, 0);
  {$ENDIF}
end;

procedure TDemoScene.DrawObject(const Space: TCPSpace; const Shape: TCPShape;
  const Data: Pointer);
var
  Body: TCPBody;
begin
  Body := Shape.Body;
  case Shape.ShapeType of
    stCircle:
      DrawCircleShape(Body, TCPCircleShape(Shape));

    stSegment:
      DrawSegmentShape(Body, TCPSegmentShape(Shape));

    stPoly:
      DrawPolyShape(Body, TCPPolyShape(Shape));
  else
    Assert(False);
  end;
end;

procedure TDemoScene.DrawPinJoint(const Joint: TCPPinJoint; const BodyA,
  BodyB: TCPBody);
var
  A, B: TCPVect;
begin
  A := BodyA.Position + Joint.Anchor1.Rotate(BodyA.Rotation);
  B := BodyB.Position + Joint.Anchor2.Rotate(BodyB.Rotation);
  A.X := X_OFF + A.X;
  A.Y := Y_OFF - A.Y;
  B.X := X_OFF + B.X;
  B.Y := Y_OFF - B.Y;
  Canvas.FillRect(A.X - 2.5, A.Y - 2.5, 5, 5, CONSTRAINT_COLOR);
  Canvas.FillRect(B.X - 2.5, B.Y - 2.5, 5, 5, CONSTRAINT_COLOR);
  Canvas.DrawLine(A.X, A.Y, B.X, B.Y, CONSTRAINT_COLOR);
end;

procedure TDemoScene.DrawPivotJoint(const Joint: TCPPivotJoint;
  const BodyA, BodyB: TCPBody);
var
  A, B: TCPVect;
begin
  A := BodyA.Position + Joint.Anchor1.Rotate(BodyA.Rotation);
  B := BodyB.Position + Joint.Anchor2.Rotate(BodyB.Rotation);
  Canvas.FillRect(X_OFF + A.X - 5, Y_OFF - A.Y - 5, 10, 10, CONSTRAINT_COLOR);
  Canvas.FillRect(X_OFF + B.X - 5, Y_OFF - B.Y - 5, 10, 10, CONSTRAINT_COLOR);
end;

procedure TDemoScene.DrawPolyShape(const Body: TCPBody;
  const Shape: TCPPolyShape);
const
  MAX_TRIANGLES = 20;
  MAX_POINTS = 50;
var
  Triangles: array [0..MAX_TRIANGLES] of TMZTriangle;
  Points: array [0..MAX_POINTS - 1] of TMZPoint;
  TriangleCount, VertexCount, I: Integer;
  Size: TCPFloat;
  V, V1, V2, V3: TCPVect;
  P: PCPVect;
  First, Next: TMZPoint;
begin
  VertexCount := Shape.VertexCount;

  if FButtonTextures.Checked then
  begin
    case VertexCount of
      3: if FUseTriangleTexture then
         begin
           { This currently only works for non-rotating triangles. First,
             calculate the length of an edge. In this case, we assume a straight
             horizontal edge between the 1st and 3rd vertex. }
           P := Shape.FirstTV;
           V1 := P^;
           Inc(P);
           V2 := P^;
           Inc(P);
           V3 := P^;
           Size := V3.X - V1.X;

           Canvas.TextureColor := ColorForShape(Shape);
           Canvas.DrawSpriteFrame(FTextures, FRAME_TRIANGLE, X_OFF + V1.X,
             Y_OFF - V2.Y, Size, Size, 0, $FF, [efBlend, efColor]);
           Exit;
         end;

      4: if FUseBoxTexture then
         begin
           { Calculate length of box }
           P := Shape.FirstTV;
           V := P^;
           Inc(P);
           Size := (V - P^).Length;

           V := Body.Position;
           Canvas.TextureColor := ColorForShape(Shape);
           Canvas.DrawSpriteFrame(FTextures, FRAME_BOX, X_OFF + V.X - 0.5 * Size,
             Y_OFF - V.Y - 0.5 * Size, Size, Size, -Body.Angle, $FF, [efBlend, efColor]);
           Exit;
         end;

      5: if FUsePentagonTexture then
         begin
           { Calculate the diameter of the pentagon by taking the distance
             between two vertices that are separated by another vertex. }
           P := Shape.FirstTV;
           V1 := P^;
           Inc(P, 2);
           V2 := P^;
           Inc(P);
           V3 := P^;
           Size := (V1 - V2).Length;

           { Estimate the center of the pentagon by taking the average of the
             first vertex and the two opposite vertices. }
           V.X := 0.25 * (V1.X + V1.X + V2.X + V3.X);
           V.Y := 0.25 * (V1.Y + V1.Y + V2.Y + V3.Y);

           Canvas.TextureColor := ColorForShape(Shape);
           Canvas.DrawSpriteFrame(FTextures, FRAME_PENTAGON,
             X_OFF + V.X - 0.5 * Size, Y_OFF - V.Y - 0.5 * Size, Size, Size,
             -Body.Angle, $FF, [efBlend, efColor]);
           Exit;
         end;
    end;
  end;

  if (not Shape.Sensor) then
  begin
    { This is a simple way to convert a convex polygon to a list of triangles. }
    TriangleCount := VertexCount - 2;
    Assert((TriangleCount > 0) and (TriangleCount < MAX_TRIANGLES));

    P := Shape.FirstTV;
    First.X := X_OFF + P.X;
    First.Y := Y_OFF - P.Y;

    Inc(P);
    Next.X := X_OFF + P.X;
    Next.Y := Y_OFF - P.Y;
    Inc(P);

    for I := 0 to TriangleCount - 1 do
    begin
      Triangles[I].P1 := First;
      Triangles[I].P2 := Next;
      Next.X := X_OFF + P.X;
      Next.Y := Y_OFF - P.Y;
      Inc(P);
      Triangles[I].P3 := Next;
    end;
    Canvas.FillTriangleList(@Triangles[0], TriangleCount, ColorForShape(Shape));
  end;

  { To draw the polygon outline, we need to convert each coordinate from
    Double (Chipmunk) to Single (ZenGL) }
  Assert(VertexCount < MAX_POINTS);
  P := Shape.FirstTV;
  for I := 0 to Shape.VertexCount - 1 do
  begin
    { Convert from Double to Single }
    Points[I].X := X_OFF + P.X;
    Points[I].Y := Y_OFF - P.Y;
    Inc(P);
  end;
  Canvas.DrawPolygon(PMZPoint(@Points[0]), VertexCount, True, LINE_COLOR);
end;

procedure TDemoScene.DrawSegmentShape(const Body: TCPBody;
  const Shape: TCPSegmentShape);
var
  A, B, C, D: TCPVect;
  Angle: TCPFloat;
  Points: array [0..5] of TMZPoint;
  Triangles: array [0..3] of TMZTriangle;
begin
  A := TCPVect.Create(X_OFF + Shape.T1.X, Y_OFF - Shape.T1.Y);
  B := TCPVect.Create(X_OFF + Shape.T2.X, Y_OFF - Shape.T2.Y);
  if (Shape.Radius > 0) then
  begin
    { We draw a "fat" line segment as an elongated hexagon:
         ______________
        /              \
        \______________/

      We use some math to calculate the 6 vertices of the hexagon. }

    C := B - A;
    Angle := C.ToAngle;
    D := TCPVect.ForAngle(Angle) * Shape.Radius;

    Points[0] := TMZPoint.Create(A.X - D.Y, A.Y + D.X);
    Points[1] := TMZPoint.Create(A.X - D.X, A.Y - D.Y);
    Points[2] := TMZPoint.Create(A.X + D.Y, A.Y - D.X);

    Points[3] := TMZPoint.Create(B.X + D.Y, B.Y - D.X);
    Points[4] := TMZPoint.Create(B.X + D.X, B.Y + D.Y);
    Points[5] := TMZPoint.Create(B.X - D.Y, B.Y + D.X);

    if (not Shape.Sensor) then
    begin
      Triangles[0] := TMZTriangle.Create(Points[0], Points[1], Points[2]);
      Triangles[1] := TMZTriangle.Create(Points[0], Points[2], Points[5]);
      Triangles[2] := TMZTriangle.Create(Points[2], Points[3], Points[5]);
      Triangles[3] := TMZTriangle.Create(Points[3], Points[4], Points[5]);
      Canvas.FillTriangleList(Triangles, ColorForShape(Shape));
    end;
    Canvas.DrawPolygon(Points, True, LINE_COLOR);
  end
  else
    Canvas.DrawLine(A.X, A.Y, B.X, B.Y, LINE_COLOR);
end;

procedure TDemoScene.DrawSlideJoint(const Joint: TCPSlideJoint; const BodyA,
  BodyB: TCPBody);
var
  A, B: TCPVect;
begin
  A := BodyA.Position + Joint.Anchor1.Rotate(BodyA.Rotation);
  B := BodyB.Position + Joint.Anchor2.Rotate(BodyB.Rotation);
  A.X := X_OFF + A.X;
  A.Y := Y_OFF - A.Y;
  B.X := X_OFF + B.X;
  B.Y := Y_OFF - B.Y;
  Canvas.FillRect(A.X - 2.5, A.Y - 2.5, 5, 5, CONSTRAINT_COLOR);
  Canvas.FillRect(B.X - 2.5, B.Y - 2.5, 5, 5, CONSTRAINT_COLOR);
  Canvas.DrawLine(A.X, A.Y, B.X, B.Y, CONSTRAINT_COLOR);
end;

procedure TDemoScene.DrawSpace;
var
  I, J: Integer;
  Body: TCPBody;
  Arbiter: PCPArbiter;
  P: TCPVect;
  XOffset, YOffset: TCPFloat;
begin
  if DrawHash then
  begin
    Canvas.SetColorMask(False, True, False, True);
    DrawSpatialHash(Space.Handle.activeShapes);
    Canvas.SetColorMask(True, False, False, False);
    DrawSpatialHash(Space.Handle.staticShapes);
    Canvas.SetColorMask(True, True, True, True);
  end;

  if FDrawShapes then
  begin
    Space.ForEachShape(DrawObject, nil);
    Space.ForEachStaticShape(DrawObject, nil);
  end;

  if DrawBoundingBoxes then
  begin
    Space.ForEachShape(DrawBoundingBox, nil);
    Space.ForEachStaticShape(DrawBoundingBox, nil);
  end;

  for I := 0 to Space.ConstraintCount - 1 do
    DrawConstraint(Space.Constraints[I]);

  if (FBodyPointSize > 0) then
  begin
    XOffset := X_OFF - FBodyPointSize / 2;
    YOffset := Y_OFF - FBodyPointSize / 2;
    for I := 0 to Space.BodyCount - 1 do
    begin
      Body := Space.Bodies[I];
      P := Body.Position;
      Canvas.FillRect(XOffset + P.X, YOffset - P.Y, FBodyPointSize, FBodyPointSize, LINE_COLOR);
    end;
  end;

  if (FCollisionPointSize > 0) then
  begin
    XOffset := X_OFF - FCollisionPointSize / 2;
    YOffset := Y_OFF - FCollisionPointSize / 2;
    for I := 0 to Space.ArbiterCount - 1 do
    begin
      Arbiter := Space.Arbiters[I];
      for J := 0 to Arbiter.ContactCount - 1 do
      begin
        P := Arbiter.ContactPoints[J].Point;
        Canvas.FillRect(XOffset + P.X, YOffset - P.Y, FCollisionPointSize,
          FCollisionPointSize, COLLISION_COLOR);
      end;
    end;
  end;
end;

procedure TDemoScene.DrawSpatialHash(const Hash: PcpSpaceHash);
var
  BB: TCPBB;
  Dim, V: TCPFloat;
  I, J, N, L, R, B, T, CellCount, Index: Integer;
  Bins: PPcpSpaceHashBin;
  Bin: PcpSpaceHashBin;
  C: Byte;
  Color: Cardinal;
begin
  BB := CPBBNew(-320, -240, 320, 240);
  Dim := Hash.clldim;
  N := Hash.numcells;

  L := Floor(BB.L / Dim);
  R := Floor(BB.R / Dim);
  B := Floor(BB.B / Dim);
  T := Floor(BB.T / Dim);

  for I := L to R do
  begin
    for J := B to T do
    begin
      CellCount := 0;
      Index := HashFunc(I, J, N);

      Bins := Hash.table;
      Inc(Bins, Index);
      Bin := Bins^;
      while Assigned(Bin) do
      begin
        Inc(CellCount);
        Bin := Bin.next;
      end;

      V := 1 - CellCount / 10;
      C := Trunc(V * 255);
      Color := C or (C shl 8) or (C shl 16);
      Canvas.FillRect(X_OFF + I * Dim, Y_OFF - (J + 1) * Dim, Dim, Dim, Color);
    end;
  end;
end;

procedure TDemoScene.DrawSpring(const Spring: TCPDampedSpring; const BodyA,
  BodyB: TCPBody);
const
  RADIUS = 7;
var
  A, B, C, A1, B1, V0, V1, V2, V3: TCPVect;
  Angle: Single;
  P: array [0..13] of TMZPoint;
  I, J: Integer;
begin
  A := BodyA.Position + Spring.Anchor1.Rotate(BodyA.Rotation);
  B := BodyB.Position + Spring.Anchor2.Rotate(BodyB.Rotation);

  A.X := X_OFF + A.X;
  A.Y := Y_OFF - A.Y;
  B.X := X_OFF + B.X;
  B.Y := Y_OFF - B.Y;

  Canvas.FillRect(A.X - 2.5, A.Y - 2.5, 5, 5, CONSTRAINT_COLOR);
  Canvas.FillRect(B.X - 2.5, B.Y - 2.5, 5, 5, CONSTRAINT_COLOR);

  { Calculate the points to create the following Spring shape:


    ___/\  /\  /\  /\  /\  ___
         \/  \/  \/  \/  \/       }

  A1 := A.LinearInterpolate(B, 0.25);
  B1 := B.LinearInterpolate(A, 0.25);

  V3 := A1.LinearInterpolate(B1, 0.2) - A1;
  Angle := V3.ToAngle;
  C := TCPVect.ForAngle(Angle + 90) * RADIUS;

  V0 := TCPVect.Create;
  V1 := V0.LinearInterpolate(V3, 0.25) - C;
  V2 := V0.LinearInterpolate(V3, 0.75) + C;

  P[0] := TMZPoint.Create(A.X, A.Y);
  P[1] := TMZPoint.Create(A1.X, A1.Y);
  P[2] := TMZPoint.Create(A1.X + V1.X, A1.Y + V1.Y);
  P[3] := TMZPoint.Create(A1.X + V2.X, A1.Y + V2.Y);
  A1 := A1 + V3;

  J := 4;
  for I := 0 to 3 do
  begin
    P[J+0] := TMZPoint.Create(A1.X + V1.X, A1.Y + V1.Y);
    P[J+1] := TMZPoint.Create(A1.X + V2.X, A1.Y + V2.Y);
    A1 := A1 + V3;
    Inc(J, 2);
  end;

  P[12] := TMZPoint.Create(A1.X, A1.Y);
  P[13] := TMZPoint.Create(B.X, B.Y);

  Canvas.DrawPolygon(P, False, CONSTRAINT_COLOR);
end;

function TDemoScene.GetDrawBoundingBoxes: Boolean;
begin
  Result := FButtonBoundingBox.Checked;
end;

function TDemoScene.GetDrawHash: Boolean;
begin
  Result := FButtonSpatialHash.Checked;
end;

procedure TDemoScene.KeyDown(const KeyCode: TMZKeyCode);
begin
  inherited;
  case KeyCode of
    kcUp:
      FArrowDirection.Y := 1;

    kcDown:
      FArrowDirection.Y := -1;

    kcLeft:
      FArrowDirection.X := -1;

    kcRight:
      FArrowDirection.X := 1;
  end;
end;

procedure TDemoScene.KeyUp(const KeyCode: TMZKeyCode);
begin
  inherited;
  case KeyCode of
    kcEscape:
      Application.Quit;

    kcEnter, kcKPEnter:
      Application.SetScene(TMZSceneClass(ClassType).Create);

    kcUp:
      FArrowDirection.Y := 0;

    kcDown:
      FArrowDirection.Y := 0;

    kcLeft:
      FArrowDirection.X := 0;

    kcRight:
      FArrowDirection.X := 0;
  end;
end;

procedure TDemoScene.MouseDown(const Button: TMZMouseButton);
begin
  inherited;
  MouseOrTouchDown(TMZMouse.Position, Ord(Button));
end;

procedure TDemoScene.MouseOrTouchDown(const Position: TMZPoint;
  const ButtonOrFinger: Integer);
var
  Point: TCPVect;
  Shape: TCPShape;
  Body: TCPBody;
begin
  inherited;
  if (Position.Y < 25) then
  begin
    if Assigned(FButtonLeft) then
    begin
      if FButtonLeft.ContainsPoint(Position) then
        FArrowDirection.X := -1
      else if FButtonRight.ContainsPoint(Position) then
        FArrowDirection.X := 1
    end;
  end
  else if (ButtonOrFinger = 0) then
  begin
    { Check if an object is picked }
    Point := MouseToSpace;
    Shape := Space.PointQuery(Point, GRABABLE_MASK_BIT, CP_NO_GROUP);
    if Assigned(Shape) then
    begin
      Body := Shape.Body;
      if Assigned(FMouseJoint) then
      begin
        Space.RemoveConstraint(FMouseJoint);
        FMouseJoint.Free;
      end;
      FMouseJoint := TCPPivotJoint.Create(FMouseBody, Body, CPVZero,
        Body.WorldToLocal(Point));
      FMouseJoint.MaxForce := 50000;
      FMouseJoint.BiasCoef := 0.15;
      Space.AddConstraint(FMouseJoint);
    end;
  end;
end;

procedure TDemoScene.MouseOrTouchUp(const Position: TMZPoint;
  const ButtonOrFinger: Integer);
begin
  FArrowDirection.X := 0;
  if (Position.Y < 25) then
  begin
    { Check if a button is clicked }
    if FButtonPrev.ContainsPoint(Position) then
    begin
      if (CurrentSceneIndex > 0) then
      begin
        Dec(CurrentSceneIndex);
        Application.SetScene(DEMO_CLASSES[CurrentSceneIndex].Create);
      end;
    end
    else if FButtonRestart.ContainsPoint(Position) then
      Application.SetScene(DEMO_CLASSES[CurrentSceneIndex].Create)
    else if FButtonNext.ContainsPoint(Position) then
    begin
      if (CurrentSceneIndex < (Length(DEMO_CLASSES) - 1)) then
      begin
        Inc(CurrentSceneIndex);
        Application.SetScene(DEMO_CLASSES[CurrentSceneIndex].Create);
      end;
    end
    {$IFNDEF iOS}
    else if FButtonAntiAlias.ContainsPoint(Position) then
    begin
      Canvas.AntiAlias := not Canvas.AntiAlias;
      FButtonAntiAlias.Checked := Canvas.AntiAlias;
    end
    {$ENDIF}
    else if FButtonSpatialHash.ContainsPoint(Position) then
      DrawHash := not DrawHash
    else if FButtonBoundingBox.ContainsPoint(Position) then
      DrawBoundingBoxes := not DrawBoundingBoxes
    else if FButtonTextures.ContainsPoint(Position) then
      FButtonTextures.Checked := not FButtonTextures.Checked;
  end
  else if (ButtonOrFinger = 0) and Assigned(FMouseJoint) then
  begin
    Space.RemoveConstraint(FMouseJoint);
    FreeAndNil(FMouseJoint);
  end;
end;

procedure TDemoScene.MouseUp(const Button: TMZMouseButton);
begin
  inherited;
  MouseOrTouchUp(TMZMouse.Position, Ord(Button));
end;

function TDemoScene.MouseToSpace: TCPVect;
begin
  {$IFDEF iOS}
  Result.X := TMZTouch.X[0] - X_OFF;
  Result.Y := Y_OFF - TMZTouch.Y[0];
  {$ELSE}
  Result.X := TMZMouse.X - X_OFF;
  Result.Y := Y_OFF - TMZMouse.Y;
  {$ENDIF}
end;

procedure TDemoScene.RemoveAutoFree(const Obj: TObject);
begin
  FObjects.Remove(Obj);
end;

procedure TDemoScene.RenderFrame;
var
  AA: Boolean;
begin
  inherited;
  Canvas.BeginBatch;
  try
    AA := Canvas.AntiAlias;
    Canvas.AntiAlias := False;
    Canvas.FillRect(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, $FFFFFF);
    Canvas.AntiAlias := AA;
    DrawSpace;
    DrawInstructions;
    DrawInfo;

    FButtonPrev.Render;
    FButtonRestart.Render;
    FButtonNext.Render;
    {$IFNDEF iOS}
    FButtonAntiAlias.Render;
    {$ENDIF}
    FButtonSpatialHash.Render;
    FButtonBoundingBox.Render;
    FButtonTextures.Render;
    if Assigned(FButtonLeft) then
    begin
      FButtonLeft.Render;
      FButtonRight.Render;
    end;

    if (FMessage <> '') then
      Canvas.DrawText(FFont, 2, SCREEN_HEIGHT - 40, 1, 0, FMessage, $FF, 0);
  finally
    Canvas.EndBatch;
  end;
end;

procedure TDemoScene.SetDrawBoundingBoxes(const Value: Boolean);
begin
  FButtonBoundingBox.Checked := Value;
end;

procedure TDemoScene.SetDrawHash(const Value: Boolean);
begin
  FButtonSpatialHash.Checked := Value;
end;

procedure TDemoScene.Shutdown;
begin
  inherited;
  FObjects.Free;
  FMouseJoint.Free;
  FSpace.Free;
  FButtonPrev.Free;
  FButtonRestart.Free;
  FButtonNext.Free;
  {$IFNDEF iOS}
  FButtonAntiAlias.Free;
  {$ENDIF}
  FButtonSpatialHash.Free;
  FButtonBoundingBox.Free;
  FButtonTextures.Free;
  FButtonLeft.Free;
  FButtonRight.Free;
end;

procedure TDemoScene.Startup;
var
  App: TDemoApplication;
  X: Single;
begin
  inherited;
  App := Application as TDemoApplication;
  FFont := App.Font;
  FTextures := App.Textures;
  FMouseBody := App.MouseBody;
  App.Caption := 'MZ04 - Chipmunk Demos - ' + Title;
  FDrawShapes := True;
  FDrawOutlines := True;
  FCollisionPointSize := 4;
  FSpace := TCPSpace.Create;
  FObjects := TObjectList.Create;

  FButtonPrev := TSimpleButton.Create(Canvas, FFont, 'Previous demo', 2, 2);
  FButtonRestart := TSimpleButton.Create(Canvas, FFont, 'Restart demo', FButtonPrev.Bounds.Right + 2, 2);
  FButtonNext := TSimpleButton.Create(Canvas, FFont, 'Next demo', FButtonRestart.Bounds.Right + 2, 2);
  X := FButtonNext.Bounds.Right;
  {$IFNDEF iOS}
  FButtonAntiAlias := TSimpleButton.Create(Canvas, FFont, 'Enable anti-aliasing', X + 2, 2);
  X := FButtonAntiAlias.Bounds.Right;
  {$ENDIF}
  FButtonSpatialHash := TSimpleButton.Create(Canvas, FFont, 'Toggles spatial hash visualization', X + 2, 2);
  FButtonBoundingBox := TSimpleButton.Create(Canvas, FFont, 'Toggle bounding boxes', FButtonSpatialHash.Bounds.Right + 2, 2);
  FButtonTextures := TSimpleButton.Create(Canvas, FFont, 'Toggle textures', FButtonBoundingBox.Bounds.Right + 2, 2);
  FButtonTextures.Checked := True;

  if (FShowArrowButtons) then
  begin
    FButtonLeft := TSimpleButton.Create(Canvas, FFont, '<--', FButtonTextures.Bounds.Right + 2, 2);
    FButtonRight := TSimpleButton.Create(Canvas, FFont, '-->', FButtonLeft.Bounds.Right + 2, 2);
  end;
end;

procedure TDemoScene.TouchDown(const Finger: Integer);
begin
  inherited;
  {$IF DEFINED(iOS) or DEFINED(ANDROID)}
  MouseOrTouchDown(TMZTouch.Position[Finger], Finger);
  {$IFEND}
end;

procedure TDemoScene.TouchUp(const Finger: Integer);
begin
  inherited;
  {$IF DEFINED(iOS) or DEFINED(ANDROID)}
  MouseOrTouchUp(TMZTouch.Position[Finger], Finger);
  {$IFEND}
end;

procedure TDemoScene.Update(const DeltaTimeMs: Double);
var
  NewPoint: TCPVect;
begin
  inherited;
  FLastTime := FLastTime + DeltaTimeMs;
  if (FLastTime < MSEC_PER_FRAME) then
    Exit;

  FMousePoint := MouseToSpace;
  NewPoint := FMousePointLast.LinearInterpolate(FMousePoint, 0.25);
  FMouseBody.Position := NewPoint;
  FMouseBody.Velocity := (NewPoint - FMousePointLast) * 60;
  FMousePointLast := NewPoint;
  UpdateFrame(FTicks);

  Inc(FTicks);
  FLastTime := FLastTime - MSEC_PER_FRAME;

  { Catch up if we cannot maintain 60fps }
  while (FLastTime > MSEC_PER_FRAME) do
    FLastTime := FLastTime - MSEC_PER_FRAME;

  TMZMouse.ClearState;
end;

{ TSimpleButton }

function TSimpleButton.ContainsPoint(const P: TMZPoint): Boolean;
begin
  Result := FBounds.ContainsPoint(P);
end;

constructor TSimpleButton.Create(const Canvas: TMZCanvas; const Font: TMZFont;
  const Caption: UTF8String; const X, Y: Single);
begin
  inherited Create;
  FCanvas := Canvas;
  FFont := Font;
  FCaption := Caption;
  FBounds.X := X;
  FBounds.Y := Y;
  FBounds.W := FCanvas.CalculateTextWidth(FFont, FCaption) + 8;
  FBounds.H := FCanvas.CalculateTextHeight(FFont, FBounds.W, FCaption) + 8;
end;

procedure TSimpleButton.Render;
begin
  if (FChecked) then
  begin
    FCanvas.FillRect(FBounds, $15428B);
    FCanvas.DrawText(FFont, FBounds, FCaption, [tfHAlignCenter, tfVAlignCenter]);
  end
  else
  begin
    FCanvas.FillRect(FBounds, $CBDEF4);
    FCanvas.DrawRect(FBounds, $15428B);
    FCanvas.DrawText(FFont, FBounds, 1, 0, FCaption, $FF, $15428B, [tfHAlignCenter, tfVAlignCenter]);
  end;
end;

end.
