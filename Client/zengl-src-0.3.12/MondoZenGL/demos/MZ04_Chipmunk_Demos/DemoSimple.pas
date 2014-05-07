unit DemoSimple;

interface

uses
  {$IFNDEF FPC}
  zglChipmunk,
  {$ENDIF}
  DemoScene,
  mzChipmunk;

type
  TDemoSimple = class(TDemoScene)
  protected
    class function Title: UTF8String; override;
    procedure Startup; override;
    procedure UpdateFrame(const Ticks: Integer); override;
  end;

implementation

{ TDemoSimple }

procedure TDemoSimple.Startup;
const
  RADIUS = 15;
  MASS   = 10;
var
  Ground, BallShape: TCPShape;
  BallBody: TCPBody;
begin
  inherited;
  UseBallTexture := True;

  { Lets set some parameters of the space:
    More iterations make the simulation more accurate but slower }
  Space.Iterations := 10;

  { These parameters tune the efficiency of the collision detection.
    For more info: http://code.google.com/p/chipmunk-physics/wiki/cpSpace }
  Space.ResizeStaticHash(30, 1000);
  Space.ResizeActiveHash(30, 1000);

  { Give it some gravity }
  Space.Gravity := CPV(0, -100);

  { Create A ground segment along the bottom of the screen
    By attaching it to Space.StaticBody instead of a body, we make it a
    static shape. }
  Ground := TCPSegmentShape.Create(Space.StaticBody, CPV(-320, -240),
    CPV(320, -240), 0);
  AutoFree(Ground);

  { Set some parameters of the shape.
    For more info: http://code.google.com/p/chipmunk-physics/wiki/cpShape }
  Ground.Elasticity := 1;
  Ground.Friction := 1;
  Ground.Layers := NOT_GRABABLE_MASK; { Used by the Demo mouse grabbing code }

  { Add the shape to the space as a static shape.
    If a shape never changes position, add it as static so Chipmunk knows it
    only needs to calculate collision information for it once when it is added.
    Do not change the postion of a static shape after adding it. }
  Space.AddShape(Ground);

  { Add a moving circle object. This time we need to give a mass and moment of
    inertia when creating the circle.}
  BallBody := TCPBody.Create(MASS, TCPBody.MomentForCircle(MASS, 0, RADIUS, CPVZero));
  AutoFree(BallBody);

  { Set some parameters of the body:
    For more info: http://code.google.com/p/chipmunk-physics/wiki/cpBody }
  BallBody.Position := CPV(0, -240 + Radius + 50);
  BallBody.Velocity := CPV(0, -20);
  Space.AddBody(BallBody);

  { Add a circle shape for the ball.
    Shapes are always defined relative to the center of gravity of the body they
    are attached to. When the body moves or rotates, the shape will move with it.
    Additionally, all of the TCPSpace.Add* functions return the thing they added
    so you can create and add in one go. }
  BallShape := Space.AddShape(TCPCircleShape.Create(BallBody, RADIUS, CPVZero));
  AutoFree(BallShape);
  BallShape.Elasticity := 0;
  BallShape.Friction := 0.9;
end;

class function TDemoSimple.Title: UTF8String;
begin
  Result := 'Simple';
end;

procedure TDemoSimple.UpdateFrame(const Ticks: Integer);
begin
  { Chipmunk allows you to use a different timestep each frame, but it works
    much better when you use a fixed timestep. An excellent article on why fixed
    timesteps for game logic can be found here:
    http://gafferongames.com/game-physics/fix-your-timestep/ }
  Space.Step(1 / FRAMES_PER_SECOND);
end;

end.