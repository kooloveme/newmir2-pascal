unit mzChipmunk;

{ Copyright (C) 2012 Erik van Bilsen

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE. }

{@@2. Class wrappers for Chipmunk
  This unit contains class wrappers for the
  <exref target="http://chipmunk-physics.net/">Chipmunk Physics Engine</exref>.

  This unit is part of MondoZenGL but does not necessarily depend on it (because
  Chipmunk doesn't perform any rendering by itself).

  Before you start using physics, you need to call <link TCPChipmunk.Initialize>.
  This will load the Chipmunk library (unless USE_CHIPMUNK_STATIC is defined) and
  initialize it.

  The starting point for a physics simulation is <link TCPSpace>. You add
  rigid bodies (<link TCPBody>), shapes (<link TCPShape> descendants) and
  constraints (<link TCPConstraint> descendants) to a space.

  A <link TCPBody> is a physical body without any shape. It has a mass and a
  moment of intertia.

  To define the shape of a body, you attach one or more <link TCPShape>
  descendant objects to it. By combining shapes, you can create any object
  you want. There are currently 3 collision shape types:
  * Circles (<link TCPCircleShape>): Fastest and simplest collision shape.
  * Line segments (<link TCPSegmentShape>): Meant mainly as a static shape.
    They can be attached to moving bodies, but they don’t currently generate
    collisions with other line segments. Can be beveled in order to give
    them a thickness.
  * Convex polygons (<link TCPPolyShape>): Slowest, but most flexible
    collision shape.

  You can also attach two bodies to each other using a <link TCPConstraint>
  descendant object (or joint). The type of constraint determines how the two
  bodies interact with each other:
  * <link TCPPinJoint>
  * <link TCPSlideJoint>
  * <link TCPPivotJoint>
  * <link TCPGrooveJoint>
  * <link TCPDampedSpring>
  * <link TCPDampedRotarySpring>
  * <link TCPRotaryLimitJoint>
  * <link TCPRatchetJoint>
  * <link TCPGearJoint>
  * <link TCPSimpleMotor> }

interface

{$INCLUDE 'mz_config.cfg'}

{$REGION 'Chipmunk Basics'}
uses
  SysUtils,
  zglChipmunk;

const
  { Summary:
      Converts radians to degrees. }
  RAD2DEG = 57.29578049;

const
  { Summary:
      Converts degrees to radians. }
  DEG2RAD = 0.017453292;

const
  { Summary:
      Infinity, for Chipmunk purposes. }
  INFINITY = 1e1000;

type
  { Summary:
      Exception class for Chipmunk specific errors. }
  ECPError = class(Exception);

type
  { Summary:
      Chipmunk floating-point type. Defaults to Single on mobile devices, or
      Double otherwide. }
  TCPFloat = cpFloat;

type
  { Summary:
      Chipmunk boolean type. }
  TCPBool = cpBool;

type
  { Summary:
      Pointer type defined for callbacks and the user definable data pointer
      on most Chipmunk structs. }
  TCPDataPointer = cpDataPointer;

type
  { Summary:
      Unique identifier for collision shape types. }
  TCPCollisionType = cpCollisionType;

type
  { Summary:
      Unique identifier for collision groups. A <link CP_NO_GROUP> value is
      defined that can be used when you don’t want to specify a group. }
  TCPGroup = cpGroup;

const
  { Summary:
      A <link TCPGroup> value that can be used when you don’t want to specify
      a group. }
  CP_NO_GROUP = 0;

type
  { Summary:
      Type used as the layers bitmask. A <link CP_ALL_LAYERS> value is defined
      that has all layer bits set. }
  TCPLayers = cpLayers;

const
  { Summary:
      A <link TCPLayers> value that has all layer bits set. }
  CP_ALL_LAYERS = TCPLayers(not 0);

type
  { Summary:
      Global Chipmunk settings.
    Remarks:
      For consistency with (Mondo)ZenGL, all angles in this unit are measured in
      degrees. This is contrary to the Chipmunk C APIs, which measures angles
      in radians. }
  TCPChipmunk = class
  {$REGION 'Internal Declarations'}
  private
    class var FInitialized: Boolean;
  private
    class procedure Finalize; static;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Initializes the Chipmunk engine.
      Remarks:
        This will also load the Chipmunk library or DLL, unless the
        USE_CHIPMUNK_STATIC define is set. If the library cannot be loaded,
        an exception is raised. }
    class procedure Initialize; static;
  end;

type
  { Summary:
      Basic Chipmunk math }
  TCPMath = class
  public
    { Summary:
        Returns the minimum of two values. }
    class function Min(const A, B: TCPFloat): TCPFloat; inline; static;

    { Summary:
        Returns the maximum of two values. }
    class function Max(const A, B: TCPFloat): TCPFloat; inline; static;
  end;
{$ENDREGION 'Chipmunk Basics'}

{$REGION 'Chipmunk Vectors'}
type
  { Summary:
      A 2D vector }
  TCPVect = record
  public
    { Summary:
        X-value of the vector }
    X: TCPFloat;

    { Summary:
        Y-value of the vector }
    Y: TCPFloat;
  public
    { Summary:
        Creates a zero vector.
      Returns:
        The vector. }
    class function Create: TCPVect; overload; inline; static;

    { Summary:
        Creates a vector.
      Parameters:
        X: X-value of the vector.
        Y: Y-Value of the vector.
      Returns:
        The vector. }
    class function Create(const X, Y: TCPFloat): TCPVect; overload; inline; static;

    { Summary:
        Returns a string respresentation of the vector in the format '(x, y)'. }
    function ToString: String;

    { Summary:
        Checks whether this vectort equals another vector.
      Parameters:
        Other: the other vector.
      Returns:
        True if this vector equals Other.
      Remarks:
        Since the coordinates are floating-point numbers, the comparison is
        performed with a little error margin (it uses Math.SameValue).
        The Equal (=) and NotEqual (<>) class operators perform a precise
        comparison. }
    function Equals(const Other: TCPVect): Boolean; inline;

    { Summary:
        Checks if two vectors are equal.
      Returns:
        True if the vectors are equal. False otherwise.
      Remarks:
        Be careful when comparing floating point numbers! This function does
        an exact comparision, so the slightest variation returns False.
        See <link Equals> for a more fuzzy check. }
    class operator Equal(const A, B: TCPVect): Boolean; inline;

    { Summary:
        Checks if two vectors are not equal.
      Returns:
        True if the vectors are not equal. False otherwise.
      Remarks:
        Be careful when comparing floating point numbers! This function does
        an exact comparision, so the slightest variation returns True.
        See <link Equals> for a more fuzzy check. }
    class operator NotEqual(const A, B: TCPVect): Boolean; inline;

    { Summary:
        Add two vectors. }
    class operator Add(const A, B: TCPVect): TCPVect; inline;

    { Summary:
        Subtracts two vectors. }
    class operator Subtract(const A, B: TCPVect): TCPVect; inline;

    { Summary:
        Negate a vector. }
    class operator Negative(const A: TCPVect): TCPVect; inline;

    { Summary:
        Scalar multiplication. }
    class operator Multiply(const A: TCPVect; const S: TCPFloat): TCPVect; inline;

    { Summary:
        Vector dot product.
      Remarks:
        This is <b>not</b> a cross product. Use the <link CrossProduct> method
        for that. }
    class operator Multiply(const A, B: TCPVect): TCPFloat; inline;

    { Summary:
        Vector dot product }
    class function DotProduct(const V1, V2: TCPVect): TCPFloat; overload; inline; static;

    { Summary:
        Vector dot product }
    function DotProduct(const Other: TCPVect): TCPFloat; overload; inline;

    { Summary:
        2D vector cross product analog. The cross product of 2D vectors results
        in a 3D vector with only a Z component.
      Returns:
        The magnitude of the Z value. }
    class function CrossProduct(const V1, V2: TCPVect): TCPFloat; overload; inline; static;

    { Summary:
        2D vector cross product analog. The cross product of 2D vectors results
        in a 3D vector with only a Z component.
      Returns:
        The magnitude of the Z value. }
    function CrossProduct(const Other: TCPVect): TCPFloat; overload; inline;

    { Summary:
        Returns a perpendicular vector. (90 degree rotation) }
    function Perp: TCPVect; overload; inline;

    { Summary:
        Returns a perpendicular vector. (90 degree rotation) }
    class function Perp(const V: TCPVect): TCPVect; overload; inline; static;

    { Summary:
        Returns a perpendicular vector. (-90 degree rotation) }
    function RPerp: TCPVect; overload; inline;

    { Summary:
        Returns a perpendicular vector. (-90 degree rotation) }
    class function RPerp(const V: TCPVect): TCPVect; overload; inline; static;

    { Summary:
        Returns the vector projection of V1 onto V2. }
    class function Project(const V1, V2: TCPVect): TCPVect; overload; inline; static;

    { Summary:
        Returns the vector projection of this vector onto Target. }
    function Project(const Target: TCPVect): TCPVect; overload; inline;

    { Summary:
        Uses complex multiplication to rotate V1 by V2. Scaling will occur if
        V1 is not a unit vector. }
    class function Rotate(const V1, V2: TCPVect): TCPVect; overload; inline; static;

    { Summary:
        Uses complex multiplication to rotate this vector by Other.
        Scaling will occur if this vector is not a unit vector. }
    function Rotate(const Other: TCPVect): TCPVect; overload; inline;

    { Summary:
        Inverse of <link Rotate>. }
    class function Unrotate(const V1, V2: TCPVect): TCPVect; overload; inline; static;

    { Summary:
        Inverse of <link Rotate>. }
    function Unrotate(const Other: TCPVect): TCPVect; overload; inline;

    { Summary:
        Returns the length of the vector. }
    function Length: TCPFloat; overload; inline;

    { Summary:
        Returns the length of a vector. }
    class function Length(const V: TCPVect): TCPFloat; overload; inline; static;

    { Summary:
        Returns the squared length of the vector. Faster than <link Length> when
        you only need to compare lengths. }
    function LengthSq: TCPFloat; overload; inline;

    { Summary:
        Returns the squared length of a vector. Faster than <link Length> when
        you only need to compare lengths. }
    class function LengthSq(const V: TCPVect): TCPFloat; overload; inline; static;

    { Summary:
        Linearly interpolate between V1 and V2. }
    class function LinearInterpolate(const V1, V2: TCPVect;
      const T: TCPFloat): TCPVect; overload; inline; static;

    { Summary:
        Linearly interpolate between this vector and Target. }
    function LinearInterpolate(const Target: TCPVect;
      const T: TCPFloat): TCPVect; overload; inline;

    { Summary:
        Linearly interpolate between V1 towards V2 by distance D. }
    class function LinearInterpolateConst(const V1, V2: TCPVect;
      const D: TCPFloat): TCPVect; overload; inline; static;

    { Summary:
        Linearly interpolate between this vector towards Target by distance D. }
    function LinearInterpolateConst(const Target: TCPVect;
      const D: TCPFloat): TCPVect; overload; inline;

    { Summary:
        Spherical linearly interpolate between V1 and V2. }
    class function SphericalLinearInterpolate(const V1, V2: TCPVect;
      const T: TCPFloat): TCPVect; overload; inline; static;

    { Summary:
        Spherical linearly interpolate between this vector and Target. }
    function SphericalLinearInterpolate(const Target: TCPVect;
      const T: TCPFloat): TCPVect; overload; inline;

    { Summary:
        Spherical linearly interpolate between V1 towards V2 by no more than
        Angle in degrees. }
    class function SphericalLinearInterpolateConst(const V1, V2: TCPVect;
      const Angle: TCPFloat): TCPVect; overload; inline; static;

    { Summary:
        Spherical linearly interpolate between this vector towards Target by
        no more than Angle in degrees. }
    function SphericalLinearInterpolateConst(const Target: TCPVect;
      const Angle: TCPFloat): TCPVect; overload; inline;

    { Summary:
        Returns a normalized copy of a vector. }
    class function Normalize(const V: TCPVect): TCPVect; overload; inline; static;

    { Summary:
        Returns a normalized copy of this vector. }
    function Normalize: TCPVect; overload; inline;

    { Summary:
        Returns a normalized copy of V or <link CPVZero> if V was already
        <link CPVZero>. Protects against divide by zero errors. }
    class function NormalizeSafe(const V: TCPVect): TCPVect; overload; inline; static;

    { Summary:
        Returns a normalized copy of this vector or <link CPVZero> if this vector
        was already <link CPVZero>. Protects against divide by zero errors. }
    function NormalizeSafe: TCPVect; overload; inline;

    { Summary:
        Clamp V to length Len. }
    class function Clamp(const V: TCPVect; const Len: TCPFloat): TCPVect; overload; inline; static;

    { Summary:
        Clamp this vector to length Len. }
    function Clamp(const Len: TCPFloat): TCPVect; overload; inline;

    { Summary:
        Returns the distance between V1 and V2. }
    class function Distance(const V1, V2: TCPVect): TCPFloat; overload; inline; static;

    { Summary:
        Returns the distance between this vector and Other. }
    function Distance(const Other: TCPVect): TCPFloat; overload; inline;

    { Summary:
        Returns the squared distance between V1 and V2. Faster than
        <link Distance> when you only need to compare distances. }
    class function DistanceSq(const V1, V2: TCPVect): TCPFloat; overload; inline; static;

    { Summary:
        Returns the squared distance between this vector and Other. Faster than
        <link Distance> when you only need to compare distances. }
    function DistanceSq(const Other: TCPVect): TCPFloat; overload; inline;

    { Summary:
        Returns True if the distance between V1 and V2 is less than Dist. }
    class function Near(const V1, V2: TCPVect;
      const Dist: TCPFloat): TCPBool; overload; inline; static;

    { Summary:
        Returns True if the distance between this vector and Other is less than
        Dist. }
    function Near(const Other: TCPVect;
      const Dist: TCPFloat): TCPBool; overload; inline;

    { Summary:
        Returns the unit length vector for the given angle (in degrees). }
    class function ForAngle(const Angle: TCPFloat): TCPVect; inline; static;

    { Summary:
        Returns the angular direction V is pointing in (in degrees). }
    class function ToAngle(const V: TCPVect): TCPFloat; overload; inline; static;

    { Summary:
        Returns the angular direction this vector is pointing in (in degrees). }
    function ToAngle: TCPFloat; overload; inline;
  end;
  PCPVect = ^TCPVect;

{ Summary:
    Convenience constructor for creating new TCPVect records. }
function CPV(const X, Y: TCPFloat): TCPVect; inline;

{ Summary:
    Zero vector.
  Remarks:
    Tech note: this used to be a class property of TCPVect, but FPC 2.6.0 on
    Mac OS X does not work well with class properties (the vector would not be
    zero if you requested it). }
const
  CPVZero: TCPVect = (X: 0; Y: 0);

{$ENDREGION 'Chipmunk Vectors'}

{$REGION 'Chipmunk Bounding Boxes'}
type
  { Summary:
      Simple bounding box record. Stored as left, bottom, right and top values. }
  TCPBB = record
  public
    { Summary:
        Left value }
    L: TCPFloat;

    { Summary:
        Bottom value }
    B: TCPFloat;

    { Summary:
        Right value }

    R: TCPFloat;

    { Summary:
        Top value }
    T: TCPFloat;
  private
    function GetHeight: TCPFloat; inline;
    function GetWidth: TCPFloat; inline;
  public
    { Summary:
        Creates a bounding box.
      Parameters:
        L: Left value.
        B: Bottom value.
        R: Right value.
        T: Top value.
      Returns:
        The vector. }
    class function Create(const L, B, R, T: TCPFloat): TCPBB; inline; static;

    { Summary:
        Returns True if the bounding boxes intersect. }
    class function Intersects(const A, B: TCPBB): TCPBool; overload; inline; static;

    { Summary:
        Returns True if this bounding boxes intersects another. }
    function Intersects(const Other: TCPBB): TCPBool; overload; inline;

    { Summary:
        Returns True if BB completely contains Other. }
    class function Contains(const BB, Other: TCPBB): TCPBool; overload; inline; static;

    { Summary:
        Returns True if this bounding box completely contains Other. }
    function Contains(const Other: TCPBB): TCPBool; overload; inline;

    { Summary:
        Returns True if BB contains V. }
    class function Contains(const BB: TCPBB; const V: TCPVect): TCPBool; overload; inline; static;

    { Summary:
        Returns True if this bounding box contains V. }
    function Contains(const V: TCPVect): TCPBool; overload; inline;

    { Summary:
        Return the minimal bounding box that contains both A and B. }
    class function Merge(const A, B: TCPBB): TCPBB; overload; inline; static;

    { Summary:
        Return the minimal bounding box that contains both this bounding box
        and Other. }
    function Merge(const Other: TCPBB): TCPBB; overload; inline;

    { Summary:
        Return the minimal bounding box that contains both BB and V. }
    class function Expand(const BB: TCPBB; const V: TCPVect): TCPBB; overload; inline; static;

    { Summary:
        Return the minimal bounding box that contains both this bounding box
        and V. }
    function Expand(const V: TCPVect): TCPBB; overload; inline;

    { Summary:
        Returns a copy of V clamped to the bounding box. }
    class function ClampVect(const BB: TCPBB; const V: TCPVect): TCPVect; overload; inline; static;

    { Summary:
        Returns a copy of V clamped to the bounding box. }
    function ClampVect(const V: TCPVect): TCPVect; overload; inline;

    { Summary:
        Returns a copy of V wrapped to the bounding box. }
    class function WrapVect(const BB: TCPBB; const V: TCPVect): TCPVect; overload; inline; static;

    { Summary:
        Returns a copy of V wrapped to the bounding box. }
    function WrapVect(const V: TCPVect): TCPVect; overload; inline;

    { Summary:
        Width of the bounding box. }
    property Width: TCPFloat read GetWidth;

    { Summary:
        Height of the bounding box. }
    property Height: TCPFloat read GetHeight;
  end;

{ Summary:
    Convenience constructor for creating new TCPBB records. }
function CPBBNew(const L, B, R, T: TCPFloat): TCPBB; inline;
{$ENDREGION 'Chipmunk Bounding Boxes'}

{$REGION 'Chipmunk Rigid Bodies'}
type
  TCPBody = class;

  { Summary:
      Event type for <link TCPBody.OnUpdateVelocity> }
  TCPBodyVelocityEvent = procedure(const Body: TCPBody; const Gravity: TCPVect;
    const Damping: TCPFloat; const DT: TCPFloat) of Object;

  { Summary:
      Event type for <link TCPBody.OnUpdatePosition> }
  TCPBodyPositionEvent = procedure(const Body: TCPBody;
    const DT: TCPFloat) of Object;

  { Summary:
      A rigid body.
    Remarks:
      * Use forces to modify the rigid bodies if possible. This will be the most
        stable.
      * Modifying a body’s velocity shouldn’t necessarily be avoided, but
        applying large changes every frame can cause strange results in the
        simulation. Experiment freely, but be warned.
      * Don’t modify a body’s position every step unless you really know what
        you are doing. Otherwise you’re likely to get the position/velocity
        badly out of sync. }
  TCPBody = class
  {$REGION 'Internal Declarations'}
  private
    FSettings: cpBody;
    FHandle: PcpBody;
    FUserData: Pointer;
    FOnUpdateVelocity: TCPBodyVelocityEvent;
    FOnUpdatePosition: TCPBodyPositionEvent;
    procedure SetOnUpdatePosition(const Value: TCPBodyPositionEvent);
    procedure SetOnUpdateVelocity(const Value: TCPBodyVelocityEvent);
    procedure SetMass(const Value: TCPFloat); inline;
    procedure SetMoment(const Value: TCPFloat); inline;
    function GetPosition: TCPVect; inline;
    procedure SetPosition(const Value: TCPVect); inline;
    function GetVelocity: TCPVect; inline;
    procedure SetVelocity(const Value: TCPVect); inline;
    function GetForce: TCPVect; inline;
    procedure SetForce(const Value: TCPVect); inline;
    function GetAngle: TCPFloat; inline;
    procedure SetAngle(const Value: TCPFloat); inline;
    function GetRotation: TCPVect; inline;
    function GetRotationalVelocity: TCPFloat; inline;
    procedure SetRotationalVelocity(const Value: TCPFloat); inline;
    function GetVelocityBias: TCPVect;
    procedure SetVelocityBias(const Value: TCPVect);
  {$WARNINGS OFF}
  private
    constructor CreateStatic(const Body: cpBody);
  {$WARNINGS ON}
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a new rigid body.
      Parameters:
        Mass: the mass of the body.
        Moment: the moment of intertia.
      Remarks:
        Guessing the mass for a body is usually fine, but guessing a moment of
        inertia can lead to a very poor simulation. See the MomentForXxx helper
        functions to approximate the moment of inertia for your body,
        adding results together if you want to use more than one. }
    constructor Create(const Mass, Moment: TCPFloat);
    destructor Destroy; override;

    { Summary:
        Helper function to calculate the moment of inertia for a hollow circle.
        R1 and R2 are the inner and outer diameters in no particular order.
        (A solid circle has an inner diameter of 0).
      Remarks:
        You can add the value of multiple MomentForXxx values together if your
        body contains multiple shapes. }
    class function MomentForCircle(const Mass, R1, R2: TCPFloat;
      const Offset: TCPVect): TCPFloat; inline; static;

    { Summary:
        Helper function to calculate the moment of inertia for a line segment.
        The endpoints A and B are relative to the body.
      Remarks:
        You can add the value of multiple MomentForXxx values together if your
        body contains multiple shapes. }
    class function MomentForSegment(const Mass: TCPFloat;
      const A, B: TCPVect): TCPFloat; inline; static;

    { Summary:
        Helper function to calculate the moment of inertia for a solid polygon
        shape.
      Remarks:
        You can add the value of multiple MomentForXxx values together if your
        body contains multiple shapes. }
    class function MomentForPolygon(const Mass: TCPFloat;
      const Vertices: array of TCPVect; const Offset: TCPVect): TCPFloat; static;

    { Summary:
        Helper function to calculate the moment of inertia for a solid box
        centered on the body.
      Remarks:
        You can add the value of multiple MomentForXxx values together if your
        body contains multiple shapes. }
    class function MomentForBox(const Mass, Width, Height: TCPFloat): TCPFloat; inline; static;

    { Summary:
        Modify the velocity of the body so that it will move to the specified
        absolute coordinates in the next timestep. Intended for objects that are
        moved manually with a custom velocity integration function. }
    procedure Slew(const Pos: TCPVect; const DT: TCPFloat); inline;

    { Summary:
        Convert from body local coordinates to world space coordinates. }
    function LocalToWorld(const V: TCPVect): TCPVect; inline;

    { Summary:
        Convert from world space coordinates to body local coordinates. }
    function WorldToLocal(const V: TCPVect): TCPVect; inline;

    { Summary:
        Apply the Impulse to body at a RelativeOffset from the center of
        gravity. Both RelativeOffset and Impulse are in world coordinates.
        RelativeOffset is relative to the position of the body, but not the
        rotation. Many people get tripped up by this. }
    procedure ApplyImpulse(const Impulse, RelativeOffset: TCPVect); inline;

    { Summary:
        Zero both the forces and torques accumulated on body. }
    procedure ResetForces; inline;

    { Summary:
        Apply (accumulate) the Force on body at a RelativeOffset (important!)
        from the center of gravity. Both RelativeOffset and Force are in world
        coordinates. }
    procedure ApplyForce(const Force, RelativeOffset: TCPVect); inline;

    { Summary:
        Returns True if body is sleeping.
      Remarks:
        See <link TCPSpace> for more information on Chipmunk’s sleeping
        feature. }
    function IsSleeping: TCPBool; inline;

    { Summary:
        Force body to sleep immediately. Make sure the body is fully set up
        before you call this. Adding this body or any shapes attached to it, or
        modifying any of the body’s properties will reactivate it. This is
        useful if you want an object to be inactive until something hits it
        such as a pile of boxes you want the player to plow through or a
        stalactite hanging from a cave ceiling.
      Remarks:
        See <link TCPSpace> for more information on Chipmunk’s sleeping
        feature. }
    procedure Sleep; inline;

    { Summary:
        Wake body up so that it starts actively simulating again if it’s
        sleeping, or reset the idle timer if it’s active.
      Remarks:
        When changing any of a body’s properties, you should also call this
        method to make sure that it is not stuck sleeping when you’ve changed a
        property that should make it move again.
        See <link TCPSpace> for more information on Chipmunk’s sleeping
        feature. }
    procedure Activate; inline;

    { Summary:
        Returns True if body is a static body for a space. }
    function IsStatic: TCPBool; inline;

    { Summary:
        Returns True if body has never been added to a space. }
    function IsRogue: TCPBool; inline;

    { Summary:
        The default method for integrating the body's velocity. You usually
        don't need call this method unless you set the <link OnUpdateVelocity>
        event and you want to start with a default calculation.
      Remarks:
        Updates the velocity of the body using Euler integration. }
    procedure UpdateVelocity(const Gravity: TCPVect;
      const Damping: TCPFloat; const DT: TCPFloat); inline;

    { Summary:
        The default method for integrating the body's position. You usually
        don't need call this method unless you set the <link OnUpdatePosition>
        event and you want to start with a default calculation. This method
        may also be useful to manually update the position of static bodies.
      Remarks:
        Updates the position of the body using Euler integration. }
    procedure UpdatePosition(const DT: TCPFloat); inline;

    { Summary:
        Mass of the body.
      Remarks:
        You should call <link Activate> when you change this property. }
    property Mass: TCPFloat read FSettings.M write SetMass;

    { Summary:
        Inverse mass of the body. }
    property MassInv: TCPFloat read FSettings.M_Inv;

    { Summary:
        Moment of inertia (MoI or sometimes just moment) of the body.
        The moment is like the rotational mass of a body.
      Remarks:
        See MomentForXxx methods to help calculate the moment.
        You should call <link Activate> when you change this property. }
    property Moment: TCPFloat read FSettings.I write SetMoment;

    { Summary:
        Inverse moment of intertia. }
    property MomentInv: TCPFloat read FSettings.I_Inv;

    { Summary:
        Position of the body.
      Remarks:
        You should call <link Activate> when you change this property. }
    property Position: TCPVect read GetPosition write SetPosition;

    { Summary:
        Velocity of the body.
      Remarks:
        You should call <link Activate> when you change this property. }
    property Velocity: TCPVect read GetVelocity write SetVelocity;

    { Summary:
        Maximum speed a body may have after updating it’s velocity.
      Remarks:
        You should call <link Activate> when you change this property. }
    property VelocityMax: TCPFloat read FSettings.V_Limit write FSettings.V_Limit;

    { Summary:
        Velocity bias used when solving penetrations and correcting
        constraints. }
    property VelocityBias: TCPVect read GetVelocityBias write SetVelocityBias;

    { Summary:
        Current force being applied to the body.
      Remarks:
        You should call <link Activate> when you change this property. }
    property Force: TCPVect read GetForce write SetForce;

    { Summary:
        Current rotation angle of the body in degrees.
      Remarks:
        You should call <link Activate> when you change this property. }
    property Angle: TCPFloat read GetAngle write SetAngle;

    { Summary:
        Cached unit length vector representing the angle of the body.
        Used for fast vector rotation using <link TCPVect.Rotate>. }
    property Rotation: TCPVect read GetRotation;

    { Summary:
        Current rotational velocity of the body in degrees per second.
      Remarks:
        You should call <link Activate> when you change this property. }
    property RotationalVelocity: TCPFloat read GetRotationalVelocity write SetRotationalVelocity;

    { Summary:
        Maximum rotational speed a body may have after updating it’s velocity.
      Remarks:
        You should call <link Activate> when you change this property. }
    property RotationalVelocityMax: TCPFloat read FSettings.W_Limit write FSettings.W_Limit;

    { Summary:
        Rotational velocity bias used when solving penetrations and correcting
        constraints. }
    property RotationalVelocityBias: TCPFloat read FSettings.W_Bias write FSettings.W_Bias;

    { Summary:
        Current torque being applied to the body.
      Remarks:
        You should call <link Activate> when you change this property. }
    property Torque: TCPFloat read FSettings.T write FSettings.T;

    { Summary:
        A user definable data pointer. If you set this to point at the game
        object the shape is for, then you can access your game object from
        Chipmunk callbacks. }
    property UserData: Pointer read FUserData write FUserData;

    { Summary:
        Internal handle used for the body.
      Remarks:
        You should only use the property if you need access to the lower-level
        Chipmunk library directly. }
    property Handle: PcpBody read FHandle;

    { Summary:
        Event that is fired to integrate the body's velocity.
      Remarks:
        If set to nil, the <link UpdateVelocity> method is used. }
    property OnUpdateVelocity: TCPBodyVelocityEvent read FOnUpdateVelocity write SetOnUpdateVelocity;

    { Summary:
        Event that is fired to integrate the body's position.
      Remarks:
        If set to nil, the <link UpdatePosition> method is used.
        Unlike the <link OnUpdateVelocity> event, it’s unlikely you’ll want to
        set this event. If you do, make sure you understand it’s source code as
        it’s an important part of the collision/joint correction process. }
    property OnUpdatePosition: TCPBodyPositionEvent read FOnUpdatePosition write SetOnUpdatePosition;
  end;
{$ENDREGION 'Chipmunk Rigid Bodies'}

{$REGION 'Internal types'}
type
  { Summary:
      A cpCircleShape without the first "shape" field. }
  _cpCircleShape = record
    // Shape: cpShape;

    // Center in body space coordinates
    C: TCPVect;

    // Radius.
    R: TCPFloat;

    // Transformed center. (world space coordinates)
    TC: TCPVect;
  end;

type
  { Summary:
      A cpSegmentShape without the first "shape" field. }
  _cpSegmentShape = record
    // Shape: cpShape;

    // Endpoints and normal of the segment. (body space coordinates)
    A, B, N: TCPVect;

    // Radius of the segment. (Thickness)
    R: TCPFloat;

    // Transformed endpoints and normal. (world space coordinates)
    TA, TB, TN: TCPVect;
  end;

type
  { Summary:
      A cpPolyShape without the first "shape" field. }
  _cpPolyShape = record
    // Shape: cpShape;

    // Vertex and axis lists.
    NumVerts: Integer;
    Verts: PcpVect;
    Axes: PcpPolyShapeAxis;

    // Transformed vertex and axis lists.
    TVerts: PcpVect;
    TAxes: PcpPolyShapeAxis;
  end;

type
  { Summary:
     A cpPinJoint without the first "constraint" field. }
  _cpPinJoint = record
    // Constraint: cpConstraint;
    Anchr1, Anchr2: TCPVect;
    Dist: TCPFloat;

    R1, R2: TCPVect;
    N: TCPVect;
    NMass: TCPFloat;

    JNAcc, JNMax: TCPFloat;
    Bias: TCPFloat;
  end;

type
  { Summary:
     A cpSlideJoint without the first "constraint" field. }
  _cpSlideJoint = record
    // Constraint: cpConstraint;
    Anchr1, Anchr2: TCPVect;
    Min, Max: TCPFloat;

    R1, R2: TCPVect;
    N: TCPVect;
    NMass: TCPFloat;

    JNAcc, JNMax: TCPFloat;
    Bias: TCPFloat;
  end;

type
  { Summary:
     A cpPivotJoint without the first "constraint" field. }
  _cpPivotJoint = record
    // Constraint: cpConstraint;
    Anchr1, Anchr2: TCPVect;

    R1, R2: TCPVect;
    K1, K2: TCPVect;

    JAcc: TCPVect;
    JMaxLen: TCPFloat;
    Bias: TCPVect;
  end;

type
  { Summary:
     A cpGrooveJoint without the first "constraint" field. }
  _cpGrooveJoint = record
    // Constraint: cpConstraint;
    Grv_N, Grv_A, Grv_B: TCPVect;
    Anchr2: TCPVect;

    Grv_Tn: TCPVect;
    Clamp: TCPFloat;
    R1, R2: TCPVect;
    K1, K2: TCPVect;

    JAcc: TCPVect;
    JMaxLen: TCPFloat;
    Bias: TCPVect;
  end;

type
  { Summary:
     A cpDampedSpring without the first "constraint" field. }
  _cpDampedSpring = record
    // Constraint: cpConstraint;
    Anchr1, Anchr2: TCPVect;
    RestLength: TCPFloat;
    Stiffness: TCPFloat;
    Damping: TCPFloat;
    SpringForceFunc: cpDampedSpringForceFunc;

    Target_Vrn: TCPFloat;
    V_Coef: TCPFloat;

    R1, R2: TCPVect;
    NMass: TCPFloat;
    N: TCPVect;
  end;

type
  { Summary:
     A cpDampedRotarySpring without the first "constraint" field. }
  _cpDampedRotarySpring = record
    // Constraint: cpConstraint;
    RestAngle: TCPFloat;
    Stiffness: TCPFloat;
    Damping: TCPFloat;
    SpringTorqueFunc: cpDampedRotarySpringTorqueFunc;

    Target_Wrn: TCPFloat;
    W_Coef: TCPFloat;

    ISum: TCPFloat;
  end;

type
  { Summary:
     A cpRotaryLimitJoint without the first "constraint" field. }
  _cpRotaryLimitJoint = record
    // Constraint: cpConstraint;
    Min, Max: TCPFloat;

    ISum: TCPFloat;

    Bias: TCPFloat;
    JAcc, JMax: TCPFloat;
  end;

type
  { Summary:
     A cpRatchetJoint without the first "constraint" field. }
  _cpRatchetJoint = record
    // Constraint: cpConstraint;
    Angle, Phase, Ratchet: TCPFloat;

    ISum: TCPFloat;

    Bias: TCPFloat;
    JAcc, JMax: TCPFloat;
  end;

type
  { Summary:
     A cpGearJoint without the first "constraint" field. }
  _cpGearJoint = record
    // Constraint: cpConstraint;
    Phase, Ratio: TCPFloat;
    Ratio_Inv: TCPFloat;

    ISum: TCPFloat;

    Bias: TCPFloat;
    JAcc, JMax: TCPFloat;
  end;

type
  { Summary:
     A cpSimpleMotor without the first "constraint" field. }
  _cpSimpleMotor = record
    // Constraint: cpConstraint;
    Rate: TCPFloat;

    ISum: TCPFloat;

    JAcc, JMax: TCPFloat;
  end;
{$ENDREGION 'Internal types'}

{$REGION 'Chipmunk Collision Shapes'}
type
  TCPShape = class;

  { Summary:
      Shape types for <link TCPShape.ShapeType>. }
  TCPShapeType = (
    { Summary:
        Circle shape.
        You can safely typecast the shape to a <link TCPCircleShape>. }
    stCircle,

    { Summary:
        Line segment shape.
        You can safely typecast the shape to a <link TCPSegmentShape>. }
    stSegment,

    { Summary:
        Convex polygon shape.
        You can safely typecast the shape to a <link TCPPolyShape>. }
    stPoly);

  { Summary:
      Contains hit test information about a segment query (see
      <link TCPShape.SegmentQuery> and <link TCPSpace.SegmentQuery> }
  TCPSegmentQueryInfo = record
  public
    { Summary:
        Shape that was hit, or nil if there was no collision. }
    Shape: TCPShape;

    { Summary:
        Distance along query segment, will always be in the range 0..1 (as
        a percentage along the segment). }
    Distance: TCPFloat;

    { Summary:
        Normal of hit surface. }
    Normal: TCPVect;
  public
    { Summary:
        Return the hit point in world coordinates where the segment first
        intersected with the shape. }
    function HitPoint(const A, B: TCPVect): TCPVect; inline;

    { Summary:
        Return the absolute distance where the segment first hit the shape. }
    function HitDist(const A, B: TCPVect): TCPFloat; inline;
  end;
  PCPSegmentQueryInfo = ^TCPSegmentQueryInfo;

  { Summary:
      Abstract base class for collision shapes.
    Remarks:
      There are currently 3 collision shape types:
      * Circles (<link TCPCircleShape>): Fastest and simplest collision shape.
      * Line segments (<link TCPSegmentShape>): Meant mainly as a static shape.
        They can be attached to moving bodies, but they don’t currently generate
        collisions with other line segments. Can be beveled in order to give
        them a thickness.
      * Convex polygons (<link TCPPolyShape>): Slowest, but most flexible
        collision shape.

      # Notes #
      * You can attach multiple collision shapes to a rigid body. This should
        allow you to create almost any shape you could possibly need.
      * Shapes attached to the same rigid body will never generate collisions.
        You don’t have to worry about overlap when attaching multiple shapes to
        a rigid body.
      * The amount of elasticity applied during a collision is determined by
        multiplying the elasticity of both shapes together. The same is done for
        determining the friction. If you want to override this default behavior,
        you can do so inside of a PreSolve event.
      * Make sure you add both the body and it’s collision shapes to a space.
        The exception is when you want to have a static body or a body that you
        integrate yourself. In that case, only add the shape.

      # Filtering Collisions #
      Chipmunk has two primary means of ignoring collisions: groups and layers.

      Groups are meant to ignore collisions between parts on a complex object.
      A ragdoll is a good example. When jointing an arm onto the torso, you’ll
      want them to allow them to overlap. Groups allow you to do exactly that.
      Shapes that have the same group don’t generate collisions. So by placing
      all of the shapes in a ragdoll in the same group, you’ll prevent it from
      colliding against other parts of itself.

      Layers allow you to separate collision shapes into mutually exclusive
      planes. Shapes can be in more than one layer, and shapes only collide with
      other shapes that are in at least one of the same layers. As a simple
      example, say shape A is in layer 1, shape B is in layer 2, and shape C is
      in layer 1 and 2. Shape A and B won’t collide with each other, but shape C
      will collide with both A and B.

      Layers can also be used to set up rule based collisions. Say you have four
      types of shapes in your game. The player, the enemies, player bullets and
      enemy bullets. The are that the player should collide with enemies, and
      bullets shouldn’t collide with the type (player or enemy) that fired them.
      Making a chart would look like this:

      <table>
        X               Player   Enemy   Player Bullet   Enemy Bullet
        -------------   ------   -----   -------------   ------------
        Player          -        (1)                     (2)
        Enemy           -        -       (3)
        Player Bullet   -        -       -
        Enemy Bullet    -        -       -               -
      </table>

      The ‘-’s are for redundant spots in the chart, and the numbers are spots
      where types should collide. You can use a layer for rule that you want to
      define. Then add the layers to each type: The player should be in layers
      1 and 2, the enemy should be in layers 1 and 3, the player bullets should
      be in layer 3, and the enemy bullets should be in layer 2. Treating layers
      as rules this way, you can define up to 32 rules. The <link TCPLayers>
      type is unsigned int which has a resolution of 32 bits on most systems.

      There is one last way of filtering collisions using collision handlers.
      While collision handlers can be more flexible, they are also the slowest
      method. So you try to use groups or layers first.

      # Queries #
      Chipmunk spaces currently support three kinds of spatial queries, point,
      segment and bounding box. Any type can be done efficiently against an
      entire space (see <link TCPSpace>, or against individual shapes.

      Point queries are useful for things like mouse picking and simple sensors.
      See <link PointQuery> or <link PointQuery>.

      Segment queries are like ray casting, but because Chipmunk uses a spatial
      hash to process collisions, it cannot process infinitely long queries like
      a ray. In practice this is still very fast and you don’t need to worry too
      much about the performance as long as you aren’t using extremely long
      segments for your queries. See <link SegmentQuery>.

      See also the helper functions <link TCPSegmentQueryInfo.HitPoint> and
      <link TCPSegmentQueryInfo.HitDist> }
  TCPShape = class abstract
  {$REGION 'Internal Declarations'}
  private
    FHandle: PcpShape;
    FBody: TCPBody;
    FUserData: Pointer;
    FSettings: cpShape; { This MUST be the LAST field in the class }
    function GetBoundingBox: TCPBB; inline;
    function GetSurfaceVelocity: TCPVect; inline;
    procedure SetSurfaceVelocity(const Value: TCPVect); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a new collision shape.
      Parameters:
        Body: the body to attach the circle to. }
    constructor Create(const Body: TCPBody);
    destructor Destroy; override;

    { Summary:
        The type of shape (circle, line segment, or convex polygon). Depending
        on its value, you can safely typecast this shape to <link TCPCircleShape>,
        <link TCPSegmentShape> or <link TCPPolyShape>. }
    class function ShapeType: TCPShapeType; virtual; abstract;

    { Summary:
        Updates and returns the bounding box of shape. }
    function CacheBoundingBox: TCPBB;

    { Summary:
        Chipmunk keeps a counter so that every new shape is given a unique hash
        value to be used in the spatial hash. Because this affects the order in
        which the collisions are found and handled, you can reset the shape
        counter every time you populate a space with new shapes. If you don’t,
        there might be (very) slight differences in the simulation. }
    class procedure ResetIdCounter; inline; static;

    { Summary:
        Check if the given point lies within the shape.
      Remarks:
        Point queries are useful for things like mouse picking and simple
        sensors. }
    function PointQuery(const P: TCPVect): TCPBool; inline;

    { Summary:
        Check if the line segment from A to B intersects the shape. Returns
        the hit details in Info.
      Remarks:
        Segment queries return more information than just a simple yes or no,
        they also return where a shape was hit and it’s surface normal at the
        hit point. t is the percentage between the query start and end points.
        If you need the hit point in world space or the absolute distance from
        start, see the segment query helper functions in
        <link TCPSegmentQueryInfo>. }
    function SegmentQuery(const A, B: TCPVect; out Info: TCPSegmentQueryInfo): TCPBool; inline;

    { Summary:
        The rigid body the shape is attached to. }
    property Body: TCPBody read FBody;

    { Summary:
        The bounding box of the shape. Only guaranteed to be valid after
        <link TCPShape.CacheBoundingBox> or <link TCPSpace.Step> is called. Moving a body
        that a shape is connected to does not update it’s bounding box. }
    property BoundingBox: TCPBB read GetBoundingBox;

    { Summary:
        A boolean value if this shape is a sensor or not. Sensors only call
        collision callbacks, and never generate real collisions. }
    property Sensor: TCPBool read FSettings.Sensor write FSettings.Sensor;

    { Summary:
        Elasticity of the shape. A value of 0.0 gives no bounce, while a value
        of 1.0 will give a “perfect” bounce. However due to inaccuracies in the
        simulation using 1.0 or greater is not recommended however.
      Remarks:
        The amount of elasticity applied during a collision is determined by
        multiplying the elasticity of both shapes together. If you want to
        override this default behavior, you can do so inside of a PreSolve
        collision event. }
    property Elasticity: TCPFloat read FSettings.E write FSettings.E;

    { Summary:
        Friction coefficient. Chipmunk uses the Coulomb friction model, a value
        of 0.0 is frictionless.
      Remarks:
        See <exref target="http://www.roymech.co.uk/Useful_Tables/Tribology/co_of_frict.htm">
        Table of friction coefficients.</exref>.
        The amount of friction applied during a collision is determined by
        multiplying the friction of both shapes together. If you want to
        override this default behavior, you can do so inside of a PreSolve
        collision event. }
    property Friction: TCPFloat read FSettings.U write FSettings.U;

    { Summary:
        The surface velocity of the object. Useful for creating conveyor belts
        or players that move around. This value is only used when calculating
        friction, not resolving the collision. }
    property SurfaceVelocity: TCPVect read GetSurfaceVelocity write SetSurfaceVelocity;

    { Summary:
        A user definable data pointer. If you set this to point at the game
        object the shapes is for, then you can access your game object from
        Chipmunk callbacks. }
    property UserData: Pointer read FUserData write FUserData;

    { Summary:
        You can assign types to Chipmunk collision shapes that trigger callbacks
        when objects of certain types touch. }
    property CollisionType: TCPCollisionType read FSettings.collision_type write FSettings.collision_type;

    { Summary:
        Shapes in the same non-zero group do not generate collisions. Useful
        when creating an object out of many shapes that you don’t want to self
        collide. Defaults to CP_NO_GROUP.
        See <link TCPShape> documentation for information about filtering
        collisions using groups and layer. }
    property Group: TCPGroup read FSettings.group write FSettings.group;

    { Summary:
        Shapes only collide if they are in the same bit-planes. i.e.
        "(A.Layers and B.Layers) <> 0" By default, a shape occupies all
        bit-planes.
        <exref target="http://en.wikipedia.org/wiki/Mask_%28computing%29#top">Wikipedia</exref>
        has a nice article on bitmasks if you are unfamiliar with how to use
        them. Defaults to CP_ALL_LAYERS.
        See <link TCPShape> documentation for information about filtering
        collisions using groups and layer. }
    property Layers: TCPLayers read FSettings.layers write FSettings.layers;

    { Summary:
        Internal handle used for the shape.
      Remarks:
        You should only use the property if you need access to the lower-level
        Chipmunk library directly. }
    property Handle: PcpShape read FHandle;
  end;

type
  { Summary:
      A circular collision shape. }
  TCPCircleShape = class(TCPShape)
  {$REGION 'Internal Declarations'}
  private
    FCircleSettings: _cpCircleShape; { This MUST be the FIRST field in the class }
  {$ENDREGION 'Internal Declarations'}
  public
    class function ShapeType: TCPShapeType; override;

    { Summary:
        Creates a new circle collision shape.
      Parameters:
        Body: the body to attach the circle to.
        Radius: the radius of the circle.
        Offset: the offset from the body’s center of gravity in body local
          coordinates. }
    constructor Create(const Body: TCPBody; const Radius: TCPFloat;
      const Offset: TCPVect);

    { Summary:
        Changes the radius of the circle.
      Remarks:
        This is an "unsafe" operation which may reduce the physical accuracy or
        numerical stability of the simulation, but will not cause crashes.
        The prime example is mutating collision shapes. Chipmunk does not
        support this directly. Mutating shapes using this API will caused
        objects in contact to be pushed apart using Chipmunk's overlap solver,
        but not using real persistent velocities. Probably not what you meant,
        but perhaps close enough. }
    procedure UnsafeSetRadius(const Radius: TCPFloat); inline;

    { Summary:
        The radius of the circle. }
    property Radius: TCPFloat read FCircleSettings.R;

    { Summary:
        The offset from the body’s center of gravity in body local
        coordinates. }
    property Center: TCPVect read FCircleSettings.C;

    { Summary:
        Transformed center in world space coordinates. }
    property TC: TCPVect read FCircleSettings.TC;
  end;

type
  { Summary:
      A line segment collision shape. }
  TCPSegmentShape = class(TCPShape)
  {$REGION 'Internal Declarations'}
  private
    FSegmentSettings: _cpSegmentShape; { This MUST be the FIRST field in the class }
  {$ENDREGION 'Internal Declarations'}
  public
    class function ShapeType: TCPShapeType; override;

    { Summary:
        Creates a new polygon collision shape.
      Parameters:
        Body: the body to attach the circle to.
        V1: first endpoint of the line segment.
        V2: second endpoint of the line segment.
        Radius: thickness of the line segment. }
    constructor Create(const Body: TCPBody; const V1, V2: TCPVect;
      const Radius: TCPFloat);

    { Summary:
        Changes the endpoints of the segment.
      Remarks:
        This is an "unsafe" operation which may reduce the physical accuracy or
        numerical stability of the simulation, but will not cause crashes.
        The prime example is mutating collision shapes. Chipmunk does not
        support this directly. Mutating shapes using this API will caused
        objects in contact to be pushed apart using Chipmunk's overlap solver,
        but not using real persistent velocities. Probably not what you meant,
        but perhaps close enough. }
    procedure UnsafeSetEndPoints(const A, B: TCPVect); inline;

    { Summary:
        First endpoint of the line segment. }
    property V1: TCPVect read FSegmentSettings.A;

    { Summary:
        Second endpoint of the line segment. }
    property V2: TCPVect read FSegmentSettings.B;

    { Summary:
        Normal of the segment. }
    property Normal: TCPVect read FSegmentSettings.N;

    { Summary:
        Thickness of the line segment. }
    property Radius: TCPFloat read FSegmentSettings.R;

    { Summary:
        Transformed first endpoint of the line segment in world space
        coordinates. }
    property T1: TCPVect read FSegmentSettings.TA;

    { Summary:
        Transformed second endpoint of the line segment in world space
        coordinates. }
    property T2: TCPVect read FSegmentSettings.TB;

    { Summary:
        Transformed normal of the segment in world space coordinates. }
    property TN: TCPVect read FSegmentSettings.TN;
  end;

type
  { Summary:
      A polygon collision shape. }
  TCPPolyShape = class(TCPShape)
  {$REGION 'Internal Declarations'}
  private
    FPolySettings: _cpPolyShape; { This MUST be the FIRST field in the class }
    function GetVertex(const Index: Integer): TCPVect; inline;
    function GetVertexCount: Integer; inline;
    function GetTV(const Index: Integer): TCPVect; inline;
    function GetFirstTV: PCPVect; inline;
    function GetFirstVertex: PCPVect; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ShapeType: TCPShapeType; override;

    { Summary:
        Creates a new polygon segment collision shape.
      Parameters:
        Body: the body to attach the circle to.
        Verts: array of vertexes defining a convex hull with a clockwise winding.
        Offset: the offset from the body’s center of gravity in body local
          coordinates.
      Remarks:
        An assertion will be thrown the vertexes are not convex or do not have
        a clockwise winding. }
    constructor Create(const Body: TCPBody; const Verts: array of TCPVect;
      const Offset: TCPVect);

    { Summary:
        Creates a new polygon segment collision shape in the form of a box.
      Parameters:
        Body: the body to attach the circle to.
        Width: width of the box.
        Height: height of the box. }
    constructor CreateBox(const Body: TCPBody; const Width, Height: TCPFloat);

    { Summary:
        The number of vertices in the shape. }
    property VertexCount: Integer read GetVertexCount;

    { Summary:
        The vertexes in the shape in local coordinates. }
    property Vertex[const Index: Integer]: TCPVect read GetVertex; default;

    { Summary:
        Returns a pointer to the first vertex. You can iterate through all
        vertexes by incrementing this pointer. }
    property FirstVertex: PCPVect read GetFirstVertex;

    { Summary:
        The transformed vertexes in the shape in world space coordinates. }
    property TV[const Index: Integer]: TCPVect read GetTV;

    { Summary:
        Returns a pointer to the first transformed vertex. You can iterate
        through all transformed vertexes by incrementing this pointer. }
    property FirstTV: PCPVect read GetFirstTV;
  end;
{$ENDREGION 'Chipmunk Collision Shapes'}

{$REGION 'Chipmunk Collision Pairs'}
type
  { Summary:
      Data structure for contact points, as used by <link TCPArbiter>. }
  TCPContact = record
  {$REGION 'Internal Declarations'}
  private
    FSettings: cpContact;
    function GetPoint: TCPVect; inline;
    procedure SetPoint(const Value: TCPVect); inline;
    function GetNormal: TCPVect; inline;
    procedure SetNormal(const Value: TCPVect); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Contact point. }
    property Point: TCPVect read GetPoint write SetPoint;

    { Summary:
        Normal. }
    property Normal: TCPVect read GetNormal write SetNormal;

    { Summary:
        Penetration distance. }
    property Distance: TCPFloat read FSettings.Dist write FSettings.Dist;
  end;
  PCPContact = ^TCPContact;

type
  { Summary:
      Data structure for tracking collisions between shapes.
    Remarks:
      First of all, why are they called arbiters? The short answer is that Box2D
      called them that way back in 2006 when I was looking at the source for
      it’s solver. An arbiter is like a judge, a person that has authority to
      settle disputes between two people. It was a fun, fitting name and was
      shorter to type than CollisionPair which I had been using. :p

      Originally arbiters were going to be an internal data type that Chipmunk
      used that wouldn’t ever be sent to outside code. In Chipmunk 4.x and
      earlier, only a single callback hook was provided for handling collision
      events. It was triggered every step that to shapes were touching. This
      made it non-trivial to track when objects started and stopped touching as
      the user would have to record and process this themselves. Many people,
      including myself, wanted to get collision begin/separate events and
      eventually I realized that information was already being stored in the
      arbiter cache that the Chipmunk maintains. With some changes to the
      collision callback API, that information is now exposed to the user.
      Internally, arbiters are used primarily for solving collision impulses.
      To external users, you can simply think of them as a weird type used
      in collision callbacks. }
  TCPArbiter = record
  {$REGION 'Internal Declarations'}
  private
    FSettings: cpArbiter;
    function GetContactPoint(const Index: Integer): PCPContact; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Get the shapes in the order that they were defined in the collision
        handler associated with this arbiter. If you defined the handler as
        <link TCPSpace.AddCollisionHandler>(1, 2, ...), you you will find that
        A.CollisionType = 1 and B.CollisionType = 2. }
    procedure GetShapes(out A, B: TCPShape); inline;

    { Summary:
        Returns True if this is the first step that the shapes touched. You can
        use this from PreSolve and PostSolve event to know if a collision
        between two shapes is new without needing to flag a boolean in your
        begin callback. }
    function IsFirstContact: Boolean; inline;

    { Summary:
        Returns the collision normal for the I’th contact point, flipping it if
        necessary.
      Remarks:
        Currently due to how Chipmunk’s collision detection is implemented, the
        collision normals will be the same for all collision points. You can
        simply do GetNormal(0) and not have to check each contact point.
        Calling this function from the Separate event is undefined. }
    function GetNormal(const I: Integer = 0): TCPVect; inline;

    { Summary:
        Returns the position of the I’th collision point.
      Remarks:
        Calling this function from the Separate event is undefined. }
    function GetPoint(const I: Integer): TCPVect; inline;

    { Summary:
        Returns the impulse that was applied this step to resolve the collision.
        This method should only be called from a PostStep event, otherwise the
        result is undefined.
      Remarks:
        If you are using the deprecated elastic iterations setting on
        your space, it will cause you to get incorrect results. Elastic
        iterations should no longer be needed, and you should be able to safely
        turn them off. }
    function TotalImpulse: TCPVect; inline;

    { Summary:
        Returns the impulse with friction that was applied this step to resolve
        the collision. This method should only be called from a PostStep event,
        otherwise the result is undefined.
      Remarks:
        If you are using the deprecated elastic iterations setting on
        your space, it will cause you to get incorrect results. Elastic
        iterations should no longer be needed, and you should be able to safely
        turn them off. }
    function TotalImpulseWithFriction: TCPVect; inline;

    { Summary:
        Causes a collision pair to be ignored as if you returned False from a
        begin callback. }
    procedure Ignore; inline;

    { Summary:
        Number of contact points for this collision. }
    property ContactCount: Integer read FSettings.NumContacts;

    { Summary:
        The contact points. }
    property ContactPoints[const Index: Integer]: PCPContact read GetContactPoint;

    { Summary:
        Calculated amount of elasticity to apply for this collision. Can be
        overriden from a PreSolve event. }
    property Elasticity: TCPFloat read FSettings.E write FSettings.E;

    { Summary:
        Calculated amount of friction to apply for this collision. Can be
        overriden from a PreSolve event. }
    property Friction: TCPFloat read FSettings.U write FSettings.U;
  end;
  PCPArbiter = ^TCPArbiter;
{$ENDREGION 'Chipmunk Collision Pairs'}

{$REGION 'Chipmunk Constraints'}
  { Summary:
      Constraint types for <link TCPConstraint.ConstraintType>. }
  TCPConstraintType = (
    { Summary:
        Pin Joint constraint.
        You can safely typecast the shape to a <link TCPPinJoint>. }
    ctPinJoint,

    { Summary:
        Slide Joint constraint.
        You can safely typecast the shape to a <link TCPSlideJoint>. }
    ctSlideJoint,

    { Summary:
        Pivot Joint constraint.
        You can safely typecast the shape to a <link TCPPivotJoint>. }
    ctPivotJoint,

    { Summary:
        Groove Joint constraint.
        You can safely typecast the shape to a <link TCPGrooveJoint>. }
    ctGrooveJoint,

    { Summary:
        Damped Spring constraint.
        You can safely typecast the shape to a <link TCPDampedSpring>. }
    ctDampedSpring,

    { Summary:
        Damped Rotary Spring constraint.
        You can safely typecast the shape to a <link TCPDampedRotarySpring>. }
    ctDampedRotarySpring,

    { Summary:
        Rotary Limit Joint constraint.
        You can safely typecast the shape to a <link TCPRotaryLimitJoint>. }
    ctRotaryLimitJoint,

    { Summary:
        Ratchet Joint constraint.
        You can safely typecast the shape to a <link TCPRatchetJoint>. }
    ctRatchetJoint,

    { Summary:
        Gear Joint constraint.
        You can safely typecast the shape to a <link TCPGearJoint>. }
    ctGearJoint,

    { Summary:
        Simple Motor constraint.
        You can safely typecast the shape to a <link TCPSimpleMotor>. }
    ctSimpleMotor);

type
  { Summary:
      A constraint is something that describes how two bodies interact with each
      other (how they constrain each other). Constraints can be simple joints
      that allow bodies to pivot around each other like the bones in your body,
      or they can be more abstract like the gear joint or motors.
    Remarks:
      # What constraints are and what they are not #
      Constraints in Chipmunk are all velocity based constraints. This means
      that they act primarily by synchronizing the velocity of two bodies. A
      pivot joint holds two anchor points on two separate bodies together by
      defining equations that say that the velocity of the anchor points must
      be the same and calculating impulses to apply to the bodies to try and
      keep it that way. A constraint takes a velocity as it’s primary input
      and produces a velocity change as it’s output. Some constraints,
      (joints in particular) apply velocity changes to correct differences in
      positions. More about this in the next section.

      A spring connected between two bodies is not a constraint. It’s very
      constraint-like as it creates forces that affect the velocities of the
      two bodies, but a spring takes distances as input and produces forces as
      it’s output. If a spring is not a constraint, then why do I have two
      varieties of spring constraints you ask? The reason is because they are
      damped springs. The damping associated with the spring is a true
      constraint that creates velocity changes based on the relative velocities
      of the two bodies it links. As it is convenient to put a damper and a
      spring together most of the time, I figured I might as well just apply
      the spring force as part of the constraint instead of having a damper
      constraint and having the user calculate and apply their own spring
      forces separately.

      # Error correction by Feedback #
      Joints in Chipmunk are not perfect. A pin joint can’t maintain the exact
      correct distance between it’s anchor points, nor can a pivot joint hold
      it’s anchor points completely together. Instead, they are designed to deal
      with this by correcting themselves over time. In Chipmunk 5, you have a
      fair amount of extra control over how joints correct themselves and can
      even use this ability to create physical effects that allow you to use
      joints in unique ways:

      * Servo motors – Ex: open/close doors or rotate things without going over
        a maximum force.
      * Winches – Pull one object towards another at a constant speed without
        going over a maximum force.
      * Mouse manipulation – Interact with objects smoothly given coarse/shaky
        mouse input.

      There are three properties that control the error correction,
      <link MaxForce>, <link MaxBias> and <link BiasCoef>. <link MaxForce> is
      pretty self explanatory, a joint or constraint will not be able to use
      more than this amount of force in order to function. If it needs more
      force to be able to hold itself together, it will fall apart.
      <link MaxBias> is the maximum speed at which error correction can be
      applied. If you change a property on a joint so that the joint will have
      to correct itself, it normally does so very quickly. By setting a
      max speed you can make the joint work like a servo, correcting itself at
      a constant rate over a longer period of time. Lastly, <link BiasCoef> is
      the percentage of error corrected every step before clamping to a maximum
      speed. You can use this to make joints correct themselves smoothly
      instead of at a constant speed, but is probably the least useful of the
      three properties by far.

      # Constraints and Collision Shapes #
      Neither constraints or collision shapes have any knowledge of the other.
      When connecting joints to a body the anchor points don’t need to be
      inside of any shapes attached to the body and it often makes sense that
      they shouldn’t. Also, adding a constraint between two bodies doesn’t
      prevent their collision shapes from colliding. In fact, this is the
      primary reason that the collision group property exists.

      # Constraint Types #
      Chipmunk supports the following constraint types:
      * <link TCPPinJoint>
      * <link TCPSlideJoint>
      * <link TCPPivotJoint>
      * <link TCPGrooveJoint>
      * <link TCPDampedSpring>
      * <link TCPDampedRotarySpring>
      * <link TCPRotaryLimitJoint>
      * <link TCPRatchetJoint>
      * <link TCPGearJoint>
      * <link TCPSimpleMotor>

      # Notes #
      You can add multiple joints between two bodies, but make sure that they
      don’t fight. Doing so can cause the bodies jitter or spin violently. }
  TCPConstraint = class abstract
  {$REGION 'Internal Declarations'}
  private
    FHandle: PcpConstraint;
    FA: TCPBody;
    FB: TCPBody;
    FSettings: cpConstraint; { This MUST be the LAST field in the class }
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a new constraint.
      Parameters:
        A: The first body that the constraint acts on.
        B: The second body that the constraint acts on. }
    constructor Create(const A, B: TCPBody);
    destructor Destroy; override;

    { Summary:
        The type of constraint. Depending on its value, you can safely typecast
        this constraint to a specific constraint class. }
    class function ConstraintType: TCPConstraintType; virtual; abstract;

    { Summary:
        The first body that the constraint acts on. }
    property A: TCPBody read FA;

    { Summary:
        The second body that the constraint acts on. }
    property B: TCPBody read FB;

    { Summary:
        The maximum force that the constraint can use to act on the two bodies.
        Defaults to INFINITY. }
    property MaxForce: TCPFloat read FSettings.MaxForce write FSettings.MaxForce;

    { Summary:
        The percentage of error corrected each step of the space. (Can cause
        issues if you don’t use a constant time step). Defaults to 0.1. }
    property BiasCoef: TCPFloat read FSettings.BiasCoef write FSettings.BiasCoef;

    { Summary:
        The maximum speed at which the constraint can apply error correction.
        Defaults to INFINITY. }
    property MaxBias: TCPFloat read FSettings.MaxBias write FSettings.MaxBias;

    { Summary:
        Internal handle used for the constraint.
      Remarks:
        You should only use the property if you need access to the lower-level
        Chipmunk library directly. }
    property Handle: PcpConstraint read FHandle;
  end;

type
  { Summary:
      A Pin Joint constraint. }
  TCPPinJoint = class(TCPConstraint)
  {$REGION 'Internal Declarations'}
  private
    FPinJointSettings: _cpPinJoint; { This MUST be the FIRST field in the class }
  {$ENDREGION 'Internal Declarations'}
  public
    class function ConstraintType: TCPConstraintType; override;

    { Summary:
        Creates a new pin joint.
      Parameters:
        A: The first body that the constraint acts on.
        B: The second body that the constraint acts on.
        Anchor1: The anchor point of the first body.
        Anchor2: The anchor point of the second body.
      Remarks:
        The distance between the two anchor points is measured when the joint
        is created. The distance between the two anchor points is measured when
        the joint is created. If you want to set a specific distance, use the
        property to override it.}
    constructor Create(const A, B: TCPBody; const Anchor1, Anchor2: TCPVect);

    { Summary:
        The anchor point of the first body. }
    property Anchor1: TCPVect read FPinJointSettings.Anchr1 write FPinJointSettings.Anchr1;

    { Summary:
        The anchor point of the second body. }
    property Anchor2: TCPVect read FPinJointSettings.Anchr2 write FPinJointSettings.Anchr2;

    { Summary:
        The distance between the two anchor points. }
    property Distance: TCPFloat read FPinJointSettings.Dist write FPinJointSettings.Dist;
  end;

type
  { Summary:
      A Slide Joint constraint. }
  TCPSlideJoint = class(TCPConstraint)
  {$REGION 'Internal Declarations'}
  private
    FSlideJointSettings: _cpSlideJoint; { This MUST be the FIRST field in the class }
  {$ENDREGION 'Internal Declarations'}
  public
    class function ConstraintType: TCPConstraintType; override;

    { Summary:
        Creates a new slide joint.
      Parameters:
        A: The first body that the constraint acts on.
        B: The second body that the constraint acts on.
        Anchor1: The anchor point of the first body.
        Anchor2: The anchor point of the second body.
        Min: Minimum allowed distance of anchor points.
        Max: Maximum allowed distance of anchor points. }
    constructor Create(const A, B: TCPBody; const Anchor1, Anchor2: TCPVect;
      const Min, Max: TCPFloat);

    { Summary:
        The anchor point of the first body. }
    property Anchor1: TCPVect read FSlideJointSettings.Anchr1 write FSlideJointSettings.Anchr1;

    { Summary:
        The anchor point of the second body. }
    property Anchor2: TCPVect read FSlideJointSettings.Anchr2 write FSlideJointSettings.Anchr2;

    { Summary:
        Minimum allowed distance of anchor points. }
    property Min: TCPFloat read FSlideJointSettings.Min write FSlideJointSettings.Min;

    { Summary:
        Maximum allowed distance of anchor points. }
    property Max: TCPFloat read FSlideJointSettings.Max write FSlideJointSettings.Max;
  end;

type
  { Summary:
      A Pivot Joint constraint. }
  TCPPivotJoint = class(TCPConstraint)
  {$REGION 'Internal Declarations'}
  private
    FPivotJointSettings: _cpPivotJoint; { This MUST be the FIRST field in the class }
  {$ENDREGION 'Internal Declarations'}
  public
    class function ConstraintType: TCPConstraintType; override;

    { Summary:
        Creates a new pivot joint.
      Parameters:
        A: The first body that the constraint acts on.
        B: The second body that the constraint acts on.
        Pivot: The point in world coordinates of the pivot. Because the pivot
          location is given in world coordinates, you must have the bodies
          moved into the correct positions already. }
    constructor Create(const A, B: TCPBody; const Pivot: TCPVect); overload;

    { Summary:
        Creates a new pivot joint.
      Parameters:
        A: The first body that the constraint acts on.
        B: The second body that the constraint acts on.
        Anchor1: The anchor point of the first body.
        Anchor2: The anchor point of the second body.
      Remarks:
        Make sure you have the bodies in the right place as the joint will fix
        itself as soon as you start simulating the space. }
    constructor Create(const A, B: TCPBody; const Anchor1, Anchor2: TCPVect); overload;

    { Summary:
        The anchor point of the first body. }
    property Anchor1: TCPVect read FPivotJointSettings.Anchr1 write FPivotJointSettings.Anchr1;

    { Summary:
        The anchor point of the second body. }
    property Anchor2: TCPVect read FPivotJointSettings.Anchr2 write FPivotJointSettings.Anchr2;
  end;

type
  { Summary:
      A Groove Joint constraint. }
  TCPGrooveJoint = class(TCPConstraint)
  {$REGION 'Internal Declarations'}
  private
    FGrooveJointSettings: _cpGrooveJoint; { This MUST be the FIRST field in the class }
    procedure SetGrooveA(const Value: TCPVect); inline;
    procedure SetGrooveB(const Value: TCPVect); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ConstraintType: TCPConstraintType; override;

    { Summary:
        Creates a new groove joint.
      Parameters:
        A: The first body that the constraint acts on.
        B: The second body that the constraint acts on.
        GrooveA: Start groove on body A.
        GrooveB: End groove on body A.
        Anchor2: End anchor on body B.
      Remarks:
        The groove goes from GrooveA to GrooveB on body A, and the pivot is
        attached to Anchor2 on body B. All coordinates are body local. }
    constructor Create(const A, B: TCPBody; const GrooveA, GrooveB, Anchor2: TCPVect);

    { Summary:
        Start groove on body A. }
    property GrooveA: TCPVect read FGrooveJointSettings.Grv_A write SetGrooveA;

    { Summary:
        End groove on body A. }
    property GrooveB: TCPVect read FGrooveJointSettings.Grv_B write SetGrooveB;

    { Summary:
        End anchor on body B. }
    property Anchor2: TCPVect read FGrooveJointSettings.Anchr2 write FGrooveJointSettings.Anchr2;
  end;

type
  TCPDampedSpring = class;

  { Summary:
      Event type for <link TCPDampedSpring.OnForce> }
  TCPDampedSpringForceEvent = function(const Spring: TCPDampedSpring;
    const Dist: TCPFloat): TCPFloat of Object;

  { Summary:
      A Damped Spring constraint. }
  TCPDampedSpring = class(TCPConstraint)
  {$REGION 'Internal Declarations'}
  private
    FDampedSpringSettings: _cpDampedSpring; { This MUST be the FIRST field in the class }
    FOnForce: TCPDampedSpringForceEvent;
    procedure SetOnForce(const Value: TCPDampedSpringForceEvent);
  {$ENDREGION 'Internal Declarations'}
  public
    class function ConstraintType: TCPConstraintType; override;

    { Summary:
        Creates a new damped spring constraint.
      Parameters:
        A: The first body that the constraint acts on.
        B: The second body that the constraint acts on.
        Anchor1: The anchor point of the first body.
        Anchor2: The anchor point of the second body.
        RestLength: The distance the spring wants to be.
        Stiffness: The spring constant
          (<exref target="http://en.wikipedia.org/wiki/Young%27s_modulus">Young’s modulus</exref>).
        Damping: how soft to make the damping of the spring. }
    constructor Create(const A, B: TCPBody; const Anchor1, Anchor2: TCPVect;
      const RestLength, Stiffness, Damping: TCPFloat);

    { Summary:
        The anchor point of the first body. }
    property Anchor1: TCPVect read FDampedSpringSettings.Anchr1 write FDampedSpringSettings.Anchr1;

    { Summary:
        The anchor point of the second body. }
    property Anchor2: TCPVect read FDampedSpringSettings.Anchr2 write FDampedSpringSettings.Anchr2;

    { Summary:
        The distance the spring wants to be. }
    property RestLength: TCPFloat read FDampedSpringSettings.RestLength write FDampedSpringSettings.RestLength;

    { Summary:
        The spring constant
        (<exref target="http://en.wikipedia.org/wiki/Young%27s_modulus">Young’s modulus</exref>). }
    property Stiffness: TCPFloat read FDampedSpringSettings.Stiffness write FDampedSpringSettings.Stiffness;

    { Summary:
        How soft to make the damping of the spring. }
    property Damping: TCPFloat read FDampedSpringSettings.Damping write FDampedSpringSettings.Damping;

    { Summary:
        Is fired to calculate the spring force. Set to nil to use the default
        calculation. }
    property OnForce: TCPDampedSpringForceEvent read FOnForce write SetOnForce;
  end;

type
  { Summary:
      A Damped Rotary Spring constraint. }
  TCPDampedRotarySpring = class(TCPConstraint)
  {$REGION 'Internal Declarations'}
  private
    FDampedRotarySpringSettings: _cpDampedRotarySpring; { This MUST be the FIRST field in the class }
    function GetRestAngle: TCPFloat; inline;
    procedure SetRestAngle(const Value: TCPFloat); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ConstraintType: TCPConstraintType; override;

    { Summary:
        Creates a new damped rotary spring constraint.
      Parameters:
        A: The first body that the constraint acts on.
        B: The second body that the constraint acts on.
        RestAngle: The relative angle in degrees that the bodies want to have.
        Stiffness: The spring constant
          (<exref target="http://en.wikipedia.org/wiki/Young%27s_modulus">Young’s modulus</exref>).
        Damping: how soft to make the damping of the spring. }
    constructor Create(const A, B: TCPBody; const RestAngle, Stiffness, Damping: TCPFloat);

    { Summary:
        The relative angle in degrees that the bodies want to have. }
    property RestAngle: TCPFloat read GetRestAngle write SetRestAngle;

    { Summary:
        The spring constant
        (<exref target="http://en.wikipedia.org/wiki/Young%27s_modulus">Young’s modulus</exref>). }
    property Stiffness: TCPFloat read FDampedRotarySpringSettings.Stiffness write FDampedRotarySpringSettings.Stiffness;

    { Summary:
        How soft to make the damping of the spring. }
    property Damping: TCPFloat read FDampedRotarySpringSettings.Damping write FDampedRotarySpringSettings.Damping;
  end;

type
  { Summary:
      A Rotary Limit Joint constraint. }
  TCPRotaryLimitJoint = class(TCPConstraint)
  {$REGION 'Internal Declarations'}
  private
    FRotaryLimitJointSettings: _cpRotaryLimitJoint; { This MUST be the FIRST field in the class }
    function GetMax: TCPFloat; inline;
    function GetMin: TCPFloat; inline;
    procedure SetMax(const Value: TCPFloat); inline;
    procedure SetMin(const Value: TCPFloat); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ConstraintType: TCPConstraintType; override;

    { Summary:
        Creates a new rotary limit joint.
      Parameters:
        A: The first body that the constraint acts on.
        B: The second body that the constraint acts on.
        Min: Minimum angular limit in degrees.
        Max: Maximum angular limit in degrees.
      Remarks:
        Constrains the relative rotations of two bodies. It is implemented so
        that it’s possible to for the range to be greater than a full
        revolution. }
    constructor Create(const A, B: TCPBody; const Min, Max: TCPFloat);

    { Summary:
        Minimum angular limit in degrees. }
    property Min: TCPFloat read GetMin write SetMin;

    { Summary:
        Maximum angular limit in degrees. }
    property Max: TCPFloat read GetMax write SetMax;
  end;

type
  { Summary:
      A Ratchet Joint constraint. }
  TCPRatchetJoint = class(TCPConstraint)
  {$REGION 'Internal Declarations'}
  private
    FRatchetJointSettings: _cpRatchetJoint; { This MUST be the FIRST field in the class }
    function GetAngle: TCPFloat; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ConstraintType: TCPConstraintType; override;

    { Summary:
        Creates a new ratchet joint.
      Parameters:
        A: The first body that the constraint acts on.
        B: The second body that the constraint acts on.
        Phase: The initial offset to use when deciding where the ratchet angles
          are.
        Ratchet: The distance between “clicks” in degrees. }
    constructor Create(const A, B: TCPBody; const Phase, Ratchet: TCPFloat);

    { Summary:
        Current angle in degrees. }
    property Angle: TCPFloat read GetAngle;

    { Summary:
        The initial offset to use when deciding where the ratchet angles are. }
    property Phase: TCPFloat read FRatchetJointSettings.Phase write FRatchetJointSettings.Phase;

    { Summary:
        The distance between “clicks”. }
    property Ratchet: TCPFloat read FRatchetJointSettings.Ratchet write FRatchetJointSettings.Ratchet;
  end;

type
  { Summary:
      A Gear Joint constraint. }
  TCPGearJoint = class(TCPConstraint)
  {$REGION 'Internal Declarations'}
  private
    FGearJointSettings: _cpGearJoint; { This MUST be the FIRST field in the class }
  {$ENDREGION 'Internal Declarations'}
  public
    class function ConstraintType: TCPConstraintType; override;

    { Summary:
        Creates a new gear joint.
      Parameters:
        A: The first body that the constraint acts on.
        B: The second body that the constraint acts on.
        Phase: The initial angular offset of the two bodies in degrees.
        Ratio: Angular velocity ratio.
      Remarks:
        Keeps the angular velocity ratio of a pair of bodies constant. Ratio is
        always measured in absolute terms. It is currently not possible to set
        the ratio in relation to a third body’s angular velocity. }
    constructor Create(const A, B: TCPBody; const Phase, Ratio: TCPFloat);

    { Summary:
        The initial offset to use when deciding where the ratchet angles are. }
    property Phase: TCPFloat read FGearJointSettings.Phase write FGearJointSettings.Phase;

    { Summary:
        The distance between “clicks”. }
    property Ratio: TCPFloat read FGearJointSettings.Ratio write FGearJointSettings.Ratio;
  end;

type
  { Summary:
      A Simple Motor constraint. }
  TCPSimpleMotor = class(TCPConstraint)
  {$REGION 'Internal Declarations'}
  private
    FSimpleMotorSettings: _cpSimpleMotor; { This MUST be the FIRST field in the class }
    function GetRate: TCPFloat; inline;
    procedure SetRate(const Value: TCPFloat); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class function ConstraintType: TCPConstraintType; override;

    { Summary:
        Creates a new simple motor joint.
      Parameters:
        A: The first body that the constraint acts on.
        B: The second body that the constraint acts on.
        Rate: The desired relative angular velocity in degrees per second.
      Remarks:
        Keeps the relative angular velocity of a pair of bodies constant. You
        will usually want to set an force (torque) maximum for motors as
        otherwise they will be able to apply a nearly infinite torque to keep
        the bodies moving. }
    constructor Create(const A, B: TCPBody; const Rate: TCPFloat);

    { Summary:
        The desired relative angular velocity in degrees per second. }
    property Rate: TCPFloat read GetRate write SetRate;
  end;
{$ENDREGION 'Chipmunk Constraints'}

{$REGION 'Chipmunk Spaces'}
type
  TCPSpace = class;

  { Summary:
      Event handler for <link TCPSpace.AddCollisionHandler>.
    Parameters:
      Space: The space that fired the event.
      Arbiter: The arbiter with collision details.
      Data: The user defined pointer you passed to AddCollisionHandler.
    Returns:
      True to process the collision normally, or False to ignore it. When you
      return False, the PreSolve and PostSolve events will never fire, but you
      will still get a Separate event. }
  TCPCollisionBeginEvent = function(const Space: TCPSpace;
    const Arbiter: PCPArbiter; const Data: Pointer): TCPBool of Object;

  { Summary:
      Event handler for <link TCPSpace.AddCollisionHandler>.
    Parameters:
      Space: The space that fired the event.
      Arbiter: The arbiter with collision details.
      Data: The user defined pointer you passed to AddCollisionHandler.
    Returns:
      True to process the collision normally, or False to ignore it. When you
      return False, the PreSolve and PostSolve events will never fire, but you
      will still get a Separate event. }
  TCPCollisionPreSolveEvent = function(const Space: TCPSpace;
    const Arbiter: PCPArbiter; const Data: Pointer): TCPBool of Object;

  { Summary:
      Event handler for <link TCPSpace.AddCollisionHandler>.
    Parameters:
      Space: The space that fired the event.
      Arbiter: The arbiter with collision details.
      Data: The user defined pointer you passed to AddCollisionHandler. }
  TCPCollisionPostSolveEvent = procedure(const Space: TCPSpace;
    const Arbiter: PCPArbiter; const Data: Pointer) of Object;

  { Summary:
      Event handler for <link TCPSpace.AddCollisionHandler>.
    Parameters:
      Space: The space that fired the event.
      Arbiter: The arbiter with collision details.
      Data: The user defined pointer you passed to AddCollisionHandler. }
  TCPCollisionSeparateEvent = procedure(const Space: TCPSpace;
    const Arbiter: PCPArbiter; const Data: Pointer) of Object;

  { Summary:
      Event handler for <link TCPSpace.AddPostStepCallback>.
    Parameters:
      Space: The space that fired the event.
      Obj: The pointer value you supplied as a key to AddPostStepCallback.
      Data: The user defined pointer you passed to AddPostStepCallback. }
  TCPPostStepEvent = procedure(const Space: TCPSpace; const Obj, Data: Pointer) of Object;

  { Summary:
      Event handler for <link TCPSpace.PointQuery>.
    Parameters:
      Shape: The shape that was found in the query.
      Data: The user defined pointer you passed to PointQuery. }
  TCPPointQueryEvent = procedure(const Shape: TCPShape; const Data: Pointer) of Object;

  { Summary:
      Event handler for <link TCPSpace.SegmentQuery>.
    Parameters:
      Shape: The shape that was found in the query.
      Distance: Distance along query segment, will always be in the range 0..1
        (as a percentage along the segment).
      Normal: Normal of the hit surface.
      Data: The user defined pointer you passed to SegmentQuery. }
  TCPSegmentQueryEvent = procedure(const Shape: TCPShape; const Distance: TCPFloat;
    const Normal: TCPVect; const Data: Pointer) of Object;

  { Summary:
      Event handler for <link TCPSpace.ForEachBody>.
    Parameters:
      Space: The space that fired the event.
      Body: The body that is enumerated.
      Data: The user defined pointer you passed to ForEachXxx. }
  TCPForEachBodyEvent = procedure(const Space: TCPSpace;
    const Body: TCPBody; const Data: Pointer) of Object;

  { Summary:
      Event handler for <link TCPSpace.ForEachShape> and
      <link TCPSpace.ForEachStaticShape>.
    Parameters:
      Space: The space that fired the event.
      Shape: The shape that is enumerated.
      Data: The user defined pointer you passed to ForEachXxx. }
  TCPForEachShapeEvent = procedure(const Space: TCPSpace;
    const Shape: TCPShape; const Data: Pointer) of Object;

  { Summary:
      Event handler for <link TCPSpace.ForEachArbiter>.
    Parameters:
      Space: The space that fired the event.
      Arbiter: The arbiter that is enumerated.
      Data: The user defined pointer you passed to ForEachArbiter. }
  TCPForEachArbiterEvent = procedure(const Space: TCPSpace;
    const Arbiter: PCPArbiter; const Data: Pointer) of Object;

  { Summary:
      Spaces in Chipmunk are the basic unit of simulation. You add rigid bodies,
      shapes and constraints to it and then step them forward through time.
    Remarks:
      # What Are Iterations, and Why Should I care? #
      Chipmunk uses an iterative solver to figure out the forces between objects
      in the space. What this means is that it builds a big list of all of the
      collisions, joints, and other constraints between the bodies and makes
      several passes over the list considering each one individually. The number
      of passes it makes is the iteration count, and each iteration makes the
      solution more accurate. If you use too many iterations, the physics should
      look nice and solid, but may use up too much CPU time. If you use too few
      iterations, the simulation may seem mushy or bouncy when the objects
      should be solid. Setting the number of iterations lets you balance between
      CPU usage and the accuracy of the physics. Chipmunk’s default of 10
      iterations is sufficient for most simple games.

      # Rogue Bodies #
      Rogue bodies are bodies that have not been added to the space, but are
      referenced from shapes or joints. Rogue bodies are a common way of
      controlling moving elements in Chipmunk such as platforms. As long as the
      body’s velocity matches the changes to it’s position, there is no problem
      with doing this. Most games will not need rogue bodies.

      In previous versions, Chipmunk also used infinite mass rogue bodies to
      attach static shapes to. Creating and maintaining your own body for this
      is no longer necessary as each space has it’s own body for attaching
      static shapes to. It is also not recommended to do this as it does not
      work with the sleeping feature. Anything touching or jointed to a rogue
      body is forbidden from sleeping.

      # Static Shapes #
      Chipmunk optimizes collision detection against shapes that do not move.
      Chipmunk doesn’t automatically know if shapes should be static or not.
      There are two ways to let a Chipmunk space know that a shape is static.
      The first is to use the space’s static body when creating the shape.
      When you call <link TCPSpace.AddShape> on such a shape, it will be added
      as an optimized static shape. Calling <link TCPSpace.RemoveShape> will
      remove the shape. If you want a shape attached to an infinite mass rogue
      body to be treated as static, you will have to add and remove it’s shape’s
      using <link TCPSpace.AddStaticShape> and <link TCPSpace.RemoveStaticShape>
      explicitly as Chipmunk has no way to know if the rogue body is meant to be
      static or not.

      Because static shapes aren’t updated automatically, you must let Chipmunk
      know that it needs to update the static shapes using
      <link TCPSpace.RehashStatic> whenever you move a static shape. If you find
      yourself moving static shapes attached to rogue bodies often, you should
      add the shapes normally using <link TCPSpace.AddShape> instead.

      # Sleeping #
      New in Chipmunk 5.3 is the ability of spaces to disable entire groups of
      objects that have stopped moving to save CPU time as well as battery life.
      In order to use this feature you must do 2 things. The first is that you
      must use the space’s static body to attach your static shapes to. Because
      the space controls this body directly, it knows that it won’t move
      unexpectedly and that means that it is safe to allow things to fall asleep
      when they are touching it. The second part is much easier, you need to
      choose a threshold value for <link TCPSpace.SleepTimeThreshold> and
      optionally <link TCPSpace.IdleSpeedThreshold>.

      # Notes #
      * When removing objects from the space, make sure you remove any other
        objects that reference it. For instance, when you remove a body, remove
        the joints and shapes attached to it.
      * The number of iterations, and the size of the time step determine the
        quality of the simulation. More iterations, or smaller time steps
        increase the quality. Keep in mind that higher quality also means higher
        CPU usage.
      * Because static shapes are only rehashed when you request it, it’s
        possible to use a much higher count argument to <link ResizeStaticHash>
        than to <link ResizeActiveHash>. Doing so will use more memory but can
        improve performance if you have a lot of static shapes.

      # Overview of Collision Detection in Chipmunk #
      In order to make collision detection in Chipmunk as fast as possible, the
      process is broken down into several stages. While I’ve tried to keep it
      conceptually simple, the implementation can be a bit daunting. Fortunately
      as a user of the library, you don’t need to understand everything about
      how it works. If you are trying to squeeze every ounce of performance out
      of Chipmunk, understanding this section is crucial.

      # Spatial Hashing #
      Chipmunk uses a spatial hash for its broad phase culling. Spatial hashes
      are very efficient for a scene made up of consistently sized objects. It
      basically works by taking the axis aligned bounding boxes for all the
      objects in the scene, mapping them onto an infinite sized grid, then
      mapping those grid cells onto a finite sized hash table. This way, you
      only have to check collisions between objects in the same hash table
      cells, and mapping the objects onto the grid can be done fairly quickly.
      Objects in the same hash table cells tend to be very close together, and
      therefore more likely to be colliding. The downside is that in order to
      get the best performance out of a spatial hash, you have to tune the size
      of the grid cells and the size of the hash table so you don’t get too
      many false positives.

      Things to keep in mind:
      * Using too small a grid size means that your objects will be split into
        many grid cells which means that the spatial hash will spend a lot of
        time filling empty hash cells for just one object and rarely finding
        any with multiple objects. Making the grid size too big means that you
        will be putting a lot of objects into a single hash cell, and collisions
        will have to be checked between all of them.
      * Using too small of a hash table means that you will map too many far
        away objects into a single hash cell. Using too many means that the hash
        table will have to spend a lot of time doing clears and rehashes.

      For more information on spatial hashing in general,
      <exref target="http://www.beosil.com/download/CollisionDetectionHashing_VMV03.pdf>Optimized Spatial Hashing for Collision Detection of Deformable Objects</exref>
      is a good paper that covers all the basics.

      # Collision Filtering #
      After the spatial hash figures out pairs of shapes that are likely to be
      near each other, it passes them back to the space to perform some
      additional filtering on the pairs. If the pairs pass all the filters, then
      Chipmunk will test if the shapes are actually overlapping. If the shape to
      shape collision check passes, then the collision handler callbacks are
      called. These tests are much faster to try than the shape to shape
      collision checks, so use these if you can to reject collisions early
      instead of rejecting them from callbacks if you can.

      * <b>Bounding Box Test</b>: The shapes are not colliding if their bounding
        boxes are not overlapping. You can’t really affect this, but this is
        when it’s done.
      * <b>Layer Test</b>: The shapes are not colliding if they don’t occupy and
        of the same layers. (the bitwise AND of their layer masks is 0)
      * <b>Group Test</b>: Shapes shouldn’t collide with other shapes in the
        same non-zero group.

      # Primitive Shape to Shape Collision Detection #
      The most expensive test that you can do to see if shapes should collide is
      to actually check based on their geometry. Circle to circle and circle to
      line collisions are pretty quick. Poly to poly collisions get more
      expensive as the number of vertexes increases. Simpler shapes make for
      faster collisions (and more importantly fewer collision points for the
      solver to run).

      # Collision Handler Filtering #
      After checking if two shapes overlap Chipmunk will look to see if you have
      defined a collision handler for the collision types of the shapes. This
      gives you a large amount of flexibility to process collisions events, but
      also gives you a very flexible way to filter out collisions. The return
      value of the begin and preSolve callback determines whether or not the
      colliding pair of shapes is discarded or not. Returning true will keep the
      pair, false will discard it. If you don’t define a handler for the given
      collision_types, Chipmunk will call the space’s default handler, which by
      default is defined to simply accept all collisions.

      While using callbacks to filter collisions is the most flexible way, keep
      in mind that by the time your callback is called all of the most expensive
      collision detection has already been done. For simulations with a lot of
      colliding objects each frame, the time spent finding collisions is small
      compared to the time spent solving the physics for them so it may not be a
      big deal. Still, use layers or groups first if you can.

      # Callbacks #
      A physics library without any events or feedback would not be very useful
      for games. How would you know when the player bumped into an enemy so that
      you could take some health points away? How would you know how hard the
      car hit something so you don’t play a loud crash noise when a pebble hits
      it? What if you need to decide if a collision should be ignored based on
      specific conditions, like implementing one way platforms? Chipmunk has a
      number of powerful callback systems that you can plug into to accomplish
      all of that.

      # Collision Handlers #
      A collision handler is a set of 4 function callbacks for the different
      collision events that Chipmunk recognizes. The event types are:

      * <b>Begin</b>: Two shapes just started touching for the first time this
        step. Return true from the callback to process the collision normally
        or false to cause Chipmunk to ignore the collision entirely. If you
        return false, the pre-solve and post-solve callbacks will never be run,
        but you will still recieve a separate event when the shapes stop
        overlapping.
      * <b>Pre-Solve</b>: Two shapes are touching during this step. Return false
        from the callback to make Chipmunk ignore the collision this step or
        true to process it normally. Additionally, you may override collision
        values such as <link TCPArbiter.Elasticity> and <link TCPArbiter.Friction>
        to provide custom friction or elasticity values.
      * <b>Post-Solve</b>: Two shapes are touching and their collision response
        has been processed. You can retrieve the collision force at this time
        if you want to use it to calculate sound volumes or damage amounts. See
        <link TCPArbiter> for more info.
      * <b>Separate</b>: Two shapes have just stopped touching for the first
        time this step.

      Collision callbacks are closely associated with <link TCPArbiter> records.
      You should familiarize yourself with those as well.

      <b>Note</b>: Shapes tagged as sensors (<link TCPShape.Sensor> = True)
      never generate collisions that get processed so collisions between sensors
      shapes and other shapes will never call the post-solve callback. They
      still generate begin, and separate callbacks, and the pre solve callback
      is also called every frame even though there is no real collision.

      See <link TCPSpace.AddCollisionHandler>,
      <link TCPSpace.RemoveCollisionHandler>,
      <link TCPSpace.SetDefaultCollisionHandler> and
      <link TCPSpace.AddPostStepCallback> for the APIs.

      # Post-Step Callbacks #
      Post-step callbacks are the one place where you can break the rules about
      adding or removing objects from within a callback. In fact, their primary
      function is to help you safely remove objects from the space that you
      wanted to disable or destroy in a collision callback.

      Post step callbacks are registered as a function and a pointer that is
      used as a key. You can only register one post step callback per key. This
      prevents you from accidentally removing an object more than once. For
      instance, say that you get a collision callback between a bullet and
      object A. You want to destroy both the bullet and object A, so you
      register a post-step callback to safely remove them from your game. Then
      you get a second collision callback between the bullet and object B. You
      register a post-step callback to remove object B, and a second post-step
      callback to remove the bullet. Because you can only register one callback
      per key, the post-step callback for the bullet will only be called once
      and you can’t accidentally try to remove it twice.

      See <link TCPSpace.AddPostStepCallback>.

      # Queries #
      Chipmunk spaces currently support three kinds of spatial queries, point,
      segment and bounding box. Any type can be done efficiently against an
      entire space, or against individual shapes (see <link TCPShape>). All types
      of queries take a collision group and layer that are used to filter
      matches out using the same rules used for filtering collisions between
      shapes. If you don’t want to filter out any matches, use CP_ALL_LAYERS for
      the layers and CP_NO_GROUP as the group.

      Point queries are useful for things like mouse picking and simple sensors.
      See <link PointQuery> or <link PointQuery>.

      Segment queries are like ray casting, but because Chipmunk uses a spatial
      hash to process collisions, it cannot process infinitely long queries like
      a ray. In practice this is still very fast and you don’t need to worry too
      much about the performance as long as you aren’t using extremely long
      segments for your queries. See <link SegmentQuery>.

      See also the helper functions <link TCPSegmentQueryInfo.HitPoint> and
      <link TCPSegmentQueryInfo.HitDist> }
  TCPSpace = class
  {$REGION 'Internal Declarations'}
  private
    FSettings: cpSpace;
    FHandle: PcpSpace;
    FStaticBody: TCPBody;
    FUserData: Pointer;
    FCollisionHandlerSet: PcpHashSet;
    FDefaultOnBegin: TCPCollisionBeginEvent;
    FDefaultOnPreSolve: TCPCollisionPreSolveEvent;
    FDefaultOnPostSolve: TCPCollisionPostSolveEvent;
    FDefaultOnSeparate: TCPCollisionSeparateEvent;
    FDefaultData: Pointer;
    FPostStepCallbacks: PcpHashSet;
    FOnPointQuery: TCPPointQueryEvent;
    FOnSegmentQuery: TCPSegmentQueryEvent;
    FOnBody: TCPForEachBodyEvent;
    FOnShape: TCPForEachShapeEvent;
    FOnArbiter: TCPForEachArbiterEvent;
    FCallbackData: Pointer;
    function GetGravity: TCPVect; inline;
    procedure SetGravity(const Value: TCPVect); inline;
    procedure InternalRemoveCollisionHandler(const A, B: TCPCollisionType);
    procedure HashEach(const Hash: PcpSpaceHash; const OnShape: TCPForEachShapeEvent;
      const Data: Pointer);
    function GetConstraintCount: Integer; inline;
    function GetConstraint(const Index: Integer): TCPConstraint; inline;
    function GetBody(const Index: Integer): TCPBody; inline;
    function GetBodyCount: Integer; inline;
    function GetArbiter(const Index: Integer): PCPArbiter; inline;
    function GetArbiterCount: Integer; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a new empty space. }
    constructor Create;
    destructor Destroy; override;

    { Summary:
        Adds a shape to the space.
      Remarks:
        The space does <b>not</b> become owner of the shape!
        You cannot call this method from within a callback other than a
        post-step callback (which is different than a post-solve callback!).
        Attempting call this method while <link TCPSpace.Step> is still
        executing will throw an assertion. }
    function AddShape(const Shape: TCPShape): TCPShape; inline;

    { Summary:
        Adds a static shape to the space.
      Remarks:
        The space does <b>not</b> become owner of the shape!
        See the section on Static Shapes in the <link TCPSpace> documentation
        for an explanation of what a static shape is and how it differs from a
        normal shape.
        You cannot call this method from within a callback other than a
        post-step callback (which is different than a post-solve callback!).
        Attempting call this method while <link TCPSpace.Step> is still
        executing will throw an assertion. }
    function AddStaticShape(const Shape: TCPShape): TCPShape; inline;

    { Summary:
        Adds a body to the space.
      Remarks:
        The space does <b>not</b> become owner of the body!
        You cannot call this method from within a callback other than a
        post-step callback (which is different than a post-solve callback!).
        Attempting call this method while <link TCPSpace.Step> is still
        executing will throw an assertion. }
    function AddBody(const Body: TCPBody): TCPBody; inline;

    { Summary:
        Adds a constraint to the space.
      Remarks:
        The space does <b>not</b> become owner of the constraint!
        You cannot call this method from within a callback other than a
        post-step callback (which is different than a post-solve callback!).
        Attempting call this method while <link TCPSpace.Step> is still
        executing will throw an assertion. }
    function AddConstraint(const Constraint: TCPConstraint): TCPConstraint; inline;

    { Summary:
        Removes a shape to the space.
      Remarks:
        Removing a shape does <b>not</b> free it!
        You cannot call this method from within a callback other than a
        post-step callback (which is different than a post-solve callback!).
        Attempting call this method while <link TCPSpace.Step> is still
        executing will throw an assertion. }
    procedure RemoveShape(const Shape: TCPShape); inline;

    { Summary:
        Removes a static shape to the space.
      Remarks:
        Removing a shape does <b>not</b> free it!
        See the section on Static Shapes in the <link TCPSpace> documentation
        for an explanation of what a static shape is and how it differs from a
        normal shape.
        You cannot call this method from within a callback other than a
        post-step callback (which is different than a post-solve callback!).
        Attempting call this method while <link TCPSpace.Step> is still
        executing will throw an assertion. }
    procedure RemoveStaticShape(const Shape: TCPShape); inline;

    { Summary:
        Removes a body to the space.
      Remarks:
        Removing a body does <b>not</b> free it!
        You cannot call this method from within a callback other than a
        post-step callback (which is different than a post-solve callback!).
        Attempting call this method while <link TCPSpace.Step> is still
        executing will throw an assertion. }
    procedure RemoveBody(const Body: TCPBody); inline;

    { Summary:
        Removes a constraint to the space.
      Remarks:
        Removing a constraint does <b>not</b> free it!
        You cannot call this method from within a callback other than a
        post-step callback (which is different than a post-solve callback!).
        Attempting call this method while <link TCPSpace.Step> is still
        executing will throw an assertion. }
    procedure RemoveConstraint(const Constraint: TCPConstraint); inline;

    { Summary:
        Chipmunk uses a spatial hash to accelerate it’s collision detection.
        While it’s not necessary to interact with the hash directly, the
        current API does expose some of this at the space level to allow you to
        tune it’s performance.
      Parameters:
        Dim: the size of the hash cells. Setting dim to the average collision
          shape size is likely to give the best performance. Setting dim too
          small will cause the shape to be inserted into many cells, setting it
          too low will cause too many objects into the same hash slot.
        Count: the <i>suggested</i> minimum number of cells in the hash table.
          If there are too few cells, the spatial hash will return many false
          positives. Too many cells will be hard on the cache and waste memory.
          Setting count to ~10x the number of objects in the space is probably a
          good starting point. Tune from there if necessary. By default, dim is
          100.0, and count is 1000.
      Remarks:
        See the Chipmunk documentation for more information and visualization. }
    procedure ResizeStaticHash(const Dim: TCPFloat = 100;
      const Count: Integer = 1000); inline;

    { Summary:
        See <link ResizeStaticHash>. }
    procedure ResizeActiveHash(const Dim: TCPFloat = 100;
      const Count: Integer = 1000); inline;

    { Summary:
        Rehashes the shapes in the static spatial hash. You must call this if
        you move any static shapes or Chipmunk won’t update their collision
        detection data. }
    procedure RehashStatic; inline;

    { Summary:
        Calls the OnBody callback for each body, passing the user Data passed
        to the method. }
    procedure ForEachBody(const OnBody: TCPForEachBodyEvent;
      const Data: Pointer);

    { Summary:
        Calls the OnShape callback for each non-static shape, passing the user
        Data passed to the method. }
    procedure ForEachShape(const OnShape: TCPForEachShapeEvent;
      const Data: Pointer);

    { Summary:
        Calls the OnShape callback for each static shape, passing the user
        Data passed to the method. }
    procedure ForEachStaticShape(const OnShape: TCPForEachShapeEvent;
      const Data: Pointer);

    { Summary:
        Calls the OnArbiter callback for each collision point arbiter, passing
        the user Data passed to the method. }
    procedure ForEachArbiter(const OnArbiter: TCPForEachArbiterEvent;
      const Data: Pointer);

    { Summary:
        Update the space for the given time step. Using a fixed time step is
        highly recommended. Doing so will increase the efficiency of the contact
        persistence, requiring an order of magnitude fewer iterations and CPU
        usage. }
    procedure Step(const DT: TCPFloat); inline;

    { Summary:
        Add a collision handler for given collision type pair. Whenever shapes
        with collision type (<link TCPShape.CollisionType>) A and collision type
        B collide, these events will be used to process the collision. Data is a
        user definable context pointer that is passed to each of the callbacks.
      Remarks:
        Nil can be provided for events you do not wish to implement,
        however Chipmunk will call it’s own default versions for these and not
        the default ones you’ve set up for the space. If you need to fall back
        on the space’s default callbacks, you’ll have to provide them
        individually to each handler definition.
        When there already is a collision handler for the (A, B) pair (or
        (B, A) pair), then it will be overridden with these events. }
    procedure AddCollisionHandler(const A, B: TCPCollisionType;
      const OnBegin: TCPCollisionBeginEvent;
      const OnPreSolve: TCPCollisionPreSolveEvent;
      const OnPostSolve: TCPCollisionPostSolveEvent;
      const OnSeparate: TCPCollisionSeparateEvent;
      const Data: Pointer);

    { Summary:
        Remove a collision handler for a given collision type pair. }
    procedure RemoveCollisionHandler(const A, B: TCPCollisionType);

    { Summary:
        Register a default collision handler to be used when no specific
        collision handler is found. The space is given a default handler when
        created that returns True for all collisions in Begin and PreSolve and
        does nothing in the PostSolve and Separate callbacks. }
    procedure SetDefaultCollisionHandler(
      const OnBegin: TCPCollisionBeginEvent;
      const OnPreSolve: TCPCollisionPreSolveEvent;
      const OnPostSolve: TCPCollisionPostSolveEvent;
      const OnSeparate: TCPCollisionSeparateEvent;
      const Data: Pointer);

    { Summary:
        Add OnPostStep to be called before <link Step> returns. Obj and Data
        will be passed to your function. Only the last callback registered for
        any unique value of Obj will be recorded. You can add post-step
        callbacks from outside of other callback functions, but they won’t be
        called until <link Step> is called again. }
    procedure AddPostStepCallback(const OnPostStep: TCPPostStepEvent;
      const Obj, Data: Pointer);

    { Summary:
        Query the space at point P filtering out matches with the given layers
        and group. OnHit is called for each shape found along with the Data
        argument passed to this method.
      Remarks:
        Point queries are useful for things like mouse picking and simple
        sensors. }
    procedure PointQuery(const P: TCPVect; const Layers: TCPLayers;
      const Group: TCPGroup; const OnHit: TCPPointQueryEvent;
      const Data: Pointer); overload;

    { Summary:
        Query space at point and return the first shape found matching the given
        layers and group. Returns nil if no shape was found. }
    function PointQuery(const P: TCPVect; const Layers: TCPLayers;
      const Group: TCPGroup): TCPShape; overload; inline;

    { Summary:
        Query space along the line segment from A to B filtering out
        matches with the given layers and group. OnHit is called with the
        normalized distance along the line and surface normal for each shape
        found along with the Data argument passed to this method. }
    procedure SegmentQuery(const A, B: TCPVect; const Layers: TCPLayers;
      const Group: TCPGroup; const OnHit: TCPSegmentQueryEvent;
      const Data: Pointer); overload;

    { Summary:
        Query space along the line segment from A to B filtering out matches
        with the given layers and group. Only the first shape encountered is
        returned and the search is short circuited. Returns nil if no shape was
        found. The Info record will be initialized with the raycast info. }
    function SegmentQuery(const A, B: TCPVect; const Layers: TCPLayers;
      const Group: TCPGroup; out Info: TCPSegmentQueryInfo): TCPShape; overload; inline;

    { Summary:
        Allow you to control the accuracy of the solver. Defaults to 10.
      Remarks:
        See the section on iterations in the <link TCPSpace> documentation for
        an explanation. }
    property Iterations: Integer read FSettings.Iterations write FSettings.Iterations;

    { Summary:
        Number of iterations to use in the impulse solver to solve elastic
        collisions. }
    property ElasticIterations: Integer read FSettings.ElasticIterations write FSettings.ElasticIterations;

    { Summary:
        Global gravity applied to the space. Defaults to <link CPVZero>.
        Can be overridden on a per body basis by writing custom integration
        functions. }
    property Gravity: TCPVect read GetGravity write SetGravity;

    { Summary:
        Amount of viscous damping to apply to the space. A value of 0.9 means
        that each body will lose 10% of it’s velocity per second. Defaults to 1.
        Like <link Gravity> can be overridden on a per body basis. }
    property Damping: TCPFloat read FSettings.Damping write FSettings.Damping;

    { Summary:
        Speed threshold for a body to be considered idle. The default value of
        0 means to let the space guess a good threshold based on gravity. }
    property IdleSpeedThreshold: TCPFloat read FSettings.IdleSpeedThreshold write FSettings.IdleSpeedThreshold;

    { Summary:
        Time a group of bodies must remain idle in order to fall asleep. The
        default value of INFINITY disables the sleeping algorithm. }
    property SleepTimeThreshold: TCPFloat read FSettings.SleepTimeThreshold write FSettings.SleepTimeThreshold;

    { Summary:
        Number of bodies in the space. }
    property BodyCount: Integer read GetBodyCount;

    { Summary:
        The bodies in the space. }
    property Bodies[const Index: Integer]: TCPBody read GetBody;

    { Summary:
        You can make static shapes by attaching it to StaticBody. This is an
        alternative to calling <link AddStaticShape>. }
    property StaticBody: TCPBody read FStaticBody;

    { Summary:
        Number of constraints in the space. }
    property ConstraintCount: Integer read GetConstraintCount;

    { Summary:
        The constrains in the space. }
    property Constraints[const Index: Integer]: TCPConstraint read GetConstraint;

    { Summary:
        Number of arbiters in the space. }
    property ArbiterCount: Integer read GetArbiterCount;

    { Summary:
        The arbiters in the space. }
    property Arbiters[const Index: Integer]: PCPArbiter read GetArbiter;

    { Summary:
        A user definable data pointer. If you set this to point at the game
        object the shapes is for, then you can access your game object from
        Chipmunk callbacks.}
    property UserData: Pointer read FUserData write FUserData;

    { Summary:
        Internal handle used for the space.
      Remarks:
        You should only use the property if you need access to the lower-level
        Chipmunk library directly. }
    property Handle: PcpSpace read FHandle;
  end;
{$ENDREGION 'Chipmunk Spaces'}

{$REGION 'Resource Strings'}
resourcestring
  RS_CP_CANNOT_LOAD_CHIPMUNK = 'Unable to load Chipmunk library "%s".';
{$ENDREGION 'Resource Strings'}

implementation

uses
  Math;

{ TCPChipmunk }

class procedure TCPChipmunk.Finalize;
begin
  {$IFNDEF USE_CHIPMUNK_STATIC}
  if (FInitialized) then
    cpFree;
  {$ENDIF}
end;

class procedure TCPChipmunk.Initialize;
begin
  if (not FInitialized) then
  begin
    {$IFNDEF USE_CHIPMUNK_STATIC}
    if (not cpLoad(libChipmunk, False)) then
      raise ECPError.CreateFmt(RS_CP_CANNOT_LOAD_CHIPMUNK, [libChipmunk]);
    {$ENDIF}

    cpInitChipmunk;
    FInitialized := True;
  end;
end;

{ TCPMath }

class function TCPMath.Max(const A, B: TCPFloat): TCPFloat;
begin
  if (A < B) then
    Result := B
  else
    Result := A;
end;

class function TCPMath.Min(const A, B: TCPFloat): TCPFloat;
begin
  if (A < B) then
    Result := A
  else
    Result := B;
end;

{ TCPVect }

function CPV(const X, Y: TCPFloat): TCPVect; inline;
begin
  Result.X := X;
  Result.Y := Y;
end;

class operator TCPVect.Add(const A, B: TCPVect): TCPVect;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class function TCPVect.Clamp(const V: TCPVect; const Len: TCPFloat): TCPVect;
begin
  if (V * V) > (Len * Len) then
    Result := V.Normalize * Len
  else
    Result := V;
end;

function TCPVect.Clamp(const Len: TCPFloat): TCPVect;
begin
  if (Self * Self) > (Len * Len) then
    Result := Self.Normalize * Len
  else
    Result := Self;
end;

class function TCPVect.Create: TCPVect;
begin
  Result.X := 0;
  Result.Y := 0;
end;

class function TCPVect.Create(const X, Y: TCPFloat): TCPVect;
begin
  Result.X := X;
  Result.Y := Y;
end;

class function TCPVect.CrossProduct(const V1, V2: TCPVect): TCPFloat;
begin
  Result := V1.X * V2.Y - V1.Y * V2.X;
end;

function TCPVect.CrossProduct(const Other: TCPVect): TCPFloat;
begin
  Result := X * Other.Y - Y * Other.X;
end;

class function TCPVect.Distance(const V1, V2: TCPVect): TCPFloat;
begin
  Result := Length(V1 - V2);
end;

function TCPVect.Distance(const Other: TCPVect): TCPFloat;
begin
  Result := Length(Self - Other);
end;

class function TCPVect.DistanceSq(const V1, V2: TCPVect): TCPFloat;
begin
  Result := LengthSq(V1 - V2);
end;

function TCPVect.DistanceSq(const Other: TCPVect): TCPFloat;
begin
  Result := LengthSq(Self - Other);
end;

function TCPVect.DotProduct(const Other: TCPVect): TCPFloat;
begin
  Result := X * Other.X + Y * Other.Y;
end;

class function TCPVect.DotProduct(const V1, V2: TCPVect): TCPFloat;
begin
  Result := V1.X * V2.X + V1.Y * V2.Y;
end;

class operator TCPVect.Equal(const A, B: TCPVect): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

function TCPVect.Equals(const Other: TCPVect): Boolean;
begin
  Result := SameValue(X, Other.X) and SameValue(Y, Other.Y);
end;

class function TCPVect.ForAngle(const Angle: TCPFloat): TCPVect;
var
  S, C: Extended;
begin
  SinCos(Angle * DEG2RAD, S, C);
  Result.X := C;
  Result.Y := S;
end;

function TCPVect.Length: TCPFloat;
begin
  Result := cpvlength(cpVect(Self));
end;

class function TCPVect.Length(const V: TCPVect): TCPFloat;
begin
  Result := cpvlength(cpVect(V));
end;

class function TCPVect.LengthSq(const V: TCPVect): TCPFloat;
begin
  Result := V * V;
end;

function TCPVect.LengthSq: TCPFloat;
begin
  Result := Self * Self;
end;

function TCPVect.LinearInterpolate(const Target: TCPVect;
  const T: TCPFloat): TCPVect;
begin
  Result := (Self * (1 - T)) + (Target * T);
end;

function TCPVect.LinearInterpolateConst(const Target: TCPVect;
  const D: TCPFloat): TCPVect;
begin
  Result := Self + Clamp(Target - Self, D);
end;

class function TCPVect.LinearInterpolateConst(const V1, V2: TCPVect;
  const D: TCPFloat): TCPVect;
begin
  Result := V1 + Clamp(V2 - V1, D);
end;

class function TCPVect.LinearInterpolate(const V1, V2: TCPVect;
  const T: TCPFloat): TCPVect;
begin
  Result := (V1 * (1 - T)) + (V2 * T);
end;

class operator TCPVect.Multiply(const A, B: TCPVect): TCPFloat;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

class operator TCPVect.Multiply(const A: TCPVect; const S: TCPFloat): TCPVect;
begin
  Result.X := A.X * S;
  Result.Y := A.Y * S;
end;

class function TCPVect.Near(const V1, V2: TCPVect;
  const Dist: TCPFloat): TCPBool;
begin
  Result := (DistanceSq(V1, V2) < (Dist * Dist));
end;

function TCPVect.Near(const Other: TCPVect; const Dist: TCPFloat): TCPBool;
begin
  Result := (DistanceSq(Other) < (Dist * Dist));
end;

class operator TCPVect.Negative(const A: TCPVect): TCPVect;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

function TCPVect.Normalize: TCPVect;
begin
  Result := Self * (1 / Length);
end;

class function TCPVect.NormalizeSafe(const V: TCPVect): TCPVect;
begin
  if (V.X = 0) and (V.Y = 0) then
    Result := CPVZero
  else
    Result := Normalize(V);
end;

function TCPVect.NormalizeSafe: TCPVect;
begin
  if (X = 0) and (Y = 0) then
    Result := CPVZero
  else
    Result := Normalize;
end;

class function TCPVect.Normalize(const V: TCPVect): TCPVect;
begin
  Result := V * (1 / V.Length);
end;

class operator TCPVect.NotEqual(const A, B: TCPVect): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y);
end;

function TCPVect.Perp: TCPVect;
begin
  Result.X := -Y;
  Result.Y := X;
end;

class function TCPVect.Perp(const V: TCPVect): TCPVect;
begin
  Result.X := -V.Y;
  Result.Y := V.X;
end;

function TCPVect.Project(const Target: TCPVect): TCPVect;
begin
  Result := Target * ((Self * Self) / (Target * Target));
end;

class function TCPVect.Project(const V1, V2: TCPVect): TCPVect;
begin
  Result := V2 * ((V1 * V1) / (V2 * V2));
end;

class function TCPVect.Rotate(const V1, V2: TCPVect): TCPVect;
begin
  Result.X := V1.X * V2.X - V1.Y * V2.Y;
  Result.Y := V1.X * V2.Y + V1.Y * V2.X;
end;

function TCPVect.Rotate(const Other: TCPVect): TCPVect;
begin
  Result.X := X * Other.X - Y * Other.Y;
  Result.Y := X * Other.Y + Y * Other.X;
end;

function TCPVect.RPerp: TCPVect;
begin
  Result.X := Y;
  Result.Y := -X;
end;

class function TCPVect.RPerp(const V: TCPVect): TCPVect;
begin
  Result.X := V.Y;
  Result.Y := -V.X;
end;

class function TCPVect.SphericalLinearInterpolate(const V1, V2: TCPVect;
  const T: TCPFloat): TCPVect;
begin
  Result := TCPVect(cpvslerp(cpVect(V1), cpVect(V2), T));
end;

function TCPVect.SphericalLinearInterpolate(const Target: TCPVect;
  const T: TCPFloat): TCPVect;
begin
  Result := TCPVect(cpvslerp(cpVect(Self), cpVect(Target), T));
end;

class function TCPVect.SphericalLinearInterpolateConst(const V1, V2: TCPVect;
  const Angle: TCPFloat): TCPVect;
begin
  Result := TCPVect(cpvslerpconst(cpVect(V1), cpVect(V2), Angle * DEG2RAD));
end;

function TCPVect.SphericalLinearInterpolateConst(const Target: TCPVect;
  const Angle: TCPFloat): TCPVect;
begin
  Result := TCPVect(cpvslerpconst(cpVect(Self), cpVect(Target), Angle * DEG2RAD));
end;

class operator TCPVect.Subtract(const A, B: TCPVect): TCPVect;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class function TCPVect.ToAngle(const V: TCPVect): TCPFloat;
begin
  Result := cpvtoangle(cpVect(V)) * RAD2DEG;
end;

function TCPVect.ToAngle: TCPFloat;
begin
  Result := cpvtoangle(cpVect(Self)) * RAD2DEG;
end;

function TCPVect.ToString: String;
begin
  Result := Format('(%.3f, %.3f)', [X, Y]);
end;

class function TCPVect.Unrotate(const V1, V2: TCPVect): TCPVect;
begin
  Result.X := V1.X * V2.X + V1.Y * V2.Y;
  Result.Y := V1.Y * V2.X - V1.X * V2.Y;
end;

function TCPVect.Unrotate(const Other: TCPVect): TCPVect;
begin
  Result.X := X * Other.X + Y * Other.Y;
  Result.Y := Y * Other.X - X * Other.Y;
end;

{ TCPBB }

function CPBBNew(const L, B, R, T: TCPFloat): TCPBB; inline;
begin
  Result.L := L;
  Result.B := B;
  Result.R := R;
  Result.T := T;
end;

class function TCPBB.Contains(const BB, Other: TCPBB): TCPBool;
begin
  Result := (BB.L < Other.L) and (BB.R > Other.R) and (BB.B < Other.B) and (BB.T > Other.T);
end;

function TCPBB.Contains(const Other: TCPBB): TCPBool;
begin
  Result := (L < Other.L) and (R > Other.R) and (B < Other.B) and (T > Other.T);
end;

class function TCPBB.Contains(const BB: TCPBB; const V: TCPVect): TCPBool;
begin
  Result := (BB.L < V.X) and (BB.R > V.X ) and (BB.B < V.Y) and (BB.T > V.Y);
end;

class function TCPBB.ClampVect(const BB: TCPBB; const V: TCPVect): TCPVect;
begin
  Result := TCPVect(cpBBClampVect(cpBB(BB), cpVect(V)));
end;

function TCPBB.ClampVect(const V: TCPVect): TCPVect;
begin
  Result := TCPVect(cpBBClampVect(cpBB(Self), cpVect(V)));
end;

function TCPBB.Contains(const V: TCPVect): TCPBool;
begin
  Result := (L < V.X) and (R > V.X ) and (B < V.Y) and (T > V.Y);
end;

class function TCPBB.Create(const L, B, R, T: TCPFloat): TCPBB;
begin
  Result.L := L;
  Result.B := B;
  Result.R := R;
  Result.T := T;
end;

class function TCPBB.Expand(const BB: TCPBB; const V: TCPVect): TCPBB;
begin
  Result := CPBBNew(TCPMath.Min(BB.L, V.X), TCPMath.Min(BB.B, V.Y),
    TCPMath.Max(BB.R, V.X), TCPMath.Max(BB.T, V.Y));
end;

function TCPBB.Expand(const V: TCPVect): TCPBB;
begin
  Result := CPBBNew(TCPMath.Min(L, V.X), TCPMath.Min(B, V.Y),
    TCPMath.Max(R, V.X), TCPMath.Max(T, V.Y));
end;

function TCPBB.GetHeight: TCPFloat;
begin
  Result := Abs(B - T);
end;

function TCPBB.GetWidth: TCPFloat;
begin
  Result := R - L;
end;

class function TCPBB.Intersects(const A, B: TCPBB): TCPBool;
begin
  Result := (A.L <= B.R) and (B.L <= A.R) and (A.B <= B.T) and (B.B <= A.T);
end;

function TCPBB.Intersects(const Other: TCPBB): TCPBool;
begin
  Result := (L <= Other.R) and (Other.L <= R) and (B <= Other.T) and (Other.B <= T);
end;

class function TCPBB.Merge(const A, B: TCPBB): TCPBB;
begin
  Result := CPBBNew(TCPMath.Min(A.L, B.L), TCPMath.Min(A.B, B.B),
    TCPMath.Max(A.R, B.R), TCPMath.Max(A.T, B.T));
end;

function TCPBB.Merge(const Other: TCPBB): TCPBB;
begin
  Result := CPBBNew(TCPMath.Min(L, Other.L), TCPMath.Min(B, Other.B),
    TCPMath.Max(R, Other.R), TCPMath.Max(T, Other.T));
end;

class function TCPBB.WrapVect(const BB: TCPBB; const V: TCPVect): TCPVect;
begin
  Result := TCPVect(cpBBWrapVect(cpBB(BB), cpVect(V)));
end;

function TCPBB.WrapVect(const V: TCPVect): TCPVect;
begin
  Result := TCPVect(cpBBWrapVect(cpBB(Self), cpVect(V)));
end;

{ TCPBody }

procedure BodyVelocityFunc(Body: PcpBody; Gravity: cpVect; Damping: cpFloat;
  DT:cpFloat ); cdecl;
var
  B: TCPBody;
begin
  Assert(Assigned(Body));
  B := Body.Data;
  if Assigned(B) and Assigned(B.FOnUpdateVelocity) then
    B.FOnUpdateVelocity(B, TCPVect(Gravity), Damping, DT);
end;

procedure BodyPositionFunc(Body: PcpBody; DT: cpFloat); cdecl;
var
  B: TCPBody;
begin
  Assert(Assigned(Body));
  B := Body.Data;
  if Assigned(B) and Assigned(B.FOnUpdatePosition) then
    B.FOnUpdatePosition(B, DT);
end;

procedure TCPBody.Activate;
begin
  cpBodyActivate(FHandle);
end;

procedure TCPBody.ApplyForce(const Force, RelativeOffset: TCPVect);
begin
  cpBodyApplyForce(FHandle, cpVect(Force), cpVect(RelativeOffset));
end;

procedure TCPBody.ApplyImpulse(const Impulse, RelativeOffset: TCPVect);
begin
  cpBodyApplyImpulse(FHandle, cpVect(Impulse), cpVect(RelativeOffset));
end;

constructor TCPBody.Create(const Mass, Moment: TCPFloat);
begin
  inherited Create;
  FHandle := @FSettings;
  cpBodyInit(FHandle, Mass, Moment);
  FSettings.data := Self;
end;

constructor TCPBody.CreateStatic(const Body: cpBody);
begin
  inherited Create;
  FSettings := Body;
  FHandle := @FSettings.space.staticBody;;
  FSettings.data := Self;
end;

procedure TCPBody.UpdatePosition(const DT: TCPFloat);
begin
  cpBodyUpdatePosition(FHandle, DT);
end;

procedure TCPBody.UpdateVelocity(const Gravity: TCPVect; const Damping,
  DT: TCPFloat);
begin
  cpBodyUpdateVelocity(FHandle, cpVect(Gravity), Damping, DT);
end;

destructor TCPBody.Destroy;
begin
  FSettings.data := nil;
  if (FHandle <> @FSettings.space.staticBody) then
    cpBodyDestroy(FHandle);
  inherited;
end;

function TCPBody.GetAngle: TCPFloat;
begin
  Result := FSettings.a * RAD2DEG;
end;

function TCPBody.GetForce: TCPVect;
begin
  Result := TCPVect(FSettings.f);
end;

function TCPBody.GetPosition: TCPVect;
begin
  Result := TCPVect(FSettings.p);
end;

function TCPBody.GetRotation: TCPVect;
begin
  Result := TCPVect(FSettings.rot);
end;

function TCPBody.GetRotationalVelocity: TCPFloat;
begin
  Result := FSettings.w * RAD2DEG;
end;

function TCPBody.GetVelocity: TCPVect;
begin
  Result := TCPVect(FSettings.v);
end;

function TCPBody.GetVelocityBias: TCPVect;
begin
  Result := TCPVect(FSettings.v_bias);
end;

function TCPBody.IsRogue: TCPBool;
begin
  Result := cpBodyIsRogue(FHandle);
end;

function TCPBody.IsSleeping: TCPBool;
begin
  Result := cpBodyIsSleeping(FHandle);
end;

function TCPBody.IsStatic: TCPBool;
var
  Space: PcpSpace;
begin
  Space := FSettings.space;
  Result := Assigned(Space) and (FHandle = @Space.staticBody);
end;

function TCPBody.LocalToWorld(const V: TCPVect): TCPVect;
begin
  Result := TCPVect(cpBodyLocal2World(FHandle, cpVect(V)));
end;

class function TCPBody.MomentForCircle(const Mass, R1, R2: TCPFloat;
  const Offset: TCPVect): TCPFloat;
begin
  Result := cpMomentForCircle(Mass, R1, R2, cpVect(Offset));
end;

class function TCPBody.MomentForBox(const Mass, Width,
  Height: TCPFloat): TCPFloat;
begin
  Result := cpMomentForBox(Mass, Width, Height);
end;

class function TCPBody.MomentForPolygon(const Mass: TCPFloat;
  const Vertices: array of TCPVect; const Offset: TCPVect): TCPFloat;
begin
  Result := cpMomentForPoly(Mass, Length(Vertices), @Vertices[0], cpVect(Offset));
end;

class function TCPBody.MomentForSegment(const Mass: TCPFloat; const A,
  B: TCPVect): TCPFloat;
begin
  Result := cpMomentForSegment(Mass, cpVect(A), cpVect(B));
end;

procedure TCPBody.ResetForces;
begin
  cpBodyResetForces(FHandle);
end;

procedure TCPBody.SetAngle(const Value: TCPFloat);
begin
  cpBodySetAngle(FHandle, Value * DEG2RAD);
end;

procedure TCPBody.SetForce(const Value: TCPVect);
begin
  FSettings.f := cpVect(Value);
end;

procedure TCPBody.SetMass(const Value: TCPFloat);
begin
  cpBodySetMass(FHandle, Value);
end;

procedure TCPBody.SetMoment(const Value: TCPFloat);
begin
  cpBodySetMoment(FHandle, Value);
end;

procedure TCPBody.SetOnUpdatePosition(const Value: TCPBodyPositionEvent);
begin
  FOnUpdatePosition := Value;
  if Assigned(Value) then
    FSettings.position_func := BodyPositionFunc
  else
    FSettings.position_func := cpBodyUpdatePosition;
end;

procedure TCPBody.SetOnUpdateVelocity(const Value: TCPBodyVelocityEvent);
begin
  FOnUpdateVelocity := Value;
  if Assigned(Value) then
    FSettings.velocity_func := BodyVelocityFunc
  else
    FSettings.velocity_func := cpBodyUpdateVelocity;
end;

procedure TCPBody.SetPosition(const Value: TCPVect);
begin
  FSettings.p := cpVect(Value);
end;

procedure TCPBody.SetRotationalVelocity(const Value: TCPFloat);
begin
  FSettings.w := Value * DEG2RAD;
end;

procedure TCPBody.SetVelocity(const Value: TCPVect);
begin
  FSettings.v := cpVect(Value);
end;

procedure TCPBody.SetVelocityBias(const Value: TCPVect);
begin
  FSettings.v_bias := cpVect(Value);
end;

procedure TCPBody.Sleep;
begin
  cpBodySleep(FHandle);
end;

procedure TCPBody.Slew(const Pos: TCPVect; const DT: TCPFloat);
begin
  cpBodySlew(FHandle, cpVect(Pos), DT);
end;

function TCPBody.WorldToLocal(const V: TCPVect): TCPVect;
begin
  Result := TCPVect(cpBodyWorld2Local(FHandle, cpVect(V)));
end;

{ TCPShape }

function TCPShape.CacheBoundingBox: TCPBB;
begin
  Result := TCPBB(cpShapeCacheBB(FHandle));
end;

constructor TCPShape.Create(const Body: TCPBody);
begin
  Assert(Assigned(Body));
  inherited Create;
  { We can safely use Shape as our handle here, because all 3 shape types
    have a regular cpShape as the first record field. }
  FHandle := @FSettings;
  FBody := Body;
end;

destructor TCPShape.Destroy;
begin
  FSettings.data := nil;
  cpShapeDestroy(FHandle);
  inherited;
end;

function TCPShape.GetBoundingBox: TCPBB;
begin
  Result := TCPBB(FSettings.bb);
end;

function TCPShape.GetSurfaceVelocity: TCPVect;
begin
  Result := TCPVect(FSettings.surface_v);
end;

function TCPShape.PointQuery(const P: TCPVect): TCPBool;
begin
  Result := cpShapePointQuery(FHandle, cpVect(P));
end;

class procedure TCPShape.ResetIdCounter;
begin
  cpResetShapeIdCounter;
end;

function TCPShape.SegmentQuery(const A, B: TCPVect;
  out Info: TCPSegmentQueryInfo): TCPBool;
var
  NativeInfo: zglChipmunk.cpSegmentQueryInfo;
begin
  Result := cpShapeSegmentQuery(FHandle, cpVect(A), cpVect(B), @NativeInfo);
  {$HINTS OFF}
  Move(NativeInfo.t, Info.Distance, SizeOf(TCPSegmentQueryInfo) - SizeOf(TCPShape));
  {$HINTS ON}
  if Assigned(NativeInfo.shape) then
    Info.Shape := NativeInfo.shape.data
  else
    Info.Shape := nil;
end;

procedure TCPShape.SetSurfaceVelocity(const Value: TCPVect);
begin
  FSettings.surface_v := cpVect(Value);
end;

{ TCPCircleShape }

constructor TCPCircleShape.Create(const Body: TCPBody; const Radius: TCPFloat;
  const Offset: TCPVect);
begin
  inherited Create(Body);
  cpCircleShapeInit(@FSettings, Body.Handle, Radius, cpVect(Offset));
  FSettings.data := Self;
end;

class function TCPCircleShape.ShapeType: TCPShapeType;
begin
  Result := stCircle;
end;

procedure TCPCircleShape.UnsafeSetRadius(const Radius: TCPFloat);
begin
  { The cpCircleShareSetRadius API is not part of zglChipmunk, so I implemented
    it myself. }
  FCircleSettings.R := Radius;
end;

{ TCPSegmentShape }

constructor TCPSegmentShape.Create(const Body: TCPBody; const V1, V2: TCPVect;
  const Radius: TCPFloat);
begin
  inherited Create(Body);
  cpSegmentShapeInit(@FSettings, Body.Handle, cpVect(V1), cpVect(V2), Radius);
  FSettings.data := Self;
end;

class function TCPSegmentShape.ShapeType: TCPShapeType;
begin
  Result := stSegment;
end;

procedure TCPSegmentShape.UnsafeSetEndPoints(const A, B: TCPVect);
begin
  { The cpSegmentShapeSetEndpoints API is not part of zglChipmunk, so I
    implemented it myself. }
  FSegmentSettings.A := A;
  FSegmentSettings.B := B;
  FSegmentSettings.N := (B - A).Normalize.Perp;
end;

{ TCPPolyShape }

constructor TCPPolyShape.Create(const Body: TCPBody;
  const Verts: array of TCPVect; const Offset: TCPVect);
begin
  inherited Create(Body);
  cpPolyShapeInit(@FSettings, Body.Handle, Length(Verts), @Verts[0], cpVect(Offset));
  FSettings.data := Self;
end;

constructor TCPPolyShape.CreateBox(const Body: TCPBody; const Width,
  Height: TCPFloat);
begin
  inherited Create(Body);
  cpBoxShapeInit(@FSettings, Body.Handle, Width, Height);
  FSettings.data := Self;
end;

function TCPPolyShape.GetFirstTV: PCPVect;
begin
  Result := FPolySettings.TVerts;
end;

function TCPPolyShape.GetFirstVertex: PCPVect;
begin
  Result := FPolySettings.Verts;
end;

function TCPPolyShape.GetTV(const Index: Integer): TCPVect;
var
  P: PCPVect;
begin
  P := FPolySettings.TVerts;
  Inc(P, Index);
  Result := P^;
end;

function TCPPolyShape.GetVertex(const Index: Integer): TCPVect;
begin
  Result := TCPVect(cpPolyShapeGetVert(FHandle, Index));
end;

function TCPPolyShape.GetVertexCount: Integer;
begin
  Result := cpPolyShapeGetNumVerts(FHandle);
end;

class function TCPPolyShape.ShapeType: TCPShapeType;
begin
  Result := stPoly;
end;

{ TCPSpace }

type
  TDefaultCollisionHandler = class
  public
    function AlwaysCollide(const Space: TCPSpace;
      const Arbiter: PCPArbiter; const Data: Pointer): TCPBool;

    procedure Nothing(const Space: TCPSpace;
      const Arbiter: PCPArbiter; const Data: Pointer);
  end;

type
  TCollisionHandler = record
    A: TCPCollisionType;
    B: TCPCollisionType;
    OnBegin: TCPCollisionBeginEvent;
    OnPreSolve: TCPCollisionPreSolveEvent;
    OnPostSolve: TCPCollisionPostSolveEvent;
    OnSeparate: TCPCollisionSeparateEvent;
    Space: TCPSpace;
    Data: Pointer;
  end;
  PCollisionHandler = ^TCollisionHandler;

var
  DefaultCollisionHandler: TCollisionHandler;
  DefaultCollisionHandlerObj: TDefaultCollisionHandler;

type
  TPostStepCallback = record
    OnPostStep: TCPPostStepEvent;
    Obj: Pointer;
    Data: Pointer;
    Space: TCPSpace;
  end;
  PPostStepCallback = ^TPostStepCallback;

function CP_HASH_PAIR(const A, B: TCPCollisionType): cpHashValue; inline;
begin
  Result := A xor B;
end;

function DefaultCollisionBeginFunc(Arb: zglChipmunk.PcpArbiter; Space: PcpSpace; Data: Pointer): TCPBool; cdecl;
var
  S: TCPSpace absolute Data;
begin
  Result := S.FDefaultOnBegin(S, PCPArbiter(Arb), S.FDefaultData);
end;

function DefaultCollisionPreSolveFunc(Arb: zglChipmunk.PcpArbiter; Space: PcpSpace; Data: Pointer): TCPBool; cdecl;
var
  S: TCPSpace absolute Data;
begin
  Result := S.FDefaultOnPreSolve(S, PCPArbiter(Arb), S.FDefaultData);
end;

procedure DefaultCollisionPostSolveFunc(Arb: zglChipmunk.PcpArbiter; Space: PcpSpace; Data: Pointer); cdecl;
var
  S: TCPSpace absolute Data;
begin
  S.FDefaultOnPostSolve(S, PCPArbiter(Arb), S.FDefaultData);
end;

procedure DefaultCollisionSeparateFunc(Arb: zglChipmunk.PcpArbiter; Space: PcpSpace; Data: Pointer); cdecl;
var
  S: TCPSpace absolute Data;
begin
  S.FDefaultOnSeparate(S, PCPArbiter(Arb), S.FDefaultData);
end;

function CollisionBeginFunc(Arb: zglChipmunk.PcpArbiter; Space: PcpSpace; Data: Pointer): TCPBool; cdecl;
var
  Handler: PCollisionHandler absolute Data;
begin
  Result := Handler.OnBegin(Handler.Space, PCPArbiter(Arb), Handler.Data);
end;

function CollisionPreSolveFunc(Arb: zglChipmunk.PcpArbiter; Space: PcpSpace; Data: Pointer): TCPBool; cdecl;
var
  Handler: PCollisionHandler absolute Data;
begin
  Result := Handler.OnPreSolve(Handler.Space, PCPArbiter(Arb), Handler.Data);
end;

procedure CollisionPostSolveFunc(Arb: zglChipmunk.PcpArbiter; Space: PcpSpace; Data: Pointer); cdecl;
var
  Handler: PCollisionHandler absolute Data;
begin
  Handler.OnPostSolve(Handler.Space, PCPArbiter(Arb), Handler.Data);
end;

procedure CollisionSeparateFunc(Arb: zglChipmunk.PcpArbiter; Space: PcpSpace; Data: Pointer); cdecl;
var
  Handler: PCollisionHandler absolute Data;
begin
  Handler.OnSeparate(Handler.Space, PCPArbiter(Arb), Handler.Data);
end;

function CollisionHandlerSetEqual(Ptr, Elt: Pointer): TCPBool; cdecl;
var
  Check: PCollisionHandler absolute Ptr;
  Pair: PCollisionHandler absolute Elt;
begin
  Result := ((Check.A = Pair.A) and (Check.B = Pair.B))
         or ((Check.B = Pair.A) and (Check.A = Pair.B));
end;

function CollisionHandlerSetTrans(Ptr: Pointer; Data: Pointer): Pointer; cdecl;
var
  Handler: PCollisionHandler absolute Ptr;
  Copy: PCollisionHandler;
begin
  GetMem(Copy, SizeOf(TCollisionHandler));
  Copy^ := Handler^;
  Result := Copy;
end;

function PostStepSetEqual(Ptr, Elt: Pointer): TCPBool; cdecl;
var
  A: PPostStepCallback absolute Ptr;
  B: PPostStepCallback absolute Elt;
begin
  Result := (A.Obj = B.Obj);
end;

function PostStepSetTrans(Ptr: Pointer; Data: Pointer): Pointer; cdecl;
var
  Callback: PPostStepCallback absolute Ptr;
  Copy: PPostStepCallback;
begin
  GetMem(Copy, SizeOf(TPostStepCallback));
  Copy^ := Callback^;
  Result := Copy;
end;

procedure PostStepFunc(Space: PcpSpace; Obj: Pointer; Data: Pointer); cdecl;
var
  Callback: PPostStepCallback absolute Data;
begin
  Callback.OnPostStep(Callback.Space, Callback.Obj, Callback.Data);
end;

procedure SpacePointQuery(Shape: PcpShape; Data: Pointer); cdecl;
var
  Space: TCPSpace absolute Data;
begin
  if Assigned(Shape) then
    Space.FOnPointQuery(Shape.data, Space.FCallbackData)
  else
    Space.FOnPointQuery(nil, Space.FCallbackData);
end;

procedure SpaceSegmentQuery(Shape: PcpShape; T: cpFloat; N: cpVect; Data: Pointer); cdecl;
var
  Space: TCPSpace absolute Data;
begin
  if Assigned(Shape) then
    Space.FOnSegmentQuery(Shape.data, T, TCPVect(N), Space.FCallbackData)
  else
    Space.FOnSegmentQuery(nil, T, TCPVect(N), Space.FCallbackData);
end;

procedure BodyIterator(Body: PcpBody; Data: Pointer); cdecl;
var
  Space: TCPSpace absolute Data;
begin
  Space.FOnBody(Space, Body.data, Space.FUserData);
end;

procedure SpaceHashIterator(Obj: Pointer; Data: Pointer); cdecl;
var
  Space: TCPSpace absolute Data;
  Shape: PcpShape absolute Obj;
begin
  Space.FOnShape(Space, Shape.data, Space.FUserData);
end;

procedure ArrayIterator(Ptr: Pointer; Data: Pointer); cdecl;
var
  Space: TCPSpace absolute Data;
begin
  Space.FOnArbiter(Space, Ptr, Space.FUserData);
end;

procedure FreeWrap(Elt: Pointer; Data: Pointer); cdecl;
begin
  FreeMem(Elt);
end;

function TCPSpace.AddBody(const Body: TCPBody): TCPBody;
begin
  Assert(Assigned(Body));
  cpSpaceAddBody(FHandle, Body.Handle);
  Result := Body;
end;

procedure TCPSpace.AddCollisionHandler(const A, B: TCPCollisionType;
  const OnBegin: TCPCollisionBeginEvent;
  const OnPreSolve: TCPCollisionPreSolveEvent;
  const OnPostSolve: TCPCollisionPostSolveEvent;
  const OnSeparate: TCPCollisionSeparateEvent;
  const Data: Pointer);
var
  Handler: TCollisionHandler;
  CBegin: cpCollisionBeginFunc;
  CPreSolve: cpCollisionPreSolveFunc;
  CPostSolve: cpCollisionPostSolveFunc;
  CSeparate: cpCollisionSeparateFunc;
  Copy: PCollisionHandler;
begin
  InternalRemoveCollisionHandler(A, B);
  Handler.A := A;
  Handler.A := B;

  if Assigned(OnBegin) then
  begin
    Handler.OnBegin := OnBegin;
    CBegin := CollisionBeginFunc;
  end
  else
  begin
    Handler.OnBegin := DefaultCollisionHandlerObj.AlwaysCollide;
    CBegin := nil;
  end;

  if Assigned(OnPreSolve) then
  begin
    Handler.OnPreSolve := OnPreSolve;
    CPreSolve := CollisionPreSolveFunc;
  end
  else
  begin
    Handler.OnPreSolve := DefaultCollisionHandlerObj.AlwaysCollide;
    CPreSolve := nil;
  end;

  if Assigned(OnPostSolve) then
  begin
    Handler.OnPostSolve := OnPostSolve;
    CPostSolve := CollisionPostSolveFunc;
  end
  else
  begin
    Handler.OnPostSolve := DefaultCollisionHandlerObj.Nothing;
    CPostSolve := nil;
  end;

  if Assigned(OnSeparate) then
  begin
    Handler.OnSeparate := OnSeparate;
    CSeparate := CollisionSeparateFunc;
  end
  else
  begin
    Handler.OnSeparate := DefaultCollisionHandlerObj.Nothing;
    CSeparate := nil;
  end;

  if (FCollisionHandlerSet = nil) then
  begin
    FCollisionHandlerSet := cpHashSetNew(0, CollisionHandlerSetEqual,
      CollisionHandlerSetTrans);
    FCollisionHandlerSet.default_value := @DefaultCollisionHandler;
  end;

  Copy := cpHashSetInsert(FCollisionHandlerSet, CP_HASH_PAIR(A, B), @Handler, nil);
  Copy.Space := Self;
  Copy.Data := Data;

  cpSpaceAddCollisionHandler(FHandle, A, B, CBegin, CPreSolve, CPostSolve,
    CSeparate, Copy);
end;

function TCPSpace.AddConstraint(const Constraint: TCPConstraint): TCPConstraint;
begin
  Assert(Assigned(Constraint));
  cpSpaceAddConstraint(FHandle, Constraint.Handle);
  Result := Constraint;
end;

procedure TCPSpace.AddPostStepCallback(const OnPostStep: TCPPostStepEvent;
  const Obj, Data: Pointer);
var
  Callback: TPostStepCallback;
  Copy: PPostStepCallback;
begin
  if (FPostStepCallbacks = nil) then
    FPostStepCallbacks := cpHashSetNew(0, PostStepSetEqual, PostStepSetTrans);

  Callback.OnPostStep := OnPostStep;
  Callback.Obj := Obj;
  Callback.Data := Data;

  {$HINTS OFF}
  Copy := cpHashSetInsert(FPostStepCallbacks, cpHashValue(Obj), @Callback, nil);
  {$HINTS ON}
  Copy.Space := Self;

  if Assigned(OnPostStep) then
    cpSpaceAddPostStepCallback(FHandle, PostStepFunc, Obj, Copy)
  else
    cpSpaceAddPostStepCallback(FHandle, nil, Obj, Copy);
end;

function TCPSpace.AddShape(const Shape: TCPShape): TCPShape;
begin
  Assert(Assigned(Shape));
  cpSpaceAddShape(FHandle, Shape.Handle);
  Result := Shape;
end;

function TCPSpace.AddStaticShape(const Shape: TCPShape): TCPShape;
begin
  Assert(Assigned(Shape));
  cpSpaceAddStaticShape(FHandle, Shape.Handle);
  Result := Shape;
end;

constructor TCPSpace.Create;
begin
  inherited Create;
  cpSpaceInit(@FSettings);
  FHandle := @FSettings;
  FStaticBody := TCPBody.CreateStatic(FSettings.staticBody);
end;

destructor TCPSpace.Destroy;
begin
  if Assigned(FCollisionHandlerSet) then
  begin
    cpHashSetEach(FCollisionHandlerSet, FreeWrap, nil);
    cpHashSetFree(FCollisionHandlerSet);
  end;

  if Assigned(FPostStepCallbacks) then
  begin
    cpHashSetEach(FPostStepCallbacks, FreeWrap, nil);
    cpHashSetFree(FPostStepCallbacks);
  end;

  FStaticBody.Free;
  cpSpaceDestroy(FHandle);
  inherited;
end;

procedure TCPSpace.ForEachArbiter(const OnArbiter: TCPForEachArbiterEvent;
  const Data: Pointer);
begin
  if Assigned(OnArbiter) then
  begin
    FOnArbiter := OnArbiter;
    FUserData := Data;
    cpArrayEach(FSettings.arbiters, ArrayIterator, Self);
  end;
end;

procedure TCPSpace.ForEachBody(const OnBody: TCPForEachBodyEvent;
  const Data: Pointer);
begin
  if Assigned(OnBody) then
  begin
    FOnBody := OnBody;
    FUserData := Data;
    cpSpaceEachBody(FHandle, BodyIterator, Self);
  end;
end;

procedure TCPSpace.ForEachShape(const OnShape: TCPForEachShapeEvent;
  const Data: Pointer);
begin
  HashEach(FSettings.activeShapes, OnShape, Data);
end;

procedure TCPSpace.ForEachStaticShape(const OnShape: TCPForEachShapeEvent;
  const Data: Pointer);
begin
  HashEach(FSettings.staticShapes, OnShape, Data);
end;

function TCPSpace.GetArbiter(const Index: Integer): PCPArbiter;
var
  P: PPointer;
begin
  Assert(Assigned(FSettings.arbiters));
  P := PPointer(FSettings.arbiters.arr);
  Inc(P, Index);
  Result := P^;
end;

function TCPSpace.GetArbiterCount: Integer;
begin
  Assert(Assigned(FSettings.arbiters));
  Result := FSettings.arbiters.num;
end;

function TCPSpace.GetBody(const Index: Integer): TCPBody;
var
  P: PPointer;
  NativeBody: PcpBody;
begin
  Assert(Assigned(FSettings.bodies));
  P := PPointer(FSettings.bodies.arr);
  Inc(P, Index);
  NativeBody := P^;
  if Assigned(NativeBody) then
    Result := NativeBody.data
  else
    Result := nil;
end;

function TCPSpace.GetBodyCount: Integer;
begin
  Assert(Assigned(FSettings.bodies));
  Result := FSettings.bodies.num;
end;

function TCPSpace.GetConstraint(const Index: Integer): TCPConstraint;
var
  P: PPointer;
  NativeConstraint: PcpConstraint;
begin
  Assert(Assigned(FSettings.constraints));
  P := PPointer(FSettings.constraints.arr);
  Inc(P, Index);
  NativeConstraint := P^;
  if Assigned(NativeConstraint) then
    Result := NativeConstraint.data
  else
    Result := nil;
end;

function TCPSpace.GetConstraintCount: Integer;
begin
  Assert(Assigned(FSettings.constraints));
  Result := FSettings.constraints.num;
end;

function TCPSpace.GetGravity: TCPVect;
begin
  Result := TCPVect(FSettings.gravity);
end;

procedure TCPSpace.HashEach(const Hash: PcpSpaceHash;
  const OnShape: TCPForEachShapeEvent; const Data: Pointer);
begin
  if Assigned(OnShape) then
  begin
    FOnShape := OnShape;
    FUserData := Data;
    cpSpaceHashEach(Hash, SpaceHashIterator, Self);
  end;
end;

procedure TCPSpace.InternalRemoveCollisionHandler(const A, B: TCPCollisionType);
var
  OldHandler: PCollisionHandler;
begin
  if Assigned(FCollisionHandlerSet) then
  begin
    OldHandler := cpHashSetRemove(FCollisionHandlerSet, CP_HASH_PAIR(A, B), nil);
    FreeMem(OldHandler);
  end;
end;

function TCPSpace.PointQuery(const P: TCPVect; const Layers: TCPLayers;
  const Group: TCPGroup): TCPShape;
var
  S: PcpShape;
begin
  S := cpSpacePointQueryFirst(FHandle, cpVect(P), Layers, Group);
  if Assigned(S) then
    Result := S.data
  else
    Result := nil;
end;

procedure TCPSpace.PointQuery(const P: TCPVect; const Layers: TCPLayers;
  const Group: TCPGroup; const OnHit: TCPPointQueryEvent; const Data: Pointer);
begin
  if Assigned(OnHit) then
  begin
    FOnPointQuery := OnHit;
    FCallbackData := Data;
    cpSpacePointQuery(FHandle, cpVect(P), Layers, Group, SpacePointQuery, Self);
  end;
end;

procedure TCPSpace.RehashStatic;
begin
  cpSpaceRehashStatic(FHandle);
end;

procedure TCPSpace.RemoveBody(const Body: TCPBody);
begin
  Assert(Assigned(Body));
  cpSpaceRemoveBody(FHandle, Body.Handle);
end;

procedure TCPSpace.RemoveCollisionHandler(const A, B: TCPCollisionType);
begin
  InternalRemoveCollisionHandler(A, B);
  cpSpaceRemoveCollisionHandler(FHandle, A, B);
end;

procedure TCPSpace.RemoveConstraint(const Constraint: TCPConstraint);
begin
  Assert(Assigned(Constraint));
  cpSpaceRemoveConstraint(FHandle, Constraint.Handle);
end;

procedure TCPSpace.RemoveShape(const Shape: TCPShape);
begin
  Assert(Assigned(Shape));
  cpSpaceRemoveShape(FHandle, Shape.Handle);
end;

procedure TCPSpace.RemoveStaticShape(const Shape: TCPShape);
begin
  Assert(Assigned(Shape));
  cpSpaceRemoveStaticShape(FHandle, Shape.Handle);
end;

procedure TCPSpace.ResizeActiveHash(const Dim: TCPFloat; const Count: Integer);
begin
  cpSpaceResizeActiveHash(FHandle, Dim, Count);
end;

procedure TCPSpace.ResizeStaticHash(const Dim: TCPFloat; const Count: Integer);
begin
  cpSpaceResizeStaticHash(FHandle, Dim, Count);
end;

procedure TCPSpace.SegmentQuery(const A, B: TCPVect; const Layers: TCPLayers;
  const Group: TCPGroup; const OnHit: TCPSegmentQueryEvent;
  const Data: Pointer);
begin
  if Assigned(OnHit) then
  begin
    FOnSegmentQuery := OnHit;
    FCallbackData := Data;
    cpSpaceSegmentQuery(FHandle, cpVect(A), cpVect(B), Layers, Group, SpaceSegmentQuery, Self);
  end;
end;

function TCPSpace.SegmentQuery(const A, B: TCPVect; const Layers: TCPLayers;
  const Group: TCPGroup; out Info: TCPSegmentQueryInfo): TCPShape;
var
  Shape: PcpShape;
  NativeInfo: zglChipmunk.cpSegmentQueryInfo;
begin
  Shape := cpSpaceSegmentQueryFirst(FHandle, cpVect(A), cpVect(B), Layers,
    Group, NativeInfo);
  {$HINTS OFF}
  Move(NativeInfo.t, Info.Distance, SizeOf(TCPSegmentQueryInfo) - SizeOf(TCPShape));
  {$HINTS ON}
  if Assigned(Shape) then
    Result := Shape.data
  else
    Result := nil;
  Info.Shape := Result;
end;

procedure TCPSpace.SetDefaultCollisionHandler(
  const OnBegin: TCPCollisionBeginEvent;
  const OnPreSolve: TCPCollisionPreSolveEvent;
  const OnPostSolve: TCPCollisionPostSolveEvent;
  const OnSeparate: TCPCollisionSeparateEvent; const Data: Pointer);
var
  CBegin: cpCollisionBeginFunc;
  CPreSolve: cpCollisionPreSolveFunc;
  CPostSolve: cpCollisionPostSolveFunc;
  CSeparate: cpCollisionSeparateFunc;
begin
  FDefaultOnBegin := OnBegin;
  FDefaultOnPreSolve := OnPreSolve;
  FDefaultOnPostSolve := OnPostSolve;
  FDefaultOnSeparate := OnSeparate;
  FDefaultData := Data;

  if Assigned(OnBegin) then
    CBegin := DefaultCollisionBeginFunc
  else
    CBegin := nil;

  if Assigned(OnPreSolve) then
    CPreSolve := DefaultCollisionPreSolveFunc
  else
    CPreSolve := nil;

  if Assigned(OnPostSolve) then
    CPostSolve := DefaultCollisionPostSolveFunc
  else
    CPostSolve := nil;

  if Assigned(OnSeparate) then
    CSeparate := DefaultCollisionSeparateFunc
  else
    CSeparate := nil;

  cpSpaceSetDefaultCollisionHandler(FHandle, CBegin, CPreSolve, CPostSolve,
    CSeparate, Self);
end;

procedure TCPSpace.SetGravity(const Value: TCPVect);
begin
  FSettings.gravity := cpVect(Value);
end;

procedure TCPSpace.Step(const DT: TCPFloat);
begin
  cpSpaceStep(FHandle, DT);
end;

{ TDefaultCollisionHandler }

function TDefaultCollisionHandler.AlwaysCollide(const Space: TCPSpace;
  const Arbiter: PCPArbiter; const Data: Pointer): TCPBool;
begin
  Result := True;
end;

procedure TDefaultCollisionHandler.Nothing(const Space: TCPSpace;
  const Arbiter: PCPArbiter; const Data: Pointer);
begin
  { Nothing }
end;

{ TCPConstraint }

constructor TCPConstraint.Create(const A, B: TCPBody);
begin
  Assert(Assigned(A));
  Assert(Assigned(B));
  inherited Create;
  FA := A;
  FB := B;
  FHandle := @FSettings;
end;

destructor TCPConstraint.Destroy;
begin
  FSettings.data := nil;
  cpConstraintDestroy(FHandle);
  inherited;
end;

{ TCPPinJoint }

class function TCPPinJoint.ConstraintType: TCPConstraintType;
begin
  Result := ctPinJoint;
end;

constructor TCPPinJoint.Create(const A, B: TCPBody; const Anchor1,
  Anchor2: TCPVect);
begin
  inherited Create(A, B);
  cpPinJointInit(@FSettings, A.Handle, B.Handle, cpVect(Anchor1), cpVect(Anchor2));
  FSettings.data := Self;
end;

{ TCPSlideJoint }

class function TCPSlideJoint.ConstraintType: TCPConstraintType;
begin
  Result := ctSlideJoint;
end;

constructor TCPSlideJoint.Create(const A, B: TCPBody; const Anchor1,
  Anchor2: TCPVect; const Min, Max: TCPFloat);
begin
  inherited Create(A, B);
  cpSlideJointInit(@FSettings, A.Handle, B.Handle, cpVect(Anchor1),
    cpVect(Anchor2), Min, Max);
  FSettings.data := Self;
end;

{ TCPPivotJoint }

constructor TCPPivotJoint.Create(const A, B: TCPBody; const Pivot: TCPVect);
var
  Anchor1, Anchor2: TCPVect;
begin
  if Assigned(A) then
    Anchor1 := A.WorldToLocal(Pivot)
  else
    Anchor1 := Pivot;

  if Assigned(B) then
    Anchor2 := B.WorldToLocal(Pivot)
  else
    Anchor2 := Pivot;

  Create(A, B, Anchor1, Anchor2);
end;

class function TCPPivotJoint.ConstraintType: TCPConstraintType;
begin
  Result := ctPivotJoint;
end;

constructor TCPPivotJoint.Create(const A, B: TCPBody; const Anchor1,
  Anchor2: TCPVect);
begin
  inherited Create(A, B);
  cpPivotJointInit(@FSettings, A.Handle, B.Handle, cpVect(Anchor1), cpVect(Anchor2));
  FSettings.data := Self;
end;

{ TCPGrooveJoint }

class function TCPGrooveJoint.ConstraintType: TCPConstraintType;
begin
  Result := ctGrooveJoint;
end;

constructor TCPGrooveJoint.Create(const A, B: TCPBody; const GrooveA, GrooveB,
  Anchor2: TCPVect);
begin
  inherited Create(A, B);
  cpGrooveJointInit(@FSettings, A.Handle, B.Handle, cpVect(GrooveA),
    cpVect(GrooveB), cpVect(Anchor2));
  FSettings.data := Self;
end;

procedure TCPGrooveJoint.SetGrooveA(const Value: TCPVect);
begin
  cpGrooveJointSetGrooveA(FHandle, cpVect(Value));
end;

procedure TCPGrooveJoint.SetGrooveB(const Value: TCPVect);
begin
  cpGrooveJointSetGrooveB(FHandle, cpVect(Value));
end;

{ TCPDampedSpring }

function DampedSpringForceFunc(Spring: PcpConstraint; Dist: cpFloat): cpFloat; cdecl;
var
  S: TCPDampedSpring;
begin
  S := Spring.data;
  Result := S.FOnForce(S, Dist);
end;

class function TCPDampedSpring.ConstraintType: TCPConstraintType;
begin
  Result := ctDampedSpring;
end;

constructor TCPDampedSpring.Create(const A, B: TCPBody; const Anchor1,
  Anchor2: TCPVect; const RestLength, Stiffness, Damping: TCPFloat);
begin
  inherited Create(A, B);
  cpDampedSpringInit(@FSettings, A.Handle, B.Handle, cpVect(Anchor1),
    cpVect(Anchor2), RestLength, Stiffness, Damping);
  FSettings.data := Self;
end;

procedure TCPDampedSpring.SetOnForce(const Value: TCPDampedSpringForceEvent);
begin
  FOnForce := Value;
  if Assigned(Value) then
    FDampedSpringSettings.SpringForceFunc := DampedSpringForceFunc
  else
    FDampedSpringSettings.SpringForceFunc := nil;
end;

{ TCPDampedRotarySpring }

class function TCPDampedRotarySpring.ConstraintType: TCPConstraintType;
begin
  Result := ctDampedRotarySpring;
end;

constructor TCPDampedRotarySpring.Create(const A, B: TCPBody; const RestAngle,
  Stiffness, Damping: TCPFloat);
begin
  inherited Create(A, B);
  cpDampedRotarySpringInit(@FSettings, A.Handle, B.Handle, RestAngle * DEG2RAD,
    Stiffness, Damping);
  FSettings.data := Self;
end;

function TCPDampedRotarySpring.GetRestAngle: TCPFloat;
begin
  Result := FDampedRotarySpringSettings.RestAngle * RAD2DEG;
end;

procedure TCPDampedRotarySpring.SetRestAngle(const Value: TCPFloat);
begin
  FDampedRotarySpringSettings.RestAngle := Value * DEG2RAD;
end;

{ TCPRotaryLimitJoint }

class function TCPRotaryLimitJoint.ConstraintType: TCPConstraintType;
begin
  Result := ctRotaryLimitJoint;
end;

constructor TCPRotaryLimitJoint.Create(const A, B: TCPBody; const Min,
  Max: TCPFloat);
begin
  inherited Create(A, B);
  cpRotaryLimitJointInit(@FSettings, A.Handle, B.Handle, Min * DEG2RAD, Max * DEG2RAD);
  FSettings.data := Self;
end;

function TCPRotaryLimitJoint.GetMax: TCPFloat;
begin
  Result := FRotaryLimitJointSettings.Max * RAD2DEG;
end;

function TCPRotaryLimitJoint.GetMin: TCPFloat;
begin
  Result := FRotaryLimitJointSettings.Min * RAD2DEG;
end;

procedure TCPRotaryLimitJoint.SetMax(const Value: TCPFloat);
begin
  FRotaryLimitJointSettings.Max := Value * DEG2RAD;
end;

procedure TCPRotaryLimitJoint.SetMin(const Value: TCPFloat);
begin
  FRotaryLimitJointSettings.Min := Value * DEG2RAD;
end;

{ TCPRatchetJoint }

class function TCPRatchetJoint.ConstraintType: TCPConstraintType;
begin
  Result := ctRatchetJoint;
end;

constructor TCPRatchetJoint.Create(const A, B: TCPBody; const Phase,
  Ratchet: TCPFloat);
begin
  inherited Create(A, B);
  cpRatchetJointInit(@FSettings, A.Handle, B.Handle, Phase, Ratchet);
  FSettings.data := Self;
end;

function TCPRatchetJoint.GetAngle: TCPFloat;
begin
  Result := FRatchetJointSettings.Angle * RAD2DEG;
end;

{ TCPGearJoint }

class function TCPGearJoint.ConstraintType: TCPConstraintType;
begin
  Result := ctGearJoint;
end;

constructor TCPGearJoint.Create(const A, B: TCPBody; const Phase,
  Ratio: TCPFloat);
begin
  inherited Create(A, B);
  cpGearJointInit(@FSettings, A.Handle, B.Handle, Phase * DEG2RAD, Ratio);
  FSettings.data := Self;
end;

{ TCPSimpleMotor }

class function TCPSimpleMotor.ConstraintType: TCPConstraintType;
begin
  Result := ctSimpleMotor;
end;

constructor TCPSimpleMotor.Create(const A, B: TCPBody; const Rate: TCPFloat);
begin
  inherited Create(A, B);
  cpSimpleMotorInit(@FSettings, A.Handle, B.Handle, Rate * DEG2RAD);
  FSettings.data := Self;
end;

function TCPSimpleMotor.GetRate: TCPFloat;
begin
  Result := FSimpleMotorSettings.Rate * RAD2DEG;
end;

procedure TCPSimpleMotor.SetRate(const Value: TCPFloat);
begin
  FSimpleMotorSettings.Rate := Value * DEG2RAD;
end;

{ TCPContact }

function TCPContact.GetNormal: TCPVect;
begin
  Result := TCPVect(FSettings.n);
end;

function TCPContact.GetPoint: TCPVect;
begin
  Result := TCPVect(FSettings.p);
end;

procedure TCPContact.SetNormal(const Value: TCPVect);
begin
  FSettings.n := cpVect(Value);
end;

procedure TCPContact.SetPoint(const Value: TCPVect);
begin
  FSettings.p := cpVect(Value);
end;

{ TCPArbiter }

function TCPArbiter.GetContactPoint(const Index: Integer): PCPContact;
begin
  Result := @FSettings.contacts[Index];
end;

function TCPArbiter.GetNormal(const I: Integer): TCPVect;
begin
  Result := TCPVect(cpArbiterGetNormal(@FSettings, I));
end;

function TCPArbiter.GetPoint(const I: Integer): TCPVect;
begin
  Result := TCPVect(cpArbiterGetPoint(@FSettings, I));
end;

procedure TCPArbiter.GetShapes(out A, B: TCPShape);
var
  AP, BP: PcpShape;
begin
  cpArbiterGetShapes(@FSettings, @AP, @BP);

  if Assigned(AP) then
    A := AP.data
  else
    A := nil;

  if Assigned(BP) then
    B := BP.data
  else
    B := nil;
end;

procedure TCPArbiter.Ignore;
begin
  cpArbiterIgnore(@FSettings);
end;

function TCPArbiter.IsFirstContact: Boolean;
begin
  Result := cpArbiterIsFirstContact(@FSettings);
end;

function TCPArbiter.TotalImpulse: TCPVect;
begin
  Result := TCPVect(cpArbiterTotalImpulse(@FSettings));
end;

function TCPArbiter.TotalImpulseWithFriction: TCPVect;
begin
  Result := TCPVect(cpArbiterTotalImpulseWithFriction(@FSettings));
end;

{ TCPSegmentQueryInfo }

function TCPSegmentQueryInfo.HitDist(const A, B: TCPVect): TCPFloat;
var
  Info: zglChipmunk.cpSegmentQueryInfo;
begin
  Info.shape := Shape.Handle;
  Info.t := Distance;
  Info.n := cpVect(Normal);
  Result := cpSegmentQueryHitDist(cpVect(A), cpVect(B), Info);
end;

function TCPSegmentQueryInfo.HitPoint(const A, B: TCPVect): TCPVect;
var
  Info: zglChipmunk.cpSegmentQueryInfo;
begin
  Info.shape := Shape.Handle;
  Info.t := Distance;
  Info.n := cpVect(Normal);
  Result := TCPVect(cpSegmentQueryHitPoint(cpVect(A), cpVect(B), Info));
end;

initialization
  TCPChipmunk.FInitialized := False;
  DefaultCollisionHandlerObj := TDefaultCollisionHandler.Create;
  FillChar(DefaultCollisionHandler, SizeOf(DefaultCollisionHandler), 0);
  DefaultCollisionHandler.OnBegin := DefaultCollisionHandlerObj.AlwaysCollide;
  DefaultCollisionHandler.OnPreSolve := DefaultCollisionHandlerObj.AlwaysCollide;
  DefaultCollisionHandler.OnPostSolve := DefaultCollisionHandlerObj.Nothing;
  DefaultCollisionHandler.OnSeparate := DefaultCollisionHandlerObj.Nothing;

finalization
  TCPChipmunk.Finalize;
  DefaultCollisionHandlerObj.Free;

end.