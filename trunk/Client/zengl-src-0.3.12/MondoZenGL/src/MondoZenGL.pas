unit MondoZenGL;

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

{@@1. Introduction
  MondoZenGL is a light-weight class wrapper around the excellent
  <exref target="http://zengl.org">ZenGL</exref> library.

  Features:
  * MondoZenGL wraps almost every aspect of ZenGL into a class. All you need to
    use is the MondoZenGL unit. However, you always have access the the ZenGL
    structures if you wish. For example, most classes have a Handle property
    that returns the underlying ZenGL structure. You can use this handle to
    interface with ZenGL directly if you want.
  * MondoZenGL is scene based. The idea is that your application consists of
    multiple scenes (like a startup screen, level select screen and actual
    levels). You descend these scenes from <link TMZScene>, and you set the
    current scene using <link TMZApplication.SetScene>. Demo MZ01 shows an
    example of this.
  * MondoZenGL is a very lightweight layer on top of ZenGL. It has very little
    impact on performance, as you can tell by comparing the FPS of the
    MondoZenGL demos with those of the original demos.
  * MondoZenGL requires FreePascal 2.5.1 or later (version 2.6.0 or later is
    recommended). MondoZenGL also compiles with Delphi 2009 or later.

  MondoZenGL consists of the following main classes and structures.

  Geometry and Collision Detection:
  * <link TMZPoint>
  * <link TMZRect>
  * <link TMZLine>
  * <link TMZCircle>

  Application and Scenes:
  * <link TMZApplication>
  * <link TMZScene>

  Timers:
  * <link TMZTimer>

  Textures and Render Targets:
  * <link TMZTexture>
  * <link TMZRenderTarget>

  Canvas and Drawing:
  * <link TMZCanvas>
  * <link TMZFont>
  * <link TMZDistortionMesh>

  Sprites:
  * <link TMZSprite>
  * <link TMZSpriteEngine>

  Particles:
  * <link TMZParticleEmitter>
  * <link TMZParticleEngine>

  Device Input:
  * <link TMZKeyboard>
  * <link TMZMouse>
  * <link TMZJoystick>

  Sound:
  * <link TMZStaticSound>
  * <link TMZStreamingSound>

  Video:
  * <link TMZVideoStream>

  File IO:
  * <link TMZFile>
  * <link TMZFileSystem>
  * <link TMZZipArchive>
  * <link TMZIniFile>

  Camera Control:
  * <link TMZCamera>

  Utilities:
  * <link TMZMath>
  * <link TMZLog>
  * <link TMZUtils>
  * <link TMZMemory>
  * <link TMZTriangulator> }

{$INCLUDE 'mz_config.cfg'}

interface

{$REGION 'General'}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  Math,

  {$IF Defined(MEMCHECK) and Defined(FPC)}
  //  HeapTrc, (temporarily) disabled. Can create Access Violations on shutdown with new FPC
  {$IFEND}

  {$IFDEF WINDOWS}
  {$IFNDEF FPC}
  Windows,
  {$ENDIF}
  {$ENDIF}

  {$IFDEF iOS}
  iPhoneAll,
  {$ENDIF}

  {$IFDEF USE_ZENGL_STATIC}
  zgl_types,
  zgl_application,
  zgl_camera_2d,
  zgl_collision_2d,
  zgl_file,
  zgl_font,
  zgl_fx,
  zgl_grid_2d,
  zgl_ini,
  zgl_joystick,
  zgl_keyboard,
  zgl_log,
  zgl_main,
  zgl_math_2d,
  zgl_memory,
  zgl_mouse,
  zgl_particles_2d,
  zgl_primitives_2d,
  zgl_render_2d,
  zgl_render_target,
  zgl_resources,
  zgl_screen,
  zgl_sound,
  zgl_sound_wav,
  zgl_sound_ogg,
  zgl_sprite_2d,
  zgl_tiles_2d,
  zgl_text,
  zgl_textures,
  zgl_textures_jpg,
  zgl_textures_png,
  {$IFDEF iOS}
  zgl_textures_pvr,
  {$ENDIF}
  zgl_textures_tga,
  zgl_timers,
  {$IF DEFINED(iOS) or DEFINED(ANDROID)}
  zgl_touch,
  {$IFEND}
  zgl_utils,
  {$IFDEF USE_THEORA}
  zgl_video,
  zgl_video_theora,
  {$ENDIF}
  zgl_window
  {$ELSE}
  zglHeader
  {$ENDIF}
  ;

const
  MONDO_ZENGL_VERSION          = 'ZenGL 0.3';
  MONDO_ZENGL_VERSION_DATE     = '2012.06.13';
  MONDO_ZENGL_VERSION_MAJOR    = 0;
  MONDO_ZENGL_VERSION_MINOR    = 3;
  MONDO_ZENGL_VERSION_REVISION = 0;

const
  REFRESH_RATE_MAXIMUM = 0;
  REFRESH_RATE_DEFAULT = 1;

type
  { Summary:
      Exception class for MondoZenGL specific errors. }
  EMZError = class(Exception);

type
  TSingleArray = array of Single;
  TUTF8StringArray = array of UTF8String;
{$ENDREGION 'General'}

{$REGION 'Geometry and Collision Detection'}
type
  { Summary:
      Function result of <link TMZPoint.Orientation>,
      <link TMZLine.Orientation> and <link TMZMath.Orientation> }
  TMZOrientation = (
    { Summary:
        Point lies to the left of the line. }
    orLeft = ORIENTATION_LEFT,

    { Summary:
        Point lies on the line. }
    orZero = ORIENTATION_ZERO,

    { Summary:
        Point lies to the right of the line. }
    orRight = ORIENTATION_RIGHT);

type
  { Summary:
      A 2D point structure. }
  TMZPoint = record
  public
    { Summary:
        X-coordinate of the point. }
    X: Single;

    { Summary:
        Y-coordinate of the point. }
    Y: Single;
  public
    { Summary:
        Creates an empty point. }
    class function Create: TMZPoint; overload; inline; static;

    { Summary:
        Creates a point.
      Parameters:
        X: X-coordinate of the point.
        Y: Y-coordinate of the point. }
    class function Create(const X, Y: Single): TMZPoint; overload; inline; static;

    { Summary:
        Checks whether point is in the given rectangle.
      Parameters:
        X: X-coordinate of the rectangle to check.
        Y: Y-coordinate of the rectangle to check.
        W: Width of the rectangle to check.
        H: Height of the rectangle to check.
      Returns:
        True if the rectangle contains this point. }
    function InRect(const X, Y, W, H: Single): Boolean; inline;

    { Summary:
        Checks whether point is in the given triangle.
      Parameters:
        P1: First point of the triangle.
        P2: Second point of the triangle.
        P3: Third point of the triangle.
      Returns:
        True if the triangle contains this point. }
    function InTriangle(const P1, P2, P3: TMZPoint): Boolean; inline;

    { Summary:
        Calculates the distance between this point and another point.
      Parameters:
        Other: the other point.
      Returns:
        The distance between the two points. }
    function DistanceTo(const Other: TMZPoint): Single; overload; inline;

    { Summary:
        Calculates the distance between this point and another point.
      Parameters:
        X: X-coordinate of the other point.
        Y: Y-coordinate of the other point.
      Returns:
        The distance between the two points. }
    function DistanceTo(const X, Y: Single): Single; overload; inline;

    { Summary:
        Calculates the squared distance between this point and another point.
      Parameters:
        Other: the other point.
      Returns:
        The squared distance between the two points.
      Remarks:
        Calculating the squared distance is useful if you don't need the actual
        distance, but just want to compare distances. In that case you should
        use SquaredDistanceTo since it is faster than <link DistanceTo>. }
    function SquaredDistanceTo(const Other: TMZPoint): Single; overload; inline;

    { Summary:
        Calculates the squared distance between this point and another point.
      Parameters:
        X: X-coordinate of the other point.
        Y: Y-coordinate of the other point.
      Returns:
        The squared distance between the two points.
      Remarks:
        Calculating the squared distance is useful if you don't need the actual
        distance, but just want to compare distances. In that case you should
        use SquaredDistanceTo since it is faster than <link DistanceTo>. }
    function SquaredDistanceTo(const X, Y: Single): Single; overload; inline;

    { Summary:
        Calculates the angle (in degrees) from this point to another point.
      Parameters:
        Other: the other point.
      Returns:
        The angle (in degrees) of the two points. }
    function AngleTo(const Other: TMZPoint): Single; overload; inline;

    { Summary:
        Calculates the angle (in degrees) from this point to another point.
      Parameters:
        X: X-coordinate of the other point.
        Y: Y-coordinate of the other point.
      Returns:
        The angle (in degrees) of the two points. }
    function AngleTo(const X, Y: Single): Single; overload; inline;

    { Summary:
        Calculates the orientation of this point to a line.
      Parameters:
        X1: X-coordinate of the start point of the line.
        Y1: Y-coordinate of the start point of the line.
        X2: X-coordinate of the end point of the line.
        Y2: Y-coordinate of the end point of the line.
      Returns:
        Whether the point lies to the left, right or on the line. }
    function Orientation(const X1, Y1, X2, Y2: Single): TMZOrientation; inline;

    { Summary:
        Checks whether this point equals another point.
      Parameters:
        Other: the other point.
      Returns:
        True if this point equals Other.
      Remarks:
        Since the coordinates are floating-point numbers, the comparison is
        performed with a little error margin (it uses Math.SameValue).
        The Equal (=) and NotEqual (<>) class operators perform a precise
        comparison. }
    function Equals(const Other: TMZPoint): Boolean; inline;

    { Summary:
        Checks if two points are equal.
      Returns:
        True if the points are equal. False otherwise.
      Remarks:
        Be careful when comparing floating point numbers! This function does
        an exact comparision, so the slightest variation returns False.
        See <link Equals> for a more fuzzy check. }
    class operator Equal(const A, B: TMZPoint): Boolean; inline;

    { Summary:
        Checks if two points are not equal.
      Returns:
        True if the points are not equal. False otherwise.
      Remarks:
        Be careful when comparing floating point numbers! This function does
        an exact comparision, so the slightest variation returns True.
        See <link Equals> for a more fuzzy check. }
    class operator NotEqual(const A, B: TMZPoint): Boolean; inline;
  end;
  PMZPoint = ^TMZPoint;

type
  { Summary:
      A 2-dimensional triangle. }
  TMZTriangle = record
  public
    { Summary:
        First point of the triangle. }
    P1: TMZPoint;

    { Summary:
        Second point of the triangle. }
    P2: TMZPoint;

    { Summary:
        Third point of the triangle. }
    P3: TMZPoint;
  public
    { Summary:
        Creates a triangle.
      Parameters:
        P1: First point of the triangle.
        P2: Second point of the triangle.
        P3: Third point of the triangle. }
    class function Create(const P1, P2, P3: TMZPoint): TMZTriangle; inline; static;

    { Summary:
        Checks whether this triangle contains the given point.
      Parameters:
        Point: the point to check.
      Returns:
        True if the triangle contains the given point. }
    function ContainsPoint(const Point: TMZPoint): Boolean; overload; inline;

    { Summary:
        Checks whether this triangle contains the given point.
      Parameters:
        X: X-coordinate of the point to check.
        Y: Y-coordinate of the point to check.
      Returns:
        True if the triangle contains the given point. }
    function ContainsPoint(const X, Y: Single): Boolean; overload; inline;

    { Summary:
        Checks whether this triangle equals another triangle.
      Parameters:
        Other: the other triangle.
      Returns:
        True if this triangle equals Other.
      Remarks:
        Since the coordinates are floating-point numbers, the comparison is
        performed with a little error margin (it uses Math.SameValue).
        The Equal (=) and NotEqual (<>) class operators perform a precise
        comparison. }
    function Equals(const Other: TMZTriangle): Boolean; inline;

    { Summary:
        Checks if two triangles are equal.
      Returns:
        True if the triangles are equal. False otherwise.
      Remarks:
        Be careful when comparing floating point numbers! This function does
        an exact comparision, so the slightest variation returns False.
        See <link Equals> for a more fuzzy check. }
    class operator Equal(const A, B: TMZTriangle): Boolean; inline;

    { Summary:
        Checks if two triangles are not equal.
      Returns:
        True if the triangles are not equal. False otherwise.
      Remarks:
        Be careful when comparing floating point numbers! This function does
        an exact comparision, so the slightest variation returns True.
        See <link Equals> for a more fuzzy check. }
    class operator NotEqual(const A, B: TMZTriangle): Boolean; inline;
  end;
  PMZTriangle = ^TMZTriangle;
  TMZTriangleArray = array of TMZTriangle;

type
  { Summary:
      A 2-dimensional circle. }
  TMZCircle = record
  public
    { Summary:
        X-coordinate of the center of the circle. }
    X: Single;

    { Summary:
        Y-coordinate of the center of the circle. }
    Y: Single;

    { Summary:
        Radius of the circle. }
    Radius: Single;
  public
    { Summary:
        Creates an empty circle. }
    class function Create: TMZCircle; overload; inline; static;

    { Summary:
        Creates a circle.
      Parameters:
        X: X-coordinate of the center of the circle.
        Y: Y-coordinate of the center of the circle.
        Radius: Radius of the circle. }
    class function Create(const X, Y, Radius: Single): TMZCircle; overload; inline; static;

    { Summary:
        Creates a circle.
      Parameters:
        Center: Coordinates of the center of the circle.
        Radius: Radius of the circle. }
    class function Create(const Center: TMZPoint; const Radius: Single): TMZCircle; overload; inline; static;

    { Summary:
        Checks whether this circle intersects another circle.
      Parameters:
        Other: the other circle.
      Returns:
        True if the circles intersect. }
    function IntersectsCircle(const Other: TMZCircle): Boolean; inline;

    { Summary:
        Checks whether this circle lies completely inside another circle.
      Parameters:
        OuterCircle: the outer circle.
      Returns:
        True if this circle lies within the outer circle. }
    function InsideCircle(const OuterCircle: TMZCircle): Boolean; inline;

    { Summary:
        Checks whether this circle lies completely inside a rectangle.
      Parameters:
        X: X-coordinate of the rectangle to check.
        Y: Y-coordinate of the rectangle to check.
        W: Width of the rectangle to check.
        H: Height of the rectangle to check.
      Returns:
        True if this circle lies within the rectangle. }
    function InsideRectangle(const X, Y, W, H: Single): Boolean; inline;

    { Summary:
        Checks whether this circle equals another circle.
      Parameters:
        Other: the other circle.
      Returns:
        True if this circle equals Other.
      Remarks:
        Since the coordinates are floating-point numbers, the comparison is
        performed with a little error margin (it uses Math.SameValue).
        The Equal (=) and NotEqual (<>) class operators perform a precise
        comparison. }
    function Equals(const Other: TMZCircle): Boolean; inline;

    { Summary:
        Checks if two circles are equal.
      Returns:
        True if the circles are equal. False otherwise.
      Remarks:
        Be careful when comparing floating point numbers! This function does
        an exact comparision, so the slightest variation returns False.
        See <link Equals> for a more fuzzy check. }
    class operator Equal(const A, B: TMZCircle): Boolean; inline;

    { Summary:
        Checks if two circles are not equal.
      Returns:
        True if the circles are not equal. False otherwise.
      Remarks:
        Be careful when comparing floating point numbers! This function does
        an exact comparision, so the slightest variation returns True.
        See <link Equals> for a more fuzzy check. }
    class operator NotEqual(const A, B: TMZCircle): Boolean; inline;
  end;
  PMZCircle = ^TMZCircle;

type
  { Summary:
      A 2D rectangle structure. }
  TMZRect = record
  public
    { Summary:
        X-coordinate of the top-left corner of the rectangle. }
    X: Single;

    { Summary:
        Y-coordinate of the top-left corner of the rectangle. }
    Y: Single;

    { Summary:
        Width of the rectangle. }
    W: Single;

    { Summary:
        Height of the rectangle. }
    H: Single;
  private
    function GetBottom: Single; inline;
    function GetRight: Single; inline;
    procedure SetBottom(const Value: Single); inline;
    procedure SetRight(const Value: Single); inline;
  public
    { Summary:
        Creates an empty rectangle. }
    class function Create: TMZRect; overload; inline; static;

    { Summary:
        Creates a rectangle.
      Parameters:
        X: X-coordinate of the top-left corner of the rectangle.
        Y: Y-coordinate of the top-left corner of the rectangle.
        W: Width of the rectangle.
        H: Height of the rectangle. }
    class function Create(const X, Y, W, H: Single): TMZRect; overload; inline; static;

    { Summary:
        Checks whether this rectangle contains the given point.
      Parameters:
        Point: the point to check.
      Returns:
        True if the rectangle contains the given point. }
    function ContainsPoint(const Point: TMZPoint): Boolean; overload; inline;

    { Summary:
        Checks whether this rectangle contains the given point.
      Parameters:
        X: X-coordinate of the point to check.
        Y: Y-coordinate of the point to check.
      Returns:
        True if the rectangle contains the given point. }
    function ContainsPoint(const X, Y: Single): Boolean; overload; inline;

    { Summary:
        Checks whether this rect intersects another rect.
      Parameters:
        Other: the other rect.
      Returns:
        True if the rectangles intersect. }
    function IntersectsRect(const Other: TMZRect): Boolean; inline;

    { Summary:
        Checks whether this rect intersects a circle.
      Parameters:
        Circle: the circle.
      Returns:
        True if the rectangle intersects the circle. }
    function IntersectsCircle(const Circle: TMZCircle): Boolean; inline;

    { Summary:
        Checks whether this rect lies completely inside another rect.
      Parameters:
        OuterRect: the other outer rect.
      Returns:
        True if this rectangle lies within OuterRect. }
    function InsideRect(const OuterRect: TMZRect): Boolean; inline;

    { Summary:
        Checks whether this rect lies completely inside a circle.
      Parameters:
        Circle: the circle.
      Returns:
        True if this rectangle lies within the circle. }
    function InsideCircle(const Circle: TMZCircle): Boolean; inline;

    { Summary:
        Clips the rectangle to another rectangle.
      Parameters:
        ClipRect: the rectangle to clip to.
      Returns:
        The clipped rectangle. }
    function ClipToRect(const ClipRect: TMZRect): TMZRect; inline;

    { Summary:
        Checks whether this rect equals another rect.
      Parameters:
        Other: the other rect.
      Returns:
        True if this rect equals Other.
      Remarks:
        Since the coordinates are floating-point numbers, the comparison is
        performed with a little error margin (it uses Math.SameValue).
        The Equal (=) and NotEqual (<>) class operators perform a precise
        comparison. }
    function Equals(const Other: TMZRect): Boolean; inline;

    { Summary:
        Checks if two rects are equal.
      Returns:
        True if the rects are equal. False otherwise.
      Remarks:
        Be careful when comparing floating point numbers! This function does
        an exact comparision, so the slightest variation returns False.
        See <link Equals> for a more fuzzy check. }
    class operator Equal(const A, B: TMZRect): Boolean; inline;

    { Summary:
        Checks if two rects are not equal.
      Returns:
        True if the rects are not equal. False otherwise.
      Remarks:
        Be careful when comparing floating point numbers! This function does
        an exact comparision, so the slightest variation returns True.
        See <link Equals> for a more fuzzy check. }
    class operator NotEqual(const A, B: TMZRect): Boolean; inline;

    { Summary:
        X-coordinate of the bottom-right corner of the rectangle. }
    property Right: Single read GetRight write SetRight;

    { Summary:
        Y-coordinate of the bottom-right corner of the rectangle. }
    property Bottom: Single read GetBottom write SetBottom;
  end;
  PMZRect = ^TMZRect;

type
  { Summary:
      A 2-dimensional line segment. }
  TMZLine = record
  public
    { Summary:
        X-coordinate of the start of the line segment. }
    X0: Single;

    { Summary:
        Y-coordinate of the start of the line segment. }
    Y0: Single;

    { Summary:
        X-coordinate of the end of the line segment. }
    X1: Single;

    { Summary:
        Y-coordinate of the end of the line segment. }
    Y1: Single;
  public
    { Summary:
        Creates an empty line segment. }
    class function Create: TMZLine; overload; inline; static;

    { Summary:
        Creates a line segment.
      Parameters:
        X0: Start X-coordinate.
        Y0: Start Y-coordinate.
        X1: End X-coordinate.
        Y1: End Y-coordinate. }
    class function Create(const X0, Y0, X1, Y1: Single): TMZLine; overload; inline; static;

    { Summary:
        Creates a line segment.
      Parameters:
        P0: Start coordinates.
        P1: End coordinates}
    class function Create(const P0, P1: TMZPoint): TMZLine; overload; inline; static;

    { Summary:
        Checks whether this line intersects another line.
      Parameters:
        Other: the other line.
        Intersection: (optional) if not-nil, will be set to the intersection
          point.
      Returns:
        True if the lines intersect. }
    function IntersectsLine(const Other: TMZLine; const Intersection: PMZPoint): Boolean; inline;

    { Summary:
        Checks whether this line intersects a rectangle.
      Parameters:
        Rect: the rectangle.
      Returns:
        True if the line intersects the rectangle. }
    function IntersectsRectangle(const Rect: TMZRect): Boolean; inline;

    { Summary:
        Checks whether this line intersects a circle.
      Parameters:
        Circle: the circle.
      Returns:
        True if the line intersects the circle. }
    function IntersectsCircle(const Circle: TMZCircle): Boolean; inline;

    { Summary:
        Calculates the angle (in degrees) of the line.
      Returns:
        The angle (in degrees) of the two line. }
    function Angle: Single; inline;

    { Summary:
        Calculates the orientation of a point to this line.
      Parameters:
        P: the point.
      Returns:
        Whether the point lies to the left, right or on the line. }
    function Orientation(const P: TMZPoint): TMZOrientation; overload; inline;

    { Summary:
        Calculates the orientation of a point to this line.
      Parameters:
        X: X-coordinate of the point.
        Y: Y-coordinate of the point.
      Returns:
        Whether the point lies to the left, right or on the line. }
    function Orientation(const X, Y: Single): TMZOrientation; overload; inline;

    { Summary:
        Checks whether this line equals another line.
      Parameters:
        Other: the other line.
      Returns:
        True if this line equals Other.
      Remarks:
        Since the coordinates are floating-point numbers, the comparison is
        performed with a little error margin (it uses Math.SameValue).
        The Equal (=) and NotEqual (<>) class operators perform a precise
        comparison. }
    function Equals(const Other: TMZLine): Boolean; inline;

    { Summary:
        Checks if two lines are equal.
      Returns:
        True if the lines are equal. False otherwise.
      Remarks:
        Be careful when comparing floating point numbers! This function does
        an exact comparision, so the slightest variation returns False.
        See <link Equals> for a more fuzzy check. }
    class operator Equal(const A, B: TMZLine): Boolean; inline;

    { Summary:
        Checks if two lines are not equal.
      Returns:
        True if the lines are not equal. False otherwise.
      Remarks:
        Be careful when comparing floating point numbers! This function does
        an exact comparision, so the slightest variation returns True.
        See <link Equals> for a more fuzzy check. }
    class operator NotEqual(const A, B: TMZLine): Boolean; inline;
  end;
  PMZLine = ^TMZLine;
{$ENDREGION 'Geometry and Collision Detection'}

{$REGION 'Memory'}
type
  { Summary:
      Seek modes for use with <link TMZFile> and <link TMZMemory>. }
  TMZSeekMode = (
    { Summary:
        Sets the current pointer relative from the beginning of the file or
        memory block. }
    smBeginning = FSM_SET,

    { Summary:
        Sets the current pointer relative from the current position in the file
        or memory block. }
    smFromCurrent = FSM_CUR,

    { Summary:
        Sets the current pointer relative from the end of the file or
        memory block. }
    smFromEnd = FSM_END);

type
  { Summary:
      Represents a block of memory. }
  TMZMemory = class
  {$REGION 'Internal Declarations'}
  private
    FSettings: zglTMemory;
    FHandle: zglPMemory;
    procedure SetSize(const Value: Longword); inline;
    procedure SetPosition(const Value: Longword); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates an empty memory block. }
    constructor Create; overload;

    { Summary:
        Allocates a block of memory.
      Parameters:
        Size: initial size of the memory block. }
    constructor Create(const Size: Integer); overload;

    { Summary:
        Loads file and stores it in memory.
      Parameters:
        Filename: the name of the file to load.
      Remarks:
        Raises an exception if the file cannot be loaded. }
    constructor Create(const Filename: UTF8String); overload;
    destructor Destroy; override;

    { Summary:
        Saves the memory block to a file.
      Parameters:
        Filename: the name of the file to save to. }
    procedure SaveToFile(const Filename: UTF8String); inline;

    { Summary:
        Seeks into the memory block.
      Parameters:
        Offset: the offset to seek. Depends on Mode.
        Mode: the seek mode to use.
      Returns:
        The new position. }
    function Seek(const Offset: Integer; const Mode: TMZSeekMode): Integer; inline;

    { Summary:
        Reads from the memory block.
      Parameters:
        Buffer: the buffer to read into.
        Count: the number of bytes to read.
      Returns:
        The number of bytes actually read. }
    function Read(var Buffer; const Count: Integer): Integer; inline;

    { Summary:
        Writes to the memory block.
      Parameters:
        Buffer: the buffer to write.
        Count: the number of bytes to write.
      Returns:
        The number of bytes actually written. }
    function Write(const Buffer; const Count: Integer): Integer; inline;

    { Summary:
        The size of the memory buffer. }
    property Size: Longword read FSettings.Size write SetSize;

    { Summary:
        The current position in the memory buffer }
    property Position: Longword read FSettings.Position write SetPosition;

    { Summary:
        The memory buffer.
      Remarks:
        This value may change as you change the size. }
    property Memory: Pointer read FSettings.Memory;

    { Summary:
        Internal handle used for the memory block.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property Handle: zglPMemory read FHandle;
  end;
{$ENDREGION 'Memory'}

{$REGION 'Textures'}
const
  NO_KEY_COLOR = TEX_NO_COLORKEY;

type
  { Summary:
      Flags for customizing textures (see <link TMZTexture>). }
  TMZTextureFlag = (
    { Summary:
        Enable generation of mipmaps. }
    tfGenerateMipMaps = 0,

    { Summary:
        Use S3TC/DXT5 texture compression.
      Remarks:
        This option is not available on all platforms. }
    tfCompressed = 3,

    { Summary:
        Makes sure the texture dimensions will be a power of two, adding borders
        if necessary. }
    tfMakePowerOf2 = 4,

    { Summary:
        Performs alpha channel precalculation to speed up alpha blending.
      Remarks:
        You should <b>not</b> use this flag if the texture is created with an
        image file that already contains a precalculated alpha channel. }
    tfPrecalculateAlpha = 5,

    { Summary:
        Converts the texture to grayscale. }
    tfConvertToGrayscale = 6,

    { Summary:
        Inverts the colors of the texture. }
    tfInvert = 7);
  TMZTextureFlags = set of TMZTextureFlag;

const
  DEFAULT_TEXTURE_FLAGS = [tfMakePowerOf2, tfPrecalculateAlpha];

type
  { Summary:
      Determines how the texture is wrapped at the edges. }
  TMZTextureWrap = (
    { Summary:
        Default wrapping. }
    twDefault = $00,

    { Summary:
        Clamps the texture to prevent wrapping artifacts. }
    twClamp = $02,

    { Summary:
        Repeats the texture at the edges. }
    twRepeat = $04);

type
  { Summary:
      Determines how textures are filtered when scaled. }
  TMZTextureFilter = (
    { Summary:
        Nearest neighbor filtering (basically disables filtering).
      Remarks:
        This is the fastest filtering method but offers the lowest quality. }
    tfNearest = $0200,

    { Summary:
        Use linear filtering (the color of each pixel is calculated using the
        weighted average of the 4 texture elements that are closest to the
        pixel.
      Remarks:
        Provides higher quality than tfNearest, but can be slower. }
    tfLinear = $0400,

    { Summary:
        Uses bilinear filtering. For upsizing, the effect is the same as
        tfLinear. For downsizing, it chooses the mipmap that most closely
        matches the size of the pixel being textured, using linear filtering.
      Remarks:
        Should be used in combination with the tfGenerateMipMaps texture
        flag (see <link TMZTextureFlag>). }
    tfBilinear = $0800,

    { Summary:
        Uses trilinear filtering. For upsizing, the effect is the same as
        tfLinear. For downsizing, it chooses the two mipmaps that most closely
        match the size of the pixel being textured, and uses linear filtering
        to produce the texture value of each mipmap. The final texture value is
        the weighted average of those two values.
      Remarks:
        Should be used in combination with the tfGenerateMipMaps texture
        flag (see <link TMZTextureFlag>). }
    tfTrilinear = $1000,

    { Summary:
        Similar to tfTrilinear, but adds anisotropy filtering.
      Remarks:
        Should be used in combination with the tfGenerateMipMaps texture
        flag (see <link TMZTextureFlag>). The level of anisotropy can be set
        using <link TMZTexture.AnisotropyLevel>. }
    tfAnisotropy = $2000);

type
  { Summary:
      Internal data format of a texture }
  TMZTextureFormat = (
    { Summary:
        RGBA format with 8 bits per channel. }
    tfRgba = $01,

    { Summary:
        RGBA format with 4 bits per channel. }
    tfRgba4444 = $02,

    { Summary:
        RGBA format using PVR2 texture compression. }
    tfRgbaPvr2 = $10,

    { Summary:
        RGBA format using PVR4 texture compression. }
    tfRgbaPvr4 = $11,

    { Summary:
        RGBA format using DXT1 texture compression. }
    tfRgbaDtx1 = $20,

    { Summary:
        RGBA format using DXT3 texture compression. }
    tfRgbaDtx3 = $21,

    { Summary:
        RGBA format using DXT5 texture compression. }
    tfRgbaDtx5 = $22);

type
  { Summary:
      Method for applying a custom effect to a <link TMZTexture>.
    Parameters:
      Data: pointer to the raw texture data.
      Width: width of the texture.
      Height: height of the texture. }
  TMZTextureEffectProc = procedure(var Data: Pointer; Width, Height: Word);

type
  { Summary:
      Represents a texture }
  TMZTexture = class
  {$REGION 'Internal Declarations'}
  private
    FHandle: zglPTexture;
    FOwnsHandle: Boolean;
    FFrameWidth: Integer;
    FFrameHeight: Integer;
    FFrameRows: Integer;
    FFrameColumns: Integer;
    class function GetAnisotropyLevel: Integer; inline; static;
    function GetFormat: TMZTextureFormat; inline;
    function GetHeight: Integer; inline;
    function GetID: Cardinal; inline;
    function GetWidth: Integer; inline;
    class procedure SetAnisotropyLevel(const Value: Integer); inline; static;
    class function IsZeroTexture(const Handle: zglPTexture): Boolean; inline; static;
    constructor Create(const Handle: zglPTexture); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a texture filled with a given color.
      Parameters:
        Width: width of the texture.
        Height: height of the texture.
        Color: (optional) color to fill the texture with (in $RRGGBB format).
          Defaults to black.
        Flags: (optional) texture flags. Defaults to [tfMakePowerOfTwo,
          tfPrecalculateAlpha].
        Filter: (optional) texture filter to use for resizing (defaults to
          tfLinear).
        Wrap: (optional) how the texture is wrapped at the edges (defaults to
          twClamp)
        CustomEffect: (optional) optional event that is called to apply a
          custom effect to the texture.
      Remarks:
        Raises an exception if the texture cannot be created. }
    constructor Create(const Width, Height: Integer; const Color: Cardinal = $000000;
      const Flags: TMZTextureFlags = DEFAULT_TEXTURE_FLAGS;
      const Filter: TMZTextureFilter = tfLinear; const Wrap: TMZTextureWrap = twClamp;
      const CustomEffect: TMZTextureEffectProc = nil); overload;

    { Summary:
        Creates a texture from a file on disk or in a ZIP archive.
      Parameters:
        Filename: the name of the file to load the texture from. The engine
          supports the file formats PNG (.png), JPEG (.jpg, .jpeg), Targa
          (.tga) and PVR (.pvr, Apple OS's only).
        KeyColor: (optional) all colors in the texture that match this color
          will be made transparent. Use NO_KEY_COLOR to disable color keying.
        Flags: (optional) texture flags. Defaults to [tfMakePowerOfTwo,
          tfPrecalculateAlpha].
        Filter: (optional) texture filter to use for resizing (defaults to
          tfLinear).
        Wrap: (optional) how the texture is wrapped at the edges (defaults to
          twClamp)
        CustomEffect: (optional) optional event that is called to apply a
          custom effect to the texture.
      Remarks:
        If a ZIP archive has been opened (see <link TMZZipArchive>), then the
        texture will be loaded from the ZIP file instead.
        Raises an exception if the file does not exist or is invalid. }
    constructor Create(const Filename: UTF8String; const KeyColor: Cardinal = NO_KEY_COLOR;
      const Flags: TMZTextureFlags = DEFAULT_TEXTURE_FLAGS;
      const Filter: TMZTextureFilter = tfLinear; const Wrap: TMZTextureWrap = twClamp;
      const CustomEffect: TMZTextureEffectProc = nil); overload;

    { Summary:
        Creates a texture from a stream.
      Parameters:
        Stream: the stream containing the image data.
        Extension: the extension used to describe the image data in the stream.
          Currently supported are .png, .jpg, .jpeg, .tga and
          .pvr (Apple OS's only)
        KeyColor: (optional) all colors in the texture that match this color
          will be made transparent. Use NO_KEY_COLOR to disable color keying.
        Flags: (optional) texture flags. Defaults to [tfMakePowerOfTwo,
          tfPrecalculateAlpha].
        Filter: (optional) texture filter to use for resizing (defaults to
          tfLinear).
        Wrap: (optional) how the texture is wrapped at the edges (defaults to
          twClamp)
        CustomEffect: (optional) optional event that is called to apply a
          custom effect to the texture.
      Remarks:
        The image is read from the current position in the stream.
        Raises an exception if the stream is invalid. }
    constructor Create(const Stream: TStream; const Extension: UTF8String;
      const KeyColor: Cardinal = NO_KEY_COLOR;
      const Flags: TMZTextureFlags = DEFAULT_TEXTURE_FLAGS;
      const Filter: TMZTextureFilter = tfLinear; const Wrap: TMZTextureWrap = twClamp;
      const CustomEffect: TMZTextureEffectProc = nil); overload;

    { Summary:
        Creates a texture from a memory.
      Parameters:
        Buffer: the memory buffer to load the texture from.
        Size: the size of the memory buffer.
        Extension: the extension used to describe the image data in the stream.
          Currently supported are .png, .jpg, .jpeg, .tga and
          .pvr (Apple OS's only)
        KeyColor: (optional) all colors in the texture that match this color
          will be made transparent. Use NO_KEY_COLOR to disable color keying.
        Flags: (optional) texture flags. Defaults to [tfMakePowerOfTwo,
          tfPrecalculateAlpha].
        Filter: (optional) texture filter to use for resizing (defaults to
          tfLinear).
        Wrap: (optional) how the texture is wrapped at the edges (defaults to
          twClamp)
        CustomEffect: (optional) optional event that is called to apply a
          custom effect to the texture.
      Remarks:
        Raises an exception if the stream is invalid. }
    constructor Create(const Buffer: Pointer; const Size: Integer;
      const Extension: UTF8String; const KeyColor: Cardinal = NO_KEY_COLOR;
      const Flags: TMZTextureFlags = DEFAULT_TEXTURE_FLAGS;
      const Filter: TMZTextureFilter = tfLinear; const Wrap: TMZTextureWrap = twClamp;
      const CustomEffect: TMZTextureEffectProc = nil); overload;

    { Summary:
        Creates a texture from a memory.
      Parameters:
        Memory: the memory buffer to load the texture from.
        Extension: the extension used to describe the image data in the stream.
          Currently supported are .png, .jpg, .jpeg, .tga and
          .pvr (Apple OS's only)
        KeyColor: (optional) all colors in the texture that match this color
          will be made transparent. Use NO_KEY_COLOR to disable color keying.
        Flags: (optional) texture flags. Defaults to [tfMakePowerOfTwo,
          tfPrecalculateAlpha].
        Filter: (optional) texture filter to use for resizing (defaults to
          tfLinear).
        Wrap: (optional) how the texture is wrapped at the edges (defaults to
          twClamp)
        CustomEffect: (optional) optional event that is called to apply a
          custom effect to the texture.
      Remarks:
        Raises an exception if the stream is invalid. }
    constructor Create(const Memory: TMZMemory; const Extension: UTF8String;
      const KeyColor: Cardinal = NO_KEY_COLOR;
      const Flags: TMZTextureFlags = DEFAULT_TEXTURE_FLAGS;
      const Filter: TMZTextureFilter = tfLinear; const Wrap: TMZTextureWrap = twClamp;
      const CustomEffect: TMZTextureEffectProc = nil); overload;

    destructor Destroy; override;

    { Summary:
        Signifies that the texture contains a frame sequence (for example
        sprite frames for an animation, or tiles in a tileset).
      Parameters:
        FrameWidth: the width of each frame.
        FrameHeight: the height of each frame.
      Remarks:
        This method will calculate the texture coordinates of each frame and
        also set the <link FrameRows> and <link FrameColumns> properties. }
    procedure SetFrameSize(const FrameWidth, FrameHeight: Integer); inline;

    { Summary:
        Applies a mask to the texture.
      Parameters:
        Mask: the texture that contains the mask.
      Remarks:
        This method sets the Alpha value of each pixel equal to the Blue value
        of each corresponding pixel in Mask. }
    procedure ApplyMask(const Mask: TMZTexture); inline;

    { Summary:
        Gets the pixel data for the texture.
      Returns:
        A newly allocated buffer of pixel data. You <b>must</b> free this buffer
        when you are done with it. }
    function GetData: PByteArray; inline;

    { Summary:
        Sets the pixel data for the texture.
      Parameters:
        Data: the buffer containing the pixel data.
        X: X-position in the texture where to start copying the pixel data.
        Y: Y-position in the texture where to start copying the pixel data.
        Width: the width of the pixel buffer.
        Height: the height of the pixel buffer.
        Stride: (optional) the stride of the pixel data.
      Remarks:
        The texture does <b>not </b> become owner of the pixel data. }
    procedure SetData(const Data: Pointer; const X, Y, Width, Height: Integer;
      const Stride: Integer = 0); inline;

    { Summary:
        Sets the new filter and wrap methods for the texture.
      Parameters:
        Filter: texture filter to use for resizing.
        Wrap: how the texture is wrapped at the edges. }
    procedure Filter(const Filter: TMZTextureFilter; const Wrap: TMZTextureWrap); inline;

    { Summary:
        Unique identifier for the texture. }
    property ID: Cardinal read GetID;

    { Summary:
        Width of the texture. }
    property Width: Integer read GetWidth;

    { Summary:
        Height of the texture. }
    property Height: Integer read GetHeight;

    { Summary:
        The internal data format of the texture. }
    property Format: TMZTextureFormat read GetFormat;

    { Summary:
        If the texture contains a sequence of frames (as set
        by <link SetFrameSize>, for example sprite frames for an animation or
        tiles for a tileset), then this property returns the width of each
        frame. Otherwise, returns 0. }
    property FrameWidth: Integer read FFrameWidth;

    { Summary:
        If the texture contains a sequence of frames (as set
        by <link SetFrameSize>, for example sprite frames for an animation or
        tiles for a tileset), then this property returns the height of each
        frame. Otherwise, returns 0. }
    property FrameHeight: Integer read FFrameHeight;

    { Summary:
        If the texture contains a sequence of frames (as set
        by <link SetFrameSize>, for example sprite frames for an animation or
        tiles for a tileset), then this property returns the number of columns
        of frames. Otherwise, returns 1. }
    property FrameColumns: Integer read FFrameColumns;

    { Summary:
        If the texture contains a sequence of frames (as set
        by <link SetFrameSize>, for example sprite frames for an animation or
        tiles for a tileset), then this property returns the number of rows of
        frames. Otherwise, returns 1. }
    property FrameRows: Integer read FFrameRows;

    { Summary:
        The level of anisotropy to use when the filter method is set to
        tfAnisotropy.
      Remarks:
        Must be in the range 0..<link TMZUtils.MaxAnisotropyLevel>.
        This level applies to <b>all</b> textures. }
    class property AnisotropyLevel: Integer read GetAnisotropyLevel write SetAnisotropyLevel;

    { Summary:
        Internal handle used for the texture.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property Handle: zglPTexture read FHandle write FHandle;
  end;
{$ENDREGION 'Textures'}

{$REGION 'Fonts'}
type
  { Summary:
      A font to use for text drawing. }
  TMZFont = class
  {$REGION 'Internal Declarations'}
  private
    FHandle: zglPFont;
    function GetMaxHeight: Integer;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a font and loads it from a file on disk or in a ZIP archive.
      Parameters:
        Filename: the file to load the font from.
      Remarks:
        If a ZIP archive has been opened (see <link TMZZipArchive>), then the
        font will be loaded from the ZIP file instead.
        Raises an exception if the file does not exist or is invalid. }
    constructor Create(const Filename: UTF8String); overload;

    { Summary:
        Creates a font and loads it from a stream.
      Parameters:
        Stream: the stream to load the font from.
      Remarks:
        The font is read from the current position in the stream.
        Raises an exception if the stream is invalid. }
    constructor Create(const Stream: TStream); overload;

    { Summary:
        Creates a font and loads it from memory.
      Parameters:
        Buffer: the memory buffer to load the font from.
        Size: the size of the memory buffer.
        Offset: (optional) the offset in the buffer to start loading from.
      Remarks:
        Raises an exception if the memory does not contain a valid font. }
    constructor Create(const Buffer: Pointer; const Size: Integer;
      const Offset: Integer = 0); overload;

    { Summary:
        Creates a font and loads it from memory.
      Parameters:
        Memory: the memory buffer to load the font from.
      Remarks:
        Raises an exception if the memory does not contain a valid font. }
    constructor Create(const Memory: TMZMemory); overload;

    destructor Destroy; override;

    { Summary:
        Maximum height of all the glyphs in the font (in pixels) }
    property MaxHeight: Integer read GetMaxHeight;

    { Summary:
        Internal handle used for the font.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property Handle: zglPFont read FHandle;
  end;
{$ENDREGION 'Fonts'}

{$REGION 'Camera'}
type
  { Summary:
      A 2D camera. }
  TMZCamera = class
  {$REGION 'Internal Declarations'}
  private
    FHandle: zglPCamera2D;
    function GetCenter: TMZPoint; inline;
    function GetCenterX: Single; inline;
    function GetCenterY: Single; inline;
    function GetRotationAngle: Single; inline;
    function GetXOffset: Single; inline;
    function GetYSOffset: Single; inline;
    function GetZoomX: Single; inline;
    function GetZoomY: Single; inline;
    procedure SetCenter(const Value: TMZPoint); inline;
    procedure SetCenterX(const Value: Single); inline;
    procedure SetCenterY(const Value: Single); inline;
    procedure SetRotationAngle(const Value: Single); inline;
    procedure SetXOffset(const Value: Single); inline;
    procedure SetYOffset(const Value: Single); inline;
    procedure SetZoomX(const Value: Single); inline;
    procedure SetZoomY(const Value: Single); inline;
  public
    constructor CreateFromHandle(const Handle: zglPCamera2D);
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a new camera }
    constructor Create;
    destructor Destroy; override;

    { Summary:
        Resets the camera to its defaults (centered to screen and no zooming,
        no rotation and no offset) }
    procedure Reset;

    { Summary:
        X-coordinate that the camera is centered at.
      Remarks:
        Defaults to the center of the screen. }
    property CenterX: Single read GetCenterX write SetCenterX;

    { Summary:
        Y-coordinate that the camera is centered at.
      Remarks:
        Defaults to the center of the screen. }
    property CenterY: Single read GetCenterY write SetCenterY;

    { Summary:
        Location (X/Y-coordinate) that the camera is centered at.
      Remarks:
        Defaults to the center of the screen. }
    property Center: TMZPoint read GetCenter write SetCenter;

    { Summary:
        Rotation angle of camera in degrees.
      Remarks:
        The camera rotates around the <link Center> coordinates. }
    property RotationAngle: Single read GetRotationAngle write SetRotationAngle;

    { Summary:
        Horizontal zoom factor.
      Remarks:
        Defaults to 1.0 }
    property ZoomX: Single read GetZoomX write SetZoomX;

    { Summary:
        Vertical zoom factor.
      Remarks:
        Defaults to 1.0 }
    property ZoomY: Single read GetZoomY write SetZoomY;

    { Summary:
        X-offset of the camera.
      Remarks:
        Defaults to 0. }
    property XOffset: Single read GetXOffset write SetXOffset;

    { Summary:
        Y-offset of the camera.
      Remarks:
        Defaults to 0. }
    property YOffset: Single read GetYSOffset write SetYOffset;

    { Summary:
        Internal handle used for the camera.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property Handle: zglPCamera2D read FHandle;
  end;
{$ENDREGION 'Camera'}

{$REGION 'Render Targets'}
type
  { Summary:
      Various options to customize the behavior of <link TMZRenderTarget> }
  TMZRenderTargetOption = (
    { Summary:
        Treats the render target as a full-screen render target, even if the
        dimensions of the render target do not match the dimensions of the
        screen. That is, the drawing operations will be scaled to fit the
        render target. For example, say the screen is 800x600 pixels and the
        render target is 512x512 pixels. Then, when you draw a 800x600 image,
        it will be scaled (and stretched) to 512x512 pixels. }
    rtFullScreen = 0,

    { Summary:
        Whether the render target has its own depth buffer. }
    rtDepthBuffer = 1,

    { Summary:
        Clears the color buffer for every frame. Disabling this option can
        improve performance if the screen does not needs to be cleared because
        fully occupied with rendered objects. }
    rtClearColorBuffer = 2,

    { Summary:
        Enables or disables clearing of the depth buffer for every frame. }
    rtClearDepthBuffer = 3,

    { Summary:
        Whether the render target will own the texture. If so, the render target
        will free the texture automatically when done. }
    rtOwnsTexture = 4);

  TMZRenderTargetOptions = set of TMZRenderTargetOption;

type
  { Summary:
      Represents a render target. You can use a render target as target for
      drawing operations. Normally, all drawing operations draw directly to
      the screen, but by setting <link TMZCanvas.RenderTarget>, you can redirect
      all drawing operations to a texture. }
  TMZRenderTarget = class
  {$REGION 'Internal Declarations'}
  private
    FHandle: zglPRenderTarget;
    FTexture: TMZTexture;
    FOwnsTexture: Boolean;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a new render target.
      Parameters:
        Texture: the texture that will be the target for drawing operations.
        Options: (optional) various flags to customize render target behavior.
      Remarks:
        If the rtOwnsTexture flag is set, then the render target will become
        owner of the texture, and you should <b>not</b> free the texture
        yourself. If the rtOwnsTexture flag is <b>not</b> set, the it is your
        responsibility to free the texture at the appropriate time.
        To active the render target, set <link TMZCanvas.RenderTarget> to
        this render target. }
    constructor Create(const Texture: TMZTexture;
        const Options: TMZRenderTargetOptions = []);
    destructor Destroy; override;

    { Summary:
        The texture that will be drawn to. }
    property Texture: TMZTexture read FTexture;

    { Summary:
        Internal handle used for the render target.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property Handle: zglPRenderTarget read FHandle;
  end;
{$ENDREGION 'Render Targets'}

{$REGION 'Canvas and Drawing'}
type
  { Summary:
      Text flags for use with <link TMZCanvas.DrawText> }
  TMZTextFlag = (
    { Summary:
        Align the text left horizontally. }
    tfHAlignLeft,

    { Summary:
        Center the text horizontally. }
    tfHAlignCenter,

    { Summary:
        Align the right left horizontally. }
    tfHAlignRight,

    { Summary:
        Justify the text horizontally. }
    tfHAlignJustify,

    { Summary:
        Align the text top the top vertically. }
    tfVAlignTop,

    { Summary:
        Center the text vertically. }
    tfVAlignCenter,

    { Summary:
        Align the text top the bottom vertically. }
    tfVAlignBottom,

    { Summary:
        Clip the text to the given rectangle. }
    tfClipRect,

    { Summary:
        Use different colors and alpha values for each corner of each character
        of text. Use <link TMZCanvas.SetPerVertexColors> to set these colors. }
    tfPerVertexColors,

    tfFXLength);
  TMZTextFlags = set of TMZTextFlag;

  { Summary:
      Effect flags used for various drawing operations in <link TMZScene> }
  TMZEffectFlag = (
    { Summary:
        Flips the texture/sprite horizontally. }
    efFlipX = 0,

    { Summary:
        Flips the texture/sprite vertically. }
    efFlipY = 1,

    { Summary:
        Use different colors and alpha values for each vertext of the drawing
        primitive. Use <link TMZCanvas.SetPerVertexColors> to set these colors. }
    efPerVertexColors = 2,

    { Summary:
        Offsets each vertex of the texture/sprite with the amounts set by
        <link TMZCanvas.SetVertexOffsets> }
    efOffsetVertices = 3,

    { Summary:
        Scales the texture/sprite when drawing by the amount set by
        <link TMZCanvas.ScaleX> and <link TMZCanvas.ScaleY>}
    efScale = 4,

    { Summary:
        When a texture/sprite is rotated, it rotates around its center by
        default. Use this flag to rotate around a different point, and set that
        point using <link TMZCanvas.SetPivotPoint> }
    efAlternatePivotPoint = 5,

    { Summary:
        Blends the texture when drawing, instead of alpha testing. }
    efBlend = 20,

    { Summary:
        Renders the texture as a single color, set by
        <link TMZCanvas.TextureColor>.
      Remarks:
        How the color is applied depends on <link TMZCanvas.ColorMode>. }
    efColor = 21);
  TMZEffectFlags = set of TMZEffectFlag;

type
  { Summary:
      Used with <link TMZCanvas.ColorMode> to set the mode used for
      rendering the color. }
  TMZColorMode = (
    { Summary:
        The color of each pixel will be mixed with the given color. }
    cmMix,

    { Summary:
        The color of each pixel will be replaced by the given color. }
    cmReplace);

type
  { Summary:
      Used with <link TMZCanvas.BlendMode> to set the blending mode. }
  TMZBlendMode = (
    { Summary:
        Normal blend mode. }
    bmNormal = FX_BLEND_NORMAL,

    { Summary:
        Additive blend mode. }
    bmAdd = FX_BLEND_ADD,

    { Summary:
        Multiplactive blend mode. }
    bmMultiply = FX_BLEND_MULT,

    { Summary:
        Black blend mode. }
    bmBlack = FX_BLEND_BLACK,

    { Summary:
        White blend mode. }
    bmWhite = FX_BLEND_WHITE,

    { Summary:
        Masked blend mode. }
    bmMask = FX_BLEND_MASK,
     {}
    bmSrcAdd = FX_BLEND_SRCADD);

type
  { Summary:
      Array of texture coordinates (going clockwise from Top-Left to
      Bottom-Left) for use with <link TMZCanvas.DrawTexture>. }
  TMZTextureCoordinates = array [0..3] of TMZPoint;

type
  { Summary:
      Options to use with <link TMZCanvas.EnableOption> and
      <link TMZCanvas.DisableOption>. }
  TMZCanvasOption = (
    { Summary:
        Clears the color buffer for every frame. Disabling this option can
        improve performance if the screen does not needs to be cleared because
        fully occupied with rendered objects. }
    coClearColorBuffer = $01,

    { Summary:
        Enable or disable the depth buffer. }
    coDepthBuffer = $02,

    { Summary:
        Enables or disables clearing of the depth buffer for every frame. }
    coClearDepthBuffer = $04,

    { Summary:
        Enable or disable the depth buffer mask. }
    coDepthMask = $08,

    { Summary:
        Enables or disables clearing of the stencil buffer for every frame. }
    coClearStencilBuffer = $10);

type
  { Summary:
      Used in <link TMZCanvas.DrawSpriteTiles> }
  TMZTileFrames = array of array of Integer;

type
  { Summary:
      Static canvas class for performing drawing operations.
    Remarks:
      All MondoZenGL classes share the same canvas. }
  TMZCanvas = class
  {$REGION 'Internal Declarations'}
  private
    class var FFlags: Cardinal;
    class var FTextureColor: Cardinal;
    class var FColorMode: TMZColorMode;
    class var FBlendMode: TMZBlendMode;
    class var FSeparateAlpha: Boolean;
    class var FScaleX: Single;
    class var FScaleY: Single;
    class var FCamera: TMZCamera;
    class var FRenderTarget: TMZRenderTarget;
    class function GetAntiAlias: Boolean; static; inline;
    class procedure SetAntiAlias(const Value: Boolean); static; inline;
    class procedure SetBlendMode(const Value: TMZBlendMode); static; inline;
    class procedure SetSeparateAlpha(const Value: Boolean); static; inline;
    class procedure SetColorMode(const Value: TMZColorMode); static; inline;
    class procedure SetTextureColor(const Value: Cardinal); static; inline;
    class procedure SetScaleX(const Value: Single); static; inline;
    class procedure SetScaleY(const Value: Single); static; inline;
    class procedure SetCamera(const Value: TMZCamera); static; inline;
    class procedure SetRenderTarget(const Value: TMZRenderTarget); static; inline;
  public
    class constructor ClassCreate;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Clears the canvas and its buffers (such as depth buffers and stencil\
        buffers). }
    class procedure Clear; static; inline;

    { Summary:
        Forces execution of render commands and swaps buffers. So, if you want
        to show something before some heavy operation, which will take a long
        time, you should to use this function. }
    class procedure Flush; static; inline;

    { Summary:
        Draws a single pixel.
      Parameters:
        X: X-coordinate.
        Y: Y-coordinate.
        Color: color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque). }
    class procedure DrawPixel(const X, Y: Single; const Color: Cardinal;
      const Alpha: Byte = $FF); overload; static; inline;

    { Summary:
        Draws a single pixel.
      Parameters:
        Point: The pixel coordinates.
        Color: color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque). }
    class procedure DrawPixel(const Point: TMZPoint; const Color: Cardinal;
      const Alpha: Byte = $FF); overload; static; inline;

    { Summary:
        Draws a line.
      Parameters:
        X1: The start X-coordinate of the line.
        Y1: The start Y-coordinate of the line.
        X2: The end X-coordinate of the line.
        Y2: The end Y-coordinate of the line.
        Color: color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: optional effect flags. Recognized flags are:
          efPerVertexColors: use separate colors/alpha values for each end point
            of the line (see <link SetPerVertexColors>, only the first 2 colors
            are used) }
    class procedure DrawLine(const X1, Y1, X2, Y2: Single; const Color: Cardinal;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = []); overload; static; inline;

    { Summary:
        Draws a line.
      Parameters:
        StartPoint: The start coordinates of the line.
        EndPoint: The endt coordinates of the line.
        Color: color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: optional effect flags. Recognized flags are:
          efPerVertexColors: use separate colors/alpha values for each end point
            of the line (see <link SetPerVertexColors>, only the first 2 colors
            are used) }
    class procedure DrawLine(const StartPoint, EndPoint: TMZPoint; const Color: Cardinal;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = []); overload; static; inline;

    { Summary:
        Draws a filled circle.
      Parameters:
        X: X-coordinate of the center of the circle.
        Y: Y-coordinate of the center of the circle.
        Radius: Radius of the circle.
        Color: fill color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Quality: (optional) Number of line segments used to approximate half the
          circle (default 32). }
    class procedure FillCircle(const X, Y, Radius: Single; const Color: Cardinal;
      const Alpha: Byte = $FF; const Quality: Word = 32); overload; static; inline;

    { Summary:
        Draws a filled circle.
      Parameters:
        Center: Coordinates of the center of the circle.
        Radius: Radius of the circle.
        Color: fill color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Quality: (optional) Number of line segments used to approximate half the
          circle (default 32). }
    class procedure FillCircle(const Center: TMZPoint; const Radius: Single;
      const Color: Cardinal; const Alpha: Byte = $FF;
      const Quality: Word = 32); overload; static; inline;

    { Summary:
        Draws an outlined circle.
      Parameters:
        X: X-coordinate of the center of the circle.
        Y: Y-coordinate of the center of the circle.
        Radius: Radius of the circle.
        Color: outline color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Quality: (optional) Number of line segments used to approximate half the
          circle (default 32). }
    class procedure DrawCircle(const X, Y, Radius: Single; const Color: Cardinal;
      const Alpha: Byte = $FF; const Quality: Word = 32); overload; static; inline;

    { Summary:
        Draws an outlined circle.
      Parameters:
        Center: Coordinates of the center of the circle.
        Radius: Radius of the circle.
        Color: outline color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Quality: (optional) Number of line segments used to approximate half the
          circle (default 32). }
    class procedure DrawCircle(const Center: TMZPoint; const Radius: Single;
      const Color: Cardinal; const Alpha: Byte = $FF;
      const Quality: Word = 32); overload; static; inline;

    { Summary:
        Draws a filled ellipse.
      Parameters:
        X: X-coordinate of the center of the ellipse.
        Y: Y-coordinate of the center of the ellipse.
        XRadius: Horizontal radius of the ellipse.
        YRadius: Vertical radius of the ellipse.
        Color: fill color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Quality: (optional) Number of line segments used to approximate half the
          ellipse (default 32). }
    class procedure FillEllipse(const X, Y, XRadius, YRadius: Single;
      const Color: Cardinal; const Alpha: Byte = $FF;
      const Quality: Word = 32); overload; static; inline;

    { Summary:
        Draws a filled ellipse.
      Parameters:
        Center: Coordinates of the center of the ellipse.
        XRadius: Horizontal radius of the ellipse.
        YRadius: Vertical radius of the ellipse.
        Color: fill color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Quality: (optional) Number of line segments used to approximate half the
          ellipse (default 32). }
    class procedure FillEllipse(const Center: TMZPoint;
      const XRadius, YRadius: Single; const Color: Cardinal;
      const Alpha: Byte = $FF; const Quality: Word = 32); overload; static; inline;

    { Summary:
        Draws an outlined ellipse.
      Parameters:
        X: X-coordinate of the center of the ellipse.
        Y: Y-coordinate of the center of the ellipse.
        XRadius: Horizontal radius of the ellipse.
        YRadius: Vertical radius of the ellipse.
        Color: outline color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Quality: (optional) Number of line segments used to approximate half the
          ellipse (default 32). }
    class procedure DrawEllipse(const X, Y, XRadius, YRadius: Single;
      const Color: Cardinal; const Alpha: Byte = $FF;
      const Quality: Word = 32); overload; static; inline;

    { Summary:
        Draws an outlined ellipse.
      Parameters:
        Center: Coordinates of the center of the ellipse.
        XRadius: Horizontal radius of the ellipse.
        YRadius: Vertical radius of the ellipse.
        Color: outline color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Quality: (optional) Number of line segments used to approximate half the
          ellipse (default 32). }
    class procedure DrawEllipse(const Center: TMZPoint;
      const XRadius, YRadius: Single; const Color: Cardinal;
      const Alpha: Byte = $FF; const Quality: Word = 32); overload; static; inline;

    { Summary:
        Draws a filled rectangle.
      Parameters:
        X: X-coordinate of the top-left corner.
        Y: Y-coordinate of the top-left corner.
        W: Width of the rectangle.
        H: Height of the rectangle.
        Color: fill color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: optional effect flags. Recognized flags are:
          efPerVertexColors: use separate colors/alpha values for each corner of
            the rectangle (see <link SetPerVertexColors>) }
    class procedure FillRect(const X, Y, W, H: Single; const Color: Cardinal;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = []); overload; static; inline;

    { Summary:
        Draws a filled rectangle.
      Parameters:
        Rect: The rectangle coordinates.
        Color: fill color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: optional effect flags. Recognized flags are:
          efPerVertexColors: use separate colors/alpha values for each corner of
            the rectangle (see <link SetPerVertexColors>) }
    class procedure FillRect(const Rect: TMZRect; const Color: Cardinal;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = []); overload; static; inline;

    { Summary:
        Draws an outlined rectangle.
      Parameters:
        X: X-coordinate of the top-left corner.
        Y: Y-coordinate of the top-left corner.
        W: Width of the rectangle.
        H: Height of the rectangle.
        Color: outline color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: optional effect flags. Recognized flags are:
          efPerVertexColors: use separate colors/alpha values for each corner of
            the rectangle (see <link SetPerVertexColors>) }
    class procedure DrawRect(const X, Y, W, H: Single; const Color: Cardinal;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = []); overload; static; inline;

    { Summary:
        Draws an outlined rectangle.
      Parameters:
        Rect: The rectangle coordinates.
        Color: outline color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: optional effect flags. Recognized flags are:
          efPerVertexColors: use separate colors/alpha values for each corner of
            the rectangle (see <link SetPerVertexColors>) }
    class procedure DrawRect(const Rect: TMZRect; const Color: Cardinal;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = []); overload; static; inline;

    { Summary:
        Draws an outlined polygon.
      Parameters:
        Points: the points of the polygon.
        Closed: whether to draw a closed or open polygon.
        Color: fill color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
      Remarks:
        There is no method for filling polygons, but you can construct polygons
        using triangle lists and fill the using <link FillTriangleList>. }
    class procedure DrawPolygon(const Points: array of TMZPoint;
      const Closed: Boolean; const Color: Cardinal;
      const Alpha: Byte = $FF); overload; static;

    { Summary:
        Draws an outlined polygon.
      Parameters:
        Points: pointer to the first point of the polygon.
        Count: number of points in the polygon.
        Closed: whether to draw a closed or open polygon.
        Color: fill color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
      Remarks:
        There is no method for filling polygons, but you can construct polygons
        using triangle lists and fill the using <link FillTriangleList>. }
    class procedure DrawPolygon(const Points: PMZPoint; const Count: Integer;
      const Closed: Boolean; const Color: Cardinal;
      const Alpha: Byte = $FF); overload; static; inline;

    { Summary:
        Draws a filled triangle list.
      Parameters:
        Triangles: the list of triangles to fill.
        Color: fill color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque). }
    class procedure FillTriangleList(const Triangles: array of TMZTriangle;
      const Color: Cardinal; const Alpha: Byte = $FF); overload; static;

    { Summary:
        Draws a filled triangle list.
      Parameters:
        Triangles: pointer to the first triangle in the list.
        Count: number of triangles in the list.
        Color: fill color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque). }
    class procedure FillTriangleList(const Triangles: PMZTriangle;
      const Count: Integer; const Color: Cardinal;
      const Alpha: Byte = $FF); overload; static; inline;

    { Summary:
        Draws an outlined triangle list.
      Parameters:
        Triangles: the list of triangles to fill.
        Color: fill color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque). }
    class procedure DrawTriangleList(const Triangles: array of TMZTriangle;
      const Color: Cardinal; const Alpha: Byte = $FF); overload; static;

    { Summary:
        Draws an outlined triangle list.
      Parameters:
        Triangles: pointer to the first triangle in the list.
        Count: number of triangles in the list.
        Color: fill color in $RRGGBB format.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque). }
    class procedure DrawTriangleList(const Triangles: PMZTriangle;
      const Count: Integer; const Color: Cardinal;
      const Alpha: Byte = $FF); overload; static; inline;

    { Summary:
        Draws some text.
      Parameters:
        Font: The font to use to draw the text.
        X: The X-coordinate to use (values depend on Flags).
        Y: The Y-coordinate to use (values depend on Flags).
        Text: The text to draw. Can contain LF symbols (#10) to force line breaks.
        Flags: (optional) effects and alignment flags.
      Remarks:
        The Text can be encoded using WINDOWS-1251 (default for FreePascal
        and Delphi < 2009) and UTF-8 (if aoUseUTF8 is set in
        <link TMZApplication.Options>). For Delphi 2009+, UTF-16 is used. }
    class procedure DrawText(const Font: TMZFont; const X, Y: Single;
      const Text: UTF8String; const Flags: TMZTextFlags = []); overload; static; inline;

    { Summary:
        Draws some text.
      Parameters:
        Font: The font to use to draw the text.
        Point: The coordinates to use (values depend on Flags).
        Text: The text to draw. Can contain LF symbols (#10) to force line breaks.
        Flags: (optional) effects and alignment flags.
      Remarks:
        The Text can be encoded using WINDOWS-1251 (default for FreePascal
        and Delphi < 2009) and UTF-8 (if aoUseUTF8 is set in
        <link TMZApplication.Options>). For Delphi 2009+, UTF-16 is used. }
    class procedure DrawText(const Font: TMZFont; const Point: TMZPoint;
      const Text: UTF8String; const Flags: TMZTextFlags = []); overload; static; inline;

    { Summary:
        Draws some text.
      Parameters:
        Font: The font to use to draw the text.
        X: The X-coordinate to use (values depend on Flags).
        Y: The Y-coordinate to use (values depend on Flags).
        Scale: Scale coefficient for the symbols (1.0 for no scaling).
        Step: Offset between symbols.
        Text: The text to draw. Can contain LF symbols (#10) to force line breaks.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Color: (optional) text color in $RRGGBB format.
        Flags: (optional) effects and alignment flags.
      Remarks:
        The Text can be encoded using WINDOWS-1251 (default for FreePascal
        and Delphi < 2009) and UTF-8 (if aoUseUTF8 is set in
        <link TMZApplication.Options>). For Delphi 2009+, UTF-16 is used. }
    class procedure DrawText(const Font: TMZFont; const X, Y, Scale, Step: Single;
      const Text: UTF8String; const Alpha: Byte = $FF;
      const Color: Cardinal = $00FFFFFF; const Flags: TMZTextFlags = []); overload; static; inline;

    { Summary:
        Draws some text.
      Parameters:
        Font: The font to use to draw the text.
        Point: The coordinates to use (values depend on Flags).
        Scale: Scale coefficient for the symbols (1.0 for no scaling).
        Step: Offset between symbols.
        Text: The text to draw. Can contain LF symbols (#10) to force line breaks.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Color: (optional) text color in $RRGGBB format.
        Flags: (optional) effects and alignment flags.
      Remarks:
        The Text can be encoded using WINDOWS-1251 (default for FreePascal
        and Delphi < 2009) and UTF-8 (if aoUseUTF8 is set in
        <link TMZApplication.Options>). For Delphi 2009+, UTF-16 is used. }
    class procedure DrawText(const Font: TMZFont; const Point: TMZPoint;
      const Scale, Step: Single; const Text: UTF8String; const Alpha: Byte = $FF;
      const Color: Cardinal = $00FFFFFF; const Flags: TMZTextFlags = []); overload; static; inline;

    { Summary:
        Draws some text.
      Parameters:
        Font: The font to use to draw the text.
        Rect: The region where the text will be rendered. Bottom of rectangle
          will be ignored if the text does not fit.
        Text: The text to draw. Can contain LF symbols (#10) to force line breaks.
        Flags: (optional) effects and alignment flags.
      Remarks:
        The Text can be encoded using WINDOWS-1251 (default for FreePascal
        and Delphi < 2009) and UTF-8 (if aoUseUTF8 is set in
        <link TMZApplication.Options>). For Delphi 2009+, UTF-16 is used. }
    class procedure DrawText(const Font: TMZFont; const Rect: TMZRect;
      const Text: UTF8String; const Flags: TMZTextFlags = []); overload; static; inline;

    { Summary:
        Draws some text.
      Parameters:
        Font: The font to use to draw the text.
        Rect: The region where the text will be rendered. Bottom of rectangle
          will be ignored if the text does not fit.
        Scale: Scale coefficient for the symbols (1.0 for no scaling).
        Step: Offset between symbols.
        Text: The text to draw. Can contain LF symbols (#10) to force line breaks.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Color: (optional) text color in $RRGGBB format.
        Flags: (optional) effects and alignment flags.
      Remarks:
        The Text can be encoded using WINDOWS-1251 (default for FreePascal
        and Delphi < 2009) and UTF-8 (if aoUseUTF8 is set in
        <link TMZApplication.Options>). For Delphi 2009+, UTF-16 is used. }
    class procedure DrawText(const Font: TMZFont; const Rect: TMZRect;
      const Scale, Step: Single; const Text: UTF8String; const Alpha: Byte = $FF;
      const Color: Cardinal = $00FFFFFF; const Flags: TMZTextFlags = []); overload; static; inline;

    { Summary:
        Calculate the width of the given text in pixels.
      Parameters:
        Font: The font to use to calculate the width.
        Text: The text for which you want to calculate the width.
        Step: (optional) Offset between symbols.
      Returns:
        The width of the text in pixels.
      Remarks:
        The Text can be encoded using WINDOWS-1251 (default for FreePascal
        and Delphi < 2009) and UTF-8 (if aoUseUTF8 is set in
        <link TMZApplication.Options>). For Delphi 2009+, UTF-16 is used. }
    class function CalculateTextWidth(const Font: TMZFont; const Text: UTF8String;
      const Step: Single = 0): Single; static; inline;

    { Summary:
        Calculate the height of the given text in pixels.
      Parameters:
        Font: The font to use to calculate the height.
        Width: The width of the rectangle to use for word wrapping.
        Text: The text for which you want to calculate the height. Can contain
          LF symbols (#10) to force line breaks.
        Scale: (optional) Scale coefficient for the symbols (1.0 for no scaling).
        Step: (optional) Offset between symbols.
      Returns:
        The height of the text in pixels.
      Remarks:
        The Text can be encoded using WINDOWS-1251 (default for FreePascal
        and Delphi < 2009) and UTF-8 (if aoUseUTF8 is set in
        <link TMZApplication.Options>). For Delphi 2009+, UTF-16 is used. }
    class function CalculateTextHeight(const Font: TMZFont; const Width: Single;
      const Text: UTF8String; const Scale: Single = 1.0;
      const Step: Single = 0): Single; static; inline;

    { Summary:
        Draws a texture with given parameters and texture coordinates.
      Parameters:
        Texture: the texture to draw.
        TL: top-left texture coordinate.
        TR: top-right texture coordinate.
        BR: bottom-right texture coordinate.
        BL: bottom-left texture coordinate.
        DX: Destination X-coordinate to render the texture to.
        DY: Destination Y-coordinate to render the texture to.
        DW: Destination width.
        DH: Destination height.
        RotationAngle: (optional) angle of rotation (in degrees) relative to
          the center of the texture.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX, efFlipY, efPerVertexColors,
          efOffsetVertices and efScale.
      Remarks:
        The texture coordinates go from Top-Right to Bottom-Left in clockwise
        order. Each coordinate ranges from 0.0 to 1.0 (where 0.0 is the left/top
        edge of the texture, and 1.0 is the right/bottom edge of the texture).
        See <link DrawSprite> for a version that uses the entire texture
        instead of the given texture coordinates. }
    class procedure DrawTexture(const Texture: TMZTexture; const TL, TR, BR,
      BL: TMZPoint; const DX, DY, DW, DH: Single; const RotationAngle: Single = 0;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = [efBlend]); overload; static; inline;

    { Summary:
        Draws a texture with given parameters and texture coordinates.
      Parameters:
        Texture: the texture to draw.
        TL: top-left texture coordinate.
        TR: top-right texture coordinate.
        BR: bottom-right texture coordinate.
        BL: bottom-left texture coordinate.
        DR: Destination rectangle to render the texture to.
        RotationAngle: (optional) angle of rotation (in degrees) relative to
          the center of the texture.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX, efFlipY, efPerVertexColors,
          efOffsetVertices and efScale.
      Remarks:
        The texture coordinates go from Top-Right to Bottom-Left in clockwise
        order. Each coordinate ranges from 0.0 to 1.0 (where 0.0 is the left/top
        edge of the texture, and 1.0 is the right/bottom edge of the texture).
        See <link DrawSprite> for a version that uses the entire texture
        instead of the given texture coordinates. }
    class procedure DrawTexture(const Texture: TMZTexture; const TL, TR, BR,
      BL: TMZPoint; const DR: TMZRect; const RotationAngle: Single = 0;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = [efBlend]); overload; static; inline;

    { Summary:
        Draws a texture with given parameters and texture coordinates.
      Parameters:
        Texture: the texture to draw.
        TextureCoords: the texture coordinates (going from Top-Right to
          Bottom-Left in clockwise order). Each coordinate ranges from 0.0 to
          1.0 (where 0.0 is the left/top edge of the texture, and 1.0 is the
          right/bottom edge of the texture).
        DX: Destination X-coordinate to render the texture to.
        DY: Destination Y-coordinate to render the texture to.
        DW: Destination width.
        DH: Destination height.
        RotationAngle: (optional) angle of rotation (in degrees) relative to
          the center of the texture.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX, efFlipY, efPerVertexColors,
          efOffsetVertices and efScale.
      Remarks:
        See <link DrawSprite> for a version that uses the entire texture
        instead of the given texture coordinates. }
    class procedure DrawTexture(const Texture: TMZTexture;
      const TextureCoords: TMZTextureCoordinates; const DX, DY, DW, DH: Single;
      const RotationAngle: Single = 0; const Alpha: Byte = $FF;
      const Flags: TMZEffectFlags = [efBlend]); overload; static; inline;

    { Summary:
        Draws a texture with given parameters and texture coordinates.
      Parameters:
        Texture: the texture to draw.
        TextureCoords: the texture coordinates (going from Top-Right to
          Bottom-Left in clockwise order). Each coordinate ranges from 0.0 to
          1.0 (where 0.0 is the left/top edge of the texture, and 1.0 is the
          right/bottom edge of the texture).
        DR: Destination rectangle to render the texture to.
        RotationAngle: (optional) angle of rotation (in degrees) relative to
          the center of the texture.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX, efFlipY, efPerVertexColors,
          efOffsetVertices and efScale.
      Remarks:
        See <link DrawSprite> for a version that uses the entire texture
        instead of the given texture coordinates. }
    class procedure DrawTexture(const Texture: TMZTexture;
      const TextureCoords: TMZTextureCoordinates; const DR: TMZRect;
      const RotationAngle: Single = 0; const Alpha: Byte = $FF;
      const Flags: TMZEffectFlags = [efBlend]); overload; static; inline;

    { Summary:
        Draws a texture using a triangle list and texture coordinates.
      Parameters:
        Texture: the texture to draw.
        TriangleList: the list of triangles to use.
        TextureCoords: the texture coordinates for each triangle.
        StartIndex: the start index in the triangle list (and texture
          coordinates).
        EndIndex: the end index in the triangle list (and texture
          coordinates).
        Color: (optional) color to use for blending the texture
          (in $RRGGBB format).
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX, efFlipY, efPerVertexColors,
          efOffsetVertices and efScale. }
    class procedure DrawTexture(const Texture: TMZTexture;
      const TriangleList, TextureCoords: array of TMZPoint; const StartIndex,
      EndIndex: Integer; const Color: Cardinal = $FFFFFF; const Alpha: Byte = $FF;
      const Flags: TMZEffectFlags = [efBlend]); overload; static;

    { Summary:
        Draws a (static) sprite (texture) with given parameters.
      Parameters:
        Texture: the texture for the sprite to draw.
        DX: Destination X-coordinate to render the texture to.
        DY: Destination Y-coordinate to render the texture to.
        DW: Destination width.
        DH: Destination height.
        RotationAngle: (optional) angle of rotation (in degrees) relative to
          the center of the sprite.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX, efFlipY, efPerVertexColors,
          efOffsetVertices and efScale.
      Remarks:
        For more control, see <link DrawSpriteFrame> and <link DrawTexture>. }
    class procedure DrawSprite(const Texture: TMZTexture;
      const DX, DY, DW, DH: Single; const RotationAngle: Single = 0;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = [efBlend]); overload; static; inline;

    { Summary:
        Draws a (static) sprite (texture) with given parameters.
      Parameters:
        Texture: the texture for the sprite to draw.
        DR: Destination rectangle to render the sprite to.
        RotationAngle: (optional) angle of rotation (in degrees) relative to
          the center of the sprite.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX, efFlipY, efPerVertexColors,
          efOffsetVertices and efScale.
      Remarks:
        For more control, see <link DrawSpriteFrame> and <link DrawTexture>. }
    class procedure DrawSprite(const Texture: TMZTexture; const DR: TMZRect;
      const RotationAngle: Single = 0; const Alpha: Byte = $FF;
      const Flags: TMZEffectFlags = [efBlend]); overload; static; inline;

    { Summary:
        Draws a part of (static) sprite (texture) with given parameters.
      Parameters:
        Texture: the texture for the sprite to draw.
        SR: Source rectangle containing the part of the sprite to draw. The
          coordinates of this rectangle must lie within the size of the sprite.
        DR: Destination rectangle to render the sprite to.
        RotationAngle: (optional) angle of rotation (in degrees) relative to
          the center of the sprite.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX, efFlipY, efPerVertexColors,
          efOffsetVertices and efScale.
      Remarks:
        For more control, see <link DrawSpriteFrame> and <link DrawTexture>. }
    class procedure DrawSprite(const Texture: TMZTexture; const SR, DR: TMZRect;
      const RotationAngle: Single = 0; const Alpha: Byte = $FF;
      const Flags: TMZEffectFlags = [efBlend]); overload; static; inline;

    { Summary:
        Draws a part of (static) sprite (texture) with given parameters.
      Parameters:
        Texture: the texture for the sprite to draw.
        SR: Source rectangle containing the part of the sprite to draw. The
          coordinates of this rectangle must lie within the size of the sprite.
        DX: Destination X-coordinate to render the sprite to.
        DY: Destination Y-coordinate to render the sprite to.
        DW: Destination width.
        DH: Destination height.
        RotationAngle: (optional) angle of rotation (in degrees) relative to
          the center of the sprite.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX, efFlipY, efPerVertexColors,
          efOffsetVertices and efScale.
      Remarks:
        For more control, see <link DrawSpriteFrame> and <link DrawTexture>. }
    class procedure DrawSprite(const Texture: TMZTexture; const SR: TMZRect;
      const DX, DY, DW, DH: Single; const RotationAngle: Single = 0; const Alpha: Byte = $FF;
      const Flags: TMZEffectFlags = [efBlend]); overload; static; inline;

    { Summary:
        Draws a sprite frame (for example for animated sprites or tiles in a
        tileset).
      Parameters:
        Texture: the texture containing the sprited frames.
        FrameNum: the frame number to draw. NOTE: Frame numbers are 1 based
          (so range from 1 to FrameCount).
        DX: Destination X-coordinate to render the sprite to.
        DY: Destination Y-coordinate to render the sprite to.
        DW: Destination width.
        DH: Destination height.
        RotationAngle: (optional) angle of rotation (in degrees) relative to
          the center of the sprite.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX, efFlipY, efPerVertexColors,
          efOffsetVertices and efScale.
      Remarks:
        The texture should have been setup for animated sprites using
        <link TMZTexture.SetFrameSize>. }
    class procedure DrawSpriteFrame(const Texture: TMZTexture; const FrameNum: Integer;
      const DX, DY, DW, DH: Single; const RotationAngle: Single = 0;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = [efBlend]); overload; static; inline;

    { Summary:
        Draws a sprite frame (for example for animated sprites or tiles in a
        tileset).
      Parameters:
        Texture: the texture containing the sprited frames.
        FrameNum: the frame number to draw. NOTE: Frame numbers are 1 based
          (so range from 1 to FrameCount).
        DR: Destination rectangle to render the sprite to.
        RotationAngle: (optional) angle of rotation (in degrees) relative to
          the center of the sprite.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX, efFlipY, efPerVertexColors,
          efOffsetVertices and efScale.
      Remarks:
        The texture should have been setup for animated sprites using
        <link TMZTexture.SetFrameSize>. }
    class procedure DrawSpriteFrame(const Texture: TMZTexture;
      const FrameNum: Integer; const DR: TMZRect; const RotationAngle: Single = 0;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = [efBlend]); overload; static; inline;

    { Summary:
        Draw a set of sprite frames as tiles in a grid.
      Parameters:
        Texture: the texture containing the sprited frames.
        XOffset: the X-offset to use.
        YOffset: the Y-offset to use.
        Frames: a 2-dimensional array of frame numbers to draw in the grid (X
          first, as in Frames[X,Y]). NOTE: Frame numbers are 1 based (so range
          from 1 to FrameCount).
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efScale, efOffsetVertices, efAlternatePivotPoint,
          efFlipX, efFlipY, efColos and efPerVertexColors
      Remarks:
        The Frames matrix can be (much) larger than the visible screen.
        Depending on the size of this matrix and XOffset and YOffset, only the
        visible subset of the tiles will be painted. This makes it possible to
        create a large map and scroll through it by modifying XOffset and
        YOffset. Remember that XOffset and YOffset point to the origin of the
        map in relation to the screen. So these offsets are usually negative
        when the map is larger than the screen. }
    class procedure DrawSpriteTiles(const Texture: TMZTexture; const XOffset,
      YOffset: Single; const Frames: TMZTileFrames; const Alpha: Byte = $FF;
      const Flags: TMZEffectFlags = [efBlend]); overload; static;

    { Summary:
        Draw a set of sprite frames as tiles in a grid.
      Parameters:
        Texture: the texture containing the sprited frames.
        Offset: the offset to use.
        Frames: a 2-dimensional array of frame numbers to draw in the grid (X
          first, as in Frames[X,Y]). NOTE: Frame numbers are 1 based (so range
          from 1 to FrameCount).
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efScale, efOffsetVertices, efAlternatePivotPoint,
          efFlipX, efFlipY, efColos and efPerVertexColors
      Remarks:
        The Frames matrix can be (much) larger than the visible screen.
        Depending on the size of this matrix and XOffset and YOffset, only the
        visible subset of the tiles will be painted. This makes it possible to
        create a large map and scroll through it by modifying XOffset and
        YOffset. Remember that XOffset and YOffset point to the origin of the
        map in relation to the screen. So these offsets are usually negative
        when the map is larger than the screen. }
    class procedure DrawSpriteTiles(const Texture: TMZTexture;
      const Offset: TMZPoint; const Frames: TMZTileFrames; const Alpha: Byte = $FF;
      const Flags: TMZEffectFlags = [efBlend]); overload; static;

    { Summary:
        Draw a set of sprite frames as tiles in a grid.
      Parameters:
        Texture: the texture containing the sprited frames.
        XOffset: the X-offset to use.
        YOffset: the Y-offset to use.
        TileWidth: target width of each tile (if different from the width of
          the sprite frames).
        TileHeight: target height of each tile (if different from the height of
          the sprite frames).
        Frames: a 2-dimensional array of frame numbers to draw in the grid (X
          first, as in Frames[X,Y]). NOTE: Frame numbers are 1 based (so range
          from 1 to FrameCount).
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efScale, efOffsetVertices, efAlternatePivotPoint,
          efFlipX, efFlipY, efColos and efPerVertexColors
      Remarks:
        The Frames matrix can be (much) larger than the visible screen.
        Depending on the size of this matrix and XOffset and YOffset, only the
        visible subset of the tiles will be painted. This makes it possible to
        create a large map and scroll through it by modifying XOffset and
        YOffset. Remember that XOffset and YOffset point to the origin of the
        map in relation to the screen. So these offsets are usually negative
        when the map is larger than the screen. }
    class procedure DrawSpriteTiles(const Texture: TMZTexture; const XOffset,
      YOffset, TileWidth, TileHeight: Single; const Frames: TMZTileFrames;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = [efBlend]); overload; static;

    { Summary:
        Draw a set of sprite frames as tiles in a grid.
      Parameters:
        Texture: the texture containing the sprited frames.
        Offset: the offset to use.
        TileWidth: target width of each tile (if different from the width of
          the sprite frames).
        TileHeight: target height of each tile (if different from the height of
          the sprite frames).
        Frames: a 2-dimensional array of frame numbers to draw in the grid (X
          first, as in Frames[X,Y]). NOTE: Frame numbers are 1 based (so range
          from 1 to FrameCount).
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efScale, efOffsetVertices, efAlternatePivotPoint,
          efFlipX, efFlipY, efColos and efPerVertexColors
      Remarks:
        The Frames matrix can be (much) larger than the visible screen.
        Depending on the size of this matrix and XOffset and YOffset, only the
        visible subset of the tiles will be painted. This makes it possible to
        create a large map and scroll through it by modifying XOffset and
        YOffset. Remember that XOffset and YOffset point to the origin of the
        map in relation to the screen. So these offsets are usually negative
        when the map is larger than the screen. }
    class procedure DrawSpriteTiles(const Texture: TMZTexture;
      const Offset: TMZPoint; const TileWidth, TileHeight: Single;
      const Frames: TMZTileFrames; const Alpha: Byte = $FF;
      const Flags: TMZEffectFlags = [efBlend]); overload; static;

    { Summary:
        Sets different colors and alpha values for each vertex of a primitive.
        These colors are used when the efPerVertexColors flag or
        tfPerVertexColors flag is set in one of the drawing methods.
      Parameters:
        C1: 1st vertex color in $RRGGBB format.
        C2: 2nd vertex color in $RRGGBB format.
        C3: 3rd vertex color in $RRGGBB format.
        C4: 4th vertex color in $RRGGBB format.
        A1: 1st vertex alpha transparency.
        A2: 2nd vertex alpha transparency.
        A3: 3rd vertex alpha transparency.
        A4: 4th vertex alpha transparency.
      Remarks:
        This version accepts 4 colors, so is useful for drawing functions like
        <link FillRect> and <link DrawRect>. }
    class procedure SetPerVertexColors(const C1, C2, C3, C4: Cardinal;
      const A1, A2, A3, A4: Byte); overload; static; inline;

    { Summary:
        Sets different colors and alpha values for each vertex of a primitive.
        These colors are used when the efPerVertexColors is set in one of the
        drawing methods.
      Parameters:
        C1: 1st vertex color in $RRGGBB format.
        C2: 2nd vertex color in $RRGGBB format.
        A1: 1st vertex alpha transparency.
        A2: 2nd vertex alpha transparency.
      Remarks:
        This version accepts 2 colors, so is useful for drawing functions like
        <link DrawLine>. }
    class procedure SetPerVertexColors(const C1, C2: Cardinal;
      const A1, A2: Byte); overload; static; inline;

    { Summary:
        Set the scale to use when the efScale flag is set when drawing a
        texture or sprite.
      Parameters:
        Scale: the scale factor to use. This factor is used for both horizontal
          and vertical scaling. }
    class procedure SetScale(const Scale: Single); overload; static; inline;

    { Summary:
        Set the scale to use when the efScale flag is set when drawing a
        texture or sprite.
      Parameters:
        ScaleX: horizontal scale factor.
        ScaleY: vertical scale factor. }
    class procedure SetScale(const ScaleX, ScaleY: Single); overload; static; inline;

    { Summary:
        Sets the vertex offsets to use when the efOffsetVertices flag is set
        when drawing a texture or sprite.
      Parameters:
        X1: X-vertex offset for 1st texture corner.
        Y1: Y-vertex offset for 1st texture corner.
        X2: X-vertex offset for 2nd texture corner.
        Y2: Y-vertex offset for 2nd texture corner.
        X3: X-vertex offset for 3rd texture corner.
        Y3: Y-vertex offset for 3rd texture corner.
        X4: X-vertex offset for 4th texture corner.
        Y4: Y-vertex offset for 4th texture corner. }
    class procedure SetVertexOffsets(const X1, Y1, X2, Y2, X3, Y3, X4,
      Y4: Single); overload; static; inline;

    { Summary:
        Sets the vertex offsets to use when the efOffsetVertices flag is set
        when drawing a texture or sprite.
      Parameters:
        P1: vertex offset for 1st texture corner.
        P2: vertex offset for 2nd texture corner.
        P3: vertex offset for 3rd texture corner.
        P4: vertex offset for 4th texture corner. }
    class procedure SetVertexOffsets(const P1, P2, P3, P4: TMZPoint); overload; static; inline;

    { Summary:
        Specifies which color components can or cannot be written to the frame
        buffer.
      Parameters:
        R: True to write the Red component, False otherwise.
        G: True to write the Green component, False otherwise.
        B: True to write the Blue component, False otherwise.
        A: True to write the ALpha component, False otherwise. }
    class procedure SetColorMask(const R, G, B, A: Boolean); static; inline;

    { Summary:
        When a texture/sprite is rotated, it rotates around its center by
        default. If the efAlternatePivotPoint effect flag is used, then the
        sprite will rotate around the pivot point set by this method instead.
      Parameters:
        Pivot: the pivot point to use for rotation. }
    class procedure SetPivotPoint(const Pivot: TMZPoint); overload; static; inline;

    { Summary:
        When a texture/sprite is rotated, it rotates around its center by
        default. If the efAlternatePivotPoint effect flag is used, then the
        sprite will rotate around the pivot point set by this method instead.
      Parameters:
        X: the X-coordinate of the pivot point to use for rotation.
        Y: the Y-coordinate of the pivot point to use for rotation. }
    class procedure SetPivotPoint(const X, Y: Single); overload; static; inline;

    { Summary:
        Begins processing a batch of similar operations (using the same
        textures etc.). This can improve performance.
      Remarks:
        You <b>must</b> call <link EndBatch> afterwards. }
    class procedure BeginBatch; static; inline;

    { Summary:
        Ends the batch started with <link BeginBatch>.
      Remarks:
        This will automatically flush the current batch (see <link FlushBatch>). }
    class procedure EndBatch; static; inline;

    { Summary:
        Flushes the batch started with <link BeginBatch>.
      Remarks:
        You rarely need to use this method as it is called automatically by
        <link EndBatch>. }
    class procedure FlushBatch; static; inline;

    { Summary:
        Enables an option for controlling rendering and behavior.
      Remarks:
        See <link TMZCanvasOption> for available options. }
    class procedure EnableOption(const Option: TMZCanvasOption); static; inline;

    { Summary:
        Disables an option for controlling rendering and behavior.
      Remarks:
        See <link TMZCanvasOption> for available options. }
    class procedure DisableOption(const Option: TMZCanvasOption); static; inline;

    { Summary:
        Reads pixels from the canvas (for example, to take a screen capture).
      Parameters:
        Bounds: The rectangle of the canvas to read.
      Returns:
        A newly allocated memory buffer with the pixel data. Each pixel is
        in $AARRGGBB format.
      Remarks:
        It is your responsibility to free the returned buffer when you are
        done with it. }
    class function ReadPixels(const Bounds: TMZRect): Pointer; overload; static; inline;

    { Summary:
        Reads pixels from the canvas (for example, to take a screen capture).
      Parameters:
        X: X-coordinate of the rectangle of the canvas to read.
        Y: Y-coordinate of the rectangle of the canvas to read.
        W: Width of the rectangle of the canvas to read.
        H: Height of the rectangle of the canvas to read.
      Returns:
        A newly allocated memory buffer with the pixel data. Each pixel is
        in $AARRGGBB format.
      Remarks:
        It is your responsibility to free the returned buffer when you are
        done with it. }
    class function ReadPixels(const X, Y, W, H: Integer): Pointer; overload; static; inline;

    { Summary:
        Whether anti-aliasing (smoothing) should be used when drawing and
        filling shapes. }
    class property AntiAlias: Boolean read GetAntiAlias write SetAntiAlias;

    { Summary:
        The texture color to use (in $RRGGBB format) when the efColor flag is
        set when drawing a texture or sprite. }
    class property TextureColor: Cardinal read FTextureColor write SetTextureColor;

    { Summary:
        The texture color mode to use when the efColor flag is set when
        drawing a texture or sprite.
      Remarks:
        Defaults to cmMix to mix the color with the texture.
        Use cmReplace to replace the entire texture with the color. }
    class property ColorMode: TMZColorMode read FColorMode write SetColorMode default cmMix;

    { Summary:
        The blend mode to use when rendering textures.
      Remarks:
        Changing this property flushes the GPU, which can decrease performance.
        See also <link SeparateAlpha>. Defaults to bmNormal. }
    class property BlendMode: TMZBlendMode read FBlendMode write SetBlendMode default bmNormal;

    { Summary:
        Used in combination with <link BlendMode> to enable/disable separate
        blending mode for the alpha channel. Only works if the video card
        supports it.
      Remarks:
        See <link TMZUtils.SupportsSeparateAlpha> to check if the system
        supports it.
        Changing this property flushes the GPU, which can decrease performance.
        Defaults to True. }
    class property SeparateAlpha: Boolean read FSeparateAlpha write SetSeparateAlpha default True;

    { Summary:
        The horizontal scale factor to use when the efScale flag is set when
        drawing a texture or sprite. }
    class property ScaleX: Single read FScaleX write SetScaleX;

    { Summary:
        The vertical scale factor to use when the efScale flag is set when
        drawing a texture or sprite. }
    class property ScaleY: Single read FScaleY write SetScaleY;

    { Summary:
        The camera to use for subsequent drawing operations.
        Set to nil to reset to the default camera.
      Remarks:
        The canvas does <b>not</b> become owner of the camera. }
    class property Camera: TMZCamera read FCamera write SetCamera;

    { Summary:
        The render target that will be the destination for all drawing
        operations. Set to nil (default) to render directly to the screen.
      Remarks:
        The canvas does <b>not</b> become owner of the render target. }
    class property RenderTarget: TMZRenderTarget read FRenderTarget write SetRenderTarget;
  end;
{$ENDREGION 'Canvas and Drawing'}

{$REGION 'Sprites'}
type
  TMZSpriteEngine = class;

  { Summary:
      Abstract base class for 2D sprites. }
  TMZSprite = class abstract
  {$REGION 'Internal Declarations'}
  private
    class var FCanvas: TMZCanvas;
  private
    FIndex: Integer;
    FEngine: TMZSpriteEngine;
    FTexture: TMZTexture;
    FZOrder: Integer;
    FPosition: TMZPoint;
    FWidth: Single;
    FHeight: Single;
    FRotationAngle: Single;
    FFrameNumber: Single;
    FAlpha: Byte;
    FEffectFlags: TMZEffectFlags;
    FKill: Boolean;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a sprite object.
      Parameters:
        Engine: The sprite engine that will manage this sprite.
        Texture: The texture containing the sprite artwork.
        ZOrder: (optional) Layer order of the sprite. Sprites with a lower
          Z-Order value will be drawn before sprites with a higher Z-Order
          value.
      Remarks:
        The sprite engine will become owner of the sprite and will
        automatically destroy this sprite. However, you can manually free
        the sprite to remove it from the engine. }
    constructor Create(const Engine: TMZSpriteEngine; const Texture: TMZTexture;
      const ZOrder: Integer = 0);

    { Summary:
        Frees the sprite and removes it from the sprite engine. }
    procedure Free;

    { Summary:
        Is called once just after construction to initialize the sprite.
        You can override this method to initialize some sprite properties or
        perform other custom initialization.
      Remarks:
        By default, this method does nothing. }
    procedure Initialize; virtual;

    { Summary:
        Draws the current sprite frame.
      Remarks:
        The default implementation just calls TMZCanvas.DrawSpriteFrame to
        draw the frame. You can override this method if you need different
        behavior. }
    procedure Draw; virtual;

    { Summary:
        Is called during each iteration of the main loop.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update.
      Remarks:
        By default, this method does nothing. }
    procedure Update(const DeltaTimeMs: Double); virtual;

    { Summary:
        The canvas used for all drawing operations.
      Remarks:
        The canvas is global and shared with all MondoZenGL objects. }
    class property Canvas: TMZCanvas read FCanvas;

    { Summary:
        Index of the sprite in <link Engine>.
      Remarks:
        The index will change if sprites with a lower <link ZOrder> are
        deleted. }
    property Index: Integer read FIndex;

    { Summary:
        The engine that manages (and owns) this sprite. }
    property Engine: TMZSpriteEngine read FEngine;

    { Summary:
        The texture with the artwork for the sprite. }
    property Texture: TMZTexture read FTexture;

    { Summary:
        Layer order of the sprite. Sprites with a lower Z-Order value
        will be drawn before sprites with a higher Z-Order value. }
    property ZOrder: Integer read FZOrder;

    { Summary:
        Destination X-coordinate to render the sprite to.
      Remarks:
        Defaults to 0. }
    property X: Single read FPosition.X write FPosition.X;

    { Summary:
        Destination Y-coordinate to render the sprite to.
      Remarks:
        Defaults to 0. }
    property Y: Single read FPosition.Y write FPosition.Y;

    { Summary:
        Destination coordinates to render the sprite to.
      Remarks:
        Defaults to (0, 0). }
    property Position: TMZPoint read FPosition write FPosition;

    { Summary:
        Destination render width.
      Remarks:
        Defaults to the frame width in the <link Texture>. }
    property Width: Single read FWidth write FWidth;

    { Summary:
        Destination render height.
      Remarks:
        Defaults to the frame height in the <link Texture>. }
    property Height: Single read FHeight write FHeight;

    { Summary:
        Angle of rotation (in degrees) relative to the center of the sprite.
      Remarks:
        When rendering the sprite, you can use the efAlternatePivotPoint flag
        in combination with <link TMZCanvas.SetPivotPoint> to rotate around
        a different point than the center. }
    property RotationAngle: Single read FRotationAngle write FRotationAngle;

    { Summary:
        The current frame number of the sprite to draw.
      Remarks:
        Frame numbers are 1 based (so range from 1 to FrameCount).
        This frame number is a Single value for easy fractional updating. This
        means that when the sprite is drawn, the rounded value of this property
        is used. }
    property FrameNumber: Single read FFrameNumber write FFrameNumber;

    { Summary:
        Level of alpha transparency at which to draw the sprite (0=fully
        transparent, 255=fully opaque).
      Remarks:
        Defaults to 255 (opaque). }
    property Alpha: Byte read FAlpha write FAlpha;

    { Summary:
        Effect flags used for drawing the sprite. Supported flags are: efBlend,
        efColor, efFlipX, efFlipY, efPerVertexColors, efOffsetVertices and
        efScale.
      Remarks:
        Defaults := [efBlend]. }
    property EffectFlags: TMZEffectFlags read FEffectFlags write FEffectFlags;
  end;

  { Summary:
      A sprite engine for managing <link TMZSprite> objects.
    Remarks:
      To add a sprite, simply create a <link TMZSprite> descendent and pass this
      sprite engine as the first parameter.
      To delete a sprite, simply free it. }
  TMZSpriteEngine = class
  {$REGION 'Internal Declarations'}
  private
    FSprites: array of TMZSprite;
    FCount: Integer;
    FModified: Boolean;
    function GetSprite(const Index: Integer): TMZSprite;
  private
    function Add(const Sprite: TMZSprite): Integer;
    procedure DeleteDeadSprites;
    procedure SortByLayer(const ALo, AHi: Integer);
    procedure SortByIndex(const ALo, AHi: Integer);
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;

    { Summary:
        Clears (and frees) all the sprites owned by this engine. }
    procedure Clear;

    { Summary:
        Renders (draws) all the sprites owned by this engine.
      Remarks:
        The sprites are rendered in order of increasing <link TMZSprite.ZOrder> }
    procedure Render;

    { Summary:
        Call this method during each iteration of the main loop to update
        all the sprites owned by this engine.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update. }
    procedure Update(const DeltaTimeMs: Double);

    { Summary:
        The current number of sprites managed by this engine. }
    property Count: Integer read FCount;

    { Summary:
        The sprites managed by this engine. }
    property Sprites[const Index: Integer]: TMZSprite read GetSprite; default;
  end;
{$ENDREGION 'Sprites'}

{$REGION 'Particle Engine'}
type
  { Summary:
      Type of <link TMZParticleEmitter>. }
  TMZParticleEmitterType = (
    { Summary:
        No particle emitter. }
    etNone = EMITTER_NONE,

    { Summary:
        Particles emit from a single point. }
    etPoint = EMITTER_POINT,

    { Summary:
        Particles emit along a line. }
    etLine = EMITTER_LINE,

    { Summary:
        Particles emit from inside a rectangle. }
    etRectangle = EMITTER_RECTANGLE,

    { Summary:
        Particles emit from inside a circle. }
    etCircle = EMITTER_CIRCLE,

    { Summary:
        Particles emit from inside a ring (or donut shape). }
    etRing = EMITTER_RING);

type
  { Summary:
      A particle emitter. }
  TMZParticleEmitter = class
  {$REGION 'Internal Declarations'}
  private
    FEmitter: zglTEmitter2D;
    FHandle: zglPEmitter2D;
    function GetEmitterType: TMZParticleEmitterType; inline;
    function GetBlendMode: TMZBlendMode; inline;
    procedure SetBlendMode(const Value: TMZBlendMode); inline;
    function GetColorMode: TMZColorMode; inline;
    procedure SetColorMode(const Value: TMZColorMode); inline;
    function GetSpinAngle: Single; inline;
    procedure SetSpinAngle(const Value: Single); inline;
    function GetSpinAngleVariation: Single; inline;
    procedure SetSpinAngleVariation(const Value: Single); inline;
    function GetAngularVelocity: Single; inline;
    procedure SetAngularVelocity(const Value: Single); inline;
    function GetAngularVelocityVariation: Single; inline;
    procedure SetAngularVelocityVariation(const Value: Single); inline;
    function GetBoundingBox: TMZRect; inline;
    function GetStartFrameNumber: Integer; inline;
    procedure SetStartFrameNumber(const Value: Integer); inline;
    function GetEndFrameNumber: Integer; inline;
    procedure SetEndFrameNumber(const Value: Integer); inline;
  private
    procedure SetEmitterType(const Typ: Byte; const X, Y: Single);
    class function CopyEmitter(const Src: zglTEmitter2D): zglTEmitter2D; static;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a new, empty particle emitter.
      Parameters:
        Texture: The texture containing the particle image(s).
      Remarks:
        You usually will load the particle emitter settings from a file, using
        on of the other constructors. However, until there is a particle
        editor to create these files, you can also setup an emitter manually.
        The easiest way to do that is to call one of the SetXxxEmitter methods
        to broadly setup the emitter, followed by setting other properties
        and/or calling other SetXxx methods to tweak the emitter. }
    constructor Create(const Texture: TMZTexture); overload;

    { Summary:
        Creates a particle emitter from a file.
      Parameters:
        Filename: the name of the file containing the particle emmiter settings.
        Texture: (optional) The texture containing the particle image(s). If
          not set, the particle emitter file must contain a reference to a valid
          texture file.
      Remarks:
        Raises an exception if the file does not exist of is not an valid
        ZenGL particle emitter file. }
    constructor Create(const Filename: UTF8String; const Texture: TMZTexture = nil); overload;

    { Summary:
        Creates a particle emitter from a stream.
      Parameters:
        Stream: the stream containing the particle emmiter settings.
        Texture: (optional) The texture containing the particle image(s). If
          not set, the particle emitter file must contain a reference to a valid
          texture file.
      Remarks:
        The data is read from the current position in the stream.
        Raises an exception if the stream is not an valid ZenGL particle
        emitter. }
    constructor Create(const Stream: TStream; const Texture: TMZTexture = nil); overload;

    { Summary:
        Creates a particle emitter from a memory.
      Parameters:
        Buffer: the memory buffer to load the emitter from.
        Size: the size of the memory buffer.
        Texture: (optional) The texture containing the particle image(s). If
          not set, the particle emitter file must contain a reference to a valid
          texture file.
      Remarks:
        Raises an exception if the memory is not an valid ZenGL particle
        emitter. }
    constructor Create(const Buffer: Pointer;
      const Size: Integer; const Texture: TMZTexture = nil); overload;

    { Summary:
        Creates a particle emitter from a memory.
      Parameters:
        Memory: the memory buffer to load the emitter from.
        Texture: (optional) The texture containing the particle image(s). If
          not set, the particle emitter file must contain a reference to a valid
          texture file.
      Remarks:
        Raises an exception if the memory is not an valid ZenGL particle
        emitter. }
    constructor Create(const Memory: TMZMemory;
      const Texture: TMZTexture = nil); overload;

    { Summary:
        Creates a particle emitter by copying an existing emitter.
      Parameters:
        Source: the emitter to copy. }
    constructor Create(const Source: TMZParticleEmitter); overload;
    destructor Destroy; override;

    { Summary:
        Draws the particles for this emitter.
      Remarks:
        You usually call this method in the main render loop (like in
        <link TMZScene.RenderFrame>. }
    procedure Render;

    { Summary:
        Must be called during each iteration of the main loop to update the
        emitter.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update.
      Remarks:
        You usually call this from the <link TMZScene.Update> method. }
    procedure Update(const DeltaTimeMs: Double);

    { Summary:
        Sets the <link EmitterType> to a point emitter. A point emitter emits
        particles from a single point.
      Parameters:
        X: X-position of the emitter.
        Y: Y-position of the emitter.
        Direction: The direction angle that particles move in (in degrees). The
          direction angle moves clockwise from 0 degrees at the 3 o-clock
          position.
        Spread: The amount of spread along the direction angle (in degrees).
      Remarks:
        You can further customize the emitter by setting the properties or
        calling some other SetXxx methods. }
    procedure SetPointEmitter(const X, Y, Direction, Spread: Single);

    { Summary:
        Sets the <link EmitterType> to a line emitter. A line emitter emits
        particles along a line.
      Parameters:
        X: X-position of the emitter (center of the line).
        Y: Y-position of the emitter (center of the line).
        Length: The length of the line in pixels.
        Direction: The direction angle of the line. The direction angle moves
          clockwise from 0 degrees at the 12 o-clock position. The particles
          will emit in a direction perpendicular to this line.
        Spread: The amount of spread in particle angles (in degrees).
        TwoSided: (optional) Whether particles are emitted along both sides
          of the line (default False).
      Remarks:
        You can further customize the emitter by setting the properties or
        calling some other SetXxx methods. }
    procedure SetLineEmitter(const X, Y, Length, Direction, Spread: Single;
      const TwoSided: Boolean = False);

    { Summary:
        Sets the <link EmitterType> to a rectangle emitter. A rectangle emitter
        emits particles from inside a rectangle.
      Parameters:
        X: X-position of the top-left corner of the rectangle.
        Y: Y-position of the top-left corner of the rectangle.
        Width: Width of the rectangle.
        Height: Height of the rectangle.
        Direction: The direction angle that particles move in (in degrees). The
          direction angle moves clockwise from 0 degrees at the 3 o-clock
          position.
        Spread: The amount of spread along the direction angle (in degrees).
      Remarks:
        You can further customize the emitter by setting the properties or
        calling some other SetXxx methods. }
    procedure SetRectangleEmitter(const X, Y, Width, Height, Direction,
      Spread: Single);

    { Summary:
        Sets the <link EmitterType> to a circle emitter. A circle emitter emits
        particles from inside a circle.
      Parameters:
        X: X-position of the center of the circle.
        Y: Y-position of the center of the circle.
        Radius: Radius of the circle.
        Direction: The direction angle that particles move in (in degrees). The
          direction angle moves clockwise from 0 degrees at the 3 o-clock
          position.
        Spread: The amount of spread along the direction angle (in degrees).
      Remarks:
        You can further customize the emitter by setting the properties or
        calling some other SetXxx methods. }
    procedure SetCircleEmitter(const X, Y, Radius, Direction, Spread: Single);

    { Summary:
        Sets the <link EmitterType> to a ring emitter. A ring emitter emits
        particles from inside a ring (or donut shape).
      Parameters:
        X: X-position of the center of the ring.
        Y: Y-position of the center of the ring.
        Radius1: radius of the outer circle of the ring.
        Radius2: radius of the inner circle of the ring.
        Direction: The direction angle that particles move in (in degrees). The
          direction angle moves clockwise from 0 degrees at the 3 o-clock
          position.
        Spread: The amount of spread along the direction angle (in degrees).
      Remarks:
        You can further customize the emitter by setting the properties or
        calling some other SetXxx methods. }
    procedure SetRingEmitter(const X, Y, Radius1, Radius2, Direction,
      Spread: Single);

    { Summary:
        The particle emitter type. The value of this property depends on
        which SetXxxEmitter method is used. }
    property EmitterType: TMZParticleEmitterType read GetEmitterType;

    { Summary:
        X-coordinate of the location of the emitter.
      Remarks:
        For some emitter types (like point and line emitters), this is the
        position of the center of the emitter. For other emitter types (like
        rectangle emitters), this is the position of the top-left corner of
        the emitter. }
    property X: Single read FEmitter.Params.Position.X write FEmitter.Params.Position.X;

    { Summary:
        Y-coordinate of the location of the emitter.
      Remarks:
        For some emitter types (like point and line emitters), this is the
        position of the center of the emitter. For other emitter types (like
        rectangle emitters), this is the position of the top-left corner of
        the emitter. }
    property Y: Single read FEmitter.Params.Position.Y write FEmitter.Params.Position.Y;

    { Summary:
        Returns the number of milliseconds the emitter has been alive.
      Remarks:
        When <link Loop> is set to True, the age will reset every time the
        emitter dies. }
    property AgeMs: Double read FEmitter.Time;

    { Summary:
        Returns the number of particles that are currently alive. }
    property ParticleCount: Integer read FEmitter.Particles;

    { Summary:
        Returns the current bounding box of the emitter. This is the rectangle
        that currently surrounds all rendered particles for this emitter. }
    property BoundingBox: TMZRect read GetBoundingBox;
  public
    {**************************************************************************
     * The properties and methods below are for customizing the emitter settings
     * manually. There is usually no need to modify these settings if you load
     * the emitter settings from a file. However, since there is no particle
     * editor (yet) to create these files, you may need to set these properties
     * or call these methods manually.
     **************************************************************************}

    { Summary:
        Sets the colors of the particle to use during its lifetime.
      Parameters:
        Colors: an array of colors to use, in $RRGGBB format.
        Ages: the ages for each color, where 0.0 is when the particle starts,
          and 1.0 is where the particle dies.
      Remarks:
        The colors will gradually change along the gradient defined by the
        Ages array. If the Ages array contains less entries than the Colors
        array, then the remaining ages will be set to 1.0. If the Ages array
        contains more entries then the Colors array, then the extra ages will
        be ignored.

        For example:
          <code>SetColors([$000000, $FFFFFF, $000000], [0, 0.4, 1])</code>

        This will gradually change the color from black to white during the
        first 40% of the lifetime, and then fade out to black again until the
        particle dies. }
    procedure SetColors(const Colors: array of Cardinal;
      const Ages: array of Single);

    { Summary:
        Sets the alpha values of the particle to use during its lifetime.
      Parameters:
        Alphas: an array of alpha values to use (0=fully transparent,
          255=fully opaque).
        Ages: the ages for each alpha value, where 0.0 is when the particle
          starts, and 1.0 is where the particle dies.
      Remarks:
        See <link SetColors> for more information on the Ages array. }
    procedure SetAlphas(const Alphas: array of Byte;
      const Ages: array of Single);

    { Summary:
        Sets the <b>additional</b> sizes of the particle to use during its
        lifetime.
      Parameters:
        Sizes: an array of (Width, Height)-pairs.
        Ages: the ages for each pair, where 0.0 is when the particle
          starts, and 1.0 is where the particle dies.
        AgesHeight: (optional) you can specify separate ages for particle height
          if you want. If set, then Ages applies to the width, and AgesHeight
          applies to the height of the particle. If not set, then the Ages array
          applies to both width and height.
      Remarks:
        The number of elements in the Sizes array must be even. Each
        width/height pair in the array represents an additional size, which is
        added to the base size of the particle (see <link ParticleWidth> and
        <link ParticleHeight>. See <link SetColors> for more information on the
        Ages array. }
    procedure SetSizes(const Sizes, Ages: array of Single;
      const AgesHeight: TSingleArray = nil);

    { Summary:
        Sets the velocity factors of the particle to use during its lifetime.
      Parameters:
        Velocities: an array of velocity factors, where 0.0 is zero velocity
          and 1.0 is equal to the <link Velocity> property.
        Ages: the ages for each velocity factor, where 0.0 is when the particle
          starts, and 1.0 is where the particle dies.
      Remarks:
        Use this method to change the velocity of the particles over time.
        See <link SetColors> for more information on the Ages
        array. }
    procedure SetVelocities(const Velocities, Ages: array of Single);

    { Summary:
        Sets the angular velocity factors of the particle to use during its
        lifetime.
      Parameters:
        Velocities: an array of velocity factors, where 0.0 is zero velocity
          and 1.0 is equal to the <link AngularVelocity> property.
        Ages: the ages for each velocity factor, where 0.0 is when the particle
          starts, and 1.0 is where the particle dies.
      Remarks:
        Use this method to change the velocity of the particles over time.
        See <link SetColors> for more information on the Ages
        array. See <link AngularVelocity> for more information about angular
        velocities. }
    procedure SetAngularVelocities(const Velocities, Ages: array of Single);

    { Summary:
        Sets the spin angle factors of the particle to use during its
        lifetime.
      Parameters:
        Factors: an array of spin angle factors, where 0.0 is zero angle
          and 1.0 is equal to the <link SpinAngle> property.
        Ages: the ages for each spin angle factor, where 0.0 is when the
          particle starts, and 1.0 is where the particle dies.
      Remarks:
        Use this method to change the spin angle of the particles over time.
        See <link SetColors> for more information on the Ages
        array. See <link SpinAngle> for more information about spin angles. }
    procedure SetSpinAngles(const Factors, Ages: array of Single);

    { Summary:
        Layer order (or Z-Order) of the emitter.
      Remarks:
        Is only used when this emitter is used with a <link TMZParticleEngine>.
        In that case, the particle engine will draw emitters with a lower
        Z-Order value before emitters with a higher Z-Order value. }
    property ZOrder: Integer read FEmitter.Params.Layer write FEmitter.Params.Layer;

    { Summary:
        Lifetime of the emitter in milliseconds.
      Remark:
        The emitter will die when its lifetime passes, unless <link Loop> is
        set to True, in which case it starts over again.}
    property EmitterLifetime: Integer read FEmitter.Params.LifeTime write FEmitter.Params.LifeTime;

    { Summary:
        Whether the emitter loops (True) or dies when all particles
        have finished (False, Default) }
    property Loop: Boolean read FEmitter.Params.Loop write FEmitter.Params.Loop default False;

    { Summary:
        The number of new particles that are created every second. }
    property EmissionRate: Integer read FEmitter.Params.Emission write FEmitter.Params.Emission;

    { Summary:
        The blend mode used for drawing the particles.
      Remarks:
        Defaults to bmNormal. }
    property BlendMode: TMZBlendMode read GetBlendMode write SetBlendMode default bmNormal;

    { Summary:
        The color mode used for drawing the particles.
      Remarks:
        Defaults to cmMix. }
    property ColorMode: TMZColorMode read GetColorMode write SetColorMode default cmMix;

    { Summary:
        Lifetime of emitted particles. }
    property ParticleLifetime: Integer read FEmitter.ParParams.LifeTimeS write FEmitter.ParParams.LifeTimeS;

    { Summary:
        The variation in lifetime for newly emitted particles in milliseconds. }
    property ParticleLifetimeVariation: Integer read FEmitter.ParParams.LifeTimeV write FEmitter.ParParams.LifeTimeV;

    { Summary:
        The number of the frame in the texture containing the particle image
        at the start of the particles lifetime. Frame numbers are 1-based.
      Remarks:
        A texture can contain multiple particle images (if
        <link TMZTexture.SetFrameSize> is used). During the lifetime of the
        particle, the frame number used will range from StartFrameNumber to
        EndFrameNumber. To keep the same frame during the lifetime, set
        StartFrameNumber and EndFrameNumber to the same value.}
    property StartFrameNumber: Integer read GetStartFrameNumber write SetStartFrameNumber;

    { Summary:
        The number of the frame in the texture containing the particle image
        at the end of the particles lifetime. Frame numbers are 1-based.
      Remarks:
        A texture can contain multiple particle images (if
        <link TMZTexture.SetFrameSize> is used). During the lifetime of the
        particle, the frame number used will range from StartFrameNumber to
        EndFrameNumber. To keep the same frame during the lifetime, set
        StartFrameNumber and EndFrameNumber to the same value.}
    property EndFrameNumber: Integer read GetEndFrameNumber write SetEndFrameNumber;

    { Summary:
        Initial width of the particle.
      Remarks:
        See also <link SetSizes> and <link WidthVariation> }
    property ParticleWidth: Single read FEmitter.ParParams.SizeXS write FEmitter.ParParams.SizeXS;

    { Summary:
        Initial height of the particle.
      Remarks:
        When <link SizeLocked> is set to True, this height will be ignored, and
        the height will always be equal to the <link ParticleWidth>.
        See also <link SetSizes> and <link HeightVariation> }
    property ParticleHeight: Single read FEmitter.ParParams.SizeYS write FEmitter.ParParams.SizeYS;

    { Summary:
        Variation in particle width. }
    property WidthVariation: Single read FEmitter.ParParams.SizeXV write FEmitter.ParParams.SizeXV;

    { Summary:
        Variation in particle height. }
    property HeightVariation: Single read FEmitter.ParParams.SizeYV write FEmitter.ParParams.SizeYV;

    { Summary:
        Set to True to lock width and height. When locked, the
        <link ParticleHeight> of the particle will always be set equal to the
        <link ParticleWidth>. }
    property SizeLocked: Boolean read FEmitter.ParParams.SizeXYBind write FEmitter.ParParams.SizeXYBind;

    { Summary:
        Initial rotation angle of each particle in degrees.
      Remarks:
        See also <link SpinAngle> and <link RotationAngleVariation> }
    property RotationAngle: Single read FEmitter.ParParams.AngleS write FEmitter.ParParams.AngleS;

    { Summary:
        Variation in rotation angle in degrees. }
    property RotationAngleVariation: Single read FEmitter.ParParams.AngleV write FEmitter.ParParams.AngleV;

    { Summary:
        The number of degrees the particle spins each second.
      Remarks:
        See also <link SetSpinAngles> and <link SpinAngleVariation>. }
    property SpinAngle: Single read GetSpinAngle write SetSpinAngle;

    { Summary:
        The variation in <link SpinAngle> angle in degrees. }
    property SpinAngleVariation: Single read GetSpinAngleVariation write SetSpinAngleVariation;

    { Summary:
        Initial velocity of each particle in pixels per second.
      Remarks:
        See also <link SetVelocities> and <link VelocityVariation> }
    property Velocity: Single read FEmitter.ParParams.VelocityS write FEmitter.ParParams.VelocityS;

    { Summary:
        Variation in initial velocity in pixels per second. }
    property VelocityVariation: Single read FEmitter.ParParams.VelocityV write FEmitter.ParParams.VelocityV;

    { Summary:
        Velocity at which the direction angle of the particle changes, in
        degrees per second.
      Remarks:
        The initial direction angle is given in one of the SetXxxEmitter methods.
        This angular velocity changes that direction angle over time. See
        also <link SetAngularVelocities> and <link AngularVelocityVariation>. }
    property AngularVelocity: Single read GetAngularVelocity write SetAngularVelocity;

    { Summary:
        Variation in <link AngularVelocity> in degrees per second. }
    property AngularVelocityVariation: Single read GetAngularVelocityVariation write SetAngularVelocityVariation;

    { Summary:
        Internal handle used for the emitter.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property Handle: zglPEmitter2D read FHandle;
  end;

type
  { Summary:
      Manages a collection of <link TMZParticleEmitter>s.
    Remarks:
      Use of this particle engine is optional. You can also opt to manage the
      particle emitters yourself. }
  TMZParticleEngine = class
  {$REGION 'Internal Declarations'}
  private
    FEmitters: array of TMZParticleEmitter;
    FEmitterCount: Integer;
    FParticleCount: Integer;
    FModified: Boolean;
    function GetEmitter(const Index: Integer): TMZParticleEmitter;
  private
    procedure DeleteEmitter(const Index: Integer);
    procedure SortByLayer(const ALo, AHi: Integer);
    procedure SortByID(const ALo, AHi: Integer);
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a new particle engine. }
    constructor Create;
    destructor Destroy; override;

    { Summary:
        Adds an emitter to the engine.
      Parameters:
        Emitter: the emitter to add.
      Remarks:
        The particle engine becomes owner of the emitter, so do not free it
        you self. Also, if the emitter is non-looping, it will be removed
        (and freed) when all its particles have died. }
    procedure AddEmitter(const Emitter: TMZParticleEmitter);

    { Summary:
        Adds a copy of the emitter to the engine.
      Parameters:
        Source: the emitter to copy and add.
        XOffset: (optional) X-displacement of the emitter.
        YOffset: (optional) Y-displacement of the emitter.
      Returns:
        Returns a copy of the emitter. You can modify this copy independently.
      Remarks:
        The original emitter will <b>not</b> be modified.
        The particle engine becomes owner of the copy, so do not free it
        yourself. }
    function AddEmitterCopy(const Source: TMZParticleEmitter;
      const XOffset: Single = 0; const YOffset: Single = 0): TMZParticleEmitter;

    { Summary:
        Removes (and frees) the given emitter from the engine.
      Parameters:
        Emitter: the emitter to remove (and free) }
    procedure RemoveEmitter(const Emitter: TMZParticleEmitter);

    { Summary:
        Clears (and Frees) all emitters. }
    procedure Clear;

    { Summary:
        Draws all emitters in Z-Order. Emitters with a lower Z-Order will be
        drawn before emitters with a higher Z-Order.
      Remarks:
        You usually call this method in the main render loop (like in
        <link TMZScene.RenderFrame>. }
    procedure Render;

    { Summary:
        Is called during each iteration of the main loop to update all the
        emitters managed by this engine.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update.
      Remarks:
        You usually call this from the <link TMZScene.Update> method. }
    procedure Update(const DeltaTimeMs: Double);

    { Summary:
        The number of emitters managed by the particle engine.
      Remarks:
        As (non-looping) emitters die, they will be automatically removed from
        this list. So the value of this property will change over time. }
    property EmitterCount: Integer read FEmitterCount;

    { Summary:
        The emitteres currently managed by the particle engine.
      Remarks:
        As (non-looping) emitters die, they will be automatically removed from
        this list. }
    property Emitters[const Index: Integer]: TMZParticleEmitter read GetEmitter; default;

    { Summary:
        Total number of particles (from all emitters) that are currently alive. }
    property ParticleCount: Integer read FParticleCount;
  end;
{$ENDREGION 'Particle Engine'}

{$REGION 'Keyboard'}
{$IFDEF FPC}
  {$NOTES OFF}
{$ENDIF}
type
  { Summary:
      Key codes for use with <link TMZKeyboard> }
  TMZKeyCode = (
    kcSysRq      = $B7,
    kcPause      = $C5,
    kcEscape     = $01,
    kcEnter      = $1C,
    kcKPEnter    = $9C,

    kcUp         = $C8,
    kcDown       = $D0,
    kcLeft       = $CB,
    kcRight      = $CD,

    kcBackspace  = $0E,
    kcSpace      = $39,
    kcTab        = $0F,
    kcTilde      = $29,

    kcInsert     = $D2,
    kcDelete     = $D3,
    kcHome       = $C7,
    kcEnd        = $CF,
    kcPageUp     = $C9,
    kcPageDown   = $D1,

    kcCtrl       = $FF - $01,
    kcCtrlLeft   = $1D,
    kcCtrlRight  = $9D,
    kcAlt        = $FF - $02,
    kcAltLeft    = $38,
    kcAltRight   = $B8,
    kcShift      = $FF - $03,
    kcShiftLeft  = $2A,
    kcShiftRight = $36,
    kcSuper      = $FF - $04,
    kcSuperLeft  = $DB,
    kcSuperRight = $DC,
    kcAppMenu    = $DD,

    kcCapsLock   = $3A,
    kcNumLock    = $45,
    kcScrollLock = $46,

    kcLeftBracket  = $1A,
    kcRightBracket = $1B,
    kcBackslash  = $2B,
    kcSlash      = $35,
    kcComma      = $33,
    kcDecimal    = $34,
    kcSemiColon  = $27,
    kcApostrophe = $28,

    kc0          = $0B,
    kc1          = $02,
    kc2          = $03,
    kc3          = $04,
    kc4          = $05,
    kc5          = $06,
    kc6          = $07,
    kc7          = $08,
    kc8          = $09,
    kc9          = $0A,

    kcMinus      = $0C,
    kcEquals     = $0D,

    kcA          = $1E,
    kcB          = $30,
    kcC          = $2E,
    kcD          = $20,
    kcE          = $12,
    kcF          = $21,
    kcG          = $22,
    kcH          = $23,
    kcI          = $17,
    kcJ          = $24,
    kcK          = $25,
    kcL          = $26,
    kcM          = $32,
    kcN          = $31,
    kcO          = $18,
    kcP          = $19,
    kcQ          = $10,
    kcR          = $13,
    kcS          = $1F,
    kcT          = $14,
    kcU          = $16,
    kcV          = $2F,
    kcW          = $11,
    kcX          = $2D,
    kcY          = $15,
    kcZ          = $2C,

    kcKP0        = $52,
    kcKP1        = $4F,
    kcKP2        = $50,
    kcKP3        = $51,
    kcKP4        = $4B,
    kcKP5        = $4C,
    kcKP6        = $4D,
    kcKP7        = $47,
    kcKP8        = $48,
    kcKP9        = $49,

    kcKPSub      = $4A,
    kcKPAdd      = $4E,
    kcKPMul      = $37,
    kcKPDiv      = $B5,
    kcKPDecimal  = $53,

    kcF1         = $3B,
    kcF2         = $3C,
    kcF3         = $3D,
    kcF4         = $3E,
    kcF5         = $3F,
    kcF6         = $40,
    kcF7         = $41,
    kcF8         = $42,
    kcF9         = $43,
    kcF10        = $44,
    kcF11        = $57,
    kcF12        = $58);
{$IFDEF FPC}
  {$NOTES ON}
{$ENDIF}

type
  { Summary:
      Key state as used by <link TMZKeyboard.LastKeyState> }
  TMZKeyState = (ksDown, ksUp);

type
  { Summary:
      Static class for working with keyboard input }
  TMZKeyboard = class
  public
    { Summary:
        Clears the keyboard state.
      Remarks:
        You must call this after processing keyboard input. }
    class procedure ClearState; static; inline;

    { Summary:
        Returns whether the given key is currently pressed (down).
      Parameters:
        KeyCode: the key to check.
      Returns:
        True if the given key is currently pressed (down).
      Remarks:
        See also <link IsKeyPressed> }
    class function IsKeyDown(const KeyCode: TMZKeyCode): Boolean; static; inline;

    { Summary:
        Returns whether the given key is currently unpressed (up) since the
        last call of <link ClearState>.
      Parameters:
        KeyCode: the key to check.
      Returns:
        True if the given key is currently unpressed (up). }
    class function IsKeyUp(const KeyCode: TMZKeyCode): Boolean; static; inline;

    { Summary:
        Returns whether the given key has been pressed since the last call of
        <link ClearState>.
      Parameters:
        KeyCode: the key to check.
      Returns:
        True if the given key was pressed.
      Remarks:
        See also <link IsKeyDown> }
    class function IsKeyPressed(const KeyCode: TMZKeyCode): Boolean; static; inline;

    { Summary:
        Returns the last state of the given key.
      Parameters:
        KeyCode: the key to check.
      Returns:
        The last state (up or down) of the key. }
    class function LastKeyState(const KeyCode: TMZKeyCode): TMZKeyState; static; inline;

    { Summary:
        Starts tracking user text input.
      Parameters:
        InitialText: (optional) the initial text.
        MaxLength: (optional) the maximum number of symbols the user can enter
          (or -1 if there is no such restriction).
      Remarks:
        See also <link EndReadText> and <link GetText> }
    class procedure BeginReadText(const InitialText: UTF8String = '';
      const MaxLength: Integer = -1); static; inline;

    { Summary:
        Returns the text entered so far since <link BeginReadText>
      Returns:
        The current entered text.
      Remarks:
        See also <link BeginReadText> and <link EndReadText> }
    class function GetText: UTF8String; static; inline;

    { Summary:
        Finished tracking user text input.
      Remarks:
        This will clear the current text (that is, calling <link GetText> after
        this call will return an empty string.
        See also <link BeginReadText> and <link GetText> }
    class procedure EndReadText; static; inline;
  end;
{$ENDREGION 'Keyboard'}

{$REGION 'Mouse'}
type
  { Summary:
      Mouse buttons as used by the <link TMZMouse> class. }
  TMZMouseButton = (mbLeft, mbMiddle, mbRight);

type
  { Summary:
      Static class for working with mouse input }
  TMZMouse = class
  {$REGION 'Internal Declarations'}
  private
    class function GetPosition: TMZPoint; static; inline;
    class function GetX: Integer; static; inline;
    class function GetY: Integer; static; inline;
    class function GetDX: Integer; static; inline;
    class function GetDY: Integer; static; inline;
    class procedure SetPosition(Value: TMZPoint); static; inline;
    class procedure SetX(const Value: Integer); static; inline;
    class procedure SetY(const Value: Integer); static; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Clears the mouse state.
      Remarks:
        You must call this after processing mouse input. }
    class procedure ClearState; static; inline;

    { Summary:
        Whether the given mouse button is currently down (pressed).
      Parameters:
        Button: the mouse button to check.
      Returns:
        True if the given button is currently down (pressed).
      Remarks:
        See also <link IsButtonClicked> }
    class function IsButtonDown(const Button: TMZMouseButton): Boolean; static; inline;

    { Summary:
        Whether the given mouse button was up (unpressed) since the last call of
        <link ClearState>
      Parameters:
        Button: the mouse button to check.
      Returns:
        True if the given button is currently up (unpressed). }
    class function IsButtonUp(const Button: TMZMouseButton): Boolean; static; inline;

    { Summary:
        Whether the given mouse button was clicked since the last call of
        <link ClearState>
      Parameters:
        Button: the mouse button to check.
      Returns:
        True if the given button is clicked.
      Remarks:
        See also <link IsButtonDown> }
    class function IsButtonClicked(const Button: TMZMouseButton): Boolean; static; inline;

    { Summary:
        Whether the given mouse button was double-clicked since the last call of
        <link ClearState>
      Parameters:
        Button: the mouse button to check.
      Returns:
        True if the given button is double-clicked. }
    class function IsButtonDoubleClicked(const Button: TMZMouseButton): Boolean; static; inline;

    { Summary:
        Whether the mouse wheel has scrolled in the up direction since the last
        call of <link ClearState>.
      Returns:
        True if the mouse wheel has scrolled up. }
    class function HasWheelScrolledUp: Boolean; static; inline;

    { Summary:
        Whether the mouse wheel has scrolled in the down direction since the last
        call of <link ClearState>.
      Returns:
        True if the mouse wheel has scrolled down. }
    class function HasWheelScrolledDown: Boolean; static; inline;

    { Summary:
        Returns the mouse coordinates relative to the top-left corner of
        the window.
      Parameters:
        X: Receives the X-coordinate.
        Y: Receives the Y-coordinate. }
    class procedure GetPos(out X, Y: Integer); static; inline;

    { Summary:
        Sets the mouse coordinates relative to the top-left corner of
        the window.
      Parameters:
        X: the X-coordinate (-1 to center to the window).
        Y: the Y-coordinate (-1 to center to the window). }
    class procedure SetPos(const X, Y: Integer); static; inline;

    { Summary:
        Centers the mouse cursor to the window. }
    class procedure Center; static; inline;

    { Summary:
        X-coordinate of mouse cursor relative to top-left corner of window }
    class property X: Integer read GetX write SetX;

    { Summary:
        Y-coordinate of mouse cursor relative to top-left corner of window }
    class property Y: Integer read GetY write SetY;

    { Summary:
        Coordinates of mouse cursor relative to top-left corner of window }
    class property Position: TMZPoint read GetPosition write SetPosition;

    { Summary:
        Delta X-coordinate of mouse cursor relative to top-left corner of window }
    class property DX: Integer read GetDX;

    { Summary:
        Delta Y-coordinate of mouse cursor relative to top-left corner of window }
    class property DY: Integer read GetDY;
  end;
{$ENDREGION 'Mouse'}

{$REGION 'Touch'}
{$IF DEFINED(iOS) or DEFINED(ANDROID)} // iOS and Android only support static builds
type
  { Summary:
      Static class for working with touch input on mobile devices. }
  TMZTouch = class
  {$REGION 'Internal Declarations'}
  private
    class function GetPosition(const Finger: Byte): TMZPoint; static; inline;
    class function GetX(const Finger: Byte): Integer; static; inline;
    class function GetY(const Finger: Byte): Integer; static; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Clears the touch state.
      Remarks:
        You must call this after processing touch input. }
    class procedure ClearState; static; inline;

    { Summary:
        Whether the given finger is currently down (pressed).
      Parameters:
        Finger: the index of the finger to check (0-4).
      Returns:
        True if the given finger is currently down (pressed).
      Remarks:
        See also <link IsFingerTapped> }
    class function IsFingerDown(const Finger: Byte): Boolean; static; inline;

    { Summary:
        Whether the given finger was up (unpressed) since the last call of
        <link ClearState>.
      Parameters:
        Finger: the index of the finger to check (0-4).
      Returns:
        True if the given finger is currently up (unpressed). }
    class function IsFingerUp(const Finger: Byte): Boolean; static; inline;

    { Summary:
        Whether the given finger was tapped since the last call of
        <link ClearState>
      Parameters:
        Finger: the index of the finger to check (0-4).
      Returns:
        True if the given finger is tapped.
      Remarks:
        See also <link IsFingerDown> }
    class function IsFingerTapped(const Finger: Byte): Boolean; static; inline;

    { Summary:
        Whether the given finger was double-tapped since the last call of
        <link ClearState>
      Parameters:
        Finger: the index of the finger to check (0-4).
      Returns:
        True if the given finger is double-tapped. }
    class function IsFingerDoubleTapped(const Finger: Byte): Boolean; static; inline;

    { Summary:
        Returns the coordinates of the given finger relative to the top-left
        corner of the window.
      Parameters:
        Finger: index of the finger (0-4).
        X: Receives the X-coordinate.
        Y: Receives the Y-coordinate. }
    class procedure GetPos(const Finger: Byte; out X, Y: Integer); static; inline;

    { Summary:
        X-coordinate of the given finger relative to top-left corner of window.
      Parameters:
        Finger: index of the finger (0-4). }
    class property X[const Finger: Byte]: Integer read GetX;

    { Summary:
        Y-coordinate of the given finger relative to top-left corner of window.
      Parameters:
        Finger: index of the finger (0-4). }
    class property Y[const Finger: Byte]: Integer read GetY;

    { Summary:
        Coordinates of the given finger relative to top-left corner of window.
      Parameters:
        Finger: index of the finger (0-4). }
    class property Position[const Finger: Byte]: TMZPoint read GetPosition;
  end;
{$IFEND}
{$ENDREGION 'Touch'}

{$REGION 'Joystick'}
type
  { Summary:
      Joystick capabilities as uses by <link TMZJoystick.Caps> }
  TMZJoystickCap = (jcHasZ, jcHasR, jcHasU, jcHasV, jcHasPov, jcPov4Dir,
    jcPovCts);
  TMZJoystickCaps = set of TMZJoystickCap;

type
  { Summary:
      Joystick axis as used by <link TMZJoystick.AxisPos> }
  TMZJoystickAxis = (jaX, jaY, jaZ, jaR, jaU, jaV, jaPovX, jaPovY);

type
  { Summary:
      Represents a joystick attached to the system }
  TMZJoystick = class
  {$REGION 'Internal Declarations'}
  private
    class var FInitialized: Boolean;
    class var FJoysticks: array of TMZJoystick;
    class var FNullJoystick: TMZJoystick;
    class procedure Initialize; static;
    class function GetJoystickCount: Integer; static;
    class function GetJoystick(const Index: Integer): TMZJoystick; static;
    class function GetNullJoystick: TMZJoystick; static;
  private
    FID: Byte;
    FHandle: zglPJoyInfo;
    function GetAxisCount: Integer;
    function GetAxisPos(const Axis: TMZJoystickAxis): Single;
    function GetButtonCount: Integer;
    function GetCaps: TMZJoystickCaps;
    function GetName: UTF8String;
  public
    constructor Create(const ID: Byte; const Handle: zglPJoyInfo);
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Whether the given button is currently down (pressed).
      Parameters:
        Button: the button index to check.
      Returns:
        True if the given button is currently down (pressed).
      Remarks:
        See also <link IsButtonPressed> }
    function IsButtonDown(const Index: Integer): Boolean; inline;

    { Summary:
        Whether the given button was up (unpressed) since the last call of
        <link ClearState>
      Parameters:
        Button: the button index to check.
      Returns:
        True if the given button is up. }
    function IsButtonUp(const Index: Integer): Boolean; inline;

    { Summary:
        Whether the given button was pressed since the last call of
        <link ClearState>
      Parameters:
        Button: the button index to check.
      Returns:
        True if the given button is pressed.
      Remarks:
        See also <link IsButtonDown> }
    function IsButtonPressed(const Index: Integer): Boolean; inline;

    { Summary:
        Clear the state of <b>all</b> joysticks.
      Remarks:
        Must be called after processing joystick input. }
    class procedure ClearState; static; inline;

    { Summary:
        The number of joysticks attached to the system. }
    class property JoystickCount: Integer read GetJoystickCount;

    { Summary:
        The joysticks attached to the system.
      Remarks:
        Returns nil if an invalid index is given. }
    class property Joysticks[const Index: Integer]: TMZJoystick read GetJoystick;

    { Summary:
        Returns a "null" (dummy) joystick. This is a joystick that is not
        connected to the system, and just returns 0 (or False) for all
        properties and functions. This joystick can be useful if the system
        does not actually have a joystick (that is, <link JoystickCount> returns
        0), but you need access to a TMZJoystick object anyway. }
    class property NullJoystick: TMZJoystick read GetNullJoystick;

    { Summary:
        The name of the joystick. }
    property Name: UTF8String read GetName;

    { Summary:
        The number of axes of the joystick. }
    property AxisCount: Integer read GetAxisCount;

    { Summary:
        The number of buttons of the joystick. }
    property ButtonCount: Integer read GetButtonCount;

    { Summary:
        The joystick capabilities }
    property Caps: TMZJoystickCaps read GetCaps;

    { Summary:
        Returns the current position of the given axis.
      Parameters:
        Axis: the axis to get the position for.
      Returns:
        The position of the given axis in the range -1.0 .. 1.0 }
    property AxisPos[const Axis: TMZJoystickAxis]: Single read GetAxisPos;

    { Summary:
        Internal handle used for the joystick.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property Handle: zglPJoyInfo read FHandle;
  end;
{$ENDREGION 'Joystick'}

{$REGION 'Application and Scenes'}
type
  { Summary:
      Application options as used with <link TMZApplication.Options>. }
  TMZApplicationOption = (
    { Summary:
        Enables logging of certain ZenGL events to a 'log.txt' file. When the
        MEMCHECK conditional define is set, memory information (and memory
        leaks) will also be logged to 'memlog.txt'. }
    aoEnableLogging,

    { Summary:
        To run the application in full-screen mode. }
    aoFullScreen,

    { Summary:
        Wwhen the application is <b>not</b> run full-screen, this option will
        center the window to the computer screen. }
    aoCenterToScreen,

    { Summary:
        Enables VSync with the monitor refresh rate. }
    aoVSync,

    { Summary:
        Enables aspect ratio correction when the size of the window does not
        match the size of the surfaces. }
    aoAspectRatioCorrection,

    { Summary:
        Whether the mouse cursor should be visible. }
    aoShowCursor,

    { Summary:
        Whether the sound subsystem is used. }
    aoUseSound,

    { Summary:
        Whether input events are used. When set, the system will forward
        mouse and keyboard events to the application and current scene.
        Otherwize, you need to poll input using the <link TMZMouse> and
        <link TMZKeyboard> classes. }
    aoUseInputEvents,

    { Summary:
        For mobile devices, whether portrait orientations are allowed.
      Remarks:
        Current only works for iOS. }
    aoAllowPortraitOrientation,

    { Summary:
        For mobile devices, whether landscape orientations are allowed.
      Remarks:
        Current only works for iOS. }
    aoAllowLandscapeOrientation,

    { Summary:
        For mobile devices, whether background music is allowed.
      Remarks:
        Current only works for iOS. }
    aoAllowBackgroundMusic);
  TMZApplicationOptions = set of TMZApplicationOption;

const
  { Summary:
      Default application options for the <link TMZApplication.Options> property. }
  DEFAULT_APPLICATION_OPTIONS = [aoEnableLogging, aoCenterToScreen,
    aoAllowPortraitOrientation, aoAllowLandscapeOrientation,
    aoAllowBackgroundMusic];

type
  { Summary:
      Device orientation }
  TMZInterfaceOrientation = (
    ioUnknown = 0,
    ioPortrait = 1,
    ioPortraitUpsideDown = 2,
    ioLandscapeRight = 3,
    ioLandscapeLeft = 4);

type
  TMZScene = class;
  TMZSceneClass = class of TMZScene;

  { Summary:
      Main MondoZenGL application class. Contains application-wide settings
      and utilities.
    Remarks:
      You always need to create one single TMZApplication instance in your
      application. You can also derive your own application class from
      TMZApplication if you wish. A typical bare-bone application will look
      like this:

        <code>
        var
          App: TMZApplication;
        begin
          App := TMZApplication.Create;
          App.ScreenWidth := 640; // ...etc. set application options
          App.SetScene(TMyScene);
        end.
        </code>

      where TMyScene is your TMZScene descendant class that will contain your
      initial startup scene.

      Note that you do <b>not</b> free the App object. That is done
      automatically when the application shuts down. }
  TMZApplication = class
  {$REGION 'Internal Declarations'}
  private
    class var FInstance: TMZApplication;
    class var FCanvas: TMZCanvas;
  private
    FCaption: UTF8String;
    FFullScreenAntiAliasingSamples: Integer;
    FOptions: TMZApplicationOptions;
    FScreenHeight: Integer;
    FScreenWidth: Integer;
    FScreenRefreshRate: Integer;
    FStencilBufferBits: Integer;
    FCurrentScene: TMZScene;
    FNewScene: TMZScene;
    FSubsystemsInitialized: Boolean;
    procedure SetCaption(const Value: UTF8String);
    class function GetCurrentRenderFrameRate: Integer; inline; static;
    class function GetCurrentVideoRamUsage: Integer; inline; static;
    class function GetWindowBounds: TMZRect; inline; static;
    class function GetWindowHandle: THandle; inline; static;
    class function GetViewportBounds: TMZRect; inline; static;
    class function GetOpenGLContext: THandle; inline; static;
    class procedure SetWindowBounds(const Value: TMZRect); inline; static;
  private
    procedure Initialize;
    procedure SetOptions(const Value: TMZApplicationOptions);
  public
    class constructor ClassCreate;
  {$ENDREGION 'Internal Declarations'}
  protected
    { Summary:
        Performs application startup. This method is called <b>after</b> the
        window and OpenGL contexts have been created. You can override this
        method to initialize the application and load application resources. }
    procedure Startup; virtual;

    { Summary:
        Performs application cleanup. This method is called <b>before</b> the
        window and OpenGL contexts will be destroyed. You can override this
        method to cleanup your application resources. }
    procedure Shutdown; virtual;

    { Summary:
        Is called during each iteration of the main loop to render the current
        frame. By default, this method is passed on to the current scene, but
        you can override this behavior. }
    procedure RenderFrame; virtual;

    { Summary:
        Is called during each iteration of the main loop to update the game
        state. The DeltaTimeMs is the number of milliseconds (1/1000th of a
        second) that has passed since the last call to Update.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update. }
    procedure Update(const DeltaTimeMs: Double); virtual;

    { Summary:
        Is called when the application becomes active (gets focus). This will
        resume the application loop and forward this method to the current
        scene. }
    procedure Activate; virtual;

    { Summary:
        Is called when the application deactivates (loses focus). This will
        pause the application loop and forward this method to the current
        scene. }
    procedure Deactivate; virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time the mouse moves.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Parameters:
        X: The mouse X-coordinate.
        Y: The mouse Y-coordinate. }
    procedure MouseMove(const X, Y: Integer); virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a mouse button is pressed.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Parameters:
        Button: The mouse button that is pressed. }
    procedure MouseDown(const Button: TMZMouseButton); virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a mouse button is released.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Parameters:
        Button: The mouse button that is release. }
    procedure MouseUp(const Button: TMZMouseButton); virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time the mouse wheel is rotated.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Parameters:
        Up: True of wheel is rotated up, False if wheel is rotated down. }
    procedure MouseWheel(const Up: Boolean); virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a keyboard key is pressed.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Parameters:
        KeyCode: the key that is pressed. }
    procedure KeyDown(const KeyCode: TMZKeyCode); virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a keyboard key is released.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Parameters:
        KeyCode: the key that is released. }
    procedure KeyUp(const KeyCode: TMZKeyCode); virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a keyboard key is entered.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Parameters:
        Symbol: the key that is entered.
      Remarks:
        This event is only fired if the keyboard is set to reading text
        (using <link TMZKeyboard.BeginReadText>) }
    procedure KeyChar(const Symbol: String); virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a finger moves over the touch screen.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Parameters:
        Finger: The finger ID.
        X: The touch X-coordinate.
        Y: The touch Y-coordinate.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure TouchMove(const Finger, X, Y: Integer); virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a finger is pressed onto the touch screen.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Parameters:
        Finger: The finger ID.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure TouchDown(const Finger: Integer); virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a finger is released from the touch screen.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Parameters:
        Finger: The finger ID.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure TouchUp(const Finger: Integer); virtual;

    { Summary:
        Is fired when a "Low Memory Warning" is received from the device.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure SysLowMemoryWarning; virtual;

    { Summary:
        Is fired when a device changes orientation.
        By default, this method is passed on to the current scene, but you can
        override this behavior.
      Parameters:
        Orientation: The new orientation.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure SysChangeOrientation(const Orientation: TMZInterfaceOrientation); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    { Summary:
        Set the current scene to display. If the previously active scene has
        AutoFree set to True, then the previous scene will be freed.
      Parameters:
        Scene: the scene to display.
      Remarks:
        The first time you call this method, the application loop will be
        started. In that case, this method will not return until the application
        is terminated. So with this typical construct:

          <code>
          var
            App: TMZApplication;
          begin
            App := TMZApplication.Create;
            App.ScreenWidth := 640; // ...etc. set application options
            App.SetScene(TMyScene.Create);
          end.
          </code>

        the SetScene method will return only when the application terminates.
        So you should set any application options (like screen size etc.)
        before you set the first scene. }
    procedure SetScene(const Scene: TMZScene);

    { Summary:
        Quits the application. }
    procedure Quit;

    { Summary:
        The current application instance. }
    class property Instance: TMZApplication read FInstance;

    { Summary:
        The canvas used for all drawing operations.
      Remarks:
        The canvas is global and shared with all MondoZenGL objects. }
    class property Canvas: TMZCanvas read FCanvas;

    { Summary:
        The currently running scene. }
    property CurrentScene: TMZScene read FCurrentScene;

    { Summary:
        Various application options.
      Remarks:
        You should set these options <b>before</b> you call <link SetScene> the
        first time. }
    property Options: TMZApplicationOptions read FOptions write SetOptions default DEFAULT_APPLICATION_OPTIONS;

    { Summary:
        The caption of the application window. }
    property Caption: UTF8String read FCaption write SetCaption;

    { Summary:
        Width of the screen (in case aoFullScreen is set in <link Options>)
        or the application window.
      Remarks:
        You should set this option <b>before</b> you call <link SetScene> the
        first time. Defaults to 640 pixels }
    property ScreenWidth: Integer read FScreenWidth write FScreenWidth default 640;

    { Summary:
        Height of the screen (in case aoFullScreen is set in <link Options>)
        or the application window.
      Remarks:
        You should set this option <b>before</b> you call <link SetScene> the
        first time. Defaults to 480 pixels }
    property ScreenHeight: Integer read FScreenHeight write FScreenHeight default 480;

    { Summary:
        Desired refresh rate of the screen in Hz. You can also set this value to
        REFRESH_RATE_MAXIMUM (the default value) to use the maximum refresh rate
        supported by the screen. The value REFRESH_RATE_DEFAULT can be used to
        use the default refresh rate of the screen. Otherwise, specify a
        desired refresh rate in Hz.
      Remarks:
        You should set this option <b>before</b> you call <link SetScene> the
        first time. This value is just a suggestion and may not be the actual
        refresh rate. }
    property ScreenRefreshRate: Integer read FScreenRefreshRate write FScreenRefreshRate default REFRESH_RATE_MAXIMUM;

    { Summary:
        Number of samples to use for Full Screen Anti-Aliasing, or 0 (default)
        to turn off Anti-Aliasing.
      Remarks:
        You should set this property <b>before</b> you call <link SetScene> the
        first time. Full Screen Anti-Aliasing is not supported by all devices
        and may slow down performance and increase (GPU) memory usage. }
    property FullScreenAntiAliasingSamples: Integer read FFullScreenAntiAliasingSamples write FFullScreenAntiAliasingSamples default 0;

    { Summary:
        Number of bits to use for the stencil buffer. }
    property StencilBufferBits: Integer read FStencilBufferBits write FStencilBufferBits default 0;

    { Summary:
        Returns the current render framerate in Frames Per Second. }
    class property CurrentRenderFrameRate: Integer read GetCurrentRenderFrameRate;

    { Summary:
        Returns the current amount of Video RAM used. }
    class property CurrentVideoRamUsage: Integer read GetCurrentVideoRamUsage;

    { Summary:
        The current position and dimensions of the application window. }
    class property WindowBounds: TMZRect read GetWindowBounds write SetWindowBounds;

    { Summary:
        Returns the internal window handle.
      Remarks:
        Not supported on iOS (returns 0). }
    class property WindowHandle: THandle read GetWindowHandle;

    { Summary:
        The current viewport offset and dimensions. }
    class property ViewportBounds: TMZRect read GetViewportBounds;

    { Summary:
        Returns the current OpenGL context on systems that support it
        (or 0 otherwise) }
    class property OpenGLContext: THandle read GetOpenGLContext;
  end;

  { Summary:
      Abstract base class for scenes in your application. Your application
      should have at least 1 (main) scene that you run using the
      <link TMZApplication.SetScene> method. }
  TMZScene = class abstract
  {$REGION 'Internal Declarations'}
  private
    class var FCanvas: TMZCanvas;
  private
    FApplication: TMZApplication;
    FAutoFree: Boolean;
    FStartedUp: Boolean;
  public
    class constructor ClassCreate;
  {$ENDREGION 'Internal Declarations'}
  protected
    { Summary:
        Is called before the scene is executed. You can override this method
        to initialize scene specific resources. }
    procedure Startup; virtual;

    { Summary:
        Is called just before the scene is terminated. You can override this
        method to cleanup scene specific resources. }
    procedure Shutdown; virtual;

    { Summary:
        Is called during each iteration of the main loop to render the current
        frame. }
    procedure RenderFrame; virtual;

    { Summary:
        Is called during each iteration of the main loop to update the game
        state. The DeltaTimeMs is the number of milliseconds (1/1000th of a
        second) that has passed since the last call to Update.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update. }
    procedure Update(const DeltaTimeMs: Double); virtual;

    { Summary:
        Is called when the application becomes active (gets focus). You can
        override this method if you need to take special action on activation. }
    procedure Activate; virtual;

    { Summary:
        Is called when the application deactivates (loses focus). You can
        override this method if you need to take special action on
        deactivation. }
    procedure Deactivate; virtual;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time the mouse moves.
        Does nothing by default.
      Parameters:
        X: The mouse X-coordinate.
        Y: The mouse Y-coordinate. }
    procedure MouseMove(const X, Y: Integer); virtual;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a mouse button is pressed.
        Does nothing by default.
      Parameters:
        Button: The mouse button that is pressed. }
    procedure MouseDown(const Button: TMZMouseButton); virtual;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a mouse button is released.
        Does nothing by default.
      Parameters:
        Button: The mouse button that is release. }
    procedure MouseUp(const Button: TMZMouseButton); virtual;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time the mouse wheel is rotated.
        Does nothing by default.
      Parameters:
        Up: True of wheel is rotated up, False if wheel is rotated down. }
    procedure MouseWheel(const Up: Boolean); virtual;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a keyboard key is pressed.
        Does nothing by default.
      Parameters:
        KeyCode: the key that is pressed. }
    procedure KeyDown(const KeyCode: TMZKeyCode); virtual;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a keyboard key is released.
        Does nothing by default.
      Parameters:
        KeyCode: the key that is released. }
    procedure KeyUp(const KeyCode: TMZKeyCode); virtual;

    { Summary:
        If the aoUseInputEvents application option is set, then this method is
        called each time a keyboard key is entered.
        Does nothing by default.
      Parameters:
        Symbol: the key that is entered.
      This event is only fired if the keyboard is set to reading text
      (using <link TMZKeyboard.BeginReadText>) }
    procedure KeyChar(const Symbol: String); virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a finger moves over the touch screen.
        Does nothing by default.
      Parameters:
        Finger: The finger ID.
        X: The touch X-coordinate.
        Y: The touch Y-coordinate.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure TouchMove(const Finger, X, Y: Integer); virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a finger is pressed onto the touch screen.
        Does nothing by default.
      Parameters:
        Finger: The finger ID.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure TouchDown(const Finger: Integer); virtual;

    { Summary:
        If the aoUseInputEvents option is set, then this method is called
        each time a finger is released from the touch screen.
        Does nothing by default.
      Parameters:
        Finger: The finger ID.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure TouchUp(const Finger: Integer); virtual;

    { Summary:
        Is fired when a "Low Memory Warning" is received from the device.
        Does nothing by default.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure SysLowMemoryWarning; virtual;

    { Summary:
        Is fired when a device changes orientation.
        Does nothing by default.
      Parameters:
        Orientation: The new orientation.
      Remarks:
        Currently, this method is only used on iOS systems. }
    procedure SysChangeOrientation(const Orientation: TMZInterfaceOrientation); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    { Summary:
        The application that is running this scene. }
    property Application: TMZApplication read FApplication;

    { Summary:
        Whether the scene will automatically be freed when the application
        starts a new scene. Defaults to True.
      Remarks:
        When <link TMZApplication.SetScene> is called, the previously active
        scene will automatically be freed if this property is set to True.
        If this property is False (default), then you need to make sure yourself
        to free the scene at the appropriate time.
        To save memory, you should try to keep the amount of "live" scenes to
        a minimum. }
    property AutoFree: Boolean read FAutoFree write FAutoFree default True;

    { Summary:
        The canvas used for all drawing operations.
      Remarks:
        The canvas is global and shared with all MondoZenGL objects. }
    class property Canvas: TMZCanvas read FCanvas;
  end;
{$ENDREGION 'Application and Scenes'}

{$REGION 'Timers'}
type
  { Summary:
      A timer that fires at regular intervals. }
  TMZTimer = class
  {$REGION 'Internal Declarations'}
  private
    FHandle: zglPTimer;
    FOnTimer: TNotifyEvent;
    function GetActive: Boolean;
    function GetInterval: Integer;
    procedure SetActive(const Value: Boolean);
    procedure SetInterval(const Value: Integer);
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a new timer.
      Parameters:
        OnTimer: The event that is fired when the timer expires.
        IntervalMs: The timer interval in milliseconds. }
    constructor Create(const OnTimer: TNotifyEvent; const IntervalMs: Integer);
    destructor Destroy; override;

    { Summary:
        The event that is fired when the timer expires. }
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;

    { Summary:
        The timer interval in milliseconds. }
    property IntervalMs: Integer read GetInterval write SetInterval;

    { Summary:
        Whether the timer is active (default True) }
    property Active: Boolean read GetActive write SetActive;

    { Summary:
        Internal handle used for the timer.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property Handle: zglPTimer read FHandle;
  end;
{$ENDREGION 'Timers'}

{$REGION 'Math'}
type
  { Summary:
      Static class with math functions. }
  TMZMath = class
  public
    { Summary:
        Returns the cosine of an integer angle in degrees.
      Parameters:
        Angle: the angle in degrees.
      Returns:
        The cosine of the angle.
      Remarks:
        The angle does not have to be between 0 and 360 degrees, but this
        function will be faster if it is. }
    class function Cos(const Angle: Integer): Single; static; inline;

    { Summary:
        Returns the sine of an integer angle in degrees.
      Parameters:
        Angle: the angle in degrees.
      Returns:
        The sine of the angle.
      Remarks:
        The angle does not have to be between 0 and 360 degrees, but this
        function will be faster if it is. }
    class function Sin(const Angle: Integer): Single; static; inline;

    { Summary:
        Calculates the distance between two points.
      Parameters:
        X1: X-coordinate of the 1st point.
        Y1: Y-coordinate of the 1st point.
        X2: X-coordinate of the 2nd point.
        Y2: Y-coordinate of the 2nd point.
      Returns:
        The distance between the two points.
      Remarks:
        See also <link TMZPoint.DistanceTo>. }
    class function Distance(const X1, Y1, X2, Y2: Single): Single; static; inline;

    { Summary:
        Calculates the squared distance between two points.
      Parameters:
        X1: X-coordinate of the 1st point.
        Y1: Y-coordinate of the 1st point.
        X2: X-coordinate of the 2nd point.
        Y2: Y-coordinate of the 2nd point.
      Returns:
        The squared distance between the two points.
      Remarks:
        Calculating the squared distance is useful if you don't need the actual
        distance, but just want to compare distances. In that case you should
        use SquaredDistance since it is faster than <link Distance>.
        See also <link TMZPoint.SquaredDistanceTo>. }
    class function SquaredDistance(const X1, Y1, X2, Y2: Single): Single; static; inline;

    { Summary:
        Calculates the angle (in degrees) of two points.
      Parameters:
        X1: X-coordinate of the 1st point.
        Y1: Y-coordinate of the 1st point.
        X2: X-coordinate of the 2nd point.
        Y2: Y-coordinate of the 2nd point.
      Returns:
        The angle (in degrees) of the two points. }
    class function Angle(const X1, Y1, X2, Y2: Single): Single; static; inline;

    { Summary:
        Calculates the orientation of a point to a line.
      Parameters:
        X: X-coordinate of the point.
        Y: Y-coordinate of the point.
        X1: X-coordinate of the start point of the line.
        Y1: Y-coordinate of the start point of the line.
        X2: X-coordinate of the end point of the line.
        Y2: Y-coordinate of the end point of the line.
      Returns:
        Whether the point lies to the left, right or on the line. }
    function Orientation(const X, Y, X1, Y1, X2, Y2: Single): TMZOrientation; overload; inline;
  end;
{$ENDREGION 'Math'}

{$REGION 'Logging'}
type
  { Summary:
      Static class for logging. }
  TMZLog = class
  public
    { Summary:
        Logs a message.
      Parameters:
        Msg: the message to log.
        IncludeTiming: (optional) whether to include timing information with
          each message (default True).
      Remarks:
        Logging only works if the application option aoEnableLogging is set.
        Logging always adds some overhead (even a little when the
        aoEnableLogging option is <b>not</b> set). }
    class procedure Log(const Msg: UTF8String;
      const IncludeTiming: Boolean = True); overload; static; inline;

    { Summary:
        Logs a message.
      Parameters:
        Msg: the message to log with formatting placeholders (%s, %d etc.)
        Args: the arguments to pass for formatting.
        IncludeTiming: (optional) whether to include timing information with
          each message (default True).
      Remarks:
        Logging only works if the application option aoEnableLogging is set.
        Logging always adds some overhead (even a little when the
        aoEnableLogging option is <b>not</b> set). }
    class procedure Log(const Msg: UTF8String; const Args: array of const;
      const IncludeTiming: Boolean = True); overload; static;
  end;
{$ENDREGION 'Logging'}

{$REGION 'Utilities'}
type
  { Summary:
      A supported screen resolution as returned by
      <link TMZUtils.ScreenResolutions>. }
  TMZScreenResolution = record
    { Summary:
        Resolution width. }
    Width: Integer;

    { Summary:
        Resolution height. }
    Height: Integer;
  end;

  { Summary:
      Array of supported screen resolution as returned by
      <link TMZUtils.ScreenResolutions>. }
  TMZScreenResolutionArray = array of TMZScreenResolution;

type
  { Summary:
      Static class with various utilities. }
  TMZUtils = class
  {$REGION 'Internal Declarations'}
  private
    class function GetZenGLVersion: Integer; static; inline;
    class function GetZenGLVersionString: UTF8String; static; inline;
    class function GetZenGLVersionDate: UTF8String; static; inline;
    class function GetMondoZenGLVersion: Integer; static; inline;
    class function GetMondoZenGLVersionString: String; static; inline;
    class function GetMondoZenGLVersionDate: String; static; inline;
    class function GetApplicationDirectory: UTF8String; static; inline;
    class function GetHomeDirectory: UTF8String; static; inline;
    class function GetDesktopWidth: Integer; static; inline;
    class function GetDesktopHeight: Integer; static; inline;
    class function GetScreenResolutions: TMZScreenResolutionArray; static; inline;
    class function GetMaxAnisotropyLevel: Integer; inline; static;
    class function GetMaxTextureSize: Integer; inline; static;
    class function GetMaxTextureUnits: Integer; inline; static;
    class function GetSupportsSeparateAlpha: Boolean; inline; static;
    class function GetSupportsAutomaticMipmapGeneration: Boolean; inline; static;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Sleeps a number of milliseconds.
      Parameters:
        SleepTimeMs: the number of milliseconds to sleep.
      Remarks:
        Only the current thread is "put to sleep" }
    class procedure Sleep(const SleepTimeMs: Integer); static;

    { Summary:
        As the standard Format function, but uses UTF8Strings instead of
        regular strings.
      Parameters:
        Format: the format string.
        Args: the arguments used for formatting.
      Returns:
        The formatted string. }
    class function Format(const Format: UTF8String;
      const Args: array of const): UTF8String; static;

    { Summary:
        Converts an integer value to a string.
      Parameters:
        Value: The value to convert.
      Returns:
        The string representation of the value. }
    class function IntToStr(const Value: Integer): UTF8String; static; inline;

    { Summary:
        Converts a string to an integer value.
      Parameters:
        Value: The string to convert.
      Returns:
        The integer value, or 0 if the string cannot be converted. }
    class function StrToInt(const Value: UTF8String): Integer; static; inline;

    { Summary:
        Converts a Single floating point value to a string.
      Parameters:
        Value: The value to convert.
        Digits: (optional) The number of digits after the decimal point
          (default 2).
      Returns:
        The string representation of the value. }
    class function FloatToStr(const Value: Single; const Digits: Integer = 2): UTF8String; static; inline;

    { Summary:
        Converts a string to a floating-point value.
      Parameters:
        Value: The string to convert.
      Returns:
        The floating-point value, or 0 if the string cannot be converted. }
    class function StrToFloat(const Value: UTF8String): Single; static; inline;

    { Summary:
        Converts a bolean value to a string.
      Parameters:
        Value: The value to convert.
      Returns:
        Either 'TRUE' or 'FALSE'. }
    class function BoolToStr(const Value: Boolean): UTF8String; static; inline;

    { Summary:
        Converts a string to a boolean value.
      Parameters:
        Value: The string to convert.
      Returns:
        True if the string has value '1' or 'True' (case-insensitive),
        or False otherwise. }
    class function StrToBool(const Value: UTF8String): Boolean; static; inline;

    { Summary:
        Returns the ZenGL version in $MaMiRe format, where Ma is major version,
        Mi is minor version and Re is revision. }
    class property ZenGLVersion: Integer read GetZenGLVersion;

    { Summary:
        Returns the ZenGL version as a string. }
    class property ZenGLVersionString: UTF8String read GetZenGLVersionString;

    { Summary:
        Returns the ZenGL version date as a string. }
    class property ZenGLVersionDate: UTF8String read GetZenGLVersionDate;

    { Summary:
        Returns the MondoZenGL version in $MaMiRe format, where Ma is major
        version, Mi is minor version and Re is revision. }
    class property MondoZenGLVersion: Integer read GetMondoZenGLVersion;

    { Summary:
        Returns the MondoZenGL version as a string. }
    class property MondoZenGLVersionString: String read GetMondoZenGLVersionString;

    { Summary:
        Returns the MondoZenGL version date as a string. }
    class property MondoZenGLVersionDate: String read GetMondoZenGLVersionDate;

    { Summary:
        Returns the application directory. }
    class property ApplicationDirectory: UTF8String read GetApplicationDirectory;

    { Summary:
        Returns the home directory.
      Remarks:
        This is the directory where you can safely write application data.
        You should create a subdirectory in this directory that is specific to
        your application or company. }
    class property HomeDirectory: UTF8String read GetHomeDirectory;

    { Summary:
        Returns the desktop width in pixels.
      Remarks:
        This is the resolution of the primary monitor/screen. }
    class property DesktopWidth: Integer read GetDesktopWidth;

    { Summary:
        Returns the desktop height in pixels.
      Remarks:
        This is the resolution of the primary monitor/screen. }
    class property DesktopHeight: Integer read GetDesktopHeight;

    { Summary:
        Returns a list of supported screen resolutions.
      Remarks:
        This can be useful for selecting a full-screen resolution. }
    class property ScreenResolutions: TMZScreenResolutionArray read GetScreenResolutions;

    { Summary:
        The maximum level of anisotropy supported by the system. Used in
        combination with <link TMZTexture.AnisotropyLevel> }
    class property MaxAnisotropyLevel: Integer read GetMaxAnisotropyLevel;

    { Summary:
        The maximum texture size (width and height) supported by the system. }
    class property MaxTextureSize: Integer read GetMaxTextureSize;

    { Summary:
        The maximum number texture processing units supported by the system. }
    class property MaxTextureUnits: Integer read GetMaxTextureUnits;

    { Summary:
        Returns True of the system supports separate blending modes for the
        Alpha channel (see <link TMZCanvas.SeparateAlpha>) }
    class property SupportsSeparateAlpha: Boolean read GetSupportsSeparateAlpha;

    { Summary:
        Returns True of the system supports automatic generation of mipmaps.
      Remarks:
        See also tfGenerateMipMaps flag in <link TMZTextureFlag> }
    class property SupportsAutomaticMipmapGeneration: Boolean read GetSupportsAutomaticMipmapGeneration;
  end;
{$ENDREGION 'Utilities'}

{$REGION 'Sound'}
type
  { Summary:
      Abstract base class for <link TMZStaticSound> and <link TMZStreamingSound>. }
  TMZSound = class abstract
  public
  end;

const
  { Summary:
      Represents all playing sound sources.
      Can be passed to <link TMZStaticSound.Stop> }
  MZ_SOUND_ALL_SOURCES = SND_ALL_SOURCES;

  { Summary:
      Represents all sound sources that are playing in a looped state.
      Can be passed to <link TMZStaticSound.Stop> }
  MZ_SOUND_ALL_LOOPED_SOURCES = SND_ALL_SOURCES_LOOPED;

type
  { Summary:
      Represents a static sound. A static sound is a (usually short) sound that
      is entirely loaded into memory for fast playback. Static sounds are
      commonly used for sound effects. }
  TMZStaticSound = class(TMZSound)
  {$REGION 'Internal Declarations'}
  private
    FHandle: zglPSound;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates an empty sound.
      Parameters:
        MaxChannels: (optional) the maximum number of channels that can be used
          to play this sound (defaults to 8). This is the maximum number of
          times this sound can be played simultaneously.
      Remarks:
        Raises an exception if the sound cannot be created. }
    constructor Create(const MaxChannels: Integer = 8); overload;

    { Summary:
        Creates a sound from a file on disk or in a ZIP archive.
      Parameters:
        Filename: the name of the file to load the sound from. The engine
          supports the file formats WAV (.wav) and OGG (.ogg).
        MaxChannels: (optional) the maximum number of channels that can be used to
          play this sound (defaults to 8). This is the maximum number of times
          this sound can be played simultaneously.
      Remarks:
        To support OGG on Windows with Delphi, you need to deploy the OGG DLL's
        (currently libogg-0.dll, libvorbis-0.dll and libvorbisfile-3.dll).
        When using FreePascal on Windows, OGG support is linked statically by
        default and no DLL's are needed.
        If a ZIP archive has been opened (see <link TMZZipArchive>), then the
        sound will be loaded from the ZIP file instead.
        Because this is a static sound, you should only load relatively short
        sound files (up to a couple of seconds).
        Raises an exception if the file does not exist or is invalid. }
    constructor Create(const Filename: UTF8String; const MaxChannels: Integer = 8); overload;

    { Summary:
        Creates a sound from a stream.
      Parameters:
        Stream: the stream containing the sound data.
        Extension: the extension used to describe the sound data in the stream.
          Currently supported are .wav and .ogg.
        MaxChannels: (optional) the maximum number of channels that can be used to
          play this sound (defaults to 8). This is the maximum number of times
          this sound can be played simultaneously.
      Remarks:
        To support OGG on Windows with Delphi, you need to deploy the OGG DLL's
        (currently libogg-0.dll, libvorbis-0.dll and libvorbisfile-3.dll).
        When using FreePascal on Windows, OGG support is linked statically by
        default and no DLL's are needed.
        The sound is read from the current position in the stream.
        Because this is a static sound, you should only load relatively short
        sound files (up to a couple of seconds).
        Raises an exception if the stream is invalid. }
    constructor Create(const Stream: TStream; const Extension: UTF8String;
      const MaxChannels: Integer = 8); overload;

    { Summary:
        Creates a sound from a memory.
      Parameters:
        Buffer: the memory buffer to load the sound from.
        Size: the size of the memory buffer.
        Extension: the extension used to describe the sound data in the stream.
          Currently supported are .wav and .ogg.
        MaxChannels: (optional) the maximum number of channels that can be used to
          play this sound (defaults to 8). This is the maximum number of times
          this sound can be played simultaneously.
      Remarks:
        To support OGG on Windows with Delphi, you need to deploy the OGG DLL's
        (currently libogg-0.dll, libvorbis-0.dll and libvorbisfile-3.dll).
        When using FreePascal on Windows, OGG support is linked statically by
        default and no DLL's are needed.
        Because this is a static sound, you should only load relatively short
        sound files (up to a couple of seconds).
        Raises an exception if the stream is invalid. }
    constructor Create(const Buffer: Pointer; const Size: Integer;
      const Extension: UTF8String; const MaxChannels: Integer = 8); overload;

    { Summary:
        Creates a sound from a memory.
      Parameters:
        Memory: the memory buffer to load the sound from.
        Extension: the extension used to describe the sound data in the stream.
          Currently supported are .wav and .ogg.
        MaxChannels: (optional) the maximum number of channels that can be used to
          play this sound (defaults to 8). This is the maximum number of times
          this sound can be played simultaneously.
      Remarks:
        To support OGG on Windows with Delphi, you need to deploy the OGG DLL's
        (currently libogg-0.dll, libvorbis-0.dll and libvorbisfile-3.dll).
        When using FreePascal on Windows, OGG support is linked statically by
        default and no DLL's are needed.
        Because this is a static sound, you should only load relatively short
        sound files (up to a couple of seconds).
        Raises an exception if the stream is invalid. }
    constructor Create(const Memory: TMZMemory; const Extension: UTF8String;
      const MaxChannels: Integer = 8); overload;
    destructor Destroy; override;

    { Summary:
        Plays the sound.
      Parameters:
        Looped: (optional) whether to loop the sound (default False).
        X: (optional) X-position of sound in 3D space.
        Y: (optional) Y-position of sound in 3D space.
        Z: (optional) Z-position of sound in 3D space.
      Returns:
        The channel number used to play the sound (ranges from 0 to
        MaxChannels-1, as passed to the constructor). You can pass this channel
        to <link Stop> to stop playback of this channel. }
    function Play(const Looped: Boolean = False; const X: Single = 0;
      const Y: Single = 0; const Z: Single = 0): Integer; inline;

    { Summary:
        Stops playback of the sound (or one or more of its channels).
      Parameters:
        Channel: (optional) the channel to stop. This is the channel number
          returned by <link Play>. You can also specify the following special
          values:
          MZ_SOUND_ALL_SOURCES (default): stops playback of all sources
            belonging to this sound.
          MZ_SOUND_ALL_LOOPED_SOURCES: stops playback of all sources belonging
            to this sound that are currently playing in a looped state. }
    procedure Stop(const Channel: Integer = MZ_SOUND_ALL_SOURCES); inline;

    { Summary:
        Stops all playing static sounds (not just this sound).
      Paramters:
        LoopedOnly: (optional) when set to True, this method will only stop
          sounds that are currently playing in a looped state. Otherwise
          (default), it will stop playback of all sounds. }
    class procedure StopAll(const LoopedOnly: Boolean = False); inline; static;

    { Summary:
        Sets the 3D position of the sound (or one or more of its channels).
      Parameters:
        X: X-position of sound in 3D space.
        Y: Y-position of sound in 3D space.
        Z: Z-position of sound in 3D space.
        Channel: (optional) the channel to position. This is the channel number
          returned by <link Play>. You can also specify MZ_SOUND_ALL_SOURCES
          (default) to set the position of all channels belonging to this sound. }
    procedure Set3DPosition(const X, Y, Z: Single;
      const Channel: Integer = MZ_SOUND_ALL_SOURCES); inline;

    { Summary:
        Sets the 3D position of all static sounds (not just this sound).
      Parameters:
        X: X-position of sound in 3D space.
        Y: Y-position of sound in 3D space.
        Z: Z-position of sound in 3D space. }
    class procedure Set3DPositionAll(const X, Y, Z: Single); inline; static;

    { Summary:
        Sets the volume of the sound (or one or more of its channels).
      Parameters:
        Volume: the volume to set, ranging from 0.0 to 1.0.
        Channel: (optional) the channel to change the volume for. This is the
          channel number returned by <link Play>. You can also specify
          MZ_SOUND_ALL_SOURCES (default) to set the volume of all sources
          belonging to this sound. }
    procedure SetVolume(const Volume: Single;
      const Channel: Integer = MZ_SOUND_ALL_SOURCES); inline;

    { Summary:
        Sets the volume of all static sounds (not just this sound).
      Parameters:
        Volume: the volume to set, ranging from 0.0 to 1.0. }
    class procedure SetVolumeAll(const Volume: Single); inline; static;

    { Summary:
        Sets the playback speed factor of the sound (or one or more of its
        channels).
      Parameters:
        Speed: the speed factor (1.0 for normal speed).
        Channel: (optional) the channel to change the volume for. This is the
          channel number returned by <link Play>. You can also specify
          MZ_SOUND_ALL_SOURCES (default) to set the volume of all sources
          belonging to this sound. }
    procedure SetSpeed(const Speed: Single;
      const Channel: Integer = MZ_SOUND_ALL_SOURCES); inline;

    { Summary:
        Sets the playback speed factor of all static sounds (not just this
        sound).
      Parameters:
        Speed: the speed factor (1.0 for normal speed). }
    class procedure SetSpeedAll(const Speed: Single); inline; static;

    { Summary:
        Returns whether the given sound channel is playing.
      Parameters:
        Channel: the channel to check. This is the channel number returned by
          <link Play>.
      Returns:
        True if the channel is currently playing, False otherwise. }
    function IsPlaying(const Channel: Integer): Boolean; inline;

    { Summary:
        Returns whether the given sound channel is in a looped state.
      Parameters:
        Channel: the channel to check. This is the channel number returned by
          <link Play>.
      Returns:
        True if the channel is currently in a looped state, False otherwise. }
    function IsLooped(const Channel: Integer): Boolean; inline;

    { Summary:
        Internal handle used for the sound.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property Handle: zglPSound read FHandle;
  end;

type
  { Summary:
      Represents a streaming sound. A streaming sound is a sound that is loaded
      as it plays. Streaming sounds are commonly used for music tracks. }
  TMZStreamingSound = class(TMZSound)
  {$REGION 'Internal Declarations'}
  private
    FID: Integer;
    FFilenameOrExtension: UTF8String;
    FMemory: zglTMemory;
    FOwnsMemory: Boolean;
    function GetLength: Double; inline;
    function GetPercentComplete: Integer;
    function GetPlaybackPosition: Double; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a streaming sound from a file.
      Parameters:
        Filename: the name of the file to load the sound from. The engine
          supports the file formats WAV (.wav) and OGG (.ogg).
      Remarks:
        To support OGG on Windows with Delphi, you need to deploy the OGG DLL's
        (currently libogg-0.dll, libvorbis-0.dll and libvorbisfile-3.dll).
        When using FreePascal on Windows, OGG support is linked statically by
        default and no DLL's are needed.
        Streaming sounds <b>cannot</b> be loaded from a ZIP archive. }
    constructor Create(const Filename: UTF8String); overload;

    { Summary:
        Creates a streaming sound from a stream.
      Parameters:
        Stream: the stream containing the sound data.
        Extension: the extension used to describe the sound data in the stream.
          Currently supported are .wav and .ogg.
      Remarks:
        To support OGG on Windows with Delphi, you need to deploy the OGG DLL's
        (currently libogg-0.dll, libvorbis-0.dll and libvorbisfile-3.dll).
        When using FreePascal on Windows, OGG support is linked statically by
        default and no DLL's are needed.
        The sound is read from the current position in the stream. }
    constructor Create(const Stream: TStream; const Extension: UTF8String); overload;

    { Summary:
        Creates a streaming sound from a memory.
      Parameters:
        Buffer: the memory buffer to load the sound from.
        Size: the size of the memory buffer.
        Extension: the extension used to describe the sound data in the stream.
          Currently supported are .wav and .ogg.
      Remarks:
        To support OGG on Windows with Delphi, you need to deploy the OGG DLL's
        (currently libogg-0.dll, libvorbis-0.dll and libvorbisfile-3.dll).
        When using FreePascal on Windows, OGG support is linked statically by
        default and no DLL's are needed.
        The memory buffer must remain valid for the entire lifetime of this
        object. Freeing the memory while this object is still in use will lead
        to access violations. }
    constructor Create(const Buffer: Pointer; const Size: Integer;
      const Extension: UTF8String); overload;

    { Summary:
        Creates a streaming sound from a memory.
      Parameters:
        Memory: the memory buffer to load the sound from.
        Extension: the extension used to describe the sound data in the stream.
          Currently supported are .wav and .ogg.
      Remarks:
        To support OGG on Windows with Delphi, you need to deploy the OGG DLL's
        (currently libogg-0.dll, libvorbis-0.dll and libvorbisfile-3.dll).
        When using FreePascal on Windows, OGG support is linked statically by
        default and no DLL's are needed.
        The memory buffer must remain valid for the entire lifetime of this
        object. Freeing the memory while this object is still in use will lead
        to access violations. }
    constructor Create(const Memory: TMZMemory; const Extension: UTF8String); overload;
    destructor Destroy; override;

    { Summary:
        Plays the streaming sound.
      Parameters:
        Looped: (optional) whether to loop the sound (default False). }
    procedure Play(const Looped: Boolean = False); inline;

    { Summary:
        Pauses playback of the streaming sound. }
    procedure Pause; inline;

    { Summary:
        Resumes playback of the streaming sound. }
    procedure Resume; inline;

    { Summary:
        Stops playback of the streaming sound. }
    procedure Stop; inline;

    { Summary:
        Sets the 3D position of the streaming sound.
      Parameters:
        X: X-position of sound in 3D space.
        Y: Y-position of sound in 3D space.
        Z: Z-position of sound in 3D space. }
    procedure Set3DPosition(const X, Y, Z: Single); inline;

    { Summary:
        Sets the 3D position of all streaming sounds (not just this sound).
      Parameters:
        X: X-position of sound in 3D space.
        Y: Y-position of sound in 3D space.
        Z: Z-position of sound in 3D space. }
    class procedure Set3DPositionAll(const X, Y, Z: Single); inline; static;

    { Summary:
        Sets the volume of the streaming sound.
      Parameters:
        Volume: the volume to set, ranging from 0.0 to 1.0. }
    procedure SetVolume(const Volume: Single); inline;

    { Summary:
        Sets the volume of all streaming sounds (not just this sound).
      Parameters:
        Volume: the volume to set, ranging from 0.0 to 1.0. }
    class procedure SetVolumeAll(const Volume: Single); inline; static;

    { Summary:
        Sets the playback speed factor of the streaming sound.
      Parameters:
        Speed: the speed factor (1.0 for normal speed). }
    procedure SetSpeed(const Speed: Single); inline;

    { Summary:
        Sets the playback speed factor of all streaming sounds (not just this
        sound).
      Parameters:
        Speed: the speed factor (1.0 for normal speed). }
    class procedure SetSpeedAll(const Speed: Single); inline; static;

    { Summary:
        Returns whether the sound is playing.
      Returns:
        True if the sound is currently playing, False otherwise. }
    function IsPlaying: Boolean; inline;

    { Summary:
        Returns whether the sound is in a looped state.
      Returns:
        True if the sound is currently in a looped state, False otherwise. }
    function IsLooped: Boolean; inline;

    { Summary:
        Internal stream ID for the sound.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property ID: Integer read FID;

    { Summary:
        Length of the stream in seconds. }
    property Length: Double read GetLength;

    { Summary:
        Current playback position in seconds. }
    property PlaybackPosition: Double read GetPlaybackPosition;

    { Summary:
        Percentage of playback completed (0-100). }
    property PercentComplete: Integer read GetPercentComplete;
  end;
{$ENDREGION 'Sprites'}

{$REGION 'Video'}
{$IFDEF USE_THEORA}
type
  { Summary:
      Represents a video stream.
    Remarks:
      Currently, only OGG Theora video is supported. }
  TMZVideoStream = class
  {$REGION 'Internal Declarations'}
  private
    FHandle: zglPVideoStream;
    FMemory: zglTMemory;
    FOwnsMemory: Boolean;
    FTexture: TMZTexture;
    function GetFrameNumber: Integer; inline;
    function GetPositionMs: Double; inline;
    function GetDurationMs: Double; inline;
    function GetFrameCount: Integer; inline;
    function GetFrameRate: Single; inline;
    function GetHeight: Integer; inline;
    function GetWidth: Integer; inline;
  private
    procedure Initialize;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a video stream from a file.
      Parameters:
        Filename: the name of the file to load the sound from. The engine
          supports the OGG Theora file formats .OGG and .OGV.
      Remarks:
        To support Theora video on Windows with Delphi, you need to deploy the
        OGG Theora DLL's (currently libtheoradec-1.dll, libvorbis-0.dll and
        libvorbisfile-3.dll). When using FreePascal on Windows, OGG Theora
        support is linked statically by default and no DLL's are needed.
        Videos <b>cannot</b> be loaded from a ZIP archive. }
    constructor Create(const Filename: UTF8String); overload;

    { Summary:
        Creates a video stream from a stream.
      Parameters:
        Stream: the stream containing the video data.
        Extension: the extension used to describe the video data in the stream.
          Currently supported are .OGG and .OGV.
      Remarks:
        To support Theora video on Windows with Delphi, you need to deploy the
        OGG Theora DLL's (currently libtheoradec-1.dll, libvorbis-0.dll and
        libvorbisfile-3.dll). When using FreePascal on Windows, OGG Theora
        support is linked statically by default and no DLL's are needed. }
    constructor Create(const Stream: TStream; const Extension: UTF8String); overload;

    { Summary:
        Creates a video stream from a memory.
      Parameters:
        Buffer: the memory buffer to load the video from.
        Size: the size of the memory buffer.
        Extension: the extension used to describe the video data in the stream.
          Currently supported are .OGG and .OGV.
      Remarks:
        To support Theora video on Windows with Delphi, you need to deploy the
        OGG Theora DLL's (currently libtheoradec-1.dll, libvorbis-0.dll and
        libvorbisfile-3.dll). When using FreePascal on Windows, OGG Theora
        support is linked statically by default and no DLL's are needed.
        The memory buffer must remain valid for the entire lifetime of this
        object. Freeing the memory while this object is still in use will lead
        to access violations. }
    constructor Create(const Buffer: Pointer; const Size: Integer;
      const Extension: UTF8String); overload;

    { Summary:
        Creates a video stream from a memory.
      Parameters:
        Memory: the memory buffer to load the video from.
        Extension: the extension used to describe the video data in the stream.
          Currently supported are .OGG and .OGV.
      Remarks:
        To support Theora video on Windows with Delphi, you need to deploy the
        OGG Theora DLL's (currently libtheoradec-1.dll, libvorbis-0.dll and
        libvorbisfile-3.dll). When using FreePascal on Windows, OGG Theora
        support is linked statically by default and no DLL's are needed.
        The memory buffer must remain valid for the entire lifetime of this
        object. Freeing the memory while this object is still in use will lead
        to access violations. }
    constructor Create(const Memory: TMZMemory; const Extension: UTF8String); overload;
    destructor Destroy; override;

    { Summary:
        Must be called during each iteration of the main loop to update the
        video stream.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
          call to Update.
        Loop: (optional) whether to loop the video when the end is reached.
      Remarks:
        You usually call this from the <link TMZScene.Update> method. }
    procedure Update(const DeltaTimeMs: Double; const Loop: Boolean = False); inline;

    { Summary:
        Seeks to the given position in the video stream.
      Parameters:
        SeekTimeMs: the time to seek to in milliseconds. }
    procedure Seek(const SeekTimeMs: Double); inline;

    { Summary:
        The texture that the video is rendered to. }
    property Texture: TMZTexture read FTexture;

    { Summary:
        Current frame number that is displayed. }
    property FrameNumber: Integer read GetFrameNumber;

    { Summary:
        Current position in the stream in milliseconds. }
    property PositionMs: Double read GetPositionMs;

    { Summary:
        Width of the video }
    property Width: Integer read GetWidth;

    { Summary:
        Height of the video }
    property Height: Integer read GetHeight;

    { Summary:
        Frame rate of the video in frames per second }
    property FrameRate: Single read GetFrameRate;

    { Summary:
        Duration of the video in milliseconds }
    property DurationMs: Double read GetDurationMs;

    { Summary:
        Number of frames in the video stream }
    property FrameCount: Integer read GetFrameCount;

    { Summary:
        Internal handle used for the video stream.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property Handle: zglPVideoStream read FHandle;
  end;
{$ENDIF USE_THEORA}
{$ENDREGION 'Video'}

{$REGION 'File IO'}
type
  { Summary:
      File open modes for use with <link TMZFile> }
  TMZFileOpenMode = (
    { Summary:
        Creates the file. }
    omCreate = FOM_CREATE,

    { Summary:
        Opens the file for reading. }
    omRead = FOM_OPENR,

    { Summary:
        Opens the file for reading and writing. }
    omReadWrite = FOM_OPENRW);

type
  { Summary:
      Represents a file on disk or inside a <link TMZZipArchive>. }
  TMZFile = class
  {$REGION 'Internal Declarations'}
  private
    FHandle: zglTFile;
    FFilename: UTF8String;
    function GetPosition: Integer; inline;
    function GetSize: Integer; inline;
    procedure SetPosition(const Value: Integer); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates or opens a file.
      Parameters:
        Filename: the name of the file to open or create.
        OpenMode: how to open the file.
      Remarks:
        Raises an exception if the file cannot be opened. }
    constructor Create(const Filename: UTF8String; const OpenMode: TMZFileOpenMode);

    { Summary:
        Closes the file. }
    destructor Destroy; override;

    { Summary:
        Seeks in the file.
      Parameters:
        Offset: the offset to seek. Depends on Mode.
        Mode: the seek mode to use.
      Returns:
        The new file pointer position.
      Remarks:
        Seeking does not work for files within a ZIP archive. }
    function Seek(const Offset: Integer; const Mode: TMZSeekMode): Integer; inline;

    { Summary:
        Reads from the file.
      Parameters:
        Buffer: the buffer to read into.
        Count: the number of bytes to read.
      Returns:
        The number of bytes actually read. }
    function Read(var Buffer; const Count: Integer): Integer; inline;

    { Summary:
        Writes to the file.
      Parameters:
        Buffer: the buffer to write.
        Count: the number of bytes to write.
      Returns:
        The number of bytes actually written. }
    function Write(const Buffer; const Count: Integer): Integer; inline;

    { Summary:
        Flushes the file.
      Remarks:
        Does not work for files within a ZIP archive. }
    procedure Flush; inline;

    { Summary:
        The name of the file. }
    property Filename: UTF8String read FFilename;

    { Summary:
        The current position in the file.
      Remarks:
        Returns 0 for files within a ZIP archive. }
    property Position: Integer read GetPosition write SetPosition;

    { Summary:
        The size of the file. }
    property Size: Integer read GetSize;

    { Summary:
        Internal handle used for the file.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property Handle: zglTFile read FHandle;
  end;

type
  { Summary:
      Static class for working with ZIP files.
    Remarks:
      You can only work with <b>one</b> ZIP file at a time. Also you should
      <b>not</b> open regular files (using <link TMZFile>) while you have
      opened a ZIP file. You need to close the ZIP file before opening regular
      files.
      Once a ZIP file has been opened, all file access will be redirected to
      the ZIP file. This includes loading textures, sounds etc. from files. }
  TMZZipArchive = class
  public
    { Summary:
        Opens a ZIP file.
      Parameters:
        Filename: the name of the ZIP file to open.
        Password: (optional) the password to use in case the ZIP file is
          encrypted.
      Remarks:
        Raises an exception if the ZIP file cannot be opened.
        While the ZIP file is open, all file access (including <link TMZFile>)
        will be redirected to the ZIP file. So you cannot work with regular
        files while the ZIP file is open. You need to close the ZIP file
        first (see <link CloseArchive>. }
    class procedure OpenArchive(const Filename:
      UTF8String; const Password: UTF8String = ''); inline; static;

    { Summary:
        Closes the ZIP file. }
    class procedure CloseArchive; inline; static;

    { Summary:
        Opens a file in the ZIP archive.
      Parameters:
        Filename: the name of the file in the ZIP archive to open.
      Returns:
        The file in the ZIP archive, or nil if the file could not be opened. }
    class function Open(const Filename: UTF8String): TMZFile; inline; static;
  end;

type
  { Summary:
      Static class for working with the file system. }
  TMZFileSystem = class
  public
    { Summary:
        Tries to create a directory.
      Parameters:
        DirectoryName: the name of the directory to create.
      Returns:
        True on success, False on failure. }
    class function CreateDirectory(const DirectoryName: UTF8String): Boolean; inline; static;

    { Summary:
        Tries to delete a file from disk.
      Parameters:
        Filename: the name of the file to delete.
      Returns:
        True on success, False on failure. }
    class function DeleteFile(const Filename: UTF8String): Boolean; inline; static;

    { Summary:
        Checks whether a file exists.
      Parameters:
        Filename: the name of the file to check.
      Returns:
        True if the file exists, False otherwise. }
    class function FileExists(const Filename: UTF8String): Boolean; inline; static;

    { Summary:
        Gets the files in the given directory.
      Parameters:
        DirectoryName: the name of the directory to search.
        IncludeDirectories: (optional) whether to include directory names in
          the search (default False):
      Returns:
        An array of the file (and optionally directory) names in the given
        directory. }
    class function Find(const DirectoryName: UTF8String;
      const IncludeDirectories: Boolean = False): TUTF8StringArray; inline; static;

    { Summary:
        Gets the name part of a filename.
      Parameters:
        Filename: the filename.
      Returns:
        The name part of the filename. }
    class function GetName(const Filename: UTF8String): UTF8String; inline; static;

    { Summary:
        Gets the extension part of a filename.
      Parameters:
        Filename: the filename.
      Returns:
        The extension part of the filename. }
    class function GetExtension(const Filename: UTF8String): UTF8String; inline; static;

    { Summary:
        Gets the directory part of a filename.
      Parameters:
        Filename: the filename.
      Returns:
        The directory part of the filename. }
    class function GetDirectory(const Filename: UTF8String): UTF8String; inline; static;

    { Summary:
        Sets the path.
      Parameters:
        Path: the path to set. }
    class procedure SetPath(const Path: UTF8String); inline; static;
  end;

type
  { Summary:
      Static class for working with INI files.
    Remarks:
      You can only work with <b>one</b> INI file at a time. }
  TMZIniFile = class
  public
    { Summary:
        Loads an INI file.
      Parameters:
        Filename: the filename of the INI file.
      Returns:
        True on success or False on failure. }
    class function Load(const Filename: UTF8String): Boolean; inline; static;

    { Summary:
        Saves the INI file to a file.
      Parameters:
        Filename: the filename to save to. }
    class procedure Save(const Filename: UTF8String); inline; static;

    { Summary:
        Adds a section with a key (without value) to the INI file.
      Parameters:
        Section: the section name (as in '[Settings]')
        Key: the key name (as in 'key=value') }
    class procedure Add(const Section, Key: UTF8String); inline; static;

    { Summary:
        Deletes a key from a section.
      Parameters:
        Section: the section name (as in '[Settings]')
        Key: the key name (as in 'key=value') }
    class procedure Delete(const Section, Key: UTF8String); inline; static;

    { Summary:
        Clears an entire INI section.
      Parameters:
        Section: the section name (as in '[Settings]') }
    class procedure Clear(const Section: UTF8String); inline; static;

    { Summary:
        Checks whether a section exists.
      Parameters:
        Section: the section name (as in '[Settings]')
      Returns:
        True if the section exists, False otherwise. }
    class function SectionExists(const Section: UTF8String): Boolean; inline; static;

    { Summary:
        Checks whether a key exists within a section.
      Parameters:
        Section: the section name (as in '[Settings]')
        Key: the key name (as in 'key=value')
      Returns:
        True if the key exists within the section, False otherwise. }
    class function KeyExists(const Section, Key: UTF8String): Boolean; inline; static;

    { Summary:
        Reads the value of a key within a section as a string.
      Parameters:
        Section: the section name (as in '[Settings]')
        Key: the key name (as in 'key=value')
      Returns:
        The value of the key, or '' if the key does not exist. }
    class function ReadString(const Section, Key: UTF8String): UTF8String; inline; static;

    { Summary:
        Reads the value of a key within a section as an integer.
      Parameters:
        Section: the section name (as in '[Settings]')
        Key: the key name (as in 'key=value')
      Returns:
        The value of the key, or 0 if the key does not exist. }
    class function ReadInt(const Section, Key: UTF8String): Integer; inline; static;

    { Summary:
        Reads the value of a key within a section as a floating-point value.
      Parameters:
        Section: the section name (as in '[Settings]')
        Key: the key name (as in 'key=value')
      Returns:
        The value of the key, or 0 if the key does not exist. }
    class function ReadFloat(const Section, Key: UTF8String): Single; inline; static;

    { Summary:
        Reads the value of a key within a section as a boolean value.
      Parameters:
        Section: the section name (as in '[Settings]')
        Key: the key name (as in 'key=value')
      Returns:
        The value of the key, or False if the key does not exist. }
    class function ReadBool(const Section, Key: UTF8String): Boolean; inline; static;

    { Summary:
        Writes a string value to a key within a section.
      Parameters:
        Section: the section name (as in '[Settings]')
        Key: the key name (as in 'key=value')
        Value: the value to write
      Returns:
        If the key already exists, it is updated and True is returned.
        If the key does not exist, it is added and False is returned. }
    class function WriteString(const Section, Key, Value: UTF8String): Boolean; inline; static;

    { Summary:
        Writes an integer value to a key within a section.
      Parameters:
        Section: the section name (as in '[Settings]')
        Key: the key name (as in 'key=value')
        Value: the value to write
      Returns:
        If the key already exists, it is updated and True is returned.
        If the key does not exist, it is added and False is returned. }
    class function WriteInt(const Section, Key: UTF8String; const Value: Integer): Boolean; inline; static;

    { Summary:
        Writes a floating-point value to a key within a section.
      Parameters:
        Section: the section name (as in '[Settings]')
        Key: the key name (as in 'key=value')
        Value: the value to write
      Returns:
        If the key already exists, it is updated and True is returned.
        If the key does not exist, it is added and False is returned. }
    class function WriteFloat(const Section, Key: UTF8String; const Value: Single): Boolean; inline; static;

    { Summary:
        Writes a boolean value to a key within a section.
      Parameters:
        Section: the section name (as in '[Settings]')
        Key: the key name (as in 'key=value')
        Value: the value to write
      Returns:
        If the key already exists, it is updated and True is returned.
        If the key does not exist, it is added and False is returned. }
    class function WriteBool(const Section, Key: UTF8String; const Value: Boolean): Boolean; inline; static;
  end;
{$ENDREGION 'File IO'}

{$REGION 'Distortion Mesh'}
type
  { Summary:
      A distortion mesh used for drawing a texture in a distorted way.
    Remarks:
      A distortion mesh is like a grid:

        <code>
        +----+----+----+
        |    |    |    |
        +----+----+----+
        |    |    |    |
        +----+----+----+
        </code>

      This distortion mesh has 2 rows and 3 columns. As you can see, the number
      of vertices per row (indicated by "+") equals the number of columns + 1.
      Likewise, the number of vertices per column equals the number of rows + 1. }
  TMZDistortionMesh = class
  {$REGION 'Internal Declarations'}
  private
    FSettings: zglTGrid2D;
    FHandle: zglPGrid2D;
    FRowCount: Integer;
    FColumnCount: Integer;
    function GetVertex(const Row, Column: Integer): TMZPoint; inline;
    procedure SetVertex(const Row, Column: Integer; const Value: TMZPoint); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a distortion mesh using the given number of rows and columns.
      Parameters:
        RowCount: the number of rows in the distortion mesh.
        ColumnCount: the number of columns in the distortion mesh.
      Remarks:
        The number of vertices per row equals ColumnCount+1.
        Likewise, the number of vertices per column equals RowCount+1.
        See the documentation for <link TMZDistortionMesh> for the reason. }
    constructor Create(const RowCount, ColumnCount: Integer);

    { Summary:
        Renders a texture using the distortion mesh.
      Parameters:
        Texture: the texture to render.
        XOffset: the X-offset to use.
        YOffset: the Y-offset to use.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX and efFlipY. }
    procedure Render(const Texture: TMZTexture; const XOffset, YOffset: Single;
      const Alpha: Byte = $FF; const Flags: TMZEffectFlags = [efBlend]); overload; inline;

    { Summary:
        Renders a texture using the distortion mesh.
      Parameters:
        Texture: the texture to render.
        SR: Source rectangle containing the part of the texture to draw. The
          coordinates of this rectangle must lie within the size of the texture.
        XOffset: the X-offset to use.
        YOffset: the Y-offset to use.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX and efFlipY. }
    procedure Render(const Texture: TMZTexture; const SR: TMZRect;
      const XOffset, YOffset: Single; const Alpha: Byte = $FF;
      const Flags: TMZEffectFlags = [efBlend]); overload; inline;

    { Summary:
        Renders a frame of a texture using the distortion mesh.
      Parameters:
        Texture: the sprite texture to render.
        FrameNum: the frame number to draw. NOTE: Frame numbers are 1 based
          (so range from 1 to FrameCount).
        XOffset: the X-offset to use.
        YOffset: the Y-offset to use.
        Alpha: (optional) Level of alpha transparency (0=fully transparent,
          255=fully opaque).
        Flags: (optional) effect flags (defaults to [efBlend]). Supported flags
          are: efBlend, efColor, efFlipX and efFlipY. }
    procedure Render(const Texture: TMZTexture; const FrameNum: Integer;
      const XOffset, YOffset: Single; const Alpha: Byte = $FF;
      const Flags: TMZEffectFlags = [efBlend]); overload; inline;

    { Summary:
        Internal handle used for the distortion mesh.
      Remarks:
        You should only use the property if you need access to the lower-level
        ZenGL library directly. }
    property Handle: zglPGrid2D read FHandle;

    { Summary:
        Number of rows in the mesh (this is 1 less than the number of vertices
        per column) }
    property RowCount: Integer read FRowCount;

    { Summary:
        Number of columns in the mesh (this is 1 less than the number of
        vertices per row) }
    property ColumnCount: Integer read FColumnCount;

    { Summary:
        The vertices of the distortion mesh.
      Parameters:
        Row: the row number of the vertex, ranging from 0 to RowCount (inclusive).
        Column: the column number of the vertex, ranging from 0 to ColumnCount (inclusive). }
    property Vertices[const Row, Column: Integer]: TMZPoint read GetVertex write SetVertex; default;
  end;
{$ENDREGION 'Distortion Mesh'}

{$REGION 'Triangulation'}
{$IFDEF USE_TRIANGULATION}
type
  { Summary:
      Static class for triangulating shapes.
    Remarks:
      You can only work with <b>one</b> triangulation calculation at a time.
      Triangulation is not supported on iOS. }
  TMZTriangulator = class
  public
    { Summary:
        Start a triangulation.
      Parameters:
        Contour: the points of the contour (shape) to triangulate.
      Remarks:
        The shape cannot have any crossing lines. You must call
        <link EndTriangulation> before you can start a new triangulation
        calculation. }
    class procedure BeginTriangulation(const Contour: array of TMZPoint); overload; static;

    { Summary:
        Start a triangulation.
      Parameters:
        Contour: the points of the contour (shape) to triangulate.
        StartIndex: the index of the first point in Contour to use.
        EndIndex: the index of the last point in Contour to use.
      Remarks:
        The shape cannot have any crossing lines. You must call
        <link EndTriangulation> before you can start a new triangulation
        calculation. }
    class procedure BeginTriangulation(const Contour: array of TMZPoint;
      const StartIndex, EndIndex: Integer); overload; static;

    { Summary:
        Adds a hole to the shape started with <link BeginTriangulation>.
      Parameters:
        Contour: the points of the contour of the hole to add.
      Remarks:
        The hole shape cannot have any crossing lines and should not cross any
        other holes or the shape passed to <link BeginTriangulation>. }
    class procedure AddHole(const Contour: array of TMZPoint); overload; static;

    { Summary:
        Adds a hole to the shape started with <link BeginTriangulation>.
      Parameters:
        Contour: the points of the contour of the hole to add.
        StartIndex: the index of the first point in Contour to use.
        EndIndex: the index of the last point in Contour to use.
      Remarks:
        The hole shape cannot have any crossing lines and should not cross any
        other holes or the shape passed to <link BeginTriangulation>. }
    class procedure AddHole(const Contour: array of TMZPoint;
      const StartIndex, EndIndex: Integer); overload; static;

    { Summary:
        Finishes the triangulation started with <link BeginTriangulation> (and
        optional calls to <link AddHole>). Calculates the triangles that make
        up the shape and holes.
      Returns:
        The triangles that make up the shape with optional holes.
      Remarks:
        When you pass invalid contours to <link BeginTriangulation> or
        <link AddHole> then the result may be empty or invalid.
        You can draw the returned triangles using
        <link TMZCanvas.DrawTriangleList> and <link TMZCanvas.FillTriangleList>. }
    class function EndTriangulation: TMZTriangleArray; static;
  end;
{$ENDIF}
{$ENDREGION 'Triangulation'}

{$REGION 'Resource Queue'}
(* Disabled for now because of multi-threading errors with FPC
type
  { Summary:
      Static class for multi-threaded resource loading.
    Remarks:
      Any resources (textures, sounds, fonts etc.) you create/load between calls
      to <link TMZResourceQueue.Start> and <link TMZResourceQueue.Stop> will
      be queued and loaded in a separate thread. }
  TMZResourceQueue = class
  public
    { Summary:
        Starts multi-threaded loading of resources.
      Parameters:
        QueueID: (optional) The queue ID to use (0-255) in case you need
          multiple queues.
      Remarks:
        Any resources (textures, sounds etc.) you create/load between calls
        to <link Start> and <link Stop> will be queued and loaded in a separate
        thread. }
    class procedure Start(const QueueID: Byte = 0); static;

    { Summary:
        Stops multi-threaded loading of resources.
      Remarks:
        Any resources (textures, sounds etc.) you create/load between calls
        to <link Start> and <link Stop> will be queued and loaded in a separate
        thread. }
    class procedure Stop; static;

    { Summary:
        Gets the total percentage of resources that has been completely loaded
        (in all queues).
      Returns:
        The percentage completed, between 0 and 100. }
    class function PercentageCompleted: Integer; overload; static;

    { Summary:
        Gets the percentage of resources that has been completely loaded in the
        given queue.
      Parameters:
        QueueID: The ID of the queue (0-255) to check.
      Returns:
        The percentage completed for the given queue, between 0 and 100. }
    class function PercentageCompleted(const QueueID: Byte): Integer; overload; static;
  end;*)
{$ENDREGION 'Resource Queue'}

{$REGION 'Resource Strings'}
resourcestring
  RS_MZ_ERROR_CANNOT_LOAD_FONT = 'Unable to load font.';
  RS_MZ_ERROR_CANNOT_LOAD_FONT_FILE = 'Unable to load font "%s".';
  RS_MZ_ERROR_CANNOT_CREATE_TEXTURE = 'Unable to create texture.';
  RS_MZ_ERROR_CANNOT_LOAD_TEXTURE_FILE = 'Unable to load texture image "%s".';
  RS_MZ_ERROR_CANNOT_LOAD_TEXTURE = 'Unable to load texture.';
  RS_MZ_ERROR_CANNOT_CREATE_SOUND = 'Unable to create sound.';
  RS_MZ_ERROR_CANNOT_LOAD_SOUND = 'Unable to load sound.';
  RS_MZ_ERROR_CANNOT_LOAD_SOUND_FILE = 'Unable to load sound file "%s".';
  RS_MZ_ERROR_CANNOT_CREATE_RENDER_TARGET = 'Unable to create render target.';
  RS_MZ_ERROR_CANNOT_LOAD_PARTCLE_EMITTER_FILE = 'Unable to load particle emitter file "%s".';
  RS_MZ_ERROR_CANNOT_LOAD_PARTCLE_EMITTER = 'Unable to load particle emitter.';
  RS_MZ_ERROR_CANNOT_OPEN_FILE = 'Unable to open file "%s".';
  RS_MZ_ERROR_CANNOT_OPEN_ZIP_FILE = 'Unable to open ZIP file "%s".';
  RS_MZ_ERROR_FILE_NIL = 'File cannot be nil.';
  RS_MZ_ERROR_CANNOT_LOAD_VIDEO = 'Unable to load video.';
  RS_MZ_ERROR_CANNOT_LOAD_VIDEO_FILE = 'Unable to load video file "%s".';
{$ENDREGION 'Resource Strings'}

{$REGION 'Internal'}
type
  TZGLTextureCoordinates = array [0..3] of zglTPoint2D;
{$ENDREGION 'Internal'}

implementation

{$IFNDEF FPC}
uses
  AnsiStrings;
{$ENDIF}

procedure SysLoad;
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.Startup;
end;

procedure SysDraw;
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.RenderFrame;
end;

procedure SysUpdate(DT: Double);
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.Update(DT);
end;

procedure SysExit;
begin
  if Assigned(TMZApplication.Instance) then
  begin
    TMZApplication.Instance.Shutdown;
    TMZApplication.Instance.Free;
  end;
end;

procedure SysActivate(Activate: Boolean);
begin
  if Assigned(TMZApplication.Instance) then
    if (Activate) then
      TMZApplication.Instance.Activate
    else
      TMZApplication.Instance.Deactivate
end;

procedure SysMouseMove(X, Y: Integer);
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.MouseMove(X, Y);
end;

procedure SysMousePress(Button: Byte);
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.MouseDown(TMZMouseButton(Button));
end;

procedure SysMouseRelease(Button: Byte);
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.MouseUp(TMZMouseButton(Button));
end;

procedure SysMouseWheel(Axis: Byte);
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.MouseWheel(Axis = M_WUP);
end;

procedure SysKeyPress(KeyCode: Byte);
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.KeyDown(TMZKeyCode(KeyCode));
end;

procedure SysKeyRelease(KeyCode: Byte);
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.KeyUp(TMZKeyCode(KeyCode));
end;

procedure SysKeyChar(Symbol: String);
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.KeyChar(Symbol);
end;

{$IFDEF iOS}
procedure SysIOSMemoryWarning;
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.SysLowMemoryWarning;
end;

procedure SysIOSChangeOrientation(Orientation: UIInterfaceOrientation);
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.SysChangeOrientation(TMZInterfaceOrientation(Orientation));
end;

procedure SysTouchMove(Finger: Byte; X, Y: Integer);
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.TouchMove(Finger, X, Y);
end;

procedure SysTouchPress(Finger: Byte);
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.TouchDown(Finger);
end;

procedure SysTouchRelease(Finger: Byte);
begin
  if Assigned(TMZApplication.Instance) then
    TMZApplication.Instance.TouchUp(Finger);
end;
{$ENDIF}

procedure TimerExpired(Sender: zglPTimer);
var
  Timer: TMZTimer;
begin
  if Assigned(Sender.UserData) then
  begin
    Timer := TMZTimer(Sender.UserData);
    if Assigned(Timer.FOnTimer) then
      Timer.FOnTimer(Timer);
  end;
end;

{ TMZLog }

class procedure TMZLog.Log(const Msg: UTF8String;
  const IncludeTiming: Boolean);
begin
  log_Add(Msg, IncludeTiming);
end;

class procedure TMZLog.Log(const Msg: UTF8String; const Args: array of const;
  const IncludeTiming: Boolean);
begin
  log_Add(TMZUtils.Format(Msg, Args), IncludeTiming);
end;

{ TMZIniFile }

class function TMZIniFile.Load(const Filename: UTF8String): Boolean;
begin
  Result := ini_LoadFromFile(Filename);
end;

class procedure TMZIniFile.Save(const Filename: UTF8String);
begin
  ini_SaveToFile(Filename);
end;

class procedure TMZIniFile.Add(const Section, Key: UTF8String);
begin
  ini_Add(Section, Key);
end;

class procedure TMZIniFile.Delete(const Section, Key: UTF8String);
begin
  ini_Del(Section, Key);
end;

class procedure TMZIniFile.Clear(const Section: UTF8String);
begin
  ini_Clear(Section);
end;

class function TMZIniFile.SectionExists(const Section: UTF8String): Boolean;
begin
  Result := ini_IsSection(Section);
end;

class function TMZIniFile.KeyExists(const Section, Key: UTF8String): Boolean;
begin
  Result := ini_IsKey(Section, Key);
end;

class function TMZIniFile.ReadString(const Section, Key: UTF8String): UTF8String;
begin
  Result := ini_ReadKeyStr(Section, Key);
end;

class function TMZIniFile.ReadInt(const Section, Key: UTF8String): Integer;
begin
  Result := ini_ReadKeyInt(Section, Key);
end;

class function TMZIniFile.ReadFloat(const Section, Key: UTF8String): Single;
begin
  Result := ini_ReadKeyFloat(Section, Key);
end;

class function TMZIniFile.ReadBool(const Section, Key: UTF8String): Boolean;
begin
  Result := ini_ReadKeyBool(Section, Key);
end;

class function TMZIniFile.WriteString(const Section, Key, Value: UTF8String): Boolean;
begin
  Result := ini_WriteKeyStr(Section, Key, Value);
end;

class function TMZIniFile.WriteInt(const Section, Key: UTF8String;
  const Value: Integer): Boolean;
begin
  Result := ini_WriteKeyInt(Section, Key, Value);
end;

class function TMZIniFile.WriteFloat(const Section, Key: UTF8String;
  const Value: Single): Boolean;
begin
  Result := ini_WriteKeyFloat(Section, Key, Value);
end;

class function TMZIniFile.WriteBool(const Section, Key: UTF8String;
  const Value: Boolean): Boolean;
begin
  Result := ini_WriteKeyBool(Section, Key, Value);
end;

{ TMZZipArchive }

class procedure TMZZipArchive.OpenArchive(const Filename: UTF8String;
  const Password: UTF8String);
begin
  if (not file_OpenArchive(Filename, Password)) then
    raise EMZError.CreateFmt(RS_MZ_ERROR_CANNOT_OPEN_ZIP_FILE, [Filename]);
end;

class procedure TMZZipArchive.CloseArchive;
begin
  file_CloseArchive;
end;

class function TMZZipArchive.Open(const Filename: UTF8String): TMZFile;
begin
  Result := TMZFile.Create(Filename, omRead);
end;

{ TMZFileSystem }

class function TMZFileSystem.CreateDirectory(const DirectoryName: UTF8String): Boolean;
begin
  Result := file_MakeDir(DirectoryName);
end;

class function TMZFileSystem.DeleteFile(const Filename: UTF8String): Boolean;
begin
  Result := file_Remove(Filename);
end;

class function TMZFileSystem.FileExists(const Filename: UTF8String): Boolean;
begin
  Result := file_Exists(Filename);
end;

class function TMZFileSystem.Find(const DirectoryName: UTF8String;
  const IncludeDirectories: Boolean): TUTF8StringArray;
var
  List: zglTFileList;
  I: Integer;
begin
  file_Find(DirectoryName, List, IncludeDirectories);
  SetLength(Result, List.Count);
  for I := 0 to List.Count - 1 do
    Result[I] := List.Items[I];
end;

class function TMZFileSystem.GetName(const Filename: UTF8String): UTF8String;
begin
  Result := file_GetName(Filename);
end;

class function TMZFileSystem.GetExtension(const Filename: UTF8String): UTF8String;
begin
  Result := file_GetExtension(Filename);
end;

class function TMZFileSystem.GetDirectory(const Filename: UTF8String): UTF8String;
begin
  Result := file_GetDirectory(Filename);
end;

class procedure TMZFileSystem.SetPath(const Path: UTF8String);
begin
  file_SetPath(Path);
end;

{ TMZFile }

function TMZFile.GetPosition: Integer;
begin
  Result := file_GetPos(FHandle);
end;

function TMZFile.GetSize: Integer;
begin
  Result := file_GetSize(FHandle);
end;

constructor TMZFile.Create(const Filename: UTF8String; const OpenMode: TMZFileOpenMode);
begin
  inherited Create;
  FFilename := Filename;
  if (not file_Open(FHandle, Filename, Ord(OpenMode))) then
    raise EMZError.CreateFmt(RS_MZ_ERROR_CANNOT_OPEN_FILE, [Filename]);
end;

destructor TMZFile.Destroy;
begin
  file_Close(FHandle);
  inherited Destroy;
end;

function TMZFile.Seek(const Offset: Integer;
  const Mode: TMZSeekMode): Integer;
begin
  Result := file_Seek(FHandle, Offset, Ord(Mode));
end;

procedure TMZFile.SetPosition(const Value: Integer);
begin
  file_Seek(FHandle, Value, FSM_SET);
end;

function TMZFile.Read(var Buffer; const Count: Integer): Integer;
begin
  Result := file_Read(FHandle, Buffer, Count);
end;

function TMZFile.Write(const Buffer; const Count: Integer): Integer;
begin
  Result := file_Write(FHandle, Buffer, Count);
end;

procedure TMZFile.Flush;
begin
  file_Flush(FHandle);
end;

{ TMZTimer }

function TMZTimer.GetInterval: Integer;
begin
  if Assigned(FHandle) then
    Result := FHandle.Interval
  else
    Result := 0;
end;

function TMZTimer.GetActive: Boolean;
begin
  if Assigned(FHandle) then
    Result := FHandle.Active
  else
    Result := False;
end;

procedure TMZTimer.SetActive(const Value: Boolean);
begin
  if Assigned(FHandle) then
    FHandle.Active := Value;
end;

procedure TMZTimer.SetInterval(const Value: Integer);
begin
  if Assigned(FHandle) then
    FHandle.Interval := Value;
end;

constructor TMZTimer.Create(const OnTimer: TNotifyEvent;
  const IntervalMs: Integer);
begin
  inherited Create;
  FOnTimer := OnTimer;
  FHandle := timer_Add(@TimerExpired, IntervalMs, True, Self);
end;

destructor TMZTimer.Destroy;
begin
  if Assigned(FHandle) then
    timer_Del(FHandle);
  inherited Destroy;
end;

{ TMZParticleEngine }

function TMZParticleEngine.GetEmitter(const Index: Integer): TMZParticleEmitter;
begin
  if (Index >= 0) and (Index < FEmitterCount) then
    Result := FEmitters[Index]
  else
    Result := nil;
end;

procedure TMZParticleEngine.DeleteEmitter(const Index: Integer);
begin
  if (Index >= 0) and (Index < FEmitterCount) then
  begin
    FEmitters[Index].Free;
    FEmitters[Index] := FEmitters[FEmitterCount - 1];
    Dec(FEmitterCount);
    FModified := True;
  end;
end;

procedure TMZParticleEngine.SortByLayer(const ALo, AHi: Integer);
var
  Lo, Hi, Mid: Integer;
  T: TMZParticleEmitter;
begin
  Lo := ALo;
  Hi := AHi;
  Mid := FEmitters[(Lo + Hi) shr 1].FEmitter.Params.Layer;

  repeat
    while (FEmitters[Lo].FEmitter.Params.Layer < Mid) do
      Inc(Lo);
    while (FEmitters[Hi].FEmitter.Params.Layer > Mid) do
      Dec(Hi);
    if (Lo <= Hi) then
    begin
      T := FEmitters[Lo];
      FEmitters[Lo] := FEmitters[Hi];
      FEmitters[Hi] := T;
      Inc(Lo);
      Dec(Hi);
    end;
  until (Lo > Hi);

  if (Hi > ALo) then
    SortByLayer(ALo, Hi);
  if (Lo < AHi) then
    SortByLayer(Lo, AHi);
end;

procedure TMZParticleEngine.SortByID(const ALo, AHi: Integer);
var
  Lo, Hi, Mid: Integer;
  T: TMZParticleEmitter;
begin
  Lo := ALo;
  Hi := AHi;
  Mid := FEmitters[(Lo + Hi) shr 1].FEmitter.ID;

  repeat
    while (FEmitters[Lo].FEmitter.ID < Mid) do
      Inc(Lo);
    while (FEmitters[Hi].FEmitter.ID > Mid) do
      Dec(Hi);
    if (Lo <= Hi) then
    begin
      T := FEmitters[Lo];
      FEmitters[Lo] := FEmitters[Hi];
      FEmitters[Hi] := T;
      Inc(Lo);
      Dec(Hi);
    end;
  until (Lo > Hi);

  if (Hi > ALo) then
    SortByID(ALo, Hi);
  if (Lo < AHi) then
    SortByID(Lo, AHi);
end;

constructor TMZParticleEngine.Create;
begin
  inherited Create;
end;

destructor TMZParticleEngine.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMZParticleEngine.AddEmitter(const Emitter: TMZParticleEmitter);
begin
  if (FEmitterCount >= Length(FEmitters)) then
    SetLength(FEmitters, FEmitterCount + 16);
  FEmitters[FEmitterCount] := Emitter;
  Emitter.FEmitter.ID := FEmitterCount;
  Inc(FEmitterCount);
  FModified := True;
end;

function TMZParticleEngine.AddEmitterCopy(const Source: TMZParticleEmitter;
  const XOffset: Single; const YOffset: Single): TMZParticleEmitter;
begin
  Result := TMZParticleEmitter.Create(Source);
  Result.FEmitter.Params.Position.X := Result.FEmitter.Params.Position.X + XOffset;
  Result.FEmitter.Params.Position.Y := Result.FEmitter.Params.Position.Y + YOffset;
  AddEmitter(Result);
end;

procedure TMZParticleEngine.RemoveEmitter(const Emitter: TMZParticleEmitter);
begin
  if Assigned(Emitter) then
    DeleteEmitter(Emitter.FEmitter.ID);
end;

procedure TMZParticleEngine.Clear;
var
  I: Integer;
begin
  for I := 0 to FEmitterCount - 1 do
    FEmitters[I].Free;
  FEmitterCount := 0;
  FEmitters := nil;
end;

procedure TMZParticleEngine.Render;
var
  OldBlend, OldColor: Byte;
  I: Integer;
begin
  OldBlend := Ord(TMZCanvas.BlendMode);
  OldColor := Ord(TMZCanvas.ColorMode);

  for I := 0 to FEmitterCount - 1 do
    FEmitters[I].Render;

  if (OldBlend <> Byte(Ord(TMZCanvas.BlendMode))) then
    fx_SetBlendMode(OldBlend);
  if (OldColor <> Byte(Ord(TMZCanvas.ColorMode))) then
    fx_SetColorMode(OldColor);
end;

procedure TMZParticleEngine.Update(const DeltaTimeMs: Double);
var
  I, L, A, B: Integer;
  E: TMZParticleEmitter;
begin
  FParticleCount := 0;
  I := 0;
  while (I < FEmitterCount) do
  begin
    E := FEmitters[I];
    E.Update(DeltaTimeMs);
    if (E.FEmitter.Life <= 0) and (not E.FEmitter.Params.Loop) and (E.FEmitter.Particles = 0) then
      DeleteEmitter(I)
    else
    begin
      Inc(I);
      Inc(FParticleCount, E.ParticleCount);
    end;
  end;

  if (FModified) and (FEmitterCount > 1) then
  begin
    L := 0;
    for I := 0 to FEmitterCount - 1 do
    begin
      E := FEmitters[I];
      if (E.FEmitter.Params.Layer > L) then
        L := E.FEmitter.Params.Layer;
      if (E.FEmitter.Params.Layer < L) then
      begin
        SortByLayer(0, FEmitterCount - 1);
        L := FEmitters[0].FEmitter.Params.Layer;
        A := 0;
        for B := 0 to FEmitterCount - 1 do
        begin
          E := FEmitters[B];
          if (L <> E.FEmitter.Params.Layer) then
          begin
            SortByID(A, B - 1);
            A := B;
            L := E.FEmitter.Params.Layer;
          end;
          if (B = (FEmitterCount - 1)) then
            SortByID(A, B);
        end;
        for A := 0 to FEmitterCount - 1 do
          FEmitters[A].FEmitter.ID := A;
        Break;
      end;
    end;
  end;
end;

{ TMZParticleEmitter }

function TMZParticleEmitter.GetEmitterType: TMZParticleEmitterType;
begin
  Result := TMZParticleEmitterType(FEmitter.Type_);
end;

function TMZParticleEmitter.GetBlendMode: TMZBlendMode;
begin
  Result := TMZBlendMode(FEmitter.ParParams.BlendMode);
end;

procedure TMZParticleEmitter.SetBlendMode(const Value: TMZBlendMode);
begin
  FEmitter.ParParams.BlendMode := Ord(Value);
end;

function TMZParticleEmitter.GetColorMode: TMZColorMode;
begin
  Result := TMZColorMode(FEmitter.ParParams.ColorMode);
end;

procedure TMZParticleEmitter.SetColorMode(const Value: TMZColorMode);
begin
  FEmitter.ParParams.ColorMode := Ord(Value);
end;

function TMZParticleEmitter.GetSpinAngle: Single;
begin
  Result := FEmitter.ParParams.SpinS * rad2deg;
end;

procedure TMZParticleEmitter.SetEmitterType(const Typ: Byte; const X, Y: Single);
begin
  FEmitter.Type_ := Typ;
  FEmitter.Params.Position.X := X;
  FEmitter.Params.Position.Y := Y;
  FEmitter.Params.Emission := EmissionRate;
end;

class function TMZParticleEmitter.CopyEmitter(
  const Src: zglTEmitter2D): zglTEmitter2D;
var
  S, D: zglPParticleParams;
  Count: Integer;
begin
  Result := Src;

  { We need to deep copy the dynamic arrays so they become independent. }
  S := @Src.ParParams;
  D := @Result.ParParams;

  Count := Length(S.Color);
  SetLength(D.Color, Count);
  if (Count > 0) then
    Move(S.Color[0], D.Color[0], Count * SizeOf(S.Color[0]));

  Count := Length(S.Alpha);
  SetLength(D.Alpha, Count);
  if (Count > 0) then
    Move(S.Alpha[0], D.Alpha[0], Count * SizeOf(S.Alpha[0]));

  Count := Length(S.SizeXD);
  SetLength(D.SizeXD, Count);
  if (Count > 0) then
    Move(S.SizeXD[0], D.SizeXD[0], Count * SizeOf(S.SizeXD[0]));

  Count := Length(S.SizeYD);
  SetLength(D.SizeYD, Count);
  if (Count > 0) then
    Move(S.SizeYD[0], D.SizeYD[0], Count * SizeOf(S.SizeYD[0]));

  Count := Length(S.VelocityD);
  SetLength(D.VelocityD, Count);
  if (Count > 0) then
    Move(S.VelocityD[0], D.VelocityD[0], Count * SizeOf(S.VelocityD[0]));

  Count := Length(S.aVelocityD);
  SetLength(D.aVelocityD, Count);
  if (Count > 0) then
    Move(S.aVelocityD[0], D.aVelocityD[0], Count * SizeOf(S.aVelocityD[0]));

  Count := Length(S.SpinD);
  SetLength(D.SpinD, Count);
  if (Count > 0) then
    Move(S.SpinD[0], D.SpinD[0], Count * SizeOf(S.SpinD[0]));
end;

procedure TMZParticleEmitter.SetSpinAngleVariation(const Value: Single);
begin
  FEmitter.ParParams.SpinV := Value * deg2rad;
end;

function TMZParticleEmitter.GetAngularVelocity: Single;
begin
  Result := FEmitter.ParParams.aVelocityS * rad2deg;
end;

procedure TMZParticleEmitter.SetAngularVelocity(const Value: Single);
begin
  FEmitter.ParParams.aVelocityS := Value * deg2rad;
end;

function TMZParticleEmitter.GetAngularVelocityVariation: Single;
begin
  Result := FEmitter.ParParams.aVelocityV * rad2deg;
end;

procedure TMZParticleEmitter.SetAngularVelocityVariation(const Value: Single);
begin
  FEmitter.ParParams.aVelocityV := Value * deg2rad;
end;

function TMZParticleEmitter.GetBoundingBox: TMZRect;
begin
  Result := TMZRect.Create(FEmitter.BBox.MinX, FEmitter.BBox.MinY,
    FEmitter.BBox.MaxX - FEmitter.BBox.MinX,
    FEmitter.BBox.MaxY - FEmitter.BBox.MinY);
end;

procedure TMZParticleEmitter.SetSpinAngle(const Value: Single);
begin
  FEmitter.ParParams.SpinS := Value * deg2rad;
end;

function TMZParticleEmitter.GetSpinAngleVariation: Single;
begin
  Result := FEmitter.ParParams.SpinV * rad2deg;
end;

constructor TMZParticleEmitter.Create(const Texture: TMZTexture);
begin
  inherited Create;
  FHandle := @FEmitter;
  emitter2d_Init(FHandle);
  if Assigned(Texture) then
    FEmitter.ParParams.Texture := Texture.Handle;
end;

constructor TMZParticleEmitter.Create(const Filename: UTF8String;
  const Texture: TMZTexture);
var
  Original: zglPEmitter2D;
begin
  inherited Create;
  Original := emitter2d_LoadFromFile(Filename);
  if (Original = nil) then
    raise EMZError.CreateFmt(RS_MZ_ERROR_CANNOT_LOAD_PARTCLE_EMITTER_FILE, [Filename]);
  FEmitter := CopyEmitter(Original^);
  FHandle := @FEmitter;
  if Assigned(Texture) then
    FEmitter.ParParams.Texture := Texture.Handle;
  emitter2d_Free(Original);
end;

constructor TMZParticleEmitter.Create(const Stream: TStream;
  const Texture: TMZTexture);
var
  Memory: zglTMemory;
  Original: zglPEmitter2D;
begin
  inherited Create;
  Memory.Size := Stream.Size - Stream.Position;
  GetMem(Memory.Memory, Memory.Size);
  try
    Memory.Position := 0;
    Stream.ReadBuffer(Memory.Memory^, Memory.Size);

    Original := emitter2d_LoadFromMemory(Memory);
    if (Original = nil) then
      raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_PARTCLE_EMITTER);
    FEmitter := CopyEmitter(Original^);
    FHandle := @FEmitter;
    if Assigned(Texture) then
      FEmitter.ParParams.Texture := Texture.Handle;
    emitter2d_Free(Original);
  finally
    FreeMem(Memory.Memory);
  end;
end;

constructor TMZParticleEmitter.Create(const Buffer: Pointer;
  const Size: Integer; const Texture: TMZTexture);
var
  Memory: zglTMemory;
  Original: zglPEmitter2D;
begin
  inherited Create;
  Memory.Memory := Buffer;
  Memory.Size := Size;
  Memory.Position := 0;

  Original := emitter2d_LoadFromMemory(Memory);
  if (Original = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_PARTCLE_EMITTER);
  FEmitter := CopyEmitter(Original^);
  FHandle := @FEmitter;
  if Assigned(Texture) then
    FEmitter.ParParams.Texture := Texture.Handle;
  emitter2d_Free(Original);
end;

constructor TMZParticleEmitter.Create(const Source: TMZParticleEmitter);
begin
  Assert(Assigned(Source));
  inherited Create;
  FEmitter := CopyEmitter(Source.FEmitter);
  FHandle := @FEmitter;
end;

constructor TMZParticleEmitter.Create(const Memory: TMZMemory;
  const Texture: TMZTexture);
var
  Original: zglPEmitter2D;
begin
  Assert(Assigned(Memory));
  inherited Create;
  Original := emitter2d_LoadFromMemory(Memory.FSettings);
  if (Original = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_PARTCLE_EMITTER);
  FEmitter := CopyEmitter(Original^);
  FHandle := @FEmitter;
  if Assigned(Texture) then
    FEmitter.ParParams.Texture := Texture.Handle;
  emitter2d_Free(Original);
end;

destructor TMZParticleEmitter.Destroy;
begin
  inherited Destroy;
end;

procedure TMZParticleEmitter.Render;
begin
  emitter2d_Draw(FHandle);
end;

procedure TMZParticleEmitter.Update(const DeltaTimeMs: Double);
begin
  emitter2d_Proc(FHandle, DeltaTimeMs);
end;

procedure TMZParticleEmitter.SetPointEmitter(const X, Y, Direction,
  Spread: Single);
begin
  SetEmitterType(EMITTER_POINT, X, Y);
  FEmitter.AsPoint.Direction := Direction * deg2rad;
  FEmitter.AsPoint.Spread := Spread * deg2rad;
end;

procedure TMZParticleEmitter.SetLineEmitter(const X, Y, Length, Direction,
  Spread: Single; const TwoSided: Boolean);
begin
  SetEmitterType(EMITTER_LINE, X, Y);
  FEmitter.AsLine.Direction := Direction * deg2rad;
  FEmitter.AsLine.Spread := Spread * deg2rad;
  FEmitter.AsLine.Size := Length;
  FEmitter.AsLine.TwoSide := TwoSided;
end;

procedure TMZParticleEmitter.SetRectangleEmitter(const X, Y, Width, Height,
  Direction, Spread: Single);
begin
  SetEmitterType(EMITTER_RECTANGLE, X, Y);
  FEmitter.AsRect.Direction := Direction * deg2rad;
  FEmitter.AsRect.Spread := Spread * deg2rad;
  FEmitter.AsRect.Rect.X := 0;
  FEmitter.AsRect.Rect.Y := 0;
  FEmitter.AsRect.Rect.W := Width;
  FEmitter.AsRect.Rect.H := Height;
end;

procedure TMZParticleEmitter.SetCircleEmitter(const X, Y, Radius, Direction,
  Spread: Single);
begin
  SetEmitterType(EMITTER_CIRCLE, X, Y);
  FEmitter.AsCircle.Direction := Direction * deg2rad;
  FEmitter.AsCircle.Spread := Spread * deg2rad;
  FEmitter.AsCircle.cX := 0;
  FEmitter.AsCircle.cY := 0;
  FEmitter.AsCircle.Radius := Radius;
end;

procedure TMZParticleEmitter.SetRingEmitter(const X, Y, Radius1, Radius2,
  Direction, Spread: Single);
begin
  SetEmitterType(EMITTER_CIRCLE, X, Y);
  FEmitter.AsRing.Direction := Direction * deg2rad;
  FEmitter.AsRing.Spread := Spread * deg2rad;
  FEmitter.AsRing.cX := 0;
  FEmitter.AsRing.cY := 0;
  FEmitter.AsRing.Radius0 := Radius1;
  FEmitter.AsRing.Radius1 := Radius2;
end;

function TMZParticleEmitter.GetEndFrameNumber: Integer;
begin
  Result := FEmitter.ParParams.Frame[1];
end;

function TMZParticleEmitter.GetStartFrameNumber: Integer;
begin
  Result := FEmitter.ParParams.Frame[0];
end;

procedure TMZParticleEmitter.SetColors(const Colors: array of Cardinal;
  const Ages: array of Single);
var
  I: Integer;
begin
  SetLength(FEmitter.ParParams.Color, Length(Colors));
  for I := 0 to Length(Colors) - 1 do
  begin
    FEmitter.ParParams.Color[I].Value := Colors[I];
    if (I < Length(Ages)) then
      FEmitter.ParParams.Color[I].Life := Ages[I]
    else
      FEmitter.ParParams.Color[I].Life := 1.0;
  end;
end;

procedure TMZParticleEmitter.SetEndFrameNumber(const Value: Integer);
begin
  FEmitter.ParParams.Frame[1] := Value;
end;

procedure TMZParticleEmitter.SetAlphas(const Alphas: array of Byte;
  const Ages: array of Single);
var
  I: Integer;
begin
  SetLength(FEmitter.ParParams.Alpha, Length(Alphas));
  for I := 0 to Length(Alphas) - 1 do
  begin
    FEmitter.ParParams.Alpha[I].Value := Alphas[I];
    if (I < Length(Ages)) then
      FEmitter.ParParams.Alpha[I].Life := Ages[I]
    else
      FEmitter.ParParams.Alpha[I].Life := 1.0;
  end;
end;

procedure TMZParticleEmitter.SetSizes(const Sizes, Ages: array of Single;
  const AgesHeight: TSingleArray);
var
  Count, I: Integer;
begin
  Assert(not Odd(Length(Sizes)));
  Count := Length(Sizes) div 2;
  SetLength(FEmitter.ParParams.SizeXD, Count);
  SetLength(FEmitter.ParParams.SizeYD, Count);
  for I := 0 to Count - 1 do
  begin
    FEmitter.ParParams.SizeXD[I].Value := Sizes[I * 2 + 0];
    FEmitter.ParParams.SizeYD[I].Value := Sizes[I * 2 + 0];

    if (I < Length(Ages)) then
      FEmitter.ParParams.SizeXD[I].Life := Ages[I]
    else
      FEmitter.ParParams.SizeXD[I].Life := 1.0;

    if (I < Length(AgesHeight)) then
      FEmitter.ParParams.SizeYD[I].Life := AgesHeight[I]
    else if (I < Length(Ages)) then
      FEmitter.ParParams.SizeYD[I].Life := Ages[I]
    else
      FEmitter.ParParams.SizeYD[I].Life := 1.0;
  end;
end;

procedure TMZParticleEmitter.SetStartFrameNumber(const Value: Integer);
begin
  FEmitter.ParParams.Frame[0] := Value;
end;

procedure TMZParticleEmitter.SetVelocities(const Velocities,
  Ages: array of Single);
var
  I: Integer;
begin
  SetLength(FEmitter.ParParams.VelocityD, Length(Velocities));
  for I := 0 to Length(Velocities) - 1 do
  begin
    FEmitter.ParParams.VelocityD[I].Value := Velocities[I];
    if (I < Length(Ages)) then
      FEmitter.ParParams.VelocityD[I].Life := Ages[I]
    else
      FEmitter.ParParams.VelocityD[I].Life := 1.0;
  end;
end;

procedure TMZParticleEmitter.SetAngularVelocities(const Velocities,
  Ages: array of Single);
var
  I: Integer;
begin
  SetLength(FEmitter.ParParams.aVelocityD, Length(Velocities));
  for I := 0 to Length(Velocities) - 1 do
  begin
    FEmitter.ParParams.aVelocityD[I].Value := Velocities[I];
    if (I < Length(Ages)) then
      FEmitter.ParParams.aVelocityD[I].Life := Ages[I]
    else
      FEmitter.ParParams.aVelocityD[I].Life := 1.0;
  end;
end;

procedure TMZParticleEmitter.SetSpinAngles(const Factors,
  Ages: array of Single);
var
  I: Integer;
begin
  SetLength(FEmitter.ParParams.SpinD, Length(Factors));
  for I := 0 to Length(Factors) - 1 do
  begin
    FEmitter.ParParams.SpinD[I].Value := Factors[I];
    if (I < Length(Ages)) then
      FEmitter.ParParams.SpinD[I].Life := Ages[I]
    else
      FEmitter.ParParams.SpinD[I].Life := 1.0;
  end;
end;

{ TMZRenderTarget }

constructor TMZRenderTarget.Create(const Texture: TMZTexture;
  const Options: TMZRenderTargetOptions);
var
  Opts: TMZRenderTargetOptions;
begin
  inherited Create;
  Opts := Options;
  FTexture := Texture;
  FOwnsTexture := (rtOwnsTexture in Opts);
  if (FOwnsTexture) then
    Exclude(Opts, rtOwnsTexture);
  if Assigned(Texture) and Assigned(Texture.Handle) then
    FHandle := rtarget_Add(Texture.Handle, Byte(Opts));
  if (FHandle = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_CREATE_RENDER_TARGET);
end;

destructor TMZRenderTarget.Destroy;
begin
  if (FOwnsTexture) then
    FTexture.Free;

  if Assigned(FHandle) then
  begin
    { Clear render target texture so it will not be freed (since we take care
      of that). }
    FHandle.Surface := nil;
    rtarget_Del(FHandle);
  end;
  inherited Destroy;
end;

{ TMZStreamingSound }

function TMZStreamingSound.GetLength: Double;
begin
  {$HINTS OFF}
  if (FID >= 0) then
    Result := (snd_Get(SND_STREAM, FID, SND_INFO_DURATION) * 0.001)
  else
    Result := 0;
  {$HINTS ON}
end;

function TMZStreamingSound.GetPercentComplete: Integer;
begin
  {$HINTS OFF}
  if (FID >= 0) then
    Result := snd_Get(SND_STREAM, FID, SND_STATE_PERCENT)
  else
    Result := 0;
  {$HINTS ON}
end;

function TMZStreamingSound.GetPlaybackPosition: Double;
begin
  {$HINTS OFF}
  if (FID >= 0) then
    Result := (snd_Get(SND_STREAM, FID, SND_STATE_TIME) * 0.001)
  else
    Result := 0;
  {$HINTS ON}
end;

constructor TMZStreamingSound.Create(const Filename: UTF8String);
begin
  inherited Create;
  FID := -1;
  FFilenameOrExtension := Filename;
end;

constructor TMZStreamingSound.Create(const Stream: TStream;
  const Extension: UTF8String);
begin
  inherited Create;
  FID := -1;
  FFilenameOrExtension := Extension;
  FMemory.Size := Stream.Size - Stream.Position;
  GetMem(FMemory.Memory, FMemory.Size);
  FOwnsMemory := True;
  Stream.ReadBuffer(FMemory.Memory^, FMemory.Size);
end;

constructor TMZStreamingSound.Create(const Buffer: Pointer;
  const Size: Integer; const Extension: UTF8String);
begin
  inherited Create;
  FID := -1;
  FFilenameOrExtension := Extension;
  FMemory.Memory := Buffer;
  FMemory.Size := Size;
end;

constructor TMZStreamingSound.Create(const Memory: TMZMemory;
  const Extension: UTF8String);
begin
  Assert(Assigned(Memory));
  inherited Create;
  FID := -1;
  FFilenameOrExtension := Extension;
  FMemory := Memory.FSettings;
end;

destructor TMZStreamingSound.Destroy;
begin
  if (FID >= -1) then
    snd_StopStream(FID);
  if FOwnsMemory then
    FreeMem(FMemory.Memory);
  inherited Destroy;
end;

procedure TMZStreamingSound.Play(const Looped: Boolean);
begin
  if Assigned(FMemory.Memory) then
    FID := snd_PlayMemory(FMemory, FFilenameOrExtension, Looped)
  else if (FFilenameOrExtension <> '') then
    FID := snd_PlayFile(FFilenameOrExtension, Looped)
  else
    FID := -1;
end;

procedure TMZStreamingSound.Pause;
begin
  if (FID >= 0) then
    snd_PauseStream(FID);
end;

procedure TMZStreamingSound.Resume;
begin
  if (FID >= 0) then
    snd_ResumeStream(FID);
end;

procedure TMZStreamingSound.Stop;
begin
  if (FID >= 0) then
    snd_StopStream(FID);
end;

procedure TMZStreamingSound.Set3DPosition(const X, Y, Z: Single);
begin
  {$HINTS OFF}
  if (FID >= 0) then
    snd_SetPos(SND_STREAM, FID, X, Y, Z);
  {$HINTS ON}
end;

class procedure TMZStreamingSound.Set3DPositionAll(const X, Y, Z: Single);
begin
  snd_SetPos(SND_STREAM, SND_ALL_STREAMS, X, Y, Z);
end;

procedure TMZStreamingSound.SetVolume(const Volume: Single);
begin
  {$HINTS OFF}
  if (FID >= 0) then
    snd_SetVolume(SND_STREAM, FID, Volume);
  {$HINTS ON}
end;

class procedure TMZStreamingSound.SetVolumeAll(const Volume: Single);
begin
  snd_SetVolume(SND_STREAM, SND_ALL_STREAMS, Volume);
end;

procedure TMZStreamingSound.SetSpeed(const Speed: Single);
begin
  {$HINTS OFF}
  if (FID >= 0) then
    snd_SetSpeed(SND_STREAM, FID, Speed);
  {$HINTS ON}
end;

class procedure TMZStreamingSound.SetSpeedAll(const Speed: Single);
begin
  snd_SetSpeed(SND_STREAM, SND_ALL_STREAMS, Speed);
end;

function TMZStreamingSound.IsPlaying: Boolean;
begin
  {$HINTS OFF}
  if (FID >= 0) then
    Result := (snd_Get(SND_STREAM, FID, SND_STATE_PLAYING) <> 0)
  else
    Result := False;
  {$HINTS ON}
end;

function TMZStreamingSound.IsLooped: Boolean;
begin
  {$HINTS OFF}
  if (FID >= 0) then
    Result := (snd_Get(SND_STREAM, FID, SND_STATE_LOOPED) <> 0)
  else
    Result := False;
  {$HINTS ON}
end;

{ TMZStaticSound }

constructor TMZStaticSound.Create(const MaxChannels: Integer);
begin
  inherited Create;
  FHandle := snd_Add(MaxChannels);
  if (FHandle = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_CREATE_SOUND);
end;

constructor TMZStaticSound.Create(const Filename: UTF8String;
  const MaxChannels: Integer);
begin
  inherited Create;
  FHandle := snd_LoadFromFile(Filename, MaxChannels);
  if (FHandle = nil) then
    raise EMZError.CreateFmt(RS_MZ_ERROR_CANNOT_LOAD_SOUND_FILE, [Filename]);
end;

constructor TMZStaticSound.Create(const Stream: TStream;
  const Extension: UTF8String; const MaxChannels: Integer);
var
  Memory: zglTMemory;
begin
  inherited Create;
  Memory.Size := Stream.Size - Stream.Position;
  GetMem(Memory.Memory, Memory.Size);
  try
    Memory.Position := 0;
    Stream.ReadBuffer(Memory.Memory^, Memory.Size);
    FHandle := snd_LoadFromMemory(Memory, Extension, MaxChannels);
    if (FHandle = nil) then
      raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_SOUND);
  finally
    FreeMem(Memory.Memory);
  end;
end;

constructor TMZStaticSound.Create(const Buffer: Pointer; const Size: Integer;
  const Extension: UTF8String; const MaxChannels: Integer);
var
  Memory: zglTMemory;
begin
  inherited Create;
  Memory.Memory := Buffer;
  Memory.Size := Size;
  Memory.Position := 0;
  FHandle := snd_LoadFromMemory(Memory, Extension, MaxChannels);
  if (FHandle = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_SOUND);
end;

constructor TMZStaticSound.Create(const Memory: TMZMemory;
  const Extension: UTF8String; const MaxChannels: Integer);
begin
  Assert(Assigned(Memory));
  inherited Create;
  FHandle := snd_LoadFromMemory(Memory.FSettings, Extension, MaxChannels);
  if (FHandle = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_SOUND);
end;

destructor TMZStaticSound.Destroy;
begin
  snd_Del(FHandle);
  inherited Destroy;
end;

function TMZStaticSound.Play(const Looped: Boolean; const X: Single;
  const Y: Single; const Z: Single): Integer;
begin
  Result := snd_Play(FHandle, Looped, X, Y, Z);
end;

procedure TMZStaticSound.Stop(const Channel: Integer);
begin
  if Assigned(FHandle) then
    snd_Stop(FHandle, Channel);
end;

class procedure TMZStaticSound.StopAll(const LoopedOnly: Boolean);
begin
  if (LoopedOnly) then
    snd_Stop(nil, SND_ALL_SOURCES_LOOPED)
  else
    snd_Stop(nil, SND_ALL_SOURCES);
end;

procedure TMZStaticSound.Set3DPosition(const X, Y, Z: Single;
  const Channel: Integer);
begin
  if Assigned(FHandle) then
    snd_SetPos(FHandle, Channel, X, Y, Z);
end;

class procedure TMZStaticSound.Set3DPositionAll(const X, Y, Z: Single);
begin
  snd_SetPos(SND_SOUNDS, SND_ALL_SOURCES, X, Y, Z);
end;

procedure TMZStaticSound.SetVolume(const Volume: Single;
  const Channel: Integer);
begin
  if Assigned(FHandle) then
    snd_SetVolume(FHandle, Channel, Volume);
end;

class procedure TMZStaticSound.SetVolumeAll(const Volume: Single);
begin
  snd_SetVolume(SND_SOUNDS, SND_ALL_SOURCES, Volume);
end;

procedure TMZStaticSound.SetSpeed(const Speed: Single; const Channel: Integer);
begin
  if Assigned(FHandle) then
    snd_SetSpeed(FHandle, Channel, Speed);
end;

class procedure TMZStaticSound.SetSpeedAll(const Speed: Single);
begin
  snd_SetSpeed(SND_SOUNDS, SND_ALL_SOURCES, Speed);
end;

function TMZStaticSound.IsPlaying(const Channel: Integer): Boolean;
begin
  if Assigned(FHandle) and (Channel >= 0) then
    Result := (snd_Get(FHandle, Channel, SND_STATE_PLAYING) <> 0)
  else
    Result := False;
end;

function TMZStaticSound.IsLooped(const Channel: Integer): Boolean;
begin
  if Assigned(FHandle) and (Channel >= 0) then
    Result := (snd_Get(FHandle, Channel, SND_STATE_LOOPED) <> 0)
  else
    Result := False;
end;

{ TMZSpriteEngine }

function TMZSpriteEngine.Add(const Sprite: TMZSprite): Integer;
begin
  Result := FCount;
  if (FCount >= Length(FSprites)) then
    SetLength(FSprites, FCount + 256);
  FSprites[FCount] := Sprite;
  Inc(FCount);
  FModified := True;
end;

procedure TMZSpriteEngine.DeleteDeadSprites;
var
  I, J: Integer;
begin
  J := 0;
  for I := 0 to FCount - 1 do
    if (FSprites[I].FKill) then
      FSprites[I].Destroy
    else
    begin
      FSprites[J] := FSprites[I];
      FSprites[J].FIndex := J;
      Inc(J);
    end;
  FCount := J;
end;

procedure TMZSpriteEngine.SortByLayer(const ALo, AHi: Integer);
var
  Lo, Hi, Mid: Integer;
  S: TMZSprite;
begin
  Lo := ALo;
  Hi := AHi;
  Mid := FSprites[( Lo + Hi ) shr 1].ZOrder;

  repeat
    while (FSprites[Lo].ZOrder < Mid) do
      Inc(Lo);
    while (FSprites[Hi].ZOrder > Mid) do
      Dec(Hi);
    if (Lo <= Hi) then
    begin
      S := FSprites[Lo];
      FSprites[Lo] := FSprites[Hi];
      FSprites[Hi] := S;
      Inc(Lo);
      Dec(Hi);
    end;
  until (Lo > Hi);

  if (Hi > ALo) then
    SortByLayer(ALo, Hi);
  if (Lo < AHi) then
    SortByLayer(Lo, AHi);
end;

procedure TMZSpriteEngine.SortByIndex(const ALo, AHi: Integer);
var
  Lo, Hi, Mid: Integer;
  S: TMZSprite;
begin
  Lo := ALo;
  Hi := AHi;
  Mid := FSprites[( Lo + Hi ) shr 1].Index;

  repeat
    while (FSprites[Lo].Index < Mid) do
      Inc(Lo);
    while (FSprites[Hi].Index > Mid) do
      Dec(Hi);
    if (Lo <= Hi) then
    begin
      S := FSprites[Lo];
      FSprites[Lo] := FSprites[Hi];
      FSprites[Hi] := S;
      Inc(Lo);
      Dec(Hi);
    end;
  until (Lo > Hi);

  if (Hi > ALo) then
    SortByIndex(ALo, Hi);
  if (Lo < AHi) then
    SortByIndex(Lo, AHi);
end;

function TMZSpriteEngine.GetSprite(const Index: Integer): TMZSprite;
begin
  Assert((Index >= 0) and (Index < FCount));
  Result := FSprites[Index];
end;

destructor TMZSpriteEngine.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMZSpriteEngine.Clear;
var
  I: Integer;
  S: TMZSprite;
begin
  for I := 0 to FCount - 1 do
  begin
    S := FSprites[I];
    { This prevents a call to TMZSpriteEngine.Delete: }
    S.FEngine := nil;
    S.Destroy;
  end;
  SetLength(FSprites, 0);
  FCount := 0;
end;

procedure TMZSpriteEngine.Render;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FSprites[I].Draw;
  DeleteDeadSprites;
end;

procedure TMZSpriteEngine.Update(const DeltaTimeMs: Double);
var
  A, B, I, Z: Integer;
  S: TMZSprite;
begin
  for I := 0 to FCount - 1 do
    FSprites[I].Update(DeltaTimeMs);
  DeleteDeadSprites;

  if FModified then
  begin
    if (FCount > 1) then
    begin
      Z := 0;
      for I := 0 to FCount - 1 do
      begin
        S := FSprites[I];
        if (S.ZOrder > Z) then
          Z := S.ZOrder;

        if (S.ZOrder < Z) Then
        begin
          SortByLayer(0, FCount - 1);
          Z := FSprites[0].ZOrder;
          A := 0;
          for B := 0 to FCount - 1 do
          begin
            S := FSprites[B];
            if (Z <> S.ZOrder) then
            begin
              SortByIndex(A, B - 1);
              A := B;
              Z := S.ZOrder;
            end;
            if (B = FCount - 1) Then
              SortByIndex(A, B);
          end;
          for A := 0 to FCount - 1 do
            FSprites[A].FIndex := A;
          Break;
        end;
      end;
    end;
    FModified := False;
  end;
end;

{ TMZSprite }

constructor TMZSprite.Create(const Engine: TMZSpriteEngine;
  const Texture: TMZTexture; const ZOrder: Integer);
begin
  Assert(Assigned(Engine));
  Assert(Assigned(Texture));
  inherited Create;
  FEngine := Engine;
  FTexture := Texture;
  FZOrder := ZOrder;
  FIndex := Engine.Add(Self);
  FWidth := Texture.Width / Texture.FrameColumns;
  FHeight := Texture.Height / Texture.FrameRows;
  FFrameNumber := 1;
  FAlpha := $FF;
  FEffectFlags := [efBlend];
  Initialize;
end;

procedure TMZSprite.Free;
begin
  { Don't free immediately. }
  FKill := True;
end;

procedure TMZSprite.Initialize;
begin
  { No default implementation }
end;

procedure TMZSprite.Draw;
begin
  { The default Draw implementation }
  Canvas.DrawSpriteFrame(FTexture, Round(FFrameNumber), FPosition.X, FPosition.Y,
    FWidth, FHeight, FRotationAngle, FAlpha, TMZEffectFlags(FEffectFlags));
end;

procedure TMZSprite.Update(const DeltaTimeMs: Double);
begin
  { No default implementation }
end;

{ TMZCamera }

function TMZCamera.GetRotationAngle: Single;
begin
  if Assigned(FHandle) then
    Result := FHandle.Angle
  else
    Result := 0;
end;

function TMZCamera.GetXOffset: Single;
begin
  if Assigned(FHandle) then
    Result := FHandle.X
  else
    Result := 0;
end;

function TMZCamera.GetYSOffset: Single;
begin
  if Assigned(FHandle) then
    Result := FHandle.Y
  else
    Result := 0;
end;

function TMZCamera.GetZoomX: Single;
begin
  if Assigned(FHandle) then
    Result := FHandle.Zoom.X
  else
    Result := 0;
end;

function TMZCamera.GetZoomY: Single;
begin
  if Assigned(FHandle) then
    Result := FHandle.Zoom.Y
  else
    Result := 0;
end;

function TMZCamera.GetCenter: TMZPoint;
begin
  if Assigned(FHandle) then
    Result := TMZPoint(FHandle.Center)
  else
    Result := TMZPoint.Create;
end;

function TMZCamera.GetCenterX: Single;
begin
  if Assigned(FHandle) then
    Result := FHandle.Center.X
  else
    Result := 0;
end;

function TMZCamera.GetCenterY: Single;
begin
  if Assigned(FHandle) then
    Result := FHandle.Center.Y
  else
    Result := 0;
end;

procedure TMZCamera.SetCenter(const Value: TMZPoint);
begin
  if Assigned(Handle) then
    FHandle.Center := zglTPoint2D(Value);
end;

procedure TMZCamera.SetCenterX(const Value: Single);
begin
  if Assigned(Handle) then
    FHandle.Center.X := Value;
end;

procedure TMZCamera.SetCenterY(const Value: Single);
begin
  if Assigned(Handle) then
    FHandle.Center.Y := Value;
end;

procedure TMZCamera.SetRotationAngle(const Value: Single);
begin
  if Assigned(Handle) then
    FHandle.Angle := Value;
end;

procedure TMZCamera.SetXOffset(const Value: Single);
begin
  if Assigned(Handle) then
    FHandle.X := Value;
end;

procedure TMZCamera.SetYOffset(const Value: Single);
begin
  if Assigned(Handle) then
    FHandle.Y := Value;
end;

procedure TMZCamera.SetZoomX(const Value: Single);
begin
  if Assigned(Handle) then
    FHandle.Zoom.X := Value;
end;

procedure TMZCamera.SetZoomY(const Value: Single);
begin
  if Assigned(Handle) then
    FHandle.Zoom.Y := Value;
end;

constructor TMZCamera.CreateFromHandle(const Handle: zglPCamera2D);
begin
  inherited Create;
  FHandle := Handle;
end;

constructor TMZCamera.Create;
begin
  inherited Create;
  GetMem(FHandle, SizeOf(zglTCamera2D));
  Reset;
end;

destructor TMZCamera.Destroy;
begin
  FreeMem(FHandle);
  inherited Destroy;
end;

procedure TMZCamera.Reset;
begin
  cam2d_Init(FHandle^);
end;

{ TMZTexture }

function TMZTexture.GetID: Cardinal;
begin
  if Assigned(FHandle) then
    Result := FHandle.ID
  else
    Result := 0;
end;

function TMZTexture.GetHeight: Integer;
begin
  if Assigned(FHandle) then
    Result := FHandle.Height
  else
    Result := 0;
end;

function TMZTexture.GetFormat: TMZTextureFormat;
begin
  if Assigned(FHandle) then
    Result := TMZTextureFormat(FHandle.Format)
  else
    Result := tfRgba;
end;

class function TMZTexture.GetAnisotropyLevel: Integer;
begin
  Result := zgl_Get(GAPI_MAX_ANISOTROPY);
end;

function TMZTexture.GetWidth: Integer;
begin
  if Assigned(FHandle) then
    Result := FHandle.Width
  else
    Result := 0;
end;

class procedure TMZTexture.SetAnisotropyLevel(const Value: Integer);
begin
  tex_SetAnisotropy(Value);
end;

class function TMZTexture.IsZeroTexture(const Handle: zglPTexture): Boolean;
begin
  {$IFDEF USE_ZENGL_STATIC}
  Result := (Handle = managerZeroTexture);
  {$ELSE}
  Result := (Handle.Width = 4) and (Handle.Height = 4) and (Handle.U = 1) and
    (Handle.V = 1) and (Handle.Flags = TEX_DEFAULT_2D) and
    (Handle.Format = TEX_FORMAT_RGBA);
  {$ENDIF}
end;

constructor TMZTexture.Create(const Width, Height: Integer;
  const Color: Cardinal; const Flags: TMZTextureFlags;
  const Filter: TMZTextureFilter; const Wrap: TMZTextureWrap;
  const CustomEffect: TMZTextureEffectProc);
var
  OptionalFlags: Integer;
begin
  inherited Create;
  FFrameRows := 1;
  FFrameColumns := 1;
  if Assigned(CustomEffect) then
  begin
    zgl_Reg(TEX_CURRENT_EFFECT, @CustomEffect);
    OptionalFlags := TEX_CUSTOM_EFFECT;
  end
  else
    OptionalFlags := 0;
  FHandle := tex_CreateZero(Width, Height, Color,
    Byte(Flags) or Ord(Filter) or Ord(Wrap) or OptionalFlags);
  if (FHandle = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_CREATE_TEXTURE);
  FOwnsHandle := True;
end;

constructor TMZTexture.Create(const Filename: UTF8String; const KeyColor: Cardinal;
  const Flags: TMZTextureFlags; const Filter: TMZTextureFilter;
  const Wrap: TMZTextureWrap; const CustomEffect: TMZTextureEffectProc);
var
  OptionalFlags: Integer;
begin
  inherited Create;
  FFrameRows := 1;
  FFrameColumns := 1;
  if Assigned(CustomEffect) then
  begin
    zgl_Reg(TEX_CURRENT_EFFECT, @CustomEffect);
    OptionalFlags := TEX_CUSTOM_EFFECT;
  end
  else
    OptionalFlags := 0;
  FHandle := tex_LoadFromFile(Filename, KeyColor,
    Byte(Flags) or Ord(Filter) or Ord(Wrap) or OptionalFlags);
  if (FHandle = nil) or IsZeroTexture(FHandle) then
  begin
    FHandle := nil;
    raise EMZError.CreateFmt(RS_MZ_ERROR_CANNOT_LOAD_TEXTURE_FILE, [Filename]);
  end;
  FOwnsHandle := True;
end;

constructor TMZTexture.Create(const Stream: TStream; const Extension: UTF8String;
  const KeyColor: Cardinal; const Flags: TMZTextureFlags;
  const Filter: TMZTextureFilter; const Wrap: TMZTextureWrap;
  const CustomEffect: TMZTextureEffectProc);
var
  Memory: zglTMemory;
  OptionalFlags: Integer;
begin
  inherited Create;
  FFrameRows := 1;
  FFrameColumns := 1;
  if Assigned(CustomEffect) then
  begin
    zgl_Reg(TEX_CURRENT_EFFECT, @CustomEffect);
    OptionalFlags := TEX_CUSTOM_EFFECT;
  end
  else
    OptionalFlags := 0;

  Memory.Size := Stream.Size - Stream.Position;
  GetMem(Memory.Memory, Memory.Size);
  try
    Memory.Position := 0;
    Stream.ReadBuffer(Memory.Memory^, Memory.Size);
    FHandle := tex_LoadFromMemory(Memory, Extension, KeyColor,
      Byte(Flags) or Ord(Filter) or Ord(Wrap) or OptionalFlags);
    if (FHandle = nil) or IsZeroTexture(FHandle) then
    begin
      FHandle := nil;
      raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_TEXTURE);
    end;
  finally
    FreeMem(Memory.Memory);
  end;
  FOwnsHandle := True;
end;

constructor TMZTexture.Create(const Buffer: Pointer; const Size: Integer;
  const Extension: UTF8String; const KeyColor: Cardinal;
  const Flags: TMZTextureFlags; const Filter: TMZTextureFilter;
  const Wrap: TMZTextureWrap; const CustomEffect: TMZTextureEffectProc);
var
  Memory: zglTMemory;
  OptionalFlags: Integer;
begin
  inherited Create;
  FFrameRows := 1;
  FFrameColumns := 1;
  if Assigned(CustomEffect) then
  begin
    zgl_Reg(TEX_CURRENT_EFFECT, @CustomEffect);
    OptionalFlags := TEX_CUSTOM_EFFECT;
  end
  else
    OptionalFlags := 0;

  Memory.Memory := Buffer;
  Memory.Size := Size;
  Memory.Position := 0;
  FHandle := tex_LoadFromMemory(Memory, Extension, KeyColor,
    Byte(Flags) or Ord(Filter) or Ord(Wrap) or OptionalFlags);
  if (FHandle = nil) or IsZeroTexture(FHandle) then
  begin
    FHandle := nil;
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_TEXTURE);
  end;
  FOwnsHandle := True;
end;

destructor TMZTexture.Destroy;
begin
  if FOwnsHandle and Assigned(FHandle) then
    tex_Del(FHandle);
  inherited Destroy;
end;

procedure TMZTexture.SetFrameSize(const FrameWidth, FrameHeight: Integer);
begin
  tex_SetFrameSize(FHandle, FrameWidth, FrameHeight);
  FFrameWidth := FrameWidth;
  FFrameHeight := FrameHeight;
  if Assigned(FHandle) then
  begin
    FFrameRows := Round(FHandle.Height) div FrameHeight;
    FFrameColumns := Round(FHandle.Width) div FrameWidth;
  end;
end;

procedure TMZTexture.ApplyMask(const Mask: TMZTexture);
begin
  if Assigned(Mask) then
    tex_SetMask(FHandle, Mask.FHandle);
end;

function TMZTexture.GetData: PByteArray;
begin
  Result := nil;
  tex_GetData(FHandle, Result);
end;

procedure TMZTexture.SetData(const Data: Pointer; const X, Y, Width,
  Height: Integer; const Stride: Integer);
begin
  tex_SetData(FHandle, Data, X, Y, Width, Height, Stride);
end;

procedure TMZTexture.Filter(const Filter: TMZTextureFilter;
  const Wrap: TMZTextureWrap);
begin
  if Assigned(FHandle) then
    tex_Filter(FHandle, Ord(Filter) or Ord(Wrap));
end;

constructor TMZTexture.Create(const Memory: TMZMemory; const Extension: UTF8String;
  const KeyColor: Cardinal; const Flags: TMZTextureFlags;
  const Filter: TMZTextureFilter; const Wrap: TMZTextureWrap;
  const CustomEffect: TMZTextureEffectProc);
var
  OptionalFlags: Integer;
begin
  inherited Create;
  FFrameRows := 1;
  FFrameColumns := 1;
  Assert(Assigned(Memory));
  if Assigned(CustomEffect) then
  begin
    zgl_Reg(TEX_CURRENT_EFFECT, @CustomEffect);
    OptionalFlags := TEX_CUSTOM_EFFECT;
  end
  else
    OptionalFlags := 0;

  FHandle := tex_LoadFromMemory(Memory.FSettings, Extension, KeyColor,
    Byte(Flags) or Ord(Filter) or Ord(Wrap) or OptionalFlags);
  if (FHandle = nil) or IsZeroTexture(FHandle) then
  begin
    FHandle := nil;
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_TEXTURE);
  end;
  FOwnsHandle := True;
end;

constructor TMZTexture.Create(const Handle: zglPTexture);
begin
  inherited Create;
  FHandle := Handle;
  FFrameRows := 1;
  FFrameColumns := 1;
end;

{ TMZMath }

class function TMZMath.Angle(const X1, Y1, X2, Y2: Single): Single;
begin
  Result := m_Angle(X1, Y1, X2, Y2);
end;

class function TMZMath.Cos(const Angle: Integer): Single;
begin
  Result := m_Cos(Angle);
end;

class function TMZMath.Distance(const X1, Y1, X2, Y2: Single): Single;
begin
  Result := m_Distance(X1, Y1, X2, Y2);
end;

function TMZMath.Orientation(const X, Y, X1, Y1, X2,
  Y2: Single): TMZOrientation;
begin
  Result := TMZOrientation(m_Orientation(X, Y, X1, Y1, X2, Y2));
end;

class function TMZMath.Sin(const Angle: Integer): Single;
begin
  Result := m_Sin(Angle);
end;

class function TMZMath.SquaredDistance(const X1, Y1, X2, Y2: Single): Single;
begin
  Result := m_FDistance(X1, Y1, X2, Y2);
end;

{ TMZRect }

class function TMZRect.Create: TMZRect;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.W := 0;
  Result.H := 0;
end;

class function TMZRect.Create(const X, Y, W, H: Single): TMZRect;
begin
  Result.X := X;
  Result.Y := Y;
  Result.W := W;
  Result.H := H;
end;

function TMZRect.ContainsPoint(const Point: TMZPoint): Boolean;
begin
  Result := col2d_PointInRect(Point.X, Point.Y, zglTRect(Self));
end;

function TMZRect.ContainsPoint(const X, Y: Single): Boolean;
begin
  Result := col2d_PointInRect(X, Y, zglTRect(Self));
end;

function TMZRect.IntersectsRect(const Other: TMZRect): Boolean;
begin
  Result := col2d_Rect(zglTRect(Self), zglTRect(Other));
end;

class operator TMZRect.NotEqual(const A, B: TMZRect): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y) or (A.W <> B.W) or (A.H <> B.H);
end;

procedure TMZRect.SetBottom(const Value: Single);
begin
  if (Value >= Y) then
    H := Value - Y;
end;

procedure TMZRect.SetRight(const Value: Single);
begin
  if (Value >= X) then
    W := Value - X;
end;

function TMZRect.IntersectsCircle(const Circle: TMZCircle): Boolean;
begin
  Result := col2d_RectVsCircle(zglTRect(Self), zglTCircle(Circle));
end;

function TMZRect.InsideRect(const OuterRect: TMZRect): Boolean;
begin
  Result := col2d_RectInRect(zglTRect(Self), zglTRect(OuterRect));
end;

function TMZRect.InsideCircle(const Circle: TMZCircle): Boolean;
begin
  Result := col2d_RectInCircle(zglTRect(Self), zglTCircle(Circle));
end;

function TMZRect.ClipToRect(const ClipRect: TMZRect): TMZRect;
begin
  Result := TMZRect(col2d_ClipRect(zglTRect(Self), zglTRect(ClipRect)));
end;

class operator TMZRect.Equal(const A, B: TMZRect): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.W = B.W) and (A.H = B.H);
end;

function TMZRect.Equals(const Other: TMZRect): Boolean;
begin
  Result := SameValue(X, Other.X) and SameValue(Y, Other.Y)
    and SameValue(W, Other.W) and SameValue(H, Other.H);
end;

function TMZRect.GetBottom: Single;
begin
  Result := Y + H;
end;

function TMZRect.GetRight: Single;
begin
  Result := X + W;
end;

{ TMZPoint }

function TMZPoint.AngleTo(const Other: TMZPoint): Single;
begin
  Result := m_Angle(X, Y, Other.X, Other.Y);
end;

function TMZPoint.AngleTo(const X, Y: Single): Single;
begin
  Result := m_Angle(X, Y, Self.X, Self.Y);
end;

class function TMZPoint.Create(const X, Y: Single): TMZPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TMZPoint.DistanceTo(const X, Y: Single): Single;
begin
  Result := m_Distance(X, Y, Self.X, Self.Y);
end;

function TMZPoint.DistanceTo(const Other: TMZPoint): Single;
begin
  Result := m_Distance(X, Y, Other.X, Other.Y);
end;

function TMZPoint.InRect(const X, Y, W, H: Single): Boolean;
var
  R: zglTRect;
begin
  R.X := X;
  R.Y := Y;
  R.W := W;
  R.H := H;
  Result := col2d_PointInRect(Self.X, Self.Y, R);
end;

function TMZPoint.InTriangle(const P1, P2, P3: TMZPoint): Boolean;
begin
  Result := col2d_PointInTriangle(X, Y, zglTPoint2D(P1), zglTPoint2D(P2),
    zglTPoint2D(P3));
end;

class operator TMZPoint.NotEqual(const A, B: TMZPoint): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y);
end;

function TMZPoint.Orientation(const X1, Y1, X2, Y2: Single): TMZOrientation;
begin
  Result := TMZOrientation(m_Orientation(X, Y, X1, Y1, X2, Y2));
end;

function TMZPoint.SquaredDistanceTo(const X, Y: Single): Single;
begin
  Result := m_FDistance(X, Y, Self.X, Self.Y);
end;

function TMZPoint.SquaredDistanceTo(const Other: TMZPoint): Single;
begin
  Result := m_FDistance(X, Y, Other.X, Other.Y);
end;

class function TMZPoint.Create: TMZPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

class operator TMZPoint.Equal(const A, B: TMZPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

function TMZPoint.Equals(const Other: TMZPoint): Boolean;
begin
  Result := SameValue(X, Other.X) and SameValue(Y, Other.Y);
end;

{ TMZTriangle }

function TMZTriangle.ContainsPoint(const Point: TMZPoint): Boolean;
begin
  Result := col2d_PointInTriangle(Point.X, Point.Y, zglTPoint2D(P1),
    zglTPoint2D(P2), zglTPoint2D(P3));
end;

function TMZTriangle.ContainsPoint(const X, Y: Single): Boolean;
begin
  Result := col2d_PointInTriangle(X, Y, zglTPoint2D(P1), zglTPoint2D(P2),
    zglTPoint2D(P3));
end;

class function TMZTriangle.Create(const P1, P2, P3: TMZPoint): TMZTriangle;
begin
  Result.P1 := P1;
  Result.P2 := P2;
  Result.P3 := P3;
end;

class operator TMZTriangle.Equal(const A, B: TMZTriangle): Boolean;
begin
  Result := (A.P1 = B.P1) and (A.P2 = B.P2) and (A.P3 = B.P3);
end;

function TMZTriangle.Equals(const Other: TMZTriangle): Boolean;
begin
  Result := P1.Equals(Other.P1) and P2.Equals(Other.P2) and P3.Equals(Other.P3);
end;

class operator TMZTriangle.NotEqual(const A, B: TMZTriangle): Boolean;
begin
  Result := (A.P1 <> B.P1) or (A.P2 <> B.P2) or (A.P3 <> B.P3);
end;

{ TMZCircle }

class function TMZCircle.Create: TMZCircle;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

class function TMZCircle.Create(const X, Y, Radius: Single): TMZCircle;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Radius := Radius;
end;

class function TMZCircle.Create(const Center: TMZPoint;
  const Radius: Single): TMZCircle;
begin
  Result.X := Center.X;
  Result.Y := Center.Y;
  Result.Radius := Radius;
end;

function TMZCircle.IntersectsCircle(const Other: TMZCircle): Boolean;
begin
  Result := col2d_Circle(zglTCircle(Self), zglTCircle(Other));
end;

class operator TMZCircle.NotEqual(const A, B: TMZCircle): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y) or (A.Radius <> B.Radius);
end;

function TMZCircle.InsideCircle(const OuterCircle: TMZCircle): Boolean;
begin
  Result := col2d_CircleInCircle(zglTCircle(Self), zglTCircle(OuterCircle));
end;

function TMZCircle.InsideRectangle(const X, Y, W, H: Single): Boolean;
var
  R: zglTRect;
begin
  R.X := X;
  R.Y := Y;
  R.W := W;
  R.H := H;
  Result := col2d_CircleInRect(zglTCircle(Self), zglTRect(R));
end;

class operator TMZCircle.Equal(const A, B: TMZCircle): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Radius = B.Radius);
end;

function TMZCircle.Equals(const Other: TMZCircle): Boolean;
begin
  Result := SameValue(X, Other.X) and SameValue(Y, Other.Y)
    and SameValue(Radius, Other.Radius);
end;

{ TMZLine }

class function TMZLine.Create: TMZLine;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

class function TMZLine.Create(const X0, Y0, X1, Y1: Single): TMZLine;
begin
  Result.X0 := X0;
  Result.Y0 := Y0;
  Result.X1 := X1;
  Result.Y1 := Y1;
end;

function TMZLine.Angle: Single;
begin
  Result := m_Angle(X0, Y0, X1, Y1);
end;

class function TMZLine.Create(const P0, P1: TMZPoint): TMZLine;
begin
  Result.X0 := P0.X;
  Result.Y0 := P0.Y;
  Result.X1 := P1.X;
  Result.Y1 := P1.Y;
end;

function TMZLine.IntersectsLine(const Other: TMZLine;
  const Intersection: PMZPoint): Boolean;
begin
  Result := col2d_Line(zglTLine(Self), zglTLine(Other), zglPPoint2D(Intersection));
end;

function TMZLine.IntersectsRectangle(const Rect: TMZRect): Boolean;
begin
  Result := col2d_LineVsRect(zglTLine(Self), zglTRect(Rect));
end;

class operator TMZLine.NotEqual(const A, B: TMZLine): Boolean;
begin
  Result := (A.X0 <> B.X0) or (A.Y0 <> B.Y0) or (A.X1 <> B.X1) or (A.Y1 <> B.Y1);
end;

function TMZLine.Orientation(const P: TMZPoint): TMZOrientation;
begin
  Result := TMZOrientation(m_Orientation(P.X, P.Y, X0, Y0, X1, Y1));
end;

function TMZLine.Orientation(const X, Y: Single): TMZOrientation;
begin
  Result := TMZOrientation(m_Orientation(X, Y, X0, Y0, X1, Y1));
end;

function TMZLine.IntersectsCircle(const Circle: TMZCircle): Boolean;
begin
  Result := col2d_LineVsCircle(zglTLine(Self), zglTCircle(Circle));
end;

class operator TMZLine.Equal(const A, B: TMZLine): Boolean;
begin
  Result := (A.X0 = B.X0) and (A.Y0 = B.Y0) and (A.X1 = B.X1) and (A.Y1 = B.Y1);
end;

function TMZLine.Equals(const Other: TMZLine): Boolean;
begin
  Result := SameValue(X0, Other.X0) and SameValue(Y0, Other.Y0)
    and SameValue(X1, Other.X1) and SameValue(Y1, Other.Y1);
end;

{ TMZMouse }

class function TMZMouse.GetPosition: TMZPoint;
begin
  Result := TMZPoint.Create(mouse_X, mouse_Y);
end;

class function TMZMouse.GetX: Integer;
begin
  Result := mouse_X;
end;

class function TMZMouse.GetY: Integer;
begin
  Result := mouse_Y;
end;

class function TMZMouse.GetDX: Integer;
begin
  Result := mouse_DX;
end;

class function TMZMouse.GetDY: Integer;
begin
  Result := mouse_DY;
end;

class procedure TMZMouse.SetPosition(Value: TMZPoint);
begin
  mouse_Lock(Round(Value.X), Round(Value.Y));
end;

class procedure TMZMouse.SetX(const Value: Integer);
begin
  mouse_Lock(Value, mouse_Y);
end;

class procedure TMZMouse.SetY(const Value: Integer);
begin
  mouse_Lock(mouse_X, Value);
end;

class procedure TMZMouse.ClearState;
begin
  mouse_ClearState;
end;

class function TMZMouse.IsButtonDown(const Button: TMZMouseButton): Boolean;
begin
  Result := mouse_Down(Ord(Button));
end;

class function TMZMouse.IsButtonUp(const Button: TMZMouseButton): Boolean;
begin
  Result := mouse_Up(Ord(Button));
end;

class function TMZMouse.IsButtonClicked(const Button: TMZMouseButton): Boolean;
begin
  Result := mouse_Click(Ord(Button));
end;

class function TMZMouse.IsButtonDoubleClicked(
  const Button: TMZMouseButton): Boolean;
begin
  Result := mouse_DblClick(Ord(Button));
end;

class function TMZMouse.HasWheelScrolledUp: Boolean;
begin
  Result := mouse_Wheel(M_WUP);
end;

class function TMZMouse.HasWheelScrolledDown: Boolean;
begin
  Result := mouse_Wheel(M_WDOWN);
end;

class procedure TMZMouse.GetPos(out X, Y: Integer);
begin
  X := mouse_X;
  Y := mouse_Y;
end;

class procedure TMZMouse.SetPos(const X, Y: Integer);
begin
  mouse_Lock(X, Y);
end;

class procedure TMZMouse.Center;
begin
  mouse_Lock;
end;

{$IF DEFINED(iOS) or DEFINED(ANDROID)}
{ TMZTouch }

class procedure TMZTouch.ClearState;
begin
  touch_ClearState;
end;

class procedure TMZTouch.GetPos(const Finger: Byte; out X, Y: Integer);
begin
  X := touch_X(Finger);
  Y := touch_Y(Finger);
end;

class function TMZTouch.GetPosition(const Finger: Byte): TMZPoint;
begin
  Result.X := touch_X(Finger);
  Result.Y := touch_Y(Finger);
end;

class function TMZTouch.GetX(const Finger: Byte): Integer;
begin
  Result := touch_X(Finger);
end;

class function TMZTouch.GetY(const Finger: Byte): Integer;
begin
  Result := touch_Y(Finger);
end;

class function TMZTouch.IsFingerDoubleTapped(const Finger: Byte): Boolean;
begin
  Result := touch_DblTap(Finger);
end;

class function TMZTouch.IsFingerDown(const Finger: Byte): Boolean;
begin
  Result := touch_Down(Finger);
end;

class function TMZTouch.IsFingerTapped(const Finger: Byte): Boolean;
begin
  Result := touch_Tap(Finger);
end;

class function TMZTouch.IsFingerUp(const Finger: Byte): Boolean;
begin
  Result := touch_Up(Finger);
end;
{$IFEND}

{ TMZCanvas }

class procedure TMZCanvas.SetScaleX(const Value: Single);
begin
  if (Value <> FScaleX) then
  begin
    FScaleX := Value;
    fx2d_SetScale(FScaleX, FScaleY);
  end;
end;

class procedure TMZCanvas.SetScaleY(const Value: Single);
begin
  if (Value <> FScaleY) then
  begin
    FScaleY := Value;
    fx2d_SetScale(FScaleX, FScaleY);
  end;
end;

class procedure TMZCanvas.SetCamera(const Value: TMZCamera);
begin
  if (Value <> FCamera) then
  begin
    FCamera := Value;
    if Assigned(Value) then
      cam2d_Set(Value.Handle)
    else
      cam2d_Set(nil);
  end;
end;

class procedure TMZCanvas.SetRenderTarget(const Value: TMZRenderTarget);
begin
  if (Value <> FRenderTarget) then
  begin
    FRenderTarget := Value;
    if Assigned(Value) then
      rtarget_Set(Value.Handle)
    else
      rtarget_Set(nil);
  end;
end;

class procedure TMZCanvas.SetBlendMode(const Value: TMZBlendMode);
begin
  if (Value <> FBlendMode) then
  begin
    FBlendMode := Value;
    fx_SetBlendMode(Ord(Value), FSeparateAlpha);
  end;
end;

class procedure TMZCanvas.SetSeparateAlpha(const Value: Boolean);
begin
  if (Value <> FSeparateAlpha) then
  begin
    FSeparateAlpha := Value;
    fx_SetBlendMode(Ord(FBlendMode), Value);
  end;
end;

class function TMZCanvas.GetAntiAlias: Boolean;
begin
  Result := ((FFlags and PR2D_SMOOTH) <> 0);
end;

class function TMZCanvas.ReadPixels(const X, Y, W, H: Integer): Pointer;
begin
  Result := nil;
  scr_ReadPixels(Result, X, Y, W, H);
end;

class function TMZCanvas.ReadPixels(const Bounds: TMZRect): Pointer;
begin
  Result := nil;
  scr_ReadPixels(Result, Trunc(Bounds.X), Trunc(Bounds.Y),
    Trunc(Bounds.W), Trunc(Bounds.H));
end;

class procedure TMZCanvas.SetAntiAlias(const Value: Boolean);
begin
  if (Value) then
    FFlags := FFlags or PR2D_SMOOTH
  else
    FFlags := FFlags and (not PR2D_SMOOTH);
end;

class constructor TMZCanvas.ClassCreate;
begin
  FFlags := 0;
  FTextureColor := NO_KEY_COLOR;
  FColorMode := cmMix;
  FBlendMode := bmNormal;
  FSeparateAlpha := True;
  FScaleX := 1;
  FScaleY := 1;
  FCamera := nil;
end;

class procedure TMZCanvas.Clear;
begin
  scr_Clear;
end;

class procedure TMZCanvas.DrawPixel(const X, Y: Single; const Color: Cardinal;
  const Alpha: Byte);
begin
  pr2d_Pixel(X, Y, Color, Alpha);
end;

class procedure TMZCanvas.DrawPixel(const Point: TMZPoint;
  const Color: Cardinal; const Alpha: Byte);
begin
  pr2d_Pixel(Point.X, Point.Y, Color, Alpha);
end;

class procedure TMZCanvas.DrawPolygon(const Points: array of TMZPoint;
  const Closed: Boolean; const Color: Cardinal; const Alpha: Byte);
begin
  DrawPolygon(@Points[0], Length(Points), Closed, Color, Alpha);
end;

class procedure TMZCanvas.DrawPolygon(const Points: PMZPoint;
  const Count: Integer; const Closed: Boolean; const Color: Cardinal;
  const Alpha: Byte);
var
  I: Integer;
  P, N: PMZPoint;
begin
  P := Points;
  N := P;
  Inc(N);
  for I := 0 to Count - 2 do
  begin
    pr2d_Line(P.X, P.Y, N.X, N.Y, Color, Alpha, FFlags);
    P := N;
    Inc(N);
  end;
  if (Closed) then
    pr2d_Line(P.X, P.Y, Points.X, Points.Y, Color, Alpha, FFlags)
end;

class procedure TMZCanvas.DrawLine(const X1, Y1, X2, Y2: Single;
  const Color: Cardinal; const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  pr2d_Line(X1, Y1, X2, Y2, Color, Alpha, Longword(Flags) or FFlags)
end;

class procedure TMZCanvas.DrawLine(const StartPoint, EndPoint: TMZPoint;
  const Color: Cardinal; const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  pr2d_Line(StartPoint.X, StartPoint.Y, EndPoint.X, EndPoint.Y, Color, Alpha,
    Longword(Flags) or FFlags)
end;

class procedure TMZCanvas.FillCircle(const X, Y, Radius: Single;
  const Color: Cardinal; const Alpha: Byte; const Quality: Word);
begin
  pr2d_Circle(X, Y, Radius, Color, Alpha, Quality, FFlags or PR2D_FILL);
end;

class procedure TMZCanvas.FillCircle(const Center: TMZPoint;
  const Radius: Single; const Color: Cardinal; const Alpha: Byte;
  const Quality: Word);
begin
  pr2d_Circle(Center.X, Center.Y, Radius, Color, Alpha, Quality, FFlags or PR2D_FILL);
end;

class procedure TMZCanvas.DrawCircle(const X, Y, Radius: Single;
  const Color: Cardinal; const Alpha: Byte; const Quality: Word);
begin
  pr2d_Circle(X, Y, Radius, Color, Alpha, Quality, FFlags);
end;

class procedure TMZCanvas.DrawCircle(const Center: TMZPoint;
  const Radius: Single; const Color: Cardinal; const Alpha: Byte;
  const Quality: Word);
begin
  pr2d_Circle(Center.X, Center.Y, Radius, Color, Alpha, Quality, FFlags);
end;

class procedure TMZCanvas.FillEllipse(const X, Y, XRadius, YRadius: Single;
  const Color: Cardinal; const Alpha: Byte; const Quality: Word);
begin
  pr2d_Ellipse(X, Y, XRadius, YRadius, Color, Alpha, Quality, FFlags or PR2D_FILL);
end;

class procedure TMZCanvas.FillEllipse(const Center: TMZPoint; const XRadius,
  YRadius: Single; const Color: Cardinal; const Alpha: Byte;
  const Quality: Word);
begin
  pr2d_Ellipse(Center.X, Center.Y, XRadius, YRadius, Color, Alpha, Quality, FFlags or PR2D_FILL);
end;

class procedure TMZCanvas.DrawEllipse(const X, Y, XRadius, YRadius: Single;
  const Color: Cardinal; const Alpha: Byte; const Quality: Word);
begin
  pr2d_Ellipse(X, Y, XRadius, YRadius, Color, Alpha, Quality, FFlags);
end;

class procedure TMZCanvas.DrawEllipse(const Center: TMZPoint; const XRadius,
  YRadius: Single; const Color: Cardinal; const Alpha: Byte;
  const Quality: Word);
begin
  pr2d_Ellipse(Center.X, Center.Y, XRadius, YRadius, Color, Alpha, Quality, FFlags);
end;

class procedure TMZCanvas.FillRect(const X, Y, W, H: Single;
  const Color: Cardinal; const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  pr2d_Rect(X, Y, W, H, Color, Alpha, Longword(Flags) or FFlags or PR2D_FILL);
end;

class procedure TMZCanvas.FillRect(const Rect: TMZRect; const Color: Cardinal;
  const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  pr2d_Rect(Rect.X, Rect.Y, Rect.W, Rect.H, Color, Alpha, Longword(Flags) or FFlags or PR2D_FILL);
end;

class procedure TMZCanvas.FillTriangleList(
  const Triangles: array of TMZTriangle; const Color: Cardinal;
  const Alpha: Byte);
begin
  pr2d_TriList(nil, @Triangles[0], nil, 0, Length(Triangles) * 3 - 1,
    Color, $FF, FFlags or PR2D_FILL);
end;

class procedure TMZCanvas.FillTriangleList(const Triangles: PMZTriangle;
  const Count: Integer; const Color: Cardinal; const Alpha: Byte);
begin
  pr2d_TriList(nil, zglPPoints2D(Triangles), nil, 0, Count * 3 - 1, Color, $FF,
    FFlags or PR2D_FILL);
end;

class procedure TMZCanvas.DrawRect(const X, Y, W, H: Single;
  const Color: Cardinal; const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  pr2d_Rect(X, Y, W, H, Color, Alpha, Longword(Flags) or FFlags);
end;

class procedure TMZCanvas.DrawRect(const Rect: TMZRect; const Color: Cardinal;
  const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  pr2d_Rect(Rect.X, Rect.Y, Rect.W, Rect.H, Color, Alpha, Longword(Flags) or FFlags);
end;

class procedure TMZCanvas.DrawText(const Font: TMZFont; const X, Y: Single;
  const Text: UTF8String; const Flags: TMZTextFlags);
begin
  if Assigned(Font) then
    text_Draw(Font.Handle, X, Y, Text, Word(Flags));
end;

class procedure TMZCanvas.DrawText(const Font: TMZFont; const Point: TMZPoint;
  const Text: UTF8String; const Flags: TMZTextFlags);
begin
  if Assigned(Font) then
    text_Draw(Font.Handle, Point.X, Point.Y, Text, Word(Flags));
end;

class procedure TMZCanvas.DrawText(const Font: TMZFont; const X, Y, Scale,
  Step: Single; const Text: UTF8String; const Alpha: Byte; const Color: Cardinal;
  const Flags: TMZTextFlags);
begin
  if Assigned(Font) then
    text_DrawEx(Font.Handle, X, Y, Scale, Step, Text, Alpha, Color, Word(Flags));
end;

class procedure TMZCanvas.DrawText(const Font: TMZFont; const Point: TMZPoint;
  const Scale, Step: Single; const Text: UTF8String; const Alpha: Byte;
  const Color: Cardinal; const Flags: TMZTextFlags);
begin
  if Assigned(Font) then
    text_DrawEx(Font.Handle, Point.X, Point.Y, Scale, Step, Text, Alpha, Color, Word(Flags));
end;

class procedure TMZCanvas.DrawText(const Font: TMZFont; const Rect: TMZRect;
  const Text: UTF8String; const Flags: TMZTextFlags);
begin
  if Assigned(Font) then
    text_DrawInRect(Font.Handle, zglTRect(Rect), Text, Word(Flags));
end;

class procedure TMZCanvas.DrawText(const Font: TMZFont; const Rect: TMZRect;
  const Scale, Step: Single; const Text: UTF8String; const Alpha: Byte;
  const Color: Cardinal; const Flags: TMZTextFlags);
begin
  if Assigned(Font) then
    text_DrawInRectEx(Font.Handle, zglTRect(Rect), Scale, Step, Text,
      Alpha, Color, Word(Flags));
end;

class function TMZCanvas.CalculateTextWidth(const Font: TMZFont;
  const Text: UTF8String; const Step: Single): Single;
begin
  if Assigned(Font) then
    Result := text_GetWidth(Font.Handle, Text, Step)
  else
    Result := 0;
end;

class function TMZCanvas.CalculateTextHeight(const Font: TMZFont;
  const Width: Single; const Text: UTF8String; const Scale: Single;
  const Step: Single): Single;
begin
  if Assigned(Font) then
    Result := text_GetHeight(Font.Handle, Width, Text, Scale, Step)
  else
    Result := 0;
end;

class procedure TMZCanvas.DrawTexture(const Texture: TMZTexture; const TL, TR,
  BR, BL: TMZPoint; const DX, DY, DW, DH: Single; const RotationAngle: Single;
  const Alpha: Byte; const Flags: TMZEffectFlags);
var
  TexCoords: array [0..3] of zglTPoint2D;
begin
  if Assigned(Texture) then
  begin
    TexCoords[0] := zglTPoint2D(TL);
    TexCoords[1] := zglTPoint2D(TR);
    TexCoords[2] := zglTPoint2D(BR);
    TexCoords[3] := zglTPoint2D(BL);
    texture2d_Draw(Texture.Handle, TexCoords, DX, DY, DW, DH, RotationAngle,
      Alpha, Longword(Flags));
  end;
end;

class procedure TMZCanvas.DrawTexture(const Texture: TMZTexture; const TL, TR,
  BR, BL: TMZPoint; const DR: TMZRect; const RotationAngle: Single;
  const Alpha: Byte; const Flags: TMZEffectFlags);
var
  TexCoords: array [0..3] of zglTPoint2D;
begin
  if Assigned(Texture) then
  begin
    TexCoords[0] := zglTPoint2D(TL);
    TexCoords[1] := zglTPoint2D(TR);
    TexCoords[2] := zglTPoint2D(BR);
    TexCoords[3] := zglTPoint2D(BL);
    texture2d_Draw(Texture.Handle, TexCoords, DR.X, DR.Y, DR.W, DR.H, RotationAngle,
      Alpha, Longword(Flags));
  end;
end;

class procedure TMZCanvas.DrawTexture(const Texture: TMZTexture;
  const TextureCoords: TMZTextureCoordinates; const DX, DY, DW, DH: Single;
  const RotationAngle: Single; const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  if Assigned(Texture) then
    texture2d_Draw(Texture.Handle, TZGLTextureCoordinates(TextureCoords),
      DX, DY, DW, DH, RotationAngle, Alpha, Longword(Flags));
end;

class procedure TMZCanvas.DrawTexture(const Texture: TMZTexture;
  const TextureCoords: TMZTextureCoordinates; const DR: TMZRect;
  const RotationAngle: Single; const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  if Assigned(Texture) then
    texture2d_Draw(Texture.Handle, TZGLTextureCoordinates(TextureCoords),
      DR.X, DR.Y, DR.W, DR.H, RotationAngle, Alpha, Longword(Flags));
end;

class procedure TMZCanvas.DrawTexture(const Texture: TMZTexture;
  const TriangleList, TextureCoords: array of TMZPoint; const StartIndex,
  EndIndex: Integer; const Color: Cardinal; const Alpha: Byte;
  const Flags: TMZEffectFlags);
var
  NativeTexture: zglPTexture;
begin
  Assert(Length(TriangleList) > 0);
  if Assigned(Texture) then
    NativeTexture := Texture.Handle
  else
    NativeTexture := nil;
  pr2d_TriList(NativeTexture, zglPPoints2D(@TriangleList[0]),
    zglPPoints2D(@TextureCoords[0]), StartIndex, EndIndex, Color,
    Alpha, Longword(Flags));
end;

class procedure TMZCanvas.DrawTriangleList(
  const Triangles: array of TMZTriangle; const Color: Cardinal;
  const Alpha: Byte);
begin
  pr2d_TriList(nil, @Triangles[0], nil, 0, Length(Triangles) * 3 - 1,
    Color, $FF, FFlags);
end;

class procedure TMZCanvas.DrawTriangleList(const Triangles: PMZTriangle;
  const Count: Integer; const Color: Cardinal; const Alpha: Byte);
begin
  pr2d_TriList(nil, zglPPoints2D(Triangles), nil, 0, Count * 3 - 1, Color, $FF,
    FFlags);
end;

class procedure TMZCanvas.DrawSprite(const Texture: TMZTexture; const DX, DY,
  DW, DH: Single; const RotationAngle: Single; const Alpha: Byte;
  const Flags: TMZEffectFlags);
begin
  if Assigned(Texture) then
    ssprite2d_Draw(Texture.Handle, DX, DY, DW, DH, RotationAngle, Alpha, Longword(Flags));
end;

class procedure TMZCanvas.DrawSprite(const Texture: TMZTexture;
  const DR: TMZRect; const RotationAngle: Single; const Alpha: Byte;
  const Flags: TMZEffectFlags);
begin
  if Assigned(Texture) then
    ssprite2d_Draw(Texture.Handle, DR.X, DR.Y, DR.W, DR.H, RotationAngle, Alpha, Longword(Flags));
end;

class procedure TMZCanvas.DrawSprite(const Texture: TMZTexture; const SR,
  DR: TMZRect; const RotationAngle: Single; const Alpha: Byte;
  const Flags: TMZEffectFlags);
begin
  if Assigned(Texture) then
    csprite2d_Draw(Texture.Handle, DR.X, DR.Y, DR.W, DR.H, RotationAngle,
      zglTRect(SR), Alpha, Longword(Flags));
end;

class procedure TMZCanvas.DrawSprite(const Texture: TMZTexture;
  const SR: TMZRect; const DX, DY, DW, DH: Single; const RotationAngle: Single;
  const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  if Assigned(Texture) then
    csprite2d_Draw(Texture.Handle, DX, DY, DW, DH, RotationAngle,
      zglTRect(SR), Alpha, Longword(Flags));
end;

class procedure TMZCanvas.DrawSpriteFrame(const Texture: TMZTexture;
  const FrameNum: Integer; const DX, DY, DW, DH: Single;
  const RotationAngle: Single; const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  if Assigned(Texture) then
    asprite2d_Draw(Texture.Handle, DX, DY, DW, DH, RotationAngle, FrameNum,
      Alpha, Longword(Flags));
end;

class procedure TMZCanvas.DrawSpriteFrame(const Texture: TMZTexture;
  const FrameNum: Integer; const DR: TMZRect; const RotationAngle: Single;
  const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  if Assigned(Texture) then
    asprite2d_Draw(Texture.Handle, DR.X, DR.Y, DR.W, DR.H, RotationAngle, FrameNum,
      Alpha, Longword(Flags));
end;

class procedure TMZCanvas.DrawSpriteTiles(const Texture: TMZTexture;
  const Offset: TMZPoint; const Frames: TMZTileFrames; const Alpha: Byte;
  const Flags: TMZEffectFlags);
begin
  DrawSpriteTiles(Texture, Offset.X, Offset.Y, Frames, Alpha, Flags);
end;

class procedure TMZCanvas.DrawSpriteTiles(const Texture: TMZTexture;
  const XOffset, YOffset, TileWidth, TileHeight: Single;
  const Frames: TMZTileFrames; const Alpha: Byte; const Flags: TMZEffectFlags);
var
  Tiles: zglTTiles2D;
begin
  if Assigned(Texture) and (Length(Frames) > 0) and (Length(Frames[0]) > 0) then
  begin
    Tiles.Count.X := Length(Frames);
    Tiles.Count.Y := Length(Frames[0]);
    Tiles.Size.W := TileWidth;
    Tiles.Size.H := TileHeight;
    TMZTileFrames(Tiles.Tiles) := Frames;
    tiles2d_Draw(Texture.Handle, XOffset, YOffset, @Tiles, Alpha, Longword(Flags));
  end;
end;

class procedure TMZCanvas.DrawSpriteTiles(const Texture: TMZTexture;
  const Offset: TMZPoint; const TileWidth, TileHeight: Single;
  const Frames: TMZTileFrames; const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  DrawSpriteTiles(Texture, Offset.X, Offset.Y, TileWidth, TileHeight, Frames, Alpha, Flags);
end;

class procedure TMZCanvas.DrawSpriteTiles(const Texture: TMZTexture;
  const XOffset, YOffset: Single; const Frames: TMZTileFrames;
  const Alpha: Byte; const Flags: TMZEffectFlags);
var
  Tiles: zglTTiles2D;
begin
  if Assigned(Texture) and (Length(Frames) > 0) and (Length(Frames[0]) > 0) then
  begin
    Tiles.Count.X := Length(Frames);
    Tiles.Count.Y := Length(Frames[0]);
    Tiles.Size.W := Texture.Width div Texture.FrameColumns;
    Tiles.Size.H := Texture.Height div Texture.FrameRows;
    TMZTileFrames(Tiles.Tiles) := Frames;
    tiles2d_Draw(Texture.Handle, XOffset, YOffset, @Tiles, Alpha, Longword(Flags));
  end;
end;

class procedure TMZCanvas.SetPerVertexColors(const C1, C2, C3, C4: Cardinal;
  const A1, A2, A3, A4: Byte);
begin
  fx2d_SetVCA(C1, C2, C3, C4, A1, A2, A3, A4);
end;

class procedure TMZCanvas.SetPerVertexColors(const C1, C2: Cardinal; const A1,
  A2: Byte);
begin
  fx2d_SetVCA(C1, C2, 0, 0, A1, A2, 0, 0);
end;

class procedure TMZCanvas.SetScale(const Scale: Single);
begin
  FScaleX := Scale;
  FScaleY := Scale;
  fx2d_SetScale(FScaleX, FScaleY);
end;

class procedure TMZCanvas.SetScale(const ScaleX, ScaleY: Single);
begin
  FScaleX := ScaleX;
  FScaleY := ScaleY;
  fx2d_SetScale(FScaleX, FScaleY);
end;

class procedure TMZCanvas.SetVertexOffsets(const X1, Y1, X2, Y2, X3, Y3, X4,
  Y4: Single);
begin
  fx2d_SetVertexes(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

class procedure TMZCanvas.SetVertexOffsets(const P1, P2, P3, P4: TMZPoint);
begin
  fx2d_SetVertexes(P1.X, P1.Y, P2.X, P2.Y, P3.X, P3.Y, P4.X, P4.Y);
end;

class procedure TMZCanvas.SetColorMask(const R, G, B, A: Boolean);
begin
  fx_SetColorMask(R, G, B, A);
end;

class procedure TMZCanvas.SetPivotPoint(const Pivot: TMZPoint);
begin
  fx2d_SetRotatingPivot(Pivot.X, Pivot.Y);
end;

class procedure TMZCanvas.SetPivotPoint(const X, Y: Single);
begin
  fx2d_SetRotatingPivot(X, Y);
end;

class procedure TMZCanvas.SetTextureColor(const Value: Cardinal);
begin
  if (Value <> FTextureColor) then
  begin
    FTextureColor := Value;
    fx2d_SetColor(Value);
  end;
end;

class procedure TMZCanvas.SetColorMode(const Value: TMZColorMode);
begin
  if (Value <> FColorMode) then
  begin
    FColorMode := Value;
    fx_SetColorMode(Ord(Value));
  end;
end;

class procedure TMZCanvas.BeginBatch;
begin
  batch2d_Begin;
end;

class procedure TMZCanvas.EndBatch;
begin
  batch2d_End;
end;

class procedure TMZCanvas.Flush;
begin
  scr_Flush;
end;

class procedure TMZCanvas.FlushBatch;
begin
  batch2d_Flush;
end;

class procedure TMZCanvas.EnableOption(const Option: TMZCanvasOption);
begin
  zgl_Enable(Ord(Option));
end;

class procedure TMZCanvas.DisableOption(const Option: TMZCanvasOption);
begin
  zgl_Disable(Ord(Option));
end;

{ TMZUtils }

class function TMZUtils.IntToStr(const Value: Integer): UTF8String;
begin
  Result := u_IntToStr(Value);
end;

class procedure TMZUtils.Sleep(const SleepTimeMs: Integer);
begin
  u_Sleep(SleepTimeMs);
end;

class function TMZUtils.StrToBool(const Value: UTF8String): Boolean;
begin
  Result := u_StrToBool(Value);
end;

class function TMZUtils.StrToFloat(const Value: UTF8String): Single;
begin
  Result := u_StrToFloat(Value);
end;

class function TMZUtils.StrToInt(const Value: UTF8String): Integer;
begin
  Result := u_StrToInt(Value);
end;

class function TMZUtils.GetScreenResolutions: TMZScreenResolutionArray;
var
  List: zglPResolutionList;
  I: Integer;
begin
  Result := nil;
  {$HINTS OFF}
  List := Pointer(zgl_get(RESOLUTION_LIST));
  {$HINTS ON}
  if Assigned(List) then
  begin
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
    begin
      Result[I].Width := List.Width[I];
      Result[I].Height := List.Height[I];
    end;
  end;
end;

class function TMZUtils.GetSupportsAutomaticMipmapGeneration: Boolean;
begin
  Result := (zgl_Get(GAPI_CAN_AUTOGEN_MIPMAP) <> 0);
end;

class function TMZUtils.GetSupportsSeparateAlpha: Boolean;
begin
  Result := (zgl_Get(GAPI_CAN_BLEND_SEPARATE) <> 0);
end;

class function TMZUtils.GetZenGLVersion: Integer;
begin
  Result := zgl_Get(ZENGL_VERSION);
end;

class function TMZUtils.GetZenGLVersionDate: UTF8String;
begin
  {$HINTS OFF}
  Result := PAnsiChar(zgl_Get(ZENGL_VERSION_DATE));
  {$HINTS ON}
end;

class function TMZUtils.GetZenGLVersionString: UTF8String;
begin
  {$HINTS OFF}
  Result := PAnsiChar(zgl_Get(ZENGL_VERSION_STRING));
  {$HINTS ON}
end;

class function TMZUtils.FloatToStr(const Value: Single;
  const Digits: Integer): UTF8String;
begin
  Result := u_FloatToStr(Value, Digits);
end;

class function TMZUtils.Format(const Format: UTF8String;
  const Args: array of const): UTF8String;
begin
  {$IFDEF FPC}
  Result := SysUtils.Format(Format, Args);
  {$ELSE}
  Result := UTF8String(AnsiStrings.Format(AnsiString(Format), Args));
  {$ENDIF}
end;

class function TMZUtils.GetHomeDirectory: UTF8String;
begin
  {$HINTS OFF}
  Result := PAnsiChar(zgl_Get(DIRECTORY_HOME));
  {$HINTS ON}
end;

class function TMZUtils.GetMaxAnisotropyLevel: Integer;
begin
  Result := zgl_Get(GAPI_MAX_ANISOTROPY);
end;

class function TMZUtils.GetMaxTextureSize: Integer;
begin
  Result := zgl_Get(GAPI_MAX_TEXTURE_SIZE);
end;

class function TMZUtils.GetMaxTextureUnits: Integer;
begin
  Result := zgl_Get(GAPI_MAX_TEXTURE_UNITS);
end;

class function TMZUtils.GetMondoZenGLVersion: Integer;
begin
  Result := (MONDO_ZENGL_VERSION_MAJOR shl 16)
         or (MONDO_ZENGL_VERSION_MINOR shl 8)
         or (MONDO_ZENGL_VERSION_REVISION);
end;

class function TMZUtils.GetMondoZenGLVersionDate: String;
begin
  Result := MONDO_ZENGL_VERSION_DATE;
end;

class function TMZUtils.GetMondoZenGLVersionString: String;
begin
  Result := MONDO_ZENGL_VERSION;
end;

class function TMZUtils.GetApplicationDirectory: UTF8String;
begin
  {$HINTS OFF}
  Result := PAnsiChar(zgl_Get(DIRECTORY_APPLICATION));
  {$HINTS ON}
end;

class function TMZUtils.BoolToStr(const Value: Boolean): UTF8String;
begin
  Result := u_BoolToStr(Value);
end;

class function TMZUtils.GetDesktopHeight: Integer;
begin
  Result := zgl_Get(DESKTOP_HEIGHT);
end;

class function TMZUtils.GetDesktopWidth: Integer;
begin
  Result := zgl_Get(DESKTOP_WIDTH);
end;

{ TMZJoystick }

class procedure TMZJoystick.Initialize;
var
  I: Integer;
begin
  if (not FInitialized) then
  begin
    FInitialized := True;
    SetLength(FJoysticks, joy_Init);
    for I := 0 to Length(FJoysticks) - 1 do
      FJoysticks[I] := TMZJoystick.Create(I, joy_GetInfo(I));
  end;
end;

class function TMZJoystick.GetJoystickCount: Integer;
begin
  Initialize;
  Result := Length(FJoysticks);
end;

class function TMZJoystick.GetJoystick(const Index: Integer): TMZJoystick;
begin
  Initialize;
  if (Index >= 0) and (Index < Length(FJoysticks)) then
    Result := FJoysticks[Index]
  else
    Result := nil;
end;

function TMZJoystick.GetName: UTF8String;
begin
  if Assigned(FHandle) then
    Result := FHandle.Name
  else
    Result := '';
end;

class function TMZJoystick.GetNullJoystick: TMZJoystick;
begin
  if (FNullJoystick = nil) then
  begin
    Initialize;
    FNullJoystick := TMZJoystick.Create(Length(FJoysticks), nil);
  end;
  Result := FNullJoystick;
end;

function TMZJoystick.GetAxisCount: Integer;
begin
  if Assigned(FHandle) then
    Result := FHandle.Count.Axes
  else
    Result := 0;
end;

function TMZJoystick.GetAxisPos(const Axis: TMZJoystickAxis): Single;
begin
  Result := joy_AxisPos(FID, Ord(Axis));
end;

function TMZJoystick.GetButtonCount: Integer;
begin
  if Assigned(FHandle) then
    Result := FHandle.Count.Buttons
  else
    Result := 0;
end;

function TMZJoystick.GetCaps: TMZJoystickCaps;
begin
  if Assigned(FHandle) then
    Result := TMZJoystickCaps(Byte(FHandle.Caps))
  else
    Result := [];
end;

constructor TMZJoystick.Create(const ID: Byte; const Handle: zglPJoyInfo);
begin
  inherited Create;
  FID := ID;
  FHandle := Handle;
end;

class constructor TMZJoystick.ClassCreate;
begin
  FInitialized := False;
end;

class destructor TMZJoystick.ClassDestroy;
var
  Joystick: TMZJoystick;
begin
  for Joystick in FJoysticks do
    Joystick.Free;
  FJoysticks := nil;
  FNullJoystick.Free;
end;

function TMZJoystick.IsButtonDown(const Index: Integer): Boolean;
begin
  Result := joy_Down(FID, Index);
end;

function TMZJoystick.IsButtonUp(const Index: Integer): Boolean;
begin
  Result := joy_Up(FID, Index);
end;

function TMZJoystick.IsButtonPressed(const Index: Integer): Boolean;
begin
  Result := joy_Press(FID, Index);
end;

class procedure TMZJoystick.ClearState;
begin
  Initialize;
  joy_ClearState;
end;

{ TMZKeyboard }

class procedure TMZKeyboard.ClearState;
begin
  key_ClearState;
end;

class function TMZKeyboard.IsKeyDown(const KeyCode: TMZKeyCode): Boolean;
begin
  Result := key_Down(Ord(KeyCode));
end;

class function TMZKeyboard.IsKeyUp(const KeyCode: TMZKeyCode): Boolean;
begin
  Result := key_Up(Ord(KeyCode));
end;

class function TMZKeyboard.IsKeyPressed(const KeyCode: TMZKeyCode): Boolean;
begin
  Result := key_Press(Ord(KeyCode));
end;

class function TMZKeyboard.LastKeyState(
  const KeyCode: TMZKeyCode): TMZKeyState;
begin
  Result := TMZKeyState(key_Last(Ord(KeyCode)));
end;

class procedure TMZKeyboard.BeginReadText(const InitialText: UTF8String;
  const MaxLength: Integer);
begin
  key_BeginReadText(InitialText, MaxLength);
end;

class function TMZKeyboard.GetText: UTF8String;
begin
  Result := key_GetText;
end;

class procedure TMZKeyboard.EndReadText;
begin
  key_EndReadText;
end;

{ TMZFont }

function TMZFont.GetMaxHeight: Integer;
begin
  if Assigned(FHandle) then
    Result := FHandle.MaxHeight
  else
    Result := 0;
end;

constructor TMZFont.Create(const Filename: UTF8String);
begin
  inherited Create;
  FHandle := font_LoadFromFile(Filename);
  if (FHandle = nil) then
    raise EMZError.CreateFmt(RS_MZ_ERROR_CANNOT_LOAD_FONT_FILE, [Filename]);
end;

constructor TMZFont.Create(const Stream: TStream);
var
  Memory: zglTMemory;
begin
  inherited Create;
  Memory.Size := Stream.Size - Stream.Position;
  GetMem(Memory.Memory, Memory.Size);
  try
    Memory.Position := 0;
    Stream.ReadBuffer(Memory.Memory^, Memory.Size);
    FHandle := font_LoadFromMemory(Memory);
    if (FHandle = nil) then
      raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_FONT);
  finally
    FreeMem(Memory.Memory);
  end;
end;

constructor TMZFont.Create(const Buffer: Pointer; const Size: Integer;
  const Offset: Integer);
var
  Memory: zglTMemory;
begin
  inherited Create;
  Memory.Memory := Buffer;
  Memory.Size := Size;
  Memory.Position := Offset;
  FHandle := font_LoadFromMemory(Memory);
  if (FHandle = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_FONT);
end;

constructor TMZFont.Create(const Memory: TMZMemory);
begin
  Assert(Assigned(Memory));
  inherited Create;
  FHandle := font_LoadFromMemory(Memory.FSettings);
  if (FHandle = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_FONT);
end;

destructor TMZFont.Destroy;
begin
  if Assigned(FHandle) then
    font_Del(FHandle);
  inherited Destroy;
end;

{ TMZScene }

class constructor TMZScene.ClassCreate;
begin
  FCanvas := nil;
end;

procedure TMZScene.Startup;
begin
  { No default implementation }
end;

procedure TMZScene.Shutdown;
begin
  { No default implementation }
end;

procedure TMZScene.RenderFrame;
begin
  { No default implementation }
end;

procedure TMZScene.Update(const DeltaTimeMs: Double);
begin
  { No default implementation }
end;

procedure TMZScene.Activate;
begin
  { No default implementation }
end;

procedure TMZScene.Deactivate;
begin
  { No default implementation }
end;

procedure TMZScene.MouseMove(const X, Y: Integer);
begin
  { No default implementation }
end;

procedure TMZScene.MouseDown(const Button: TMZMouseButton);
begin
  { No default implementation }
end;

procedure TMZScene.MouseUp(const Button: TMZMouseButton);
begin
  { No default implementation }
end;

procedure TMZScene.MouseWheel(const Up: Boolean);
begin
  { No default implementation }
end;

procedure TMZScene.KeyDown(const KeyCode: TMZKeyCode);
begin
  { No default implementation }
end;

procedure TMZScene.KeyUp(const KeyCode: TMZKeyCode);
begin
  { No default implementation }
end;

procedure TMZScene.KeyChar(const Symbol: String);
begin
  { No default implementation }
end;

procedure TMZScene.TouchMove(const Finger, X, Y: Integer);
begin
  { No default implementation }
end;

procedure TMZScene.TouchDown(const Finger: Integer);
begin
  { No default implementation }
end;

procedure TMZScene.TouchUp(const Finger: Integer);
begin
  { No default implementation }
end;

procedure TMZScene.SysLowMemoryWarning;
begin
  { No default implementation }
end;

procedure TMZScene.SysChangeOrientation(
  const Orientation: TMZInterfaceOrientation);
begin
  { No default implementation }
end;

constructor TMZScene.Create;
begin
  Assert(Assigned(TMZApplication.Instance));
  inherited Create;
  FApplication := TMZApplication.Instance;
  FAutoFree := True;
end;

destructor TMZScene.Destroy;
begin
  Shutdown;
  inherited Destroy;
end;

{ TMZApplication }

procedure TMZApplication.SetCaption(const Value: UTF8String);
begin
  FCaption := Value;
  wnd_SetCaption(Value);
end;

class function TMZApplication.GetCurrentRenderFrameRate: Integer;
begin
  Result := zgl_Get(RENDER_FPS);
end;

class function TMZApplication.GetCurrentVideoRamUsage: Integer;
begin
  Result := zgl_Get(RENDER_VRAM_USED);
end;

class function TMZApplication.GetOpenGLContext: THandle;
begin
  {$IFDEF USE_DIRECT3D}
  Result := 0;
  {$ELSE}
  Result := zgl_Get(GAPI_CONTEXT);
  {$ENDIF}
end;

class function TMZApplication.GetViewportBounds: TMZRect;
begin
  Result := TMZRect.Create(
    zgl_Get(VIEWPORT_OFFSET_X), zgl_Get(VIEWPORT_OFFSET_Y),
    zgl_Get(VIEWPORT_WIDTH), zgl_Get(VIEWPORT_HEIGHT));
end;

class function TMZApplication.GetWindowBounds: TMZRect;
begin
  Result := TMZRect.Create(
    zgl_Get(WINDOW_X), zgl_Get(WINDOW_Y),
    zgl_Get(WINDOW_WIDTH), zgl_Get(WINDOW_HEIGHT));
end;

class function TMZApplication.GetWindowHandle: THandle;
begin
  {$IFDEF iOS}
  Result := 0;
  {$ELSE}
  Result := zgl_Get(WINDOW_HANDLE);
  {$ENDIF}
end;

class constructor TMZApplication.ClassCreate;
begin
  FInstance := nil;
end;

procedure TMZApplication.Startup;
begin
  if (not FSubsystemsInitialized) then
  begin
    if (aoUseSound in FOptions) then
      snd_Init;
    FSubsystemsInitialized := True;
  end;

  if Assigned(FCurrentScene) then
  begin
    FCurrentScene.Startup;
    FCurrentScene.FStartedUp := True;
  end;
end;

procedure TMZApplication.Shutdown;
begin
  if Assigned(FCurrentScene) and FCurrentScene.AutoFree then
    FreeAndNil(FCurrentScene);
end;

procedure TMZApplication.RenderFrame;
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.RenderFrame;
end;

procedure TMZApplication.Update(const DeltaTimeMs: Double);
begin
  if Assigned(FNewScene) then
  begin
    if Assigned(FCurrentScene) and FCurrentScene.AutoFree then
      FreeAndNil(FCurrentScene);
    if (not FNewScene.FStartedUp) then
    begin
      FNewScene.Startup;
      FNewScene.FStartedUp := True;
    end;
    FCurrentScene := FNewScene;
    FNewScene := nil;
  end;

  if Assigned(FCurrentScene) then
    FCurrentScene.Update(DeltaTimeMs);
end;

procedure TMZApplication.Activate;
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.Activate;
end;

procedure TMZApplication.Deactivate;
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.Deactivate;
end;

procedure TMZApplication.MouseMove(const X, Y: Integer);
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.MouseMove(X, Y);
end;

procedure TMZApplication.MouseDown(const Button: TMZMouseButton);
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.MouseDown(Button);
end;

procedure TMZApplication.MouseUp(const Button: TMZMouseButton);
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.MouseUp(Button);
end;

procedure TMZApplication.MouseWheel(const Up: Boolean);
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.MouseWheel(Up);
end;

procedure TMZApplication.KeyDown(const KeyCode: TMZKeyCode);
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.KeyDown(KeyCode);
end;

procedure TMZApplication.KeyUp(const KeyCode: TMZKeyCode);
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.KeyUp(KeyCode);
end;

procedure TMZApplication.KeyChar(const Symbol: String);
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.KeyChar(Symbol);
end;

procedure TMZApplication.TouchMove(const Finger, X, Y: Integer);
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.TouchMove(Finger, X, Y);
end;

procedure TMZApplication.TouchDown(const Finger: Integer);
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.TouchDown(Finger);
end;

procedure TMZApplication.TouchUp(const Finger: Integer);
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.TouchUp(Finger);
end;

procedure TMZApplication.SysLowMemoryWarning;
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.SysLowMemoryWarning;
end;

procedure TMZApplication.SysChangeOrientation(
  const Orientation: TMZInterfaceOrientation);
begin
  if Assigned(FCurrentScene) then
    FCurrentScene.SysChangeOrientation(Orientation);
end;

constructor TMZApplication.Create;
begin
  Assert(FInstance = nil, 'You should only create a single TMZApplication instance in your application');
  inherited Create;
  {$IFNDEF USE_ZENGL_STATIC}
  zglLoad(libZenGL);
  {$ENDIF}
  FOptions := DEFAULT_APPLICATION_OPTIONS;
  FScreenWidth := 640;
  FScreenHeight := 480;
  FScreenRefreshRate := REFRESH_RATE_MAXIMUM;
  FInstance := Self;
  zgl_Reg(SYS_EXIT, @SysExit);
end;

destructor TMZApplication.Destroy;
begin
  FInstance := nil;
  inherited Destroy;
end;

procedure TMZApplication.Initialize;
{$IFDEF MEMCHECK}
var
  MemLogFilename: UTF8String;
{$ENDIF}

  procedure SetAppFlag(const Flag: Longword; const Option: TMZApplicationOption);
  begin
    if (Option in FOptions) then
      zgl_Enable(Flag)
    else
      zgl_Disable(Flag);
  end;

begin
  SetAppFlag(APP_USE_LOG, aoEnableLogging);
  SetAppFlag(WND_USE_AUTOCENTER, aoCenterToScreen);
  SetAppFlag(CORRECT_RESOLUTION, aoAspectRatioCorrection);

  {$IFDEF iOS}
  SetAppFlag(SCR_ORIENTATION_PORTRAIT, aoAllowPortraitOrientation);
  SetAppFlag(SCR_ORIENTATION_LANDSCAPE, aoAllowLandscapeOrientation);
  SetAppFlag(SND_ALLOW_BACKGROUND_MUSIC, aoAllowBackgroundMusic);
  {$ENDIF}

  {$IFDEF MEMCHECK}
  if (aoEnableLogging in FOptions) then
  begin
    {$IFDEF DARWIN}
    MemLogFilename := appWorkDir + '../memlog.txt';
    {$ELSE}
    MemLogFilename := 'memlog.txt';
    {$ENDIF}
    file_Remove(MemLogFilename);
    {$IFDEF FPC}
//    SetHeapTraceOutput(MemLogFilename);
    {$ELSE}
    ReportMemoryLeaksOnShutdown := True;
    {$ENDIF}
  end;
  {$ENDIF}

  zgl_Reg(SYS_LOAD, @SysLoad);
  zgl_Reg(SYS_DRAW, @SysDraw);
  zgl_Reg(SYS_UPDATE, @SysUpdate);
  zgl_Reg(SYS_ACTIVATE, @SysActivate);

  {$IFDEF iOS}
  zgl_Reg(SYS_iOS_MEMORY_WARNING, @SysIOSMemoryWarning);
  zgl_Reg(SYS_iOS_CHANGE_ORIENTATION, @SysIOSChangeOrientation);
  {$ENDIF}

  if (aoUseInputEvents in FOptions) then
  begin
    zgl_Reg(INPUT_MOUSE_MOVE, @SysMouseMove);
    zgl_Reg(INPUT_MOUSE_PRESS, @SysMousePress);
    zgl_Reg(INPUT_MOUSE_RELEASE, @SysMouseRelease);
    zgl_Reg(INPUT_MOUSE_WHEEL, @SysMouseWheel);

    zgl_Reg(INPUT_KEY_CHAR, @SysKeyChar);
    zgl_Reg(INPUT_KEY_PRESS, @SysKeyPress);
    zgl_Reg(INPUT_KEY_RELEASE, @SysKeyRelease);

    {$IFDEF iOS}
    zgl_Reg(INPUT_TOUCH_MOVE, @SysTouchMove);
    zgl_Reg(INPUT_TOUCH_PRESS, @SysTouchPress);
    zgl_Reg(INPUT_TOUCH_RELEASE, @SysTouchRelease);
    {$ENDIF}
  end;
  //关闭当丢失焦点自动暂停功能。
  zgl_Disable( APP_USE_AUTOPAUSE );

  wnd_SetCaption(FCaption);

  wnd_ShowCursor(aoShowCursor in FOptions);

  scr_SetOptions(FScreenWidth, FScreenHeight, FScreenRefreshRate,
    (aoFullScreen in FOptions), (aoVSync in FOptions));
end;

procedure TMZApplication.SetOptions(const Value: TMZApplicationOptions);
begin
  if (FOptions <> Value) then
  begin
    if Assigned(FCurrentScene) then
    begin
      { Application is already running. Change to new options. }
      if (FOptions * [aoFullScreen]) <> (Value * [aoFullScreen]) then
        scr_SetOptions(FScreenWidth, FScreenHeight, FScreenRefreshRate,
          (aoFullScreen in Value), (aoVSync in Value));
    end;
    FOptions := Value;
  end;
end;

procedure TMZApplication.SetScene(const Scene: TMZScene);
begin
  Assert(Assigned(Scene));
  if (FCurrentScene = nil) then
  begin
    { This is the first scene. We need to initialize the engine. }
    Initialize;
    FCurrentScene := Scene;
    { This will call FCurrentScene.Startup: }
    zgl_Init(FFullScreenAntiAliasingSamples, FStencilBufferBits);
  end
  else
    { Do not replace current scene immediately. Wait until the next loop. }
    FNewScene := Scene;
end;

class procedure TMZApplication.SetWindowBounds(const Value: TMZRect);
begin
  wnd_SetPos(Trunc(Value.X), Trunc(Value.Y));
  wnd_SetSize(Trunc(Value.W), Trunc(Value.H));
end;

procedure TMZApplication.Quit;
begin
  zgl_Exit;
end;

{ TMZMemory }

constructor TMZMemory.Create;
begin
  inherited Create;
  FHandle := @FSettings;
end;

constructor TMZMemory.Create(const Size: Integer);
begin
  Create;
  mem_SetSize(FSettings, Size);
end;

constructor TMZMemory.Create(const Filename: UTF8String);
begin
  Create;
  mem_LoadFromFile(FSettings, Filename);
  if (FSettings.Memory = nil) then
    raise EMZError.CreateFmt(RS_MZ_ERROR_CANNOT_OPEN_FILE, [Filename]);
end;

destructor TMZMemory.Destroy;
begin
  {$IFDEF USE_ZENGL_STATIC}
  zgl_memory.mem_Free(FSettings);
  {$ELSE}
  zglHeader.mem_Free(FSettings);
  {$ENDIF}
  inherited;
end;

function TMZMemory.Read(var Buffer; const Count: Integer): Integer;
begin
  Result := mem_Read(FSettings, Buffer, Count);
end;

procedure TMZMemory.SaveToFile(const Filename: UTF8String);
begin
  mem_SaveToFile(FSettings, Filename);
end;

function TMZMemory.Seek(const Offset: Integer;
  const Mode: TMZSeekMode): Integer;
begin
  Result := mem_Seek(FSettings, Offset, Ord(Mode));
end;

procedure TMZMemory.SetPosition(const Value: Longword);
begin
  mem_Seek(FSettings, Value, FSM_SET);
end;

procedure TMZMemory.SetSize(const Value: Longword);
begin
  mem_SetSize(FSettings, Value);
end;

function TMZMemory.Write(const Buffer; const Count: Integer): Integer;
begin
  Result := mem_Write(FSettings, Buffer, Count);
end;

{ TMZTriangulator }

{$IFDEF USE_TRIANGULATION}
class procedure TMZTriangulator.AddHole(const Contour: array of TMZPoint);
begin
  tess_AddHole(@Contour[0], 0, Length(Contour) - 1, False);
end;

class procedure TMZTriangulator.AddHole(const Contour: array of TMZPoint;
  const StartIndex, EndIndex: Integer);
begin
  tess_AddHole(@Contour[0], StartIndex, EndIndex, False);
end;

class procedure TMZTriangulator.BeginTriangulation(
  const Contour: array of TMZPoint);
begin
  tess_Triangulate(@Contour[0], 0, Length(Contour) - 1, True);
end;

class procedure TMZTriangulator.BeginTriangulation(
  const Contour: array of TMZPoint; const StartIndex, EndIndex: Integer);
begin
  tess_Triangulate(@Contour[0], StartIndex, EndIndex, True);
end;

class function TMZTriangulator.EndTriangulation: TMZTriangleArray;
var
  Points: zglPPoints2D;
  NumPoints, NumTriangles: Integer;
begin
  Points := nil;
  NumPoints := tess_GetData(Points);
  try
    Assert((NumPoints mod 3) = 0);
    NumTriangles := NumPoints div 3;
    SetLength(Result, NumTriangles);
    Move(Points[0], Result[0], NumPoints * SizeOf(TMZPoint));
  finally
    FreeMem(Points);
  end;
end;
{$ENDIF}

{ TMZDistortionMesh }

constructor TMZDistortionMesh.Create(const RowCount, ColumnCount: Integer);
begin
  inherited Create;
  FHandle := @FSettings;
  FRowCount := RowCount;
  FColumnCount := ColumnCount;
  FSettings.Cols := ColumnCount + 1;
  FSettings.Rows := RowCount + 1;
  SetLength(FSettings.Grid, FSettings.Cols, FSettings.Rows);
end;

function TMZDistortionMesh.GetVertex(const Row, Column: Integer): TMZPoint;
begin
  Assert((Row >= 0) and (Row <= RowCount));
  Assert((Column >= 0) and (Column <= ColumnCount));
  Result := TMZPoint(FSettings.Grid[Column, Row]);
end;

procedure TMZDistortionMesh.Render(const Texture: TMZTexture; const SR: TMZRect;
  const XOffset, YOffset: Single; const Alpha: Byte;
  const Flags: TMZEffectFlags);
begin
  if Assigned(Texture) then
    cgrid2d_Draw(Texture.Handle, XOffset, YOffset, FHandle, zglTRect(SR), Alpha, Longword(Flags));
end;

procedure TMZDistortionMesh.Render(const Texture: TMZTexture;
  const FrameNum: Integer; const XOffset, YOffset: Single; const Alpha: Byte;
  const Flags: TMZEffectFlags);
begin
  if Assigned(Texture) then
    agrid2d_Draw(Texture.Handle, XOffset, YOffset, FHandle, FrameNum, Alpha, Longword(Flags));
end;

procedure TMZDistortionMesh.Render(const Texture: TMZTexture; const XOffset,
  YOffset: Single; const Alpha: Byte; const Flags: TMZEffectFlags);
begin
  if Assigned(Texture) then
    sgrid2d_Draw(Texture.Handle, XOffset, YOffset, FHandle, Alpha, Longword(Flags));
end;

procedure TMZDistortionMesh.SetVertex(const Row, Column: Integer;
  const Value: TMZPoint);
begin
  Assert((Row >= 0) and (Row <= RowCount));
  Assert((Column >= 0) and (Column <= ColumnCount));
  FSettings.Grid[Column, Row] := zglTPoint2D(Value);
end;

{ TMZResourceQueue }

(*class function TMZResourceQueue.PercentageCompleted: Integer;
begin
  Result := res_GetCompleted;
end;

class function TMZResourceQueue.PercentageCompleted(
  const QueueID: Byte): Integer;
begin
  Result := res_GetPercentage(QueueID);
end;

class procedure TMZResourceQueue.Start(const QueueID: Byte);
begin
  res_BeginQueue(QueueID);
end;

class procedure TMZResourceQueue.Stop;
begin
  res_EndQueue;
end;*)

{ TMZVideoStream }

{$IFDEF USE_THEORA}
constructor TMZVideoStream.Create(const Stream: TStream; const Extension: UTF8String);
begin
  inherited Create;
  FMemory.Size := Stream.Size - Stream.Position;
  GetMem(FMemory.Memory, FMemory.Size);
  FOwnsMemory := True;
  Stream.ReadBuffer(FMemory.Memory^, FMemory.Size);
  FHandle := video_OpenMemory(FMemory, Extension);
  if (FHandle = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_VIDEO);
  Initialize;
end;

constructor TMZVideoStream.Create(const Filename: UTF8String);
begin
  inherited Create;
  FHandle := video_OpenFile(Filename);
  if (FHandle = nil) then
    raise EMZError.CreateFmt(RS_MZ_ERROR_CANNOT_LOAD_VIDEO_FILE, [Filename]);
  Initialize;
end;

constructor TMZVideoStream.Create(const Memory: TMZMemory;
  const Extension: UTF8String);
begin
  Assert(Assigned(Memory));
  inherited Create;
  FMemory := Memory.FSettings;
  FHandle := video_OpenMemory(FMemory, Extension);
  if (FHandle = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_VIDEO);
  Initialize;
end;

constructor TMZVideoStream.Create(const Buffer: Pointer; const Size: Integer;
  const Extension: UTF8String);
begin
  inherited Create;
  FMemory.Memory := Buffer;
  FMemory.Size := Size;
  FHandle := video_OpenMemory(FMemory, Extension);
  if (FHandle = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_VIDEO);
  Initialize;
end;

destructor TMZVideoStream.Destroy;
begin
  if Assigned(FHandle) then
    video_Del(FHandle);
  if FOwnsMemory then
    FreeMem(FMemory.Memory);
  FTexture.Free;
  inherited Destroy;
end;

function TMZVideoStream.GetDurationMs: Double;
begin
  if Assigned(FHandle) then
    Result := FHandle.Info.Duration
  else
    Result := 0;
end;

function TMZVideoStream.GetFrameCount: Integer;
begin
  if Assigned(FHandle) then
    Result := FHandle.Info.Frames
  else
    Result := 0;
end;

function TMZVideoStream.GetFrameNumber: Integer;
begin
  if Assigned(FHandle) then
    Result := FHandle.Frame
  else
    Result := 0;
end;

function TMZVideoStream.GetFrameRate: Single;
begin
  if Assigned(FHandle) then
    Result := FHandle.Info.FrameRate
  else
    Result := 0;
end;

function TMZVideoStream.GetHeight: Integer;
begin
  if Assigned(FHandle) then
    Result := FHandle.Info.Height
  else
    Result := 0;
end;

function TMZVideoStream.GetPositionMs: Double;
begin
  if Assigned(FHandle) then
    Result := FHandle.Time
  else
    Result := 0;
end;

function TMZVideoStream.GetWidth: Integer;
begin
  if Assigned(FHandle) then
    Result := FHandle.Info.Width
  else
    Result := 0;
end;

procedure TMZVideoStream.Initialize;
begin
  if (FHandle = nil) or (FHandle.Texture = nil) then
    raise EMZError.Create(RS_MZ_ERROR_CANNOT_LOAD_VIDEO);
  FTexture := TMZTexture.Create(FHandle.Texture);
end;

procedure TMZVideoStream.Seek(const SeekTimeMs: Double);
begin
  video_Seek(FHandle, SeekTimeMs);
end;

procedure TMZVideoStream.Update(const DeltaTimeMs: Double; const Loop: Boolean);
begin
  video_Update(FHandle, DeltaTimeMs, Loop);
end;
{$ENDIF}

end.
