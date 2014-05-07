unit mzTileMaps;

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

{@@3. Tile Maps Extensions
  The mzTileMaps unit is an extension to MondoZenGL to support tile maps. Some
  of the features:
  * Support for maps containing multiple layers.
  * Each layer can use a different tile set image. Each layer uses a fixed tile
    size, but different layers can use different tile sizes. For example, a
    background layer can use large 100x100 pixel tiles, while a foreground layer
    uses smaller 20x20 pixel tiles.
  * Supports parallax scrolling. By making layers with different dimensions, you
    can create parallax scrolling effects. For example, a smaller background
    layer will scroll more slowly than a larger foreground layer.
  * Supports animated tiles.
  * Uses automatic texture management. Multiple tile maps may use the same
    textures for the tile set image. When you load a new tile map that uses a
    same texture as loaded by a previous map, than the currently loaded texture
    will be used, otherwise, the texture will be loaded. This means that you do
    not have to worry about duplicate textures in memory and when to load and
    release textures.
  * The TileMapConverter utility (in the Tools directory) can be used to convert
    tile maps created by Tiled Map Editor or tIDE (see later).

  # Tile Map Classes #
  The main class you will use is <link TMZTileMap>. With this class you can
  load (and save) tile maps and render them. A tile map contains one or more
  <link TMZTileSet>s, which represent the texture containing the tile images.
  A tile map also contains one or more <link TMZTileLayer>s containing the
  map layout. Each layer uses a single <link TMZTileSet> for its artwork.
  Finally, a tile map can (optionally) contain a numer of animated tile
  definitions (<link TMZTileAnimated>). You usually don't use these directly
  as they are managed by the tile map classes.

  # Converting Tile Maps #
  This extension does <b>not</b> come with a map editor. However, you can use
  the supplied TileMapConverter utility (in the Tools directory) to convert
  tile maps created with these two editors:
  * <exref target="http://www.mapeditor.org/">Tiled Map Editor</exref>
  * <exref target="http://tide.codeplex.com/">tIDE (Tilemap Integrated
    Development Environment)</exref>

  TileMapConverter is a command line utility which expects just a single
  parameter:

  <code>TileMapConverter <Filename.tide/tmx></code>

  This will convert the given tile map to a binary "MondoZenGL Tile Map" format
  (with a ".mztm" extension). The converter will create a subdirectory using
  the name of the map (without extension) and will put the converted tile map
  and all its resources (images) into that directory. The converter may
  automatically adjust some images to make them work more efficiently with
  (Mondo)ZenGL (for example, it will remove an Margins and Spacing that can be
  used with the tile map editors).

  The original tile map may use different directories (for example a directory
  for the map file and a different directory for the images). The converter will
  put all (converted) files into a single directory. That makes it easier to
  deploy these files for mobile devices.

  The converter can handle a lot of the features of tIDE and Tiled. However,
  some features are unsupported by this version of the Tile Map Engine.
  Converting files that use these features can result in errors (in case the
  feature makes the conversion impossible) or warnings (the conversion will
  succeed, but not all features may be available).

  The following tIDE (.tide) features are currently not supported:
  * Multiple tile sets per layer (only a single tile set per layer is supported).
  * The tile dimensions of the layer must match the tile dimensions of the
    tile set used for that layer.
  * Blend modes are ignored. tIDE supports normal (Alpha) blending of tiles and
    additive blending. We only support normal blending and ignore any tiles that
    have a different blending mode.
  * Custom properties per tile are ignored.

  The following Tiled (.tmx) features are currently not supported:
  * Multiple tile sets per layer (only a single tile set per layer is supported).
  * Isometric tile maps (only orthogonal tile maps are supported).
  * Tile maps that reference external (TSX) tile sets.
  * \<tileset> elements containing \<tile> elements (usually found in external
    TSX tile sets). These are ignored.
  * GZIP compressed layers (the more common ZLIB compressed layers are
    supported though).
  * Layer opacity. Tiled supports layers with different opacity (0.0 - 1.0).
    Layer opacity is ignored and every layer is rendered with full opacity.
  * Tile flipping. Tiled supports flipping tiles horizontally and/or vertically
    and/or diagonally. Tile flipping attributes are ignored.
  * Object Groups. With Tiled you can add object groups to a layer. These are
    ignored. }

{$INCLUDE 'mz_config.cfg'}

interface

uses
  Classes,
  SysUtils,
  Math,
  {$IFNDEF FPC}
  {$IFDEF USE_ZENGL_STATIC}
  zgl_file,
  zgl_textures,
  {$ELSE}
  zglHeader,
  {$ENDIF}
  {$ENDIF}
  MondoZenGL;

type
  { Summary:
      Exception type used when reading invalid tile maps. }
  EMZTileError = class(Exception);

type
  TMZTileMap = class;
  TMZTileWriter = class;
  TMZTileReader = class;

  { Summary:
      A custom property as used in <link TMZTileProperties> }
  TMZTileProperty = record
    { Summary:
        Name of the property }
    Name: UTF8String;

    { Summary:
        Value of the property }
    Value: UTF8String;
  end;

  { Summary:
      A collection of custom properties. Custom properties can be added to a
      <link TMZTileSet>, <link TMZTileLayer> and <link TMZTileMap>. }
  TMZTileProperties = class
  {$REGION 'Internal Declarations'}
  private
    FItems: array of TMZTileProperty;
    function GetCount: Integer; inline;
  private
    procedure Load(const Count: Integer; const Reader: TMZTileReader);
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Adds a custom property.
      Parameters:
        Name: the name of the property. The name is treated case-sensitively.
        Value: the value of the property. }
    procedure Add(const Name, Value: UTF8String);

    { Summary:
        Returns the property name and value at the given index.
      Parameters:
        Index: the index of the property to get.
        Name: receives the name of the property.
        Value: receives the value of the property. }
    procedure Get(const Index: Integer; out Name, Value: UTF8String); overload;

    { Summary:
        Gets the value of the property with the given name.
      Parameters:
        Name: the name of the property to get. The search is case-sensitive.
      Returns:
        The value of the property, or an empty string if there is no property
        with the given name. }
    function Get(const Name: UTF8String): UTF8String; overload;

    { Summary:
        The number of custom properties. }
    property Count: Integer read GetCount;
  end;

  { Summary:
      Base class for <link TMZTileSet>, <link TMZTileLayer> and
      <link TMZTileMap>. Supports custom properties. }
  TMZTileBase = class
  {$REGION 'Internal Declarations'}
  private
    FProperties: TMZTileProperties;
  {$ENDREGION 'Internal Declarations'}
  public
    destructor Destroy; override;

    { Summary:
        Adds a custom property.
      Parameters:
        Name: the name of the property. The name is treated case-sensitively.
        Value: the value of the property. }
    procedure AddProperty(const Name, Value: UTF8String);

    { Summary:
        The custom properties tied to this object.
      Remarks:
        Will be nil if there are no custom properties. }
    property Properties: TMZTileProperties read FProperties;
  end;

  { Summary:
      Represents a tile set texture containing the tile images.
      A <link TMZTileMap> owns a collection of tile sets, and a
      <link TMZTileLayer> gets its images from a tile set. }
  TMZTileSet = class(TMZTileBase)
  {$REGION 'Internal Declarations'}
  private
    FImageFilename: UTF8String;
    FRowCount: Integer;
    FColumnCount: Integer;
    FTileWidth: Integer;
    FTileHeight: Integer;
    FTexture: TMZTexture; // Reference
  private
    procedure Load(const Reader: TMZTileReader);
    procedure Save(const Writer: TMZTileWriter);
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        The name of the file containing the texture with the tile images. }
    property ImageFilename: UTF8String read FImageFilename write FImageFilename;

    { Summary:
        The number of rows of tiles in the texture image. }
    property RowCount: Integer read FRowCount write FRowCount;

    { Summary:
        The number of columns of tiles in the texture image. }
    property ColumnCount: Integer read FColumnCount write FColumnCount;

    { Summary:
        The width of each tile in the texture image. }
    property TileWidth: Integer read FTileWidth write FTileWidth;

    { Summary:
        The height of each tile in the texture image. }
    property TileHeight: Integer read FTileHeight write FTileHeight;

    { Summary:
        The texture used for the tile set.
      Remarks:
        The tile set does <b>not</b> become owner of the texture. }
    property Texture: TMZTexture read FTexture write FTexture;
  end;

  { Summary:
      An array of <link TMZTileSet> objects. }
  TMZTileSetArray = array of TMZTileSet;

  { Summary:
      A single layer in a <link TMZTileMap> }
  TMZTileLayer = class(TMZTileBase)
  {$REGION 'Internal Declarations'}
  private
    FMap: TMZTileMap; // Reference
    FTileSet: TMZTileSet; // Reference
    FRowCount: Integer;
    FColumnCount: Integer;
    FTileWidth: Integer;
    FTileHeight: Integer;
    FVisible: Boolean;
    FCells: TMZTileFrames;
    function GetHeight: Integer; inline;
    function GetWidth: Integer; inline;
  private
    procedure Load(const Reader: TMZTileReader);
    procedure Save(const Writer: TMZTileWriter);
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a new layer.
      Parameters:
        Map: the map that owns this layer.
      Remarks:
        Never create layer objects yourself. The tile map will create the layer
        when loading a tile map file. If you need to manually add a layer to a
        tile map, then use <link TMZTileMap.AddLayer> instead. }
    constructor Create(const Map: TMZTileMap);

    { Summary:
        Sets the dimensions of the layer (in tiles).
      Parameters:
        ColumnCount: the number of columns of tiles in the layer.
        RowCount: the number of rows of tiles in the layer. }
    procedure SetSize(const ColumnCount, RowCount: Integer);

    { Summary:
        Renders the layer.
      Parameters:
        ViewCenter: the location in the layer that is mapped to the center of
          the screen (in local layer coordinates).
      Remarks:
        There is usually no need to call this method yourself. It is called
        automatically when you render the map (see <link TMZTileMap.Render>) }
    procedure Render(const ViewCenter: TMZPoint);

    { Summary:
        Converts a set of map coordinates to local layer coordinates. If the
        size of layer equals the size of the map, then the coordinates aren't
        changed. If the layer is smaller than the map, then to coordinates
        will be adjusted to take parallax into account.
      Parameters:
        MapPoint: the coordinates in "map space".
      Returns:
        The coordinates in "local layer space". }
    function MapToLocal(const MapPoint: TMZPoint): TMZPoint;

    { Summary:
        A reference to the tile set that is used for the tile images for this
        layer.
      Remarks:
        The layer does not own the tile set. This must be a tile sets that is
        owned by the tile map (see <link TMZTileMap.TileSets>). }
    property TileSet: TMZTileSet read FTileSet write FTileSet;

    { Summary:
        RowCount: the number of rows of tiles in the layer.
      Remarks:
        To change the number of rows, use <link SetSize>. }
    property RowCount: Integer read FRowCount;

    { Summary:
        ColumnCount: the number of columns of tiles in the layer.
      Remarks:
        To change the number of columns, use <link SetSize>. }
    property ColumnCount: Integer read FColumnCount;

    { Summary:
        The width of each tile in the layer. }
    property TileWidth: Integer read FTileWidth write FTileWidth;

    { Summary:
        The height of each tile in the layer. }
    property TileHeight: Integer read FTileHeight write FTileHeight;

    { Summary:
        Width of the layer in pixels. }
    property Width: Integer read GetWidth;

    { Summary:
        Height of the layer in pixels. }
    property Height: Integer read GetHeight;

    { Summary:
        Whether the layer is visible. }
    property Visible: Boolean read FVisible write FVisible;

    { Summary:
        The cells that make up the layer. This is a 2-dimensional array of tile
        numbers (ordered by column first, than by row, as in Cells[Col,Row]).
      Remarks:
        The first tile in the tile set has tile number 1. Tile number 0 is used
        for empty tiles. }
    property Cells: TMZTileFrames read FCells;
  end;

  { Summary:
      An array of <link TMZTileLayer> objects. }
  TMZTileLayerArray = array of TMZTileLayer;

  { Summary:
      A definition of an animated tile. A <link TMZTileMap> can have a
      collection of animated tile definitions. }
  TMZTileAnimated = class
  {$REGION 'Internal Declarations'}
  private
    type
      TMZTileAnimatedRef = record
        Row: Integer;
        Col: Integer;
        Layer: TMZTileLayer;
      end;
  private
    FMap: TMZTileMap; // Reference
    FIndex: Integer;
    FInterval: Integer;
    FCurTime: Double;
    FFrameIndex: Integer;
    FFrames: array of Integer;
    FRefs: array of TMZTileAnimatedRef;
  private
    procedure Load(const Reader: TMZTileReader);
    procedure Save(const Writer: TMZTileWriter);
    procedure Setup;
    procedure Update(const DeltaTimeMs: Double);
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates a new animated tile definition.
      Parameters:
        Map: the map that owns this layer.
        Index: the index of this tile in the set of animated tiles. }
    constructor Create(const Map: TMZTileMap; const Index: Integer);

    { Summary:
        Checks whether this animate tile definition matches another one. Returns
        True of this definition has the same animation interval and frame
        numbers as the other object.
      Parameters:
        Obj: the object to check for equality against this object.
      Returns:
        True if this object represents the same animated tile definition as
        Obj. }
    function Equals(Obj: TObject): Boolean; override;

    { Summary:
        Adds a frame the the definition.
      Parameters:
        Frame: the frame number (1-based) }
    procedure AddFrame(const Frame: Integer);

    { Summary:
        The animation interval (number of milliseconds between frames) }
    property Interval: Integer read FInterval write FInterval;
  end;

  { Summary:
      An array of animated tile definitions. }
  TMZTileAnimatedArray = array of TMZTileAnimated;

  { Summary:
      Main class for working with tile maps. }
  TMZTileMap = class(TMZTileBase)
  {$REGION 'Internal Declarations'}
  private
    type
      TMZTileTexture = class
      private
        FTexture: TMZTexture;
        FUsed: Boolean;
      public
        constructor Create(const Texture: TMZTexture);
        destructor Destroy; override;
      end;
  private
    class var FTextures: TStringList;
  private
    FResourceDirectory: UTF8String;
    FDescription: UTF8String;
    FTileSets: TMZTileSetArray;
    FLayers: TMZTileLayerArray;
    FAnimatedTiles: TMZTileAnimatedArray;
    FWidth: Integer;
    FHeight: Integer;
    FViewCenter: TMZPoint;
    FConstrainView: Boolean;
    procedure SetConstrainView(const Value: Boolean);
    procedure SetViewCenter(const Value: TMZPoint);
    procedure SetViewCenterX(const Value: Single);
    procedure SetViewCenterY(const Value: Single);
  private
    procedure LoadTexturesIfNeeded;
    procedure ConstrainViewToScreen;
  public
    class constructor ClassCreate;
    class destructor ClassDestroy;
  {$ENDREGION 'Internal Declarations'}
  public
    { Summary:
        Creates an empty tile map. }
    constructor Create; overload;

    { Summary:
        Loads a tile map from a file.
      Parameters:
        Filename: the name of the file containing the tile map.
        LoadTextures: (optional) whether to load the textures referenced in the
          tile map. Defaults to True, but you can set it to False if you are
          only interested in the tile map data.
      Remarks:
        Raises an exception if the file cannot be found or is not a valid
        MondoZenGL Tile Map file, or when one of the textures it references
        cannot be found.
        Textures are only loaded if they aren't used by a previously loaded
        map. So if several maps use the same texture, then it is loaded only
        once. }
    constructor Create(const Filename: UTF8String;
      const LoadTextures: Boolean = True); overload;
    destructor Destroy; override;

    { Summary:
        Renders the map using the location specified in the <link ViewCenter>
        property. }
    procedure Render;

    { Summary:
        When the map contains animated tiles, you must call this method during
        each iteration of the main loop to update animations.
      Parameters:
        DeltaTimeMs: the number of milliseconds that has passed since the last
        call to Update. }
    procedure Update(const DeltaTimeMs: Double);

    { Summary:
        Adds a new tile set to the tile map.
      Returns:
        The newly added tile set.
      Remarks:
        You only need this method if you are manually creating tile maps.
        Otherwise, the tile sets will be loaded from the tile map file. }
    function AddTileSet: TMZTileSet;

    { Summary:
        Returns the index of the given tile set in the <link TileSets> array.
      Returns:
        The index of the given tile set, or -1 if not found. }
    function IndexOfTileSet(const TileSet: TMZTileSet): Integer;

    { Summary:
        Adds a new layer to the tile map.
      Returns:
        The newly added layer.
      Remarks:
        You only need this method if you are manually creating tile maps.
        Otherwise, the layers will be loaded from the tile map file. }
    function AddLayer: TMZTileLayer;

    { Summary:
        Adds a new animated tile definition to the tile map.
      Returns:
        The newly added animated tile definition.
      Remarks:
        You only need this method if you are manually creating tile maps.
        Otherwise, the animated tile definitions will be loaded from the tile
        map file. }
    function AddAnimatedTile(const Anim: TMZTileAnimated): Integer;

    { Summary:
        Saves the tile map to a file.
      Parameters:
        Filename: the name of the file to save the tile map to. }
    procedure SaveToFile(const Filename: UTF8String);

    { Summary:
        The description of the tile map. This can be the name of a level, or
        somethings else you want to display to the user for this tile map. }
    property Description: UTF8String read FDescription write FDescription;

    { Summary:
        The tile sets (see <link TMZTileSet>) that are used by this tile map. }
    property TileSets: TMZTileSetArray read FTileSets;

    { Summary:
        The layers (see <link TMZTileLayer>) that make up this tile map. }
    property Layers: TMZTileLayerArray read FLayers;

    { Summary:
        The animated tile definitions (see <link TMZTileAnimated>) that may be
        used for this tile map. }
    property AnimatedTiles: TMZTileAnimatedArray read FAnimatedTiles;

    { Summary:
        Width of the tile map in pixels. This is the width of the largest layer
        in the tile map. }
    property Width: Integer read FWidth;

    { Summary:
        Height of the tile map in pixels. This is the height of the largest layer
        in the tile map. }
    property Height: Integer read FHeight;

    { Summary:
        The location in the map that is mapped to the center of the screen
        when rendering. In other words, this is the position in the map that
        you want to view at the center of the screen.
      Remarks:
        Defaults to the center of the map. You can also set the individual
        coordinates using <link ViewCenterX> and <link ViewCenterY>. See also
        <link ConstrainView> for limitations on setting this property. }
    property ViewCenter: TMZPoint read FViewCenter write SetViewCenter;

    { Summary:
        The X-coordinate of the location in the map that is mapped to the center
        of the screen when rendering. In other words, this is the position in
        the map that you want to view at the center of the screen.
      Remarks:
        Defaults to the center of the map. You can also set both the X- and
        Y-coordinates at once using the <link ViewCenter> property. See also
        <link ConstrainView> for limitations on setting this property. }
    property ViewCenterX: Single read FViewCenter.X write SetViewCenterX;

    { Summary:
        The Y-coordinate of the location in the map that is mapped to the center
        of the screen when rendering. In other words, this is the position in
        the map that you want to view at the center of the screen.
      Remarks:
        Defaults to the center of the map. You can also set both the X- and
        Y-coordinates at once using the <link ViewCenter> property. See also
        <link ConstrainView> for limitations on setting this property.}
    property ViewCenterY: Single read FViewCenter.Y write SetViewCenterY;

    { Summary:
        Whether <link ViewCenter> property is constrained so you cannot scroll
        past the boundaries of the map. Defaults to True.
      Remarks:
        When False, you can set ViewCenter to any value you like. When True, the
        ViewCenter may be adjusted so that any "empty" area around the map is
        never rendered. You should only set this value to True if the map is at
        least as large as the screen, otherwise the results may be
        unpredictable. }
    property ConstrainView: Boolean read FConstrainView write SetConstrainView;
  end;

  { Summary:
      Internal class used for writing tile maps.
    Remarks:
      There is no need to use this class yourself. }
  TMZTileWriter = class
  {$REGION 'Internal Declarations'}
  private
    FFile: TMZFile;
    FTagPos: Integer;
  private
    procedure StartTag(const TagName: UTF8String);
    procedure EndTag;
    procedure WriteInt32(const Value: Integer);
    procedure WriteInt16(const Value: Integer);
    procedure WriteInt8(const Value: Integer);
    procedure WriteBool(const Value: Boolean);
    procedure WriteString(const Value: UTF8String);
    procedure WriteProperties(const Value: TMZTileProperties);

    property Target: TMZFile read FFile;
  public
    constructor Create(const Filename: UTF8String);
    destructor Destroy; override;
  {$ENDREGION 'Internal Declarations'}
  end;

  { Summary:
      Internal class used for reading tile maps.
    Remarks:
      There is no need to use this class yourself. }
  TMZTileReader = class
  {$REGION 'Internal Declarations'}
  private
    FFile: TMZFile;
  private
    function ReadTag(out TagName: UTF8String; out Size: Integer): Boolean;
    function ReadInt32: Integer;
    function ReadInt16: Integer;
    function ReadInt8: Integer;
    function ReadBool: Boolean;
    function ReadString: UTF8String;
    function ReadProperties: TMZTileProperties;

    property Source: TMZFile read FFile;
  public
    constructor Create(const Filename: UTF8String);
    destructor Destroy; override;
  {$ENDREGION 'Internal Declarations'}
  end;

implementation

{ MondoZenGL Tile Map (.mztm) Format
  ==================================
  All Integer values are in Little Endian (Intel) format. The format is tag
  based to allow for future expansion. Each tag has the following format:

  4 Bytes: tagname (4 Ansi characters). If the first letter is uppercase, then
           this tag cannot be ignored. Parsers that do not understand this tag
           should create an error. If the first letter is lowercase, then the
           tag is not crucial and can be ignored by parsers that do not
           understand the tag.
  4 Bytes: the number of bytes of data that follow for this tag (NOT including
           this field and the tagname).

  Each tile map file MUST start with a MZTM tag. The remaining tags must be
  ordered in such a way that tags that reference other tags appear after that
  other tag. For example, a layer references a tile set, so that tile set tag
  must appear before the layer tag. The safest way to store all tags is to use
  this tag order:
  -MZTM
  -TSET
  -ANIM
  -LAYR

  Most data has a fixed size format. Common exceptions are strings and custom
  properties:

  Strings (identified as 'UTF8Str') are stored as UTF8-encoded strings in the
  following format:
  2 Bytes: the size of the string in bytes (NOT necessarily the length of the
           string).
  Data   : the UTF8 characters making up the string.

  Custom properties (identifier as 'Props') are stored in the following format:
  2 Bytes: the number of custom properties. Then, for each property:
           UTF8Str: the name of the property.
           UTF8Str: the value of the property.

  'MZTM' tag
  ----------
  General Tile Map information.

  8 Bytes: common tag header
  4 Bytes: bitstream version ($00010000 for version 1.0). Older parsers should
           reject bitstreams with a higher Major version than their own version.
           (If the Minor version is higher, than that should not break
           compatibility).
  4 Bytes: size of the entire file (to check if it is complete).
  2 Bytes: total number of tile sets in the file. There must be this many
           'TSET' tags in the file.
  2 Bytes: total number of layers in the file. There must be this many 'LAYR'
           tags in the file.
  2 Bytes: total number of animated tile definitions in the file. There must be
           this many 'ANIM' tags in the file.
  UTF8Str: description of the map.
  Props  : custom properties for the map.

  'TSET' tag
  ----------
  Tile set information. A Tile Map file can contain multiple tile sets. Each
  tile set is numbered according to the order of appearance in the file.

  8 Bytes: common tag header
  2 Bytes: number of tile columns in the set.
  2 Bytes: number of tile rows in the set.
  2 Bytes: the width of each tile in pixels.
  2 Bytes: the height of each tile in pixels.
  UTF8Str: relative path to the image file containing the tile images.
  Props  : custom properties for the tile sheet.

  'ANIM' tag
  ----------
  Defines one or more animated tiles. The layers can later reference regular
  tiles or animated tiles. Animated tiles are numbered according to the order of
  appearance in the file.

  8 Bytes: common tag header.
  2 Bytes: the interval between frames in milliseconds.
  2 Bytes: the number of frames in the animation.
  Count*(2 Bytes): the frame numbers in the animation.

  'LAYR' tag
  ----------
  A Tile Map file can contain multiple layers. Layers rendered from first to
  last. That means that the first layer contains the "furthest" background,
  and the last layer is the most foreground layer. Layers have can different
  dimensions to allow for parallax scrolling.

  8 Bytes: common tag header
  2 Bytes: The number of the tile set to use for this layer.
  2 Bytes: width of the layer in number of tiles.
  2 Bytes: height of the layer in number of tiles.
  1 Byte : whether the layer is visible (1) or not (0)
  1 Byte : whether the layer contains animated tiles (1) or not (0)
  1 Byte : number of bytes to use to store each cell (1 or 2, see later)
  Props  : custom properties for the layer

  What follows are the tile numbers that make up the layer. Tile number 0 is
  used to represent blank tiles (which can be common when there are multiple
  layers). The other tile numbers are used to represent normal tiles (positive
  tile numbers) or animated tiles (negative tile numbers). So, the first tile
  in a tile set has number 1, and the first animated tile has number -1.
  Note that the map is stored column-first (top-to-bottom, left-to-right) for
  consistency with the (Mondo)ZenGL DrawSpriteTiles method.

  To make the data a bit more compact, 2 tricks are used:

  1. Zero-run encoding is used to store multiple blank tiles. When tile number
     0 is encountered, an extra byte is read containing the number of blank
     tiles to use (where 0 means 1 blank tile, and 255 means 256 blank tiles).
     If the run is longer than 256, then multipe (0, Count) runs are used.
  2. Tile numbers are stored using 1 or 2 byte signed or unsigned integers using
     the following rules.
     a. If the layer does NOT contain animated tiles, then:
        -1 (unsigned) byte is used if the tile set contains 255 tiles or less.
        -2 (unsigned) bytes are used if the tile contains 256 tiles or more.
     b. If the layer DOES contain animated tiles, then:
        -1 signed byte is used if the tile set contains 127 tiles or less.
        -2 signed bytes are used if the tile set contains 128 tiles or more,
         or if there are 128 or more animated tile definitions.

  The data is decoded until Width x Height tiles numbers have been read. }

const
  MZ_TILEMAP_VERSION = $00010000;
  MZ_TILEMAP_TAG_MZTM = 'MZTM';
  MZ_TILEMAP_TAG_TSET = 'TSET';
  MZ_TILEMAP_TAG_ANIM = 'ANIM';
  MZ_TILEMAP_TAG_LAYR = 'LAYR';

{ TMZTileReader }

constructor TMZTileReader.Create(const Filename: UTF8String);
begin
  inherited Create;
  FFile := TMZFile.Create(Filename, omRead);
end;

destructor TMZTileReader.Destroy;
begin
  FFile.Free;
  inherited Destroy;
end;

function TMZTileReader.ReadTag(out TagName: UTF8String; out
  Size: Integer): Boolean;
var
  Tag: array [0..3] of AnsiChar;
begin
  Size := 0;
  Result := (FFile.Position < (FFile.Size - 8));
  if (Result) then
  begin
    if (FFile.Read(Tag, 4) <> 4) then
    begin
      Result := False;
      Exit;
    end;

    TagName := Tag;

    if (FFile.Read(Size, 4) <> 4) then
      Result := False;
  end;
end;

function TMZTileReader.ReadInt32: Integer;
begin
  FFile.Read(Result, 4);
end;

function TMZTileReader.ReadInt16: Integer;
var
  S: Smallint;
begin
  FFile.Read(S, 2);
  Result := S;
end;

function TMZTileReader.ReadInt8: Integer;
var
  S: Shortint;
begin
  FFile.Read(S, 1);
  Result := S;
end;

function TMZTileReader.ReadBool: Boolean;
begin
  FFile.Read(Result, 1);
end;

function TMZTileReader.ReadString: UTF8String;
var
  Len: Integer;
begin
  Len := ReadInt16;
  SetLength(Result, Len);
  if (Len > 0) then
    FFile.Read(Result[1], Len);
end;

function TMZTileReader.ReadProperties: TMZTileProperties;
var
  Count: Integer;
begin
  Count := ReadInt16;
  if (Count > 0) then
  begin
    Result := TMZTileProperties.Create;
    Result.Load(Count, Self);
  end
  else
    Result := nil;
end;

{ TMZTileWriter }

constructor TMZTileWriter.Create(const Filename: UTF8String);
begin
  inherited Create;
  TMZFile.Create(Filename, omCreate).Free;
  FFile := TMZFile.Create(Filename, omReadWrite);
end;

destructor TMZTileWriter.Destroy;
begin
  FFile.Free;
  inherited Destroy;
end;

procedure TMZTileWriter.StartTag(const TagName: UTF8String);
begin
  Assert(Length(TagName) = 4);
  FFile.Write(TagName[1], 4);
  FFile.Write(TagName[1], 4); { Dummy. Will be overwritten by tag size later }
  FTagPos := FFile.Position;
end;

procedure TMZTileWriter.EndTag;
var
  CurPos, TagSize: Integer;
begin
  CurPos := FFile.Position;
  TagSize := CurPos - FTagPos;
  FFile.Position := FTagPos - 4;
  FFile.Write(TagSize, 4);
  FFile.Position := CurPos;
end;

procedure TMZTileWriter.WriteInt32(const Value: Integer);
begin
  FFile.Write(Value, 4)
end;

procedure TMZTileWriter.WriteInt16(const Value: Integer);
begin
  if (Value < -32768) or (Value > 32767) then
    raise EMZTileError.Create('Integer value too large to write');
  FFile.Write(Value, 2);
end;

procedure TMZTileWriter.WriteInt8(const Value: Integer);
begin
  FFile.Write(Value, 1);
end;

procedure TMZTileWriter.WriteBool(const Value: Boolean);
begin
  FFile.Write(Value, 1);
end;

procedure TMZTileWriter.WriteString(const Value: UTF8String);
var
  Len: Integer;
begin
  Len := Length(Value);
  WriteInt16(Len);
  if (Len > 0) then
    FFile.Write(Value[1], Len);
end;

procedure TMZTileWriter.WriteProperties(const Value: TMZTileProperties);
var
  I: Integer;
  Name, Val: UTF8String;
begin
  if Assigned(Value) then
  begin
    WriteInt16(Value.Count);
    for I := 0 to Value.Count - 1 do
    begin
      Value.Get(I, Name, Val);
      WriteString(Name);
      WriteString(Val);
    end;
  end
  else
    WriteInt16(0);
end;

{ TMZTileSet }

procedure TMZTileSet.Load(const Reader: TMZTileReader);
begin
  FColumnCount := Reader.ReadInt16;
  FRowCount := Reader.ReadInt16;
  FTileWidth := Reader.ReadInt16;
  FTileHeight := Reader.ReadInt16;
  FImageFilename := Reader.ReadString;
  FProperties := Reader.ReadProperties;
end;

procedure TMZTileSet.Save(const Writer: TMZTileWriter);
begin
  Writer.StartTag(MZ_TILEMAP_TAG_TSET);
  Writer.WriteInt16(FColumnCount);
  Writer.WriteInt16(FRowCount);
  Writer.WriteInt16(FTileWidth);
  Writer.WriteInt16(FTileHeight);
  Writer.WriteString(FImageFilename);
  Writer.WriteProperties(FProperties);
  Writer.EndTag;
end;

{ TMZTileAnimated }

procedure TMZTileAnimated.Load(const Reader: TMZTileReader);
var
  I, Count: Integer;
begin
  FInterval := Reader.ReadInt16;
  Count := Reader.ReadInt16;
  SetLength(FFrames, Count);
  for I := 0 to Count - 1 do
    FFrames[I] := Reader.ReadInt16;
end;

procedure TMZTileAnimated.Save(const Writer: TMZTileWriter);
var
  I: Integer;
begin
  Writer.StartTag(MZ_TILEMAP_TAG_ANIM);
  Writer.WriteInt16(FInterval);
  Writer.WriteInt16(Length(FFrames));
  for I := 0 to Length(FFrames) - 1 do
    Writer.WriteInt16(FFrames[I]);
  Writer.EndTag;
end;

procedure TMZTileAnimated.Setup;
var
  FirstFrame, Row, Col, Frame, RefIndex: Integer;
  Layer: TMZTileLayer;
  Cells: TMZTileFrames;
begin
  FCurTime := 0;
  FFrameIndex := 0;
  if (Length(FFrames) > 0) then
  begin
    FirstFrame := FFrames[0];
    Frame := -(FIndex + 1);
    RefIndex := 0;
    for Layer in FMap.Layers do
    begin
      Cells := Layer.Cells;
      for Col := 0 to Layer.ColumnCount - 1 do
      begin
        for Row := 0 to Layer.RowCount - 1 do
        begin
          if (Cells[Col,Row] = Frame) then
          begin
            Cells[Col,Row] := FirstFrame;
            if (RefIndex >= Length(FRefs)) then
              SetLength(FRefs, RefIndex + 16);
            FRefs[RefIndex].Row := Row;
            FRefs[RefIndex].Col := Col;
            FRefs[RefIndex].Layer := Layer;
            Inc(RefIndex);
          end;
        end;
      end;
    end;
    SetLength(FRefs, RefIndex);
  end;
end;

procedure TMZTileAnimated.Update(const DeltaTimeMs: Double);
var
  I, Frame: Integer;
begin
  if (Length(FRefs) > 0) then
  begin
    FCurTime := FCurTime + DeltaTimeMs;
    while (FCurTime >= FInterval) do
    begin
      Inc(FFrameIndex);
      if (FFrameIndex >= Length(FFrames)) then
        FFrameIndex := 0;
      FCurTime := FCurTime - FInterval;
    end;

    Frame := FFrames[FFrameIndex];
    for I := 0 to Length(FRefs) - 1 do
      FRefs[I].Layer.Cells[FRefs[I].Col, FRefs[I].Row] := Frame;
  end;
end;

constructor TMZTileAnimated.Create(const Map: TMZTileMap; const Index: Integer);
begin
  Assert(Assigned(Map));
  inherited Create;
  FMap := Map;
  FIndex := Index;
end;

function TMZTileAnimated.Equals(Obj: TObject): Boolean;
var
  Other: TMZTileAnimated absolute Obj;
  I: Integer;
begin
  Result := inherited Equals(Obj);
  if (not Result) and (Obj is TMZTileAnimated) then
  begin
    Result := (FInterval = Other.FInterval) and (Length(FFrames) = Length(Other.FFrames));
    if (Result) then
    begin
      for I := 0 to Length(FFrames) - 1 do
      begin
        if (FFrames[I] <> Other.FFrames[I]) then
        begin
          Result := False;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TMZTileAnimated.AddFrame(const Frame: Integer);
begin
  SetLength(FFrames, Length(FFrames) + 1);
  FFrames[Length(FFrames) - 1] := Frame;
end;

{ TMZTileLayer }

function TMZTileLayer.GetHeight: Integer;
begin
  Result := FRowCount * FTileHeight;
end;

function TMZTileLayer.GetWidth: Integer;
begin
  Result := FColumnCount * FTileWidth;
end;

procedure TMZTileLayer.Load(const Reader: TMZTileReader);
var
  I, J, V, BytesPerCell: Integer;
  HasAnimatedTiles: Boolean;
begin
  Assert(Assigned(FMap));
  I := Reader.ReadInt16;
  if (I < 0) or (I >= Length(FMap.TileSets)) then
    raise EMZTileError.Create('Invalid tile layer in Tile Map file. Invalid tile set reference.');
  FTileSet := FMap.TileSets[I];
  FTileWidth := FTileSet.TileWidth;
  FTileHeight := FTileSet.TileHeight;
  FColumnCount := Reader.ReadInt16;
  FRowCount := Reader.ReadInt16;
  SetLength(FCells, FColumnCount, FRowCount);
  for I := 0 to FColumnCount - 1 do
    FillChar(FCells[I,0], FRowCount * SizeOf(Integer), 0);
  FVisible := Reader.ReadBool;
  HasAnimatedTiles := Reader.ReadBool;
  BytesPerCell := Reader.ReadInt8;
  FProperties := Reader.ReadProperties;

  I := 0;
  while (I < FColumnCount) do
  begin
    J := 0;
    while (J < FRowCount) do
    begin
      if (HasAnimatedTiles) then
      begin
        if (BytesPerCell = 1) then
          V := Reader.ReadInt8
        else
          V := Reader.ReadInt16;
      end
      else
      begin
        V := 0;
        Reader.Source.Read(V, BytesPerCell);
      end;

      if (V = 0) then
      begin
        { Run }
        Reader.Source.Read(V, 1);
        Inc(J, V);
        while (J >= FRowCount) do
        begin
          Dec(J, FRowCount);
          Inc(I);
        end;
      end
      else
        FCells[I,J] := V;
      Inc(J);
    end;
    Inc(I);
  end;
end;

function TMZTileLayer.MapToLocal(const MapPoint: TMZPoint): TMZPoint;
var
  W, H, W2, H2, Diff: Integer;
  Fraction: Single;
begin
  W := TMZApplication.Instance.ScreenWidth;
  H := TMZApplication.Instance.ScreenHeight;
  W2 := W div 2;
  H2 := H div 2;

  Diff := FMap.Width - W;
  if (Diff = 0) then
    Result.X := MapPoint.X
  else
  begin
    Fraction := (MapPoint.X - W2) / Diff;
    Result.X := (Fraction * (Width - W)) + W2;
  end;

  Diff := FMap.Height - H;
  if (Diff = 0) then
    Result.Y := MapPoint.Y
  else
  begin
    Fraction := (MapPoint.Y - H2) / Diff;
    Result.Y := (Fraction * (Height - H)) + H2;
  end;
end;

procedure TMZTileLayer.Render(const ViewCenter: TMZPoint);
var
  Offset: TMZPoint;
begin
  Offset.X := -(ViewCenter.X - (TMZApplication.Instance.ScreenWidth div 2));
  Offset.Y := -(ViewCenter.Y - (TMZApplication.Instance.ScreenHeight div 2));
  TMZCanvas.DrawSpriteTiles(FTileSet.Texture, Offset, FCells);
end;

procedure TMZTileLayer.Save(const Writer: TMZTileWriter);
var
  I, J, BytesPerCell, Run, Num, TopNum: Integer;
  HasAnimatedTiles: Boolean;

  procedure WriteRun;
  var
    I: Integer;
  begin
    Assert(Run > 0);
    while (Run > 256) do
    begin
      I := 0;
      Writer.Target.Write(I, BytesPerCell);
      I := 255;
      Writer.Target.Write(I, 1);
      Dec(Run, 256);
    end;
    I := 0;
    Writer.Target.Write(I, BytesPerCell);
    I := Run - 1;
    Writer.Target.Write(I, 1);
    Run := 0;
  end;

  procedure WriteCell(const Number: Integer);
  begin
    if (Number = 0) then
      Inc(Run)
    else
    begin
      if (Run > 0) then
        WriteRun;
      Writer.Target.Write(Number, BytesPerCell);
    end;
  end;

begin
  HasAnimatedTiles := False;
  TopNum := 0;
  for I := 0 to FColumnCount - 1 do
  begin
    for J := 0 to FRowCount - 1 do
    begin
      Num := FCells[I,J];
      if (Num < 0) then
      begin
        HasAnimatedTiles := True;
        Num := -Num;
      end;
      if (Num > TopNum) then
        TopNum := Num;
    end;
  end;

  Assert(Assigned(FMap));
  I := FMap.IndexOfTileSet(FTileSet);
  if (I < 0) then
    raise EMZTileError.Create('Invalid tile set in tile layer');

  if (HasAnimatedTiles) then
  begin
    if (TopNum >= 128) then
      BytesPerCell := 2
    else
      BytesPerCell := 1;
  end
  else
  begin
    if (TopNum >= 256) then
      BytesPerCell := 2
    else
      BytesPerCell := 1;
  end;

  Writer.StartTag(MZ_TILEMAP_TAG_LAYR);
  Writer.WriteInt16(I);
  Writer.WriteInt16(FColumnCount);
  Writer.WriteInt16(FRowCount);
  Writer.WriteBool(FVisible);
  Writer.WriteBool(HasAnimatedTiles);
  Writer.WriteInt8(BytesPerCell);
  Writer.WriteProperties(FProperties);

  Run := 0;
  for I := 0 to FColumnCount - 1 do
    for J := 0 to FRowCount - 1 do
      WriteCell(FCells[I,J]);
  if (Run > 0) then
    WriteRun;

  Writer.EndTag;
end;

constructor TMZTileLayer.Create(const Map: TMZTileMap);
begin
  inherited Create;
  FMap := Map;
  FVisible := True;
end;

procedure TMZTileLayer.SetSize(const ColumnCount, RowCount: Integer);
var
  Col: Integer;
begin
  FColumnCount := ColumnCount;
  FRowCount := RowCount;
  SetLength(FCells, FColumnCount, FRowCount);
  for Col := 0 to ColumnCount - 1 do
    FillChar(FCells[Col,0], FRowCount * SizeOf(Integer), 0);
end;

{ TMZTileBase }

destructor TMZTileBase.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

procedure TMZTileBase.AddProperty(const Name, Value: UTF8String);
begin
  if (Name <> '') then
  begin
    if (FProperties = nil) then
      FProperties := TMZTileProperties.Create;
    FProperties.Add(Name, Value);
  end;
end;

{ TMZTileProperties }

function TMZTileProperties.GetCount: Integer;
begin
  Result := Length(FItems);
end;

procedure TMZTileProperties.Load(const Count: Integer;
  const Reader: TMZTileReader);
var
  I: Integer;
begin
  SetLength(FItems, Count);
  for I := 0 to Count - 1 do
  begin
    FItems[I].Name := Reader.ReadString;
    FItems[I].Value := Reader.ReadString;
  end;
end;

procedure TMZTileProperties.Add(const Name, Value: UTF8String);
var
  I: Integer;
begin
  if (Name <> '') then
  begin
    I := Length(FItems);
    SetLength(FItems, I + 1);
    FItems[I].Name := Name;
    FItems[I].Value := Value;
  end;
end;

procedure TMZTileProperties.Get(const Index: Integer; out Name, Value: UTF8String);
begin
  if (Index >= 0) and (Index < Length(FItems)) then
  begin
    Name := FItems[Index].Name;
    Value := FItems[Index].Value;
  end
  else
  begin
    Name := '';
    Value := '';
  end;
end;

function TMZTileProperties.Get(const Name: UTF8String): UTF8String;
var
  I: Integer;
begin
  for I := 0 to Length(FItems) - 1 do
  begin
    if (FItems[I].Name = Name) then
    begin
      Result := FItems[I].Value;
      Exit;
    end;
  end;
  Result := '';
end;

{ TMZTileMap }

constructor TMZTileMap.Create;
begin
  inherited Create;
  FConstrainView := True;
end;

constructor TMZTileMap.Create(const Filename: UTF8String;
  const LoadTextures: Boolean);
var
  Reader: TMZTileReader;
  S, Tag: UTF8String;
  I, Size, TagPos, Count, TileSetIndex, AnimIndex, LayerIndex: Integer;
  FirstTag: Boolean;
  Layer: TMZTileLayer;
  Anim: TMZTileAnimated;
begin
  inherited Create;
  FConstrainView := True;
  FirstTag := True;
  TileSetIndex := 0;
  AnimIndex := 0;
  LayerIndex := 0;

  S := Filename;
  for I := 1 to Length(S) do
  begin
    if (S[I] = '\') then
      S[I] := '/';
  end;
  FResourceDirectory := '';
  for I := Length(S) downto 1 do
  begin
    if (S[I] = '/') then
    begin
      FResourceDirectory := Copy(S, 1, I);
      Break;
    end;
  end;
  Reader := TMZTileReader.Create(S);
  try
    while Reader.ReadTag(Tag, Size) do
    begin
      TagPos := Reader.Source.Position;
      if (FirstTag) then
      begin
        FirstTag := False;
        if (Tag <> MZ_TILEMAP_TAG_MZTM) then
          raise EMZTileError.Create('Tile Map file must start with a MZTM tag');

        if ((Reader.ReadInt32 shr 16) > (MZ_TILEMAP_VERSION shr 16)) then
          raise EMZTileError.Create('Unsupported Tile Map version');

        if (Reader.ReadInt32 <> Reader.Source.Size) then
          raise EMZTileError.Create('Invalid Tile Map file');

        Count := Reader.ReadInt16;
        Assert(Count >= 0);
        SetLength(FTileSets, Count);

        Count := Reader.ReadInt16;
        Assert(Count >= 0);
        SetLength(FLayers, Count);

        Count := Reader.ReadInt16;
        Assert(Count >= 0);
        SetLength(FAnimatedTiles, Count);

        FDescription := Reader.ReadString;
        FProperties := Reader.ReadProperties;
      end
      else if (Tag = MZ_TILEMAP_TAG_TSET) then
      begin
        if (TileSetIndex >= Length(FTileSets)) then
          raise EMZTileError.Create('Invalid Tile Map file. There are more tile sets than specified in the header.');
        FTileSets[TileSetIndex] := TMZTileSet.Create;
        FTileSets[TileSetIndex].Load(Reader);
        Inc(TileSetIndex);
      end
      else if (Tag = MZ_TILEMAP_TAG_ANIM) then
      begin
        if (AnimIndex >= Length(FAnimatedTiles)) then
          raise EMZTileError.Create('Invalid Tile Map file. There are more animated tiles than specified in the header.');
        Anim := TMZTileAnimated.Create(Self, AnimIndex);
        FAnimatedTiles[AnimIndex] := Anim;
        Anim.Load(Reader);
        Inc(AnimIndex);
      end
      else if (Tag = MZ_TILEMAP_TAG_LAYR) then
      begin
        if (LayerIndex >= Length(FLayers)) then
          raise EMZTileError.Create('Invalid Tile Map file. There are more layers than specified in the header.');
        Layer := TMZTileLayer.Create(Self);
        FLayers[LayerIndex] := Layer;
        Layer.Load(Reader);
        FWidth := Max(FWidth, Layer.Width);
        FHeight := Max(FHeight, Layer.Height);
        Inc(LayerIndex);
      end
      else if (not (Tag[1] in ['A'..'Z'])) then
        raise EMZTileError.Create('Unsupported tag in Tile Map file');

      Reader.Source.Position := TagPos + Size;
    end;
  finally
    Reader.Free;
  end;

  if LoadTextures then
    LoadTexturesIfNeeded;

  for Anim in FAnimatedTiles do
    Anim.Setup;

  FViewCenter.X := 0.5 * FWidth;
  FViewCenter.Y := 0.5 * FHeight;
end;

class constructor TMZTileMap.ClassCreate;
begin
  FTextures := TStringList.Create;
  FTextures.Duplicates := dupError;
  FTextures.Sorted := True;
  FTextures.CaseSensitive := False;
  FTextures.OwnsObjects := True;
end;

class destructor TMZTileMap.ClassDestroy;
var
  I: Integer;
begin
  { At this point, ZenGL has already destroyed the texture handles. We still
    need to destroy the TMZTexture objects though, but not release the handles
    they encapsulate. }
  if Assigned(FTextures) then
  begin
    for I := 0 to FTextures.Count - 1 do
      TMZTileTexture(FTextures.Objects[I]).FTexture.Handle := nil;
    FTextures.Free;
  end;
end;

procedure TMZTileMap.ConstrainViewToScreen;
var
  W, H: Integer;
begin
  W := TMZApplication.Instance.ScreenWidth div 2;
  H := TMZApplication.Instance.ScreenHeight div 2;
  FViewCenter.X := EnsureRange(FViewCenter.X, W, FWidth - W);
  FViewCenter.Y := EnsureRange(FViewCenter.Y, H, FHeight - H);
end;

destructor TMZTileMap.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FTileSets) - 1 do
    FTileSets[I].Free;
  FTileSets := nil;

  for I := 0 to Length(FLayers) - 1 do
    FLayers[I].Free;
  FLayers := nil;

  for I := 0 to Length(FAnimatedTiles) - 1 do
    FAnimatedTiles[I].Free;
  FAnimatedTiles := nil;

  inherited Destroy;
end;

function TMZTileMap.AddTileSet: TMZTileSet;
begin
  Result := TMZTileSet.Create;
  SetLength(FTileSets, Length(FTileSets) + 1);
  FTileSets[Length(FTileSets) - 1] := Result;
end;

function TMZTileMap.IndexOfTileSet(const TileSet: TMZTileSet): Integer;
begin
  for Result := 0 to Length(FTileSets) - 1 do
  begin
    if (FTileSets[Result] = TileSet) then
      Exit;
  end;
  Result := -1;
end;

procedure TMZTileMap.LoadTexturesIfNeeded;
var
  TileSet: TMZTileSet;
  I: Integer;
  Texture: TMZTexture;
  TileTexture: TMZTileTexture;
begin
  { First, check which textures are already loaded }
  for I := 0 to FTextures.Count - 1 do
    TMZTileTexture(FTextures.Objects[I]).FUsed := False;

  for TileSet in FTileSets do
  begin
    I := FTextures.IndexOf(String(TileSet.ImageFilename));
    if (I >= 0) then
      TMZTileTexture(FTextures.Objects[I]).FUsed := True;
  end;

  { Free all textures that aren't used anymore before loading any new ones.
    This reduces the memory footprint. }
  for I := FTextures.Count - 1 downto 0 do
  begin
    if (not TMZTileTexture(FTextures.Objects[I]).FUsed) then
      FTextures.Delete(I);
  end;

  { Now, load the new textures. Note that multiple tilesets within the same
    tilemap can still use the same texture. So we check for duplicates again. }
  for TileSet in FTileSets do
  begin
    I := FTextures.IndexOf(String(TileSet.ImageFilename));
    if (I < 0) then
    begin
      Texture := TMZTexture.Create(FResourceDirectory + TileSet.ImageFilename);
      Texture.SetFrameSize(TileSet.TileWidth, TileSet.TileHeight);
      TileTexture := TMZTileTexture.Create(Texture);
      FTextures.AddObject(String(TileSet.ImageFilename), TileTexture);
    end
    else
      Texture := TMZTileTexture(FTextures.Objects[I]).FTexture;

    TileSet.Texture := Texture;
  end;
end;

procedure TMZTileMap.Render;
var
  Layer: TMZTileLayer;
  P: TMZPoint;
begin
  for Layer in FLayers do
  begin
    if Layer.Visible then
    begin
      P := Layer.MapToLocal(FViewCenter);
      Layer.Render(P);
    end;
  end;
end;

function TMZTileMap.AddLayer: TMZTileLayer;
begin
  Result := TMZTileLayer.Create(Self);
  SetLength(FLayers, Length(FLayers) + 1);
  FLayers[Length(FLayers) - 1] := Result;
end;

function TMZTileMap.AddAnimatedTile(const Anim: TMZTileAnimated): Integer;
begin
  Result := Length(FAnimatedTiles);
  SetLength(FAnimatedTiles, Result + 1);
  FAnimatedTiles[Result] := Anim;
end;

procedure TMZTileMap.SaveToFile(const Filename: UTF8String);
var
  Writer: TMZTileWriter;
  I, FileSizePos, FileSize: Integer;
begin
  Writer := TMZTileWriter.Create(Filename);
  try
    Writer.StartTag(MZ_TILEMAP_TAG_MZTM);
    Writer.WriteInt32(MZ_TILEMAP_VERSION);
    FileSizePos := Writer.Target.Position;
    Writer.WriteInt32(0); { File size. Will be overwritten later. }
    Writer.WriteInt16(Length(FTileSets));
    Writer.WriteInt16(Length(FLayers));
    Writer.WriteInt16(Length(FAnimatedTiles));
    Writer.WriteString(FDescription);
    Writer.WriteProperties(FProperties);
    Writer.EndTag;

    for I := 0 to Length(FTileSets) - 1 do
      FTileSets[I].Save(Writer);

    for I := 0 to Length(FAnimatedTiles) - 1 do
      FAnimatedTiles[I].Save(Writer);

    for I := 0 to Length(FLayers) - 1 do
      FLayers[I].Save(Writer);

    { Update file size }
    FileSize := Writer.Target.Position;
    Writer.Target.Position := FileSizePos;
    Writer.WriteInt32(FileSize);
  finally
    Writer.Free;
  end;
end;

procedure TMZTileMap.SetConstrainView(const Value: Boolean);
begin
  if (Value <> FConstrainView) then
  begin
    FConstrainView := Value;
    if Value then
      ConstrainViewToScreen;
  end;
end;

procedure TMZTileMap.SetViewCenter(const Value: TMZPoint);
begin
  if (Value <> FViewCenter) then
  begin
    FViewCenter := Value;
    if FConstrainView then
      ConstrainViewToScreen;
  end;
end;

procedure TMZTileMap.SetViewCenterX(const Value: Single);
begin
  if (Value <> FViewCenter.X) then
  begin
    FViewCenter.X := Value;
    if FConstrainView then
      ConstrainViewToScreen;
  end;
end;

procedure TMZTileMap.SetViewCenterY(const Value: Single);
begin
  if (Value <> FViewCenter.Y) then
  begin
    FViewCenter.Y := Value;
    if FConstrainView then
      ConstrainViewToScreen;
  end;
end;

procedure TMZTileMap.Update(const DeltaTimeMs: Double);
var
  Anim: TMZTileAnimated;
begin
  for Anim in FAnimatedTiles do
    Anim.Update(DeltaTimeMs);
end;

{ TMZTileMap.TMZTileTexture }

constructor TMZTileMap.TMZTileTexture.Create(const Texture: TMZTexture);
begin
  inherited Create;
  FTexture := Texture;
  FUsed := True;
end;

destructor TMZTileMap.TMZTileTexture.Destroy;
begin
  FTexture.Free;
  inherited;
end;

end.

