{---------------------------------------}
{------- Graphical User Interface ------}
{---------------------------------------}
{                                       }
{ version:  0.1a                        }
{ date:     02.10.2012                  }
{                                       }
{ component palette:                    }
{                                       }
{   BUTTON, CHECKBOX, COMBOBOX, EDIT,   }
{   LABEL, IMAGE, IMAGEBUTTON, TABLE,   }
{   MULTIBUTTON, PROGRESSBAR, TRACKBAR, }
{   RADIOBUTTON, LIST, MAINMENU,        }
{   POPUPMENU, PAGECONTROL              }
{                                       }
{------------ developed by: ------------}
{                                       }
{              Desertkun                }
{                                       }
{                email:                 }
{         desertkun@gmail.com           }
{                                       }
{---------------------------------------}

{$M+}

unit zglGui;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$IF defined(WIN32) or defined(WIN64)}
  {$DEFINE WIN}
{$IFEND}

interface

uses
{$IFDEF STATIC}
  zgl_main,
  zgl_text,
  zgl_window,
  zgl_screen,
  zgl_utils,
  zgl_math_2d,
  zgl_font,
  zgl_textures,
  zgl_render,
  zgl_grid_2d,
  zgl_mouse,
  zgl_collision_2d,
  zgl_primitives_2d,
  zgl_keyboard,
  zgl_sprite_2d,
  zgl_file,
  zgl_log,
  zgl_ini,
  zgl_textures_tga,
  zgl_fx,
  zgl_render_target,
  zgl_render_2d,
{$ELSE}
  zglHeader,
{$ENDIF}
{$IFDEF FPC}
{$IFDEF GUI_USE_EDITOR}
  LCLClasses,
{$ENDIF}
{$ENDIF}
{$IFDEF WIN}
  Windows,
{$ENDIF}
  Classes;

{$IF Defined(GUI_USE_ALL)
  or Defined(GUI_USE_BASIC)
  or Defined(GUI_USE_ADDITIONAL)
  or Defined(GUI_USE_LISTGROUP)
  or Defined(GUI_USE_MENU)
  or Defined(GUI_USE_PAGE)
  or Defined(GUI_SHUT_THE_FUCK_UP)}
    {$DEFINE GUI_USE_FACTOR}
{$IFEND}

{$IFNDEF GUI_USE_FACTOR}
  {$MESSAGE Warn 'please specify which components you want to use, now used all (see the comment below)'}
  // define in compiler such variables:
  // GUI_USE_ALL for all, or
  //   GUI_USE_BASIC -> for button, checkbox, edit, label
  //   GUI_USE_ADDITIONAL -> for image, imagebutton, multibutton, progressbar, radiobutton, trackbar
  //   GUI_USE_LISTGROUP -> for list, table
  //   GUI_USE_PAGE -> for pagecontrol
  //   GUI_SHUT_THE_FUCK_UP -> use nothing, but don't warn too (to define by yourself)
  {$IFNDEF GUI_SHUT_THE_FUCK_UP}
    {$DEFINE GUI_USE_ALL}
  {$ENDIF}
{$ENDIF}

{$IFDEF GUI_USE_ALL}
  {$DEFINE GUI_USE_BASIC}
  {$DEFINE GUI_USE_ADDITIONAL}
  {$DEFINE GUI_USE_LISTGROUP}
  {$DEFINE GUI_USE_MENU}
  {$DEFINE GUI_USE_PAGE}
{$ENDIF}

{$IFDEF GUI_USE_BASIC}
  {$DEFINE GUI_USE_BUTTON}
  {$DEFINE GUI_USE_CHECKBOX}
  {$DEFINE GUI_USE_COMBOBOX}
  {$DEFINE GUI_USE_EDIT}
  {$DEFINE GUI_USE_LABEL}
{$ENDIF}

{$IFDEF GUI_USE_ADDITIONAL}
  {$DEFINE GUI_USE_IMAGE}
  {$DEFINE GUI_USE_IMAGEBUTTON}
  {$DEFINE GUI_USE_MULTIBUTTON}
  {$DEFINE GUI_USE_PROGRESSBAR}
  {$DEFINE GUI_USE_RADIOBUTTON}
  {$DEFINE GUI_USE_TRACKBAR}
{$ENDIF}

{$IFDEF GUI_USE_LISTGROUP}
  {$DEFINE GUI_USE_LIST}
  {$DEFINE GUI_USE_TABLE}
{$ENDIF}

{$IFDEF GUI_USE_MENU}
  {$DEFINE GUI_USE_MAINMENU}
  {$DEFINE GUI_USE_POPUPMENU}
{$ENDIF}

{$IFDEF GUI_USE_PAGE}
  {$DEFINE GUI_USE_PAGECONTROL}
{$ENDIF}

const
  TEXT_CENTER = TEXT_HALIGN_CENTER or TEXT_VALIGN_CENTER;
  TEXT_LEFT = TEXT_HALIGN_LEFT or TEXT_VALIGN_CENTER;
  TEXT_RIGHT = TEXT_HALIGN_RIGHT or TEXT_VALIGN_CENTER;

  GUI_VERSION = '0.1a';

  // SCROLL
  SCROLL_NONE = 0;
  SCROLL_LEFT = 1;
  SCROLL_TOP  = 2;
  SCROLL_BOTH = 3;

  // STATES
  STATE_DEFAULT = 0;

  // mouse
  MOUSE_NONE = 0;
  MOUSE_POINTER = 1;
  MOUSE_EDIT = 2;

const
  SKINTEX_COUNT = 31;
  SKINCFG_COUNT = 6;

type
  TCaption = UTF8String;
  TFontAlign = (
    faTopLeft, faTopCenter, faTopRight, faMiddleLeft, faMiddleCenter,
    faMiddleRight, faBottomLeft, faBottomCenter, faBottomRight);

var
  FontAlignes: array [faTopLeft .. faBottomRight] of Integer = (
    TEXT_HALIGN_LEFT or TEXT_VALIGN_TOP,          // faTopLeft
    TEXT_HALIGN_CENTER or TEXT_VALIGN_TOP,        // faTopCenter
    TEXT_HALIGN_RIGHT or TEXT_VALIGN_TOP,         // faTopRight
    TEXT_HALIGN_LEFT or TEXT_VALIGN_CENTER,       // faMiddleLeft
    TEXT_HALIGN_CENTER or TEXT_VALIGN_CENTER,     // faMiddleCenter
    TEXT_HALIGN_RIGHT or TEXT_VALIGN_CENTER,      // faMiddleRight
    TEXT_HALIGN_LEFT or TEXT_VALIGN_BOTTOM,       // faBottomLeft
    TEXT_HALIGN_CENTER or TEXT_VALIGN_BOTTOM,     // faBottomCenter
    TEXT_HALIGN_RIGHT or TEXT_VALIGN_BOTTOM       // faBottomRight
  );

  SkinTextures: array [0.. SKINTEX_COUNT - 1] of TCaption =
   ('Button', 'MultiButton', 'CheckBox', 'CheckItem', 'CloseBtn', 'ComboBox',
    'ComboBoxButton', 'ComboBoxDrop', 'ComboBoxScroll', 'Edit', 'EditPoint',
    'Form', 'FormResize', 'List', 'ListItem', 'MainMenu', 'MainMenuItem',
    'PageControl', 'PageControlSheet', 'PopupMenu', 'PopupMenuItem',
    'ProgressBar', 'ProgressBarProgress', 'RadioButton', 'ScrollBottom',
    'ScrollRight', 'TrackBarSlider', 'Table', 'TableDelimiter', 'TableItem',
    'Mouse');
  SkinConfigs: array[0.. SKINCFG_COUNT - 1] of TCaption =
    ('EditLine', 'MainMenuLine', 'TableGrid', 'TreeColor', 'TreeWidth',
    'SelectionColor');

type
  TSkinTextures = (skinButton, skinMultiButton, skinCheckBox, skinCheckItem,
    skinCloseBtn, skinComboBox, skinComboBoxButton, skinComboBoxDrop,
    skinComboBoxScroll, skinEdit, skinEditPoint, skinForm, skinFormResize,
    skinList, skinListItem, skinMainMenu, skinMainMenuItem, skinPageControl,
    skinPageControlSheet, skinPopupMenu, skinPopupMenuItem, skinProgressBar,
    skinProgressBarProgress, skinRadioButton, skinScrollBottom, skinScrollRight,
    skinTrackBarSlider, skinTable, skinTableDelimiter, skinTableItem, skinMouse);
  TSkinConfigs = (cfgEditLine, cfgMainMenuLine, cfgTableGrid, cfgTreeColor,
    cfgTreeWidth, cfgSelectionColor);

  TDisplayEffect = (deNone, deZoomIn, deFadeIn, deSlideDown);
  TColor = type integer;

  TFileName = TCaption;
  TFontState = (fstNormal, fstActive, fstDisabled);
  THandleEvent = (heNone, heMoveObject, heResizeObject, heInputText,
                  heComboField, heComboMove, hePopup,
                  heTrackBar, heScrollH, heScrollW);
  TImageAlign = (iaLeft, iaRight);
  TComponentAlign = (caNone, caLeft, caTop, caRight, caBottom, caClient);
  TEditableMethod = (emNone, emTrue);

  zglTGui = class;
  zglTFrame = class;
  zglTGUIObject = class;
{$IFDEF GUI_USE_MULTIBUTTON}
  {$DEFINE GUI_USE_POPUPMENU}
{$ENDIF}
{$IFDEF GUI_USE_MAINMENU}
  {$DEFINE GUI_USE_POPUPMENU}
  zglTMainMenuItem = class;
  zglTMenuItem = class;
{$ENDIF}
{$IFDEF GUI_USE_POPUPMENU}
  zglTPopupMenu = class;
{$ENDIF}
{$IFDEF GUI_USE_TABLE}
  zglTTableItem = class;
{$ENDIF}

  // EVENTS
  zglTEvent = procedure (Sender: zglTGUIObject) of Object;
{$IFDEF GUI_USE_MAINMENU}
  zglTMMEvent = procedure (SelectedElement: zglTMainMenuItem) of Object;
  zglTMenuEvent = procedure (Sender: zglTGUIObject;
    SelectedElement: zglTMenuItem; X, Y: integer) of Object;
{$ENDIF}
  zglTGuiEvent = procedure (Sender: zglTGui) of Object;
  zglTMouseEvent = procedure (Sender: zglTGUIObject; X, Y: integer) of Object;
  zglTPopupEvent = procedure (Sender: zglTGUIObject; X, Y: integer;
    var CanPopup: Boolean) of Object;
  zglTDrawEvent = procedure (Sender: zglTGUIObject; X, Y, W, H,
    scrX, scrY: Single) of Object;
  zglTKeyEvent = procedure (Sender: zglTGUIObject; Key: Byte;
    var Cancel: Boolean) of Object;
{$IFDEF GUI_USE_TABLE}
  zglTTableEvent = procedure (Sender: zglTGUIObject;
    SelectedElement: zglTTableItem) of Object;
{$ENDIF}

  // Parent class for RTTI
  zglTEditableClass = class {$IFDEF FPC}( TPersistent ){$ENDIF}
    private
      fSelfCount: Integer;
      fData: Pointer;
      fEditable: TEditableMethod;
    published
      procedure UseCounter(Count: integer = 1);
      procedure Drop;
    public
      property SelfCount: integer read fSelfCount;
      property Editable: TEditableMethod read fEditable write fEditable;
      // Any userful pointer value to you (user option)
      property Data: pointer read fData write fData;

      constructor Create(pEditable: boolean = True); overload;
      constructor Create(pEditable: TEditableMethod = emTrue); overload;
  end;

  // Named Object

  zglTNamedObject = class (zglTEditableClass)
    private
      fName: TCaption;
{$IFDEF GUI_USE_EDITOR}
      fDefaultProperties: TStringList;
{$ENDIF}
      fOnNameChange: zglTEvent;
      procedure setName(capt: TCaption);
    published
      // Component's name (needed for Editor)
      property Name: TCaption read fName write setName;
      // Name change Event
      property OnNameChange: zglTEvent read fOnNameChange write fOnNameChange;
    public
{$IFDEF GUI_USE_EDITOR}
      property DefaultProperties: TStringList read fDefaultProperties write fDefaultProperties;
{$ENDIF}
      constructor Create(pEditable: boolean = True); overload;
      constructor Create(pEditable: TEditableMethod = emTrue); overload;
  end;

  // Determines what to do with the component, if the parent size changed.
  TStretch = class (zglTEditableClass)
    private
      fSTR_LEFT: boolean;
      fSTR_TOP: boolean;
      fSTR_BOTTOM: boolean;
      fSTR_RIGHT: boolean;
    published
      property STR_LEFT: Boolean read fSTR_LEFT write fSTR_LEFT;
      property STR_TOP: Boolean read fSTR_TOP write fSTR_TOP;
      property STR_BOTTOM: Boolean read fSTR_BOTTOM write fSTR_BOTTOM;
      property STR_RIGHT: Boolean read fSTR_RIGHT write fSTR_RIGHT;
    public
      constructor Create;
  end;

  // zglTRect in class variant (needed to RTTI)
  zglTCRect = class (zglTEditableClass)
    private
      fX, fY, fW, fH: Single;
    published
      property X: Single read fX write fX;
      property Y: Single read fY write fY;
      property W: Single read fW write fW;
      property H: Single read fH write fH;
    public
      function GetRect:  zglTRect;
      constructor Create;
  end;

  // FONT OBJECT: Standart ZenGL font wrapper
  zglTFontObject = class (zglTEditableClass)
    private
      fData: zglPFont;
      fSize: single;
      fColor: TColor;
      fAlpha: Byte;
      fAlign: TFontAlign;
      fFileName: AnsiString;

      procedure setFileName(name: AnsiString);
    public
      constructor Create(pData: zglPFont; pSize: Single; pColor: TColor;
        pAlpha: Byte; pAlign: TFontAlign = faMiddleCenter);
      constructor CreateDefaults(pGui: zglTGui);
      property Data: zglPFont read fData write fData;
    published
      property FileName: AnsiString read fFileName write setFileName;

      property Size: single read fSize write fSize;
      property Color: TColor read fColor write fColor;
      property Alpha: Byte read fAlpha write fAlpha;
      property Align: TFontAlign read fAlign write fAlign;
  end;

  // TEXTURE OBJECT: Standart ZenGL texture wrapper
  zglTCTexture = class(zglTEditableClass)
    private
      fTexture: zglPTexture;
      fFileName: TCaption;
      fUseFrames: Boolean;
      fTexWidth, fTexHeight, fFrameOffset: integer;

      procedure Draw(X, Y, W, H: Single; Frame: integer; Alpha: Byte);

      procedure setTexWidth(W: integer);
      procedure setTexHeight(H: integer);
      procedure setFileName(Name: TCaption);
    published
      property FileName: TCaption read fFileName write setFileName;
      property UseFrames: boolean read fUseFrames write fUseFrames;
      property TexWidth: integer read fTexWidth write setTexWidth;
      property TexHeight: integer read fTexHeight write setTexHeight;
      property FrameOffset: integer read fFrameOffset write fFrameOffset;
    public
      property Texture: zglPTexture read fTexture write fTexture;
      constructor Create(pTexture: zglPTexture);
      destructor Destroy; override;
  end;


  // Three fonts container: active, inactive and disabled
  zglTFontContainer = class (zglTNamedObject)
    private
      fNormal : zglTFontObject;
      fActive: zglTFontObject;
      fDisabled: zglTFontObject;
      fFileName: AnsiString;

      procedure setFileName(name: AnsiString);
    public
      function GetForState(state: TFontState): zglTFontObject; overload;
      function GetForState(state: boolean): zglTFontObject; overload;

      constructor Create(pNormal, pActive, pDisabled: zglTFontObject); overload;
      constructor Create(pData: zglPFont; pSize: Single; pColor: integer;
        pAlpha: Byte; pAlign: TFontAlign = faMiddleCenter); overload;
      constructor CreateDefaults(pGui: zglTGui);
      destructor Destroy; override;
    published
      property FileName: AnsiString read fFileName write setFileName;
      property Normal: zglTFontObject read fNormal write fNormal;
      property Active: zglTFontObject read fActive write fActive;
      property Disabled: zglTFontObject read fDisabled write fDisabled;
  end;

  // Base gui object (can be invisible)
  zglTBaseGUIObject = class (zglTNamedObject)
    private
      fGui: zglTGui;
      fFont: zglTFontContainer;
      function getFont: zglTFontContainer;
      procedure setFont(fontName: zglTFontContainer);
      procedure HandleDraw(evnt: THandleEvent); virtual;
      function HandleUpdate(dt: Double; evnt: THandleEvent): boolean; virtual;
      procedure HandleBegin(evnt: THandleEvent); virtual;
      procedure HandleLeave(evnt: THandleEvent); virtual;
    public
      property Gui: zglTGui read fGui write fGui;
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui);
    published
      property Font: zglTFontContainer read getFont write setFont;
  end;

  zglTGuiEffect = class;
  zglTGuiSkin = class;

  // Base object

  { zglTGUIObject }

  zglTGUIObject = class(zglTBaseGUIObject)
    private
      fJustPressed, fNeedScroll, fPressed,
        fHover, fVisible, fCatchMouseClick,
        fEditMode, fEvenNotMove, fContainer, fEnabled: Boolean;
      fParent: zglTFrame;
      fCaption: TCaption;
      fSysData: pointer;
      fMouse: Integer;

      fMouse_X, fMouse_Y, fOffsetX, fOffsetY,
        fTag, fCorner: integer;
      fStretch: TStretch;
      fDtX, fDtY, fMinWidth, fMinHeight: single;
      fRootPos: zglTPoint2D;
      fRect: zglTCRect;
      fAlign: TComponentAlign;
{$IFDEF GUI_USE_POPUPMENU}
      fPopupMenu: zglTPopupMenu;
{$ENDIF}
{$IFDEF GUI_USE_EDITOR}
      fForceVisible: boolean;
{$ENDIF}
{$IFDEF GUI_USE_EDITOR}
      fState: Pointer;
{$ENDIF}
      fOnResize, fOnMove: zglTEvent;
      fOnDraw: zglTDrawEvent;
      fOnCaptionChange: zglTEvent;
      fOnClick, fOnMouseEnter, fOnMouseLeave,
        fOnMouseMove: zglTMouseEvent;

      function IsHover(upr: zglPRect; oX: single = 0; oY: single = 0): boolean;
      function updateOffset(r:zglTRect;
        offsetX: single = 0; offsetY: single = 0):zglTRect;
      function getRootFont (pActive: boolean): zglTFontObject;
      function getEnabled: Boolean;
      function getOffectRect(r: zglTRect; oX, oY: integer): zglTRect;
      procedure setCaption(capt: TCaption); virtual;
      procedure setAlign(a: TComponentAlign);
      procedure updateSkin(skin: zglTGuiSkin); virtual;
{$IFDEF GUI_USE_EDITOR}
      function getVisible: boolean;
{$ENDIF}
{$IFDEF GUI_USE_POPUPMENU}
      procedure setPopupMenu(menuName: zglTPopupMenu);
{$ENDIF}
      procedure DrawEdit(oX, oY: single); virtual;
      procedure OnAddMe; virtual;

      property OffsetX: integer read fOffsetX write fOffsetX;
      property OffsetY: integer read fOffsetY write fOffsetY;
      property Mouse: integer read fMouse write fMouse;

    published
      procedure UpdateAlign; virtual;
      function HandleUpdate(dt: Double; evnt: THandleEvent): boolean; override;
      function UpdateRect(oX: single = 0; oY: single = 0):zglTRect;
      (* Draw object procedure
        clntRect - drawing area, if the component does not enter into it,
          it will be cut off.
        oX, oY - drawtree relative coordinates
      *)
      procedure Draw(clntRect: zglPRect; oX, oY: single); virtual; abstract;
      (* Update object procedure
        dt - Delay coefficient
        clntRect - updating area, if the component does not enter into it,
          it will be cut off.
        updateChind - is need update the child elements?
        oX, oY - drawtree relative coordinates
      *)
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); virtual;

      // Is object visible?
      property Visible: Boolean read {$IFDEF GUI_USE_EDITOR} getVisible {$ELSE} fVisible {$ENDIF} write fVisible;
      // Is object enabled?
      property Enabled: Boolean read getEnabled write fEnabled;
      // Caption (or Text)
      property Caption: TCaption read fCaption write setCaption;
      // Minimum object width
      property MinWidth: single read fMinWidth write fMinWidth;
      // Minimum object height
      property MinHeight: single read fMinHeight write fMinHeight;
      // Object orientation by the parent (if any)
      property Align: TComponentAlign read fAlign write setAlign;
      // Binding to the borders of the parent element
      //   when parent is resized
      property Stretch: TStretch read fStretch write fStretch;
      // The size and coordinates for the object
      property Rect: zglTCRect read fRect write fRect;
      // Any userful integer value to you (user option)
      property Tag: integer read fTag write fTag;
      // Detection of a mouse click. If there's none, when
      // you click on the object, the parent object will handle the
      // click event too.
      property CatchMouseClick: boolean
        read fCatchMouseClick write fCatchMouseClick;
      // Redraw Event
      property OnDraw: zglTDrawEvent read fOnDraw write fOnDraw;
      // Click Event
      property OnClick: zglTMouseEvent read fOnClick write fOnClick;
      // Move Event
      property OnMove: zglTEvent read fOnMove write fOnMove;
      // Resize Event
      property OnResize: zglTEvent read fOnResize write fOnResize;
      // Caption change Event
      property OnCaptionChange: zglTEvent
        read fOnCaptionChange write fOnCaptionChange;
      // Mouse Enter Event
      property OnMouseEnter: zglTMouseEvent
        read fOnMouseEnter write fOnMouseEnter;
      // Mouse Leave Event
      property OnMouseLeave: zglTMouseEvent
        read fOnMouseLeave write fOnMouseLeave;
      // Mouse Move Event
      property OnMouseMove: zglTMouseEvent
        read fOnMouseMove write fOnMouseMove;
    public
      constructor CreateDefaults(pGui: zglTGui);
      (* constructor
        pGui - GUI
        pX, pY, pW, pH - coords and size
        pVisible - visibility
      *)
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pVisible: Boolean = true);
      destructor Destroy; override;
{$IFDEF GUI_USE_EDITOR}
      property ForceVisible: boolean read fForceVisible write fForceVisible;
      property State: Pointer read fState write fState;
{$ENDIF}
      // Detection of mouse button being pressed
      property JustPressed: boolean read fJustPressed;
      // Whether the object is a container for other objects
      property Container: boolean read fContainer;
      // Is the mouse over the object
      property Hover: Boolean read fHover;
      // Parent object (or nil)
      property Parent: zglTFrame read fParent;
      // Obsolete object coordinates, it's read-only.
      //  (because the usual - relative to the parent)
      property RootPos: zglTPoint2d read fRootPos;
{$IFDEF GUI_USE_POPUPMENU}
      // Drop-down menu for the object, which rolled down after clicking RMB.
      property PopupMenu: zglTPopupMenu read fPopupMenu write setPopupMenu;
{$ENDIF}
      // Edit mode, needed for the Editor
      property _EditMode: boolean read fEditMode write fEditMode;
      // Any userful pointer value to you (user option)
      property SysData: pointer read fSysData write fSysData;
      // Move object
      procedure Move (pX, pY: Single);
      // Resize object
      procedure Resize (pW, pH: Single); virtual;
      // Show object
      procedure Show; virtual;
      // Show as modal way
      procedure ShowModal; virtual;
      // Hide object
      procedure Hide; virtual;
  end;

  zglTForm = class;

  // Effects manager
  zglTGuiEffect = class
    private
      fComponent: zglTForm;
      fRender: zglPRenderTarget;
      fTime, fEndTime: single;
      fInited: boolean;
      fGui: zglTGui;
    published
      procedure DrawEffect(clntRect: zglPRect; oX, oY: single); virtual; abstract;
      procedure UpdateEffect(dt: Double); virtual;
      procedure Draw(clntRect: zglPRect; oX, oY: single; RenderRes: boolean = true);
      procedure Update(dt: Double);
      procedure Init; virtual;
      procedure Clear;
    public
      property Inited: boolean read fInited write fInited;
      property Time: single read fTime write fTime;
      property EndTime: single read fEndTime write fEndTime;
      property Render: zglPRenderTarget read fRender write fRender;
      property Component: zglTForm read fComponent write fComponent;
      property Gui: zglTGui read fGui write fGui;

      constructor Create(pGui: zglTGUI; pComponent: zglTForm);
      destructor Destroy; override;
  end;

  zglTZoomInEffect = class(zglTGuiEffect)
     published
       procedure DrawEffect(clntRect: zglPRect; oX, oY: single); override;
     public
      constructor Create(pGui: zglTGUI; pComponent: zglTForm);
  end;

  zglTFadeInEffect = class(zglTGuiEffect)
     published
       procedure DrawEffect(clntRect: zglPRect; oX, oY: single); override;
     public
      constructor Create(pGui: zglTGUI; pComponent: zglTForm);
  end;

  zglTSlideDownEffect = class(zglTGuiEffect)
     published
       procedure DrawEffect(clntRect: zglPRect; oX, oY: single); override;
     public
      constructor Create(pGui: zglTGUI; pComponent: zglTForm);
  end;

  // The class-list. It is used for: zglTFrame, its derivatives and zglTGUI
  zglTGUIObjectsList = class
    private
      fGui: zglTGUI;
      fMaster: zglTFrame;
      fObjects: TList;
      function getObject(Index: Integer):zglTGUIObject;
      function getObjectsCount:integer;
    public
      // Items elements
      property Items[Index: integer]:zglTGUIObject read getObject;
      // Items count
      property Count:integer read getObjectsCount;
      // Add new element in the list
      function Add(obj: zglTGUIObject): zglTGUIObject;
      // Remove item from list
      procedure Remove(obj: zglTGUIObject; doDrop: boolean = true);
      // Remove all items from list (clear)
      procedure RemoveAll;
      // Move the specified item in the end of list (it will appear at the top)
      procedure MoveToTop(obj: zglTGUIObject);
      // Move the specified item in the begin of list (it will appear at the bottom)
      procedure MoveToBottom(obj: zglTGUIObject);
      // Find object from list by name (and from list item's child objects)
      function Find(Name: TCaption): zglTGUIObject; overload;
      // Find object from list by pointer (return index, otherwise -1)
      function Find(Obj: zglTGUIObject): integer; overload;
      // clear items
      procedure Clear(free: boolean = true);
      // constructor
      constructor Create(pGui: zglTGUI; pMaster: zglTFrame);
      // destructor
      destructor Destroy; override;
  end;

{$IFDEF GUI_USE_EDITOR}
  // object for gui editor
  zglTUnknownObject = class(zglTGUIObject)
    private
      fProperties: TStringList;
      fClassName: TCaption;
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      property UnknownProperties: TStringList read fProperties write fProperties;
      property UnknClassName: TCaption read fClassName write fClassName;
    public
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pCaption: TCaption; pVisible: Boolean = true);
      constructor CreateDefaults(pGui: zglTGui);
      destructor Destroy; override;
  end;
{$ENDIF}

{$IFDEF GUI_USE_LABEL}
  // Label object
  zglTLabel = class(zglTGUIObject)
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
    public
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pCaption: TCaption; pVisible: Boolean = true);
      constructor CreateDefaults(pGui: zglTGui);
  end;
{$ENDIF}
{$IFDEF GUI_USE_BUTTON}
  // Button object
  zglTButton = class(zglTGUIObject)
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
    public
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pCaption: TCaption; pVisible: Boolean = true);
  end;
{$ENDIF}
{$IFDEF GUI_USE_MULTIBUTTON}
  // Button object
  zglTMultiButton = class(zglTGUIObject)
    private
      fDropMenu: zglTPopupMenu;
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;

      property DropMenu: zglTPopupMenu read fDropMenu write fDropMenu;
    public
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pCaption: TCaption; pVisible: Boolean = true);
      destructor Destroy; override;
  end;
{$ENDIF}
{$IFDEF GUI_USE_PROGRESSBAR}
  // Progress Bar
  zglTProgressBar = class(zglTGUIObject)
    private
      fProgress, fLastProgress, fMax, fMin: integer;
      fOnChange: zglTEvent;
      procedure setProgress(progress: Integer);
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      // Current Progress, = Min ... Max
      property Progress: Integer read fProgress write setProgress;
      // Max possible Progress. Default: 100
      property Max: Integer read fMax write fMax;
      // Min possible Progress. Default: 0
      property Min: Integer read fMin write fMin;
      // Change progress event
      property OnChange: zglTEvent read fOnChange write fOnChange;
    public
      procedure SetSafeProgress(progress: Integer);
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pVisible: Boolean = true);
  end;
{$ENDIF}
{$IFDEF GUI_USE_TRACKBAR}
  // Track Bar (Progress bar with moving progress button)
  zglTTrackBar = class(zglTProgressBar)
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      procedure HandleDraw(evnt: THandleEvent); override;
      function HandleUpdate(dt: Double; evnt: THandleEvent): boolean; override;
      procedure HandleLeave(evnt: THandleEvent); override;
    public
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pVisible: Boolean = true);
  end;
{$ENDIF}
{$IFDEF GUI_USE_IMAGE}
  // Image
  zglTImage = class(zglTGUIObject)
    private
      fTexture: zglTCTexture;
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      // Imagebutton texture. sprite texHeight x texHeight
      property Texture: zglTCTexture read fTexture write fTexture;
    public
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single; texWidth,
        texHeight: word; pTexture: zglPTexture = nil; pTextureOffset: Word = 0;
        pVisible: Boolean = true);
      destructor Destroy; override;
  end;
{$ENDIF}
{$IFDEF GUI_USE_IMAGEBUTTON}
  // ImageButton
  zglTImageButton = class(zglTGUIObject)
    private
      fTexture: zglTCTexture;
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      // Imagebutton texture. Three-frame sprite texHeight x texHeight, where:
      //   1st frame - normal state
      //   2nd frame - mouse-hovered state
      //   3rd frame - pressed-mouse-button state
      property Texture: zglTCTexture read fTexture write fTexture;
    public
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single; texWidth,
        texHeight: word; pTexture: zglPTexture = nil; pTextureOffset: Word = 0;
        pVisible: Boolean = true);
      destructor Destroy; override;
  end;
{$ENDIF}
{$IFDEF GUI_USE_EDIT}
  // Edit
  zglTEdit = class(zglTGUIObject)
    private
      fAlign, fMaxLength: integer;
      fUpd, fTextOffset: single;
      fSelection, fMouseDropped: Boolean;
      fSelectionStart, fSelectionEnd, fSelectionPos, fSelectionBegin: Word;
      fPasswordChar: Char;
      fLastVal, fLastLeftVal, fLeftCaption, fRightCaption: TCaption;
      fOnChange, fOnEnterKey: zglTEvent;
      fOnKeyDown: zglTKeyEvent;
      procedure setCaption(capt: TCaption); override;
      procedure DeleteSelection;
      procedure CopySelection;
      procedure SelectAll;
      procedure CheckSelection(dt: Double);
      property SelectionBegin: word read fSelectionBegin write fSelectionBegin;
      property SelectionPos: word read fSelectionPos write fSelectionPos;
      property MouseDropped: boolean read fMouseDropped write fMouseDropped;

      property LeftCaption: TCaption read fLeftCaption write fLeftCaption;
      property RightCaption: TCaption read fRightCaption write fRightCaption;
      property TextOffset: single read fTextOffset write fTextOffset;
      property LastVal: TCaption read fLastVal write fLastVal;
      property LastLeftVal: TCaption read fLastLeftVal write fLastLeftVal;

      function GetSelection(oX, oY: Single): integer;
      function GetCursorPos(Id: integer): integer;
    published
      procedure HandleDraw(evnt: THandleEvent); override;
      function HandleUpdate(dt: Double; evnt: THandleEvent): boolean; override;
      procedure HandleLeave(evnt: THandleEvent); override;
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      // Maximum text length
      property MaxLength: integer read fMaxLength write fMaxLength;
      // Password Char. Default: 0 (No password char)
      property PasswordChar: Char read fPasswordChar write fPasswordChar;
      // Text align: TEXT_LEFT, TEXT_CENTER, TEXT_RIGHT
      property Align: integer read fAlign write fAlign;
      // Change text event
      property OnChange: zglTEvent read fOnChange write fOnChange;
      // Enter button press event
      property OnEnterKey: zglTEvent read fOnEnterKey write fOnEnterKey;
      // Key down event
      property OnKeyDown: zglTKeyEvent read fOnKeyDown write fOnKeyDown;
    public
      // Is text selected
      property Selection: boolean read fSelection write fSelection;
      // Selection start
      property SelectionStart: word read fSelectionStart write fSelectionStart;
      // Selection end
      property SelectionEnd: word read fSelectionEnd write fSelectionEnd;
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pCaption: TCaption; pAlign: Integer = TEXT_LEFT; pVisible: Boolean = true);
      // Focus edit on this object
      procedure Focus;
  end;
{$ENDIF}
{$IFDEF GUI_USE_COMBOBOX}
  // Drop-bown combobox object
  zglTComboBox = class(zglTGUIObject)
    private
      fSelected, fComboMax: Word;
      fOffset, fdOffset, fsOffset: Integer;
      fPopupRect: Single;
      fDropSideDown: Boolean;
      fItems: TStringList;
      fOnChange: zglTEvent;

      property PopupRect: Single read fPopupRect write fPopupRect;
      property DropSideDown: boolean read fDropSideDown write fDropSideDown;
    published
      procedure HandleBegin(evnt: THandleEvent); override;
      procedure HandleDraw(evnt: THandleEvent); override;
      function HandleUpdate(dt: Double; evnt: THandleEvent): boolean; override;
      procedure HandleLeave(evnt: THandleEvent); override;
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      // Selected element
      property Selected: Word read fSelected write fSelected;
      // Maximum number of elements that can be displayed without scrollbar
      //  (otherwise the scrollbar appears)
      property ComboMax: Word read fComboMax write fComboMax;
      // Change selection event
      property OnChange: zglTEvent read fOnChange write fOnChange;
      // List items  strings
      property Items: TStringList read fItems write fItems;
    public
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW: Single; pH: Single
        = 17; pComboMax: Word = 8; pVisible: Boolean = true);
      destructor Destroy; override;
  end;
{$ENDIF}
{$IFDEF GUI_USE_CHECKBOX}
  // Checkbox
  zglTCheckBox = class(zglTGUIObject)
    private
      fChecked: Boolean;
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      // Is the checkbox checked
      property Checked: Boolean read fChecked write fChecked;
    public
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pCaption: TCaption; pVisible: Boolean = true);
  end;
{$ENDIF}
{$IFDEF GUI_USE_RADIOBUTTON}
  // Radio button. The parent should be zglTRadioBox
  zglTRadioButton = class(zglTGUIObject)
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
    public
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pCaption: TCaption; pVisible: Boolean = true);
  end;
{$ENDIF}
{$IFDEF GUI_USE_MAINMENU}
  // Main menu item
  zglTMainMenuItem = class (zglTEditableClass)
    private
      Popup: zglTPopupMenu;
      Text: TCaption;
      X, Width: integer;
    public
      CallBack: zglTMMEvent;
      constructor Create(Text_: TCaption; Width_: integer;
        Popup_: zglTPopupMenu; CallBack_: zglTMMEvent = nil);
      destructor Destroy; override;
  end;
  // Main menu
  zglTMainMenu = class (zglTGUIObject)
    private
      fItems: TList;
      ContexItem: zglTMainMenuItem;
      procedure CalcOffsets;
      function getItemsCount: Integer;
      function getItem(Index: integer): zglTMainMenuItem;
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
    public
      property ItemsCount: integer read getItemsCount;
      property Items[Index: integer]: zglTMainMenuItem read getItem;
      // Add item to the menu
      procedure AddItem(Item: zglTMainMenuItem);
      // Delete item from the menu
      procedure RemoveItem(Item: zglTMainMenuItem);
      destructor Destroy; override;
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW: Single; pH: Single
        = 16; pVisible: Boolean = true); virtual;
  end;
  // Drop-down menu item
  zglTMenuItem = class (zglTEditableClass)
    private
      fText: TCaption;
      fIconId: integer;
      fEnabled: Boolean;
      fCheckbox, fChecked: boolean;
      fCallBack: zglTMenuEvent;
    public
      property IconId: integer read fIconId write fIconId;
      property Checkbox: boolean read fCheckbox write fCheckbox;
      property Checked: boolean read fChecked write fChecked;
      property Text: TCaption read fText write fText;
      property Enabled: Boolean read fEnabled write fEnabled;
      property CallBack: zglTMenuEvent read fCallBack write fCallBack;
      constructor Create(Text_: TCaption; CallBack_: zglTMenuEvent; pEnabled: Boolean = true);
  end;
{$ENDIF}
{$IFDEF GUI_USE_POPUPMENU}
  // Drop-down menu
  zglTPopupMenu = class (zglTBaseGUIObject)
    private
      fItems: TList;
      fShowIcons: boolean;
      fIconsTexture: zglPTexture;
      fIconsSize: zglTPoint2D;
      X, Y, Width, MinWidth: integer;
      fPopupTime: single;
      fCaller: zglTGUIObject;
      fOnPopup: zglTPopupEvent;
      function getItem(index: Integer): zglTMenuItem;
      function getItemsCount: integer;

      property PopupTime: single read fPopupTime write fPopupTime;
    published
      procedure HandleDraw(evnt: THandleEvent); override;
      function HandleUpdate(dt: Double; evnt: THandleEvent): boolean; override;
      procedure HandleLeave(evnt: THandleEvent); override;
      // Drop to down menu event
      property OnPopup: zglTPopupEvent read fOnPopup write fOnPopup;
    public
      // Object that caused the menu
      property Caller: zglTGUIObject read fCaller write fCaller;
      // Menu items
      property Items[Index: integer]: zglTMenuItem read getItem;
      // Menu item count
      property ItemsCount: Integer read getItemsCount;
      // Make the menu popup
      procedure Popup(Sender: zglTGUIObject; X_, Y_: Integer; pMinWidth: integer = 0);
      // Add item to the items list
      function AddItem(Item: zglTMenuItem): zglTPopupMenu;
      // Remove item from the items list (with release)
      procedure RemoveItem(Item: zglTMenuItem);
      // Show the items icons
      property ShowIcons: boolean read fShowIcons write fShowIcons;
      // Items icons texture (sprite)
      property IconsTexture: zglPTexture read fIconsTexture write fIconsTexture;
      // Set tie items icons texture (sprite) size
      procedure SetIconsSize(W, H: integer);
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; MinWidth_: integer); virtual;
      destructor Destroy; override;
  end;
{$ENDIF}
  // An object that has a scroll bar
  zglTScrollBox = class(zglTGUIObject)
    private
      fScroll: zglTPoint2D;
      fScrollHeight, fScrollWidth: single;
      fClientRect, fBorder: zglTRect;
      procedure setScrollHeight(s: single);
      procedure setScrollWidth(s: single);
      procedure UpdateClientRect;
      procedure OnAddMe; override;
    published
      procedure UpdateAlign; override;
      procedure HandleDraw(evnt: THandleEvent); override;
      function HandleUpdate(dt: Double; evnt: THandleEvent): boolean; override;
      procedure HandleLeave(evnt: THandleEvent); override;
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      // Vertical scroll distance
      property ScrollHeight: single read fScrollHeight write setScrollHeight;
      // Horizontal scroll distance
      property ScrollWidth: single read fScrollWidth write setScrollWidth;
    public
      // Scroll position (0, 0) means not scrolled
      property Scroll: zglTPoint2D read fScroll;
      // Client rect (interior dimensions), read only.
      property ClientRect: zglTRect read fClientRect;
      // Move the scroll
      procedure ScrollTo (pX, pY: Single);
      procedure Resize (pW, pH: Single); override;
  end;
{$IFDEF GUI_USE_LIST}
  zglTList = class;
  // Classic list item (without hierarchy)
  zglTListItem = class (zglTEditableClass)
    private
      fList: zglTList;
      fCaption: TCaption;
      fHeight: integer;
      fSysData: Pointer;
      fTag: integer;
      fIconId: Integer;
      fPadding: zglTRect;
      fIconsTexture: zglPTexture;
      fIconsSize: zglTPoint2D;
    published
      procedure Draw(oX, oY, oW, oH: Integer; clntRect: zglTRect); virtual;
      procedure Update(dt: Double; oX, oY, oW, oH: Integer;
        clntRect: zglTRect); virtual;
      // User tag data
      property Tag: integer read fTag write fTag;
      // Caption
      property Caption: TCaption read fCaption write fCaption;
      // Item icon id
      property IconId: integer read fIconId write fIconId;
    public
      function GetPosition: integer;
      // Set the item icon size
      procedure SetIconsSize(W, H: integer);
      // Set the item icon padding
      procedure SetPadding(X, Y, W, H: Single);
      // Item icons texture (can be inherited from list)
      property IconsTexture: zglPTexture read fIconsTexture write fIconsTexture;
      // User system data (for editor)
      property SysData: Pointer read fSysData write fSysData;
      // Vertical height
      property Height: integer read fHeight write fHeight;
      constructor Create(pList: zglTList; pCaption: TCaption; pHeight: Integer);
      destructor Destroy; override;
  end;

  // Tree list item (with hierarchy)
  zglTTreeListItem = class (zglTListItem)
    private
      fParent: zglTListItem;
      fLevel: integer;
    published
      procedure Draw(oX, oY, oW, oH: Integer; clntRect: zglTRect); override;
      procedure Update(dt: Double; oX, oY, oW, oH: Integer;
        clntRect: zglTRect); override;
    public
      // Parent item (if == nil then item is root)
      property Parent: zglTListItem read fParent write fParent;
      // Drawn level (not recommended to change)
      property Level: integer read fLevel write fLevel;
      constructor Create(pList: zglTList; pParent: zglTListItem;
        pCaption: TCaption; pHeight: Integer);
      destructor Destroy; override;
  end;

  TOnDrawItem = procedure (Item: zglTListItem; clntRect: zglTRect;
    oX, oY, oW, oH: single; var DrawOther: boolean) of Object;
  TOnUpdateItem = procedure (Item: zglTListItem;  clntRect: zglTRect;
    dt: Double; oX, oY, oW, oH: single; var CatchClick: boolean) of Object;

  // List
  zglTList = class(zglTScrollBox)
    private
      fItems: TList;
      fTextAlign: Integer;
      fSelected: zglTListItem;
      fShowIcons: boolean;
      fItemsPerLine: integer;
      fIconsTexture: zglPTexture;
      fIconsSize: zglTPoint2D;
      fOnDrawItem: TOnDrawItem;
      fOnUpdateItem: TOnUpdateItem;
      fOnSelectItem: zglTMouseEvent;
      procedure UpdateItems;
      function getItem(Index: Integer): zglTListItem;
      function getItemsCount: Integer;
      procedure setSelected(item: zglTListItem);
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      // Text align for the item
      property TextAlign: Integer read fTextAlign write fTextAlign;
      // Is needed to list items has icons
      property ShowIcons: boolean read fShowIcons write fShowIcons;
      // Redraw item event
      property OnDrawItem: TOnDrawItem read fOnDrawItem write fOnDrawItem;
      // Update item event
      property OnUpdateItem: TOnUpdateItem read fOnUpdateItem write fOnUpdateItem;
      // Select item event
      property OnSelectItem: zglTMouseEvent read fOnSelectItem write fOnSelectItem;
    public
      // List items icon texture (sprite)
      property IconsTexture: zglPTexture read fIconsTexture write fIconsTexture;
      // Count of the items per line
      property ItemsPerLine: Integer read fItemsPerLine write fItemsPerLine;
      // Selected item (otherwise = nil)
      property Selected: zglTListItem read fSelected write setSelected;
      // List items
      property Items[Index: integer]: zglTListItem read getItem;
      // List items count
      property ItemsCount: Integer read getItemsCount;
      // Set the items icon texture (sprite) size
      procedure SetIconsSize(W, H: integer);
      // Get item by DATA property, assigned to
      function ItemByData(Data: Pointer): zglTListItem;
      // Ass item to list
      procedure AddItem(Item: zglTListItem);
      // Insert item at some index
      procedure InsertItem(Index: integer; Item: zglTListItem);
      // Move item to some index
      procedure MoveItem(Item: zglTListItem; Index: integer);
      // Insert item after other item
      procedure InsertAfter(After, Item: zglTListItem);
      // Insert item before other item
      procedure InsertBefore(Before, Item: zglTListItem);
      // Remove item from the items list (with release)
      procedure RemoveItem(Item: zglTListItem);
      // Clear the list (remove all items)
      procedure Clear;
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pVisible: Boolean = true);
      destructor Destroy; override;
  end;
{$ENDIF}

{$IFDEF GUI_USE_TABLE}
  zglTTable = class;
  // Table header (column)
  zglTTableHeader = class (zglTEditableClass)
    private
      fCaption: TCaption;
      fWidth, fAlign: Integer;
      fTable: zglTTable;
      fBold: Boolean;
    published
      // Header's table
      property Table: zglTTable read fTable write fTable;
      // Header caption
      property Caption: TCaption read fCaption write fCaption;
      // Is needed to draw with bold
      property Bold: Boolean read fBold write fBold;
      // Header width
      property Width: Integer read fWidth write fWidth;
      // Header text alignment
      property Align: Integer read fAlign write fAlign;
    public
      constructor Create(pTable: zglTTable;
        pCaption: TCaption; pWidth: Integer = 100;
        pAlign: integer = TEXT_LEFT; pBold: boolean = false);
      destructor Destroy; override;
  end;

  // Table item (row)
  zglTTableItem = class (zglTEditableClass)
    private
      fColumns: TStringList;
      fTable: zglTTable;
      function getColumnsCount: Integer;
      function getComponent(index: Word): zglTGUIObject;
    published
      // Row's table
      property Table: zglTTable read fTable write fTable;
    public
      // Row's columns count
      property ColumnsCount: integer read getColumnsCount;
      // Row's user components
      property Components[index: Word]: zglTGUIObject read getComponent;
      // Columns
      property Columns: TStringList read fColumns;
      // Add new column in row (as text)
      function AddColumn(Value: TCaption): zglTTableItem;
      // Add new GUI object in row (in new column)
      function AddComponent(Obj: zglTGUIObject): zglTTableItem;
      constructor Create(pTable: zglTTable);
      destructor Destroy; override;
  end;

  // Table

  { zglTTable }

  zglTTable = class(zglTScrollBox)
    private
      fHeaders, fItems: TList;
      fRowSize: integer;
      fDrawGrid: Boolean;
      fSelected, fHovered: zglTTableItem;
      fOnSelect: zglTTableEvent;

      procedure recalcSizes;
      function getHeadersCount: Integer;
      function getHeader(index: integer): zglTTableHeader;
      function getItemsCount: Integer;
      function getItem(index: integer): zglTTableItem;
      procedure updateSkin(skin: zglTGuiSkin); override;
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      // Draw grid lines
      property DrawGrid: boolean read fDrawGrid write fDrawGrid;
      // Row size
      property RowSize: integer read fRowSize write fRowSize;
    public
      // Selected item
      property Selected: zglTTableItem read fSelected write fSelected;
      // Select item event
      property OnSelect: zglTTableEvent read fOnSelect write fOnSelect;
      // Hovered item by the mouse
      property Hovered: zglTTableItem read fHovered;
      // Headers count
      property HeadersCount: Integer read getHeadersCount;
      // Headers list
      property Headers[index: integer]: zglTTableHeader read getHeader;
      // Items (rows) count
      property ItemsCount: Integer read getItemsCount;
      // Items (rows)
      property Items[index: integer]: zglTTableItem read getItem;
      // Add new header (column)
      procedure AddHeader(Header: zglTTableHeader);
      // Remove header from the headers list
      procedure RemoveHeader(Header: zglTTableHeader);
      // Add new item (row)
      procedure AddItem(Item: zglTTableItem);
      // Remove item (row) from the items list
      procedure RemoveItem(Item: zglTTableItem);
      // Clear the table (remove all rows)
      procedure Clear;
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pVisible: Boolean = true);
      destructor Destroy; override;
  end;
{$ENDIF}

  // Frame (a container for other objects,
  //   anything that can include smth is the child of this frame)
  zglTFrame = class(zglTScrollBox)
    private
      fItems: zglTGUIObjectsList;
      fCanMove, fCanResize: Boolean;
      procedure OnAdd(Sender: zglTGUIObject); virtual;
      procedure OnRemove(Sender: zglTGUIObject); virtual;
    published
      procedure HandleDraw(evnt: THandleEvent); override;
      function HandleUpdate(dt: Double; evnt: THandleEvent): boolean; override;
      procedure HandleLeave(evnt: THandleEvent); override;
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      // Can be moved with the mouse
      property CanMove: Boolean read fCanMove write fCanMove;
      // Can be resized with the mouse by the right-bottom corner
      property CanResize: Boolean read fCanResize write fCanResize;
    public
      // Child elements
      property Items:zglTGUIObjectsList read fItems;
      // Move to gui's center (usually it's screen center)
      procedure MoveToCenter;
      // Resize to the full gui's rect (usually it's fullscreen)
      procedure FullScreen;
      // Create a full screen way
      constructor CreateFullScreen(pGui: zglTGui; pVisible: Boolean = true);
      procedure Resize (pW, pH: Single); override;
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pVisible: Boolean = true);
      destructor Destroy; override;
  end;

{$IFDEF GUI_USE_PAGECONTROL}
  zglTPageControl = class;

  // Tabs. To make the tabs work, the parent should be zglTPageControl.
  zglTPageControlSheet = class (zglTFrame)
    private
      fImage: zglPTexture;
      fImageSize: zglTPoint2D;
      X, Width: integer;
      procedure setCaption(capt: TCaption); override;
      procedure setImage(tex: zglPTexture);
    published
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
    public
      // The picture to the tab. (Otherwise Nil)
      property Image: zglPTexture read fImage write setImage;
      // Set the tab piscture size
      procedure SetImageSize(X, Y: integer);
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pCaption: TCaption;
        pVisible: Boolean = true; pImage: zglPTexture = nil);
      destructor Destroy; override;
      procedure Show; override;
      procedure Hide; override;
  end;
  // Tab's container

  { zglTPageControl }

  zglTPageControl = class (zglTFrame)
    private
      fSelected: Word;
      function getSheet(Index: Integer):zglTPageControlSheet;
      procedure setSelected(Index: Word);
      procedure RelocateButtons;
      procedure updateSkin(skin: zglTGuiSkin); override;
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      procedure OnAdd(Sender: zglTGUIObject); override;
      procedure OnRemove(Sender: zglTGUIObject); override;
      // Selected tab index
      property Selected: Word read fSelected write setSelected;
    public
      // Control tabs list
      property Sheets[Index: integer]:zglTPageControlSheet read getSheet;
      procedure Resize (pW, pH: Single); override;
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pVisible: Boolean = true);
      destructor Destroy; override;
  end;
{$ENDIF}

{$IFDEF GUI_USE_RADIOBUTTON}
  // Radio buttons container
  zglTRadioBox = class(zglTFrame)
    private
      fSelected: zglTRadioButton;
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clntRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
    public
      // Checked radio button
      property Selected: zglTRadioButton read fSelected write fSelected;
      // Select a radio button
      procedure Select(pI: zglTGUIObject);
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pVisible: Boolean = true);
  end;
{$ENDIF}

  // Form (usually root object)
  zglTForm = class(zglTFrame)
    private
{$IFDEF GUI_USE_MAINMENU}
      fMainMenu: zglTMainMenu;
{$ENDIF}
      fEffect: zglTGuiEffect;
      fCaptionAlign: TFontAlign;
      fDisplayEffect: TDisplayEffect;
      fOnClose: zglTEvent;
      fCloseButton: Boolean;
{$IFDEF GUI_USE_MAINMENU}
      procedure setMainMenu(mm: zglTMainMenu);
{$ENDIF}
      procedure setDisplayEffect(de: TDisplayEffect);
      procedure updateSkin(skin: zglTGuiSkin); override;
    published
      procedure Draw(clntRect: zglPRect; oX, oY: single); override;
      procedure Update(dt: Double; clientRect: zglPRect; updateChind: boolean;
        oX, oY: single); override;
      // Caption text align
      property CaptionAlign: TFontAlign read fCaptionAlign write fCaptionAlign;
      // Is draw the close button
      property CloseButton: boolean read fCloseButton write fCloseButton;
      // Close event (means pressing the "close" button in right-top corner)
      property OnClose: zglTEvent read fOnClose write fOnClose;
      // Assigned Effect
      property DisplayEffect: TDisplayEffect read fDisplayEffect write setDisplayEffect;
    public
      procedure Show; override;
{$IFDEF GUI_USE_MAINMENU}
      // Form main menu (Otherwise Nil)
      property MainMenu: zglTMainMenu read fMainMenu write setMainMenu;
{$ENDIF}
      // Attached effect
      property Effect: zglTGuiEffect read fEffect write fEffect;
      constructor CreateDefaults(pGui: zglTGui);
      constructor Create(pGui: zglTGui; pX, pY, pW, pH: Single;
        pCaption: TCaption; pVisible: Boolean = false);
      destructor Destroy; override;
  end;
  // Event handler
  zglTHandler = class
    private
      gui: zglTGui;
      fhObject: zglTBaseGUIObject;
      fhHandle: THandleEvent;
      canHover: boolean;
      procedure HandleEvent(obj: zglTBaseGUIObject; evnt: THandleEvent);
      procedure Update(dt: Double);
      procedure Draw;
    public
      constructor Create;
      procedure Clear;
      property HObject: zglTBaseGUIObject read fhObject write fhObject;
      property HHandle: THandleEvent read fhHandle write fhHandle;
  end;
  // Skin class
  zglPGuiSkinTex = ^zglTGuiSkinTex;
  zglTGuiSkinTex = record
    Texture: zglPTexture;
    W, H, BtnW, BtnH, ScrW, ScrH, OffX, OffY: integer;
  end;

  { zglTGuiSkin }

  zglTGuiSkin = class(TObject)
    private
      fSkinTex: array of zglTGuiSkinTex;
      fSkinCfg: array of Integer;
      procedure DrawTex(TexId: TSkinTextures; X, Y, dX, dY, dW, dH: Single);
      procedure DrawTexScaled(TexId: TSkinTextures; X, Y, W, H, dX, dY, dW, dH: Single);
      procedure DrawField(TexId: TSkinTextures;
        X, Y, W, H, dX, dY, dW, dH: Single);
      function Correct(SkinSize, RealSize: single): Single;
      function getSkinTex(Id: TSkinTextures): zglPGuiSkinTex;
      function getSkinCfg(Id: TSkinConfigs): integer;
    public
      property SkinTex[Index: TSkinTextures]: zglPGuiSkinTex read getSkinTex;
      property SkinCfg[Index: TSkinConfigs]: integer read getSkinCfg;

      procedure Load(FileName: TCaption; Filter: LongWord = TEX_DEFAULT_2D; ZipFilePassword: TCaption = '');

      constructor Create(FileName: TCaption; Filter: LongWord = TEX_DEFAULT_2D; ZipFilePassword: TCaption = '');
      destructor Destroy; override;
  end;

  // Main GUI
  zglTGui = class
    private
      fHandler: zglTHandler;
      fModal: zglTGUIObjectsList;
      fItems: TList;
      fFont: zglTFontContainer;
      fVisible: Boolean;
      grid: zglTGrid2D;
      fSkin: zglTGuiSkin;
      fCounter: integer;
      fRect: zglTRect;
      fMouse: Integer;
      fOnResize: zglTGuiEvent;
      fHover: zglTGUIObject;
      fState: Word;
{$IFDEF GUI_USE_EDITOR}
      fForceVisible: boolean;
{$ENDIF}
      fFormToDelete: zglTForm;

      procedure closeShowMessage(Sender: zglTGUIObject; X, Y: integer);
      function getCounter: integer;
      procedure setSkin(const AValue: zglTGuiSkin);
      procedure updateSkinItems(list: zglTGUIObjectsList);
      property Counter: integer read getCounter;
      procedure setState(State: Word);
      function getStatesCount: Integer;
      function getState(Index: Word): zglTGUIObjectsList;
      function getItems: zglTGUIObjectsList;
      procedure setFont(fontName: zglTFontContainer);

      property FormToDelete: zglTForm read fFormToDelete write fFormToDelete;
    published
      // Resize event
      property OnResize: zglTGuiEvent read fOnResize write fOnResize;
    public
      procedure updateSkin;
      function getRect(X, Y, W, H: single):zglTRect;
      //
      property Mouse: integer read fMouse write fMouse;
      // Add State
      function AddState: Word;
      // Remove condition. Attention: can shift identity
      procedure DeleteState(State: Word);
      // Some state items list
      property ItemsForState[index: Word]: zglTGUIObjectsList read getState;
      // Current state (Default is STATE_DEFAULT)
      property State: Word read fState write setState;
{$IFDEF GUI_USE_EDITOR}
      property ForceVisible: boolean read fForceVisible write fForceVisible;
{$ENDIF}
      // Count of states
      property StatesCount: integer read getStatesCount;
      // Current GUI skin
      property Skin: zglTGuiSkin read fSkin write setSkin;
      // GUI handler
      property Handler: zglTHandler read fHandler write fHandler;
      // Object, which the mouse is over at the moment (Otherwise = nil)
      property Hover: zglTGUIObject read fHover;
      // Modal forms (the one that appears on top of all, if any)
      property Modal: zglTGUIObjectsList read fModal;
      // Hide the modal form
      procedure ClearModal;
      // Show the user message (like VCL's ShowMessage)
      procedure ShowMessage(Caption, Data: TCaption; Effect: TDisplayEffect = deZoomIn);
      // Child elements for the current state (usually forms)
      property Items:zglTGUIObjectsList read getItems;
      // Defualt font
      property Font: zglTFontContainer read fFont write setFont;
      // Is the GUI visible
      property Visible: Boolean read fVisible write fVisible;
      // Draw rect
      property Rect: zglTRect read fRect;
      // Draw the GUI
      procedure Draw;
      // Draw the GUI mouse
      procedure DrawMouse;
      // Draw the GUI
      procedure Update(dt: Double);
      constructor Create(pSkin: zglTGuiSkin;
        screenX, screenY: integer;
        screenWidth, screenHeight: integer;
        defaultFont: zglTFontContainer);
      procedure Resize(screenWidth, screenHeight: integer);
      destructor Destroy; override;
  end;

function scissor_Rect(rct: zglTRect; clntRect, Ptr: zglPRect; var ne: boolean): boolean;

implementation

{$IFDEF WIN}
function GetClipboardText(Wnd: HWND; var Str: string): Boolean;
var
  hData: HGlobal;
  Res: string;
begin
  Result := True;
  if OpenClipboard(Wnd) then
  begin
    try
      hData := GetClipboardData(CF_TEXT);
      if hData <> 0 then
      begin
        try
          SetString(Res, PChar(GlobalLock(hData)), GlobalSize(hData));
        finally
          GlobalUnlock(hData);
        end;
      end
      else
      Result := False;
      Res := PAnsiChar(@Res[1]);
    finally
      CloseClipboard;
    end;
    Str := Res;
  end
  else
    Result := False;
end;
{$ELSE}
function GetClipboardText(Wnd: longint; var Str: string): Boolean;
begin
  Result := false;
end;
{$ENDIF}

{$IFDEF WIN}
function SetClipboardText( Wnd: HWND; Value: string ): boolean;
var
  hData: HGlobal;
  pData: pointer;
  Len: integer;
begin
   Result := true;
   if OpenClipboard( Wnd ) then
   begin
      try
         Len := Length( Value ) + 1;
         hData := GlobalAlloc( GMEM_MOVEABLE or GMEM_DDESHARE, Len );
         try
            pData := GlobalLock( hData );
            try
               Move( PChar( Value )^, pData^, Len );
               EmptyClipboard;
               SetClipboardData( CF_TEXT, hData );
            finally
               GlobalUnlock( hData );
            end;
         except
            GlobalFree( hData );
            raise
         end;
      finally
         CloseClipboard;
      end;
   end
   else
      Result := false;
end;
{$ELSE}
procedure SetClipboardText(Wnd: longint; Str: string);
begin
  //
end;
{$ENDIF}

function CopyUtf8(From: UTF8String; Index: integer; Length: Integer): UTF8String;
begin

end;

function Min(f, d: Single): Single; overload;
begin
  if f > d then Result := d
    else Result := f;
end;

function Min(f, d: Integer): Integer; overload;
begin
  if f > d then Result := d
    else Result := f;
end;

function Max(f, d: Single): Single; overload;
begin
  if f < d then Result := d
    else Result := f;
end;

function Max(f, d: Integer): Integer; overload;
begin
  if f > d then Result := d
    else Result := f;
end;


function scissor_Rect(rct: zglTRect; clntRect, Ptr: zglPRect; var ne: boolean): boolean;
var Res: zglTRect;
begin
  if clntRect = nil then begin
    ne := false;
    Result := True;
    Exit;
  end;

  Res.X := Max(rct.X, clntRect^.X);
  Res.Y := Max(rct.Y, clntRect^.Y);
  Res.W := Min(rct.X + rct.W, clntRect^.X + clntRect^.W);
  Res.W := Res.W - Res.X;
  Res.H := Min(rct.Y + rct.H, clntRect^.Y + clntRect^.H);
  Res.H := Res.H - Res.Y;
  if Ptr <> Nil then
    Ptr^ := Res;
  if (Res.W > 0) and (Res.H > 0) then begin
    result := true;
    scissor_Begin(Round(Res.X), Round(Res.Y),
      Round(Res.W), Round(Res.H));
    ne := true;
  end else begin
    Result := False;
    ne := False;
  end;
end;

constructor zglTEditableClass.Create(pEditable: boolean = true);
begin
  if pEditable then
    Editable := emTrue
  else
    Editable := emNone;
  fSelfCount := 0;
  Data := nil;
end;

constructor zglTEditableClass.Create(pEditable: TEditableMethod = emTrue);
begin
  Editable := pEditable;
  fSelfCount := 0;
  Data := nil;
end;

constructor zglTNamedObject.Create(pEditable: boolean = true);
begin
  inherited;
  OnNameChange := nil;
end;

constructor zglTNamedObject.Create(pEditable: TEditableMethod = emTrue);
begin
  inherited;
  OnNameChange := nil;
end;

procedure zglTEditableClass.UseCounter;
begin
  Inc(fSelfCount, Count);
end;

procedure zglTEditableClass.Drop;
begin
  Dec(fSelfCount);
  if SelfCount <= 0 then
    Free;
end;

constructor TStretch.Create;
begin
  inherited Create(True);
end;

constructor zglTCRect.Create;
begin
  inherited Create(True);
end;

// zglTBaseGUIObject

constructor zglTBaseGUIObject.CreateDefaults(pGui: zglTGui);
begin
  Gui := pGui;
end;

constructor zglTBaseGUIObject.Create;
begin
  Gui := pGui;
end;

procedure zglTBaseGUIObject.setFont(fontName: zglTFontContainer);
begin
  if (fFont <> fontName) then begin
    if Assigned(fFont) then
      fFont.Drop;
    fFont := fontName;
    if Assigned(fFont) then
      fFont.UseCounter;
  end;
end;

function zglTBaseGUIObject.getFont: zglTFontContainer;
begin
  if Assigned(fFont) then
    result := fFont
  else
    result := Self.Gui.fFont;
end;

function zglTBaseGUIObject.HandleUpdate;
begin
  Result := true;
end;

procedure zglTBaseGUIObject.HandleLeave;
begin
  //
end;

procedure zglTBaseGUIObject.HandleBegin;
begin
  //
end;

procedure zglTBaseGUIObject.HandleDraw;
begin
  //
end;

// zglTGUIObject

function zglTGUIObject.HandleUpdate;
var r: zglTRect;
begin
  result := inherited HandleUpdate(dt, evnt);
  case evnt of
    heMoveObject: begin
      r := updateRect;
      Move(mouse_X - fOffsetX, mouse_Y - fOffsetY);
      Result := not isHover(@r);
      if not mouse_Down(M_BLEFT) then begin
        Self.Gui.Handler.Clear;
        UpdateAlign;
        //fPressed := false;
        Result := true;
      end;
    end;
    heResizeObject: begin
      Result := false;
      r := updateRect(fDtX, fDtY);
      if fCorner and 1 > 0 then begin
        Resize(fOffsetX - mouse_X, fRect.H);
        Move(fOffsetX - r.W + Rect.X - r.X, fRect.Y);
      end else begin
        Resize(mouse_X - r.X, fRect.H);
      end;
      if fCorner and 2 > 0 then begin
        Resize(fRect.W, fOffsetY - mouse_Y);
        Move(fRect.X, fOffsetY - r.H + Rect.Y - r.Y);
      end else begin
        Resize(fRect.W, mouse_Y - r.Y);
      end;
      if fRect.W < 8 then fRect.W := 8;
      if fRect.H < 8 then fRect.H := 8;

      if not mouse_Down(M_BLEFT) then begin
        Self.Gui.Handler.Clear;
        fPressed := False;
        UpdateAlign;
        Result := true;
      end;
    end;
  end;
end;

function zglTGUIObject.updateRect;
begin
  Result := fRect.GetRect;
  Result.X := fRect.X + oX;
  Result.Y := fRect.Y + oY;
end;

function zglTGUIObject.updateOffset;
begin
  result.W := r.W - offsetX * 2;
  result.H := r.H - offsetY * 2;
  result.X := r.X + offsetX;
  result.Y := r.Y + offsetY;
end;

procedure zglTGUIObject.Move;
begin
  fRect.X := pX; fRect.Y := pY;
  if Assigned(fOnMove) then begin
    fOnMove(Self);
  end;
end;

procedure zglTGUIObject.setCaption(capt: TCaption);
begin
  fCaption := capt;
  if Assigned(fOnCaptionChange) then
    fOnCaptionChange(Self);
end;

procedure zglTNamedObject.setName(capt: TCaption);
var i: integer;
    AnsiCapt: AnsiString;
begin
  AnsiCapt := AnsiString(Capt);
  if capt = '' then
    exit;
  for i := 1 to Length(AnsiCapt) do
    if (not (AnsiCapt[i] in ['a'..'z', 'A'..'Z', '_', '0'..'9'])) or
      ((AnsiCapt[i] in ['0'..'9']) and (i = 1)) then
        Exit;
  fName := Capt;
  if Assigned(fOnNameChange) then
    fOnNameChange(zglTGUIObject(Self));
end;

procedure zglTGUIObject.setAlign;
begin
  fAlign := a;
  UpdateAlign;
end;

procedure zglTGUIObject.updateSkin(skin: zglTGuiSkin);
begin
  //
end;

{$IFDEF GUI_USE_POPUPMENU}
procedure zglTGUIObject.setPopupMenu(menuName: zglTPopupMenu);
begin
  if (fPopupMenu <> menuName) then begin
    if Assigned(fPopupMenu) then
      fPopupMenu.Drop;
    fPopupMenu := menuName;
    if Assigned(fPopupMenu) then
      fPopupMenu.UseCounter;
  end;
end;


{$ENDIF}

procedure zglTGUIObject.Resize;
begin
  fRect.W := Max(pW, fMinWidth);
  fRect.H := Max(pH, fMinHeight);
  if Assigned(fOnResize) then
    fOnResize(self);
end;

procedure zglTGUIObject.UpdateAlign;
begin
  if (fAlign <> caNone) and (Assigned(fParent)) then begin
    case fAlign of
      caLeft: begin
        Move(0, 0);
        Resize(fRect.W, fParent.ClientRect.H);
      end;
      caTop: begin
        Move(0, 0);
        Resize(fParent.ClientRect.W, fRect.H);
      end;
      caRight: begin
        Move(fParent.ClientRect.W - fRect.W, 0);
        Resize(fRect.W, fParent.ClientRect.H);
      end;
      caBottom: begin
        Move(0, Parent.ClientRect.H - fRect.H);
        Resize(fParent.ClientRect.W, fRect.H);
      end;
      caClient: begin
        Move(0, 0);
        Resize(fParent.ClientRect.W, fParent.ClientRect.H);
      end;
    end;
    Resize(fRect.W, fRect.H);
  end;
end;

procedure zglTGUIObject.Show;
begin
  fVisible := true;
end;

procedure zglTGUIObject.Hide;
begin
  fVisible := false;
  if Self.Gui.Modal.Find(Self) <> -1 then
    Self.Gui.Modal.Remove(Self, false);
end;

function zglTGUIObject.GetOffectRect(r: zglTRect; oX, oY: integer): zglTRect;
begin
  Result.X := r.X + oX;
  Result.Y := r.Y + oY;
  Result.W := r.W - oX * 2;
  Result.H := r.H - oY * 2;
end;

function zglTGUIObject.getEnabled: Boolean;
begin
  if Assigned(Parent) then
    Result := fEnabled and Parent.Enabled
  else
    Result := fEnabled;
end;

procedure zglTGUIObject.ShowModal;
begin
  Self.Gui.Modal.Add(Self);
  Show;
end;

function zglTGUIObject.getRootFont;
var obj: zglTGUIObject;
    fnt: zglTFontContainer;
begin
  obj := Self;
  fnt := obj.fFont;
  while not Assigned(fnt) do begin
    obj := obj.Parent;
    if Assigned(obj) then
      fnt := obj.fFont
    else
      fnt := Self.Gui.Font;
  end;
  if enabled then
    case pActive of
      true: Result := fnt.GetForState(fstActive);
      else Result := fnt.GetForState(fstNormal);
    end
  else
    Result := fnt.GetForState(fstDisabled);
end;

{$IFDEF GUI_USE_EDITOR}
function zglTGuiObject.getVisible: boolean;
begin
  result := fVisible or (Gui.ForceVisible and ForceVisible);
end;
{$ENDIF}

function zglTGUIObject.isHover;
var r: zglTRect;
begin
  if not Enabled then begin
    Result := false;
    exit;
  end;
  r := updateRect(oX, oY);
  result := col2d_PointInRect(Mouse_X, Mouse_Y, r);
  if Assigned(upr) then
    Result := Result and col2d_PointInRect(Mouse_X, Mouse_Y, upr^);
end;

procedure zglTGUIObject.OnAddMe;
begin
  //
end;

procedure zglTGUIObject.DrawEdit;
var r: zglTRect;
    skinTx: zglPGuiSkinTex;
    sciEnd: Boolean;
    {$IFDEF GUI_USE_EDITOR}
    i, j: integer;
    w: single;
    fnt: zglTFontObject;
    {$ENDIF}
begin
  if not Visible then Exit;
  if fEditMode  then begin
    r := updateRect(oX, oY);
    skinTx := Self.Gui.Skin.SkinTex[skinEditPoint];
    if (Gui.Handler.HObject = self) and (Gui.Handler.HHandle = heResizeObject) then exit;
    pr2d_Rect(r.X, r.Y, r.W, r.H, $FFFFFF, 32);
    Self.Gui.Skin.DrawTex(skinEditPoint,
      r.X, r.Y,
      0, 0, skinTx^.W, skinTx^.H);
    Self.Gui.Skin.DrawTex(skinEditPoint,
      r.X + r.W - skinTx^.W, r.Y,
      0, 0, skinTx^.W, skinTx^.H);
    Self.Gui.Skin.DrawTex(skinEditPoint,
      r.X, r.Y + r.H - skinTx^.H ,
      0, 0, skinTx^.W, skinTx^.H);
    Self.Gui.Skin.DrawTex(skinEditPoint,
      r.X + r.W - skinTx^.W, r.Y + r.H - skinTx^.H,
      0, 0, skinTx^.W, skinTx^.H);
    {$IFDEF GUI_USE_EDITOR}
    // Force visible
    if Visible and (not fVisible) then begin
      if scissor_Rect(r, @r, nil, sciEnd) then begin
        fx_SetBlendMode(FX_BLEND_ADD, true);
        for j := 0 to round(r.H) div 4 do
          for i := 0 to round(r.W) div 4 do
            if (j + i) mod 2 = 0 then
               pr2d_Rect(r.X + i * 4, r.Y + j * 4, 4, 4, $FFFFFF, 32, PR2D_FILL or FX_BLEND);
        fx_SetBlendMode(FX_BLEND_NORMAL, false);
        fnt := getRootFont(Gui.Handler.hObject = self);
        w := text_GetWidth(fnt.Data, 'invisible');
        pr2d_Rect(r.X + 1, r.Y + 1, w * 0.75 + 1, text_GetHeight(fnt.Data, w, 'invisible') * 0.75, 0, 255, PR2D_FILL);
        text_DrawEx(fnt.Data, r.X + 1, r.Y + 1, 0.75, 0, 'invisible', 255, $FFFFFF, TEXT_VALIGN_TOP or TEXT_HALIGN_LEFT);
        if sciEnd then
          scissor_End;
      end;
    end;
    {$ENDIF}
  end;
end;

procedure zglTGUIObject.Update;
var nowIsHover: Boolean;
    r: zglTRect;
    pX, pY: Integer;
    skinTx: zglPGuiSkinTex;
begin
  if not Visible then Exit;

  nowIsHover := false;
  if (Gui.Handler.canHover) then begin
    if isHover(clntRect, oX, oY) and (Gui.fHover = nil) then begin
      nowIsHover := true;
      if fCatchMouseClick then
        Self.Gui.fHover := self;
    end;
  end;
  fJustPressed := false;
  fRootPos.X := oX;
  fRootPos.Y := oY;
  r := updateRect(oX, oY);
  if nowIsHover then begin
{$IFDEF GUI_USE_POPUPMENU}
    if mouse_Click(M_BRIGHT) then begin
      if Assigned(PopupMenu) then begin
        fPressed := true;
        fJustPressed := true;
        Self.Gui.Handler.Clear;
        if fCatchMouseClick then
          mouse_ClearState;
        PopupMenu.Popup(self, mouse_X, mouse_Y);
      end;
    end;
{$ENDIF}
    if mouse_Click(M_BLEFT) then begin
      if fEditMode then begin
        if not fEvenNotMove then begin
          fDtX := oX; fDtY := oY;
          pX := mouse_X - round(r.X);
          pY := mouse_Y - round(r.Y);
          skinTx := Self.Gui.Skin.SkinTex[skinEditPoint];
          fCorner := Byte((pX <= skinTx^.W) and (pY <= skinTx^.H)) * 4 +
                Byte((pX >= r.W - skinTx^.W) and (pY <= skinTx^.H)) * 3 +
                Byte((pX >= r.W - skinTx^.W) and (pY >= r.H - skinTx^.H * 2)) * 1 +
                Byte((pX <= skinTx^.W) and (pY >= r.H - skinTx^.H)) * 2;
          if fCorner > 0 then begin
            Dec(fCorner);

            if fCorner and 1 > 0 then
              fOffsetX := round(r.X + r.W)
            else
              fOffsetX := round(fRect.X);

            if fCorner and 2 > 0 then
              fOffsetY := round(r.Y + r.H)
            else
              fOffsetY := round(fRect.H);
            Self.Gui.Handler.HandleEvent(Self, heResizeObject);
          end else begin
            fOffsetX := mouse_X - round(Rect.X);
            fOffsetY := mouse_Y - round(Rect.Y);
            Self.Gui.Handler.HandleEvent(Self, heMoveObject);
          end;
          if fCatchMouseClick then
            mouse_ClearState;
          fPressed := true;
          fJustPressed := false;
        end;
      end else begin
        fPressed := true;
        fJustPressed := true;
        Self.Gui.Handler.Clear;
        if fCatchMouseClick then
          mouse_ClearState;
      end;
    end;
    if ((fMouse_X <> Mouse_X) or (fMouse_Y <> Mouse_Y))
      and Assigned(OnMouseMove) then begin
      OnMouseMove(Self, mouse_X - round(r.X), Mouse_Y - round(r.Y));
    end;
    fMouse_X := Mouse_X; fMouse_Y := Mouse_Y;
  end;
  if nowIsHover xor fHover then begin
    if nowIsHover then begin
      fMouse_X := -1; fMouse_Y := -1;
      if Assigned(OnMouseEnter) then
        OnMouseEnter(Self, mouse_X - round(r.X), Mouse_Y - round(r.Y));
    end else begin
      if Assigned(OnMouseLeave) then
        OnMouseLeave(Self, fmouse_X - round(r.X), fMouse_Y - round(r.Y));
    end;
    fHover := nowIsHover;
  end;
  if fPressed and (not mouse_Down(M_BLEFT)) then begin
    if nowIsHover then begin
      if Assigned(OnClick) then begin
        OnClick(self, Round(mouse_X - fRootPos.X), Round(mouse_Y - fRootPos.Y));
      end;
    end;
    fPressed := false;
  end;
end;

constructor zglTGUIObject.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  fPressed := False; fHover := false;  fTag := 0;
  fRect := zglTCRect.Create;
  fCatchMouseClick := true;
  fStretch := TStretch.Create;
  fStretch.STR_LEFT := false;
  fStretch.STR_TOP := false;
  fStretch.STR_RIGHT := false;
  fStretch.STR_BOTTOM := false;
  fAlign := caNone;

  Mouse := MOUSE_POINTER;

  {$IFDEF GUI_USE_EDITOR}
     ForceVisible := false;
  {$ENDIF}
  fMinWidth := 1; fMinHeight := 1;
  Visible := true;
  Enabled := True;
  fEditMode := false; fEvenNotMove := False;
  fOnCaptionChange := nil;

  fNeedScroll := false;
  fName := 'Noname' + Copy(TCaption(ClassName), 5, Length(ClassName)) +
    u_IntToStr(Gui.Counter); fContainer := false;
end;

constructor zglTGUIObject.Create;
begin
  CreateDefaults(pGui);
  fRect.X := pX; fRect.Y := pY; fRect.W := pW; fRect.H := pH;
  Visible := pVisible;
end;

destructor zglTGUIObject.Destroy;
begin
  fRect.Free;
  fStretch.Free;
  Font := nil;
  inherited;
end;

// zglTSomeFont

constructor zglTFontObject.Create;
begin
  inherited Create(True);
  Data := pData; Size := pSize;
  Color := pColor; Alpha := pAlpha;
  Align := pAlign;
end;

constructor zglTFontObject.CreateDefaults;
begin
  inherited Create(True);
  Data := nil;
  Size := 1;
  Color := 0;
  Alpha := 255;
  Align := faMiddleCenter;
end;

// zgltTLabel
{$IFDEF GUI_USE_LABEL}
procedure zglTLabel.Draw;
var fnt: zglTFontObject;
    rct: zglTRect;
    sciEnd: boolean;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);
  if scissor_Rect(rct, clntRect, nil, sciEnd) then begin
    fnt := getRootFont(fHover);
    text_DrawInRectEx(fnt.Data, rct, fnt.Size, 0, Caption,
      fnt.Alpha, fnt.Color, FontAlignes[fnt.Align] or TEXT_CLIP_RECT);
    if sciEnd then
      scissor_End;
  end;
  inherited DrawEdit(oX, oY);
end;

procedure zglTLabel.Update;
begin
  if not Visible then Exit;
  inherited;
end;

constructor zglTLabel.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  Caption := pCaption;
end;

constructor zglTLabel.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  Caption := '';
end;
{$ENDIF}
// zglTEdit
{$IFDEF GUI_USE_EDIT}
procedure zglTEdit.Draw;
var fnt: zglTFontObject;
    rct: zglTRect;
    cnt: integer;
    i, ipart, w, off, f, selStart, selWidth, selPos: single;
    s: TCaption;
    skinTx: zglPGuiSkinTex;
    sciEnd: boolean;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);
  if scissor_Rect(rct, clntRect, nil, sciEnd) then begin
    s := Caption;
    if PasswordChar <> #0 then begin
      s := '';
      for cnt := 1 to utf8_Length(Caption) do
        s := s + PasswordChar;
    end;
    fnt := getRootFont(Gui.Handler.hObject = self);
    skinTx := Self.Gui.Skin.SkinTex[skinEdit];
    Self.Gui.Skin.DrawField(skinEdit, rct.X, rct.Y, rct.W, rct.H,
      0, 0, skinTx^.W div 2, skinTx^.H div 2);
    rct := updateOffset(rct, skinTx^.OffX, skinTx^.OffY);
    off := TextOffset;
    case Align of
      TEXT_RIGHT: f := 0;
      TEXT_CENTER: f := 0.5;
      else f := 1;
    end;

    i := (rct.W) * (1 - f);
    ipart := (rct.W - text_getWidth(fnt.Data, s) * fnt.Size) * (1 - f) + TextOffset;
    if SelectionStart > 0 then begin
      w := i + text_getWidth(fnt.Data, utf8_Copy(s, 1, SelectionStart))
        * fnt.Size * f;
    end else begin
      w := i;
    end;
    text_DrawEx(fnt.Data, round(rct.X + i + off), round(rct.Y + rct.H/2),
      fnt.Size, 0, s, fnt.Alpha, fnt.Color, Align);

    if Selection and (Gui.Handler.hObject = self) then begin
      if SelectionStart > 0 then
        selStart := ipart +
          text_getWidth(fnt.Data, utf8_Copy(s, 1, SelectionStart)) * fnt.Size
      else
        selStart := ipart;
      selWidth := text_getWidth(fnt.Data, utf8_Copy(s, SelectionStart + 1,
          SelectionEnd - SelectionStart))
        * fnt.Size;
      pr2d_rect(
        round(rct.X) + selStart,
        round(rct.Y),
        selWidth,
        rct.H, Self.Gui.Skin.SkinCfg[cfgSelectionColor], 255, PR2D_FILL);
      text_DrawEx(fnt.Data,
        round(rct.X + selStart + selWidth * (1 - f)), round(rct.Y + rct.H/2),
        fnt.Size, 0, utf8_Copy(s, SelectionStart + 1,
          SelectionEnd - SelectionStart),
        fnt.Alpha, $FFFFFF - fnt.Color, Align);
    end;

    if (Gui.Handler.hObject = self) and (fUpd < 250) then begin
      if SelectionPos > 0 then
        selPos := ipart +
          text_getWidth(fnt.Data, utf8_Copy(s, 1, SelectionPos)) * fnt.Size
      else
        selPos := ipart;
      pr2d_line(rct.X + selPos, rct.Y, rct.X + selPos, rct.Y+ rct.H,
        Self.Gui.Skin.SkinCfg[cfgEditLine], 255);
    end;
    if sciEnd then
      scissor_End;
  end;
  inherited DrawEdit(oX, oY);
end;

function zglTEdit.GetCursorPos(Id: integer): integer;
var fnt: zglTFontObject;
begin
  fnt := getRootFont(Gui.Handler.hObject = self);
  Result := round(text_getWidth(fnt.Data,
    utf8_Copy(fCaption, 1, id)) * fnt.Size);
end;

function zglTEdit.GetSelection(oX, oY: Single): integer;
var i, f, w, acc: single;
    j, l, last, id: integer;
    ch: longword;
    rct: zglTRect;
    fnt: zglTFontObject;
begin
  case Align of
      TEXT_RIGHT: f := 0;
      TEXT_CENTER: f := 0.5;
      else f := 1;
    end;

    rct := updateRect(oX, oY);
    fnt := getRootFont(Gui.Handler.hObject = self);
    i := (rct.W - text_getWidth(fnt.Data, fCaption) * fnt.Size) * (1 - f);
    acc := 0;

    if (Mouse_X - TextOffset) < rct.X + i then begin
      Result := 0;
      exit;
    end;

    l := Length(fCaption);

    j := 1; id := 0;

    while j <= l do begin
      last := j;
      ch := utf8_GetID(fCaption, last, @j);
      if (fnt.Data.CharDesc[ch] <> nil) then
        w := fnt.Data.CharDesc[ch].ShiftP * fnt.Size
      else
        w := 0;
      if (Mouse_X - TextOffset) <= round(rct.X + i + acc + w / 2 - (f - 1)) then begin
        Result := id;
        exit;
      end;
      acc := acc + w;
      inc(id);
    end;
    Result := l;
end;

procedure zglTEdit.Update;
var b: Boolean;
begin
  if not Visible then Exit;
  b := mouse_DblClick(M_BLEFT);
  inherited;
  if fJustPressed then begin
    Selection := false;
    SelectionPos := GetSelection(oX, oY);
    Focus;
    SelectionBegin := SelectionPos;
    OffsetX := round(oX);
    OffsetY := round(oY);
    MouseDropped := b;
    if b then
      SelectAll;
  end;
end;

procedure zglTEdit.setCaption;
begin
  inherited;
  if Assigned(fOnChange) then
    fOnChange(Self);
  fLastVal := capt;
end;

constructor zglTEdit.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  Align := pAlign;
  Caption := pCaption;
  LeftCaption := Caption;
  SelectionPos := 65535;
  RightCaption := '';
  MaxLength := -1;
  PasswordChar := #0;
  fOnChange := nil;
  fOnKeyDown := nil;
  fOnEnterKey := nil;
  LastVal := pCaption;
  LastLeftVal := pCaption;
  TextOffset := 0;
  Mouse := MOUSE_EDIT;
end;

constructor zglTEdit.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  Align := TEXT_LEFT;
  Caption := '';
  MaxLength := -1;
  PasswordChar := #0;
  fOnChange := nil;
  fOnKeyDown := nil;
  fOnEnterKey := nil;
  SelectionPos := 65535;
  Selection := false;
  LastVal := '';
  LastLeftVal := '';
  TextOffset := 0;
  Mouse := MOUSE_EDIT;
end;

procedure zglTEdit.Focus;
begin
  if SelectionPos <> 65535 then begin
    LeftCaption := utf8_Copy(Caption, 1, SelectionPos);
    RightCaption := utf8_Copy(Caption, SelectionPos + 1, utf8_Length(Caption));
  end else begin
    SelectionPos := utf8_Length(Caption);
    LeftCaption := Caption;
    RightCaption := '';
  end;
  fLastLeftVal := LeftCaption;
  key_clearState;
  key_EndReadText;
  key_BeginReadText(LeftCaption, fMaxLength);
  Self.Gui.Handler.HandleEvent(Self, heInputText);
end;

procedure zglTEdit.HandleDraw;
begin
  //
end;

procedure zglTEdit.HandleLeave;
begin
  case evnt of
    heInputText: begin
      LeftCaption := key_GetText;
      SelectionPos := utf8_Length(LeftCaption);
      Caption := LeftCaption + RightCaption;
      LeftCaption := '';
      RightCaption := '';
      key_EndReadText;
    end
  end;
end;

procedure zglTEdit.SelectAll;
begin
  LeftCaption := Caption;
  fLastLeftVal := LeftCaption;
  RightCaption := '';
  Selection := true;
  SelectionStart := 0;
  SelectionBegin := 0;
  SelectionEnd := utf8_Length(LeftCaption);
  SelectionPos := SelectionEnd;
  key_BeginReadText(LeftCaption);
end;

procedure zglTEdit.CopySelection;
var s: String;
begin
  if Selection then begin
    s := utf8_Copy(Caption, SelectionStart + 1, SelectionEnd - SelectionStart);
    SetClipboardText(zgl_Get(WINDOW_HANDLE), s);
  end;
end;

procedure zglTEdit.DeleteSelection;
begin
  if Selection then begin
    LeftCaption := utf8_Copy(Caption, 1, SelectionStart);
    RightCaption := utf8_Copy(Caption, SelectionEnd + 1, utf8_Length(Caption));
    SelectionPos := utf8_Length(LeftCaption);
    fCaption := LeftCaption + RightCaption;
    Selection := false;
  end;
end;

procedure zglTEdit.CheckSelection;
var
   cp, wdh, sll: integer;
   skinTx: zglPGuiSkinTex;
begin
  skinTx := Self.Gui.Skin.SkinTex[skinEdit];
  cp := GetCursorPos(SelectionPos);
  sll := GetCursorPos(utf8_Length(Caption));
  wdh := round(Rect.W - skinTx^.OffX * 2);

  if wdh < sll then begin
    if cp < -TextOffset then
      TextOffset := TextOffset + dt * 0.1
    else
    if cp > -TextOffset + wdh then
      TextOffset := TextOffset - dt * 0.1
    else
    if wdh > sll + TextOffset + 2 then
      TextOffset := TextOffset + dt * 0.1;

  end else
    TextOffset := 0;
end;

function zglTEdit.HandleUpdate;
var s: TCaption;
    bff: string;
    b: Boolean;
    cp: integer;
begin
  inherited HandleUpdate(dt, evnt);
  case evnt of
    heInputText: begin
      if not MouseDropped then begin
        SelectionPos := GetSelection(OffsetX, OffsetY);
        SelectionEnd := SelectionPos;
        Selection := SelectionPos <> SelectionBegin;
        if Selection then begin
          if SelectionPos > SelectionBegin then begin
            SelectionStart := SelectionBegin;
            SelectionEnd := SelectionPos;
          end else begin
            SelectionStart := SelectionPos;
            SelectionEnd := SelectionBegin;
          end;
        end;
        if not Mouse_Down(M_BLEFT) then begin
          MouseDropped := true;
        end;
      end;
      if key_Last(KA_DOWN) <> 0 then begin
        b := false;
        if key_Last(KA_DOWN) = K_ENTER then begin
          if Assigned(fOnEnterKey) then
            fOnEnterKey(Self);
          b := true;
        end else begin
          if Assigned(fOnKeyDown) then
            fOnKeyDown(Self, key_Last(KA_DOWN), b);
        end;
        if b then begin
          key_ClearState;
          key_BeginReadText(LeftCaption);
          Result := true;
          exit;
        end;
      end;
      if key_Down(K_CTRL) and key_Press(K_A) then begin
        key_ClearState;
        SelectAll;
      end;
      if key_Down(K_CTRL) and key_Press(K_C) then begin
        key_ClearState;
        CopySelection;
        key_BeginReadText(LeftCaption);
      end;
      if key_Down(K_CTRL) and key_Press(K_X) then begin
        key_ClearState;
        CopySelection;
        DeleteSelection;
        key_BeginReadText(LeftCaption);
      end;
      if key_Down(K_CTRL) and key_Press(K_V) then begin
        key_ClearState;
        GetClipboardText(zgl_Get(WINDOW_HANDLE), bff);
        DeleteSelection;
        LeftCaption := LeftCaption + TCaption(bff);
        Caption := LeftCaption + RightCaption;
        key_BeginReadText(LeftCaption);
      end;
      LeftCaption := key_GetText;
      if not Selection then
        SelectionPos := utf8_Length(LeftCaption);
      if (LeftCaption <> fLastLeftVal)
          or key_Press(K_BACKSPACE)
          or key_Press(K_DELETE) then begin
        if Selection then begin
          if utf8_Length(LeftCaption) > utf8_Length(fLastLeftVal) then
            s := utf8_Copy(LeftCaption,
              utf8_Length(fLastLeftVal) + 1,
              utf8_Length(LeftCaption))
          else
            s := '';
          LeftCaption := utf8_Copy(Caption, 1, SelectionStart) + s;
          RightCaption := utf8_Copy(Caption, SelectionEnd + 1, utf8_Length(Caption));
          Caption := LeftCaption + RightCaption;
          key_BeginReadText(LeftCaption);
          key_ClearState;
          SelectionPos := utf8_Length(LeftCaption);
          Selection := false;
        end;
        fLastLeftVal := LeftCaption;
          Caption := LeftCaption + RightCaption;
      end;

      if key_Press(K_LEFT) then begin
        cp := SelectionPos;
        if utf8_Length(LeftCaption) > 0 then begin
          fUpd := 0;
          RightCaption := utf8_Copy(LeftCaption, utf8_Length(LeftCaption), 1) + RightCaption;
          utf8_Delete(fLeftCaption, utf8_Length(LeftCaption), 1);
          SelectionPos := SelectionPos - 1;
          fLastLeftVal := LeftCaption;
          key_BeginReadText(LeftCaption);

          if key_Down(K_SHIFT) then begin
            if (cp > SelectionPos) then begin
              if not Selection then begin
                Selection := true;
                SelectionBegin := cp;
              end;
              if SelectionPos > SelectionBegin then begin
                SelectionStart := SelectionBegin;
                SelectionEnd := SelectionPos;
              end else begin
                SelectionStart := SelectionPos;
                SelectionEnd := SelectionBegin;
              end;
            end;
          end else Selection := false;

        end;
        key_clearState;
      end;
      if key_Press(K_RIGHT) then begin
        cp := SelectionPos;
        if utf8_Length(RightCaption) > 0 then begin
          fUpd := 0;
          LeftCaption := LeftCaption + utf8_Copy(RightCaption, 1, 1);
          utf8_Delete(fRightCaption, 1, 1);
          SelectionPos := SelectionPos + 1;
          fLastLeftVal := LeftCaption;
          key_BeginReadText(LeftCaption);
          if key_Down(K_SHIFT) then begin
            if (cp < SelectionPos) then begin
              if not Selection then begin
                Selection := true;
                SelectionBegin := cp;
              end;
              if SelectionPos > SelectionBegin then begin
                SelectionStart := SelectionBegin;
                SelectionEnd := SelectionPos;
              end else begin
                SelectionStart := SelectionPos;
                SelectionEnd := SelectionBegin;
              end;
            end;
          end else Selection := false;
        end;

        key_clearState;
      end;
      fCaption := LeftCaption + RightCaption;

      CheckSelection(dt);

      fUpd := fUpd + dt;
      if fUpd > 500 then fUpd := 0;
    end;
  end;

  Result := true;
end;
{$ENDIF}
// zglTComboBox
{$IFDEF GUI_USE_COMBOBOX}
procedure zglTComboBox.Draw;
var fnt: zglTFontObject;
    rct: zglTRect;
    skinTx: zglPGuiSkinTex;
    sciEnd: boolean;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);
  if scissor_Rect(rct, clntRect, nil, sciEnd) then begin
    fnt := getRootFont(Gui.Handler.hObject = self);
    skinTx := Self.Gui.Skin.SkinTex[skinComboBox];
    Self.Gui.Skin.DrawField(skinComboBox, rct.X, rct.Y, rct.W, rct.H,
      0, 0, skinTx^.W div 2, skinTx^.H div 2);
    rct := updateOffset(rct, skinTx^.OffX, skinTx^.OffY);
    skinTx := Self.Gui.Skin.SkinTex[skinComboBoxButton];
    Self.Gui.Skin.DrawTex(skinComboBoxButton,
      rct.X + rct.W - skinTx^.W, round(rct.Y + rct.H / 2 - skinTx^.H / 2),
      0, skinTx^.H * Byte(fHover), skinTx^.W, skinTx^.H);
    if fPressed then begin
      rct.X := rct.X + 1; rct.Y := rct.Y + 1;
    end;
    if sciEnd then
      scissor_End;
    rct.W := rct.W - skinTx^.W;
    if scissor_Rect(rct, clntRect, nil, sciEnd) then begin
      text_DrawInRectEx(fnt.Data, rct, fnt.Size, 0, Caption,
        fnt.Alpha, fnt.Color, FontAlignes[fnt.Align] or TEXT_CLIP_RECT);
      if sciEnd then
        scissor_End;
    end;
  end;
  inherited DrawEdit(oX, oY);
end;

procedure zglTComboBox.Update;
var rct: zglTRect;
    listTx: zglPGuiSkinTex;
    ic: integer;
begin
  if not Visible then Exit;
  inherited;
  if Selected >= Items.Count then
    Selected := Items.Count - 1;
  if (Items.Count = 0) then begin
    Caption := '';
  end else begin
    Caption := TCaption(Items[Selected]);
  end;

  if fJustPressed then begin
    fOffset := 0;
    fdOffset := 0;
    if Self.Gui.Handler.hObject = nil then begin
      listTx := Self.Gui.Skin.SkinTex[skinListItem];
      rct := updateRect(oX, oY);
      ic := Items.Count;
      if ic > ComboMax then ic := ComboMax;
      if (rct.Y + rct.H + ic * listTx^.H > Self.Gui.Rect.Y + Self.Gui.Rect.H) then begin
        OffsetY := round(ic * listTx^.H + rct.H);
        DropSideDown := false;
      end else begin
        DropSideDown := true;
        OffsetY := 0;
      end;
      //
      Self.Gui.Handler.HandleEvent(Self, heComboField);
    end else
      Self.Gui.Handler.Clear;
  end;
end;

constructor zglTComboBox.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  Items := TStringList.Create;
  fSelected := 0; fComboMax := pComboMax;  fOffset := 0;
  fOnChange := nil;
end;

constructor zglTComboBox.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  Items := TStringList.Create;
  fSelected := 0; fComboMax := 8; fOffset := 0;
  fOnChange := nil;
end;

destructor zglTComboBox.Destroy;
begin
  Items.Free;
  inherited;
end;

procedure zglTComboBox.HandleDraw;
var rct, r, dr: zglTRect;
    i, ic: integer;
    fnt: zglTFontObject;
    b: boolean;
    pX, pY, ScrollCoef: single;
    skinTx, listTx, dropTx: zglPGuiSkinTex;
    sciEnd: boolean;
begin
  case evnt of
    heComboField, heComboMove: begin
      rct := updateRect(fRootPos.X, fRootPos.Y);

      rct.Y := rct.Y + rct.H - OffsetY;
      ic := Items.Count;
      if ic > ComboMax then begin
        ic := ComboMax;
        b := True;
      end else b := False;

      listTx := Self.Gui.Skin.SkinTex[skinListItem];
      skinTx := Self.Gui.Skin.SkinTex[skinComboBoxScroll];
      dropTx := Self.Gui.Skin.SkinTex[skinComboBoxDrop];

      rct.H := ic * listTx^.H;

      r := rct; r.H := listTx^.H;

      ScrollCoef := m_Sin(round(PopupRect * 90));

      if not DropSideDown then begin
        rct.Y := rct.Y + rct.H * (1 - ScrollCoef);
      end else begin
        r.Y := r.Y - rct.H * (1 - ScrollCoef);
      end;
      rct.H := rct.H * ScrollCoef;
      if scissor_Rect(rct, @rct, nil, sciEnd) then begin

        Self.Gui.Skin.DrawField(skinComboBoxDrop, rct.X, rct.Y, rct.W, rct.H,
          0, 0, dropTx^.W div 2, dropTx^.H div 2);

        // need to draw scrollbars
        if b then begin
          // top scrollbar button
          pX := rct.X + rct.W - skinTx^.BtnW; pY := rct.Y;
          Self.Gui.Skin.DrawTex(skinComboBoxScroll, pX, pY,
            byte(col2d_PointInRect(mouse_X, mouse_Y,
              Self.Gui.getRect(pX, pY, skinTx^.BtnW, skinTx^.BtnH))
              and mouse_Down(M_BLEFT)) * skinTx^.BtnW,
            0, skinTx^.BtnW, skinTx^.BtnH);
          // middle space
          Self.Gui.Skin.DrawField(skinComboBoxScroll,
            pX, pY + skinTx^.BtnH,
              skinTx^.ScrW, rct.H - skinTx^.BtnH * 2,
            skinTx^.BtnW * 2, 0,
              skinTx^.ScrW div 2, (skinTx^.BtnH * 2 + skinTx^.ScrH) div 2);
          // bottom crollbar button
          pY := rct.Y + rct.H - skinTx^.BtnH;
          Self.Gui.Skin.DrawTex(skinComboBoxScroll, pX, pY,
            byte(col2d_PointInRect(mouse_X, mouse_Y,
              Self.Gui.getRect(pX, pY, skinTx^.BtnW, skinTx^.BtnH))
              and mouse_Down(M_BLEFT)) * skinTx^.BtnW,
            skinTx^.BtnH + skinTx^.ScrH, skinTx^.BtnW, skinTx^.BtnH);
          // middle scrollbar
          pY := rct.Y + skinTx^.btnH + fdOffset;
          Self.Gui.Skin.DrawTex(skinComboBoxScroll, pX, pY,
            byte(evnt = heComboMove) * skinTx^.ScrW,
            skinTx^.BtnH, skinTx^.ScrW, skinTx^.ScrH);
          r.W := r.W - skinTx^.ScrW;
          rct.W := rct.W - skinTx^.ScrW;
        end;

        for i := 0 to ic - 1 do begin
          b := col2d_PointInRect(mouse_X, mouse_Y, r) and (evnt = heComboField);
          if b or (i + fOffset = Selected) then begin

            Self.Gui.Skin.DrawField(skinListItem, r.X, r.Y, r.W, r.H,
              0, Byte(not b) * listTx^.H, listTx^.W div 2, listTx^.H div 2);
          end;
          fnt := getRootFont(i + fOffset = Selected);

          dr := GetOffectRect(r, dropTx^.OffX, dropTx^.OffY);

          text_DrawInRectEx(fnt.Data, dr, fnt.Size, 0,
            TCaption(Items[i + fOffset]), fnt.Alpha, fnt.Color,
            FontAlignes[fnt.Align] or TEXT_CLIP_RECT);
          r.Y := r.Y + listTx^.H;
        end;

        if sciEnd then
          scissor_End;
      end;
    end;
  end;
end;

procedure zglTComboBox.HandleBegin;
begin
  if evnt = heComboField then
    PopupRect := 0;
end;

procedure zglTComboBox.HandleLeave;
begin
  //
end;

function zglTComboBox.HandleUpdate;
var rct, r: zglTRect;
    ic: integer;
    pY: single;
    mU, mD: Boolean;
    skinTx, listTx: zglPGuiSkinTex;
begin
  Result := inherited HandleUpdate(dt, evnt);
  rct := updateRect(fRootPos.X, fRootPos.Y);
  rct.Y := rct.Y + rct.H - OffsetY;
  ic := Items.Count;

  if (PopupRect < 1) then begin
    PopupRect := PopupRect + dt * 0.005;
  end else
    PopupRect := 1;

  listTx := Self.Gui.Skin.SkinTex[skinListItem];
  skinTx := Self.Gui.Skin.SkinTex[skinComboBoxScroll];

  if ic > ComboMax then begin
    ic := ComboMax;
    rct.W := rct.W - skinTx^.ScrW;
    r.Y := rct.Y;
    r.X := rct.X + rct.W;
    r.W := skinTx^.scrW;
    r.H := ComboMax * listTx^.H;
    pY := rct.Y + skinTx^.BtnH + fdOffset;
    mU := mouse_Wheel(M_WUP);
    mD := mouse_Wheel(M_WDOWN);
    if col2d_PointInRect(mouse_X, mouse_Y, r) or mU or mD then begin
      Result := false;
      if mouse_Click(M_BLEFT) or mU or mD then begin
        // top
        if (mouse_Y - r.Y < skinTx^.BtnH) or mU then begin
          Dec(fOffset);
          if fOffset < 0 then
            fOffset := 0;
        end else
        // bottom
        if (r.Y + r.H - mouse_Y  < skinTx^.BtnH) or mD then begin
          Inc(fOffset);
          if fOffset > Items.Count - ComboMax then
            fOffset := Items.Count - ComboMax;
        end else
        // middle
        if (mouse_Y >= pY) and (mouse_Y <= pY + skinTx^.ScrH) then begin
          fsOffset := mouse_Y - Round(pY);
          Self.Gui.Handler.HandleEvent(self, heComboMove);
        end else begin
          fOffset := Trunc(((mouse_Y - r.Y - skinTx^.BtnH) /
            (r.H - skinTx^.BtnH * 2))
            * (Items.Count - ComboMax ));
        end;
        fdOffset := Round((fOffset / (Items.Count - ComboMax ))
          * (r.H - skinTx^.ScrH - skinTx^.BtnH * 2));
        if fCatchMouseClick then
          mouse_ClearState;
        exit;
      end;
    end else
      Result := true;
  end;
  rct.H := ic * listTx^.H;
  if col2d_PointInRect(mouse_X, mouse_Y, rct) then begin
    Result := False;
    if mouse_Click(M_BLEFT) then begin
      Result := True;
      Selected := trunc((mouse_Y - rct.Y) / listTx^.H) + fOffset;
      if Assigned(fOnChange) then
        fOnChange(Self);
      if fCatchMouseClick then
        mouse_ClearState;
       Self.Gui.Handler.Clear;
    end;
  end;
  case evnt of
    heComboMove: begin
      Result := false;
      fdOffset := round(mouse_Y - rct.Y - skinTx^.BtnH - fsOffset);
      if fdOffset < 0 then fdOffset := 0;
      if fdOffset > rct.H - skinTx^.BtnH * 2 - skinTx^.ScrH then
        fdOffset := round(rct.H - skinTx^.BtnH * 2 - skinTx^.ScrH);
      fOffset := Trunc((fdOffset / (rct.H - skinTx^.BtnH * 2 - skinTx^.ScrH))
        * (Items.Count - ComboMax ));
      if not mouse_Down(M_BLEFT) then begin
        Self.Gui.Handler.HandleEvent(self, heComboField);
      end;
    end;
  end;
end;
{$ENDIF}
// zglTButton
{$IFDEF GUI_USE_BUTTON}
procedure zglTButton.Draw;
var fnt: zglTFontObject;
    rct: zglTRect;
    i: integer;
    skinTx: zglPGuiSkinTex;
    sciEnd: boolean;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);
  if scissor_Rect(rct, clntRect, nil, sciEnd) then begin
    skinTx := Self.Gui.Skin.SkinTex[skinButton];
    if Enabled then
      i := Byte(fPressed) * 2 + Byte(fHover and (not fPressed))
    else
      i := 3;
    Self.Gui.Skin.DrawField(skinButton, rct.X, rct.Y, rct.W, rct.H,
      0, i * skinTx^.H, skinTx^.W div 2, skinTx^.H div 2);
    fnt := getRootFont(fHover);
    if fPressed then begin
      rct.X := rct.X + 1; rct.Y := rct.Y + 1;
    end;
    text_DrawInRectEx(fnt.Data, rct, fnt.Size,
      0, Caption, fnt.Alpha, fnt.Color, FontAlignes[fnt.Align]);
    if sciEnd then
      scissor_End;
  end;
  inherited DrawEdit(oX, oY);
end;

procedure zglTButton.Update;
begin
  if not Visible then Exit;
  inherited;
end;

constructor zglTButton.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  Caption := pCaption;
end;

constructor zglTButton.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  Caption := '';
end;
{$ENDIF}

// zglTMultiButton
{$IFDEF GUI_USE_MULTIBUTTON}
procedure zglTMultiButton.Draw;
var fnt: zglTFontObject;
    rct: zglTRect;
    i: integer;
    skinTx, dropTx: zglPGuiSkinTex;
    sciEnd, sciSubEnd: boolean;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);

  if scissor_Rect(rct, clntRect, nil, sciEnd) then begin
    skinTx := Self.Gui.Skin.SkinTex[skinButton];
    dropTx := Self.Gui.Skin.SkinTex[skinMultiButton];

    if mouse_X >= rct.X + rct.W - dropTx^.W then
      fPressed := false;

    if Enabled then begin
      if mouse_X < rct.X + rct.W - dropTx^.W then
        i := Byte(fPressed) * 2 + Byte(fHover and (not fPressed))
      else
        i := 0;
    end else
      i := 3;

    Self.Gui.Skin.DrawField(skinButton, rct.X, rct.Y, rct.W, rct.H,
        0, i * skinTx^.H, skinTx^.W div 2, skinTx^.H div 2);

    if Enabled then begin
      if mouse_X >= rct.X + rct.W - dropTx^.W then
        i := Byte(fPressed) * 2 + Byte(fHover and (not fPressed))
      else
        i := 0;
    end else
      i := 3;

    Self.Gui.Skin.DrawTex(skinMultiButton,
      round(rct.X + rct.W - dropTx^.W), round(rct.Y + rct.H / 2 - dropTx^.H / 2),
      i * dropTx^.W, 0, dropTx^.W, dropTx^.H);

    fnt := getRootFont(fHover);
    if fPressed then begin
      rct.X := rct.X + 1; rct.Y := rct.Y + 1;
    end;

    rct.W := max(rct.W - dropTx^.W, 0);
    if scissor_Rect(rct, clntRect, nil, sciSubEnd) then begin
      text_DrawInRectEx(fnt.Data, rct, fnt.Size,
        0, Caption, fnt.Alpha, fnt.Color, FontAlignes[fnt.Align] or TEXT_CLIP_RECT);
      if sciSubEnd then
        scissor_End;
    end;
    if sciEnd then
      scissor_End;
  end;
  inherited DrawEdit(oX, oY);
end;

procedure zglTMultiButton.Update;
var rct: zglTRect;
    rightSide: Boolean;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);
  rightSide := mouse_X >= rct.X + rct.W - Self.Gui.Skin.SkinTex[skinMultiButton]^.W;
  if rightSide then begin
    fPressed := false;
  end;
  inherited;
  if JustPressed and rightSide then begin
    DropMenu.Popup(Self, round(rct.X), Round(rct.Y + rct.H), round(Rect.W));
  end;
end;

constructor zglTMultiButton.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  Caption := pCaption;
  MinWidth := Self.Gui.Skin.SkinTex[skinMultiButton].W;
  MinHeight := Self.Gui.Skin.SkinTex[skinMultiButton].H;

  DropMenu := zglTPopupMenu.Create(Gui, Round(pW));
end;

constructor zglTMultiButton.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  Caption := '';
  MinWidth := Self.Gui.Skin.SkinTex[skinMultiButton].W;
  MinHeight := Self.Gui.Skin.SkinTex[skinMultiButton].H;

  DropMenu := zglTPopupMenu.Create(Gui, Round(Rect.W));
end;

destructor zglTMultiButton.Destroy;
begin
  inherited;
  DropMenu.Free;
end;
{$ENDIF}

// zglTCheckBox
{$IFDEF GUI_USE_CHECKBOX}
constructor zglTCheckBox.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  fChecked := false;
  Caption := pCaption;
end;

constructor zglTCheckBox.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  Checked := false;
  Caption := '';
end;

procedure zglTCheckBox.Draw;
var
    rct: zglTRect;
    fnt: zglTFontObject;
    i: integer;
    checkSize: single;
    skinTx: zglPGuiSkinTex;
    sciEnd: boolean;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);
  if scissor_Rect(rct, clntRect, nil, sciEnd) then begin
    if Enabled then
      i := 2 * Byte(fHover xor fPressed) + Byte(not Checked)
    else
      i := 4 + Byte(not Checked);
    fnt := getRootFont(fHover);
    skinTx := Self.Gui.Skin.SkinTex[skinCheckBox];
    checkSize := min(skinTx^.H, rct.H);
    Self.Gui.Skin.DrawTexScaled(skinCheckBox, rct.X, rct.Y + rct.H / 2 - checkSize /2,
      min(skinTx^.W, rct.H), checkSize,
      0, i * skinTx^.H, skinTx^.W, skinTx^.H);
    rct.X := rct.X + checkSize; rct.W := rct.W - skinTx^.W;
    if fPressed then begin
      rct.X := rct.X + 1; rct.Y := rct.Y + 1;
    end;
    text_DrawInRectEx(fnt.Data, rct, fnt.Size, 0,
      Caption, fnt.Alpha, fnt.Color, TEXT_LEFT or TEXT_CLIP_RECT);
    if sciEnd then
      scissor_End;
  end;
  inherited DrawEdit(oX, oY);
end;

procedure zglTCheckBox.Update;
begin
  if not Visible then Exit;
  inherited;
  if fJustPressed then begin
    Checked := not Checked;
  end;
end;
{$ENDIF}
// zglTFrame
constructor zglTFrame.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  with fBorder do begin
    X := 0; Y := 0; W := 0; H := 0;
  end;
  UpdateClientRect;
  with fScroll do begin
    X := 0; Y := 0;
  end;
  fItems := zglTGUIObjectsList.Create(pGui, self);
  fCanMove := false;
  fCanResize := false;
  fContainer := true;
  fScrollHeight := 0;
  fScrollWidth := 0;
end;

constructor zglTFrame.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  with fBorder do begin
    X := 0; Y := 0; W := 0; H := 0;
  end;
  UpdateClientRect;
  with fScroll do begin
    X := 0; Y := 0;
  end;
  fItems := zglTGUIObjectsList.Create(pGui, self);
  fCanMove := false;
  fCanResize := false;
  fContainer := true;
  fScrollHeight := 0;
  fScrollWidth := 0;
end;

constructor zglTFrame.CreateFullScreen;
begin
  inherited Create(pGui, 0, 0, pGui.fRect.W, pGui.fRect.H, pVisible);
  with fBorder do begin
    X := 0; Y := 0; W := 0; H := 0;
  end;
  UpdateClientRect;
  with fScroll do begin
    X := 0; Y := 0;
  end;
  fItems := zglTGUIObjectsList.Create(pGui, self);
  fCanMove := false;
  fCanResize := false;
  fContainer := true;
  fScrollHeight := 0;
  fScrollWidth := 0;
end;

procedure zglTFrame.FullScreen;
begin
  Resize(gui.fRect.W, Self.Gui.fRect.H);
end;

procedure zglTFrame.onAdd;
begin
  Sender.UpdateAlign;
end;

procedure zglTFrame.onRemove;
begin
  //
end;

procedure zglTFrame.HandleDraw;
begin
  //
end;

procedure zglTFrame.HandleLeave;
begin
  //
end;

function zglTFrame.HandleUpdate;
begin
  Result := inherited HandleUpdate(dt, evnt);
end;

destructor zglTFrame.Destroy;
begin
  fItems.Free;
  inherited;
end;

procedure zglTFrame.Resize;
var i: Integer;
    obj: zglTGUIObject;
begin
  pW := Max(pW, fMinWidth); pH := Max(pH, fMinHeight);
  for i := 0 to Items.Count - 1 do begin
    obj := Items.Items[i];
    if obj.Stretch.STR_TOP then begin
      obj.Move(obj.Rect.X, obj.Rect.Y + (pH - Rect.H));
    end;
    if obj.Stretch.STR_LEFT then begin
      obj.Move(obj.Rect.X + (pW - Rect.W), obj.Rect.Y);
    end;
    if (obj.Stretch.STR_BOTTOM) and (not (obj.Stretch.STR_TOP)) then begin
      obj.Resize(obj.Rect.W, obj.Rect.H + (pH - Rect.H));
    end;
    if (obj.Stretch.STR_RIGHT) and (not (obj.Stretch.STR_LEFT)) then begin
      obj.Resize(obj.Rect.W + (pW - Rect.W), obj.Rect.H);
    end;
  end;
  inherited;
  for i := 0 to Items.Count - 1 do
    zglTGUIObject(Items.Items[i]).UpdateAlign;
end;

procedure zglTFrame.MoveToCenter;
begin
  fRect.X := Round(gui.fRect.W / 2 - Rect.W / 2);
  fRect.Y := Round(gui.fRect.H / 2 - Rect.H / 2);
end;

procedure zglTFrame.Draw;
var i: integer;
    r, rct, fUpdateRect: zglTRect;
    tmp: zglPRect;
    sciEnd: boolean;
begin
  if not Visible then Exit;
  r := updateRect(oX + fClientRect.X, oY + fClientRect.Y);
  fUpdateRect := Self.Gui.GetRect(r.X, r.Y,
                     fClientRect.W, fClientRect.H);
  for i := 0 to fItems.Count - 1 do begin
    if scissor_Rect(fUpdateRect, clntRect, @rct, sciEnd) then begin
      if clntRect = nil then
        tmp := nil
      else
        tmp := @rct;
      fItems.Items[i].Draw(tmp, r.X + fScroll.X,
                           r.Y + fScroll.Y);
      if sciEnd then
        scissor_End;
    end;
  end;
  if Assigned(fOnDraw) then
    fOnDraw(Self, oX + fRect.X + fClientRect.X,
                  oY + fRect.Y + fClientRect.Y,
                  fClientRect.W, fCLientRect.H,
                  fScroll.X, fScroll.Y);
  DrawEdit(oX, oY);
  inherited;
end;

procedure zglTFrame.Update;
var i: integer;
    r, fUpdateRect: zglTRect;
    skinTx: zglPGuiSkinTex;
begin
  if not Visible then Exit;
  r := updateRect(oX + ClientRect.X, oY + ClientRect.Y);
  fUpdateRect := Self.Gui.GetRect(r.X, r.Y,
                     ClientRect.W, ClientRect.H);
  if updateChind and Enabled then
    for i := fItems.Count - 1 downto 0 do begin
      fItems.Items[i].Update(dt, @fUpdateRect, true,
        r.X + fScroll.X, r.Y + fScroll.Y);
    end;
  inherited;
  if Assigned(Gui) then
    if fJustPressed then begin
      skinTx := Self.Gui.Skin.SkinTex[skinFormResize];
      r :=  Self.Gui.GetRect(Rect.X + oX + Rect.W - skinTx^.W,
                        Rect.Y + oY + Rect.H - skinTx^.H,
                     skinTx^.W, skinTx^.H);
      if CanResize and isHover(@r, oX, oY) then begin
        fCorner := 0;
        Self.Gui.Handler.HandleEvent(Self, heResizeObject);
      end else
      if CanMove and (Gui.Handler.hObject <> Self) then begin
        fOffsetX := mouse_X - round(Rect.X);
        fOffsetY := mouse_Y - round(Rect.Y);
        Self.Gui.Handler.HandleEvent(Self, heMoveObject);
      end;

      if fCatchMouseClick then
        mouse_ClearState;
    end;
end;
// zglTForm
constructor zglTForm.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  Caption := pCaption;
  CanMove := false;
  CanResize := False;
  CloseButton := false;
  fMinWidth := 1;
  fMinHeight := 1;
  fOnClose := nil;
  CaptionAlign := faMiddleLeft;

  fDisplayEffect := deNone;

{$IFDEF GUI_USE_MAINMENU}
  fMainMenu := nil;
{$ENDIF}
  updateSkin(Gui.Skin);
end;

procedure zglTForm.Show;
var de: TDisplayEffect;
begin
  inherited;
  if DisplayEffect <> deNone then begin
    de := DisplayEffect;
    DisplayEffect := deNone;
    DisplayEffect := de;
  end;
end;

constructor zglTForm.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  Caption := '';
  CanMove := false;
  CanResize := False;
  CloseButton := false;
  fMinWidth := 1;
  fMinHeight := 1;
  CaptionAlign := faMiddleLeft;
  fOnClose := nil;

  fDisplayEffect := deNone;

{$IFDEF GUI_USE_MAINMENU}
  fMainMenu := nil;
{$ENDIF}
  updateSkin(Gui.Skin);
end;

destructor zglTForm.Destroy;
begin
  inherited;
  if Assigned(Effect) then
    Effect.Free;
end;

procedure zglTForm.updateSkin(skin: zglTGuiSkin);
var skinTx: zglPGuiSkinTex;
begin
  skinTx := skin.SkinTex[skinForm];
  with fBorder do begin
    X := skinTx^.OffX; Y := skinTx^.OffY;
    W := skinTx^.OffX; H := skinTx^.OffX;
  end;
  UpdateClientRect;
end;

procedure zglTForm.setDisplayEffect(de: TDisplayEffect);
begin
  if de <> DisplayEffect then begin
    if Assigned(Effect) then begin
      Effect.Free;
      Effect := nil;
    end;
  end;
  fDisplayEffect := de;
  if DisplayEffect <> deNone then begin
    case DisplayEffect of
      deNone: begin
      end;
      deZoomIn: begin
        Effect := zglTZoomInEffect.Create(Gui, Self);
      end;
      deFadeIn: begin
        Effect := zglTFadeInEffect.Create(Gui, Self);
      end;
      deSlideDown: begin
        Effect := zglTSlideDownEffect.Create(Gui, Self);
      end;
    end;
  end;
end;

{$IFDEF GUI_USE_MAINMENU}
procedure zglTForm.setMainMenu;
var skinTx: zglPGuiSkinTex;
begin
  if Assigned(fMainMenu) then begin
    fMainMenu.fParent := nil;
  end;
  fMainMenu := mm;
  skinTx := Self.Gui.Skin.SkinTex[skinForm];
  fBorder.Y := skinTx^.OffY;
  if Assigned(fMainMenu) then begin
    fMainMenu.fParent := Self;
    OnAdd(fMainMenu);
    fBorder.Y := skinTx^.OffY + fMainMenu.fRect.H;
  end;
  UpdateClientRect;
end;
{$ENDIF}

procedure zglTForm.Draw;
var
    r, fUpdateRect, cl: zglTRect;
    fnt: zglTFontObject;
    skinTx, frmTx: zglPGuiSkinTex;
    sciEnd: boolean;
begin
  if not Visible then Exit;

  r := updateRect(oX, oY);
  if scissor_Rect(r, clntRect, nil, sciEnd) then begin
    frmTx := Self.Gui.Skin.SkinTex[skinForm];
    Self.Gui.Skin.DrawField(skinForm, r.X, r.Y, r.W, r.H,
      0, 0, frmTx^.W div 2, frmTx^.H div 2);
    fnt := getRootFont(false);
    if CanResize then begin
      skinTx := Self.Gui.Skin.SkinTex[skinFormResize];

      Self.Gui.Skin.DrawTex(skinFormResize,
        r.X + r.W - skinTx^.W, r.Y + r.H - skinTx^.H,
        0, 0, skinTx^.W, skinTx^.H);
    end;
    if CloseButton then begin
      skinTx := Self.Gui.Skin.SkinTex[skinCloseBtn];
      cl := Self.Gui.getRect(r.X + r.W - skinTx^.W,
        r.Y, skinTx^.W, skinTx^.H);
      Self.Gui.Skin.DrawTex(skinCloseBtn,
        r.X + r.W - skinTx^.W, r.Y,
        Byte(col2d_PointInRect(mouse_X, mouse_Y, cl)) * skinTx^.W,
        0, skinTx^.W, skinTx^.H);
      r.W := r.W - skinTx^.W;
    end;
    inherited;
    r.H := frmTx^.OffY; r.X := r.X + frmTx^.OffX;
    r.W := r.W - frmTx^.OffX * 2;
    text_DrawInRectEx(fnt.Data, r, fnt.Size,
      0, Caption, fnt.Alpha, fnt.Color, FontAlignes[CaptionAlign] or TEXT_CLIP_RECT);
{$IFDEF GUI_USE_MAINMENU}
    if Assigned(fMainMenu) then begin
      r := updateRect(oX + ClientRect.X, oY + frmTx^.OffY);
      fUpdateRect := Self.Gui.GetRect(r.X, r.Y,
        ClientRect.W, fMainMenu.fRect.H);
      fMainMenu.Draw(@fUpdateRect, r.X, r.Y);
    end;
{$ENDIF}

    if sciEnd then
      scissor_End;
  end;
  inherited DrawEdit(oX, oY);
end;

procedure zglTForm.Update;
var r, fUpdateRect, cl: zglTRect;
    skinTx, frmTx: zglPGuiSkinTex;
begin
  if not Visible then Exit;

{$IFDEF GUI_USE_MAINMENU}
  if Assigned(fMainMenu) then begin
    frmTx := Self.Gui.Skin.SkinTex[skinForm];
    r := updateRect(oX + ClientRect.X, oY + frmTx^.OffY);
    fUpdateRect := Self.Gui.GetRect(r.X, r.Y,
      ClientRect.W, fMainMenu.fRect.H);
    fMainMenu.fRect.W := fClientRect.W;
    fMainMenu.Update(dt, @fUpdateRect, true, r.X, r.Y);
  end;
{$ENDIF}
  if CloseButton then begin
    r := updateRect(oX, oY);
    skinTx := Self.Gui.Skin.SkinTex[skinCloseBtn];
    cl := Self.Gui.getRect(r.X + r.W - skinTx^.W,
      r.Y, skinTx^.W, skinTx^.H);
    if col2d_PointInRect(mouse_X, mouse_Y, cl)
         and mouse_Click(M_BLEFT) then begin
      if Assigned(fOnClose) then begin
        fOnClose(Self);
      end;
      mouse_ClearState;
    end;
  end;
  inherited;
  UpdateClientRect;

end;

// zglTProgressBar
{$IFDEF GUI_USE_PROGRESSBAR}
procedure zglTProgressBar.Draw;
var rct: zglTRect;
    skinTx: zglPGuiSkinTex;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);
  skinTx := Self.Gui.Skin.SkinTex[skinProgressBar];
  Self.Gui.Skin.DrawField(skinProgressBar, rct.X, rct.Y, rct.W, rct.H,
    0, 0, skinTx^.W div 2, skinTx^.H div 2);
  skinTx := Self.Gui.Skin.SkinTex[skinProgressBarProgress];
  rct.X := rct.X + skinTx^.OffX;
  rct.Y := rct.Y + skinTx^.OffY;
  rct.H := rct.H - skinTx^.OffY * 2;
  rct.W := ((fProgress - fMin) / (fMax - fMin)) * (rct.W - skinTx^.OffX * 2);
  Self.Gui.Skin.DrawField(skinProgressBarProgress,
    rct.X, rct.Y, rct.W, rct.H,
    0, 0, skinTx^.W div 2, skinTx^.H div 2);
  inherited DrawEdit(oX, oY);
end;

constructor zglTProgressBar.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  fMax := 100;
  fMin := 0;
  fProgress := 0;
  fLastProgress := 0;
end;

constructor zglTProgressBar.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  fMax := 100;
  fMin := 0;
  fProgress := 0;
  fLastProgress := 0;
end;

procedure zglTProgressBar.setProgress;
begin
  fProgress := progress;
  if fProgress < fMin then
    fProgress := fMin;
  if fProgress > fMax then
    fProgress := fMax;
  if fProgress <> fLastProgress then begin
    if Assigned(fOnChange) then
      fOnChange(Self);
    fLastProgress := fProgress;
  end;
end;

procedure zglTProgressBar.SetSafeProgress;
begin
  fProgress := progress;
  if fProgress < fMin then
    fProgress := fMin;
  if fProgress > fMax then
    fProgress := fMax;
end;

{$ENDIF}
// zglTTrackBar
{$IFDEF GUI_USE_TRACKBAR}
constructor zglTTrackBar.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  fMax := 100;
  fMin := 0;
  fProgress := 0;
end;

constructor zglTTrackBar.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  fMax := 100;
  fMin := 0;
  fProgress := 0;
end;

procedure zglTTrackBar.Update;
var rct, rd: zglTRect;
    skinTx: zglPGuiSkinTex;
begin
  if not Visible then Exit;
  inherited;
  rct := updateRect(oX, oY);
  skinTx := Self.Gui.Skin.SkinTex[skinTrackBarSlider];
  rd.X := Round(rct.X + ((fProgress - fMin) / (fMax - fMin)) *
    (rct.W - skinTx^.W));
  rd.Y := round(rct.Y + rct.H / 2 - skinTx^.H / 2);
  rd.W := skinTx^.W;
  rd.H := skinTx^.H;
  if fJustPressed then begin
    if isHover(@rd, oX, oY) then
      Self.Gui.Handler.HandleEvent(Self, heTrackBar)
    else
      Progress := Min +
        Round(((Mouse_X - Rect.X - RootPos.X - skinTx^.H / 2)
          / (Rect.W - skinTx^.W)) * (Max - Min));

    if fCatchMouseClick then
      mouse_ClearState;
  end;
end;

procedure zglTTrackBar.Draw;
var rct, rctP: zglTRect;
    skinTx, skinPr: zglPGuiSkinTex;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);
  skinTx := Self.Gui.Skin.SkinTex[skinProgressBar];
  skinPr := Self.Gui.Skin.SkinTex[skinProgressBarProgress];
  Self.Gui.Skin.DrawField(skinProgressBar, rct.X, rct.Y, rct.W, rct.H,
    0, 0, skinTx^.W div 2, skinTx^.H div 2);

  skinTx := Self.Gui.Skin.SkinTex[skinTrackBarSlider];

  rctP := rct;

  rctP.X := rct.X + skinPr^.OffX;
  rctP.Y := rct.Y + skinPr^.OffY;
  rctP.H := rct.H - skinPr^.OffY * 2;
  rctP.W := ((fProgress - fMin) / (fMax - fMin)) * (rct.W - skinPr^.OffX * 2);
  Self.Gui.Skin.DrawField(skinProgressBarProgress,
    rctP.X, rctP.Y, rctP.W, rctP.H,
    0, 0, skinPr^.W div 2, skinPr^.H div 2);

  rct.X := Round(rct.X + ((fProgress - fMin) / (fMax - fMin)) *
    (rct.W - skinTx^.W));
  rct.Y := round(rct.Y + rct.H / 2 - skinTx^.H / 2);
  rct.W := skinTx^.W;
  rct.H := skinTx^.H;

  Self.Gui.Skin.DrawTex(skinTrackBarSlider,
    rct.X, rct.Y,
    0, Byte(isHover(@rct, oX, oY)) * skinTx^.H,
    skinTx^.W, skinTx^.H);
  DrawEdit(oX, oY);
end;

procedure zglTTrackBar.HandleDraw;
begin
  //
end;

procedure zglTTrackBar.HandleLeave;
begin
  //
end;

function zglTTrackBar.HandleUpdate;
var skinTx: zglPGuiSkinTex;
begin
  Result := inherited HandleUpdate(dt, evnt);
  case evnt of
    heTrackBar: begin
      skinTx := Self.Gui.Skin.SkinTex[skinTrackBarSlider];
      Progress := Min +
        Round(((Mouse_X - Rect.X - RootPos.X - skinTx^.W / 2)
          / (Rect.W - skinTx^.W)) * (Max - Min));
      if not mouse_Down(M_BLEFT) then begin
        Self.Gui.Handler.Clear;
        Result := true;
      end;
    end;
  end;
end;
{$ENDIF}
// zglTImageButton
{$IFDEF GUI_USE_IMAGEBUTTON}
procedure zglTImageButton.Draw;
var rct: zglTRect;
    i: integer;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);
  i := Byte(fPressed)*2 + Byte(fHover and (not fPressed));
  Texture.Draw(rct.X, rct.Y, rct.W, rct.H, i, 255);
  inherited DrawEdit(oX, oY);
end;

procedure zglTImageButton.Update;
begin
  if not Visible then Exit;
  inherited;
end;

constructor zglTImageButton.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  Texture := zglTCTexture.Create(pTexture);
  Texture.TexWidth := texWidth;
  Texture.TexHeight := texHeight;
  Texture.FrameOffset := pTextureOffset;
  Texture.UseFrames := true;
end;

constructor zglTImageButton.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  Texture := zglTCTexture.Create(nil);
  Texture.TexWidth := 0;
  Texture.TexHeight := 0;
  Texture.FrameOffset := 0;
  Texture.UseFrames := true;
end;

destructor zglTImageButton.Destroy;
begin
  Texture.Free;
  inherited;
end;
{$ENDIF}

// zglTImage
{$IFDEF GUI_USE_IMAGE}
procedure zglTImage.Draw;
var rct: zglTRect;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);
  Texture.Draw(rct.X, rct.Y, rct.W, rct.H, 0, 255);
  inherited DrawEdit(oX, oY);
end;

procedure zglTImage.Update;
begin
  if not Visible then Exit;
  inherited;
end;

constructor zglTImage.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  Texture := zglTCTexture.Create(pTexture);
  Texture.TexWidth := texWidth;
  Texture.TexHeight := texHeight;
  Texture.FrameOffset := pTextureOffset;
end;

constructor zglTImage.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  Texture := zglTCTexture.Create(nil);
  Texture.TexWidth := 0;
  Texture.TexHeight := 0;
  Texture.FrameOffset := 0;
end;

destructor zglTImage.Destroy;
begin
  Texture.Drop;
  inherited;
end;
{$ENDIF}

{$IFDEF GUI_USE_RADIOBUTTON}
// zglTRadioButton
constructor zglTRadioButton.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  Caption := pCaption;
end;

constructor zglTRadioButton.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  Caption := '';
end;

procedure zglTRadioButton.Draw;
var
    rct: zglTRect;
    fnt: zglTFontObject;
    i: integer;
    checkSize: single;
    skinTx: zglPGuiSkinTex;
    sciEnd: boolean;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);
  if scissor_Rect(rct, clntRect, nil, sciEnd) then begin
    if Enabled then
      i := 2 * Byte(fHover xor fPressed)
        + Byte((Parent as zglTRadioBox).Selected <> Self)
    else
      i := 4 + Byte((Parent as zglTRadioBox).Selected <> Self);
    fnt := getRootFont(fHover);
    skinTx := Self.Gui.Skin.SkinTex[skinRadioButton];
    checkSize := min(skinTx^.H, rct.H);
    Self.Gui.Skin.DrawTexScaled(skinRadioButton, rct.X, rct.Y + rct.H / 2 - checkSize /2,
      min(skinTx^.W, rct.H), checkSize,
      0, i * skinTx^.H, skinTx^.W, skinTx^.H);
    rct.X := rct.X + checkSize; rct.W := rct.W - skinTx^.H;
    if fPressed then begin
      rct.X := rct.X + 1; rct.Y := rct.Y + 1;
    end;
    text_DrawInRectEx(fnt.Data, rct, fnt.Size,
      0, Caption, fnt.Alpha, fnt.Color, TEXT_LEFT or TEXT_CLIP_RECT);
    if sciEnd then
      scissor_End;
  end;
  inherited DrawEdit(oX, oY);
end;

procedure zglTRadioButton.Update;
begin
  inherited;
  if fJustPressed then begin
    if Parent.ClassType = zglTRadioBox then begin
      (Parent as zglTRadioBox).Select(Self);
    end;
  end;
end;
// zglTRadioBox
constructor zglTRadioBox.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH);
  fSelected := nil;
end;

constructor zglTRadioBox.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  fSelected := nil;
end;

procedure zglTRadioBox.Draw;
begin
  if not Visible then Exit;
  inherited;
  inherited DrawEdit(oX, oY);
end;

procedure zglTRadioBox.Select;
begin
  fSelected := zglTRadioButton(pI);
end;

procedure zglTRadioBox.Update;
begin
  if not Visible then Exit;
  inherited;
end;
{$ENDIF}
// zglTScrollBox
procedure zglTScrollBox.setScrollHeight;
begin
  fScrollHeight := s;
  UpdateClientRect;
end;

procedure zglTScrollBox.setScrollWidth;
begin
  fScrollWidth := s;
  UpdateClientRect;
end;

procedure zglTScrollBox.UpdateAlign;
begin
  inherited;
  UpdateClientRect;
end;

procedure zglTScrollBox.Draw;
var f: single;
    rct: zglTRect;
    skinTx: zglPGuiSkinTex;
begin
  if fScrollHeight > fClientRect.H then begin
    skinTx := Self.Gui.Skin.SkinTex[skinScrollRight];

    // Фон прокрутки
    Self.Gui.Skin.DrawField(skinScrollRight,
      oX + fRect.X + fClientRect.X + fClientRect.W,
      oY + fRect.Y + fClientRect.Y,
      skinTx^.W,
      fClientRect.H,
      0, 0, skinTx^.W div 2, skinTx^.H div 2);

    // Верхняя кнопка прокрутки
    rct := Self.Gui.getRect(oX + fRect.X + fClientRect.X + fClientRect.W,
      oY + fRect.Y + fClientRect.Y,
      skinTx^.BtnW, skinTx^.BtnH);

    Self.Gui.Skin.DrawTex(skinScrollRight, rct.X, rct.Y,
      Byte(isHover(@rct, oX, oY)) * skinTx^.BtnW + skinTx^.W, 0,
      skinTx^.BtnW, skinTx^.BtnH);

    // Нижняя кнопка прокрутки
    rct := Self.Gui.getRect(oX + fRect.X + fClientRect.X + fClientRect.W,
      oY + fRect.Y + fClientRect.Y + fClientRect.H - skinTx^.BtnH,
      skinTx^.BtnW, skinTx^.BtnH);
    Self.Gui.Skin.DrawTex(skinScrollRight,
      rct.X, rct.Y,
      Byte(isHover(@rct, oX, oY)) * skinTx^.ScrW + skinTx^.W,
      skinTx^.BtnH, skinTx^.BtnW, skinTx^.BtnH);

    // Полоса прокрутки
    f := ((fClientRect.H - skinTx^.BtnH * 2) * fClientRect.H) / fScrollHeight;
    rct := Self.Gui.getRect(
      oX + fRect.X + fClientRect.X + fClientRect.W,
      oY + fRect.Y + fClientRect.Y + skinTx^.BtnH -
        (fScroll.Y / (fScrollHeight - fClientRect.H)) *
        (fClientRect.H - f - skinTx^.BtnH * 2),
      skinTx^.BtnW, f);

    Self.Gui.Skin.DrawField(skinScrollRight,
      rct.X, rct.Y - skinTx^.OffY, rct.W, rct.H + skinTx^.OffY * 2,
      Byte(isHover(@rct, oX, oY)) * skinTx^.ScrW + skinTx^.W + skinTx^.BtnW * 2,
      0,
      skinTx^.ScrW div 2, skinTx^.ScrH div 2);

  end;
  if fScrollWidth > fClientRect.W then begin
    skinTx := Self.Gui.Skin.SkinTex[skinScrollBottom];

    // Фон прокрутки
    Self.Gui.Skin.DrawField(skinScrollBottom,
      oX + fRect.X + fClientRect.X,
      oY + fRect.Y + fClientRect.Y + fClientRect.H,
      fClientRect.W,
      skinTx^.H,
      0, 0, skinTx^.W div 2, skinTx^.H div 2);

    // Левая кнопка прокрутки
    rct := Self.Gui.getRect(oX + fRect.X + fClientRect.X,
      oY + fRect.Y + fClientRect.Y + fClientRect.H,
      skinTx^.btnW, skinTx^.btnH);
    Self.Gui.Skin.DrawTex(skinScrollBottom, rct.X, rct.Y,
      0, Byte(isHover(@rct, oX, oY)) * skinTx^.btnH + skinTx^.H,
      skinTx^.btnW, skinTx^.btnH);

    // Правая кнопка прокрутки
    rct := Self.Gui.getRect(
      oX + fRect.X + fClientRect.X + fClientRect.W - skinTx^.btnW,
      oY + fRect.Y + fClientRect.Y + fClientRect.H,
      skinTx^.btnW, skinTx^.btnH);
    Self.Gui.Skin.DrawTex(skinScrollBottom, rct.X, rct.Y,
      skinTx^.btnW,
      Byte(isHover(@rct, oX, oY)) * skinTx^.btnH + skinTx^.H,
      skinTx^.btnW, skinTx^.btnH);

    // Полоса прокрутки
    f := ((fClientRect.W - skinTx^.btnW * 2) * fClientRect.W) / fScrollWidth;
    rct := Self.Gui.getRect(
      oX + fRect.X + fClientRect.X + skinTx^.btnW -
        (fScroll.X / (fScrollWidth - fClientRect.W)) *
        (fClientRect.W - f - skinTx^.btnW * 2),
      oY + fRect.Y + fClientRect.Y + fClientRect.H,
      f, skinTx^.scrH);

    Self.Gui.Skin.DrawField(skinScrollBottom,
      rct.X - skinTx^.OffX, rct.Y, rct.W + skinTx^.OffX * 2, rct.H,
      0, Byte(isHover(@rct, oX, oY)) * skinTx^.scrH + skinTx^.H + skinTx^.BtnH * 2,
      skinTx^.scrW div 2, skinTx^.scrH div 2);

  end;
  inherited;
end;

procedure zglTScrollBox.Update;
var f: single;
    rct: zglTRect;
    skinTx: zglPGuiSkinTex;
begin
  inherited;

  if fScrollHeight > fClientRect.H then begin
    skinTx := Self.Gui.Skin.SkinTex[skinScrollRight];
    // Верхняя кнопка прокрутки
    rct := Self.Gui.getRect(oX + fRect.X + fClientRect.X + fClientRect.W,
      oY + fRect.Y + fClientRect.Y,
      skinTx^.BtnW, skinTx^.BtnH);
    if (isHover(@rct, oX, oY) and fJustPressed)
      or (mouse_Wheel(M_WUP) and fHover) then begin
      ScrollTo(fScroll.X, fScroll.Y + fClientRect.H);
      mouse_ClearState;
    end;

    // Полоса прокрутки
    f := ((fClientRect.H - skinTx^.BtnH * 2) * fClientRect.H) / fScrollHeight;
    rct := Self.Gui.getRect(
      oX + fRect.X + fClientRect.X + fClientRect.W,
      oY + fRect.Y + fClientRect.Y + skinTx^.BtnH -
        (fScroll.Y / (fScrollHeight - fClientRect.H)) *
        (fClientRect.H - f - skinTx^.BtnH * 2),
      skinTx^.BtnW, f);

    if isHover(@rct, oX, oY) and fJustPressed then begin
      fOffsetY := round(mouse_Y - rct.Y);
      fCorner := round(oY + fRect.Y + fClientRect.Y + skinTx^.BtnW);
      Self.Gui.Handler.HandleEvent(Self, heScrollH);
      mouse_ClearState;
    end;

    // Нижняя кнопка прокрутки
    rct := Self.Gui.getRect(oX + fRect.X + fClientRect.X + fClientRect.W,
      oY + fRect.Y + fClientRect.Y + fClientRect.H - skinTx^.BtnH,
      skinTx^.BtnW, skinTx^.BtnH);
    if (isHover(@rct, oX, oY) and fJustPressed)
      or (mouse_Wheel(M_WDOWN) and fHover) then begin
      ScrollTo(fScroll.X, fScroll.Y - fClientRect.H);
      mouse_ClearState;
    end;
  end;
  if fScrollWidth > fClientRect.W then begin
    skinTx := Self.Gui.Skin.SkinTex[skinScrollBottom];
    // Верхняя кнопка прокрутки
    rct := Self.Gui.getRect(oX + fRect.X + fClientRect.X,
      oY + fRect.Y + fClientRect.Y + fClientRect.H,
      skinTx^.btnW, skinTx^.btnH);
    if isHover(@rct, oX, oY) and fJustPressed then begin
      ScrollTo(fScroll.X + fClientRect.W, fScroll.Y);
      mouse_ClearState;
    end;

    // Полоса прокрутки
    f := ((fClientRect.W - skinTx^.btnW * 2) * fClientRect.W) / fScrollWidth;
    rct := Self.Gui.getRect(
      oX + fRect.X + fClientRect.X + skinTx^.btnW -
        (fScroll.X / (fScrollWidth - fClientRect.W)) *
        (fClientRect.W - f - skinTx^.btnW * 2),
      oY + fRect.Y + fClientRect.Y + fClientRect.H,
      f, skinTx^.scrH);

    if isHover(@rct, oX, oY) and fJustPressed then begin
      fOffsetX := round(mouse_X - rct.X);
      fCorner := round(oX + fRect.X + fClientRect.X + skinTx^.btnW);
      Self.Gui.Handler.HandleEvent(Self, heScrollW);
      mouse_ClearState;
    end;

    // Нижняя кнопка прокрутки
    rct := Self.Gui.getRect(
      oX + fRect.X + fClientRect.X + fClientRect.W - skinTx^.btnW,
      oY + fRect.Y + fClientRect.Y + fClientRect.H,
    skinTx^.btnW, skinTx^.btnH);
    if isHover(@rct, oX, oY) and fJustPressed then begin
      ScrollTo(fScroll.X - fClientRect.W, fScroll.Y);
      mouse_ClearState;
    end;
  end;
end;

procedure zglTScrollBox.Resize;
begin
  inherited;
  UpdateClientRect;
  ScrollTo(fScroll.X, fScroll.Y);
  if fScrollWidth <= fClientRect.W then
    ScrollTo(0, fScroll.Y);
  if fScrollHeight <= fClientRect.H then
    ScrollTo(fScroll.X, 0);
end;

procedure zglTScrollBox.HandleDraw;
begin
  //
end;

procedure zglTScrollBox.HandleLeave;
begin
  //
end;

function zglTScrollBox.HandleUpdate;
var f: single;
    skinTx: zglPGuiSkinTex;
begin
  Result := inherited HandleUpdate(dt, evnt);
  case evnt of
    heScrollH: begin
      skinTx := Self.Gui.Skin.SkinTex[skinScrollRight];
      f := ((fClientRect.H - skinTx^.BtnH * 2) * fClientRect.H) / fScrollHeight;
      ScrollTo(fScroll.X,
        - ((mouse_Y - fOffsetY - fCorner)
           / (fClientRect.H - skinTx^.BtnH * 2 - f)) *
        (fScrollHeight - fClientRect.H));
      if not mouse_Down(M_BLEFT) then begin
        Self.Gui.Handler.Clear;
        Result := true;
      end;
    end;
    heScrollW: begin
      skinTx := Self.Gui.Skin.SkinTex[skinScrollBottom];
      f := ((fClientRect.W - skinTx^.BtnW * 2) * fClientRect.W) / fScrollWidth;
      ScrollTo(- ((mouse_X - fOffsetX - fCorner)
           / (fClientRect.W - skinTx^.BtnW * 2 - f)) *
        (fScrollWidth - fClientRect.W), fScroll.Y);
      if not mouse_Down(M_BLEFT) then begin
        Self.Gui.Handler.Clear;
        Result := true;
      end;
    end;
  end;
end;

procedure zglTScrollBox.OnAddMe;
begin
  inherited;
  UpdateClientRect;
end;

procedure zglTScrollBox.UpdateClientRect;
begin
  fClientRect.X := fBorder.X;
  fClientRect.Y := fBorder.Y;
  fClientRect.W := fRect.W - fBorder.X - fBorder.W;
  fClientRect.H := fRect.H - fBorder.Y - fBorder.H;
  if fScrollWidth > fClientRect.W then
    fClientRect.H := fClientRect.H - Self.Gui.Skin.SkinTex[skinScrollBottom]^.BtnH;
  if fScrollHeight > fClientRect.H then
    fClientRect.W := fClientRect.W - Self.Gui.Skin.SkinTex[skinScrollBottom]^.BtnW;
end;

procedure zglTScrollBox.ScrollTo;
begin
  fScroll.X := Min(Max(pX, -(fScrollWidth - fClientRect.W)), 0);
  fScroll.Y := Min(Max(pY, -(fScrollHeight - fClientRect.H)), 0);
end;

{$IFDEF GUI_USE_LIST}
// zglTListItem
constructor zglTListItem.Create;
begin
  fCaption := pCaption;
  fHeight := pHeight;
  fList := pList;
  fIconId := 0;
  fIconsTexture := fList.IconsTexture;
  fIconsSize := fList.fIconsSize;
  fPadding.X := 0; fPadding.Y := 0; fPadding.W := 0; fPadding.H := 0;
end;

function zglTListItem.GetPosition: integer;
begin
  Result := fList.fItems.IndexOf(Self);
end;

procedure zglTListItem.SetIconsSize(W, H: integer);
begin
  fIconsSize.X := W;
  fIconsSize.Y := H;
end;

procedure zglTListItem.SetPadding(X, Y, W, H: single);
begin
  fPadding.X := X;
  fPadding.Y := Y;
  fPadding.W := W;
  fPadding.H := H;
end;

destructor zglTListItem.Destroy;
begin
  //
  inherited;
end;

procedure zglTListItem.Draw;
var fnt: zglTFontObject;
    rct, r: zglTRect;
    p: zglTPoint2D;
    b: boolean;
    skinTx: zglPGuiSkinTex;
begin
  rct := fList.Gui.getRect(oX, oY, oW, oH);
  r := fList.UpdateRect(oX, oY);
  skinTx := fList.Gui.Skin.SkinTex[skinListItem];
  if (fList.fSelected = Self) then begin
    fList.Gui.Skin.DrawField(skinListItem,
      rct.X, rct.Y, rct.W, rct.H,
      0, 0, skinTx^.W div 2, skinTx^.H div 2);
  end else
  if not Assigned(fList.Gui.Handler.hObject) and
    col2d_PointInRect(Mouse_X, Mouse_Y, rct) and
    col2d_PointInRect(Mouse_X, Mouse_Y, clntRect) then begin
    fList.Gui.Skin.DrawField(skinListItem,
      rct.X, rct.Y, rct.W, rct.H,
      0, skinTx^.H, skinTx^.W div 2, skinTx^.H div 2);
  end;
  fnt := fList.getRootFont(false);

  b := true;

  if Assigned(fList.fOnDrawItem) then begin
    fList.fOnDrawItem(Self, clntRect, rct.X,
      rct.Y, rct.W, rct.H, b);
  end;

  if not b then exit;

  if fList.ShowIcons and (IconId > 0) then begin
    asprite2d_Draw(IconsTexture,
      rct.X, rct.Y + Height / 2 - fIconsSize.Y / 2,
      fIconsSize.X, fIconsSize.Y,
      0, IconId);
    rct.X := rct.X + fIconsSize.X;
    rct.W := rct.W - fIconsSize.X;
  end;

  rct.X := rct.X + fPadding.X;
  rct.W := rct.W - fPadding.X - fPadding.W;
  rct.Y := rct.Y + fPadding.Y;
  rct.H := rct.H - fPadding.Y - fPadding.H;

  if fList.fTextAlign and TEXT_HALIGN_RIGHT > 0 then begin
    p.X := rct.X + rct.W;
  end else
  if fList.fTextAlign and TEXT_HALIGN_LEFT > 0 then begin
    p.X := rct.X;
  end else begin
    p.X := rct.X + rct.W / 2;
  end;
  if fList.fTextAlign and TEXT_VALIGN_BOTTOM > 0 then begin
    p.Y := rct.Y + rct.H;
  end else
  if fList.fTextAlign and TEXT_VALIGN_TOP > 0 then begin
    p.Y := rct.Y;
  end else begin
    p.Y := rct.Y + rct.H / 2;
  end;

  text_DrawEx(fnt.Data, p.X, p.Y, fnt.Size, 0, Caption,
    fnt.Alpha, fnt.Color, fList.fTextAlign);
end;

procedure zglTListItem.Update;
var rct: zglTRect;
    b: Boolean;
begin
  rct := fList.Gui.getRect(oX, oY, oW, oH);
  if col2d_PointInRect(Mouse_X, Mouse_Y, rct) and
    col2d_PointInRect(Mouse_X, Mouse_Y, clntRect) then begin
    b := true;
    if Assigned(fList.fOnUpdateItem) then begin
      fList.fOnUpdateItem(Self, clntRect, dt,
        rct.X, rct.Y, rct.W, rct.H, b);
    end;
    if not b then exit;
    if fList.fJustPressed then begin
      fList.fSelected := Self;
      if Assigned(fList.OnSelectItem) then
        fList.OnSelectItem
          (fList, round(Mouse_X - rct.X), round(Mouse_Y - rct.Y));
      mouse_ClearState;
    end;
  end;
end;
// zglTTreeListItem
constructor zglTTreeListItem.Create;
begin
  inherited Create(pList, pCaption, pHeight);
  Parent := pParent;
  Level := 1;
  if Parent = nil then
    Level := 0
  else if Parent is zglTTreeListItem then
    Level := zglTTreeListItem(Parent).Level + 1
  else
    Level := 1;
end;

destructor zglTTreeListItem.Destroy;
begin
  //
  inherited;
end;

procedure zglTTreeListItem.Draw;
begin
  inherited Draw(oX + fList.Gui.Skin.SkinCfg[cfgTreeWidth] * Level, oY,
    oW - fList.Gui.Skin.SkinCfg[cfgTreeWidth] * Level, oH, clntRect);
end;

procedure zglTTreeListItem.Update;
begin
  inherited Update(dt, oX + fList.Gui.Skin.SkinCfg[cfgTreeWidth] * Level, oY,
    oW - fList.Gui.Skin.SkinCfg[cfgTreeWidth] * Level, oH, clntRect);
end;
// zglTList
constructor zglTList.Create;
var skinTx: zglPGuiSkinTex;
begin
  inherited;
  fItems := TList.Create;
  fSelected := nil;
  fShowIcons := false;
  fIconsTexture := nil;
  fIconsSize.X := 16; fIconsSize.Y := 16;
  fTextAlign := TEXT_LEFT;
  ItemsPerLine := 1;
  skinTx := Self.Gui.Skin.SkinTex[skinList];
  fBorder := Self.Gui.getRect(skinTx^.OffX, skinTx^.OffY,
                         skinTx^.OffX, skinTx^.OffY);
  UpdateClientRect;

  fOnDrawItem := nil;
  fOnUpdateItem := nil;
  fOnSelectItem := nil;
end;

constructor zglTList.CreateDefaults(pGui: zglTGui);
var skinTx: zglPGuiSkinTex;
begin
  inherited CreateDefaults(pGui);
  fItems := TList.Create;
  fSelected := nil;
  fShowIcons := false;
  fIconsTexture := nil;
  fIconsSize.X := 16; fIconsSize.Y := 16;
  fTextAlign := TEXT_LEFT;
  ItemsPerLine := 1;
  skinTx := Self.Gui.Skin.SkinTex[skinList];
  fBorder := Self.Gui.getRect(skinTx^.OffX, skinTx^.OffY,
                         skinTx^.OffX, skinTx^.OffY);
  UpdateClientRect;

  fOnDrawItem := nil;
  fOnUpdateItem := nil;
  fOnSelectItem := nil;
end;

destructor zglTList.Destroy;
var i: integer;
begin
  for i := 0 to ItemsCount - 1 do
    Items[i].Drop;
  fItems.Free;
  inherited;
end;

procedure zglTList.SetIconsSize(W, H: integer);
begin
  fIconsSize.X := W;
  fIconsSize.Y := H;
end;

function zglTList.getItem;
begin
  Result := fItems.Items[Index];
end;

procedure zglTList.Clear;
var i: Integer;
begin
  for i := 0 to fItems.Count - 1 do begin
    zglTList(fItems[i]).Drop;
  end;
  fItems.Clear;
  Selected := nil;
  UpdateItems;
end;

function zglTList.ItemByData;
var i: Integer;
begin
  Result := nil;
  for i := 0 to fItems.Count - 1 do
    if zglTListItem(fItems.Items[i]).Data = data then begin
      Result := fItems.Items[i];
      break;
    end;
end;

function zglTList.getItemsCount;
begin
  Result := fItems.Count;
end;

procedure zglTList.UpdateItems;
var i, h: integer;
begin
  h := 0;
  for i := 0 to fItems.Count - 1 do begin
    if i mod ItemsPerLine = 0 then
      h := h + Items[i].fHeight;
  end;
  ScrollHeight := h;
  ScrollTo(fScroll.X, fScroll.Y);
end;

procedure zglTList.AddItem;
begin
  fItems.Add(Item);
  Item.UseCounter;
  UpdateItems;
end;

procedure zglTList.InsertItem;
begin
  fItems.Insert(Index, Item);
  Item.UseCounter;
  UpdateItems;
end;

procedure zglTList.MoveItem(Item: zglTListItem; Index: integer);
begin
  fItems.Move(fItems.indexOf(Item), Index);
end;

procedure zglTList.InsertAfter;
begin
  fItems.Insert(fItems.indexOf(After) + 1, Item);
  Item.UseCounter;
  UpdateItems;
end;

procedure zglTList.InsertBefore;
begin
  fItems.Insert(fItems.indexOf(Before), Item);
  Item.UseCounter;
  UpdateItems;
end;


procedure zglTList.RemoveItem;
begin
  if fSelected = Item then
    fSelected := nil;
  fItems.Remove(Item);
  Item.Drop;
  UpdateItems;
end;

procedure zglTList.Draw;
var i, h: integer;
    r, rct, fUpdateRect: zglTRect;
    skinTx: zglPGuiSkinTex;
    sciEnd: boolean;
begin
  if not Visible then Exit;
  r := updateRect(oX, oY);
  fUpdateRect := Self.Gui.GetRect(r.X + fClientRect.X, r.Y + fClientRect.Y,
                     fClientRect.W, fClientRect.H);
  skinTx := Self.Gui.Skin.SkinTex[skinList];
  Self.Gui.Skin.DrawField(skinList, r.X, r.Y,
    r.W, r.H,
    0, 0, skinTx^.W div 2, skinTx^.H div 2);
  inherited;
  if scissor_Rect(fUpdateRect, clntRect, @rct, sciEnd) then begin
    h := 0;
    for i:= 0 to ItemsCount - 1 do begin
      Items[i].Draw(round(r.X + fClientRect.X) +
          Round((i mod ItemsPerLine) * (fClientRect.W / ItemsPerLine)),
        Round(r.Y + fClientRect.Y + fScroll.Y) + h,
        Round(fClientRect.W / ItemsPerLine),
        Items[i].Height,  fUpdateRect);
      if i mod ItemsPerLine = ItemsPerLine - 1 then
        h := h + Items[i].Height;
    end;
    if sciEnd then
      scissor_End;
  end;
  inherited DrawEdit(oX, oY);
end;

procedure zglTList.setSelected(item: zglTListItem);
begin
  fSelected := item;
  if Assigned(OnSelectItem) then
    OnSelectItem(Self, 0, 0);
end;

procedure zglTList.Update;
var i, h: integer;
    r, fUpdateRect: zglTRect;
begin
  r := updateRect(oX, oY);
  fUpdateRect := Self.Gui.GetRect(r.X + fClientRect.X, r.Y + fClientRect.Y,
                     fClientRect.W, fClientRect.H);
  h := 0;
  for i:= 0 to ItemsCount - 1 do begin
    Items[i].Update(dt,
      Round(r.X + fClientRect.X +
        (i mod ItemsPerLine) * (fClientRect.W / ItemsPerLine)),
      Round(oY + fRect.Y + fClientRect.Y + fScroll.Y) + h,
      Round(fClientRect.W / ItemsPerLine),
      Items[i].Height,
      fUpdateRect);
    if i mod ItemsPerLine = ItemsPerLine - 1 then
      h := h + Items[i].Height;
  end;
  inherited;
end;
{$ENDIF}
// zglTTable
{$IFDEF GUI_USE_TABLE}
constructor zglTTableItem.Create;
begin
  fColumns := TStringList.Create;
  Table := pTable;
end;

destructor zglTTableItem.Destroy;
var i: integer;
begin
  for i := 0 to Columns.Count - 1 do begin
    if Components[i] <> nil then
      Components[i].Drop;
  end;
  Columns.Free;
  inherited;
end;

function zglTTableItem.getColumnsCount: Integer;
begin
  Result := fColumns.Count;
end;

function zglTTableItem.getComponent(index: Word): zglTGUIObject;
begin
  if index < fColumns.Count then
    Result := zglTGuiObject(fColumns.Objects[index])
  else
    Result := nil;
end;

function zglTTableItem.AddColumn(Value: TCaption): zglTTableItem;
begin
  fColumns.Add(string(Value));
  Result := Self;
end;

function zglTTableItem.AddComponent(Obj: zglTGUIObject): zglTTableItem;
var i: integer;
    skinTx: zglPGuiSkinTex;
begin
  Obj.UseCounter;
  fColumns.AddObject('', Obj);
  skinTx := Table.Gui.Skin.SkinTex[skinTableItem];
  i := fColumns.Count - 1;
  if Columns.Objects[i] <> nil then begin
    Components[i].Move(0, 0);
    Components[i].Resize(
      Table.Headers[i].Width - skinTx^.OffX * 2,
      Table.RowSize - skinTx^.OffY * 2);
  end;
  Result := Self;
end;

constructor zglTTableHeader.Create;
var skinTx: zglPGuiSkinTex;
begin
  Table := pTable;
  skinTx := Table.Gui.Skin.SkinTex[skinTableDelimiter];
  Caption := pCaption;
  Align := pAlign;
  Bold := pBold;
  Width := pWidth + skinTx^.OffX * 2 + skinTx^.W;
end;

destructor zglTTableHeader.Destroy;
begin
  inherited;
end;

procedure zglTTable.AddHeader(Header: zglTTableHeader);
begin
  Header.UseCounter;
  fHeaders.Add(Header);
  RecalcSizes;
end;

procedure zglTTable.RemoveHeader(Header: zglTTableHeader);
begin
  fHeaders.Remove(Header);
  Header.Drop;
  RecalcSizes;
end;

procedure zglTTable.Clear;
var i: integer;
begin
  for i := 0 to ItemsCount - 1 do begin
    Items[i].Drop;
  end;
  fItems.Clear;
  Selected := nil;
end;

procedure zglTTable.AddItem(Item: zglTTableItem);
begin
  fItems.Add(Item);
  Item.UseCounter;
  RecalcSizes;
end;

procedure zglTTable.RemoveItem(Item: zglTTableItem);
begin
  fItems.Remove(Item);
  Item.Drop;
  RecalcSizes;
end;

procedure zglTTable.RecalcSizes;
var i, w: integer;
begin
  w := 0;
  for i := 0 to fHeaders.Count - 1 do
     w := w + zglTTableHeader(fHeaders.Items[i]).Width;
  ScrollWidth := w;
  ScrollHeight := ItemsCount * RowSize;
end;

procedure zglTTable.Draw;
var r, r_, rct, fUpdateRect: zglTRect;
    skinTx, dlmTx, itemTx: zglPGuiSkinTex;
    w, i, j, c, id: integer;
    fnt: zglTFontObject;
    hdr: zglTTableHeader;
    sciEnd: boolean;
begin
  if not Visible then Exit;
  r := updateRect(oX, oY);
  fUpdateRect := Self.Gui.GetRect(r.X + fClientRect.X, r.Y + fClientRect.Y,
                     fClientRect.W, fClientRect.H);
  skinTx := Self.Gui.Skin.SkinTex[skinTable];
  dlmTx := Self.Gui.Skin.SkinTex[skinTableDelimiter];
  itemTx := Self.Gui.Skin.SkinTex[skinTableItem];
  Self.Gui.Skin.DrawField(skinTable, r.X, r.Y,
    r.W, r.H,
    0, 0, skinTx^.W div 2, skinTx^.H div 2);

  fnt := getRootFont(false);

  if scissor_Rect(r, clntRect, @rct, sciEnd) then begin
    w := round(r.X + fClientRect.X + fScroll.X);
    r.H := skinTx^.OffY{ - skinTx^.OffX};
    for i := 0 to HeadersCount - 1 do begin
      hdr := fHeaders.Items[i];
      r.X := w + dlmTx^.OffX;
      r.W := hdr.Width - dlmTx^.OffX * 2 - dlmTx^.W;
      text_DrawInRectEx(fnt.Data, r, fnt.Size,
        0, hdr.Caption, fnt.Alpha, fnt.Color, hdr.Align or TEXT_CLIP_RECT);
      if hdr.Bold then begin
        r.X := r.X + 1;
        text_DrawInRectEx(fnt.Data, r, fnt.Size,
          0, hdr.Caption, fnt.Alpha, fnt.Color, hdr.Align or TEXT_CLIP_RECT);
      end;
      w := w + hdr.Width;
      if (i <> fHeaders.Count - 1) then
        Self.Gui.Skin.DrawTex(skinTableDelimiter, w, r.Y,
          0, 0, dlmTx^.W, dlmTx^.H);

      if DrawGrid then
        pr2d_line(w - itemTx^.OffX * 2,
          r.Y + r.H,
          w - itemTx^.OffX * 2,
          fUpdateRect.Y + fUpdateRect.H, Self.Gui.Skin.SkinCfg[cfgTableGrid]);
    end;
    if sciEnd then
      scissor_End;
    inherited DrawEdit(oX, oY);
  end;

  for i := 0 to ItemsCount - 1 do begin
    c := Min(Items[i].Columns.Count, HeadersCount);
    w := round(rct.X + Scroll.X);
    for j := 0 to c - 1 do begin
      if scissor_Rect(fUpdateRect, clntRect, nil, sciEnd) then begin
        r := Self.Gui.GetRect(w,
          round(fUpdateRect.Y + i * RowSize + fScroll.Y), Headers[j].Width, RowSize);
        if (Selected = Items[i]) then
          id := 2
        else if (Hovered = Items[i]) then
          id := 1
        else
          id := 0;

        Self.Gui.Skin.DrawField(skinTableItem, r.X, r.Y,
          r.W, r.H,
          0, id * itemTx^.H,
          itemTx^.W div 2, itemTx^.H div 2);
        if Items[i].Columns.Objects[j] = nil then begin

          r_ := Self.Gui.GetRect(r.X + itemTx^.OffX,
            r.Y + itemTx^.OffY,
            r.W - itemTx^.OffX * 2,
            r.H - itemTx^.OffY * 2);
          text_DrawInRectEx(fnt.Data, r_, fnt.Size,
            0, TCaption(Items[i].Columns.Strings[j]), fnt.Alpha, fnt.Color,
            Headers[j].Align or TEXT_CLIP_RECT);
          if Headers[j].Bold then begin
            r.X := r.X + 1;
            text_DrawInRectEx(fnt.Data, r_, fnt.Size,
              0, TCaption(Items[i].Columns.Strings[j]), fnt.Alpha, fnt.Color, Headers[j].Align or TEXT_CLIP_RECT);
          end;

        end else begin
          Items[i].Components[j].Draw(
            @fUpdateRect, r.X + itemTx^.OffX, r.Y + itemTx^.OffY);
        end;
        if sciEnd then
          scissor_End;
      end;
      if DrawGrid then
        if scissor_Rect(fUpdateRect, clntRect, nil, sciEnd) then begin
          pr2d_line(fUpdateRect.X,
            fUpdateRect.Y + i * RowSize + fScroll.Y,
            fUpdateRect.X + fUpdateRect.W,
            fUpdateRect.Y + i * RowSize + fScroll.Y,
            Self.Gui.Skin.SkinCfg[cfgTableGrid]);
         if sciEnd then
           scissor_End;
        end;
      w := w + Headers[j].Width;
    end;

  end;

  inherited;
end;

procedure zglTTable.Update;
var r, rct, fUpdateRect: zglTRect;
    w, i, j, c: integer;
begin
  if not Visible then Exit;
  fUpdateRect := updateRect(oX, oY);
  rct := Self.Gui.GetRect(fUpdateRect.X + fClientRect.X,
                     fUpdateRect.Y + fClientRect.Y,
                     fClientRect.W, fClientRect.H);

  for i := 0 to ItemsCount - 1 do begin
    r := Self.Gui.GetRect(rct.X,
      round(rct.Y + i * RowSize + fScroll.Y), rct.W, RowSize);
    if Hover and col2d_PointInRect(Mouse_X, Mouse_Y, r) then begin
      fHovered := Items[i];
      if JustPressed then begin
        Selected := Items[i];
        if Assigned(OnSelect) then begin
          OnSelect(Self, Selected);
        end;
      end;
    end;
    c := Min(Items[i].Columns.Count, HeadersCount);
    w := round(rct.X + Scroll.X);
    for j := 0 to c - 1 do begin
      r := Self.Gui.GetRect(w,
        round(rct.Y + i * RowSize + fScroll.Y), Headers[j].Width, RowSize);
      if Items[i].Columns.Objects[j] <> nil then begin
        Items[i].Components[j].Update(
          dt, @fUpdateRect, true, r.X, r.Y);
      end;
      w := w + Headers[j].Width;
    end;
  end;

  inherited;
end;

constructor zglTTable.Create;
begin
  inherited;
  fHeaders := TList.Create;
  fItems := TList.Create;
  DrawGrid := false;
  RowSize := 20;
  OnSelect := nil;
  updateSkin(Gui.Skin);
end;

constructor zglTTable.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  fHeaders := TList.Create;
  fItems := TList.Create;
  DrawGrid := false;
  RowSize := 20;
  OnSelect := nil;
  updateSkin(Gui.Skin);
end;

destructor zglTTable.Destroy;
var i: integer;
begin
  for i := 0 to fHeaders.Count - 1 do
     zglTTableHeader(fHeaders.Items[i]).Drop;
  fHeaders.Free;
  for i := 0 to ItemsCount - 1 do
     Items[i].Drop;
  fItems.Free;
  inherited;
end;

function zglTTable.getHeadersCount: Integer;
begin
  result := fHeaders.Count;
end;

function zglTTable.getHeader(index: integer): zglTTableHeader;
begin
  result := fHeaders.Items[index];
end;


function zglTTable.getItemsCount: Integer;
begin
  result := fItems.Count;
end;

function zglTTable.getItem(index: integer): zglTTableItem;
begin
  result := fItems.Items[index];
end;

procedure zglTTable.updateSkin(skin: zglTGuiSkin);
var skinTx: zglPGuiSkinTex;
begin
  skinTx := Self.Gui.Skin.SkinTex[skinTable];
  with fBorder do begin
    X := skinTx^.OffX; Y := skinTx^.OffY;
    W := skinTx^.OffX; H := skinTx^.OffX;
  end;
  UpdateClientRect;
end;

{$ENDIF}

{$IFDEF GUI_USE_MAINMENU}
// zglTMainMenuItem
constructor zglTMainMenuItem.Create;
begin
  Text := Text_;
  CallBack := CallBack_;
  Width := Width_;
  Popup := Popup_;
  Popup.UseCounter;
end;

destructor zglTMainMenuItem.Destroy;
begin
  Popup.Drop;
  inherited;
end;
// zglTMainMenu
constructor zglTMainMenu.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  Visible := true;
  fItems := TList.Create;
  ContexItem := nil;
end;

constructor zglTMainMenu.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  Visible := true;
  fItems := TList.Create;
  ContexItem := nil;
end;

procedure zglTMainMenu.CalcOffsets;
var i, px: integer;
    item: zglTMainMenuItem;
begin
  px := 0;
  for i := 0 to fItems.Count -1 do begin
    item := fItems.Items[i];
    item.X := px;
    Inc(px, item.Width);
  end;
end;

procedure zglTMainMenu.AddItem;
begin
  fItems.Add(Item);
  Item.UseCounter;
  CalcOffsets;
end;

procedure zglTMainMenu.RemoveItem;
begin
  fItems.Remove(Item);
  CalcOffsets;
  Item.Drop;
end;

procedure zglTMainMenu.Draw;
var rct, r: zglTRect;
    i: integer;
    item: zglTMainMenuItem;
    fnt: zglTFontObject;
    b: Boolean;
    skinTx, itemTx: zglPGuiSkinTex;
begin
  rct := updateRect(oX, oY);
  skinTx := Self.Gui.Skin.SkinTex[skinMainMenu];
  itemTx := Self.Gui.Skin.SkinTex[skinMainMenuItem];
  Self.Gui.Skin.DrawField(skinMainMenu,
    rct.X, rct.Y, rct.W, rct.H,
    0, 0, skinTx^.W div 2, skinTx^.H div 2);
  r.Y := rct.Y; r.H := rct.H;
  for i := 0 to fItems.Count -1 do begin
    item := fItems.Items[i];
    r.W := item.Width;
    r.X := rct.X + item.X;
    b := isHover(@r, oX, oY) or ((Gui.Handler.hObject = self) and (ContexItem = item));
    fnt := getRootFont(b);
    Self.Gui.Skin.DrawField(skinMainMenuItem,
      r.X, r.Y, r.W, r.H,
      0, Byte(b) * itemTx^.H,
      itemTx^.W div 2, itemTx^.H div 2);
    text_DrawEx(fnt.Data,
      rct.X + item.X + item.Width / 2,
      rct.Y + rct.H / 2,
      fnt.Size, 0,
      item.Text, fnt.Alpha, fnt.Color, TEXT_HALIGN_CENTER or TEXT_VALIGN_CENTER);
  end;
  inherited DrawEdit(oX, oY);
end;

procedure zglTMainMenu.Update;
var rct, r: zglTRect;
    i: integer;
    item: zglTMainMenuItem;
begin
  if not Visible then Exit;
  inherited;
  rct := updateRect(oX, oY);
  r.Y := rct.Y; r.H := rct.H;
  for i := 0 to fItems.Count -1 do begin
    item := fItems.Items[i];
    r.W := item.Width;
    r.X := rct.X + Item.X;
    if isHover(@r, oX, oY) then begin
      if fPressed or ((Gui.Handler.hHandle = hePopup)
        and (Gui.Handler.hObject <> item.Popup)) then begin
        if Assigned(item.Callback) then begin
          item.Callback(item);
        end;
        item.Popup.Popup(Self,
          round(r.X) + 1, round(r.Y + r.H) + 1);
      end;
    end;
  end;
end;

function zglTMainMenu.getItemsCount: Integer;
begin
  Result := fItems.Count;
end;

function zglTMainMenu.getItem(Index: integer): zglTMainMenuItem;
begin
  Result := fItems[Index];
end;


destructor zglTMainMenu.Destroy;
var i: integer;
begin
  for i := 0 to ItemsCount - 1 do
    Items[i].Drop;
  fItems.Free;
  inherited;
end;
{$ENDIF}
{$IFDEF GUI_USE_POPUPMENU}
//zglTPopupMenuItem
constructor zglTMenuItem.Create;
begin
  Text := Text_;
  Checkbox := false;
  Checked := false;
  CallBack := CallBack_;
  Enabled := pEnabled;
  IconId := 0;
end;
//zglTPopupMenu
procedure zglTPopupMenu.Popup;
var i, j: integer;
    pmi: zglTMenuItem;
    fnt: zglTFontObject;
    b: Boolean;
    skinTx: zglPGuiSkinTex;
begin
  skinTx := Self.Gui.Skin.SkinTex[skinPopupMenuItem];

  if pMinWidth <> 0 then begin
    MinWidth := pMinWidth;
    Width := 0;
  end;

  PopupTime := 1;

  if Width = 0 then begin
    Width := MinWidth;
    fnt := Font.GetForState(fstNormal);
    for i := 0 to fItems.Count -1 do begin
      pmi := fItems.Items[i];
      j := round(text_GetWidth(fnt.Data, pmi.Text) * fnt.Size +
        Byte(ShowIcons) * (fIconsSize.X + skinTx^.OffX)) +
          skinTx^.OffX * 2;
      if Width < j then
        Width := j;
    end;
  end;
  Caller := Sender;
  if X_ + Width > Self.Gui.Rect.X + Self.Gui.fRect.W then
    X_ := Round(Gui.Rect.X + Self.Gui.fRect.W - Width);
  if Y_ + fItems.Count * skinTx^.H > Self.Gui.Rect.Y + Self.Gui.fRect.H then
    Y_ := round(Gui.Rect.Y + Self.Gui.fRect.H - fItems.Count * skinTx^.H);
  X := X_; Y := Y_;
  b := true;
  if Assigned(OnPopup) then
    OnPopup(Sender, X, Y, b);
  if b then
    Self.Gui.Handler.HandleEvent(self, hePopup);
end;

function zglTPopupMenu.getItem;
begin
  Result := fItems.Items[index];
end;

function zglTPopupMenu.getItemsCount;
begin
  Result := fItems.Count;
end;

procedure zglTPopupMenu.SetIconsSize(W, H: integer);
begin
  fIconsSize.X := W;
  fIconsSize.Y := H;
end;

procedure zglTPopupMenu.HandleDraw(evnt: THandleEvent);
var r, rct: zglTRect;
    i, icX: integer;
    item: zglTMenuItem;
    fnt: zglTFontObject;
    coef: Single;
    b: Boolean;
    skinTx, itemTx, chkTx: zglPGuiSkinTex;
    sciEnd: Boolean;
begin
  case evnt of
    hePopup: begin
      skinTx := Self.Gui.Skin.SkinTex[skinPopupMenu];
      itemTx := Self.Gui.Skin.SkinTex[skinPopupMenuItem];
      chkTx := Self.Gui.Skin.SkinTex[skinCheckItem];
      if fItems.Count = 0 then exit;
      rct := Self.Gui.getRect(X - skinTx^.OffX, Y - skinTx^.OffY,
        Width + skinTx^.OffX * 2,
        fItems.Count * (itemTx^.H + itemTx^.OffY * 2) + skinTx^.OffY * 2);

      coef := 1 - m_Cos(round(PopupTime * 90));

      if scissor_Rect(rct, @rct, nil, sciEnd) then begin
        rct.Y := rct.Y - rct.H * coef;
        Self.Gui.Skin.DrawField(skinPopupMenu,
          rct.X, rct.Y, rct.W, rct.H,
          0, 0, skinTx^.W div 2, skinTx^.H div 2);
        r.X := X; r.H := itemTx^.H + itemTx^.OffY * 2; r.W := Width;
        for i := 0 to fItems.Count -1 do begin
          item := fItems.Items[i];
          r.X := X;
          r.Y := Y + i * (itemTx^.H + itemTx^.OffY * 2) - rct.H * coef;
          if (item.Text = '-') then begin
            pr2d_Line(r.X + 2, r.Y + itemTx^.H div 2,
                      r.X + r.W - 2, r.Y + itemTx^.H div 2,
                      Self.Gui.Skin.SkinCfg[cfgMainMenuLine], 255);
          end else begin
            b := col2d_PointInRect(Mouse_X, Mouse_Y, r);
            if not item.Enabled then
              fnt := Font.GetForState(fstDisabled)
            else
              fnt := Font.GetForState(b);
            Self.Gui.Skin.DrawField(skinPopupMenuItem,
              r.X, r.Y, r.W, r.H,
              0,  Byte(b and item.Enabled) * itemTx^.H,
              itemTx^.W div 2, itemTx^.H div 2);
            icX := 0;
            if item.Checked then begin
              Self.Gui.Skin.DrawTex(skinCheckItem, r.X + itemTx^.OffX, r.Y + itemTx^.OffY,
                0, 0,
                chkTx^.W, chkTx^.H);
            end else
            if item.IconId > 0 then begin
              asprite2d_Draw(IconsTexture,
                r.X + itemTx^.OffX, r.Y + itemTx^.OffY, fIconsSize.X, fIconsSize.Y, 0, item.IconId);
              icX := round(fIconsSize.X + itemTx^.OffX);
            end;
            r.X := X + itemTx^.OffX;
            text_DrawEx(fnt.Data,
              r.X + chkTx^.W * Byte(item.Checkbox) + icX,
              r.Y + r.H / 2,
              fnt.Size, 0, item.Text, fnt.Alpha, fnt.Color,
              TEXT_HALIGN_LEFT or TEXT_VALIGN_CENTER);
          end;
        end;
        if sciEnd then
          scissor_End;
      end;
    end;
  end;
end;

function zglTPopupMenu.HandleUpdate;
var r, rct: zglTRect;
    i: integer;
    item: zglTMenuItem;
    itemTx: zglPGuiSkinTex;
begin

  if PopupTime > 0 then begin
    PopupTime := PopupTime - 0.005 * dt;
    if PopupTime < 0 then
      PopupTime := 0;
  end;

  itemTx := Self.Gui.Skin.SkinTex[skinPopupMenuItem];
  rct := Self.Gui.getRect(X, Y, Width, fItems.Count * (itemTx^.H + itemTx^.OffY * 2));
  Result := not col2d_PointInRect(Mouse_X, Mouse_Y, rct);
  r.X := X; r.H := itemTx^.H; r.W := Width;
  for i := fItems.Count -1 downto 0 do begin
    item := fItems.Items[i];
    r.Y := Y + i * (itemTx^.H + itemTx^.OffY * 2);
    if col2d_PointInRect(Mouse_X, Mouse_Y, r) then begin
      if item.Enabled then
        if mouse_Click(M_BLEFT) then begin
          if Assigned(Item.Callback) then begin
            r := Caller.updateRect;
            Item.CallBack(Caller, item, X - round(r.X), Y - round(r.Y));
          end;
          self.Caller := nil;
          Result := true;
          if Self.Gui.Handler.hHandle = hePopup then
            Self.Gui.Handler.Clear;
          mouse_ClearState;
        end;
    end;
  end;

end;

procedure zglTPopupMenu.HandleLeave(evnt: THandleEvent);
begin
  //
end;

constructor zglTPopupMenu.Create;
begin
  inherited Create(pGui);
  fItems := TList.Create;
  fOnPopup := nil;
  ShowIcons := false;
  IconsTexture := nil;
  with fIconsSize do begin
    X := 16; Y := 16;
  end;
  MinWidth := MinWidth_;
end;

constructor zglTPopupMenu.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  fItems := TList.Create;
  fOnPopup := nil;
  ShowIcons := false;
  IconsTexture := nil;
  with fIconsSize do begin
    X := 16; Y := 16;
  end;
  MinWidth := 100;
end;

function zglTPopupMenu.AddItem;
begin
  fItems.Add(Item);
  Item.UseCounter;
  result := self;
end;

procedure zglTPopupMenu.RemoveItem;
begin
  fItems.Remove(Item);
  Item.Drop;
end;

destructor zglTPopupMenu.Destroy;
var i: integer;
begin
  for i := 0 to ItemsCount - 1 do
    Items[i].Drop;
  fItems.Free;
  inherited;
end;
{$ENDIF}
{$IFDEF GUI_USE_PAGECONTROL}
// zglTPageControlSheet
constructor zglTPageControlSheet.Create;
begin
  inherited Create(pGui, 0, 0, 0, 0);
  fCaption := pCaption;
  fImage := pImage;
  Visible := true;
  fEditMode := false;
  CatchMouseClick := true;
  fEvenNotMove := true;
  UpdateClientRect;
end;

constructor zglTPageControlSheet.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  fCaption := '';
  fImage := nil;
  fParent := nil;
  Visible := true;
  fEditMode := false;
  CatchMouseClick := true;
  fEvenNotMove := true;
  UpdateClientRect;
end;

procedure zglTPageControlSheet.Show;
begin
  inherited;
  zglTPageControl(Parent).RelocateButtons;
end;

procedure zglTPageControlSheet.Hide;
begin
  inherited;
  zglTPageControl(Parent).RelocateButtons;
end;

procedure zglTPageControlSheet.Draw;
begin
  inherited;
  DrawEdit(oX, oY);
end;

procedure zglTPageControlSheet.Update;
begin
  if not Visible then Exit;
  inherited;
end;

procedure zglTPageControlSheet.setImage;
begin
  fImage := tex;
  fImageSize.X := 16;
  fImageSize.Y := 16;
  zglTPageControl(fParent).RelocateButtons;
end;

procedure zglTPageControlSheet.SetImageSize;
begin
  fImageSize.X := X;
  fImageSize.Y := Y;
  zglTPageControl(fParent).RelocateButtons;
end;


procedure zglTPageControlSheet.setCaption(capt: TCaption);
begin
  inherited;
  if Assigned(fParent) then
    zglTPageControl(fParent).RelocateButtons;
end;

destructor zglTPageControlSheet.Destroy;
begin
  inherited;
end;
// zglTPageControl
constructor zglTPageControl.Create;
begin
  inherited;
  CatchMouseClick := true;
  updateSkin(Gui.Skin);
end;

constructor zglTPageControl.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  CatchMouseClick := true;
  updateSkin(Gui.Skin);
end;

procedure zglTPageControl.updateSkin(skin: zglTGuiSkin);
var skinTx, sheetTx: zglPGuiSkinTex;
begin
  skinTx := Self.Gui.Skin.SkinTex[skinPageControl];
  sheetTx := Self.Gui.Skin.SkinTex[skinPageControlSheet];
  with fBorder do begin
    X := skinTx^.OffX; Y := sheetTx^.H + sheetTx^.OffY * 2 + skinTx^.OffY;
    W := skinTx^.OffX; H := skinTx^.OffY;
  end;
  UpdateClientRect;
end;

procedure zglTPageControl.Draw;
var rct, r, prct: zglTRect;
    i: integer;
    pcs: zglTPageControlSheet;
    fnt: zglTFontObject;
    b: Boolean;
    skinTx, sheetTx: zglPGuiSkinTex;
    sciEnd: boolean;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);
  skinTx := Self.Gui.Skin.SkinTex[skinPageControl];
  sheetTx := Self.Gui.Skin.SkinTex[skinPageControlSheet];
  Self.Gui.Skin.DrawField(skinPageControl,
    rct.X, rct.Y + sheetTx^.H + sheetTx^.OffY * 2,
    rct.W, rct.H - sheetTx^.H - sheetTx^.OffY * 2,
    0, 0, skinTx^.W div 2, skinTx^.H div 2);
  r := Self.Gui.getRect(0, rct.Y, 0, sheetTx^.H + sheetTx^.OffY * 2);
  for i := 0 to fItems.Count -1 do begin
    pcs := zglTPageControlSheet(fItems.Items[i]);
    if pcs.Visible then begin
      r.X := rct.X + pcs.X;
      r.W := pcs.Width;
      b := (fSelected = i);
      Self.Gui.Skin.DrawField(skinPageControlSheet,
        rct.X + pcs.X, rct.Y,
        pcs.Width, sheetTx^.H + sheetTx^.OffY * 2,
        0, Byte(b) * sheetTx^.H ,
        sheetTx^.W div 2, sheetTx^.H div 2);
      fnt := getRootFont(isHover(@r, oX, oY));
      if pcs.fImage <> nil then begin
        ssprite2d_Draw(pcs.fImage,
          rct.X + pcs.X + sheetTx^.OffX,
          rct.Y + sheetTx^.OffY + (sheetTx^.H div 2) -
            pcs.fImageSize.Y / 2,
          pcs.fImageSize.X, pcs.fImageSize.Y, 0);
      end;
      text_DrawEx(fnt.Data,
        rct.X + pcs.X + sheetTx^.OffX +
          Byte(pcs.fImage <> nil) * (sheetTx^.OffX + pcs.fImageSize.X),
        rct.Y + sheetTx^.OffY + (sheetTx^.H div 2),
        fnt.Size, 0, pcs.fCaption, fnt.Alpha, fnt.Color,
        TEXT_HALIGN_LEFT or TEXT_VALIGN_CENTER);
    end;
  end;
  if scissor_Rect(rct, clntRect, @prct, sciEnd) then begin
    if fItems.Count > 0 then begin
      zglTPageControlSheet(fItems.Items[fSelected]).Draw(@prct,
        rct.X + fScroll.X, rct.Y + fScroll.Y);
    end;
    if sciEnd then
      scissor_End;
  end;
  inherited DrawEdit(oX, oY);
end;

procedure zglTPageControl.Update;
var rct, r: zglTRect;
    i: integer;
    pcs: zglTPageControlSheet;
    sheetTx: zglPGuiSkinTex;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);

  sheetTx := Self.Gui.Skin.SkinTex[skinPageControlSheet];

  r := Self.Gui.getRect(0, rct.Y, 0, sheetTx^.H + sheetTx^.OffY * 2);
  if Enabled then
    for i := 0 to fItems.Count -1 do begin
      pcs := zglTPageControlSheet(fItems.Items[i]);
      if pcs.Visible then begin
        r.X := rct.X + pcs.X;
        r.W := pcs.Width;
        if isHover(@r, oX, oY) and fJustPressed then begin
          fSelected := i;
        end;
      end;
    end;
  if fItems.Count > 0 then begin
    pcs := zglTPageControlSheet(fItems.Items[fSelected]);
    pcs.Update(dt, clntRect, true, rct.X + fScroll.X, rct.Y + fScroll.Y);
    //if pcs.fHover and pcs.CatchMouseClick then
    //  lockHover := true;
  end;
  inherited Update(dt, clntRect, false, oX, oY);
end;

procedure zglTPageControl.onAdd;
begin
  inherited;
  zglTPageControlSheet(Sender).Move(ClientRect.X, ClientRect.Y);
  zglTPageControlSheet(Sender).Resize(ClientRect.W, ClientRect.H);
  fSelected := fItems.Count -1;
  RelocateButtons;
end;

procedure zglTPageControl.Resize;
var i: Integer;
begin
  for i := 0 to fItems.Count - 1 do begin
    zglTPageControlSheet(fItems.Items[i]).Move(ClientRect.X, ClientRect.Y);
    zglTPageControlSheet(fItems.Items[i]).Resize(ClientRect.W, ClientRect.H);
  end;
  inherited;
end;

function zglTPageControl.getSheet;
begin
  result := zglTPageControlSheet(fItems.Items[Index]);
end;

procedure zglTPageControl.setSelected;
begin
  fSelected := Index;
  if fSelected > fItems.Count - 1 then
    fSelected := fItems.Count - 1;
end;

procedure zglTPageControl.RelocateButtons;
var i, px, l: integer;
    sht: zglTPageControlSheet;
    fnt: zglTFontObject;
    sheetTx: zglPGuiSkinTex;
begin
  sheetTx := Self.Gui.Skin.SkinTex[skinPageControlSheet];

  px := 0;
  fnt := getRootFont(false);
  for i := 0 to fItems.Count - 1 do begin
    sht := zglTPageControlSheet(fItems.Items[i]);
    if sht.Visible then begin
      sht.X := px;
      l := Round( text_GetWidth(fnt.Data, sht.fCaption) * fnt.Size )
        + sheetTx^.OffX * 2;
      if sht.fImage <> nil then
        l := l + sheetTx^.OffX + round(sht.fImageSize.X);
      sht.Width := l;
      Inc(px, l);
    end;
  end;
end;

procedure zglTPageControl.onRemove;
begin
  if fSelected = fItems.Count -1 then
    fSelected := fItems.Count -2;
  RelocateButtons;
end;

destructor zglTPageControl.Destroy;
begin
  inherited;
end;
{$ENDIF}

{$IFDEF GUI_USE_EDITOR}
procedure zglTUnknownObject.Draw;
var fnt: zglTFontObject;
    rct: zglTRect;
    sciEnd: boolean;
begin
  if not Visible then Exit;
  rct := updateRect(oX, oY);

  if scissor_Rect(rct, clntRect, nil, sciEnd) then begin
    pr2d_Rect(rct.X, rct.Y, rct.W, rct.H, $FF0000, 64);
    fnt := getRootFont(fHover); ;
    text_DrawInRectEx(fnt.Data, rct, fnt.Size, 0, UnknownProperties.Text,
      fnt.Alpha, fnt.Color, FontAlignes[fnt.Align] or TEXT_CLIP_RECT);
    if sciEnd then
      scissor_End;
  end;
  rct.Y := rct.Y - 16;
  rct.H := 16;
  if scissor_Rect(rct, clntRect, nil, sciEnd) then begin
    pr2d_Rect(rct.X, rct.Y, rct.W, rct.H, $FF0000, 32, PR2D_FILL);
    text_DrawInRectEx(fnt.Data, rct, fnt.Size, 0, UnknClassName,
      fnt.Alpha, fnt.Color, FontAlignes[fnt.Align] or TEXT_CLIP_RECT);
    if sciEnd then
      scissor_End;
  end;
  inherited DrawEdit(oX, oY);
end;

procedure zglTUnknownObject.Update;
begin
  if not Visible then Exit;
  inherited;
end;

constructor zglTUnknownObject.Create;
begin
  inherited Create(pGui, pX, pY, pW, pH, pVisible);
  Caption := pCaption;
  UnknownProperties := TStringList.Create;
end;

constructor zglTUnknownObject.CreateDefaults(pGui: zglTGui);
begin
  inherited CreateDefaults(pGui);
  Caption := 'zglTUnknown';
end;

destructor zglTUnknownObject.Destroy;
begin
  UnknownProperties.Free;
  inherited;
end;
{$ENDIF}

// FONT

constructor zglTFontContainer.Create(pNormal, pActive, pDisabled: zglTFontObject);
begin
  inherited Create(emTrue);
  Normal := pNormal;
  Normal.UseCounter;
  Active := pActive;
  Active.UseCounter;
  Disabled := pDisabled;
  Disabled.UseCounter;
end;

constructor zglTFontContainer.CreateDefaults;
begin
  inherited Create(emTrue);
  Normal := zglTFontObject.CreateDefaults(pGui);
  Normal.UseCounter;
  Active := zglTFontObject.CreateDefaults(pGui);
  Active.UseCounter;
  Disabled := zglTFontObject.CreateDefaults(pGui);
  Disabled.UseCounter;
end;

destructor zglTFontContainer.Destroy;
begin
  Normal.Drop;
  Active.Drop;
  Disabled.Drop;
end;

constructor zglTFontContainer.Create(pData: zglPFont; pSize: Single;
  pColor: integer; pAlpha: Byte; pAlign: TFontAlign = faMiddleCenter);
begin
  inherited Create(false);
  Normal := zglTFontObject.Create(pData, pSize, pColor, pAlpha, pAlign);
  Active := Normal;
  Disabled := Normal;
  Normal.UseCounter(3);
end;

procedure zglTFontContainer.setFileName(name: AnsiString);
var f: zglPFont;
begin
  fFileName := name;
  f := font_LoadFromFile(name);
  Normal.fData := f;
  if Active <> Normal then
    Active.fData := f;
  if Disabled <> Normal then
    Disabled.fData := f;
end;

procedure zglTFontObject.setFileName(name: AnsiString);
var f: zglPFont;
begin
  fFileName := name;
  f := font_LoadFromFile(name);
  fData := f;
end;

function zglTFontContainer.GetForState(state: TFontState): zglTFontObject;
begin
  case state of
    fstNormal: Result := Normal;
    fstActive: Result := Active;
    else Result := Disabled;
  end;
end;

function zglTFontContainer.GetForState(state: boolean): zglTFontObject;
begin
  case state of
    false: Result := Normal;
    true: Result := Active;
  end;
end;

// zglTHandler

constructor zglTHandler.Create;
begin
  hHandle := heNone;
  canHover := true;
end;

procedure zglTHandler.HandleEvent;
var lhObject: zglTBaseGUIObject;
begin
  if hObject <> nil then begin
    hObject.HandleLeave(hHandle);
  end;
  lhObject := hObject;
  hHandle := evnt; hObject := obj;
  canHover := false;
  if (hObject <> nil) and (obj <> lhObject) then begin
    hObject.HandleBegin(hHandle);
  end;
end;

procedure zglTHandler.Clear;
begin
  if hObject <> nil then begin
    hObject.HandleLeave(hHandle);
  end;
  hObject := nil; hHandle := heNone;
  canHover := True;
end;

procedure zglTHandler.Draw;
begin
  if Assigned(hObject) then begin
    hObject.HandleDraw(hHandle);
  end;
end;


procedure zglTHandler.Update;
begin
  if Assigned(hObject) then begin
    canHover := hObject.HandleUpdate(dt, hHandle);
  end;
end;

// zglTCTexture

procedure zglTCTexture.setTexWidth(W: integer);
begin
  fTexWidth := W;
  if fTexture <> nil then begin
    if TexWidth = 0 then
      fTexWidth := fTexture^.Width;
    tex_SetFrameSize(fTexture, TexWidth, TexHeight);
  end;
end;

procedure zglTCTexture.setTexHeight(H: integer);
begin
  fTexHeight := H;
  if fTexture <> nil then begin
    if TexHeight = 0 then
      fTexHeight := fTexture^.Height;
    tex_SetFrameSize(fTexture, TexWidth, TexHeight);
  end;
end;

procedure zglTCTexture.setFileName(Name: TCaption);
begin
  fFileName := Name;
  if fTexture <> nil then
    tex_Del(fTexture);
  fTexture := tex_LoadFromFile(Name);
  if fTexture <> nil then begin
    if TexWidth = 0 then
      fTexWidth := fTexture^.Width;
    if TexHeight = 0 then
      fTexHeight := fTexture^.Height;
    tex_SetFrameSize(fTexture, TexWidth, TexHeight);
  end;
end;

procedure zglTCTexture.Draw(X, Y, W, H: Single; Frame: integer; Alpha: Byte);
begin
  if UseFrames then
    asprite2d_Draw(Texture, X, Y, W, H, 0, Frame + FrameOffset, Alpha)
  else
    ssprite2d_Draw(Texture, X, Y, W, H, 0, Alpha);
end;

constructor zglTCTexture.Create(pTexture: zglPTexture);
begin
  inherited Create(true);
  Texture := pTexture;
  if Assigned(texture) then
  begin
  fTexWidth := Texture^.Width;
  fTexHeight := Texture^.Height;
  end;
  fFileName := '';
  fTexWidth := 0;
  fTexHeight := 0;
  FrameOffset := 0;
  UseFrames := false;
end;

destructor zglTCTexture.Destroy;
begin
  if Texture <> nil then
    tex_Del(fTexture);
  inherited;
end;

// zglTGuiSkin

function zglTGuiSkin.getSkinTex(Id: TSkinTextures): zglPGuiSkinTex;
begin
  Result := @fSkinTex[Cardinal(Id)];
end;

function zglTGuiSkin.getSkinCfg(Id: TSkinConfigs): integer;
begin
  Result := fSkinCfg[Cardinal(Id)];
end;

procedure zglTGuiSkin.Load(FileName: TCaption;
  Filter: LongWord; ZipFilePassword: TCaption);
var i: integer;
begin
  if not file_Exists(FileName) then begin
    log_Add('Error: ' + TCaption(FileName) + ' do not exists!');
    exit;
  end;

  SetLength(fSkinTex, SKINTEX_COUNT);
  SetLength(fSkinCfg, SKINCFG_COUNT);

  for i := 0 to SKINTEX_COUNT - 1 do
    fSkinTex[i].Texture := nil;

  for i := 0 to SKINCFG_COUNT - 1 do
    fSkinCfg[i] := 0;
  if file_OpenArchive(FileName, ZipFilePassword) then begin
    if not ini_LoadFromFile('config.ini') then
      log_Add('Error: ' + FileName + '/config.ini do not exists!');

    for i := 0 to SKINTEX_COUNT - 1 do begin
      fSkinTex[i].Texture :=
        tex_LoadFromFile('tx' + SkinTextures[i] + '.tga', $FF000000,
          Filter);
      fSkinTex[i].W :=
        ini_ReadKeyInt(SkinTextures[i], 'W');
      fSkinTex[i].H :=
        ini_ReadKeyInt(SkinTextures[i], 'H');
      fSkinTex[i].BtnW :=
        ini_ReadKeyInt(SkinTextures[i], 'BtnW');
      fSkinTex[i].BtnH :=
        ini_ReadKeyInt(SkinTextures[i], 'BtnH');
      fSkinTex[i].ScrW :=
        ini_ReadKeyInt(SkinTextures[i], 'ScrW');
      fSkinTex[i].ScrH :=
        ini_ReadKeyInt(SkinTextures[i], 'ScrH');
      fSkinTex[i].OffX :=
        ini_ReadKeyInt(SkinTextures[i], 'OffX');
      fSkinTex[i].OffY :=
        ini_ReadKeyInt(SkinTextures[i], 'OffY');
    end;

    for i := 0 to SKINCFG_COUNT - 1 do begin
      fSkinCfg[i] := ini_ReadKeyInt('Options', SkinConfigs[i]);
    end;

    tex_SetFrameSize(getSkinTex(skinMouse)^.Texture,
      getSkinTex(skinMouse)^.W, getSkinTex(skinMouse)^.H);

    ini_Free;
    file_CloseArchive;
  end else begin
    log_Add('Error: skin file is not a zip archive or wrong password!');
  end;
end;

procedure zglTGuiSkin.DrawTex;
var rct: zglTRect;
begin
  rct.X := dX; rct.Y := dY; rct.W := dW; rct.H := dH;
  csprite2d_Draw(SkinTex[TexId]^.Texture, X, Y, dW, dH, 0, rct);
end;

procedure zglTGuiSkin.DrawTexScaled;
var rct: zglTRect;
begin
  rct.X := dX; rct.Y := dY; rct.W := dW; rct.H := dH;
  csprite2d_Draw(SkinTex[TexId]^.Texture, X, Y, W, H, 0, rct);
end;


function zglTGuiSkin.Correct(SkinSize, RealSize: single): Single;
begin
  if SkinSize > RealSize / 2 then
    Result :=  RealSize / 2
  else
    Result := SkinSize;
end;

procedure zglTGuiSkin.DrawField;
var rct: zglTRect;
    WdW, HdH, _dW, _dH: Single;
    fTex: zglPTexture;
begin
  fTex := SkinTex[TexId]^.Texture;
  rct.X := dX; rct.Y := dY; rct.W := dW; rct.H := dH;

  _dW := Correct(dW, W);
  _dH := Correct(dH, H);

  WdW := W - _dW;
  HdH := H - _dH;
  csprite2d_Draw(fTex, X, Y, _dW, _dH, 0, rct);
  rct.X := round(dX + dW);
  csprite2d_Draw(fTex, X + WdW, Y, _dW, _dH, 0, rct);
  rct.Y := round(dY + dH);
  csprite2d_Draw(fTex, X + WdW, Y + HdH, _dW, _dH, 0, rct);
  rct.X := round(dX);
  csprite2d_Draw(fTex, X, Y + HdH, _dW, _dH, 0, rct);
  rct.X := round(dX + dW); rct.Y := round(dY); rct.W := 0; rct.H := dH;
  csprite2d_Draw(fTex, X + dW, Y, W - _dW*2, _dH, 0, rct);
  rct.Y := round(dY + dH);
  csprite2d_Draw(fTex, X + dW, Y + HdH, W - _dW*2, _dH, 0, rct);
  rct.H := 0;
  csprite2d_Draw(fTex, X + dW, Y + dH, W - _dW*2, H - _dH*2, 0, rct);
  rct.W := round(dW); rct.X := round(dX);
  csprite2d_Draw(fTex, X, Y + dH, _dW, H - _dH*2, 0, rct);
  rct.X := round(dX + dW);
  csprite2d_Draw(fTex, X + WdW, Y + dH, _dW, H - _dH*2, 0, rct);
end;

constructor zglTGuiSkin.Create(FileName: TCaption; Filter: LongWord; ZipFilePassword: TCaption);
begin
  Load(FileName, Filter, ZipFilePassword);
end;

destructor zglTGuiSkin.Destroy;
var i: integer;
begin
  for i := 0 to SKINTEX_COUNT - 1 do
    if fSkinTex[i].Texture <> nil then
      tex_Del(fSkinTex[i].Texture);
  fSkinTex := nil;
  fSkinCfg := nil;
  inherited;
end;

// GUI

function zglTGui.AddState: Word;
begin
  fItems.Add(zglTGUIObjectsList.Create(Self, nil));
  Result := fItems.Count - 1;
end;

procedure zglTGui.DeleteState(State: Word);
begin
  if fItems.Count > 1 then begin
    if State < fItems.Count then begin
      zglTGUIObjectsList(fItems.Items[State]).Free;
      fItems.Delete(State);
    end;
  end;
end;

function zglTGui.getStatesCount: Integer;
begin
  Result := fItems.Count;
end;

procedure zglTGui.setFont(fontName: zglTFontContainer);
begin
  if (fFont <> fontName) then begin
    if Assigned(fFont) then
      fFont.Drop;
    fFont := fontName;
    if Assigned(fFont) then
      fFont.UseCounter;
  end;
end;

procedure zglTGui.updateSkin;
var i: Integer;
begin
  for i := 0 to StatesCount - 1 do begin
    updateSkinItems(ItemsForState[i]);
  end;
end;

procedure zglTGui.closeShowMessage(Sender: zglTGUIObject; X, Y: integer);
begin
  Sender.Parent.Hide;
  FormToDelete := zglTForm(Sender.Parent);
end;

function zglTGui.getState(Index: Word): zglTGUIObjectsList;
begin
  if Index < fItems.Count then
    Result := fItems.Items[index]
  else
    Result := nil;
end;

constructor zglTGui.Create;
begin
  Handler := zglTHandler.Create;
  Handler.gui := self;
  fItems := TList.Create;
  State := AddState;
  fVisible := True; fHover := nil;
  Font := defaultFont;
  fRect := getRect(screenX, screenY, screenWidth, screenHeight);
  fSkin := pSkin;
  grid.Cols := 4; grid.Rows := 4;
  SetLength(grid.Grid, 4, 4);
  fCounter := 1;
  FormToDelete := nil;
  Mouse := MOUSE_POINTER;
  {$IFDEF GUI_USE_EDITOR}
  ForceVisible := false;
  {$ENDIF}
  fModal := zglTGUIObjectsList.Create(self, nil);
end;

function zglTGui.getCounter: integer;
begin
  Result := fCounter;
  inc(fCounter);
end;

procedure zglTGui.setSkin(const AValue: zglTGuiSkin);
begin
  if fSkin = AValue then exit;
  fSkin := AValue;
  updateSkin;
end;

procedure zglTGui.updateSkinItems(list: zglTGUIObjectsList);
var i: integer;
begin
  for i := 0 to list.Count - 1 do begin
    list.Items[i].updateSkin(Skin);
    if list.Items[i].Container then begin
      updateSkinItems(zglTFrame(list.Items[i]).Items);
    end;
  end;
end;

procedure zglTGui.Resize;
begin
  fRect.W := screenWidth;
  fRect.H := screenHeight;
  if Assigned(fOnResize) then
    fOnResize(self);
end;

destructor zglTGui.Destroy;
var i: integer;
begin
  for i := 0 to fItems.Count - 1 do
    zglTGUIObjectsList(fItems.Items[i]).Free;
  fModal.Clear(false);
  fModal.Free;
  fItems.Free;
  Handler.Free;
  if fFont <> nil then
    fFont.Drop;
  inherited;
end;

procedure zglTGui.DrawMouse;
var tex: zglPGuiSkinTex;
begin
  if Mouse > 0 then begin
    tex := Skin.getSkinTex(skinMouse);
    asprite2d_Draw(tex^.Texture,
      mouse_X - tex^.W div 2,
      mouse_Y - tex^.H div 2,
      tex^.W, tex^.H, 0, Mouse);
  end;
end;


procedure zglTGui.Draw;
var i: integer;
begin
  if not Visible then Exit;
  if Modal.Count > 0 then begin
    for i := 0 to Items.Count - 1 do
      if Modal.Find(Items.Items[i]) = -1 then
        if Items.Items[i].Visible then
          Items.Items[i].Draw(@fRect, fRect.X, fRect.Y);
    for i := 0 to Modal.Count - 1 do begin
      if Modal.Items[i].Visible then begin
        pr2d_Rect(fRect.X, fRect.Y, fRect.W, fRect.H, $0, 64, PR2D_FILL);
        if Modal.Items[i] is zglTForm then begin
          if Assigned(zglTForm(Modal.Items[i]).Effect) then
            zglTForm(Modal.Items[i]).Effect.Draw(@fRect, fRect.X, fRect.Y)
          else
            Modal.Items[i].Draw(@fRect, fRect.X, fRect.Y);
        end else
          Modal.Items[i].Draw(@fRect, fRect.X, fRect.Y);
      end;
    end;
  end else
    for i := 0 to Items.Count - 1 do
      if Items.Items[i].Visible then
        if Items.Items[i] is zglTForm then begin
          if Assigned(zglTForm(Items.Items[i]).Effect) then
            zglTForm(Items.Items[i]).Effect.Draw(@fRect, fRect.X, fRect.Y)
          else
            Items.Items[i].Draw(@fRect, fRect.X, fRect.Y);
        end else
          Items.Items[i].Draw(@fRect, fRect.X, fRect.Y);
  Handler.Draw;
end;

procedure zglTGui.Update;
var i: integer;
begin
  if not Visible then Exit;
  fHover := nil;
  Handler.Update(dt);
  if Modal.Count > 0 then begin
    if Modal.Items[Modal.Count - 1] is zglTForm then
      if Assigned(zglTForm(Modal.Items[Modal.Count - 1]).Effect) then
        zglTForm(Modal.Items[Modal.Count - 1]).Effect.Update(dt);
    Modal.Items[Modal.Count - 1].Update(dt, @fRect, true, fRect.X, fRect.Y);
  end else begin
    for i := Items.Count - 1 downto 0 do
      if Items.Items[i].Visible then begin
        if Items.Items[i] is zglTForm then
          if Assigned(zglTForm(Items.Items[i]).Effect) then
            zglTForm(Items.Items[i]).Effect.Update(dt);
        Items.Items[i].Update(dt, @fRect, true, fRect.X, fRect.Y);
      end;
  end;

  if mouse_Click(M_BLEFT) then
    Handler.Clear;
  if FormToDelete <> nil then begin
    Items.Remove(FormToDelete);
    FormToDelete := nil;
  end;
  if Assigned(Hover) then begin
    if Hover.Mouse > 0 then
      Mouse := Hover.Mouse;
  end;
end;

procedure zglTGui.ShowMessage(Caption, Data: TCaption; Effect: TDisplayEffect = deZoomIn);
var f: zglTForm;
    cpt: zglTLabel;
    ok: zglTButton;
    tW, tH: single;
    fnt: zglTFontObject;
begin
  fnt := Font.GetForState(fstNormal);
  tW := text_GetWidth(fnt.Data, Data) * fnt.Size + 20;
  tH := text_GetHeight(fnt.Data, text_GetWidth(fnt.Data, Data), Data, fnt.Size);

  f := zglTForm.Create(self, 0, 0, Min(Max(tW, 200), Rect.W - 20), 90 + tH, Caption);

  cpt := zglTLabel.Create(Self, 0, 10, f.ClientRect.W, 30 + tH, Data);
  f.Items.Add(cpt);

  ok := zglTButton.Create(Self, f.ClientRect.W / 2 - 40, 45 + tH, 80, 20, 'OK');
  f.Items.Add(ok);
  ok.OnClick := Self.closeShowMessage;

  f.DisplayEffect := Effect;

  Items.Add(f);
  f.MoveToCenter;
  f.ShowModal;
end;

procedure zglTGui.ClearModal;
begin
  fModal.Clear;
end;

function zglTGui.getRect(X, Y, W, H: single):zglTRect;
begin
  result.X := X;
  result.Y := Y;
  result.W := W;
  result.H := H;
end;


procedure zglTGui.setState(State: Word);
begin
  fState := State;
  if fState >= fItems.Count then begin
    fState := fItems.Count - 1;
  end;
end;

function zglTGui.getItems: zglTGUIObjectsList;
begin
  Result := fItems.Items[State];
end;

constructor zglTFadeInEffect.Create(pGui: zglTGUI; pComponent: zglTForm);
begin
  inherited;
  EndTime := 600;
end;

procedure zglTFadeInEffect.DrawEffect;
var rct: zglTRect;
begin
  rct := Component.UpdateRect(oX, oY);

  ssprite2d_Draw(
    Render.Surface,
    rct.X, rct.Y,
    rct.W, rct.H, 0, Round(Time / EndTime * 255));
end;

constructor zglTSlideDownEffect.Create(pGui: zglTGUI; pComponent: zglTForm);
begin
  inherited;
  EndTime := 300;
end;

procedure zglTSlideDownEffect.DrawEffect;
var rct: zglTRect;
    coef: Single;
begin
  rct := Component.UpdateRect(oX, oY);

  if Time = EndTime then
    coef := 1
  else
    coef := Sin(deg2rad * Time / EndTime * 90);

  ssprite2d_Draw(
    Render.Surface,
    rct.X, rct.Y - rct.H * (1 - coef) * 0.5,
    rct.W, rct.H * (coef * 0.5 + 0.5), 0, Round(coef * 255));
end;

constructor zglTZoomInEffect.Create(pGui: zglTGUI; pComponent: zglTForm);
begin
  inherited;
  EndTime := 600;
end;

procedure zglTZoomInEffect.DrawEffect;
var rct: zglTRect;
    coef: Single;
begin
  rct := Component.UpdateRect(oX, oY);

  if Time = EndTime then
    coef := 1
  else
    // why 126.8698?
    // cuz in end we need X x 1.25 = 1,
    //   so X = 1 / 1.25 = 0.8,
    //   so arcsin(0.8) = 126.8698
    coef := Sin(deg2rad * Time / EndTime * 126.8698) * 1.25;

  ssprite2d_Draw(
    Render.Surface,
    rct.X + rct.W * (1 - coef) * 0.5, rct.Y + rct.H * (1 - coef) * 0.5,
    rct.W * coef, rct.H * coef, 0);
end;

procedure zglTGuiEffect.UpdateEffect;
begin
  //
end;

procedure zglTGuiEffect.Draw;
var r: zglTRect;
begin
  if not Inited then begin
    Inited := true;
    Init;
  end;
  r := Component.Rect.GetRect;
  r.X := 0; r.Y := 0;
  rtarget_Set(Render);
    Component.Draw(nil, - Component.Rect.X, - Component.Rect.Y);
  rtarget_Set(nil);
  if RenderRes then
    DrawEffect(clntRect, oX, oY);
end;

procedure zglTGuiEffect.Init;
begin
  batch2d_Flush();
  Render := rtarget_Add(
    tex_CreateZero(
      round(Component.Rect.W),
      round(Component.Rect.H),
      $FFFFFFFF,
      TEX_DEFAULT_2D),
    RT_CLEAR_COLOR);
end;

procedure zglTGuiEffect.Clear;
begin
  Time := 0;
end;

procedure zglTGuiEffect.Update;
begin
  UpdateEffect(dt);
  Time := Time + dt;

  if EndTime <> 0 then
    if Time > EndTime then begin
      Component.Effect := nil;
      Free;
    end;
end;

constructor zglTGuiEffect.Create;
begin
  Component := pComponent;

  Gui := pGui;

  Time := 0;
  EndTime := 0;
end;

destructor zglTGuiEffect.Destroy;
begin
  inherited;
  rtarget_Del(fRender);
end;

constructor zglTGUIObjectsList.Create;
begin
  fObjects := TList.Create;
  fGui := pGui;
  fMaster := pMaster;
end;

destructor zglTGUIObjectsList.Destroy;
var i: integer;
    item: zglTGUIObject;
begin
  for i := 0 to fObjects.Count -1 do begin
    item := fObjects.Items[i];
    Item.Drop;
  end;
  fObjects.Free;
  inherited;
end;

procedure zglTGUIObjectsList.RemoveAll;
var i: integer;
    item: zglTGUIObject;
begin
  for i := 0 to fObjects.Count -1 do begin
    item := fObjects.Items[i];
    Item.Free;
  end;
  fObjects.Clear;
end;

procedure zglTGUIObjectsList.Remove;
begin
  if Assigned(fMaster) then
    fMaster.onRemove(obj);
  fObjects.Remove(obj);
  if doDrop then
     obj.Drop;
end;

function zglTGUIObjectsList.Add;
begin
  Obj.UseCounter;
  fObjects.Add(obj);
  Obj.fParent := fMaster;
  Obj.Gui := fGui;
  Obj.OnAddMe;
  Result := obj;
  if Assigned(fMaster) then
    fMaster.onAdd(obj);
end;

function zglTGUIObjectsList.getObjectsCount;
begin
  Result := fObjects.Count;
end;

function zglTGUIObjectsList.getObject(Index: Integer):zglTGUIObject;
begin
  Result := fObjects.Items[Index];
end;

procedure zglTGUIObjectsList.MoveToTop(obj: zglTGUIObject);
begin
  fObjects.Move(fObjects.IndexOf(obj), fObjects.Count - 1);
end;

procedure zglTGUIObjectsList.MoveToBottom(obj: zglTGUIObject);
begin
  fObjects.Move(fObjects.IndexOf(obj), 0);
end;

function zglTGUIObjectsList.Find(Obj: zglTGUIObject): integer;
begin
  Result := fObjects.IndexOf(Obj);
end;

procedure zglTGUIObjectsList.Clear(free: boolean);
var i: integer;
begin
  if free then
    for i := 0 to Count - 1 do
      Items[i].Free;
  fObjects.Clear;
end;

function zglTGUIObjectsList.Find(Name: TCaption): zglTGUIObject;
var i: integer;
    obj: zglTGUIObject;
begin
  Result := nil;
  for i := 0 to Count - 1 do begin
    if Items[i].Name = Name then begin
      Result := Items[i];
      exit;
    end;
    if Items[i].Container then begin
      obj := zglTFrame(Items[i]).Items.Find(Name);
      if Assigned(obj) then begin
        Result := obj;
        exit;
      end;
    end;
  end;
end;

function zglTCRect.GetRect:  zglTRect;
begin
  Result.X := X;
  Result.Y := Y;
  Result.W := W;
  Result.H := H;
end;

initialization

end.
