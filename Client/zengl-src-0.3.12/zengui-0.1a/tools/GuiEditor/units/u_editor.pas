unit u_editor;

interface

uses zglGui, SysUtils, Classes,

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
  zgl_textures_tga, zgl_textures_jpg, zgl_textures_png,

  {$IFDEF FPC}
  Interfaces,
  {$ENDIF}
  TypInfo, Dialogs, Filectrl, u_components, u_editorgui, RegExpr;

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

  procedure Init;
  procedure Draw;
  procedure Update( dt : Double );
  procedure Quit;

const
  sX_ = 800;
  sY_ = 600;
  FullScreen_ = False;
  SYSWIDTH = 300;
  SYSOFFSET = 8;
  FORMSHEIGHT = 200;
  PROPERTIESHEIGHT = 200;
  DEFFORMSIZE = 400;
  LINESS_COUNT = 4;

  DEMOMESSAGE = 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, '
    + 'sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. U'
    + 't enim ad minim veniam, quis nostrud exercitation ullamco laboris ni'
    + 'si ut aliquip ex ea commodo consequat. Duis aute irure dolor in repr'
    + 'ehenderit in voluptate velit esse cillum dolore eu fugiat nulla pari'
    + 'atur. Excepteur sint occaecat cupidatat non proident, sunt in culpa '
    + 'qui officia deserunt mollit anim id est laborum';

  TEX_FORM = 1;
  TEX_BUTTON = 2;
  TEX_CHECKBOX = 3;
  TEX_COMBOBOX = 4;
  TEX_EDIT = 5;
  TEX_LABEL = 6;
  TEX_IMAGE = 7;
  TEX_PROGRESSBAR = 8;
  TEX_RADIOGROUP = 9;
  TEX_RADIOBUTTON = 10;
  TEX_TRACKBAR = 11;
  TEX_PAGECONTROL = 12;
  TEX_PAGE = 13;
  TEX_LIST = 14;
  TEX_TABLE = 15;
  TEX_STATE = 16;
  TEX_UNKNOWN = 17;
  TEX_IMAGEBUTTON = 18;
  TEX_MULTIBUTTON = 19;
  TEX_FRAME = 20;

  EVENTS_COUNT = 9;

var
  sX, sY: integer;
  FullScreen: Boolean;

  EVENTS: array[0.. EVENTS_COUNT - 1] of record
    name: TCaption;
    value: TCaption;
  end = (
    (name: 'zglTEvent';
      value: 'Sender: zglTGUIObject'),
    (name: 'zglTMMEvent';
      value: 'SelectedElement: zglTMainMenuItem'),
    (name: 'zglTMenuEvent';
      value: 'Sender: zglTGUIObject; SelectedElement: zglTMenuItem; X, Y: integer'),
    (name: 'zglTGuiEvent';
      value: 'Sender: zglTGui'),
    (name: 'zglTMouseEvent';
      value: 'Sender: zglTGUIObject; X, Y: integer'),
    (name: 'zglTPopupEvent';
      value: 'Sender: zglTGUIObject; X, Y: integer; var CanPopup: Booleant'),
    (name: 'zglTDrawEvent';
      value: 'Sender: zglTGUIObject; X, Y, W, H, scrX, scrY: Single'),
    (name: 'zglTKeyEvent';
      value: 'Sender: zglTGUIObject; Key: Byte; var Cancel: Boolean'),
    (name: 'zglTTableEvent';
      value: 'Sender: zglTGUIObject; SelectedElement: zglTTableItem')
  );

type
  TCreateCallback = function(X, Y, W, H: Integer): zglTGUIObject;
  TActionEvent = (aeClear, aeOpen, aeExit);
  PTextFile = ^TextFile;

  TEditorEvent = class(TObject)
    private
      fText: TStringList;
      fEventName, fEventType: TCaption;
      fItem: zglTTableItem;
    public
      property Item: zglTTableItem read fItem write fItem;
      property EventName: TCaption read fEventName write fEventName;
      property EventType: TCaption read fEventType write fEventType;
      property Text: TStringList read fText write fText;
      constructor Create;
      destructor Destroy; override;
  end;

  TEventHandler = class(TObject)
    private
      fEvent: TEditorEvent;
      fEventObject: zglTNamedObject;
      fName: TCaption;
    public
      property Name: TCaption read fName write fName;
      property EventObject: zglTNamedObject read fEventObject write fEventObject;
      property Event: TEditorEvent read fEvent write fEvent;
      constructor Create;
      destructor Destroy; override;
  end;

  TEditor = class;

  TGUIState = class(TObject)
    private
      fListItem: zglTListItem;
      fId: integer;
      fName: TCaption;
    public
      property ListItem: zglTListItem read fListItem write fListItem;
      property Name: TCaption read fName write fName;
      property Id: Integer read fId write fId;

      constructor Create(Ed: TEditor);
      destructor Destroy; override;
  end;

  TEditor = class(TObject)
    private
      fEvents, fEventHandlers, fStates: TList;
      f_Interface, f_Implementation, fCurrent: TCaption;
      fWorkingDir: string;
      fActionEvent: TActionEvent;
      fEdited: Boolean;
    public
      property ActionEvent: TActionEvent read fActionEvent write fActionEvent;
      property _Interface: TCaption read f_Interface write f_Interface;
      property _Implementation: TCaption read f_Implementation write f_Implementation;
      property Current: TCaption read fCurrent write fCurrent;
      property Edited: boolean read fEdited write fEdited;
      property WorkingDir: string read fWorkingDir write fWorkingDir;

      function CreateForm: zglTForm;
      function CreateFrame: zglTFrame;
      function CreateFont: zglTFontContainer;
      function BrowseRelativePath: boolean;
      procedure InitObject(Obj: zglTNamedObject);
      procedure RegisterObject(Obj: zglTNamedObject);
      procedure InitObjParams(Root, Obj: zglTNamedObject; Prefix: TCaption = '');
      procedure AddProperty(Obj: zglTNamedObject; Name, Value: TCaption);
      procedure AddObject(Owner: zglTFrame; Obj: zglTGuiObject; InitObj: boolean = true);
      procedure RegisterComponent(Name: TCaption; CrtCllbck: PCoponentCaller);
      function RegisterState: TGUIState;
      procedure EditComponent(Obj: zglTNamedObject; Level: Byte = 0);
      function RegisterEvent(pName, pType: TCaption): TEditorEvent;
      function RegisterEventHandle(Obj: zglTNamedObject;
        Event: TEditorEvent; pName: TCaption): TEventHandler;
      function GetObjectEvent(Obj: zglTNamedObject;
        pName: TCaption): TEventHandler;
      procedure Clear;
      procedure DeleteEventHandle(Ev: TEventHandler);
      procedure DeleteEvent(Ev: TEditorEvent);
      procedure DeleteState(st: TGUIState);
      function GetState(Id: integer): TGUIState;
      function GetEvent(pName: TCaption): TEditorEvent;
      function GetEventId(pName: TCaption): integer;
      constructor Create;
      destructor Destroy; override;
  end;

  { THandler }

  THandler = class
    class procedure guiSelect(Sender: zglTGUIObject; X, Y: integer);
    class procedure onStartCreate(Sender: zglTGUIObject;
      SelectedElement: zglTMenuItem; X, Y: integer);
    class procedure createFrame(Sender: zglTGUIObject;
      SelectedElement: zglTMenuItem; X, Y: integer);
    class procedure guiUpdateName(Sender: zglTGUIObject);
    class procedure guiUpdateFontName(Sender: zglTGUIObject);
    class procedure guiUpdateEventName(Sender: zglTGUIObject);
    class procedure guiUpdateState(Sender: zglTGUIObject);
    class procedure guiPropBoolean(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiBrowseFileName(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiBrowseColor(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiPropString(Sender: zglTGUIObject);
    class procedure guiPropChar(Sender: zglTGUIObject);
    class procedure guiPropSingle(Sender: zglTGUIObject);
    class procedure guiPropInteger(Sender: zglTGUIObject);
    class procedure guiPropTrackBar(Sender: zglTGUIObject);
    class procedure guiPropEnum(Sender: zglTGUIObject);
    class procedure guiPropSelector(Sender: zglTGUIObject);
    class procedure guiEvntProp(Sender: zglTGUIObject);
    class procedure guiEditValues(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiCreateForm(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiCreateState(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiCreateFont(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiNE(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiNEOk(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiNECancel(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiDeleteFormOk(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiDeleteEvent(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiDeleteFormCancel(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiSelectForm(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiSelectFont(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiDeleteForm(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiDeleteFont(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiDeleteState(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiNewObject(Sender: zglTGUIObject; X, Y: integer);

    class procedure beginLoadGui(Sender: zglTGUIObject;
      SelectedElement: zglTMenuItem; X, Y: integer);
    class procedure loadGui(Sender: zglTGUIObject;
      SelectedElement: zglTMenuItem; X, Y: integer);
    class procedure saveGui(Sender: zglTGUIObject;
      SelectedElement: zglTMenuItem; X, Y: integer);
    class procedure exitGui(Sender: zglTGUIObject;
      SelectedElement: zglTMenuItem; X, Y: integer);
    class procedure clearGui(Sender: zglTGUIObject;
      SelectedElement: zglTMenuItem; X, Y: integer);
    class procedure configGui(Sender: zglTGUIObject;
      SelectedElement: zglTMenuItem; X, Y: integer);
    class procedure SetWorkingDir(Sender: zglTGUIObject;
      SelectedElement: zglTMenuItem; X, Y: integer);
    class procedure LoadSkin(Sender: zglTGUIObject;
      SelectedElement: zglTMenuItem; X, Y: integer);

  end;

  TGuiLoader = class
    _Name, _ClassName: TCaption;
    Obj: zglTNamedObject;
  end;


  TEditorState = (esNormal, esAdd, esStart);
  
var
  fntMain: zglPFont;
  Editor: TEditor;
  MainGui, Gui: zglTGui;
  skinMain: zglTGuiSkin;
  frmForms, frmAsk, frmProperties, frmNewEvent: zglTForm;
  lstForms, lstStates, lstOtherComponents: zglTList;
  pcOptions, pcForms: zglTPageControl;
  pcsProperties, pcsEvents, pcsPropList,
    pcsForms, pcsStates, pcsFonts: zglTPageControlSheet;
  tblProperties, tblEvents, tblPtopList: zglTTable;
  ppmNewObject: zglTPopupMenu;
  editObject: zglTGUIObject;
  btnCreateForm: zglTMultiButton;
  btnDeleteForm, btnNewFont, btnDeleteFont,
    btnDeleteOk, btnDeleteCancel, btnNewObject,
    btnNewEvent, btnDeleteEvent,
    btnNEOk, btnNECancel, btnCreateState, btnDeleteState: zglTButton;
  cmbStateTo: zglTComboBox;
  edtEvName: zglTEdit;
  cmbEvType: zglTComboBox;
  selected, slctOwner: zglTGUIObject;
  txComponents, txSS: zglPTexture;
  resList: zglPResolutionList;
  fntLeft: zglTFontContainer;
  mmMenu: zglTMainMenu;
  miCurrent: zglTMenuItem;
  EditorState: TEditorState = esNormal;
  CallCreator: PCoponentCaller;
  StartX, StartY: integer;
  Counter: Single = 0;
  sttDef: TGUIState;
  
const
  FRMASC_OBJECT = 0;
  FRMASC_STATE = 1;
  FRMASC_FONT = 2;

implementation

procedure Fill( tex : zglPTexture; X, Y, Width, Height : Single;
  OffsetX: Single = 0; OffsetY: Single = 0; Angle: Single = 0 );
var coord : array[ 0..3 ] of zglTPoint2D;
begin
  coord[ 0 ].X := OffsetX;
  coord[ 0 ].Y := OffsetY;
  coord[ 1 ].X := -Width / tex^.Width + OffsetX;
  coord[ 1 ].Y := OffsetY;
  coord[ 2 ].X := -Width / tex^.Width + OffsetX;
  coord[ 2 ].Y := Height / tex^.Height + OffsetY;
  coord[ 3 ].X := OffsetX;
  coord[ 3 ].Y := Height / tex^.Height + OffsetY;
  tex_Filter( tex, tex^.Flags xor TEX_CLAMP or TEX_REPEAT );
  texture2d_Draw( tex, coord, X, Y, Width, Height, Angle );
  tex_Filter( tex, tex^.Flags xor TEX_REPEAT or TEX_CLAMP );
end;

procedure DrawSSRect(Tex: zglPTexture; X, Y, W, H, Offset: Integer);
begin
  Fill(Tex, X, Y, W, 1, Offset / LINESS_COUNT);
  Fill(Tex, X, Y + H - 1, W, 1, 1 - Offset / LINESS_COUNT);
  Fill(Tex, X - H / 2  + 0.5,
    Y + H / 2 - 0.5, H, 1, 1 - Offset / LINESS_COUNT, 0, 90);
  Fill(Tex, X - H / 2 + W - 0.5,
    Y + H / 2 - 0.5, H, 1, 1 - Offset / LINESS_COUNT, 0, 270);
end;

class procedure THandler.guiUpdateName(Sender: zglTGUIObject);
var li: zglTListItem;
    gs: TGUIState;
begin
  li := zglTListItem(Sender.Data);
  if Sender.Caption <> '' then
    li.Caption := '"' + Sender.Caption + '": ' + Sender.Name
  else
    li.Caption := Sender.Name;
  if Sender.State <> nil then begin
    gs := TGUIState(Sender.State);
    li.Caption := li.Caption + ' [' + gs.Name + ']';
  end;
end;

class procedure THandler.guiUpdateFontName(Sender: zglTGUIObject);
var li: zglTListItem;
    gs: TGUIState;
begin
  li := zglTListItem(Sender.Data);
  li.Caption := Sender.Name;
end;

class procedure THandler.guiUpdateEventName(Sender: zglTGUIObject);
var li: TEditorEvent;
    i: integer;
    AnsiCapt: AnsiString;
begin
  AnsiCapt := AnsiString(Sender.Caption);
  if Sender.Caption = '' then
    exit;
  for i := 1 to Length(AnsiCapt) do
    if (not (AnsiCapt[i] in ['a'..'z', 'A'..'Z', '_', '0'..'9'])) or
      ((AnsiCapt[i] in ['0'..'9']) and (i = 1)) then
        Exit;
  li := TEditorEvent(Sender.Data);
  li.EventName := TCaption(AnsiCapt);
end;

constructor TGUIState.Create(Ed: TEditor);
begin
  Id := Ed.fStates.Count;
  if Ed.fStates.Count = 0 then
    Name := 'DEF.'
  else
    Name := u_IntToStr(Ed.fStates.Count);
  ListItem := zglTListItem.Create(lstStates, Name,
    16);
  ListItem.IconId := TEX_STATE;
  ListItem.Data := Self;
  lstStates.AddItem(ListItem);
end;

destructor TGUIState.Destroy;
var i: integer;
    ist: TGUIState;
begin
  for i := 1 to Gui.StatesCount - 1 do begin
    ist := TGUIState(Gui.ItemsForState[i].Items[0].State);
    if ist = Self then begin
      Gui.ItemsForState[i].Items[0].State := sttDef;
    end;
  end;
  lstStates.RemoveItem(ListItem);
  inherited;
end;

constructor TEditor.Create;
begin
  fEvents := TList.Create;
  fEventHandlers := TList.Create;
  fStates := TList.Create;
  Clear;
end;

destructor TEditor.Destroy;
var i: integer;
begin

  Clear;

  for i := 0 to fEvents.Count - 1 do
    TEditorEvent(fEvents.Items[i]).Free;
  fEvents.Free;
  for i := 0 to fEventHandlers.Count - 1 do
    TEventHandler(fEventHandlers.Items[i]).Free;
  fEventHandlers.Free;
  for i := 0 to fStates.Count - 1 do
    TGUIState(fStates.Items[i]).Free;
  fStates.Free;

  skinMain.Free;

  Gui.Free;
  MainGui.Free;
  inherited;
end;

class procedure THandler.SetWorkingDir(Sender: zglTGUIObject;
  SelectedElement: zglTMenuItem; X, Y: integer);
begin
  frmInformation.MoveToCenter;
  frmInformation.ShowModal;
end;

class procedure THandler.LoadSkin
  (Sender: zglTGUIObject; SelectedElement: zglTMenuItem; X, Y: integer);
var od: TOpenDialog;
begin
  od := TOpenDialog.Create(Nil);
  if Editor.WorkingDir <> '' then
    od.InitialDir := Editor.WorkingDir;
  od.Filter := 'Skin file|*.skin';
  if od.Execute then begin
    Gui.Skin.Load(od.FileName);
    Gui.updateSkin;
  end;
  od.Free;
end;

class procedure THandler.ConfigGui(Sender: zglTGUIObject;
  SelectedElement: zglTMenuItem; X, Y: integer);
begin
  frmConfig.ShowModal;
end;

class procedure THandler.ExitGui(Sender: zglTGUIObject;
  SelectedElement: zglTMenuItem; X, Y: integer);
begin
  if Editor.Edited then begin
    Editor.ActionEvent := aeExit;
    frmConfirmClear.MoveToCenter;
    frmConfirmClear.ShowModal;
  end else zgl_Exit;
end;

 class procedure THandler.ClearGui(Sender: zglTGUIObject;
  SelectedElement: zglTMenuItem; X, Y: integer);
begin
  if Editor.Edited then begin
    Editor.ActionEvent := aeClear;
    frmConfirmClear.MoveToCenter;
    frmConfirmClear.ShowModal;
  end else Editor.Clear;
end;

class procedure THandler.guiSelect(Sender: zglTGUIObject; X, Y: integer);
begin
  Editor.Edited := true;
  selected := Sender;
  lstForms.Selected := zglTListItem(Selected.Data);

  Gui.ForceVisible := false;
  Editor.EditComponent(Sender);
  Gui.ForceVisible := true;
end;

function IsSelectable(Obj: zglTNamedObject; prop: PPropInfo): boolean;
var i: integer;
begin
  result := false;
  for i := 0 to SELECTABLE_COUNT - 1 do begin
    if (SELECTABLE[i] = Obj.ClassName + '.' + prop^.Name) or (SELECTABLE[i] = prop^.Name) then begin
      result := true;
      exit;
    end;
  end;
end;

procedure EditParameter(Obj: zglTNamedObject;
  Name, Prefix, Value: TCaption);
var
  props: PPropList;
  prop: PPropInfo;
  i, countOf: integer;
  found: boolean;
  o: TObject;
  uo: zglTUnknownObject;
  ActualParam, FormalParam: TCaption;

  function RegisterOpt(prp: PPropInfo): boolean;
  var bj: zglTEditableClass;
      j: integer;
  begin
    Result := false;
    if u_StrUp(prp^.Name) = u_StrUp(ActualParam) then begin
      Result := true;
      case prp^.PropType^.Kind of
        tkEnumeration {$IFDEF FPC}, tkBool{$ENDIF}: begin
          SetEnumProp(Obj, prp, Value);
        end;
        tkLString, tkWString, tkString, tkUString {$IFDEF FPC}, tkAString {$ENDIF}: begin
          SetStrProp(Obj, prp, Value);
        end;
        tkFloat: begin
          SetFloatProp(Obj, prp, u_StrToFloat(Value));
        end;
        tkChar: begin
          if Length(Value) > 0 then
            SetOrdProp(Obj, prp, Ord(Value[1]));
        end;
        tkInteger: begin
          SetOrdProp(Obj, prp, u_StrToInt(Value));
        end;
        tkMethod: begin
          Editor.RegisterEventHandle(
            Obj, Editor.GetEvent(Value), ActualParam);
        end;
        tkClass: begin
          o := GetObjectProp(Obj, prp);
          if o is TStringList then begin
            TStringList(o).Add(Value);
          end else
          if o.ClassType.InheritsFrom(zglTEditableClass) then begin
            bj := zglTEditableClass(GetObjectProp(Obj, prp));
            case bj.Editable of
              emTrue: begin
                if IsSelectable(Obj, prp) then begin
                  for j := 0 to lstOtherComponents.ItemsCount - 1 do
                    if zglTNamedObject(lstOtherComponents.Items[j].Data).Name = Value then begin
                      SetObjectProp(Obj, prp, lstOtherComponents.Items[j].Data);
                      break;
                    end;
                end else
                  EditParameter(zglTGUIObject(bj),
                    FormalParam, Prefix, Value);
              end;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  i := pos('.', string(Name));
  if i <> 0 then begin
    ActualParam := TCaption(Copy(Name, 1, i - 1));
    FormalParam := TCaption(Copy(Name, i + 1, Length(Name)));
  end else begin
    ActualParam := Name;
    FormalParam := '';
  end;

  props := AllocMem(SizeOf(props^));

  countOf := GetPropList(Obj.ClassInfo,
    [tkEnumeration, tkLString, tkWString, tkString, tkUString{$IFDEF FPC}, tkAString{$ENDIF},
     tkFloat, tkInteger, tkMethod, tkClass{$IFDEF FPC}, tkBool{$ENDIF}],
    props);

  i := 0;
  found := false;

  while Assigned(props^[i]) do begin
    prop := props^[i];
    try
      if Assigned(prop^.GetProc)  and
        (prop^.Name[1] <> '_') then begin
        found := found or RegisterOpt(prop);
      end;
    except

    end;
    Inc(i);
  end;

  if not found and (Obj is zglTUnknownObject) then begin
    uo := zglTUnknownObject(Obj);
    uo.UnknownProperties.Add(Name + '=' + Prefix + Value);
  end;

  props := nil;
end;

class procedure THandler.beginLoadGui(Sender: zglTGUIObject;
  SelectedElement: zglTMenuItem; X, Y: integer);
begin
  if Editor.Edited then begin
    Editor.ActionEvent := aeOpen;
    frmConfirmClear.MoveToCenter;
    frmConfirmClear.ShowModal;
  end else LoadGui(Sender, SelectedElement, X, Y);
end;

class procedure THandler.LoadGui(Sender: zglTGUIObject;
  SelectedElement: zglTMenuItem; X, Y: integer);
var od: TOpenDialog;
    F: TextFile;
    str: AnsiString;
    i: integer;
    ev: TEditorEvent;
    check: TRegExpr;
    loader: TList;
    ldr_f, ldr_l: TGuiLoader;

    function FindLoader(Name: TCaption): TGuiLoader;
    var j: integer;
    begin
      result := nil;
      for j := 0 to loader.Count - 1 do
        if TGuiLoader(loader.Items[j])._Name = Name then begin
          Result := TGuiLoader(loader.Items[j]);
          exit;
        end;
    end;
    function CreateObj(ClassNm: TCaption): zglTGUIObject;
    var
      j: integer;
      ccl: PCoponentCaller;
    begin
      result := nil;
      for j := 0 to ppmNewObject.ItemsCount - 1 do begin
        ccl := ppmNewObject.Items[j].Data;
        if ccl^.ObjClassName = ClassNm then begin
          CallCreator := ccl;
          result := TCreateCallback(ccl^.CrtCllbck)(0, 0, 100, 100);
          Exit;
        end;
      end;
      CallCreator := @cmpUnknownObject;
      Result := CreateUnknownObject(ClassNm);
    end;
    procedure AddLoader(Name, ClassName: TCaption; Root: boolean);
    var ldr: TGuiLoader;
    begin
      ldr := TGuiLoader.Create;
      ldr._Name := Name;
      ldr._ClassName := ClassName;
      if ClassName = 'zglTForm' then begin
        ldr.Obj := Editor.CreateForm;
        zglTGuiObject(ldr.Obj).OnClick := THandler.guiSelect;
      end else
      if ClassName = 'zglTFontContainer' then begin
        ldr.Obj := Editor.CreateFont;
      end else
      if ClassName = 'zglTFrame' then begin
        if Root then begin
          ldr.Obj := Editor.CreateFrame;
          zglTGuiObject(ldr.Obj).OnClick := THandler.guiSelect;
        end else begin
          ldr.Obj := CreateObj(ClassName);
          Editor.InitObject(ldr.Obj);
        end;
      end else begin
        ldr.Obj := CreateObj(ClassName);
        Editor.InitObject(ldr.Obj);
      end;
      ldr.Obj.Name := Name;
      loader.Add(ldr);
    end;
    procedure SetProperty(ldr: TGuiLoader; Name, Prefix, Value: TCaption);
    begin
      EditParameter(ldr.Obj, Name, Prefix, Value);
    end;
begin

  od := TOpenDialog.Create(nil);
  od.Filter := '*.pas files|*.pas';
  od.Options := [ofFileMustExist];
  if od.Execute then begin
    loader := TList.Create;
    // clear the GUI
    Editor.Clear;
    AssignFile(F, od.FileName);
    Reset(F);
    check := TRegExpr.Create;
    // flags
    check.ModifierS := true;
    while not Eof(F) do begin
      Readln(F, str);
      i := Pos('//', TCaption(str));
      if i = 0 then begin
        check.Expression := '{WORKINGDIR=([a-zA-Z0-9_:\\/ ]+)}';
        if check.Exec(TCaption(str)) then begin
          Editor.WorkingDir := check.Match[1];

          if DirectoryExists(Editor.WorkingDir) then begin
            ChDir(Editor.WorkingDir);
          end else begin
            MainGui.ShowMessage('Error', 'Working directory ' + Editor.WorkingDir + ' do not exists!', deZoomIn);
            Editor.WorkingDir := '';
          end;
          Readln(F, str);
          Continue;
        end;
        check.Expression := 'CREATED_GUI_VERSION = ''([a-zA-Z0-9_\.]+)'';';
        if check.Exec(TCaption(str)) then begin
          if GUI_VERSION <> TCaption(check.Match[1]) then begin
            MainGui.ShowMessage('Error', 'File gui version = ' + check.Match[1] + ', but need ' + GUI_VERSION + '!', deZoomIn);
            CloseFile(f);
            Editor.Clear;
            Exit;
          end;
          Readln(F, str);
          Continue;
        end;

        check.Expression := '{EVENT\s([a-zA-Z0-9_]+):([a-zA-Z0-9_]+)}';
        if check.Exec(TCaption(str)) then begin
          ev := Editor.RegisterEvent(check.Match[1], check.Match[2]);
          ev.Text.Clear;
          Readln(F, str);
          while Pos('{END}', TCaption(str)) = 0 do begin
            ev.Text.Add(TCaption(str));
            Readln(F, str);
          end;
          Readln(F, str);
          Continue;
        end;
        check.Expression := '{INTERFACE}';
        if check.Exec(TCaption(str)) then begin
          editor._Interface := '';
          Readln(F, str);
          while Pos('{END}', TCaption(str)) = 0 do begin
            if editor._Interface <> '' then
              editor._Interface := editor._Interface + #13#10 + str
            else
              editor._Interface := str;
            Readln(F, str);
          end;
          Readln(F, str);
          Continue;
        end;
        check.Expression := '{IMPLEMENTATION}';
        if check.Exec(TCaption(str)) then begin
          editor._Implementation := '';
          Readln(F, str);
          while Pos('{END}', TCaption(str)) = 0 do begin
            if editor._Implementation <> '' then
              editor._Implementation := editor._Implementation + #13#10 + str
            else
              editor._Implementation := str;
            Readln(F, str);
          end;
          Readln(F, str);
          Continue;
        end;
        i := Pos('{INIT}', TCaption(str));
        if i <> 0 then begin
          Readln(F, str);
          while Pos('{END}', TCaption(str)) = 0 do begin
            // AddState
            check.Expression := 'Gui.AddState;';
            if check.Exec(TCaption(str)) then begin
              Editor.RegisterState;
              Readln(F, str);
              Continue;
            end;
            // Object
            check.Expression := '([a-zA-Z0-9_]+) *:= *([a-zA-Z0-9_]+)\.CreateDefaults\(Gui\); *({ROOT})?';
            if check.Exec(TCaption(str)) then begin
              AddLoader(check.Match[1], check.Match[2], check.Match[3] = '{ROOT}');
              Readln(F, str);
              Continue;
            end;
            // Object parameters
            check.Expression := 'with +([a-zA-Z0-9_]+) +do +begin';
            if check.Exec(TCaption(str)) then begin
              ldr_f := FindLoader(check.Match[1]);
              if ldr_f <> nil then begin
                Readln(F, str);
                while Pos('end;', TCaption(str)) = 0 do begin
                  check.Expression := '([a-zA-Z0-9_\.]+) *:= *(zglTGuiEventHandler\.)?''?(.*?)''?;';
                  if check.Exec(TCaption(str)) then begin
                    SetProperty(ldr_f, check.Match[1], check.Match[2], check.Match[3]);
                    Readln(F, str);
                    Continue;
                  end;
                  check.Expression := '([a-zA-Z0-9_\.]+) *\. *Add\(''(.*?)''\)';
                  if check.Exec(TCaption(str)) then begin
                    SetProperty(ldr_f, check.Match[1], '', check.Match[2]);
                    Readln(F, str);
                    Continue;
                  end;
                  Readln(F, str);
                end;
              end;
              Readln(F, str);
              Continue;
            end;
            // Object registration
            check.Expression := 'Gui\.Items\.Add\(([a-zA-Z0-9_]+)\)';
            if check.Exec(TCaption(str)) then begin
              ldr_f := FindLoader(check.Match[1]);
              if ldr_f <> nil then begin
                Gui.ForceVisible := false;
                Editor.EditComponent(ldr_f.Obj, 0);
                Gui.ForceVisible := true;
              end;
              Readln(F, str);
              Continue;
            end;
            // Object registration
            check.Expression := 'Gui\.ItemsForState\[([0-9]+)\]\.Add\(([a-zA-Z0-9_]+)\)';
            if check.Exec(TCaption(str)) then begin
              ldr_f := FindLoader(check.Match[2]);
              if ldr_f <> nil then begin
                if ldr_f.Obj.InheritsFrom(zglTGuiObject) then
                  if zglTGuiObject(ldr_f.Obj).Parent = nil then begin
                    zglTGuiObject(ldr_f.Obj).State := Editor.GetState(u_StrToInt(check.Match[1]));
                    THandler.guiUpdateName(zglTGuiObject(ldr_f.Obj));
                  end;
                Gui.ForceVisible := false;
                Editor.EditComponent(ldr_f.Obj, 0);
                Gui.ForceVisible := true;
              end;
              Readln(F, str);
              Continue;
            end;
            // Object registration
            check.Expression := '([a-zA-Z0-9_]+)\.Items\.Add\(([a-zA-Z0-9_]+)\)';
            if check.Exec(TCaption(str)) then begin
              ldr_f := FindLoader(check.Match[1]);
              if ldr_f <> nil then begin
                ldr_l := FindLoader(check.Match[2]);
                if ldr_l <> nil then begin
                  if ldr_l.Obj.InheritsFrom(zglTGuiObject) then begin
                    Editor.AddObject(zglTFrame(ldr_f.Obj), zglTGuiObject(ldr_l.Obj), false);
                    Gui.ForceVisible := false;
                    Editor.EditComponent(ldr_l.Obj, 0);
                    Gui.ForceVisible := true;
                  end;
                end;
              end;
              Readln(F, str);
              Continue;
            end;
            Readln(F, str);
          end;
        end;
      end;
    end;
    check.Free;
    cmbStateTo.Items.Clear;
    for i := 0 to Editor.fStates.Count - 1 do
      if i = 0 then
        cmbStateTo.Items.Add('DEF.')
      else
        cmbStateTo.Items.Add(u_IntToStr(i));
    CloseFile(F);

    for i := 0 to loader.Count - 1 do
      TGuiLoader(loader[i]).Free;
    loader.Free;

    Editor.Edited := false;
  end;
  od.Free;
end;

procedure HashVars(f: PTextFile; Obj: zglTGUIObject);
var i: integer;
begin
  if Obj is zglTUnknownObject then begin
    Writeln(F^, '  ' + Obj.Name + ': ' + zglTUnknownObject(Obj).UnknClassName + ';');
  end else
    Writeln(F^, '  ' + Obj.Name + ': ' + Obj.ClassName + ';');
  if Obj.Container then begin
    for i := 0 to zglTFrame(Obj).Items.Count - 1 do
      HashVars(f, zglTFrame(Obj).Items.Items[i]);
  end;
end;


procedure HashParameters(f: PTextFile; Root, Obj: zglTNamedObject;
  PreData: TCaption; NamePrefix: TCaption = '');
var
  i, countOf: integer;
  props: PPropList;
  prop: PPropInfo;
  o: TObject;

  procedure RegisterOpt(prp: PPropInfo);
  var bj: zglTEditableClass;
      ev: TEventHandler;
      i, i_: integer;
      sl: TStringList;
      vl: AnsiString;

    function NotDef(Name, Value: TCaption): Boolean;
    begin
      Result := Root.DefaultProperties.Values[NamePrefix + Name] <> Value;
    end;
  begin
    case prp^.PropType^.Kind of
      tkEnumeration{$IFDEF FPC}, tkBool{$ENDIF}: begin
        if NotDef(prp^.Name,
            GetEnumName(prp^.PropType{$IFNDEF FPC}^{$ENDIF}, GetOrdProp(Obj, prp)))
        then begin
          Write(f^, PreData + NamePrefix + prp^.Name + ' := ');
          write(f^, GetEnumName(prp^.PropType{$IFNDEF FPC}^{$ENDIF}, GetOrdProp(Obj, prp)));
          Writeln(f^, ';')
        end;
      end;
      tkLString, tkWString, tkString, tkUString{$IFDEF FPC}, tkAString{$ENDIF}: begin
        if NotDef(prp^.Name, GetStrProp(Obj, prp)) then
          Writeln(f^, PreData + NamePrefix + prp^.Name + ' := ''' +
            StringReplace(GetStrProp(Obj, prp),
              '''', '''''', [rfReplaceAll]) + '''; ');
      end;
      tkFloat: begin
        if NotDef(prp^.Name, u_FloatToStr(GetFloatProp(Obj, prp))) then
        Writeln(f^, PreData + NamePrefix + prp^.Name + ' := ' +
          u_FloatToStr(GetFloatProp(Obj, prp)) + '; ');
      end;
      tkChar: begin
        if NotDef(prp^.Name, Char(GetOrdProp(Obj, prp))) then
        Writeln(f^, PreData + NamePrefix + prp^.Name + ' := ''' +
          Char(GetOrdProp(Obj, prp)) + '''; ');
      end;
      tkInteger: begin
        if NotDef(prp^.Name, u_IntToStr(GetOrdProp(Obj, prp))) then
        Writeln(f^, PreData + NamePrefix + prp^.Name + ' := ' +
          u_IntToStr(GetOrdProp(Obj, prp)) + '; ');
      end;
      tkMethod: begin
        ev := Editor.GetObjectEvent(Obj, prp^.Name);
        if ev <> nil then begin
          Writeln(f^, PreData + NamePrefix + prp^.Name + ' := zglTGuiEventHandler.' +
            ev.Event.EventName + '; ');
        end;
      end;
      tkClass: begin
        o := GetObjectProp(Obj, prp);
        if o is TStringList then begin
          if Obj is zglTUnknownObject then begin
            if prp^.Name = 'UnknownProperties' then exit;
          end;
          sl := TStringList(o);
          for i := 0 to sl.Count - 1 do
            Writeln(f^, PreData + NamePrefix + prp^.Name + '.Add(''' +
              TCaption(sl.Strings[i]) + '''); ');
        end else
        if Assigned(o) then
          if o.ClassType.InheritsFrom(zglTEditableClass) then begin
            bj := zglTEditableClass(GetObjectProp(Obj, prp));
            case bj.Editable of
              emTrue: begin
                if IsSelectable(Obj, prp) then begin
                  vl := zglTNamedObject(o).Name;
                  if vl <> '' then
                    if NotDef(prp^.Name, vl) then
                      Writeln(f^, PreData + NamePrefix + prp^.Name + ' := ' + vl + '; ');
                end else
                  HashParameters(f, Root, zglTNamedObject(GetObjectProp(Obj, prp)),
                    PreData, NamePrefix + prp^.Name + '.');
              end;
            end;
          end;
      end
    end;
  end;

begin
  props := AllocMem(SizeOf(props^));

  countOf := GetPropList(Obj.ClassInfo,
    [tkEnumeration, tkLString, tkWString, tkString, tkUString
     {$IFDEF FPC}, tkAString{$ENDIF}, tkChar,
     tkFloat, tkInteger, tkMethod, tkClass{$IFDEF FPC}, tkBool{$ENDIF}],
    props);


  i := 0;
  while Assigned(props^[i]) do begin
    prop := props^[i];
    try
      if (prop^.Name[1] <> '_') then begin
        RegisterOpt(prop);
      end;
    except

    end;
    Inc(i);
  end;

  props := nil;
end;

procedure HashObjects(f: PTextFile; Obj: zglTNamedObject; Level: Byte = 1);
var i: integer;
    s, st: ansistring;
    li: TGUIState;
    lsi: zglTListItem;
    sl: TStringList;
begin
  s := '';
  for i := 0 to Level - 1 do
    s := s + '  ';
  if Obj.InheritsFrom(zglTGUIObject)  then begin
    if zglTGUIObject(Obj).Parent = nil then begin
      if Obj is zglTUnknownObject then begin
        Writeln(F^, TCaption(s) + Obj.Name + ' := ' + zglTUnknownObject(Obj).UnknClassName + '.CreateDefaults(Gui); {ROOT}');
      end else begin
        Writeln(F^, TCaption(s) + Obj.Name + ' := ' + Obj.ClassName + '.CreateDefaults(Gui); {ROOT}');
      end;
    end else begin
      if Obj is zglTUnknownObject then begin
        Writeln(F^, TCaption(s) + Obj.Name + ' := ' + zglTUnknownObject(Obj).UnknClassName + '.CreateDefaults(Gui);');
      end else begin
        Writeln(F^, TCaption(s) + Obj.Name + ' := ' + Obj.ClassName + '.CreateDefaults(Gui);');
      end;
    end;
  end else begin
    Writeln(F^, TCaption(s) + Obj.Name + ' := ' + Obj.ClassName + '.CreateDefaults(Gui);');
  end;
  // options
  lsi := zglTListItem(obj.Data);
  Writeln(F^, TCaption(s) + 'with ' + Obj.Name + ' do begin');
  HashParameters(f, Obj, Obj, TCaption(s) + '  ');
  if Obj is zglTUnknownObject then begin
    sl := zglTUnknownObject(Obj).UnknownProperties;
    for i := 0 to sl.Count - 1 do begin
      Writeln(F^, TCaption(s) + '  ' + sl.Names[i] + ' := ' + sl.ValueFromIndex[i] + ';');
    end;
  end;
  Writeln(F^, TCaption(s) + 'end;');


  if Obj.InheritsFrom(zglTGUIObject)  then begin
    // parent
    if zglTGUIObject(Obj).Parent = nil then begin
      li := TGUIState(zglTGUIObject(Obj).State);
      if li.Id = 0 then begin
        Write(F^, s + 'Gui.Items');
      end else begin
        st := AnsiString(u_IntToStr(li.Id));
        Write(F^, s + 'Gui.ItemsForState[' + st + ']');
      end;
    end else
      Write(F^, TCaption(s) + zglTGUIObject(Obj).Parent.Name + '.Items');
    Writeln(F^, '.Add(' + Obj.Name + ');');
    // child
    if zglTGUIObject(Obj).Container then begin
      for i := 0 to zglTFrame(Obj).Items.Count - 1 do
        HashObjects(f, zglTFrame(Obj).Items.Items[i], Level + 1);
    end;

  end;


end;

function GetTypeInfoFromName(aTypeName : TCaption) : TCaption;
var i: integer;
begin
  Result := '';
  for i := 0 to EVENTS_COUNT - 1 do
    if EVENTS[i].name = aTypeName then begin
      Result := EVents[i].value;
      break;
    end;
end;

class procedure THandler.SaveGui(Sender: zglTGUIObject;
  SelectedElement: zglTMenuItem; X, Y: integer);
var sd: TSaveDialog;
    f, bcp: TextFile;
    s, nm: TCaption;
    i, j: integer;
    ev: TEditorEvent;
    str, fName: string;
begin
  sd := TSaveDialog.Create(nil);
  sd.Options := [ofOverwritePrompt];
  sd.Filter := 'Pas files|*.pas';

  if SelectedElement = miCurrent then
    fName := Editor.Current
  else begin
    if not sd.Execute then begin
      sd.Free;
      exit;
    end;
    s := sd.FileName;
    if ExtractFileExt(s) = '' then
      s := s + '.pas';
    fName := s;
    Editor.Current := fName;
    miCurrent.Enabled := true;
    miCurrent.Text := 'Save ' + ExtractFileName(fName);
  end;

  if file_Exists(fName) then begin
    AssignFile(f, fName);
    AssignFile(bcp, fName + '.bak');
    Rewrite(bcp);
    Reset(f);

    while not Eof(f) do begin
      Readln(f, str);
      Writeln(bcp, str);
    end;

    CloseFile(f);
    CloseFile(bcp);
  end;

  Editor.Edited := false;

  AssignFile(f, fName);
  Rewrite(f);
  nm := ExtractFileName(fName);
  Writeln(f,
    'unit ' + Copy(nm, 1, pos('.', nm) - 1) + ';'#13#10#13#10 +
    '// PLEASE DO NOT REMOVE THE {*} COMMENTS, YOU CAN LOOSE YOU SOURCE!' +
    #13#10#13#10'interface'#13#10#13#10'// Store here uses or proc definition'#13#10 +
    '{INTERFACE}'#13#10 +
      editor._Interface +
    #13#10'{END}'#13#10#13#10 +
    'var');
  for i := 1 to Gui.StatesCount - 1 do begin
    HashVars(@f, Gui.ItemsForState[i].Items[0]);
  end;
  for i := 0 to lstOtherComponents.ItemsCount - 1 do begin
    Writeln(f, '  ' + zglTNamedObject(lstOtherComponents.Items[i].Data).Name + ': ' +
                      zglTNamedObject(lstOtherComponents.Items[i].Data).ClassName + ';');
  end;
  Writeln(f, #13#10'procedure InitGui(gui: zglTGui);'#13#10);

  Writeln(f, 'const');
  Writeln(f, '  CREATED_GUI_VERSION = ''' + GUI_VERSION + ''';'#13#10);

  Writeln(f, 'type');
  Writeln(f, '  zglTGuiEventHandler = class');

  for i := 0 to Editor.fEvents.Count - 1 do begin
    ev := TEditorEvent(Editor.fEvents.Items[i]);
    Writeln(f, '    class procedure ' + ev.EventName + '(' +
      GetTypeInfoFromName(ev.EventType) + ');');
  end;
  Writeln(f, '  end;'#13#10);

  if Editor.WorkingDir <> '' then
    Writeln(f, '{WORKINGDIR=' + Editor.WorkingDir + '}'#13#10);

  Writeln(f, 'implementation'#13#10#13#10'// Store here recursive uses or proc implementation' +
  #13#10'{IMPLEMENTATION}'#13#10 +
    editor._Implementation + #13#10'{END}'#13#10);

  for i := 0 to Editor.fEvents.Count - 1 do begin
    ev := TEditorEvent(Editor.fEvents.Items[i]);
    Writeln(f, 'class procedure zglTGuiEventHandler.' + ev.EventName + '(' +
      GetTypeInfoFromName(ev.EventType) + ');');
    Writeln(f, '{EVENT ' + ev.EventName + ':' + ev.EventType + '}');
    for j := 0 to ev.Text.Count - 1 do
      Writeln(f, ev.Text.Strings[j]);
    Writeln(f, '{END}'#13#10);
  end;

  Writeln(f, 'procedure InitGui(gui: zglTGui);'#13#10 +
    'begin'#13#10'  {INIT}');

  Writeln(f, '  {$IF CREATED_GUI_VERSION <> GUI_VERSION}');
  Writeln(f, '    {$MESSAGE Fatal ''GUI version diff with generated code.''}');
  Writeln(f, '  {$IFEND}');

  for i := 1 to Editor.fStates.Count - 1 do
    Writeln(f, '  Gui.AddState;');

  for i := 0 to lstOtherComponents.ItemsCount - 1 do begin
    HashObjects(@f, zglTNamedObject(lstOtherComponents.Items[i].Data));
  end;

  for i := 1 to Gui.StatesCount - 1 do begin
    HashObjects(@f, Gui.ItemsForState[i].Items[0]);
  end;

  Writeln(f, '  {END}'#13#10'end;'#13#10#13#10'end.');
  CloseFile(f);

  sd.Free;
end;

class procedure THandler.guiUpdateState(Sender: zglTGUIObject);
var frm: zglTGUIObject;
begin
  if lstForms.Selected <> nil then begin
    frm := zglTGUIObject(lstForms.Selected.Data);
    if frm <> nil then begin
      if frm.Parent = nil then begin
        frm.State := Editor.GetState(cmbStateTo.Selected);
        guiUpdateName(frm);
      end;
    end;
  end;
end;

class procedure THandler.onStartCreate(Sender: zglTGUIObject;
  SelectedElement: zglTMenuItem; X, Y: integer);
begin
  CallCreator := SelectedElement.Data;
  EditorState := esAdd;
end;

class procedure THandler.createFrame(Sender: zglTGUIObject;
  SelectedElement: zglTMenuItem; X, Y: integer);
var frm: zglTFrame;
begin
  frm := Editor.CreateFrame;
  frm.OnClick := THandler.guiSelect;
end;

constructor TEditorEvent.Create;
begin
  Text := TStringList.Create;
end;

destructor TEditorEvent.Destroy;
begin
  Text.Free;
  inherited;
end;

constructor TEventHandler.Create;
begin
  //
end;

destructor TEventHandler.Destroy;
begin
  //
  inherited;
end;

function TEditor.RegisterState: TGUIState;
begin
  Result := TGUIState.Create(Self);
  fStates.Add(Result);
end;

procedure TEditor.RegisterComponent(Name: TCaption; CrtCllbck: PCoponentCaller);
var mi: zglTMenuItem;
begin
  mi := zglTMenuItem.Create(Name, THandler.onStartCreate);
  mi.Data := CrtCllbck;
  mi.IconId := CrtCllbck^.CreateImage;
  ppmNewObject.AddItem(mi);
end;   

function TEditor.RegisterEvent(pName, pType: TCaption): TEditorEvent;
var edt: zglTEdit;
begin
  Result := TEditorEvent.Create;
  Result.EventName := pName;
  Result.EventType := pType;
  Result.Text.Text := 'begin'#13#10#13#10'end;';
  Result.Item := zglTTableItem.Create(tblPtopList);

  edt := zglTEdit.Create(MainGui, 0, 0, 10, 10, pName);
  edt.Data := Result;
  edt.OnChange := THandler.guiUpdateEventName;

  Result.Item.AddComponent(edt);
  Result.Item.AddColumn(pType);
  Result.Item.Data := Result;

  tblPtopList.AddItem(Result.Item);

  fEvents.Add(Result);
end;

function TEditor.GetState(Id: integer): TGUIState;
begin
  Result := TGUIState(fStates.Items[Id]);
end;

function TEditor.GetEventId(pName: TCaption): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to EVENTS_COUNT - 1 do begin
    if (EVENTS[i].name = pName) then begin
      Result := i;
      exit;
    end;
  end;
end;

function TEditor.GetEvent(pName: TCaption): TEditorEvent;
var i: integer;
    ev: TEditorEvent;
begin
  Result := nil;
  for i := 0 to fEvents.Count - 1 do begin
    ev := TEditorEvent(fEvents.Items[i]);
    if (ev.EventName = pName) then begin
      Result := ev;
      exit;
    end;
  end;
end;

function TEditor.GetObjectEvent(Obj: zglTNamedObject;
  pName: TCaption): TEventHandler;
var i: integer;
    ev: TEventHandler;
begin
  Result := nil;
  for i := 0 to fEventHandlers.Count - 1 do begin
    ev := TEventHandler(fEventHandlers.Items[i]);
    if (ev.EventObject = Obj) and (ev.Name = pName) then begin
      Result := ev;
      exit;
    end;
  end;
end;

procedure DeleteChild(Obj: zglTGUIObject);
var li: zglTListItem;
    i: integer;
begin
  li := zglTListItem(Obj.Data);
  Obj.DefaultProperties.Free;
  lstForms.RemoveItem(li);

  if Obj.Container then begin
    for i := 0 to zglTFrame(Obj).Items.Count - 1 do
      DeleteChild(zglTFrame(Obj).Items.Items[i]);
  end;
end;

procedure TEditor.Clear;
var i: integer;
begin
  Edited := False;
  Current := '';
  for i := 0 to fEvents.Count - 1 do
    TEditorEvent(fEvents.Items[i]).Free;
  fEvents.Clear;
  for i := 0 to fEventHandlers.Count - 1 do
    TEventHandler(fEventHandlers.Items[i]).Free;
  fEventHandlers.Clear;
  for i := 0 to fStates.Count - 1 do
    TGUIState(fStates.Items[i]).Free;
  fStates.Clear;
  sttDef := RegisterState;

  while Gui.StatesCount > 1 do begin
    DeleteChild(Gui.ItemsForState[Gui.StatesCount - 1].Items[0]);
    Gui.DeleteState(Gui.StatesCount - 1);
  end;

  WorkingDir := '';

  Gui.State := 0;
  selected := nil;
  slctOwner := nil;
  lstForms.Clear;
  tblPtopList.Clear;
  tblEvents.Clear;
  tblProperties.Clear;

  _Interface := 'uses zglGui;';
  _Implementation := '';
end;

procedure TEditor.DeleteEventHandle(Ev: TEventHandler);
begin
  fEventHandlers.Remove(Ev);
  Ev.Free;
end;

procedure TEditor.DeleteEvent(Ev: TEditorEvent);
var i: Integer;
  evh: TEventHandler;
begin
  for i := fEventHandlers.Count - 1 downto 0 do begin
    evh := TEventHandler(fEventHandlers.Items[i]);
    if evh.Event = Ev then begin
      DeleteEventHandle(evh);
    end;
  end;
  fEvents.Remove(Ev);
  Ev.Free;
end;

procedure TEditor.DeleteState(st: TGUIState);
var i: Integer;
begin
  fStates.Remove(st);
  st.Free;
  for i := 1 to fStates.Count - 1 do begin
    with lstStates.Items[i] do begin
      Caption := u_IntToStr(i);
    end;
    GetState(i).Name := u_IntToStr(i);
    GetState(i).Id := i;
  end;
  cmbStateTo.Items.Clear;
  for i := 0 to fStates.Count - 1 do
    cmbStateTo.Items.Add(GetState(i).Name);

  for i := 1 to Gui.StatesCount - 1 do begin
    THandler.guiUpdateName(Gui.ItemsForState[i].Items[0]);
  end;
end;

function TEditor.RegisterEventHandle(Obj: zglTNamedObject;
  Event: TEditorEvent; pName: TCaption): TEventHandler;
begin
  Result := TEventHandler.Create;
  Result.Name := pName;
  Result.EventObject := Obj;
  Result.Event := Event;
  fEventHandlers.Add(Result);
end;

procedure TEditor.AddProperty(Obj: zglTNamedObject; Name, Value: TCaption);
begin
  Obj.DefaultProperties.Add(Name + '=' + Value);
end;

procedure TEditor.InitObjParams(Root, Obj: zglTNamedObject; Prefix: TCaption = '');
var
  i, countOf: integer;
  props: PPropList;
  prop: PPropInfo;

  procedure CaclOpt(prp: PPropInfo);
  var bj: zglTEditableClass;
      o: TObject;
  begin
    case prp^.PropType^.Kind of
      tkEnumeration{$IFDEF FPC}, tkBool{$ENDIF}: begin
        AddProperty(Root,
          Prefix + prp^.Name,
          GetEnumName(prp^.PropType{$IFNDEF FPC}^{$ENDIF}, GetOrdProp(Obj, prp)));
      end;
      tkLString, tkWString, tkString, tkUString{$IFDEF FPC}, tkAString{$ENDIF}: begin
        AddProperty(Root,
          Prefix + prp^.Name,
          GetStrProp(Obj, prp));
      end;
      tkFloat: begin
        AddProperty(Root,
          Prefix + prp^.Name,
          u_FloatToStr(GetFloatProp(Obj, prp)));
      end;
      tkInteger: begin
        AddProperty(Root,
          Prefix + prp^.Name,
          u_IntToStr(GetOrdProp(Obj, prp)));
      end;
      tkChar: begin
        AddProperty(Root,
          Prefix + prp^.Name,
          Char(GetOrdProp(Obj, prp)));
      end;
      tkClass: begin
        if prp^.Name <> 'Rect' then begin
          o := GetObjectProp(Obj, prp);
          if Assigned(o) then
          if o.ClassType.InheritsFrom(zglTEditableClass) then begin
            bj := zglTEditableClass(o);
            case bj.Editable of
              emTrue: begin
                if not IsSelectable(Obj, prp) then
                  InitObjParams(Root, zglTGUIObject(o),
                    Prefix + prp^.Name + '.');
              end;
            end;
          end;
        end;
      end
    end;
  end;

begin

  props := AllocMem(SizeOf(props^));

  countOf := GetPropList(Obj.ClassInfo,
    [tkEnumeration, tkLString, tkWString, tkString, tkUString{$IFDEF FPC}, tkAString{$ENDIF},
     tkFloat, tkInteger, tkChar, tkMethod, tkClass{$IFDEF FPC}, tkBool{$ENDIF}],
    props);

  i := 0;
  while Assigned(props^[i]) do begin
    prop := props^[i];
    try
      if Assigned(prop^.GetProc)  and
        (prop^.Name[1] <> '_') then begin
        CaclOpt(prop);
      end;
    except

    end;
    inc(i);
  end;

  props := nil;
end;

procedure TEditor.RegisterObject(Obj: zglTNamedObject);
begin
  Obj.DefaultProperties := TStringList.Create;
end;

function TEditor.BrowseRelativePath: boolean;
var Dir: String;
begin
  if SelectDirectory('Select a directory', '', Dir) then begin
    Editor.WorkingDir := Dir;
    ChDir(Dir);
    Result := true;
    exit;
  end;
  Result := false;
end;

procedure TEditor.InitObject(Obj: zglTNamedObject);
begin
  RegisterObject(Obj);
  InitObjParams(Obj, Obj);
end;

procedure TEditor.AddObject(Owner: zglTFrame; Obj: zglTGuiObject; InitObj: boolean = true);
var tli: zglTTreeListItem;
begin
  Editor.Edited := true;

  if Obj.Caption <> '' then
    tli := zglTTreeListItem.Create(lstForms, zglTListItem(Owner.Data),
    '"' + Obj.Caption + '": ' + Obj.Name, 16)
  else
    tli := zglTTreeListItem.Create(lstForms, zglTListItem(Owner.Data),
    Obj.Name, 16);

  tli.Data := Obj;
  Obj.Data := tli;
  
  lstForms.InsertAfter(zglTListItem(Owner.Data), tli);
  tli.IconId := CallCreator^.CreateImage;

  Obj.OnNameChange := THandler.guiUpdateName;
  Obj.OnCaptionChange := THandler.guiUpdateName;
  Obj.OnClick := THandler.guiSelect;
  Obj._EditMode := true;

  if InitObj then begin
    InitObject(Obj);
  end;

  Owner.Items.Add(Obj);
  THandler.guiSelect(Obj, 0, 0);
end;

function TEditor.CreateFont: zglTFontContainer;
var fnt: zglTFontContainer;
    li: zglTListItem;
begin
  fnt := zglTFontContainer.CreateDefaults(MainGui);
  fnt.Name := 'Font' + u_IntToStr(Random(1000) + 1000);
  fnt.UseCounter;

  Editor.InitObject(fnt);

  li := zglTListItem.Create(lstOtherComponents, fnt.Name, 16);
  li.Data := fnt;
  fnt.Data := li;
  fnt.OnNameChange := THandler.guiUpdateFontName;
  lstOtherComponents.AddItem(li);
  lstOtherComponents.Selected := li;
  Result := fnt;
end;

function TEditor.CreateForm: zglTForm;
var li: zglTListItem;
    st: Word;
begin
  Editor.Edited := true;

  st := Gui.AddState;
  Result := zglTForm.Create(Gui, 0, 0, DEFFORMSIZE, DEFFORMSIZE,
    'Noname' + u_IntToStr(st), true);

  InitObject(Result);

  Result._EditMode := true;
  Result.OnNameChange := THandler.guiUpdateName;
  Result.OnCaptionChange := THandler.guiUpdateName;
  Result.OnClick := THandler.guiSelect;
  Result.ForceVisible := true;
  Gui.ItemsForState[st].Add(Result);
  if Result.Caption <> '' then
    li := zglTListItem.Create(lstForms,
      '"' + Result.Caption + '": ' + Result.Name +
      ' [' + sttDef.Name + ']', 16)
  else
    li := zglTListItem.Create(lstForms,
      Result.Name, 16);
  li.Data := Result;
  li.Tag := st;
  Result.State := sttDef;
  Result.Data := li;
  lstForms.AddItem(li);
  lstForms.Selected := li;
  li.IconId := TEX_FORM;

  btnDeleteForm.Enabled := true;
  btnNewObject.Enabled := true;
end;


function TEditor.CreateFrame: zglTFrame;
var li: zglTListItem;
    st: Word;
begin
  Editor.Edited := true;

  st := Gui.AddState;
  Result := zglTFrame.Create(Gui, 0, 0, DEFFORMSIZE, DEFFORMSIZE, true);

  InitObject(Result);

  Result._EditMode := true;
  Result.OnNameChange := THandler.guiUpdateName;
  Result.OnCaptionChange := THandler.guiUpdateName;
  Result.OnClick := THandler.guiSelect;
  Result.ForceVisible := true;
  Gui.ItemsForState[st].Add(Result);
  if Result.Caption <> '' then
    li := zglTListItem.Create(lstForms,
      '"' + Result.Caption + '": ' + Result.Name +
      ' [' + sttDef.Name + ']', 16)
  else
    li := zglTListItem.Create(lstForms,
      Result.Name, 16);
  li.Data := Result;
  li.Tag := st;
  Result.State := sttDef;
  Result.Data := li;
  lstForms.AddItem(li);
  lstForms.Selected := li;
  li.IconId := TEX_FRAME;

  btnDeleteForm.Enabled := true;
  btnNewObject.Enabled := true;
end;

class procedure THandler.guiPropBoolean(Sender: zglTGUIObject; X, Y: integer);
var prop: PPropInfo;
    Obj: TObject;
begin
  Editor.Edited := true;
  Obj := TObject(Sender.Data);
  prop := PPropInfo(Sender.SysData);
  if zglTCheckBox(Sender).Checked then begin
     SetEnumProp(Obj, prop, 'true');
  end else begin
    SetEnumProp(Obj, prop, 'false');
  end;
end;

class procedure THandler.guiBrowseFileName(Sender: zglTGUIObject; X, Y: integer);
var prop: PPropInfo;
    Obj: TObject;
    od: TOpenDialog;
    dt: string;
    i: Integer;
begin
  if Editor.WorkingDir = '' then begin
    frmInformation.MoveToCenter;
    frmInformation.ShowModal;
    exit;
  end;
  Obj := TObject(Sender.Data);
  prop := PPropInfo(Sender.SysData);
  od := TOpenDialog.Create(nil);    
  od.InitialDir := Editor.WorkingDir;
  if od.Execute then begin
    if Pos(Editor.WorkingDir, od.FileName) = 1 then begin
      dt := Copy(od.FileName, length(Editor.WorkingDir) + 2, Length(od.FileName));
    end else begin
      dt := od.FileName;
      MainGui.ShowMessage('Warning', 'Selected file do not contains in Relative folder, editor will set the absolute path.');
    end;

    for i := 1 to Length( dt ) do
      if dt[ i ] = '\' Then
        dt[ i ] := '/';

    SetStrProp(Obj, prop, dt);
    Sender.Caption := dt;
  end;
end;

class procedure THandler.guiBrowseColor(Sender: zglTGUIObject; X, Y: integer);
var prop: PPropInfo;
    Obj: TObject;
    od: TColorDialog;
    clr: TColor;
begin
  Obj := TObject(Sender.Data);
  prop := PPropInfo(Sender.SysData);
  od := TColorDialog.Create(nil);
  clr := GetOrdProp(Obj, prop);
  // switch BGR to RGB cuz ZenGL uses first one
  clr := ((clr and $FF) shl 16) or (clr and $FF00) or ((clr and $FF0000) shr 16);
  od.Color := clr;
  if od.Execute then begin
    clr := od.Color;
    // switch RGB to BGR cuz ZenGL uses last one
    clr := ((clr and $FF) shl 16) or (clr and $FF00) or ((clr and $FF0000) shr 16);
    SetOrdProp(Obj, prop, clr);
    Sender.Caption := '#' + IntToHex(clr, 6);
  end;
end;

class procedure THandler.guiPropString(Sender: zglTGUIObject);
var prop: PPropInfo;
    Obj: TObject;
begin
  Editor.Edited := true;
  Obj := TObject(Sender.Data);
  prop := PPropInfo(Sender.SysData);
  SetStrProp(Obj, prop, Sender.Caption);
end;

class procedure THandler.guiPropChar(Sender: zglTGUIObject);
var prop: PPropInfo;
    Obj: TObject;
    val: AnsiChar;
begin
  Editor.Edited := true;
  Obj := TObject(Sender.Data);
  prop := PPropInfo(Sender.SysData);
  if Length(zglTEdit(Sender).Caption) >= 1 then begin
    val := zglTEdit(Sender).Caption[1];
  end else begin
    val := #0;
  end;
  SetOrdProp(Obj, prop, Ord(val));
end;

class procedure THandler.guiPropSingle(Sender: zglTGUIObject);
var prop: PPropInfo;
    Obj: TObject;
begin
  Editor.Edited := true;
  Obj := TObject(Sender.Data);
  prop := PPropInfo(Sender.SysData);
  SetFloatProp(Obj, prop, u_StrToFloat(zglTEdit(Sender).Caption));
end;

class procedure THandler.guiPropInteger(Sender: zglTGUIObject);
var prop: PPropInfo;
    Obj: TObject;
begin
  Editor.Edited := true;
  Obj := TObject(Sender.Data);
  prop := PPropInfo(Sender.SysData);
  SetOrdProp(Obj, prop, u_StrToInt(zglTEdit(Sender).Caption));
end;

class procedure THandler.guiPropTrackBar(Sender: zglTGUIObject);
var prop: PPropInfo;
    Obj: TObject;
begin
  Editor.Edited := true;
  Obj := TObject(Sender.Data);
  prop := PPropInfo(Sender.SysData);
  SetOrdProp(Obj, prop, zglTTrackBar(Sender).Progress);
end;

class procedure THandler.guiPropEnum(Sender: zglTGUIObject);
var prop: PPropInfo;
    Obj: TObject;
begin
  Editor.Edited := true;
  Obj := TObject(Sender.Data);
  prop := PPropInfo(Sender.SysData);
  SetEnumProp(Obj, prop, zglTComboBox(Sender).Items[zglTComboBox(Sender).Selected]);
end;

class procedure THandler.guiPropSelector(Sender: zglTGUIObject);
var prop: PPropInfo;
    Obj: TObject;
    nm: AnsiString;
    i: integer;
begin
  Editor.Edited := true;
  Obj := TObject(Sender.Data);
  prop := PPropInfo(Sender.SysData);

  if zglTComboBox(Sender).Selected = 0 then begin
    SetObjectProp(Obj, prop, nil);
  end else begin
    nm := zglTComboBox(Sender).Items[zglTComboBox(Sender).Selected];
    for i := 0 to lstOtherComponents.ItemsCount - 1 do
      if zglTNamedObject(lstOtherComponents.Items[i].Data).Name = nm then begin
        SetObjectProp(Obj, prop, lstOtherComponents.Items[i].Data);
        exit;
      end;
    SetObjectProp(Obj, prop, nil);
  end;
end;

class procedure THandler.guiEditValues(Sender: zglTGUIObject; X, Y: integer);
var val: TStringList;
    li: zglTListItem;
    i: integer;
begin
  val := Sender.Data;
  lstEditItems.Clear;

  for i := 0 to val.Count - 1 do begin
    li := zglTListItem.Create(lstEditItems, val.Strings[i], 16);
    lstEditItems.AddItem(li);
  end;

  frmEditValue.Data := val;
  frmEditValue.MoveToCenter;
  frmEditValue.ShowModal;
end;

class procedure THandler.guiEvntProp(Sender: zglTGUIObject);
var Obj: zglTGUIObject;
    evh: TEventHandler;
    ev: TEditorEvent;
    prop: PPropInfo;
    nm: TCaption;
begin
  Obj := zglTGUIObject(Sender.Data);
  prop := Sender.SysData;
  evh := Editor.GetObjectEvent(Obj, prop^.Name);
  nm := zglTComboBox(Sender).Items[zglTComboBox(Sender).Selected];
  if nm = 'NEW' then begin
    edtEvName.Caption := '';
    cmbEvType.Selected := Sender.Tag;
    frmNewEvent.Data := Sender;
    frmNewEvent.ShowModal;
    exit;
  end;
  if evh <> nil then begin
    if nm = '' then begin
      Editor.DeleteEventHandle(evh);
      exit;
    end;
    ev := Editor.GetEvent(nm);
    if ev <> nil then begin
      evh.Event := ev;
    end;
  end else begin
    if nm = '' then exit;
    ev := Editor.GetEvent(nm);
    if ev <> nil then begin
      Editor.RegisterEventHandle(obj, ev, prop^.Name);
    end;
  end;
end;

procedure TEditor.EditComponent(Obj: zglTNamedObject; Level: Byte = 0);
var
  i, countOf: integer;
  props: PPropList;
  prop: PPropInfo;
  PT : PTypeData;
  PI : PTypeInfo;
  PPI: PPropInfo;
  tinfo: Ansistring;
  itm: zglTTableItem;
  chk: zglTCheckBox;
  edt: zglTEdit;
  ev: TEditorEvent;
  evh: TEventHandler;
  btn: zglTButton;
  cmb: zglTComboBox;
  trck: zglTTrackBar;
  pti: PTypeInfo;
  o: TObject;


  procedure RegisterOpt(tbl: zglTTable; prp: PPropInfo);
  var j, i_: integer;
      bj: zglTEditableClass;
      b: Boolean;
  begin
    itm := zglTTableItem.Create(tbl);
    if Level <> 0 then begin
      setlength(tinfo, Level * 2);
      FillChar(tinfo[1], Level * 2, ' ');
      itm.AddColumn(TCaption(tinfo) + '.' + TCaption(prp^.Name));
    end else
      itm.AddColumn(prp^.Name);

    tbl.AddItem(itm);
    case prp^.PropType^.Kind of
      tkEnumeration{$IFDEF FPC}, tkBool{$ENDIF}: begin
        tinfo := u_StrDown(prp^.PropType^.Name);
        if tinfo = 'boolean' then begin
          chk := zglTCheckBox.Create(MainGui, 0, 0, 10, 10, '');
          chk.Checked := GetOrdProp(Obj, prp) = 1;
          chk.Data := Obj;
          chk.SysData := prp;
          chk.OnClick := THandler.guiPropBoolean;
          itm.AddComponent(chk);
        end else begin
          cmb := zglTComboBox.Create(MainGui, 0, 0, 10, 10);
          pti := prp^.PropType{$IFNDEF FPC}^{$ENDIF};
          with GetTypeData(pti)^ do
            for j := MinValue to MaxValue do
              cmb.Items.Add(GetEnumName(pti, j));
          cmb.Data := Obj;
          cmb.SysData := prp;
          cmb.Selected := GetOrdProp(Obj, prp);
          cmb.OnChange := THandler.guiPropEnum;
          itm.AddComponent(cmb);
        end;
      end;
      tkLString, tkWString, tkString, tkUString{$IFDEF FPC}, tkAString{$ENDIF}: begin
        if prp^.Name <> 'FileName' then begin
          edt := zglTEdit.Create(MainGui, 0, 0, 10, 10,
            GetStrProp(Obj, prp));
          edt.Data := Obj;
          edt.SysData := prp;
          edt.OnChange := THandler.guiPropString;
          itm.AddComponent(edt);
        end else begin
          if GetStrProp(Obj, prp) = '' then
            btn := zglTButton.Create(MainGui, 0, 0, 10, 10, 'Browse ...')
          else
            btn := zglTButton.Create(MainGui, 0, 0, 10, 10, GetStrProp(Obj, prp));
          btn.Data := Obj;
          btn.SysData := prp;
          btn.OnClick := THandler.guiBrowseFileName;
          itm.AddComponent(btn);
        end;
      end;
      tkFloat: begin
        edt := zglTEdit.Create(MainGui, 0, 0, 10, 10,
          u_FloatToStr(GetFloatProp(Obj, prp)));
        edt.Data := Obj;
        edt.SysData := prp;
        edt.OnChange := THandler.guiPropSingle;
        itm.AddComponent(edt);
      end;
      tkChar: begin
        if GetOrdProp(Obj, prp) > 0 then
          edt := zglTEdit.Create(MainGui, 0, 0, 10, 10,
            Char(GetOrdProp(Obj, prp)))
        else
          edt := zglTEdit.Create(MainGui, 0, 0, 10, 10, '');
        edt.Data := Obj;
        edt.SysData := prp;
        edt.OnChange := THandler.guiPropChar;
        itm.AddComponent(edt);
      end;
      tkInteger: begin
        tinfo := u_StrDown(prp^.PropType^.Name);
        if tinfo = 'byte' then begin
          trck := zglTTrackBar.Create(MainGui, 0, 0, 10, 10);
          trck.Min := 0;
          trck.Max := 255;
          trck.Data := Obj;
          trck.SysData := prp;
          trck.Progress := GetOrdProp(Obj, prp);
          trck.OnChange := THandler.guiPropTrackBar;
          itm.AddComponent(trck);
        end else
        if tinfo = 'tcolor' then begin
          btn := zglTButton.Create(MainGui, 0, 0, 10, 10, '#' + IntToHex(GetOrdProp(Obj, prp), 6));
          btn.Data := Obj;
          btn.SysData := prp;
          btn.OnClick := THandler.guiBrowseColor;
          itm.AddComponent(btn);
        end else begin
          edt := zglTEdit.Create(MainGui, 0, 0, 10, 10,
            u_IntToStr(GetOrdProp(Obj, prp)));
          edt.Data := Obj;
          edt.SysData := prp;
          edt.OnChange := THandler.guiPropInteger;
          itm.AddComponent(edt);
        end;
      end;
      tkMethod: begin
        cmb := zglTComboBox.Create(MainGui, 0, 0, 10, 10);
        cmb.Items.Add('');
        cmb.SysData := prp;
        evh := Editor.GetObjectEvent(Obj, prp^.Name);
        for j := 0 to Editor.fEvents.Count - 1 do begin
          ev := TEditorEvent(Editor.fEvents.Items[j]);
          if ev.EventType = TCaption(prp^.PropType^.Name) then begin
            cmb.Items.Add(ev.EventName);
            if evh <> nil then begin
              if (evh.Event = ev) then
                cmb.Selected := cmb.Items.Count - 1;
            end;
          end;
        end;
        cmb.Font := fntLeft;
        cmb.Items.Add('NEW');
        cmb.Data := Obj;
        cmb.Tag := Editor.GetEventId(TCaption(prp^.PropType^.Name));
        cmb.OnChange := THandler.guiEvntProp;
        itm.AddComponent(cmb);
      end;
      tkClass: begin
        o := GetObjectProp(TObject(Obj), prp);
        if Assigned(o) then begin
          if o.InheritsFrom(zglTEditableClass) then begin
            bj := zglTEditableClass(o);
            case bj.Editable of
              emNone: begin
                itm.AddColumn('Not editable.');
              end;
              emTrue: begin
                if IsSelectable(Obj, prp) then begin
                  cmb := zglTComboBox.Create(MainGui, 0, 0, 10, 10);
                  cmb.Font := fntLeft;
                  cmb.Data := Obj;
                  cmb.SysData := prp;
                  cmb.Tag := i_;
                  cmb.Selected := 0;
                  cmb.Items.Add('-- NONE --');

                  for j := 0 to lstOtherComponents.ItemsCount - 1 do
                    if zglTNamedObject(lstOtherComponents.Items[j].Data).ClassName = prp^.PropType^.Name then begin
                      cmb.Items.Add(zglTNamedObject(lstOtherComponents.Items[j].Data).Name);
                      if o = lstOtherComponents.Items[j].Data then
                        cmb.Selected := j + 1;
                    end;
                  cmb.OnChange := THandler.guiPropSelector;

                  itm.AddComponent(cmb);
                end else begin
                  itm.AddColumn(TCaption(prp^.PropType^.Name));
                  EditComponent(zglTGUIObject(o),
                    Level + 1);
                end;
              end;
            end;
          end else begin
            if o is TStringList then begin
              btn := zglTButton.Create(MainGui, 0, 0, 10, 10, 'Edit values ...');
              btn.Data := o;
              btn.OnClick := THandler.guiEditValues;
              itm.AddComponent(btn);
            end else
              itm.AddColumn('Unknown class: ' + o.ClassName);
          end;
        end;
      end
      else
        itm.AddColumn(u_IntToStr(Integer(prp^.PropType^.Kind)));
    end;
  end;


begin
  if Level = 0 then begin
    tblProperties.Clear;
    tblProperties.Data := Obj;
    tblEvents.Clear;
    tblEvents.Data := Obj;
  end;

  PI := Obj.ClassInfo;
  PT := GetTypeData(PI);

  if PT^.PropCount = 0 then exit;

  new(props);
  GetPropInfos(PI, props);

  for i := 0 to Pred(PT^.PropCount) do begin
    prop := props^[i];
    tinfo := prop^.Name;
    try
      if (not (prop^.PropType^.Kind in
        [tkMethod])) and Assigned(prop^.GetProc) and
        (prop^.Name[1] <> '_') then begin
        RegisterOpt(tblProperties, prop);
      end;
      if ((prop^.PropType^.Kind in
        [tkMethod])) and Assigned(prop^.GetProc) and
        (prop^.Name[1] <> '_') then begin
        RegisterOpt(tblEvents, prop);

      end;
    except

    end;
  end;
end;

class procedure THandler.guiCreateForm(Sender: zglTGUIObject; X, Y: integer);
var frm: zglTForm;
begin
  frm := Editor.CreateForm;
  frm.OnClick := THandler.guiSelect;
end;

class procedure THandler.guiCreateFont(Sender: zglTGUIObject; X, Y: integer);
var fnt: zglTFontContainer;
begin
  fnt := Editor.CreateFont;
  THandler.guiSelectFont(Sender, X, Y);
end;

class procedure THandler.guiCreateState(Sender: zglTGUIObject; X, Y: integer);
var i: Integer;
begin
  Editor.RegisterState;
  cmbStateTo.Items.Clear;
  for i := 0 to Editor.fStates.Count - 1 do
    if i = 0 then
      cmbStateTo.Items.Add('DEF.')
    else
      cmbStateTo.Items.Add(u_IntToStr(i));
end;

class procedure THandler.guiNE(Sender: zglTGUIObject; X, Y: integer);
begin
  edtEvName.Caption := '';
  cmbEvType.Selected := 0;
  frmNewEvent.Data := nil;
  frmNewEvent.ShowModal;
end;

class procedure THandler.guiNEOk(Sender: zglTGUIObject; X, Y: integer);
var i: integer;
    AnsiCapt: AnsiString;
    cmb: zglTComboBox;
begin
  AnsiCapt := AnsiString(edtEvName.Caption);
  if edtEvName.Caption = '' then
    exit;
  for i := 1 to Length(AnsiCapt) do
    if (not (AnsiCapt[i] in ['a'..'z', 'A'..'Z', '_', '0'..'9'])) or
      ((AnsiCapt[i] in ['0'..'9']) and (i = 1)) then
        Exit;
  Editor.RegisterEvent(edtEvName.Caption,
    cmbEvType.Items[cmbEvType.Selected]);
  if frmNewEvent.Data <> nil then begin
    cmb := zglTComboBox(frmNewEvent.Data);
    cmb.Items[cmb.Selected] := edtEvName.Caption;
    cmb.Items.Add('NEW');
    guiEvntProp(cmb);
  end;
  frmNewEvent.Hide;
end;

class procedure THandler.guiNECancel(Sender: zglTGUIObject; X, Y: integer);
var cmb: zglTComboBox;
    obj: zglTGUIObject;
begin
  if frmNewEvent.Data <> nil then begin
    cmb := zglTComboBox(frmNewEvent.Data);
    obj := zglTGUIObject(cmb.Data);
    Editor.EditComponent(obj);
    frmProperties.Caption := 'Inspector: ' + obj.ClassName;
  end;
  frmNewEvent.Hide;
end;


class procedure THandler.guiDeleteFormOk(Sender: zglTGUIObject; X, Y: integer);
var frm: zglTGUIObject;
    li, lit: zglTListItem;
    st: TGUIState;
    i: integer;
    fnt: zglTFontContainer;
begin
  case frmAsk.Tag of
    FRMASC_OBJECT: begin
      btnDeleteForm.Enabled := false;
      btnNewObject.Enabled := false;
      gui.State := 0;
      li := zglTListItem(frmAsk.Data);
      frm := zglTGUIObject(li.Data);
      DeleteChild(frm);
      if frm.Parent = nil then begin
        for i := li.Tag + 1 to Gui.StatesCount - 1 do begin
          lit := zglTListItem(Gui.ItemsForState[i].Items[0].Data);
          lit.Tag := lit.Tag - 1;
        end;
        Gui.DeleteState(li.Tag);
        lstForms.Selected := nil;
        tblProperties.Clear;
        tblEvents.Clear;
      end else begin
        frm.Parent.Items.Remove(frm);
        lstForms.Selected := zglTListItem(frm.Parent.Data);
      end;
      if selected = frm then
        selected := nil;
    end;
    FRMASC_STATE: begin
      li := zglTListItem(frmAsk.Data);
      st := TGUIState(li.Data);
      Editor.DeleteState(st);
    end;
    FRMASC_FONT: begin
      li := zglTListItem(frmAsk.Data);
      fnt := zglTFontContainer(li.Data);
      if fnt.SelfCount > 1 then begin
        MainGui.ShowMessage('Error',
          'Some components uses this font, remove the usage first.');
      end else begin
        fnt.Drop;
        lstOtherComponents.RemoveItem(li);
        lstOtherComponents.Selected := nil;
      end;

    end;
  end;
  frmAsk.Hide;
end;

class procedure THandler.guiDeleteEvent(Sender: zglTGUIObject; X, Y: integer);
var ev: TEditorEvent;
begin
  if tblPtopList.Selected <> nil then begin
    if tblPtopList.Selected.Data <> nil then begin
      if Sender.Data = nil then begin
        Sender.Data := tblPtopList.Selected.Data;
        Sender.Caption := 'Are u shure?';
      end else begin
        if Sender.Data = tblPtopList.Selected.Data then begin
          ev := TEditorEvent(Sender.Data);
          tblPtopList.RemoveItem(ev.Item);
          Editor.DeleteEvent(ev);
          tblPtopList.Selected := nil;
        end;
        Sender.Data := nil;
        Sender.Caption := 'Delete';
      end;
    end;
  end;
end;

class procedure THandler.guiDeleteFormCancel(Sender: zglTGUIObject; X, Y: integer);
begin
  frmAsk.Hide;
end;

class procedure THandler.guiSelectFont(Sender: zglTGUIObject; X, Y: integer);
begin
  if lstOtherComponents.Selected <> nil then begin
    btnDeleteFont.Enabled := true;
    lstForms.Selected := nil;
    Editor.EditComponent(zglTGUIObject(lstOtherComponents.Selected.Data));
  end;
end;

class procedure THandler.guiSelectForm(Sender: zglTGUIObject; X, Y: integer);
var frm, prnt: zglTGUIObject;
begin
  if lstForms.Selected <> nil then begin
    frm := zglTGUIObject(lstForms.Selected.Data);
    if frm <> nil then begin
      if frm.Parent = nil then begin
        Gui.State := zglTListItem(frm.Data).Tag;
        cmbStateTo.Selected := TGUIState(frm.State).Id;
      end else begin
        prnt := frm.Parent;
        while prnt.Parent <> nil do
          prnt := prnt.Parent;
        Gui.State := zglTListItem(prnt.Data).Tag;
      end;
      Editor.EditComponent(frm);
      frmProperties.Caption := 'Inspector: ' + frm.ClassName;
      selected := frm;
    end;

    lstOtherComponents.Selected := nil;
    pcForms.Selected := 0;

    btnDeleteForm.Enabled := true;
    btnNewObject.Enabled := true;

  end;
end;

class procedure THandler.guiDeleteForm(Sender: zglTGUIObject; X, Y: integer);
begin
  if lstForms.Selected <> nil then begin
    frmAsk.Caption := 'Are you sure?';
    frmAsk.Tag := FRMASC_OBJECT;
    frmAsk.Data := lstForms.Selected;
    frmAsk.DisplayEffect := deSlideDown;
    frmAsk.ShowModal;
  end;
end;

class procedure THandler.guiDeleteFont(Sender: zglTGUIObject; X, Y: integer);
begin
  if lstOtherComponents.Selected <> nil then begin
    frmAsk.Caption := 'Are you sure?';
    frmAsk.Tag := FRMASC_FONT;
    frmAsk.Data := lstOtherComponents.Selected;
    frmAsk.DisplayEffect := deSlideDown;
    frmAsk.ShowModal;
  end;
end;

class procedure THandler.guiDeleteState(Sender: zglTGUIObject; X, Y: integer);
begin
  if lstStates.Selected <> nil then begin
    if lstStates.Selected.Caption <> 'DEF.' then begin
      frmAsk.Caption := 'Are you sure?';
      frmAsk.Tag := FRMASC_STATE;
      frmAsk.Data := lstStates.Selected;
      frmAsk.DisplayEffect := deSlideDown;
      frmAsk.ShowModal;
    end;
  end;
end;

class procedure THandler.guiNewObject(Sender: zglTGUIObject; X, Y: integer);
begin
  if lstForms.Selected <> nil then begin
    ppmNewObject.Popup(Sender, mouse_X(), mouse_Y());
  end;
end;

procedure Init;
var i, slct: integer;
    mi: zglTMenuItem;
begin

  // loading a font
  fntMain := font_LoadFromFile('Data/Main.zfi');

  // loading skins
  skinMain := zglTGuiSkin.Create('Data/main.skin');

  txSS := tex_LoadFromFile('Data/txSS.tga');
  txComponents := tex_LoadFromFile('Data/txComponents.tga');
  tex_SetFrameSize(txComponents, 16, 16);

  fntLeft := zglTFontContainer.Create(
        zglTFontObject.Create(fntMain, 1, $000000, 255, faMiddleLeft),
        zglTFontObject.Create(fntMain, 1, $101080, 255, faMiddleLeft),
        zglTFontObject.Create(fntMain, 1, $555555, 128, faMiddleLeft)
      );
  fntLeft.UseCounter;

  // creating a gui object
  Gui := zglTGui.Create(skinMain, SYSWIDTH + SYSOFFSET, SYSOFFSET,
    sX - SYSWIDTH - SYSOFFSET * 2, sY - SYSOFFSET * 2,
      zglTFontContainer.Create(
        zglTFontObject.Create(fntMain, 1, $000000, 255),
        zglTFontObject.Create(fntMain, 1, $101010, 255),
        zglTFontObject.Create(fntMain, 1, $555555, 128)
      ));
  MainGui := zglTGui.Create(zglTGuiSkin.Create('Data/main.skin'), 0, 0,
    SYSWIDTH, sY, zglTFontContainer.Create(
        zglTFontObject.Create(fntMain, 1, $000000, 255),
        zglTFontObject.Create(fntMain, 1, $101080, 255),
        zglTFontObject.Create(fntMain, 1, $555555, 128)
      ));

  mmMenu := zglTMainMenu.Create(MainGui, 0, 0, SYSWIDTH);

  miCurrent := zglTMenuItem.Create('Save current', THandler.saveGui, false);

  mmMenu.AddItem(zglTMainMenuItem.Create('GUI', 80,
    zglTPopupMenu.Create(MainGui, 140)
      .AddItem(zglTMenuItem.Create('Open gui ...', THandler.beginLoadGui))
      .AddItem(zglTMenuItem.Create('Save gui ...', THandler.saveGui))
      .AddItem(miCurrent)
      .AddItem(zglTMenuItem.Create('Clear', THandler.clearGui))
      .AddItem(zglTMenuItem.Create('-', nil))
      .AddItem(zglTMenuItem.Create('Exit', THandler.exitGui))));

  mmMenu.AddItem(zglTMainMenuItem.Create('Editor', 80,
    zglTPopupMenu.Create(MainGui, 100)
      .AddItem(zglTMenuItem.Create('Configuration', THandler.ConfigGui))
      .AddItem(zglTMenuItem.Create('Set working dir', THandler.SetWorkingDir))
      .AddItem(zglTMenuItem.Create('Load other skin', THandler.LoadSkin))));

  MainGui.Items.Add(mmMenu);

  frmAsk := zglTForm.Create(MainGui, 0, 18, 180, 60,
    '', false);
  frmAsk.MoveToCenter;
    btnDeleteOk := zglTButton.Create(MainGUi, 10,
      10, 60, 20, 'Delete');
    btnDeleteOk.OnClick := THandler.guiDeleteFormOk;
    frmAsk.Items.Add(btnDeleteOk);
    btnDeleteCancel := zglTButton.Create(MainGUi, 90,
      10, 60, 20, 'Cancel');
    btnDeleteCancel.OnClick := THandler.guiDeleteFormCancel;
    frmAsk.Items.Add(btnDeleteCancel);
  MainGui.Items.Add(frmAsk);

  frmNewEvent := zglTForm.Create(MainGui, 0, 18, 180, 120,
    'Create new event', false);
  frmNewEvent.MoveToCenter;
    btnNEOk := zglTButton.Create(MainGUi, 10,
      70, 60, 20, 'Create');
    btnNEOk.OnClick := THandler.guiNEOk;
    frmNewEvent.Items.Add(btnNEOk);
    btnNECancel := zglTButton.Create(MainGUi, 100,
      70, 60, 20, 'Cancel');
    btnNECancel.OnClick := THandler.guiNECancel;
    frmNewEvent.Items.Add(btnNECancel);
    edtEvName := zglTEdit.Create(MainGui,
      10, 26, 150, 20, '');
    frmNewEvent.Items.Add(edtEvName);
    cmbEvType := zglTComboBox.Create(MainGui,
      10, 46, 150, 20, EVENTS_COUNT);
    for i := 0 to EVENTS_COUNT - 1 do begin
      cmbEvType.Items.Add(EVENTS[i].name);
    end;
    frmNewEvent.Items.Add(cmbEvType);
    frmNewEvent.Items.Add(zglTLabel.Create(
      MainGui, 10, 0, 150, 25, 'New event name (A-Z, 0-9):'));
  MainGui.Items.Add(frmNewEvent);

  frmForms := zglTForm.Create(MainGui, 0, 17, SYSWIDTH, FORMSHEIGHT,
    'Forms', true);

  pcForms := zglTPageControl.Create(MainGui, 0, 0, 10, 10);
  pcForms.Align := caClient;

  frmForms.Items.Add(pcForms);

  pcsForms := zglTPageControlSheet.Create(MainGui, 'Forms');
  pcForms.Items.Add(pcsForms);
  pcsStates := zglTPageControlSheet.Create(MainGui, 'States');
  pcForms.Items.Add(pcsStates);
  pcsFonts := zglTPageControlSheet.Create(MainGui, 'Other components');
  pcForms.Items.Add(pcsFonts);

  btnNewFont := zglTButton.Create(MainGUi, 1,
    FORMSHEIGHT - 72, 95, 20, 'New font');
  btnNewFont.OnClick := THandler.guiCreateFont;
  pcsFonts.Items.Add(btnNewFont);

  btnDeleteFont := zglTButton.Create(MainGUi, 97,
    FORMSHEIGHT - 72, 100, 20, 'Delete font');
  btnDeleteFont.Enabled := false;
  btnDeleteFont.OnClick := THandler.guiDeleteFont;
  pcsFonts.Items.Add(btnDeleteFont);

  lstOtherComponents := zglTList.Create(MainGui, 4, 4, SYSWIDTH - 20,
    FORMSHEIGHT - 72);
  lstOtherComponents.OnSelectItem := THandler.guiSelectFont;
  lstOtherComponents.Align := caTop;
  pcsFonts.Items.Add(lstOtherComponents);

  pcForms.Selected := 0;

  lstForms := zglTList.Create(MainGui, 4, 4, SYSWIDTH - 20,
    FORMSHEIGHT - 72);
  lstForms.OnSelectItem := THandler.guiSelectForm;
  lstForms.Align := caTop;
  lstForms.IconsTexture := txComponents;
  lstForms.SetIconsSize(16, 16);
  lstForms.ShowIcons := true;
  pcsForms.Items.Add(lstForms);

  btnCreateForm := zglTMultiButton.Create(MainGUi, 1,
    FORMSHEIGHT - 72, 85, 20, 'New form');
  btnCreateForm.OnClick := THandler.guiCreateForm;

  mi := zglTMenuItem.Create('New frame', THandler.createFrame);
  mi.IconId := TEX_FRAME;

  btnCreateForm.DropMenu.IconsTexture := txComponents;
  btnCreateForm.DropMenu.ShowIcons := true;

  btnCreateForm.DropMenu.AddItem(mi);
  pcsForms.Items.Add(btnCreateForm);

  cmbStateTo := zglTComboBox.Create(MainGui,
    87, FORMSHEIGHT - 72, 64, 20);
  cmbStateTo.Items.Add('DEF.');
  cmbStateTo.OnChange := THandler.guiUpdateState;
  pcsForms.Items.Add(cmbStateTo);

  btnDeleteForm := zglTButton.Create(MainGUi, 152,
    FORMSHEIGHT - 72, 60, 20, 'Delete');
  btnDeleteForm.Enabled := False;
  btnDeleteForm.OnClick := THandler.guiDeleteForm;
  pcsForms.Items.Add(btnDeleteForm);

  btnNewObject := zglTButton.Create(MainGUi, 213,
    FORMSHEIGHT - 72, 70, 20, 'New obj');
  btnNewObject.Enabled := False;
  btnNewObject.OnClick := THandler.guiNewObject;
  pcsForms.Items.Add(btnNewObject);

  lstStates := zglTList.Create(MainGui, 1, 1, SYSWIDTH - 20,
    FORMSHEIGHT - 72);
  lstStates.Align := caTop;
  lstStates.IconsTexture := txComponents;
  lstStates.SetIconsSize(16, 16);
  lstStates.ShowIcons := true;
  lstStates.ItemsPerLine := 2;

  btnCreateState := zglTButton.Create(MainGUi, 1,
    FORMSHEIGHT - 72, 85, 20, 'New state');
  btnCreateState.OnClick := THandler.guiCreateState;
  pcsStates.Items.Add(btnCreateState);

  btnDeleteState := zglTButton.Create(MainGUi, 87,
    FORMSHEIGHT - 72, 60, 20, 'Delete');
  btnDeleteState.OnClick := THandler.guiDeleteState;
  pcsStates.Items.Add(btnDeleteState);

  pcsStates.Items.Add(lstStates);

  MainGui.Items.Add(frmForms);

  frmProperties := zglTForm.Create(MainGui, 0,
    frmForms.Rect.Y + frmForms.Rect.H + 1,
    SYSWIDTH, sY - (frmForms.Rect.Y + frmForms.Rect.H + 1),
    'Inspector', true);

  pcOptions := zglTPageControl.Create(MainGui, 0, 0, 100, 100);
  pcOptions.Align := caClient;

  pcsProperties := zglTPageControlSheet.Create(MainGui, 'Properties');
  pcOptions.Items.Add(pcsProperties);
  pcsEvents := zglTPageControlSheet.Create(MainGui, 'Events');
  pcOptions.Items.Add(pcsEvents);
  pcsPropList := zglTPageControlSheet.Create(MainGui, 'Events List');
  pcOptions.Items.Add(pcsPropList);
  pcOptions.Selected := 0;

  tblPtopList := zglTTable.Create(MainGui, 0, 0, 100, 100);
  tblPtopList.Align := caBottom;
  tblPtopList.Rect.H := sY - (frmForms.Rect.Y + frmForms.Rect.H + 1) - 76;
  tblPtopList.DrawGrid := true;
  tblPtopList.AddHeader(
    zglTTableHeader.Create(tblPtopList, 'Procedure', 122, TEXT_LEFT, true));
  tblPtopList.AddHeader(
    zglTTableHeader.Create(tblPtopList, 'Type', 122, TEXT_LEFT));
  pcsPropList.Items.Add(tblPtopList);

  btnNewEvent := zglTButton.Create(MainGui, 1, 2, 120, 20, 'New procedure');
  btnNewEvent.OnClick := THandler.guiNE;
  pcsPropList.Items.Add(btnNewEvent);
  btnDeleteEvent := zglTButton.Create(MainGui, 122, 2, 100, 20, 'Delete');
  btnDeleteEvent.Data := nil;
  btnDeleteEvent.OnClick := THandler.guiDeleteEvent;
  pcsPropList.Items.Add(btnDeleteEvent);

  frmProperties.Items.Add(pcOptions);

  tblProperties := zglTTable.Create(MainGui, 0, 0, 100, 100);
  tblProperties.Align := caClient;
  tblProperties.DrawGrid := true;
  tblProperties.AddHeader(
    zglTTableHeader.Create(tblProperties, 'Property', 122, TEXT_LEFT, true));
  tblProperties.AddHeader(
    zglTTableHeader.Create(tblProperties, 'Value', 122, TEXT_LEFT));

  pcsProperties.Items.Add(tblProperties);

  tblEvents := zglTTable.Create(MainGui, 0, 0, 100, 100);
  tblEvents.Align := caClient;
  tblEvents.DrawGrid := true;
  tblEvents.AddHeader(
    zglTTableHeader.Create(tblEvents, 'Event', 122, TEXT_LEFT, true));
  tblEvents.AddHeader(
    zglTTableHeader.Create(tblEvents, 'Procedure', 122, TEXT_LEFT));

  pcsEvents.Items.Add(tblEvents);

  MainGui.Items.Add(frmProperties);

  ppmNewObject := zglTPopupMenu.Create(MainGui, 100);
  ppmNewObject.ShowIcons := true;
  ppmNewObject.SetIconsSize(16, 16);
  ppmNewObject.IconsTexture := txComponents;

  selected := nil;

  Editor := TEditor.Create;

  InitGui(MainGui);

  resList := zglPResolutionList(zgl_Get(RESOLUTION_LIST));
  slct := -1;
  for i := 0 to resList^.Count - 1 do begin
    cmbWindowSize.Items.Add(u_IntToStr(resList^.Width[i]) + 'x' +
      u_IntToStr(resList^.Height[i]));
    if (resList^.Width[i] = sX) and (resList^.Height[i] = sY) then
      slct := i;
  end;

  if slct <> -1 then begin
    cmbWindowSize.Selected := slct;
  end;
  chkFullScreen.Checked := FullScreen;

  frmConfig.MoveToCenter;

  InitComponents;
end;

procedure Draw;
var owner: TCaption;
    fnt: zglTFontContainer;
    r: zglTRect;
begin

  Gui.ForceVisible := true;

  // here we are drawing guis
  pr2d_Rect(0, 0, sX, sY, $FFFFFF, 255, PR2D_FILL);
  pr2d_Line(SYSWIDTH, 0, SYSWIDTH, sY, $666666, 64, PR2D_FILL);
  pr2d_Rect(0, 0, SYSWIDTH, sY, $000000, 16, PR2D_FILL);
  case EditorState of
    esNormal: begin
      case pcForms.Selected of
        2: begin
          if lstOtherComponents.Selected <> nil then begin
            fnt := zglTFontContainer(lstOtherComponents.Selected.Data);
            r := Gui.Rect;
            r.H := Gui.Rect.H / 3;
            scissor_Begin(round(r.X), round(r.Y), round(r.W), round(r.H));
            if fnt.Normal.Data <> nil then
              text_DrawInRectEx(fnt.Normal.Data,
                r,
                fnt.Normal.Size,
                0,
                'Normal:' + #13#10 + DEMOMESSAGE,
                fnt.Normal.Alpha,
                fnt.Normal.Color,
                TEXT_CLIP_RECT or FontAlignes[fnt.Normal.Align]
                )
            else
              text_DrawInRectEx(fntMain,
                r,
                fnt.Normal.Size,
                0,
                'Normal: FONT NOT LOADED',
                fnt.Normal.Alpha,
                fnt.Normal.Color,
                TEXT_CLIP_RECT or FontAlignes[fnt.Normal.Align]
                );
            scissor_End;
            r.Y := r.Y + r.H;
            scissor_Begin(round(r.X), round(r.Y), round(r.W), round(r.H));
            pr2d_Rect(r.x, r.y, r.w, r.H, 0, 16, PR2D_FILL);
            if fnt.Active.Data <> nil then
              text_DrawInRectEx(fnt.Active.Data,
                r,
                fnt.Active.Size,
                0,
                'Active:' + #13#10 + DEMOMESSAGE,
                fnt.Active.Alpha,
                fnt.Active.Color,
                TEXT_CLIP_RECT or FontAlignes[fnt.Active.Align]
                )
            else
              text_DrawInRectEx(fntMain,
                r,
                fnt.Active.Size,
                0,
                'Active: FONT NOT LOADED',
                fnt.Active.Alpha,
                fnt.Active.Color,
                TEXT_CLIP_RECT or FontAlignes[fnt.Active.Align]
                );
            scissor_End;
            r.Y := r.Y + r.H;
            scissor_Begin(round(r.X), round(r.Y), round(r.W), round(r.H));
            pr2d_Rect(r.x, r.y, r.w, r.H, 0, 32, PR2D_FILL);
            if fnt.Disabled.Data <> nil then
              text_DrawInRectEx(fnt.Disabled.Data,
                r,
                fnt.Disabled.Size,
                0,
                'Disabled:' + #13#10 + DEMOMESSAGE,
                fnt.Disabled.Alpha,
                fnt.Disabled.Color,
                TEXT_CLIP_RECT or FontAlignes[fnt.Disabled.Align]
                )
            else
              text_DrawInRectEx(fntMain,
                r,
                fnt.Disabled.Size,
                0,
                'Disabled: FONT NOT LOADED',
                fnt.Disabled.Alpha,
                fnt.Disabled.Color,
                TEXT_CLIP_RECT or FontAlignes[fnt.Disabled.Align]
                );
            scissor_End;
          end;
        end;
        else begin
          Gui.Draw;

          if selected <> nil then begin
            if selected.Visible then

              DrawSSRect(txSS,
                round(selected.RootPos.X + selected.Rect.X - 1),
                round(selected.RootPos.Y + selected.Rect.Y - 1),
                round(selected.Rect.W + 2),
                round(selected.Rect.H + 2),
                Round(Counter / 100) mod 4);
          end;
          if (Gui.Hover <> nil) and (Gui.Hover <> selected) then begin
            pr2d_Rect(Gui.Hover.RootPos.X + Gui.Hover.Rect.X - 1,
                      Gui.Hover.RootPos.Y + Gui.Hover.Rect.Y - 1,
                      Gui.Hover.Rect.W + 2, Gui.Hover.Rect.H + 2, $000000, 128);
          end;
        end;
      end;
      MainGui.Draw;
    end;    
    esAdd, esStart: begin   
      Gui.Draw;
      MainGui.Draw;     
      pr2d_Rect(0, 0, SYSWIDTH, sY, $000000, 240, PR2D_FILL);
      text_DrawEx(fntMain,
        10, 10, 1, 0,
        'Select Area to create component.'#13#10'Press RMB to cancel.', 255, $FFFFFF);

      if Gui.Hover <> nil then begin
        if Gui.Hover.Container then begin
          if (CallCreator^.CallAssistance = '') or
             (CallCreator^.CallAssistance = Gui.Hover.ClassName) then
            slctOwner := Gui.Hover
          else
            if EditorState <> esStart then
              slctOwner := nil;
        end else begin
          if (CallCreator^.CallAssistance = '') or
             (CallCreator^.CallAssistance = Gui.Hover.Parent.ClassName) then
            slctOwner := Gui.Hover.Parent
          else if EditorState <> esStart then
                  slctOwner := nil;
        end;
      end else if EditorState <> esStart then
                  slctOwner := nil;
          
      if slctOwner <> nil then begin
        owner := '"' + slctOwner.Caption + '": ' + slctOwner.Name;    
      end else owner := '-- NONE --';
      
      text_DrawEx(fntMain, 
        10, sY - 20, 1, 0, 
        'Owner: ' + owner, 255, $FFFFFF);        
        if slctOwner <> nil then begin
          pr2d_Rect(slctOwner.RootPos.X + slctOwner.Rect.X - 1,
                    slctOwner.RootPos.Y + slctOwner.Rect.Y - 1,
                    slctOwner.Rect.W + 2, slctOwner.Rect.H + 2, $FF3333, 32, PR2D_FILL);
        end;
      if EditorState = esStart then begin
        pr2d_Rect(StartX, StartY, mouse_X() - StartX, mouse_Y() - StartY, 0);
      end;
    end;
  end;


  Gui.ForceVisible := false;

  //MainGui.DrawMouse;
end;

procedure Update( dt : Double );
var own: zglTFrame;
    StartW, StartH: integer;
begin
  Counter := Counter + dt;
  case EditorState of
    esNormal: begin
      MainGui.Update(dt);
      Gui.ForceVisible := true;
      case pcForms.Selected of
        2: begin

        end;
        else begin
          Gui.Update(dt);
        end;
      end;
      Gui.ForceVisible := false;
    end;   
    esAdd: begin
      if mouse_Click(M_BRIGHT) then begin
        mouse_ClearState;
        EditorState := esNormal;
      end;  
      if mouse_Click(M_BLEFT) and (slctOwner <> nil) then begin
        mouse_ClearState;
        EditorState := esStart;
        StartX := mouse_X();
        StartY := mouse_Y();
      end;  
      mouse_ClearState;
      Gui.ForceVisible := true;
      Gui.Update(dt);
      Gui.ForceVisible := false;
    end;
    esStart: begin
      if mouse_Click(M_BRIGHT) then begin
        mouse_ClearState;
        EditorState := esNormal;
      end;    
      if not mouse_Down(M_BLEFT) then begin
        mouse_ClearState;
        own := zglTFrame(slctOwner);         
        StartW := mouse_X() - StartX;
        StartH := mouse_Y() - StartY;
        StartX := StartX - round(own.RootPos.X + own.Rect.X + own.ClientRect.X);
        StartY := StartY - round(own.RootPos.Y + own.Rect.Y + own.ClientRect.Y);
        if StartW < 0 then begin
          StartW := -StartW;
          Dec(StartX, StartW);
        end;    
        if StartH < 0 then begin
          StartH := -StartH;
          Dec(StartY, StartH);    
        end;

        Editor.AddObject(own, TCreateCallback(CallCreator^.CrtCllbck)(
          StartX, StartY, StartW, StartH
        ));
        EditorState := esNormal;
      end;    
      mouse_ClearState;
      Gui.ForceVisible := true;
      Gui.Update(dt);
      Gui.ForceVisible := false;
    end;
  end;
end;

procedure Quit;
begin

  ppmNewObject.Drop;

  fntLeft.Free;

  ini_WriteKeyInt('Main', 'sX', sX);
  ini_WriteKeyInt('Main', 'sY', sY);
  ini_WriteKeyBool('Main', 'Fullscreen', Fullscreen);
  ini_SaveToFile('Config.ini');
  FreeComponents;
  Editor.Free;
end;

end.