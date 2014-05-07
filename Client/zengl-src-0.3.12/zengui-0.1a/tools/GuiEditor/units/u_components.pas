unit u_components;

interface

uses zglGui;

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

procedure InitComponents;
procedure FreeComponents;
function CreateUnknownObject(ObjClassName: string): zglTGUIObject;

type
  PCoponentCaller = ^TCoponentCaller;
  TCoponentCaller = object
    CrtCllbck: pointer;
    CallAssistance, ObjClassName: TCaption;
    CreateImage: integer;
    procedure Init(Name, ClassName: TCaption; Callback: Pointer;
      CreateImage_: Integer; Assist: TCaption = '');
  end;

type
  PComponentSelector = ^TComponentSelector;
  TComponentSelector = record
    GetCount: function(Instance: zglTEditableClass): integer;
    GetValue: function(Instance: zglTEditableClass; Index: Integer): TCaption;
    GetName: function(Instance: zglTEditableClass; Index: Integer): TCaption;
    GetIndex: function(Instance: zglTEditableClass): Integer;
    GetInstance: function(Instance: zglTEditableClass; Index: integer): zglTEditableClass;
  end;

const
  SELECTABLE_COUNT = 1;

var
  cmpButton_, cmpImageButton_, cmpMultiButton_, cmpImage_, cmpCheckBox_,
  cmpComboBox_, cmpEdit_, cmpLabel_, cmpProgressBar_, cmpTrackBar_,
  cmpList_, cmpFrame_, cmpRadioBox_, cmpRadioButton_, cmpTable_,
  cmpPageControl_, cmpPageControlSheet_, cmpUnknownObject_,
  cmpUnknownObject :TCoponentCaller;
  slcFontContainer: TComponentSelector;

  SELECTABLE: array[0 .. SELECTABLE_COUNT - 1] of AnsiString = (
    'Font'
  );

implementation

uses u_editor;


procedure TCoponentCaller.Init(Name, ClassName: TCaption; Callback: Pointer;
  CreateImage_: Integer; Assist: TCaption = '');
begin
  ObjClassName := ClassName;
  CrtCllbck := Callback;
  CreateImage := CreateImage_;
  CallAssistance := Assist;
  Editor.RegisterComponent(Name, @Self);
end;

function cmpButton(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTButton;
begin
  btn := zglTButton.Create(Gui, X, Y, W, H, 'Button');
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpImageButton(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTImageButton;
begin
  btn := zglTImageButton.Create(Gui, X, Y, W, H, 0, 0);
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpMultiButton(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTMultiButton;
begin
  btn := zglTMultiButton.Create(Gui, X, Y, W, H, 'MultiButton');
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpImage(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTImage;
begin
  btn := zglTImage.Create(Gui, X, Y, W, H, 0, 0);
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpList(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTList;
begin
  btn := zglTList.Create(Gui, X, Y, W, H);
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpCheckBox(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTCheckBox;
begin
  btn := zglTCheckBox.Create(Gui, X, Y, W, H, 'CheckBox');
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpComboBox(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTComboBox;
begin
  btn := zglTComboBox.Create(Gui, X, Y, W, H);
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpEdit(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTEdit;
begin
  btn := zglTEdit.Create(Gui, X, Y, W, H, 'Edit');
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpLabel(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTLabel;
begin
  btn := zglTLabel.Create(Gui, X, Y, W, H, 'Label');
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpFrame(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTFrame;
begin
  btn := zglTFrame.Create(Gui, X, Y, W, H);
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpRadioBox(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTRadioBox;
begin
  btn := zglTRadioBox.Create(Gui, X, Y, W, H);
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpRadioButton(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTRadioButton;
begin
  btn := zglTRadioButton.Create(Gui, X, Y, W, H, 'RadioButton');
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpTable(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTTable;
begin
  btn := zglTTable.Create(Gui, X, Y, W, H);
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpProgressBar(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTProgressBar;
begin
  btn := zglTProgressBar.Create(Gui, X, Y, W, H);
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpTrackBar(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTTrackBar;
begin
  btn := zglTTrackBar.Create(Gui, X, Y, W, H);
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpPageControl(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTPageControl;
begin
  btn := zglTPageControl.Create(Gui, X, Y, W, H);
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpPageControlSheet(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTPageControlSheet;
begin
  btn := zglTPageControlSheet.Create(Gui, 'Tab', True);
  btn.ForceVisible := true;
  Result := btn;
end;

function cmpUnknownObjectFunc(X, Y, W, H: Integer): zglTGUIObject;
var btn: zglTUnknownObject;
begin
  btn := zglTUnknownObject.Create(Gui, X, Y, W, H, '');
  btn.UnknClassName := 'zglTSomeOtherObject';
  btn.ForceVisible := true;
  Result := btn;
end;

function CreateUnknownObject(ObjClassName: string): zglTGUIObject;
var btn: zglTUnknownObject;
begin
  btn := zglTUnknownObject.Create(Gui, 0, 0, 0, 0, '');
  btn.UnknClassName := ObjClassName;
  btn.ForceVisible := true;
  Result := btn;
end;

procedure FreeComponents;
begin
  //
end;

procedure InitComponents;
begin
  cmpButton_.Init('Button', 'zglTButton', @cmpButton, TEX_BUTTON);
  cmpImageButton_.Init('ImageButton', 'zglTImageButton', @cmpImageButton, TEX_IMAGEBUTTON);
  cmpMultiButton_.Init('MultiButton', 'zglTMultiButton', @cmpMultiButton, TEX_MULTIBUTTON);
  cmpImage_.Init('Image', 'zglTImage', @cmpImage, TEX_IMAGE);
  cmpCheckBox_.Init('Checkbox', 'zglTCheckBox', @cmpCheckBox, TEX_CHECKBOX);
  cmpComboBox_.Init('Combobox', 'zglTComboBox', @cmpComboBox, TEX_COMBOBOX);
  cmpEdit_.Init('Edit', 'zglTEdit', @cmpEdit, TEX_EDIT);
  cmpLabel_.Init('Label', 'zglTLabel', @cmpLabel, TEX_LABEL);
  cmpProgressBar_.Init('ProgressBar', 'zglTProgressBar', @cmpProgressBar, TEX_PROGRESSBAR);
  cmpTrackBar_.Init('TrackBar', 'zglTTrackBar', @cmpTrackBar, TEX_TRACKBAR);
  cmpList_.Init('List', 'zglTList', @cmpList, TEX_LIST);
  cmpFrame_.Init('Frame', 'zglTFrame', @cmpFrame, TEX_FRAME);
  cmpRadioBox_.Init('RadioBox', 'zglTRadioBox', @cmpRadioBox, TEX_RADIOGROUP);
  cmpRadioButton_.Init('RadioButton', 'zglTRadioButton', @cmpRadioButton, TEX_RADIOBUTTON,
    'zglTRadioBox');

  cmpTable_.Init('Table', 'zglTTable', @cmpTable, TEX_TABLE);
  cmpPageControl_.Init('PageControl', 'zglTPageControl', @cmpPageControl, TEX_PAGECONTROL);
  cmpPageControlSheet_.Init('PageControl TAB', 'zglTPageControlSheet', @cmpPageControlSheet, TEX_PAGECONTROL,
    'zglTPageControl');
  cmpUnknownObject_.Init('Other object...', 'zglTUnknownObject', @cmpUnknownObjectFunc,
    TEX_UNKNOWN);

  cmpUnknownObject.CreateImage := TEX_BUTTON;

end;

end.
