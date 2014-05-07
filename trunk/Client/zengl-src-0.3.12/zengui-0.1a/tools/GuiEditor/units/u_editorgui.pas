unit u_editorgui;

// PLEASE DO NOT REMOVE THE {*} COMMENTS!

interface

// Store here uses or proc definition
{INTERFACE}
uses zglGui, Classes, zgl_screen, zgl_main;
{END}

var
  frmConfig: zglTForm;
  lblWindowSize: zglTLabel;
  cmbWindowSize: zglTComboBox;
  chkFullScreen: zglTCheckBox;
  btnSaveConfig: zglTButton;
  frmEditValue: zglTForm;
  btnEditValueSave: zglTButton;
  edtEditCurrentValue: zglTEdit;
  lstEditItems: zglTList;
  btnAddEditItem: zglTButton;
  btnDeleteEditItem: zglTButton;
  btnUpEditItem: zglTButton;
  btnDownEditItem: zglTButton;
  btnEditCancel: zglTButton;
  frmConfirmClear: zglTForm;
  lblDataWarning: zglTLabel;
  btnClearYes: zglTButton;
  btnClearNo: zglTButton;
  frmInformation: zglTForm;
  lblInformationTitle: zglTLabel;
  btnInformBrowse: zglTButton;
  btnInformClose: zglTButton;

procedure InitGui(gui: zglTGui);

const
  CREATED_GUI_VERSION = '0.1a';

type
  zglTGuiEventHandler = class
    class procedure guiEditItemChange(Sender: zglTGUIObject);
    class procedure guiSaveConfig(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiSaveItems(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiSelectEditItem(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiNewEditItem(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiDeleteEditItem(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiUpEditItem(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiDownEditItem(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiEditItemsCancel(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiClearNo(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiClearYes(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiInformClose(Sender: zglTGUIObject; X, Y: integer);
    class procedure guiInformBrowse(Sender: zglTGUIObject; X, Y: integer);
  end;

implementation

// Store here recursive uses or proc implementation
{IMPLEMENTATION}
uses u_editor;
{END}

class procedure zglTGuiEventHandler.guiEditItemChange(Sender: zglTGUIObject);
{EVENT guiEditItemChange:zglTEvent}
var li: zglTListItem;
begin
  li := lstEditItems.Selected;
  if li <> nil then begin
    li.Caption := edtEditCurrentValue.Caption;
  end;
end;
{END}

class procedure zglTGuiEventHandler.guiSaveConfig(Sender: zglTGUIObject; X, Y: integer);
{EVENT guiSaveConfig:zglTMouseEvent}
begin
  sX := resList^.Width[cmbWindowSize.Selected];
  sY := resList^.Height[cmbWindowSize.Selected];
  FullScreen := chkFullScreen.Checked;
  scr_SetOptions(sX, sY, REFRESH_MAXIMUM, FullScreen, True);
  MainGui.Resize(SYSWIDTH, sY);
  Gui.Resize(sX - SYSWIDTH - SYSOFFSET * 2, sY - SYSOFFSET * 2);
  frmProperties.Resize(SYSWIDTH, sY - (frmForms.Rect.Y + frmForms.Rect.H + 1));
  frmConfig.Hide;
end;
{END}

class procedure zglTGuiEventHandler.guiSaveItems(Sender: zglTGUIObject; X, Y: integer);
{EVENT guiSaveItems:zglTMouseEvent}
var val: TStringList;
    i: integer;
begin
  val := TStringList(frmEditValue.Data);
  val.Clear;
  for i := 0 to lstEditItems.ItemsCount -1 do begin
      val.Add(lstEditItems.Items[i].Caption);
  end;
  lstEditItems.Clear;
  edtEditCurrentValue.Caption := '';
  frmEditValue.Hide;
end;
{END}

class procedure zglTGuiEventHandler.guiSelectEditItem(Sender: zglTGUIObject; X, Y: integer);
{EVENT guiSelectEditItem:zglTMouseEvent}
var li: zglTListItem;
begin
  li := lstEditItems.Selected;
  if li <> nil then begin
    edtEditCurrentValue.Caption := li.Caption;
    edtEditCurrentValue.Enabled := true;
    btnDeleteEditItem.Enabled := true;
    btnUpEditItem.Enabled := li.GetPosition() > 0;
    btnDownEditItem.Enabled := li.GetPosition() < lstEditItems.ItemsCount - 1;
  end;
end;
{END}

class procedure zglTGuiEventHandler.guiNewEditItem(Sender: zglTGUIObject; X, Y: integer);
{EVENT guiNewEditItem:zglTMouseEvent}
var li: zglTListItem;
begin
  li := zglTListItem.Create(lstEditItems, 'NEWITEM', 16);
  lstEditItems.AddItem(li);
  lstEditItems.Selected := li;
  edtEditCurrentValue.Caption := li.Caption;
  edtEditCurrentValue.Enabled := true;
  btnDeleteEditItem.Enabled := true;
  btnUpEditItem.Enabled := li.GetPosition() > 0;
  btnDownEditItem.Enabled := li.GetPosition() < lstEditItems.ItemsCount - 1;
end;
{END}

class procedure zglTGuiEventHandler.guiDeleteEditItem(Sender: zglTGUIObject; X, Y: integer);
{EVENT guiDeleteEditItem:zglTMouseEvent}
var li: zglTListItem;
begin
  li := lstEditItems.Selected;
  if li <> nil then begin
    lstEditItems.RemoveItem(li);
    edtEditCurrentValue.Caption := '-- SELECT ONCE --';
    edtEditCurrentValue.Enabled := false;
    btnDeleteEditItem.Enabled := false;
    btnUpEditItem.Enabled := false;
    btnDownEditItem.Enabled := false;
  end;
end;
{END}

class procedure zglTGuiEventHandler.guiUpEditItem(Sender: zglTGUIObject; X, Y: integer);
{EVENT guiUpEditItem:zglTMouseEvent}
var li: zglTListItem;
begin
  li := lstEditItems.Selected;
  if li <> nil then begin
    if li.GetPosition() > 0 then
      lstEditItems.MoveItem(li, li.GetPosition() - 1);
    btnUpEditItem.Enabled := li.GetPosition() > 0;
    btnDownEditItem.Enabled := li.GetPosition() < lstEditItems.ItemsCount - 1;
  end;
end;
{END}

class procedure zglTGuiEventHandler.guiDownEditItem(Sender: zglTGUIObject; X, Y: integer);
{EVENT guiDownEditItem:zglTMouseEvent}
var li: zglTListItem;
begin
  li := lstEditItems.Selected;
  if li <> nil then begin
    if li.GetPosition() < lstEditItems.ItemsCount - 1 then
      lstEditItems.MoveItem(li, li.GetPosition() + 1);
    btnUpEditItem.Enabled := li.GetPosition() > 0;
    btnDownEditItem.Enabled := li.GetPosition() < lstEditItems.ItemsCount - 1;
  end;
end;
{END}

class procedure zglTGuiEventHandler.guiEditItemsCancel(Sender: zglTGUIObject; X, Y: integer);
{EVENT guiEditItemsCancel:zglTMouseEvent}
begin
  lstEditItems.Clear;
  edtEditCurrentValue.Caption := '';
  frmEditValue.Hide;
end;
{END}

class procedure zglTGuiEventHandler.guiClearNo(Sender: zglTGUIObject; X, Y: integer);
{EVENT guiClearNo:zglTMouseEvent}
begin
  frmConfirmClear.Hide;
end;
{END}

class procedure zglTGuiEventHandler.guiClearYes(Sender: zglTGUIObject; X, Y: integer);
{EVENT guiClearYes:zglTMouseEvent}
begin
  frmConfirmClear.Hide;
  case Editor.ActionEvent of
    aeClear: begin
      Editor.Clear();
    end;
    aeExit: begin
      zgl_Exit;
    end;
    aeOpen: begin
      THandler.LoadGui(Sender, nil, X, Y);
    end;
  end;
end;
{END}

class procedure zglTGuiEventHandler.guiInformClose(Sender: zglTGUIObject; X, Y: integer);
{EVENT guiInformClose:zglTMouseEvent}
begin
  frmInformation.Hide;
end;
{END}

class procedure zglTGuiEventHandler.guiInformBrowse(Sender: zglTGUIObject; X, Y: integer);
{EVENT guiInformBrowse:zglTMouseEvent}
begin
  if Editor.BrowseRelativePath then
    frmInformation.Hide;
end;
{END}

procedure InitGui(gui: zglTGui);
begin
  {INIT}
  {$IF CREATED_GUI_VERSION <> GUI_VERSION}
    {$MESSAGE Fatal 'GUI version diff with generated code.'}
  {$IFEND}
  frmConfig := zglTForm.CreateDefaults(Gui);
  with frmConfig do begin
    Caption := 'Configuration'; 
    Name := 'frmConfig'; 
    Rect.H := 134.00; 
    Rect.W := 162.00; 
    Rect.X := 7.00; 
    Rect.Y := 6.00; 
    Visible := False;
    DisplayEffect := deZoomIn;
  end;
  Gui.Items.Add(frmConfig);
    lblWindowSize := zglTLabel.CreateDefaults(Gui);
    with lblWindowSize do begin
      Caption := 'Editor window size:'; 
      Name := 'lblWindowSize'; 
      Rect.H := 14.00; 
      Rect.W := 136.00; 
      Rect.X := 7.00; 
      Rect.Y := 8.00; 
    end;
    frmConfig.Items.Add(lblWindowSize);
    cmbWindowSize := zglTComboBox.CreateDefaults(Gui);
    with cmbWindowSize do begin
      Name := 'cmbWindowSize'; 
      Rect.H := 18.00; 
      Rect.W := 135.00; 
      Rect.X := 8.00; 
      Rect.Y := 27.00; 
      Selected := 65535; 
    end;
    frmConfig.Items.Add(cmbWindowSize);
    chkFullScreen := zglTCheckBox.CreateDefaults(Gui);
    with chkFullScreen do begin
      Caption := 'Fullscreen'; 
      Name := 'chkFullScreen'; 
      Rect.H := 18.00; 
      Rect.W := 132.00; 
      Rect.X := 10.00; 
      Rect.Y := 50.00; 
    end;
    frmConfig.Items.Add(chkFullScreen);
    btnSaveConfig := zglTButton.CreateDefaults(Gui);
    with btnSaveConfig do begin
      Caption := 'Save'; 
      Name := 'btnSaveConfig'; 
      OnClick := zglTGuiEventHandler.guiSaveConfig; 
      Rect.H := 25.00; 
      Rect.W := 85.00; 
      Rect.X := 33.00; 
      Rect.Y := 78.00;
    end;
    frmConfig.Items.Add(btnSaveConfig);
  frmEditValue := zglTForm.CreateDefaults(Gui);
  with frmEditValue do begin
    CanMove := True;
    CanResize := True;
    Caption := 'Edit values'; 
    MinHeight := 300.00; 
    MinWidth := 276.00; 
    Name := 'frmEditValue'; 
    Rect.H := 300.00; 
    Rect.W := 276.00; 
    Rect.X := 49.00; 
    Rect.Y := 152.00; 
    Visible := False;
    DisplayEffect := deSlideDown;
  end;
  Gui.Items.Add(frmEditValue);
    btnEditValueSave := zglTButton.CreateDefaults(Gui);
    with btnEditValueSave do begin
      Caption := 'Save'; 
      Name := 'btnEditValueSave'; 
      OnClick := zglTGuiEventHandler.guiSaveItems; 
      Rect.H := 24.00; 
      Rect.W := 57.00; 
      Rect.X := 205.00; 
      Rect.Y := 247.00; 
      Stretch.STR_BOTTOM := True;
      Stretch.STR_LEFT := True;
      Stretch.STR_RIGHT := True;
      Stretch.STR_TOP := True;
    end;
    frmEditValue.Items.Add(btnEditValueSave);
    edtEditCurrentValue := zglTEdit.CreateDefaults(Gui);
    with edtEditCurrentValue do begin
      Caption := ''; 
      Enabled := False;
      Name := 'edtEditCurrentValue'; 
      OnChange := zglTGuiEventHandler.guiEditItemChange; 
      Rect.H := 20.00; 
      Rect.W := 260.00; 
      Rect.X := 1.00; 
      Rect.Y := 6.00; 
      Stretch.STR_RIGHT := True;
    end;
    frmEditValue.Items.Add(edtEditCurrentValue);
    lstEditItems := zglTList.CreateDefaults(Gui);
    with lstEditItems do begin
      Name := 'lstEditItems'; 
      OnSelectItem := zglTGuiEventHandler.guiSelectEditItem; 
      Rect.H := 212.00; 
      Rect.W := 261.00; 
      Rect.X := 1.00; 
      Rect.Y := 30.00; 
      Stretch.STR_BOTTOM := True;
      Stretch.STR_RIGHT := True;
    end;
    frmEditValue.Items.Add(lstEditItems);
    btnAddEditItem := zglTButton.CreateDefaults(Gui);
    with btnAddEditItem do begin
      Caption := '+'; 
      Name := 'btnAddEditItem'; 
      OnClick := zglTGuiEventHandler.guiNewEditItem; 
      Rect.H := 24.00; 
      Rect.W := 24.00; 
      Rect.X := 2.00; 
      Rect.Y := 248.00; 
      Stretch.STR_BOTTOM := True;
      Stretch.STR_TOP := True;
    end;
    frmEditValue.Items.Add(btnAddEditItem);
    btnDeleteEditItem := zglTButton.CreateDefaults(Gui);
    with btnDeleteEditItem do begin
      Caption := '-'; 
      Enabled := False;
      Name := 'btnDeleteEditItem'; 
      OnClick := zglTGuiEventHandler.guiDeleteEditItem; 
      Rect.H := 24.00; 
      Rect.W := 24.00; 
      Rect.X := 28.00; 
      Rect.Y := 247.00; 
      Stretch.STR_BOTTOM := True;
      Stretch.STR_TOP := True;
    end;
    frmEditValue.Items.Add(btnDeleteEditItem);
    btnUpEditItem := zglTButton.CreateDefaults(Gui);
    with btnUpEditItem do begin
      Caption := 'Up'; 
      Enabled := False;
      Name := 'btnUpEditItem'; 
      OnClick := zglTGuiEventHandler.guiUpEditItem; 
      Rect.H := 24.00; 
      Rect.W := 48.00; 
      Rect.X := 54.00; 
      Rect.Y := 247.00; 
      Stretch.STR_BOTTOM := True;
      Stretch.STR_TOP := True;
    end;
    frmEditValue.Items.Add(btnUpEditItem);
    btnDownEditItem := zglTButton.CreateDefaults(Gui);
    with btnDownEditItem do begin
      Caption := 'Down'; 
      Enabled := False;
      Name := 'btnDownEditItem'; 
      OnClick := zglTGuiEventHandler.guiDownEditItem; 
      Rect.H := 24.00; 
      Rect.W := 48.00; 
      Rect.X := 104.00; 
      Rect.Y := 247.00; 
      Stretch.STR_BOTTOM := True;
      Stretch.STR_TOP := True;
    end;
    frmEditValue.Items.Add(btnDownEditItem);
    btnEditCancel := zglTButton.CreateDefaults(Gui);
    with btnEditCancel do begin
      Caption := 'Cancel'; 
      Name := 'btnEditCancel'; 
      OnClick := zglTGuiEventHandler.guiEditItemsCancel; 
      Rect.H := 24.00; 
      Rect.W := 51.00; 
      Rect.X := 153.00; 
      Rect.Y := 247.00; 
      Stretch.STR_BOTTOM := True;
      Stretch.STR_LEFT := True;
      Stretch.STR_RIGHT := True;
      Stretch.STR_TOP := True;
    end;
    frmEditValue.Items.Add(btnEditCancel);
  frmConfirmClear := zglTForm.CreateDefaults(Gui);
  with frmConfirmClear do begin
    Caption := 'Your data will be lost'; 
    Name := 'frmConfirmClear'; 
    Rect.H := 96.00; 
    Rect.W := 233.00; 
    Rect.X := 85.00; 
    Rect.Y := 134.00; 
    Visible := False;
    DisplayEffect := deZoomIn;
  end;
  Gui.Items.Add(frmConfirmClear);
    lblDataWarning := zglTLabel.CreateDefaults(Gui);
    with lblDataWarning do begin
      Caption := 'If you do this, unsaved data will be lost. Are you sure?'; 
      Name := 'lblDataWarning'; 
      Rect.H := 31.00; 
      Rect.W := 216.00; 
      Rect.X := 3.00; 
      Rect.Y := 5.00; 
    end;
    frmConfirmClear.Items.Add(lblDataWarning);
    btnClearYes := zglTButton.CreateDefaults(Gui);
    with btnClearYes do begin
      Caption := 'Yes'; 
      Name := 'btnClearYes'; 
      OnClick := zglTGuiEventHandler.guiClearYes; 
      Rect.H := 26.00; 
      Rect.W := 103.00; 
      Rect.X := 3.00; 
      Rect.Y := 45.00; 
    end;
    frmConfirmClear.Items.Add(btnClearYes);
    btnClearNo := zglTButton.CreateDefaults(Gui);
    with btnClearNo do begin
      Caption := 'No'; 
      Name := 'btnClearNo'; 
      OnClick := zglTGuiEventHandler.guiClearNo; 
      Rect.H := 26.00; 
      Rect.W := 108.00; 
      Rect.X := 108.00; 
      Rect.Y := 45.00; 
    end;
    frmConfirmClear.Items.Add(btnClearNo);
  frmInformation := zglTForm.CreateDefaults(Gui);
  with frmInformation do begin
    Caption := 'Information'; 
    Name := 'frmInformation'; 
    Rect.H := 121.00; 
    Rect.W := 226.00; 
    Rect.X := 94.00; 
    Rect.Y := 83.00;
    Visible := False;
    DisplayEffect := deZoomIn;
  end;
  Gui.Items.Add(frmInformation);
    lblInformationTitle := zglTLabel.CreateDefaults(Gui);
    with lblInformationTitle do begin
      Caption := 'You must set the path to the executable file to make the editor correctly set a relative path.'; 
      Name := 'lblInformationTitle'; 
      Rect.H := 60.00; 
      Rect.W := 209.00; 
      Rect.X := 3.00; 
      Rect.Y := 5.00; 
    end;
    frmInformation.Items.Add(lblInformationTitle);
    btnInformBrowse := zglTButton.CreateDefaults(Gui);
    with btnInformBrowse do begin
      Caption := 'Browse ...'; 
      Name := 'btnInformBrowse'; 
      OnClick := zglTGuiEventHandler.guiInformBrowse; 
      Rect.H := 24.00; 
      Rect.W := 99.00; 
      Rect.X := 2.00; 
      Rect.Y := 71.00; 
    end;
    frmInformation.Items.Add(btnInformBrowse);
    btnInformClose := zglTButton.CreateDefaults(Gui);
    with btnInformClose do begin
      Caption := 'Close'; 
      Name := 'btnInformClose'; 
      OnClick := zglTGuiEventHandler.guiInformClose; 
      Rect.H := 24.00; 
      Rect.W := 106.00; 
      Rect.X := 104.00; 
      Rect.Y := 71.00; 
    end;
    frmInformation.Items.Add(btnInformClose);
  {END}
end;

end.
