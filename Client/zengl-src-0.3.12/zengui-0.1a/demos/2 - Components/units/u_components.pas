unit u_components;

// PLEASE DO NOT REMOVE THE {*} COMMENTS, YOU CAN LOOSE YOU SOURCE!

interface

// Store here uses or proc definition
{INTERFACE}
uses zglGui, zgl_main;
{END}

var
  MainForm: zglTForm;
  SomeButton: zglTButton;
  SomeImageButton: zglTImageButton;
  SomeImage: zglTImage;
  SomeCheckBox: zglTCheckBox;
  SomeComboBox: zglTComboBox;
  SomeEdit: zglTEdit;
  SomeLabel: zglTLabel;
  SomeProgressBar: zglTProgressBar;
  SomeTrackBar: zglTTrackBar;
  SomeList: zglTList;
  SomeOtherLabel: zglTLabel;
  SomeRadioBox: zglTRadioBox;
  RadioButton1: zglTRadioButton;
  RadioButton2: zglTRadioButton;
  PageControl: zglTPageControl;
  TableSheet: zglTPageControlSheet;
  SomeTable: zglTTable;
  LolSheet: zglTPageControlSheet;
  SomeOtherButton: zglTButton;
  SomeMultiButton: zglTMultiButton;
  btnZoomIn: zglTButton;
  btnFadeIn: zglTButton;
  NonameLabel24: zglTLabel;
  btnSlideDown: zglTButton;
  oherFont: zglTFontContainer;

procedure InitGui(gui: zglTGui);

type
  zglTGuiEventHandler = class
    class procedure onClickMe(Sender: zglTGUIObject; X, Y: integer);
    class procedure progressChange(Sender: zglTGUIObject);
    class procedure showEffect(Sender: zglTGUIObject; X, Y: integer);
  end;

{WORKINGDIR=D:\work\svn\gui\bin}

implementation

// Store here recursive uses or proc implementation
{IMPLEMENTATION}

{END}

class procedure zglTGuiEventHandler.onClickMe(Sender: zglTGUIObject; X, Y: integer);
{EVENT onClickMe:zglTMouseEvent}
begin
  zgl_exit;
end;
{END}

class procedure zglTGuiEventHandler.progressChange(Sender: zglTGUIObject);
{EVENT progressChange:zglTEvent}
begin
  SomeProgressBar.Progress := 100 - zglTTrackBar(Sender).Progress;
end;
{END}

class procedure zglTGuiEventHandler.showEffect(Sender: zglTGUIObject; X, Y: integer);
{EVENT showEffect:zglTMouseEvent}
begin
  if Sender.Caption = 'Zoom In' then
    Sender.Gui.ShowMessage('Hello', 'I am effected window by Zoom In!', deZoomIn);

  if Sender.Caption = 'Fade In' then
    Sender.Gui.ShowMessage('Hello', 'I am effected window by Fade In!', deFadeIn);

  if Sender.Caption = 'Slide Down' then
    Sender.Gui.ShowMessage('Hello', 'I am effected window by Slide Down!', deSlideDown);
end;
{END}

procedure InitGui(gui: zglTGui);
begin
  {INIT}
  oherFont := zglTFontContainer.CreateDefaults(Gui);
  with oherFont do begin
    Active.Color := 16711680;
    Disabled.Alpha := 93;
    FileName := 'Data/Other.zfi';
    Name := 'oherFont';
  end;
  MainForm := zglTForm.CreateDefaults(Gui); {ROOT}
  with MainForm do begin
    CanMove := True;
    CanResize := True;
    CloseButton:=True;
    Caption := 'Some Form';
    Name := 'MainForm';
    Rect.H := 468.00;
    Rect.W := 474.00;
    Rect.X := 71.00;
    Rect.Y := 34.00;
  end;
  Gui.Items.Add(MainForm);
    SomeButton := zglTButton.CreateDefaults(Gui);
    with SomeButton do begin
      Caption := 'I am button';
      Name := 'SomeButton';
      Rect.H := 38.00;
      Rect.W := 139.00;
      Rect.X := 7.00;
      Rect.Y := 9.00;
    end;
    MainForm.Items.Add(SomeButton);
    SomeImageButton := zglTImageButton.CreateDefaults(Gui);
    with SomeImageButton do begin
      Name := 'SomeImageButton';
      Rect.H := 32.00;
      Rect.W := 128.00;
      Rect.X := 13.00;
      Rect.Y := 56.00;
      Texture.FileName := 'media/imagebutton.png';
      Texture.FrameOffset := 1;
      Texture.TexHeight := 32;
      Texture.TexWidth := 128;
    end;
    MainForm.Items.Add(SomeImageButton);
    SomeImage := zglTImage.CreateDefaults(Gui);
    with SomeImage do begin
      Name := 'SomeImage';
      Rect.H := 128.00;
      Rect.W := 128.00;
      Rect.X := 159.00;
      Rect.Y := 11.00;
      Align:=caClient;
      Texture.FileName := 'media/pushkin.png';
     // Texture.FileName := 'D:/11.png';
      Texture.TexHeight := 128;
      Texture.TexWidth := 128;
    end;
    MainForm.Items.Add(SomeImage);

    SomeCheckBox := zglTCheckBox.CreateDefaults(Gui);
    with SomeCheckBox do begin
      Caption := 'Check me!';
      Name := 'SomeCheckBox';
      Rect.H := 22.00;
      Rect.W := 129.00;
      Rect.X := 12.00;
      Rect.Y := 97.00;
    end;
    MainForm.Items.Add(SomeCheckBox);
    SomeComboBox := zglTComboBox.CreateDefaults(Gui);
    with SomeComboBox do begin
      Caption := 'First';
      Items.Add('First');
      Items.Add('Second');
      Items.Add('Third');
      Name := 'SomeComboBox';
      Rect.H := 23.00;
      Rect.W := 138.00;
      Rect.X := 7.00;
      Rect.Y := 127.00;
    end;
    MainForm.Items.Add(SomeComboBox);
    SomeEdit := zglTEdit.CreateDefaults(Gui);
    with SomeEdit do begin
      Caption := 'Edit me';
      Name := 'SomeEdit';
      Rect.H := 22.00;
      Rect.W := 137.00;
      Rect.X := 295.00;
      Rect.Y := 12.00;
    end;
    MainForm.Items.Add(SomeEdit);
    SomeLabel := zglTLabel.CreateDefaults(Gui);
    with SomeLabel do begin
      Caption := '<- image (i am label)';
      Name := 'SomeLabel';
      Rect.H := 22.00;
      Rect.W := 159.00;
      Rect.X := 297.00;
      Rect.Y := 42.00;

    end;
    MainForm.Items.Add(SomeLabel);
    SomeProgressBar := zglTProgressBar.CreateDefaults(Gui);
    with SomeProgressBar do begin
      Name := 'SomeProgressBar';
      Progress := 20;
      Rect.H := 18.00;
      Rect.W := 126.00;
      Rect.X := 160.00;
      Rect.Y := 144.00;
    end;
    MainForm.Items.Add(SomeProgressBar);
    SomeTrackBar := zglTTrackBar.CreateDefaults(Gui);
    with SomeTrackBar do begin
      Name := 'SomeTrackBar';
      OnChange := zglTGuiEventHandler.progressChange;
      Progress := 80;
      Rect.H := 15.00;
      Rect.W := 125.00;
      Rect.X := 160.00;
      Rect.Y := 167.00;
    end;
    MainForm.Items.Add(SomeTrackBar);
    SomeList := zglTList.CreateDefaults(Gui);
    with SomeList do begin
      Name := 'SomeList';
      Rect.H := 131.00;
      Rect.W := 136.00;
      Rect.X := 8.00;
      Rect.Y := 157.00;
    end;
    MainForm.Items.Add(SomeList);
    SomeOtherLabel := zglTLabel.CreateDefaults(Gui);
    with SomeOtherLabel do begin
      Caption := '<- Sorry, but lists, tables and some other components cannot be edited in editor, only by code :(';
      Font := oherFont;
      Name := 'SomeOtherLabel';
      Rect.H := 74.00;
      Rect.W := 247.00;
      Rect.X := 159.00;
      Rect.Y := 194.00;
    end;
    MainForm.Items.Add(SomeOtherLabel);
    SomeRadioBox := zglTRadioBox.CreateDefaults(Gui);
    with SomeRadioBox do begin
      Name := 'SomeRadioBox';
      Rect.H := 56.00;
      Rect.W := 134.00;
      Rect.X := 9.00;
      Rect.Y := 296.00;
    end;
    MainForm.Items.Add(SomeRadioBox);
      RadioButton1 := zglTRadioButton.CreateDefaults(Gui);
      with RadioButton1 do begin
        Caption := 'First';
        Name := 'RadioButton1';
        Rect.H := 19.00;
        Rect.W := 104.00;
        Rect.X := 8.00;
        Rect.Y := 5.00;
      end;
      SomeRadioBox.Items.Add(RadioButton1);
      RadioButton2 := zglTRadioButton.CreateDefaults(Gui);
      with RadioButton2 do begin
        Caption := 'Second';
        Name := 'RadioButton2';
        Rect.H := 21.00;
        Rect.W := 104.00;
        Rect.X := 8.00;
        Rect.Y := 27.00;
      end;
      SomeRadioBox.Items.Add(RadioButton2);
    PageControl := zglTPageControl.CreateDefaults(Gui);
    with PageControl do begin
      Name := 'PageControl';
      Rect.H := 140.00;
      Rect.W := 300.00;
      Rect.X := 159.00;
      Rect.Y := 271.00;
      Selected := 1;
    end;
    MainForm.Items.Add(PageControl);
      TableSheet := zglTPageControlSheet.CreateDefaults(Gui);
      with TableSheet do begin
        Caption := 'Table';
        Name := 'TableSheet';
        Rect.H := 112.00;
        Rect.W := 296.00;
        Rect.X := 2.00;
        Rect.Y := 26.00;
      end;
      PageControl.Items.Add(TableSheet);
        SomeTable := zglTTable.CreateDefaults(Gui);
        with SomeTable do begin
          Align := caClient;
          DrawGrid := True;
          Name := 'SomeTable';
          Rect.H := 112.00;
          Rect.W := 296.00;
          Rect.X := 0.00;
          Rect.Y := 0.00;
        end;
        TableSheet.Items.Add(SomeTable);
      LolSheet := zglTPageControlSheet.CreateDefaults(Gui);
      with LolSheet do begin
        Caption := 'Lol';
        Name := 'LolSheet';
        Rect.H := 112.00;
        Rect.W := 296.00;
        Rect.X := 2.00;
        Rect.Y := 26.00;
      end;
      PageControl.Items.Add(LolSheet);
        SomeOtherButton := zglTButton.CreateDefaults(Gui);
        with SomeOtherButton do begin
          Caption := 'Click me to close demo';
          Name := 'SomeOtherButton';
          OnClick := zglTGuiEventHandler.onClickMe;
          Rect.H := 75.00;
          Rect.W := 177.00;
          Rect.X := 48.00;
          Rect.Y := 32.00;
        end;
        LolSheet.Items.Add(SomeOtherButton);
    SomeMultiButton := zglTMultiButton.CreateDefaults(Gui);
    with SomeMultiButton do begin
      Caption := 'I am multi';
      Name := 'SomeMultiButton';
      Rect.H := 27.00;
      Rect.W := 158.00;
      Rect.X := 298.00;
      Rect.Y := 75.00;
    end;
    MainForm.Items.Add(SomeMultiButton);
    btnZoomIn := zglTButton.CreateDefaults(Gui);
    with btnZoomIn do begin
      Caption := 'Zoom In';
      Name := 'btnZoomIn';
      OnClick := zglTGuiEventHandler.showEffect;
      Rect.H := 24.00;
      Rect.W := 73.00;
      Rect.X := 296.00;
      Rect.Y := 140.00;
    end;
    MainForm.Items.Add(btnZoomIn);
    btnFadeIn := zglTButton.CreateDefaults(Gui);
    with btnFadeIn do begin
      Caption := 'Fade In';
      Name := 'btnFadeIn';
      OnClick := zglTGuiEventHandler.showEffect;
      Rect.H := 24.00;
      Rect.W := 78.00;
      Rect.X := 375.00;
      Rect.Y := 140.00;
    end;
    MainForm.Items.Add(btnFadeIn);
    NonameLabel24 := zglTLabel.CreateDefaults(Gui);
    with NonameLabel24 do begin
      Caption := 'Effects:';
      Rect.H := 18.00;
      Rect.W := 159.00;
      Rect.X := 296.00;
      Rect.Y := 121.00;
    end;
    MainForm.Items.Add(NonameLabel24);
    btnSlideDown := zglTButton.CreateDefaults(Gui);
    with btnSlideDown do begin
      Caption := 'Slide Down';
      Name := 'btnSlideDown';
      OnClick := zglTGuiEventHandler.showEffect;
      Rect.H := 21.00;
      Rect.W := 93.00;
      Rect.X := 296.00;
      Rect.Y := 169.00;
    end;
    MainForm.Items.Add(btnSlideDown);
  {END}
end;

end.
