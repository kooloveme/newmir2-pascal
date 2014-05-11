unit Guidesign;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Samples.Spin,MZGui;
Const
GUILabelCaption='控件描述:';
type
  TfrmGuiDesgin = class(TForm)
    btn_loadGuiList: TButton;
    se_X: TSpinEdit;
    se_Y: TSpinEdit;
    btn_ShowHide: TButton;
    grp_GUi: TGroupBox;
    lbl1: TLabel;
    lbl2: TLabel;
    btn_update: TButton;
    TReeV: TTreeView;
    procedure btn_loadGuiListClick(Sender: TObject);
    procedure TreeVChange(Sender: TObject; Node: TTreeNode);
    Procedure AddChildList(Node: TTreeNode; Gui: TGuiObject);
    procedure RefreshGui(X,Y:integer);
    procedure btn_ShowHideClick(Sender: TObject);
    procedure btn_updateClick(Sender: TObject);
    procedure se_XChange(Sender: TObject);
    procedure se_YChange(Sender: TObject);
  private
    FocusGui:TGuiObject;
    FocusGUIVisable:Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmGuiDesgin: TfrmGuiDesgin;

implementation

{$R *.dfm}

procedure TfrmGuiDesgin.AddChildList(Node: TTreeNode; Gui: TGuiObject);
var
I:Integer;
G:TGuiObject;
Nodes:TTreeNodes;
begin
  Nodes:=TreeV.Items;
  for I := 0 to Gui.SubGuiObjects.Count -1  do
    begin
      G:=Gui.SubGuiObjects[i];
      Nodes.AddChildObject(Node,G.Caption,G);
      if G.SubGuiObjects.Count > 0 then AddChildList(Node,G);
    end;

end;

procedure TfrmGuiDesgin.btn_loadGuiListClick(Sender: TObject);
var
i:Integer;
Nodes:TTreeNodes;
Node:TTreeNode;
GUI:TGuiObject;
begin
TreeV.Items.Clear;
Nodes:=TreeV.Items;
//遍历GUI列表
 { GUI:=TGuiManager.GetInstance.SubGuiObjects[0];
  Nodes.AddObject(nil,GUI.Caption,Gui);
  if GUI.SubGuiObjects.Count > 0 then
  begin
  AddChildList(Nodes.Item[0],GUI);
  end;}
for i := 0 to TGuiManager.GetInstance.Count-1 do
begin
  GUI:=TGuiManager.GetInstance.SubGuiObjects[i];
  Nodes.AddObject(nil,GUI.Caption,Gui);
  if GUI.SubGuiObjects.Count > 0 then
  begin
  AddChildList(Nodes.Item[i],GUI);
  end;

end;
end;

procedure TfrmGuiDesgin.btn_ShowHideClick(Sender: TObject);
begin
if not (Sender=TreeV) then FocusGUIVisable:=not FocusGUIVisable;
if FocusGUIVisable then btn_ShowHide.Caption:='隐藏此GUI'
                   else btn_ShowHide.Caption:='显示此GUI';
FocusGui.Visable:=FocusGUIVisable;
//if not (Sender=TreeV) then RefreshGui;

//这里使用Sender是因为 如果是选中列表我不需要更改控件状态和 刷新控件状态
end;

procedure TfrmGuiDesgin.btn_updateClick(Sender: TObject);
begin
FocusGui.Rect.X:=StrToInt(se_X.Text);
FocusGui.Rect.Y:=StrToInt(se_Y.Text);
end;

procedure TfrmGuiDesgin.RefreshGui(X,Y:integer);
begin
FocusGui.Visable:=FocusGUIVisable;
end;



procedure TfrmGuiDesgin.se_XChange(Sender: TObject);
begin
FocusGui.Rect.X:=StrToInt(se_X.Text);
end;

procedure TfrmGuiDesgin.se_YChange(Sender: TObject);
begin
FocusGui.Rect.Y:=StrToInt(se_Y.Text);
end;

procedure TfrmGuiDesgin.TreeVChange(Sender: TObject; Node: TTreeNode);
begin
lbl1.Caption:=GUILabelCaption+TGuiObject(node.Data).Caption;
FocusGui:=node.Data;
se_X.Text:=IntToStr(Trunc(FocusGui.Rect.X));
se_Y.Text:=IntToStr(TRunc(FocusGui.Rect.y));
FocusGUIVisable:=FocusGui.Visable;
btn_ShowHideClick(TreeV);
end;

end.
