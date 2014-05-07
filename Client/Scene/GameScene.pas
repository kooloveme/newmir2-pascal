unit GameScene;

interface
uses
MondoZenGl,MZGui;
type
    TGameScene=Class(TMZScene)
    Protected
      Class Var ClickDown:Boolean;
      Class Function GetKeyStates:TKeyStates;
      procedure MouseDown(const Button: TMZMouseButton); override;
      procedure MouseMove(const X: Integer; const Y: Integer); override;
      procedure MouseUp(const Button: TMZMouseButton); override;
      procedure KeyDown(const KeyCode: TMZKeyCode); override;
      procedure KeyUp(const KeyCode: TMZKeyCode); override;
      procedure Update(const DeltaTimeMs: Double); override;
    End;

implementation

{ TGameScene }

class function TGameScene.GetKeyStates: TKeyStates;
begin
  Result:=[];
  if TMZKeyboard.IsKeyPressed(kcAlt) then Result:=Result+[kAlt];
  if TMZKeyboard.IsKeyPressed(kcCtrl) then Result:=Result+[kCtrl];
  if TMZKeyboard.IsKeyPressed(kcShift) then Result:=Result+[kShift];
  if TMZMouse.IsButtonDown(mbLeft) then Result:=Result+[mLeft];
  if TMZMouse.IsButtonDown(mbMiddle) then result:=Result+[mMiddle];
  if TMZMouse.IsButtonDown(mbRight) then Result:=Result+[mRight];
end;

procedure TGameScene.KeyDown(const KeyCode: TMZKeyCode);
begin
  inherited;
  TGuiManager.GetInstance.KeyDown(GetkeyStates,keyCode);
end;

procedure TGameScene.KeyUp(const KeyCode: TMZKeyCode);
begin
  inherited;
  TGuiManager.GetInstance.KeyUp(GetkeyStates,KeyCode);
end;

procedure TGameScene.MouseDown(const Button: TMZMouseButton);
var
X,Y:integer;
begin
inherited;
TMZMouse.GetPos(x,y);
TGuiManager.GetInstance.MouseDown(GetkeyStates,TMZMouseButton(Button),x,y);
if TMZMouseButton(Button) =  mbLeft then ClickDown:=True;
end;

procedure TGameScene.MouseMove(const X, Y: Integer);
begin
  inherited;
  TGuiManager.GetInstance.MouseMove(GetkeyStates,X,Y);
end;

procedure TGameScene.MouseUp(const Button: TMZMouseButton);
var
X,Y:Integer;
begin
inherited;
TMZMouse.GetPos(X,Y);
TGuiManager.GetInstance.MouseUp(GetKeyStates,TMZMouseButton(Button),x,y);
if ClickDown then
begin
  if TMZMouseButton(Button) =mbLeft then
  begin
  TGuiManager.GetInstance.Click(GetKeyStates,x,y);
  ClickDown:=False;
  end;
end;
end;

procedure TGameScene.Update(const DeltaTimeMs: Double);
begin
  inherited;
  TGuiManager.GetInstance.Update(DeltaTimeMs);
end;

end.
