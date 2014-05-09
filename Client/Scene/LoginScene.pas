unit LoginScene;

interface
uses MondoZenGl,MZGui,Texture,GfxFont,Vcl.Graphics,GameScene;
type
  TLoginScene = class(TGameScene)
  private
    //Gui组件
    frm_LoginForm                :TGuiForm;
      btn_LoginOk                :TGuiButton;
      btn_Cls                    :TGuiButton;
      edt_ID                     :TGuiEdit;
      edt_Password               :TGuiEdit;
      btn_NewAccount             :TGuiButton;
      btn_ChangePw               :TGuiButton;
    frm_ChangePw                 :TGuiForm;

    m_TmpTexture                   :TTexture;
    m_bCanDraw                   :Boolean;
    procedure NewAccountClick(key: TKeyStates; X, Y: integer);
    { private declarations }
  protected
    procedure Startup; override;
    procedure Shutdown; override;
    procedure RenderFrame; override;
    { protected declarations }
  public

    { public declarations }

  end;
implementation
uses
ResManager,DrawEx,SoundEngine,Share;



{ TLoginScene }
procedure TLoginScene.RenderFrame;

begin
  inherited;
  m_TmpTexture := TResManager.GetInstance.GetOtherTexture(CHRSEL,22);
  DrawTexture2Canvas(Canvas,m_TmpTexture.m_Texture,0,0);
  TGuiManager.GetInstance.Draw;
end;

procedure TLoginScene.Shutdown;
begin
  inherited;
 TGuiManager.GetInstance.ResetToScene(nil);
end;

procedure TLoginScene.Startup;
begin
  m_bCanDraw := False;
  TGuiManager.GetInstance.ResetToScene(Self);
  frm_LoginForm := TGuiForm.Create(TResManager.GetInstance.GetPrguseTexture(1,60,False));
  With frm_LoginForm do
  begin
    Rect.x  :=  252;
    Rect.y  := 165;
    CanMove := False;
    Caption := '登陆窗口';
  end;
  TGuiManager.GetInstance.Add(frm_LoginForm);
  With  TGuiManager.GetInstance do
  begin
    Rect.x  := 0;
    Rect.Y  := 0;
    Rect.W  := 800;
    Rect.H  := 600;
    Caption := 'GUI管理器';
  end;
  btn_LoginOk := TGuiButton.Create;
  with btn_LoginOk do
  begin
    Rect.X         := 170;
    Rect.Y         := 164;
    TexturePressed := TResManager.GetInstance.GetPrguseTexture(1,62,False);
    Rect.W         := TexturePressed.m_Texture.Width;
    Rect.H         := TexturePressed.m_Texture.Height;
    Caption        := '提交按钮';
  end;
  frm_LoginForm.Add(btn_LoginOk);
  edt_ID := TGuiEdit.Create;
  with edt_ID do
  begin
    Rect.X    := 97;
    Rect.Y    := 83;
    Rect.W    := 140;
    Rect.H    := 20;
    Caption   := '账号编辑框';
    MaxLength := 15;
    Font      := g_MainFont;
  end;
  frm_LoginForm.Add(edt_ID);
  edt_Password := TGuiEdit.Create;
  With edt_Password do
  begin
    Rect.x          := 97;
    Rect.Y          := 115;
    Rect.W          := 140;
    Rect.H          := 20;
    Caption         := '密码编辑框';
    Font            := g_MainFont;
    MaxLength       := 20;
    isInPutPassWord := True;
  end;
  frm_LoginForm.Add(edt_Password);
  btn_Cls := TGuiButton.Create;
  with btn_Cls do
  begin
    TexturePressed := TResManager.GetInstance.GetPrguseTexture(1,64,False);
   //TextureNormal := TexturePressed;
    Rect.X         := 252;
    Rect.Y         := 28;
    Rect.W         := TexturePressed.m_Texture.Width;
    Rect.H         := TexturePressed.m_Texture.Height;
    Caption        := '关闭按钮';
  end;
  frm_LoginForm.Add(btn_Cls);
  btn_NewAccount := TGuiButton.Create;
  with btn_NewAccount do
  begin
    Rect.X            := 25;
    Rect.Y            := 207;
    TexturePressed    := TResManager.GetInstance.GetPrguseTexture(1,61,False);
    //TextureNormal   := TexturePressed;
    Rect.W            := TexturePressed.m_Texture.Width;
    Rect.H            := TexturePressed.m_Texture.Height;
    Caption           := '新用户按钮';
    OnClick           := NewAccountClick;
  end;
  frm_LoginForm.Add(btn_NewAccount);
  btn_ChangePw := TGuiButton.Create;
  with btn_ChangePw do
  begin
     Rect.X         := 130;
     Rect.Y         := 207;
     TexturePressed := TResManager.GetInstance.GetPrguseTexture(1,53,False);
     //TextureNormal:= TexturePressed;
     Rect.W         := TexturePressed.m_Texture.Width;
     Rect.H         := TexturePressed.m_Texture.Height;
     Caption        := '修改密码按钮';
  end;
  frm_LoginForm.Add(btn_ChangePw);
  frm_ChangePw := TGuiForm.Create(TResManager.GetInstance.GetPrguseTexture(1,63,False));
  with frm_ChangePw do
  begin
    Rect.X           := 81;
    Rect.Y           := 51;
    Caption          := '新用户窗口';
    Visable           := False;
  end;
  TGuiManager.GetInstance.Add(frm_ChangePw);
  TSoundEngine.GetInstance.RegSound('背景音乐',g_sClientPath+'Wav\Log-in-long2.wav');
  TSoundEngine.GetInstance.PlaySound('背景音乐',True);
  m_bCanDraw := True;
  end;


procedure TLoginScene.NewAccountClick(key: TKeyStates; X, Y: integer);
begin
  frm_LoginForm.Visable := False;
  frm_ChangePw.Visable := True;
end;

end.

