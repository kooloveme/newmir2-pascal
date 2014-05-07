unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, StdCtrls, ZConnection, ZDataset, ZStoredProcedure,LoginServer,LoginSrvConfig;

type

  { TMainForm }

  TMainForm = class(TForm)
    LogMemo: TMemo;
    Mi_Account: TMenuItem;
    Mi_DB: TMenuItem;
    MI_Option: TMenuItem;
    MI_Config: TMenuItem;
    MI_StopServer: TMenuItem;
    Mitem_Server: TMenuItem;
    MI_ServerStart: TMenuItem;
    MMenu: TMainMenu;
    stat: TStatusBar;
    ZConnection1: TZConnection;
    ZQuery1: TZQuery;
    ZStoredProc1: TZStoredProc;
    procedure FormCreate(Sender: TObject);
    procedure Mitem_ServerClick(Sender: TObject);
    procedure Mi_AccountClick(Sender: TObject);
    procedure MI_ConfigClick(Sender: TObject);
    procedure MI_OptionClick(Sender: TObject);
    procedure MI_ServerStartClick(Sender: TObject);
    procedure MI_StopServerClick(Sender: TObject);
    procedure ZConnection1AfterConnect(Sender: TObject);
    procedure ZConnection1AfterDisconnect(Sender: TObject);
    procedure ZConnection1BeforeConnect(Sender: TObject);
  private
    Server:TLoginSrv;
    { private declarations }
  public
    Config: TLoginSrvConfig;
    { public declarations }
  end;

procedure MainOut(Msg: string);

var
  MainForm: TMainForm;

implementation
uses ufrmdb,ufrmConfig;
{$R *.lfm}

{ TMainForm }

procedure TMainForm.Mitem_ServerClick(Sender: TObject);
begin

end;

procedure TMainForm.Mi_AccountClick(Sender: TObject);
begin
  frmDB.Show;
end;

procedure TMainForm.MI_ConfigClick(Sender: TObject);
begin

end;

procedure TMainForm.MI_OptionClick(Sender: TObject);
begin
  FrmConfig.Show;
end;

procedure TMainForm.MI_ServerStartClick(Sender: TObject);
var
  s: string;
begin
  with ufrmConfig.FrmConfig do
  begin
    ZConnection1.HostName := Config.DataBaseIP;
    ZConnection1.Port := Config.DataBasePort;
    Zconnection1.User := Config.DataBaseUser;
    Zconnection1.Password := Config.DataBasePw;
    Zconnection1.Database := Config.DataBaseName;
  end;
  Zconnection1.Connected := True;

  Server.StartServer;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  Logmemo.Color:=clblack;
  Logmemo.font.Color:=clLime;
  Config := TLoginSrvConfig.Create(ExtractFilePath(ParamStr(0)) + 'config.xml');
  Config.ReadConfig;
  Caption := Formatdatetime('YYYY/MM/DD HH:NN', now());
  Server:=TLoginSrv.create(config.ListenPort);

end;

procedure TMainForm.MI_StopServerClick(Sender: TObject);
begin
  ZConnection1.Connected:=False;
  Server.StopServer;
end;

procedure TMainForm.ZConnection1AfterConnect(Sender: TObject);
begin
  MainOut('数据库连接成功...');
end;

procedure TMainForm.ZConnection1AfterDisconnect(Sender: TObject);
begin
  mainout('已断开数据库连接...');
end;

procedure TMainForm.ZConnection1BeforeConnect(Sender: TObject);
begin
  MainOut('正在尝试连接数据库...');
end;

procedure MainOut(Msg: string);
var
  s: string;
begin
  s := FormatDateTime('M/D-HH:MM:SS', Now());
  s := s + '     ' + msg;
  MainForm.LogMemo.Append(s);
end;

end.
