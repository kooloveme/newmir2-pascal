unit ufrmConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ValEdit, ExtCtrls, LoginSrvConfig;

type

  { TFrmConfig }

  TFrmConfig = class(TForm)

    btn_Ok: TButton;
    Label1: TLabel;
    Label2: TLabel;
    mm_Option: TMemo;
    mm_Values: TMemo;
    procedure btn_OkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public

    { public declarations }
  end;

var
  FrmConfig: TFrmConfig;
  //服务器状�?
  gDBConnected: boolean = False;
  gServerReady: boolean = False;

implementation
uses
  main;
{$R *.lfm}

{ TFrmConfig }

procedure TFrmConfig.FormCreate(Sender: TObject);
begin

end;

procedure TFrmConfig.btn_OkClick(Sender: TObject);
begin
  with main.MainForm.Config do
  begin
    DataBaseIP := Trim(mm_Values.Lines[0]);
    DataBasePort := StrToInt(Trim(mm_Values.Lines[1]));
    DataBaseUser := Trim(mm_Values.Lines[2]);
    DataBasePw := Trim(mm_Values.Lines[3]);
    DataBaseName := Trim(mm_Values.Lines[4]);
    ListenPort := StrToInt(Trim(mm_Values.Lines[5]));
    ServerKey := Trim(mm_Values.Lines[6]);
    MaxLoginGateCount := StrToInt(Trim(mm_Values.Lines[7]));
    QueryInterval:=StrToInt(Trim(mm_Values.Lines[8]));
    WriteConfig;
  end;
  self.Hide;
end;

procedure TFrmConfig.FormDestroy(Sender: TObject);
begin
end;

procedure TFrmConfig.FormShow(Sender: TObject);
begin
  with main.MainForm.Config do
  begin
    mm_Values.Clear;
    mm_Values.Append(DataBaseIP);
    mm_Values.Append(IntToStr(DataBasePort));
    mm_Values.Append(DataBaseUser);
    mm_Values.Append(DataBasePw);
    mm_Values.Append(DataBaseName);
    mm_Values.Append(IntToStr(ListenPort));
    mm_Values.Append(ServerKey);
    mm_Values.Append(IntToStr(MaxLoginGateCount));
    mm_Values.Append(IntToStr(QueryInterval));
  end;
end;

end.
