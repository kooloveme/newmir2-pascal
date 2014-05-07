program LoginSrv;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  lnetvisual,
  Main,
  zcomponent,
  ufrmConfig,
  LoginSrvConfig, ufrmDB, AccountDB, LoginServer;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TfrmConfig, frmConfig);
  Application.CreateForm(TfrmDB, frmDB);
  Application.Run;
end.
