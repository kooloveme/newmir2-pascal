unit ufrmDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  Menus, ZDataset, ZStoredProcedure, DB;

type

  { TfrmDB }

  TfrmDB = class(TForm)
    Datasource1: TDatasource;
    DBGrid1: TDBGrid;
    Mi_QACCOUNT: TMenuItem;
    MI_DB: TMenuItem;
    MI_LOAD: TMenuItem;
    M_DB: TMainMenu;
    procedure FormCreate(Sender: TObject);
    procedure MI_LOADClick(Sender: TObject);
    procedure Mi_QACCOUNTClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmDB: TfrmDB;

implementation

uses Main, ufrmconfig;

{$R *.lfm}

{ TfrmDB }

procedure TfrmDB.FormCreate(Sender: TObject);
begin
  DBGrid1.Align := alClient;

end;

procedure TfrmDB.MI_LOADClick(Sender: TObject);
begin
  main.MainForm.ZQuery1.SQL.Clear;
  main.MainForm.ZQuery1.SQL.add('SELECT * FROM tb_account');
  try
    main.MainForm.ZQuery1.Open();
  finally
  end;
end;

procedure TfrmDB.Mi_QACCOUNTClick(Sender: TObject);
begin

end;

end.
