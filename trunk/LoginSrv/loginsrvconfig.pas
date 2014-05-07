unit LoginSrvConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLFile;

type

  { TLoginSrvConfig }

  TLoginSrvConfig = class
    DataBaseIP: string;
    DataBasePort: word;
    DataBaseUser: string;
    DataBasePw: string;
    DataBaseName: string;
    DataBaseTable: string;
    ListenPort: word;
    ServerKey: string;
    MaxLoginGateCount: word;
    LoginSrvKey:String;
    QueryInterval:integer; //在查询线程内的间隔
    constructor Create(FileName: string);
    destructor Destroy; override;
    procedure WriteConfig;
    procedure ReadConfig;

  private
    XML: TXmlFile;
  end;

implementation

{ TLoginSrvConfig }

constructor TLoginSrvConfig.Create(FileName: string);
begin
  inherited Create;
  XML := TXMLFile.Create(FileName, 'config', False);
end;

destructor TLoginSrvConfig.Destroy;
begin
  XML.Free;
  inherited Destroy;
end;

procedure TLoginSrvConfig.WriteConfig;
begin
  XML.WriteString('DataBase', 'IP', DataBaseIP);
  XML.WriteInteger('DataBase', 'Port', DataBasePort);
  XML.Writestring('DataBase', 'User', DataBaseUser);
  XML.WriteString('DataBase', 'Password', DataBasePw);
  XML.Writestring('DataBase', 'DBName', DataBaseName);
  XML.WriteInteger('Server', 'ListenPort', ListenPort);
  XML.WriteString('Server', 'ServerKey', Serverkey);
  XML.WriteInteger('Server', 'MaxLoginGateCount', MaxLoginGateCount);
  XML.WriteInteger('Server', 'QueryInterval', QueryInterval);

end;

procedure TLoginSrvConfig.ReadConfig;
begin
  DataBaseIP := XML.ReadString('DataBase', 'IP', '127.0.0.1');
  DataBasePort := XML.ReadInteger('DataBase', 'Port', 3050);
  DataBaseUser := XML.Readstring('DataBase', 'User', 'SYSDBA');
  DataBasePw := XML.ReadString('DataBase', 'Password', 'masterkey');
  DataBaseName := XML.readstring('DataBase', 'DBName', 'C:\Account.fdb');
  ListenPort := XML.ReadInteger('Server', 'ListenPort', 7000);
  ServerKey := XML.ReadString('Server', 'ServerKey', 'onmyway');
  MaxLoginGateCount := XML.ReadInteger('Server', 'MaxLoginGateCount', 8);
  QueryInterval:=XML.ReadInteger('Server', 'QueryInterval', 10);
end;

end.
