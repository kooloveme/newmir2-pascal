unit AccountDB;

{$mode objfpc}{$H+}
{the unit depend on ZEOSDBO,此单元依赖与ZEOSDBO组件}
interface

uses
  Classes, SysUtils,ZDataSet,ZStoredProcedure,LNetComponents;
const
  Success='成功';
  Unknow='未知原因';
  QueryError='查询失败,请检查是连接数据库！';
  KeyError='密码错误';
  AccountExist='账号已存在';
  IsFreezed='账号被冻结';
  AccountNone='账号不存在';
  LenError='账号密码长度错误';


type

  TCreateAccount=Record
    Username:String;
    Password:string;
    Name:string;
    Birthday:string;
    IDCode:string;//身份证号码
    QuestionA:String;
    AnswerA:string;
    QuestionB:String;
    AnswerB:string;
    PhoneNum:string;
    MobilePhoneNum:string;
    EMail:string;
  end;
  PCreateAccount=^TCreateAccount;

  { TAccountOperation }

  TAccountOperation=Class
  private
    FReson:String;
    ZProcedure:TZStoredProc;
    Public
    Function VerifyAccount(Const Account:String;Const Password:String):Boolean;
    Function ChangePassword(const Account:string;const password:string;const NewPassword:string):Boolean;
    Function CreateAccount(Info:PCreateAccount):Boolean;
    property Reson:String Read FReson;
    constructor Create(ZProc:TZStoredProc);
  end;

  { TQueryDBThread }

  TQueryDBThread=class(TThread)
  constructor Create(TCP:TLTCPComponent;interval:integer);
  destructor Destroy;override;
  procedure Execute;override;
  private
  LTCP:TLTCPComponent;
  DB:TAccountOperation;
  FInterval:integer;
  Public
  QueryList:TThreadList;
  end;

implementation
uses
  main;
{ TQueryDBThread }

constructor TQueryDBThread.Create(TCP: TLTCPComponent; interval: integer);
begin
  FreeOnTerminate := True; //线程任务结束后立即释放对象
  inherited Create(True);//创建对象后不会立即线程任务
  LTCP:=TCP;
  FInterval:=interval;
  DB:=TAccountOperation.Create(MainForm.ZStoredProc1);
  QueryList:=TThreadList.create;
end;

destructor TQueryDBThread.Destroy;
begin
  DB.free;
  QueryList.free;
  inherited;
end;

procedure TQueryDBThread.Execute;
var
  List:TList;
begin
  while not Terminated  do
     begin
       List:=QueryList.LockList;
       if List.Count > 0 then
       begin
        //解析消息。按要求进行操作。
        List.Delete(0);
       end;
       QueryList.UnlockList;
       sleep(Finterval);
     end;
end;

{ TAccountOperation }

function TAccountOperation.VerifyAccount(const Account: String;
  const Password: String): Boolean;
var
  state:integer;
begin
 result:=False;
 FReson:=Unknow;
 ZProcedure.Close;
 ZProcedure.StoredProcName:='P_VERIFYACCOUNT';
 ZProcedure.Params.ParamByName('UNAME').Value:=Account;
 ZProcedure.Params.ParamByName('PW').Value:=Password;
 try
 ZProcedure.ExecProc;
 state:=ZProcedure.ParamByName('VERIFYSTATE').AsInteger;
 except
 FReson:=QueryError;
 exit;
 end;
 case state of
 0:FReson:=KeyError;
 1:begin
    Result:=True;
    FReson:=Success;
    end;
 2:FReson:=IsFreezed;
 3:FReson:=AccountNone;
 end;
end;

function TAccountOperation.ChangePassword(const Account: string;
  const password: string; const NewPassword: string): Boolean;
var
  state:integer;
begin
 result:=False;
 Freson:=Unknow;
 ZProcedure.Close;
 ZProcedure.StoredProcName:='P_CHANGEPASSWORD';
 ZProcedure.Params.ParamByName('UNAME').Value:=Account;
 ZProcedure.Params.ParamByName('PW').Value:=Password;
 ZProcedure.Params.ParamByName('NEWPW').Value:=NewPassword;
  try
  Freson:=QueryError;
 ZProcedure.ExecProc;
  state:=ZProcedure.ParamByName('STATE').Value;
 except
  exit;
  end;
 case state of
 0:FReson:=KeyError;
 1:begin
    FReson:=Success;
    result:=True;
 end;
 2:FReson:=AccountNone;
 end;
end;

function TAccountOperation.CreateAccount(Info: PCreateAccount): Boolean;
var
  state:integer;
begin
  result:=False;
  FReson:=LenError;
  With Info^ do begin
 if not (length(username) in [4..15]) then exit;
 if not (length(password) in [4..15]) then exit;
 ZProcedure.Close;
 ZProcedure.StoredProcName:='P_CHANGEPASSWORD';
 ZProcedure.Params.ParamByName('UNAME').Value:=username;
 ZProcedure.Params.ParamByName('PW').Value:=Password;
 ZProcedure.Params.ParamByName('NAME').Value:=Name;
 ZProcedure.Params.ParamByName('IDCODE').Value:=IDCode;
 ZProcedure.Params.ParamByName('BIRTHDAY').Value:=Birthday;
 ZProcedure.Params.ParamByName('QUZ1').Value:=QuestionA;
 ZProcedure.Params.ParamByName('ANS1').Value:=AnswerA;
 ZProcedure.Params.ParamByName('QUZ2').Value:=QuestionB;
 ZProcedure.Params.ParamByName('ANS2').Value:=AnswerB;
 ZProcedure.Params.ParamByName('PHONE').Value:=PhoneNum;
 ZProcedure.Params.ParamByName('PMOBILE').Value:=MobilePhoneNum;
 ZProcedure.Params.ParamByName('EMAIL').Value:=EMail;
 ZProcedure.Params.ParamByName('JDATE').Value:=Formatdatetime('YYYY/MM/DD HH:NN',now());
  try
 ZProcedure.ExecProc;
 state:=ZProcedure.ParamByName('STATE').Value;
 except
  FReson:=QueryError;
  exit;
  end;
 case state of
 0:FReson:=Unknow;
 1:
   begin
   FReson:=Success;
   Result:=True;
   end;
 2:FReson:=AccountExist;
 end;

  end;
end;


constructor TAccountOperation.Create(ZProc:TZStoredProc);
begin
inherited Create;
FReson:='';
ZProcedure:=ZProc;
end;


end.


