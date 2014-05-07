unit LoginServer;

{$mode objfpc}{$H+}
{服务实现类}

interface

uses
  Classes, SysUtils,LNetComponents,LNet,AccountDB;
Type
   SocketMsg=record
     Socket:TLSocket;
     Message:String;
   end;
   PSocketMsg=^SocketMsg;

  { TLoginSrv }

  TLoginSrv=class

    Private
      IsStarted:boolean;
      Port:Word;
      TCP:TLTCPComponent;
      DBQuery:TQueryDBThread;
      Procedure LoginGateConnected(aSocket:TLSocket);//网关连接事件
      procedure LoginGateDisConnected(aSocket:TLSocket);//网关断开事件
      Procedure MsgRecevied(aSocket:TLSocket);//消息到达
      procedure OnError(const msg: string; aSocket: TLSocket);//错误事件
    public
      constructor Create(ListenPort:Word);
      destructor Destroy;override;
      Function StartServer():boolean;
      procedure StopServer();
      property ServerStarted:boolean read IsStarted;
  end;

implementation
uses Main;
{ TLoginSrv }

constructor TLoginSrv.Create(ListenPort: Word);
begin
  isStarted:=False;
  Port:=ListenPort;
  TCP:=TLTCPComponent.Create(nil);
  TCP.OnAccept:=@LoginGateConnected;
  TCP.OnDisconnect:=@LoginGateDisConnected;
  TCP.OnReceive:=@MsgRecevied;
  TCP.OnError:=@OnError;
  DBQuery:=TQueryDBThread.Create(TCP,main.MainForm.Config.QueryInterval);
end;

destructor TLoginSrv.Destroy;
begin
  if isStarted then StopServer();
  TCP.Free;
  inherited Destroy;
end;

function TLoginSrv.StartServer: boolean;
begin
  MainOut('开始监听服务端口...');
 if TCP.Listen(Port) then isStarted:=True else isStarted:=False;
 if isStarted then MainOut('监听服务端口成功...') else MainOut('监听服务端口失败...');
 if isStarted then DBQuery.Start;
end;

procedure TLoginSrv.StopServer;
begin
 TCP.Disconnect(True);
 MainOut('网络服务已停止');
 DBQuery.Suspend;
end;

procedure TLoginSrv.LoginGateConnected(aSocket: TLSocket);
begin

end;

procedure TLoginSrv.LoginGateDisConnected(aSocket: TLSocket);
begin

end;

procedure TLoginSrv.MsgRecevied(aSocket: TLSocket);
var
  s:string;
  msg:PSocketMsg;
  List:TList;
begin
 if TCP.GetMessage(s,aSocket) > 0 then
 begin
  New(msg);
  msg^.Socket:=aSocket;
  msg^.Message:=s;
  List:=DBQuery.QueryList.LockList;
  List.Add(msg);
  DBQuery.QueryList.UnlockList;
 end;
end;

procedure TLoginSrv.OnError(const msg: string; aSocket: TLSocket);
begin

end;



end.

