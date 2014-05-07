{ Unit pbLoginGateAndLoginSrvMessages.pas }
{ Generated from LoginGateAndLoginSrv.proto }
{ Package LoginGateAndLoginSrv }

unit pbLoginGateAndLoginSrvMessages;

interface

uses
  cUtils,
  cStrings,
  cProtoBufUtils,
  pbClientAndLoginGateMessages;



{ TGate2SrvMsgRecord }

type
  TGate2SrvMsgRecord = record
    SessionID : LongInt;
    Gatemsg : TClientMsgRecord;
  end;
  PGate2SrvMsgRecord = ^TGate2SrvMsgRecord;

procedure Gate2SrvMsgRecordInit(var A: TGate2SrvMsgRecord);
procedure Gate2SrvMsgRecordFinalise(var A: TGate2SrvMsgRecord);
function pbEncodeDataGate2SrvMsgRecord(var Buf; const BufSize: Integer; const A: TGate2SrvMsgRecord): Integer;
function pbEncodeValueGate2SrvMsgRecord(var Buf; const BufSize: Integer; const A: TGate2SrvMsgRecord): Integer;
function pbEncodeFieldGate2SrvMsgRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TGate2SrvMsgRecord): Integer;
function pbDecodeValueGate2SrvMsgRecord(const Buf; const BufSize: Integer; var Value: TGate2SrvMsgRecord): Integer;
procedure pbDecodeFieldGate2SrvMsgRecord(const Field: TpbProtoBufDecodeField; var Value: TGate2SrvMsgRecord);



{ TSrv2GateMsgRecord }

type
  TSrv2GateMsgRecord = record
    SessionID : LongInt;
    SrvMsg : TLoginGateMsgRecord;
  end;
  PSrv2GateMsgRecord = ^TSrv2GateMsgRecord;

procedure Srv2GateMsgRecordInit(var A: TSrv2GateMsgRecord);
procedure Srv2GateMsgRecordFinalise(var A: TSrv2GateMsgRecord);
function pbEncodeDataSrv2GateMsgRecord(var Buf; const BufSize: Integer; const A: TSrv2GateMsgRecord): Integer;
function pbEncodeValueSrv2GateMsgRecord(var Buf; const BufSize: Integer; const A: TSrv2GateMsgRecord): Integer;
function pbEncodeFieldSrv2GateMsgRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TSrv2GateMsgRecord): Integer;
function pbDecodeValueSrv2GateMsgRecord(const Buf; const BufSize: Integer; var Value: TSrv2GateMsgRecord): Integer;
procedure pbDecodeFieldSrv2GateMsgRecord(const Field: TpbProtoBufDecodeField; var Value: TSrv2GateMsgRecord);



implementation



{ TGate2SrvMsgRecord }

procedure Gate2SrvMsgRecordInit(var A: TGate2SrvMsgRecord);
begin
  with A do
  begin
    SessionID := 0;
    ClientMsgRecordInit(Gatemsg);
  end;
end;

procedure Gate2SrvMsgRecordFinalise(var A: TGate2SrvMsgRecord);
begin
  with A do
  begin
    ClientMsgRecordFinalise(Gatemsg);
  end;
end;

function pbEncodeDataGate2SrvMsgRecord(var Buf; const BufSize: Integer; const A: TGate2SrvMsgRecord): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldInt32(P^, L, 1, A.SessionID);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldClientMsgRecord(P^, L, 2, A.Gatemsg);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeValueGate2SrvMsgRecord(var Buf; const BufSize: Integer; const A: TGate2SrvMsgRecord): Integer;
var
  P : PByte;
  L, N, I : Integer;
begin
  P := @Buf;
  L := BufSize;
  N := pbEncodeDataGate2SrvMsgRecord(P^, 0, A);
  I := pbEncodeValueInt32(P^, L, N);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeDataGate2SrvMsgRecord(P^, L, A);
  Assert(I = N);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldGate2SrvMsgRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TGate2SrvMsgRecord): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldKey(P^, L, FieldNum, pwtVarBytes);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeValueGate2SrvMsgRecord(P^, L, A);
  Dec(L, I);
  Result := BufSize - L;
end;

procedure pbDecodeFieldGate2SrvMsgRecord_CallbackProc(const Field: TpbProtoBufDecodeField; const Data: Pointer);
var
  A : PGate2SrvMsgRecord;
begin
  A := Data;
  case Field.FieldNum of
    1 : pbDecodeFieldInt32(Field, A^.SessionID);
    2 : pbDecodeFieldClientMsgRecord(Field, A^.Gatemsg);
  end;
end;

function pbDecodeValueGate2SrvMsgRecord(const Buf; const BufSize: Integer; var Value: TGate2SrvMsgRecord): Integer;
var
  P : PByte;
  L, I, N : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbDecodeValueInt32(P^, L, N);
  Dec(L, I);
  Inc(P, I);
  pbDecodeProtoBuf(P^, N, pbDecodeFieldGate2SrvMsgRecord_CallbackProc, @Value);
  Dec(L, N);
  Result := BufSize - L;
end;

procedure pbDecodeFieldGate2SrvMsgRecord(const Field: TpbProtoBufDecodeField; var Value: TGate2SrvMsgRecord);
begin
  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, pbDecodeFieldGate2SrvMsgRecord_CallbackProc, @Value);
end;



{ TSrv2GateMsgRecord }

procedure Srv2GateMsgRecordInit(var A: TSrv2GateMsgRecord);
begin
  with A do
  begin
    SessionID := 0;
    LoginGateMsgRecordInit(SrvMsg);
  end;
end;

procedure Srv2GateMsgRecordFinalise(var A: TSrv2GateMsgRecord);
begin
  with A do
  begin
    LoginGateMsgRecordFinalise(SrvMsg);
  end;
end;

function pbEncodeDataSrv2GateMsgRecord(var Buf; const BufSize: Integer; const A: TSrv2GateMsgRecord): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldInt32(P^, L, 1, A.SessionID);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldLoginGateMsgRecord(P^, L, 2, A.SrvMsg);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeValueSrv2GateMsgRecord(var Buf; const BufSize: Integer; const A: TSrv2GateMsgRecord): Integer;
var
  P : PByte;
  L, N, I : Integer;
begin
  P := @Buf;
  L := BufSize;
  N := pbEncodeDataSrv2GateMsgRecord(P^, 0, A);
  I := pbEncodeValueInt32(P^, L, N);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeDataSrv2GateMsgRecord(P^, L, A);
  Assert(I = N);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldSrv2GateMsgRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TSrv2GateMsgRecord): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldKey(P^, L, FieldNum, pwtVarBytes);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeValueSrv2GateMsgRecord(P^, L, A);
  Dec(L, I);
  Result := BufSize - L;
end;

procedure pbDecodeFieldSrv2GateMsgRecord_CallbackProc(const Field: TpbProtoBufDecodeField; const Data: Pointer);
var
  A : PSrv2GateMsgRecord;
begin
  A := Data;
  case Field.FieldNum of
    1 : pbDecodeFieldInt32(Field, A^.SessionID);
    2 : pbDecodeFieldLoginGateMsgRecord(Field, A^.SrvMsg);
  end;
end;

function pbDecodeValueSrv2GateMsgRecord(const Buf; const BufSize: Integer; var Value: TSrv2GateMsgRecord): Integer;
var
  P : PByte;
  L, I, N : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbDecodeValueInt32(P^, L, N);
  Dec(L, I);
  Inc(P, I);
  pbDecodeProtoBuf(P^, N, pbDecodeFieldSrv2GateMsgRecord_CallbackProc, @Value);
  Dec(L, N);
  Result := BufSize - L;
end;

procedure pbDecodeFieldSrv2GateMsgRecord(const Field: TpbProtoBufDecodeField; var Value: TSrv2GateMsgRecord);
begin
  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, pbDecodeFieldSrv2GateMsgRecord_CallbackProc, @Value);
end;



end.

