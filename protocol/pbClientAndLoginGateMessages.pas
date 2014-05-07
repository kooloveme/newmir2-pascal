{ Unit pbClientAndLoginGateMessages.pas }
{ Generated from ClientAndLoginGate.proto }
{ Package ClientAndLoginGate }

unit pbClientAndLoginGateMessages;

interface

uses
  cUtils,
  cStrings,
  cProtoBufUtils;



{ TMessageType }

type
  TMessageType = (
    messagetypeSLOGINSRV = 0,
    messagetypeCHECKACCOUNT = 1,
    messagetypeCHANAGEPASSWORD = 2,
    messagetypeCREATENEWACCOUNT = 3,
    messagetypeELOGINSRV = 4,
    messagetypeSROLESRV = 5,
    messagetypeGETROLE = 6,
    messagetypeCREATENEWROLE = 7,
    messagetypeDELROLE = 8,
    messagetypeEROLESRV = 9,
    messagetypeSMANAGESRV = 10,
    messagetypeGETGAMEGATE = 11,
    messagetypeEMANAGESRV = 12
  );

function pbEncodeValueMessageType(var Buf; const BufSize: Integer; const Value: TMessageType): Integer;
function pbEncodeFieldMessageType(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: TMessageType): Integer;
function pbDecodeValueMessageType(const Buf; const BufSize: Integer; var Value: TMessageType): Integer;
procedure pbDecodeFieldMessageType(const Field: TpbProtoBufDecodeField; var Value: TMessageType);



{ TRespondState }

type
  TRespondState = (
    respondstateSUCCESS = 1,
    respondstateFAILED = 2,
    respondstateERROR = 3,
    respondstateUNKNOW = 4
  );

function pbEncodeValueRespondState(var Buf; const BufSize: Integer; const Value: TRespondState): Integer;
function pbEncodeFieldRespondState(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: TRespondState): Integer;
function pbDecodeValueRespondState(const Buf; const BufSize: Integer; var Value: TRespondState): Integer;
procedure pbDecodeFieldRespondState(const Field: TpbProtoBufDecodeField; var Value: TRespondState);



{ TVerifyAccountRecord }

type
  TVerifyAccountRecord = record
    Username : AnsiString;
    Password : AnsiString;
  end;
  PVerifyAccountRecord = ^TVerifyAccountRecord;

procedure VerifyAccountRecordInit(var A: TVerifyAccountRecord);
procedure VerifyAccountRecordFinalise(var A: TVerifyAccountRecord);
function pbEncodeDataVerifyAccountRecord(var Buf; const BufSize: Integer; const A: TVerifyAccountRecord): Integer;
function pbEncodeValueVerifyAccountRecord(var Buf; const BufSize: Integer; const A: TVerifyAccountRecord): Integer;
function pbEncodeFieldVerifyAccountRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TVerifyAccountRecord): Integer;
function pbDecodeValueVerifyAccountRecord(const Buf; const BufSize: Integer; var Value: TVerifyAccountRecord): Integer;
procedure pbDecodeFieldVerifyAccountRecord(const Field: TpbProtoBufDecodeField; var Value: TVerifyAccountRecord);



{ TCreateAccountRecord }

type
  TCreateAccountRecord = record
    ID : AnsiString;
    Password : AnsiString;
    Birthday : AnsiString;
    Name : AnsiString;
    IDCardCode : AnsiString;
    QuestionA : AnsiString;
    AnswerA : AnsiString;
    QuestionB : AnsiString;
    AnswerB : AnsiString;
    PhoneNum : AnsiString;
    EMail : AnsiString;
  end;
  PCreateAccountRecord = ^TCreateAccountRecord;

procedure CreateAccountRecordInit(var A: TCreateAccountRecord);
procedure CreateAccountRecordFinalise(var A: TCreateAccountRecord);
function pbEncodeDataCreateAccountRecord(var Buf; const BufSize: Integer; const A: TCreateAccountRecord): Integer;
function pbEncodeValueCreateAccountRecord(var Buf; const BufSize: Integer; const A: TCreateAccountRecord): Integer;
function pbEncodeFieldCreateAccountRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TCreateAccountRecord): Integer;
function pbDecodeValueCreateAccountRecord(const Buf; const BufSize: Integer; var Value: TCreateAccountRecord): Integer;
procedure pbDecodeFieldCreateAccountRecord(const Field: TpbProtoBufDecodeField; var Value: TCreateAccountRecord);



{ TChangePasswordRecord }

type
  TChangePasswordRecord = record
    Username : AnsiString;
    Password : AnsiString;
    NewPassword : AnsiString;
  end;
  PChangePasswordRecord = ^TChangePasswordRecord;

procedure ChangePasswordRecordInit(var A: TChangePasswordRecord);
procedure ChangePasswordRecordFinalise(var A: TChangePasswordRecord);
function pbEncodeDataChangePasswordRecord(var Buf; const BufSize: Integer; const A: TChangePasswordRecord): Integer;
function pbEncodeValueChangePasswordRecord(var Buf; const BufSize: Integer; const A: TChangePasswordRecord): Integer;
function pbEncodeFieldChangePasswordRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TChangePasswordRecord): Integer;
function pbDecodeValueChangePasswordRecord(const Buf; const BufSize: Integer; var Value: TChangePasswordRecord): Integer;
procedure pbDecodeFieldChangePasswordRecord(const Field: TpbProtoBufDecodeField; var Value: TChangePasswordRecord);



{ TCreateRoleRecord }

type
  TCreateRoleRecord = record
    RoleName : AnsiString;
    Sex : RawByteString;
    Job : RawByteString;
  end;
  PCreateRoleRecord = ^TCreateRoleRecord;

procedure CreateRoleRecordInit(var A: TCreateRoleRecord);
procedure CreateRoleRecordFinalise(var A: TCreateRoleRecord);
function pbEncodeDataCreateRoleRecord(var Buf; const BufSize: Integer; const A: TCreateRoleRecord): Integer;
function pbEncodeValueCreateRoleRecord(var Buf; const BufSize: Integer; const A: TCreateRoleRecord): Integer;
function pbEncodeFieldCreateRoleRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TCreateRoleRecord): Integer;
function pbDecodeValueCreateRoleRecord(const Buf; const BufSize: Integer; var Value: TCreateRoleRecord): Integer;
procedure pbDecodeFieldCreateRoleRecord(const Field: TpbProtoBufDecodeField; var Value: TCreateRoleRecord);



{ TDelRoleRecord }

type
  TDelRoleRecord = record
    RoleID : LongInt;
    RoleName : AnsiString;
  end;
  PDelRoleRecord = ^TDelRoleRecord;

procedure DelRoleRecordInit(var A: TDelRoleRecord);
procedure DelRoleRecordFinalise(var A: TDelRoleRecord);
function pbEncodeDataDelRoleRecord(var Buf; const BufSize: Integer; const A: TDelRoleRecord): Integer;
function pbEncodeValueDelRoleRecord(var Buf; const BufSize: Integer; const A: TDelRoleRecord): Integer;
function pbEncodeFieldDelRoleRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TDelRoleRecord): Integer;
function pbDecodeValueDelRoleRecord(const Buf; const BufSize: Integer; var Value: TDelRoleRecord): Integer;
procedure pbDecodeFieldDelRoleRecord(const Field: TpbProtoBufDecodeField; var Value: TDelRoleRecord);



{ TClientMsgRecord }

type
  TClientMsgRecord = record
    MsgType : TMessageType;
    VerifyInfo : TVerifyAccountRecord;
    ChangePwInfo : TChangePasswordRecord;
    Account : TCreateAccountRecord;
    CrRole : TCreateRoleRecord;
    Drole : TDelRoleRecord;
  end;
  PClientMsgRecord = ^TClientMsgRecord;

procedure ClientMsgRecordInit(var A: TClientMsgRecord);
procedure ClientMsgRecordFinalise(var A: TClientMsgRecord);
function pbEncodeDataClientMsgRecord(var Buf; const BufSize: Integer; const A: TClientMsgRecord): Integer;
function pbEncodeValueClientMsgRecord(var Buf; const BufSize: Integer; const A: TClientMsgRecord): Integer;
function pbEncodeFieldClientMsgRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TClientMsgRecord): Integer;
function pbDecodeValueClientMsgRecord(const Buf; const BufSize: Integer; var Value: TClientMsgRecord): Integer;
procedure pbDecodeFieldClientMsgRecord(const Field: TpbProtoBufDecodeField; var Value: TClientMsgRecord);



{ TRoleInfoRecord }

type
  TRoleInfoRecord = record
    RoleID : LongInt;
    RoleName : AnsiString;
    Sex : RawByteString;
    Job : RawByteString;
    Level : LongInt;
  end;
  PRoleInfoRecord = ^TRoleInfoRecord;

procedure RoleInfoRecordInit(var A: TRoleInfoRecord);
procedure RoleInfoRecordFinalise(var A: TRoleInfoRecord);
function pbEncodeDataRoleInfoRecord(var Buf; const BufSize: Integer; const A: TRoleInfoRecord): Integer;
function pbEncodeValueRoleInfoRecord(var Buf; const BufSize: Integer; const A: TRoleInfoRecord): Integer;
function pbEncodeFieldRoleInfoRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TRoleInfoRecord): Integer;
function pbDecodeValueRoleInfoRecord(const Buf; const BufSize: Integer; var Value: TRoleInfoRecord): Integer;
procedure pbDecodeFieldRoleInfoRecord(const Field: TpbProtoBufDecodeField; var Value: TRoleInfoRecord);



{ TDynArrayRoleInfoRecord }

type
  TDynArrayRoleInfoRecord = array of TRoleInfoRecord;

function pbEncodeFieldDynArrayRoleInfoRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: TDynArrayRoleInfoRecord): Integer;
function pbEncodeFieldDynArrayRoleInfoRecord_Packed(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: TDynArrayRoleInfoRecord): Integer;
procedure pbDecodeFieldDynArrayRoleInfoRecord(const Field: TpbProtoBufDecodeField; var Value: TDynArrayRoleInfoRecord);
procedure pbDecodeFieldDynArrayRoleInfoRecord_Packed(const Field: TpbProtoBufDecodeField; var Value: TDynArrayRoleInfoRecord);



{ TLoginGateMsgRecord }

type
  TLoginGateMsgRecord = record
    MsgType : TMessageType;
    State : TRespondState;
    Reason : AnsiString;
    RoleList : TDynArrayRoleInfoRecord;
  end;
  PLoginGateMsgRecord = ^TLoginGateMsgRecord;

procedure LoginGateMsgRecordInit(var A: TLoginGateMsgRecord);
procedure LoginGateMsgRecordFinalise(var A: TLoginGateMsgRecord);
function pbEncodeDataLoginGateMsgRecord(var Buf; const BufSize: Integer; const A: TLoginGateMsgRecord): Integer;
function pbEncodeValueLoginGateMsgRecord(var Buf; const BufSize: Integer; const A: TLoginGateMsgRecord): Integer;
function pbEncodeFieldLoginGateMsgRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TLoginGateMsgRecord): Integer;
function pbDecodeValueLoginGateMsgRecord(const Buf; const BufSize: Integer; var Value: TLoginGateMsgRecord): Integer;
procedure pbDecodeFieldLoginGateMsgRecord(const Field: TpbProtoBufDecodeField; var Value: TLoginGateMsgRecord);



implementation



{ TMessageType }

function pbEncodeValueMessageType(var Buf; const BufSize: Integer; const Value: TMessageType): Integer;
begin
  Result := pbEncodeValueInt32(Buf, BufSize, Ord(Value));
end;

function pbEncodeFieldMessageType(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: TMessageType): Integer;
begin
  Result := pbEncodeFieldInt32(Buf, BufSize, FieldNum, Ord(Value));
end;

function pbDecodeValueMessageType(const Buf; const BufSize: Integer; var Value: TMessageType): Integer;
var I : LongInt;
begin
  Result := pbDecodeValueInt32(Buf, BufSize, I);
  Value := TMessageType(I);
end;

procedure pbDecodeFieldMessageType(const Field: TpbProtoBufDecodeField; var Value: TMessageType);
var I : LongInt;
begin
  pbDecodeFieldInt32(Field, I);
  Value := TMessageType(I);
end;



{ TRespondState }

function pbEncodeValueRespondState(var Buf; const BufSize: Integer; const Value: TRespondState): Integer;
begin
  Result := pbEncodeValueInt32(Buf, BufSize, Ord(Value));
end;

function pbEncodeFieldRespondState(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: TRespondState): Integer;
begin
  Result := pbEncodeFieldInt32(Buf, BufSize, FieldNum, Ord(Value));
end;

function pbDecodeValueRespondState(const Buf; const BufSize: Integer; var Value: TRespondState): Integer;
var I : LongInt;
begin
  Result := pbDecodeValueInt32(Buf, BufSize, I);
  Value := TRespondState(I);
end;

procedure pbDecodeFieldRespondState(const Field: TpbProtoBufDecodeField; var Value: TRespondState);
var I : LongInt;
begin
  pbDecodeFieldInt32(Field, I);
  Value := TRespondState(I);
end;



{ TVerifyAccountRecord }

procedure VerifyAccountRecordInit(var A: TVerifyAccountRecord);
begin
  with A do
  begin
    Username := '';
    Password := '';
  end;
end;

procedure VerifyAccountRecordFinalise(var A: TVerifyAccountRecord);
begin
  with A do
  begin
  end;
end;

function pbEncodeDataVerifyAccountRecord(var Buf; const BufSize: Integer; const A: TVerifyAccountRecord): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldString(P^, L, 1, A.Username);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 2, A.Password);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeValueVerifyAccountRecord(var Buf; const BufSize: Integer; const A: TVerifyAccountRecord): Integer;
var
  P : PByte;
  L, N, I : Integer;
begin
  P := @Buf;
  L := BufSize;
  N := pbEncodeDataVerifyAccountRecord(P^, 0, A);
  I := pbEncodeValueInt32(P^, L, N);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeDataVerifyAccountRecord(P^, L, A);
  Assert(I = N);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldVerifyAccountRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TVerifyAccountRecord): Integer;
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
  I := pbEncodeValueVerifyAccountRecord(P^, L, A);
  Dec(L, I);
  Result := BufSize - L;
end;

procedure pbDecodeFieldVerifyAccountRecord_CallbackProc(const Field: TpbProtoBufDecodeField; const Data: Pointer);
var
  A : PVerifyAccountRecord;
begin
  A := Data;
  case Field.FieldNum of
    1 : pbDecodeFieldString(Field, A^.Username);
    2 : pbDecodeFieldString(Field, A^.Password);
  end;
end;

function pbDecodeValueVerifyAccountRecord(const Buf; const BufSize: Integer; var Value: TVerifyAccountRecord): Integer;
var
  P : PByte;
  L, I, N : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbDecodeValueInt32(P^, L, N);
  Dec(L, I);
  Inc(P, I);
  pbDecodeProtoBuf(P^, N, pbDecodeFieldVerifyAccountRecord_CallbackProc, @Value);
  Dec(L, N);
  Result := BufSize - L;
end;

procedure pbDecodeFieldVerifyAccountRecord(const Field: TpbProtoBufDecodeField; var Value: TVerifyAccountRecord);
begin
  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, pbDecodeFieldVerifyAccountRecord_CallbackProc, @Value);
end;



{ TCreateAccountRecord }

procedure CreateAccountRecordInit(var A: TCreateAccountRecord);
begin
  with A do
  begin
    ID := '';
    Password := '';
    Birthday := '';
    Name := '';
    IDCardCode := '';
    QuestionA := '';
    AnswerA := '';
    QuestionB := '';
    AnswerB := '';
    PhoneNum := '';
    EMail := '';
  end;
end;

procedure CreateAccountRecordFinalise(var A: TCreateAccountRecord);
begin
  with A do
  begin
  end;
end;

function pbEncodeDataCreateAccountRecord(var Buf; const BufSize: Integer; const A: TCreateAccountRecord): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldString(P^, L, 1, A.ID);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 2, A.Password);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 3, A.Birthday);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 4, A.Name);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 5, A.IDCardCode);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 6, A.QuestionA);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 7, A.AnswerA);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 8, A.QuestionB);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 9, A.AnswerB);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 10, A.PhoneNum);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 11, A.EMail);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeValueCreateAccountRecord(var Buf; const BufSize: Integer; const A: TCreateAccountRecord): Integer;
var
  P : PByte;
  L, N, I : Integer;
begin
  P := @Buf;
  L := BufSize;
  N := pbEncodeDataCreateAccountRecord(P^, 0, A);
  I := pbEncodeValueInt32(P^, L, N);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeDataCreateAccountRecord(P^, L, A);
  Assert(I = N);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldCreateAccountRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TCreateAccountRecord): Integer;
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
  I := pbEncodeValueCreateAccountRecord(P^, L, A);
  Dec(L, I);
  Result := BufSize - L;
end;

procedure pbDecodeFieldCreateAccountRecord_CallbackProc(const Field: TpbProtoBufDecodeField; const Data: Pointer);
var
  A : PCreateAccountRecord;
begin
  A := Data;
  case Field.FieldNum of
    1 : pbDecodeFieldString(Field, A^.ID);
    2 : pbDecodeFieldString(Field, A^.Password);
    3 : pbDecodeFieldString(Field, A^.Birthday);
    4 : pbDecodeFieldString(Field, A^.Name);
    5 : pbDecodeFieldString(Field, A^.IDCardCode);
    6 : pbDecodeFieldString(Field, A^.QuestionA);
    7 : pbDecodeFieldString(Field, A^.AnswerA);
    8 : pbDecodeFieldString(Field, A^.QuestionB);
    9 : pbDecodeFieldString(Field, A^.AnswerB);
    10 : pbDecodeFieldString(Field, A^.PhoneNum);
    11 : pbDecodeFieldString(Field, A^.EMail);
  end;
end;

function pbDecodeValueCreateAccountRecord(const Buf; const BufSize: Integer; var Value: TCreateAccountRecord): Integer;
var
  P : PByte;
  L, I, N : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbDecodeValueInt32(P^, L, N);
  Dec(L, I);
  Inc(P, I);
  pbDecodeProtoBuf(P^, N, pbDecodeFieldCreateAccountRecord_CallbackProc, @Value);
  Dec(L, N);
  Result := BufSize - L;
end;

procedure pbDecodeFieldCreateAccountRecord(const Field: TpbProtoBufDecodeField; var Value: TCreateAccountRecord);
begin
  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, pbDecodeFieldCreateAccountRecord_CallbackProc, @Value);
end;



{ TChangePasswordRecord }

procedure ChangePasswordRecordInit(var A: TChangePasswordRecord);
begin
  with A do
  begin
    Username := '';
    Password := '';
    NewPassword := '';
  end;
end;

procedure ChangePasswordRecordFinalise(var A: TChangePasswordRecord);
begin
  with A do
  begin
  end;
end;

function pbEncodeDataChangePasswordRecord(var Buf; const BufSize: Integer; const A: TChangePasswordRecord): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldString(P^, L, 1, A.Username);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 2, A.Password);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 3, A.NewPassword);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeValueChangePasswordRecord(var Buf; const BufSize: Integer; const A: TChangePasswordRecord): Integer;
var
  P : PByte;
  L, N, I : Integer;
begin
  P := @Buf;
  L := BufSize;
  N := pbEncodeDataChangePasswordRecord(P^, 0, A);
  I := pbEncodeValueInt32(P^, L, N);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeDataChangePasswordRecord(P^, L, A);
  Assert(I = N);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldChangePasswordRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TChangePasswordRecord): Integer;
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
  I := pbEncodeValueChangePasswordRecord(P^, L, A);
  Dec(L, I);
  Result := BufSize - L;
end;

procedure pbDecodeFieldChangePasswordRecord_CallbackProc(const Field: TpbProtoBufDecodeField; const Data: Pointer);
var
  A : PChangePasswordRecord;
begin
  A := Data;
  case Field.FieldNum of
    1 : pbDecodeFieldString(Field, A^.Username);
    2 : pbDecodeFieldString(Field, A^.Password);
    3 : pbDecodeFieldString(Field, A^.NewPassword);
  end;
end;

function pbDecodeValueChangePasswordRecord(const Buf; const BufSize: Integer; var Value: TChangePasswordRecord): Integer;
var
  P : PByte;
  L, I, N : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbDecodeValueInt32(P^, L, N);
  Dec(L, I);
  Inc(P, I);
  pbDecodeProtoBuf(P^, N, pbDecodeFieldChangePasswordRecord_CallbackProc, @Value);
  Dec(L, N);
  Result := BufSize - L;
end;

procedure pbDecodeFieldChangePasswordRecord(const Field: TpbProtoBufDecodeField; var Value: TChangePasswordRecord);
begin
  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, pbDecodeFieldChangePasswordRecord_CallbackProc, @Value);
end;



{ TCreateRoleRecord }

procedure CreateRoleRecordInit(var A: TCreateRoleRecord);
begin
  with A do
  begin
    RoleName := '';
    Sex := '';
    Job := '';
  end;
end;

procedure CreateRoleRecordFinalise(var A: TCreateRoleRecord);
begin
  with A do
  begin
  end;
end;

function pbEncodeDataCreateRoleRecord(var Buf; const BufSize: Integer; const A: TCreateRoleRecord): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldString(P^, L, 1, A.RoleName);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldBytes(P^, L, 2, A.Sex);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldBytes(P^, L, 3, A.Job);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeValueCreateRoleRecord(var Buf; const BufSize: Integer; const A: TCreateRoleRecord): Integer;
var
  P : PByte;
  L, N, I : Integer;
begin
  P := @Buf;
  L := BufSize;
  N := pbEncodeDataCreateRoleRecord(P^, 0, A);
  I := pbEncodeValueInt32(P^, L, N);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeDataCreateRoleRecord(P^, L, A);
  Assert(I = N);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldCreateRoleRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TCreateRoleRecord): Integer;
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
  I := pbEncodeValueCreateRoleRecord(P^, L, A);
  Dec(L, I);
  Result := BufSize - L;
end;

procedure pbDecodeFieldCreateRoleRecord_CallbackProc(const Field: TpbProtoBufDecodeField; const Data: Pointer);
var
  A : PCreateRoleRecord;
begin
  A := Data;
  case Field.FieldNum of
    1 : pbDecodeFieldString(Field, A^.RoleName);
    2 : pbDecodeFieldBytes(Field, A^.Sex);
    3 : pbDecodeFieldBytes(Field, A^.Job);
  end;
end;

function pbDecodeValueCreateRoleRecord(const Buf; const BufSize: Integer; var Value: TCreateRoleRecord): Integer;
var
  P : PByte;
  L, I, N : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbDecodeValueInt32(P^, L, N);
  Dec(L, I);
  Inc(P, I);
  pbDecodeProtoBuf(P^, N, pbDecodeFieldCreateRoleRecord_CallbackProc, @Value);
  Dec(L, N);
  Result := BufSize - L;
end;

procedure pbDecodeFieldCreateRoleRecord(const Field: TpbProtoBufDecodeField; var Value: TCreateRoleRecord);
begin
  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, pbDecodeFieldCreateRoleRecord_CallbackProc, @Value);
end;



{ TDelRoleRecord }

procedure DelRoleRecordInit(var A: TDelRoleRecord);
begin
  with A do
  begin
    RoleID := 0;
    RoleName := '';
  end;
end;

procedure DelRoleRecordFinalise(var A: TDelRoleRecord);
begin
  with A do
  begin
  end;
end;

function pbEncodeDataDelRoleRecord(var Buf; const BufSize: Integer; const A: TDelRoleRecord): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldInt32(P^, L, 1, A.RoleID);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 2, A.RoleName);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeValueDelRoleRecord(var Buf; const BufSize: Integer; const A: TDelRoleRecord): Integer;
var
  P : PByte;
  L, N, I : Integer;
begin
  P := @Buf;
  L := BufSize;
  N := pbEncodeDataDelRoleRecord(P^, 0, A);
  I := pbEncodeValueInt32(P^, L, N);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeDataDelRoleRecord(P^, L, A);
  Assert(I = N);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldDelRoleRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TDelRoleRecord): Integer;
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
  I := pbEncodeValueDelRoleRecord(P^, L, A);
  Dec(L, I);
  Result := BufSize - L;
end;

procedure pbDecodeFieldDelRoleRecord_CallbackProc(const Field: TpbProtoBufDecodeField; const Data: Pointer);
var
  A : PDelRoleRecord;
begin
  A := Data;
  case Field.FieldNum of
    1 : pbDecodeFieldInt32(Field, A^.RoleID);
    2 : pbDecodeFieldString(Field, A^.RoleName);
  end;
end;

function pbDecodeValueDelRoleRecord(const Buf; const BufSize: Integer; var Value: TDelRoleRecord): Integer;
var
  P : PByte;
  L, I, N : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbDecodeValueInt32(P^, L, N);
  Dec(L, I);
  Inc(P, I);
  pbDecodeProtoBuf(P^, N, pbDecodeFieldDelRoleRecord_CallbackProc, @Value);
  Dec(L, N);
  Result := BufSize - L;
end;

procedure pbDecodeFieldDelRoleRecord(const Field: TpbProtoBufDecodeField; var Value: TDelRoleRecord);
begin
  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, pbDecodeFieldDelRoleRecord_CallbackProc, @Value);
end;



{ TClientMsgRecord }

procedure ClientMsgRecordInit(var A: TClientMsgRecord);
begin
  with A do
  begin
    MsgType := messagetypeSLOGINSRV;
    VerifyAccountRecordInit(VerifyInfo);
    ChangePasswordRecordInit(ChangePwInfo);
    CreateAccountRecordInit(Account);
    CreateRoleRecordInit(CrRole);
    DelRoleRecordInit(Drole);
  end;
end;

procedure ClientMsgRecordFinalise(var A: TClientMsgRecord);
begin
  with A do
  begin
    DelRoleRecordFinalise(Drole);
    CreateRoleRecordFinalise(CrRole);
    CreateAccountRecordFinalise(Account);
    ChangePasswordRecordFinalise(ChangePwInfo);
    VerifyAccountRecordFinalise(VerifyInfo);
  end;
end;

function pbEncodeDataClientMsgRecord(var Buf; const BufSize: Integer; const A: TClientMsgRecord): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldMessageType(P^, L, 1, A.MsgType);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldVerifyAccountRecord(P^, L, 2, A.VerifyInfo);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldChangePasswordRecord(P^, L, 3, A.ChangePwInfo);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldCreateAccountRecord(P^, L, 4, A.Account);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldCreateRoleRecord(P^, L, 6, A.CrRole);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldDelRoleRecord(P^, L, 7, A.Drole);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeValueClientMsgRecord(var Buf; const BufSize: Integer; const A: TClientMsgRecord): Integer;
var
  P : PByte;
  L, N, I : Integer;
begin
  P := @Buf;
  L := BufSize;
  N := pbEncodeDataClientMsgRecord(P^, 0, A);
  I := pbEncodeValueInt32(P^, L, N);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeDataClientMsgRecord(P^, L, A);
  Assert(I = N);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldClientMsgRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TClientMsgRecord): Integer;
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
  I := pbEncodeValueClientMsgRecord(P^, L, A);
  Dec(L, I);
  Result := BufSize - L;
end;

procedure pbDecodeFieldClientMsgRecord_CallbackProc(const Field: TpbProtoBufDecodeField; const Data: Pointer);
var
  A : PClientMsgRecord;
begin
  A := Data;
  case Field.FieldNum of
    1 : pbDecodeFieldMessageType(Field, A^.MsgType);
    2 : pbDecodeFieldVerifyAccountRecord(Field, A^.VerifyInfo);
    3 : pbDecodeFieldChangePasswordRecord(Field, A^.ChangePwInfo);
    4 : pbDecodeFieldCreateAccountRecord(Field, A^.Account);
    6 : pbDecodeFieldCreateRoleRecord(Field, A^.CrRole);
    7 : pbDecodeFieldDelRoleRecord(Field, A^.Drole);
  end;
end;

function pbDecodeValueClientMsgRecord(const Buf; const BufSize: Integer; var Value: TClientMsgRecord): Integer;
var
  P : PByte;
  L, I, N : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbDecodeValueInt32(P^, L, N);
  Dec(L, I);
  Inc(P, I);
  pbDecodeProtoBuf(P^, N, pbDecodeFieldClientMsgRecord_CallbackProc, @Value);
  Dec(L, N);
  Result := BufSize - L;
end;

procedure pbDecodeFieldClientMsgRecord(const Field: TpbProtoBufDecodeField; var Value: TClientMsgRecord);
begin
  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, pbDecodeFieldClientMsgRecord_CallbackProc, @Value);
end;



{ TRoleInfoRecord }

procedure RoleInfoRecordInit(var A: TRoleInfoRecord);
begin
  with A do
  begin
    RoleID := 0;
    RoleName := '';
    Sex := '';
    Job := '';
    Level := 0;
  end;
end;

procedure RoleInfoRecordFinalise(var A: TRoleInfoRecord);
begin
  with A do
  begin
  end;
end;

function pbEncodeDataRoleInfoRecord(var Buf; const BufSize: Integer; const A: TRoleInfoRecord): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldInt32(P^, L, 1, A.RoleID);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 2, A.RoleName);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldBytes(P^, L, 3, A.Sex);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldBytes(P^, L, 4, A.Job);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldInt32(P^, L, 5, A.Level);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeValueRoleInfoRecord(var Buf; const BufSize: Integer; const A: TRoleInfoRecord): Integer;
var
  P : PByte;
  L, N, I : Integer;
begin
  P := @Buf;
  L := BufSize;
  N := pbEncodeDataRoleInfoRecord(P^, 0, A);
  I := pbEncodeValueInt32(P^, L, N);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeDataRoleInfoRecord(P^, L, A);
  Assert(I = N);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldRoleInfoRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TRoleInfoRecord): Integer;
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
  I := pbEncodeValueRoleInfoRecord(P^, L, A);
  Dec(L, I);
  Result := BufSize - L;
end;

procedure pbDecodeFieldRoleInfoRecord_CallbackProc(const Field: TpbProtoBufDecodeField; const Data: Pointer);
var
  A : PRoleInfoRecord;
begin
  A := Data;
  case Field.FieldNum of
    1 : pbDecodeFieldInt32(Field, A^.RoleID);
    2 : pbDecodeFieldString(Field, A^.RoleName);
    3 : pbDecodeFieldBytes(Field, A^.Sex);
    4 : pbDecodeFieldBytes(Field, A^.Job);
    5 : pbDecodeFieldInt32(Field, A^.Level);
  end;
end;

function pbDecodeValueRoleInfoRecord(const Buf; const BufSize: Integer; var Value: TRoleInfoRecord): Integer;
var
  P : PByte;
  L, I, N : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbDecodeValueInt32(P^, L, N);
  Dec(L, I);
  Inc(P, I);
  pbDecodeProtoBuf(P^, N, pbDecodeFieldRoleInfoRecord_CallbackProc, @Value);
  Dec(L, N);
  Result := BufSize - L;
end;

procedure pbDecodeFieldRoleInfoRecord(const Field: TpbProtoBufDecodeField; var Value: TRoleInfoRecord);
begin
  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, pbDecodeFieldRoleInfoRecord_CallbackProc, @Value);
end;



{ TDynArrayRoleInfoRecord }

function pbEncodeFieldDynArrayRoleInfoRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: TDynArrayRoleInfoRecord): Integer;
var
  P : PByte;
  I, L, N : Integer;
begin
  P := @Buf;
  L := BufSize;
  for I := 0 to Length(Value) - 1 do
    begin
      N := pbEncodeFieldRoleInfoRecord(P^, L, FieldNum, Value[I]);
      Inc(P, N);
      Dec(L, N);
    end;
  Result := BufSize - L;
end;

function pbEncodeFieldDynArrayRoleInfoRecord_Packed(var Buf; const BufSize: Integer; const FieldNum: Integer; const Value: TDynArrayRoleInfoRecord): Integer;
var
  P : PByte;
  I, T, L, N : Integer;
begin
  P := @Buf;
  T := 0;
  for I := 0 to Length(Value) - 1 do
    Inc(T, pbEncodeValueRoleInfoRecord(P^, 0, Value[I]));
  L := BufSize;
  N := pbEncodeFieldVarBytesHdr(P^, L, FieldNum, T);
  Inc(P, N);
  Dec(L, N);
  for I := 0 to Length(Value) - 1 do
    begin
      N := pbEncodeValueRoleInfoRecord(P^, L, Value[I]);
      Inc(P, N);
      Dec(L, N);
    end;
  Result := BufSize - L;
end;

procedure pbDecodeFieldDynArrayRoleInfoRecord(const Field: TpbProtoBufDecodeField; var Value: TDynArrayRoleInfoRecord);
var
  L : Integer;
begin
  L := Length(Value);
  SetLength(Value, L + 1);
  RoleInfoRecordInit(Value[L]);
  pbDecodeFieldRoleInfoRecord(Field, Value[L]);
end;

procedure pbDecodeFieldDynArrayRoleInfoRecord_Packed(const Field: TpbProtoBufDecodeField; var Value: TDynArrayRoleInfoRecord);
var
  P : PByte;
  L, N, I : Integer;
begin
  P := Field.ValueVarBytesPtr;
  L := 0;
  N := Field.ValueVarBytesLen;
  while N > 0 do
    begin
      SetLength(Value, L + 1);
      RoleInfoRecordInit(Value[L]);
      I := pbDecodeValueRoleInfoRecord(P^, N, Value[L]);
      Inc(L);
      Inc(P, I);
      Dec(N, I);
    end;
end;



{ TLoginGateMsgRecord }

procedure LoginGateMsgRecordInit(var A: TLoginGateMsgRecord);
begin
  with A do
  begin
    MsgType := messagetypeSLOGINSRV;
    State := respondstateSUCCESS;
    Reason := '';
    RoleList := nil;
  end;
end;

procedure LoginGateMsgRecordFinalise(var A: TLoginGateMsgRecord);
begin
  with A do
  begin
  end;
end;

function pbEncodeDataLoginGateMsgRecord(var Buf; const BufSize: Integer; const A: TLoginGateMsgRecord): Integer;
var
  P : PByte;
  L : Integer;
  I : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbEncodeFieldMessageType(P^, L, 1, A.MsgType);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldRespondState(P^, L, 2, A.State);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldString(P^, L, 3, A.Reason);
  Dec(L, I);
  Inc(P, I);
  I := pbEncodeFieldDynArrayRoleInfoRecord(P^, L, 4, A.RoleList);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeValueLoginGateMsgRecord(var Buf; const BufSize: Integer; const A: TLoginGateMsgRecord): Integer;
var
  P : PByte;
  L, N, I : Integer;
begin
  P := @Buf;
  L := BufSize;
  N := pbEncodeDataLoginGateMsgRecord(P^, 0, A);
  I := pbEncodeValueInt32(P^, L, N);
  Inc(P, I);
  Dec(L, I);
  I := pbEncodeDataLoginGateMsgRecord(P^, L, A);
  Assert(I = N);
  Dec(L, I);
  Result := BufSize - L;
end;

function pbEncodeFieldLoginGateMsgRecord(var Buf; const BufSize: Integer; const FieldNum: Integer; const A: TLoginGateMsgRecord): Integer;
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
  I := pbEncodeValueLoginGateMsgRecord(P^, L, A);
  Dec(L, I);
  Result := BufSize - L;
end;

procedure pbDecodeFieldLoginGateMsgRecord_CallbackProc(const Field: TpbProtoBufDecodeField; const Data: Pointer);
var
  A : PLoginGateMsgRecord;
begin
  A := Data;
  case Field.FieldNum of
    1 : pbDecodeFieldMessageType(Field, A^.MsgType);
    2 : pbDecodeFieldRespondState(Field, A^.State);
    3 : pbDecodeFieldString(Field, A^.Reason);
    4 : pbDecodeFieldDynArrayRoleInfoRecord(Field, A^.RoleList);
  end;
end;

function pbDecodeValueLoginGateMsgRecord(const Buf; const BufSize: Integer; var Value: TLoginGateMsgRecord): Integer;
var
  P : PByte;
  L, I, N : Integer;
begin
  P := @Buf;
  L := BufSize;
  I := pbDecodeValueInt32(P^, L, N);
  Dec(L, I);
  Inc(P, I);
  pbDecodeProtoBuf(P^, N, pbDecodeFieldLoginGateMsgRecord_CallbackProc, @Value);
  Dec(L, N);
  Result := BufSize - L;
end;

procedure pbDecodeFieldLoginGateMsgRecord(const Field: TpbProtoBufDecodeField; var Value: TLoginGateMsgRecord);
begin
  pbDecodeProtoBuf(Field.ValueVarBytesPtr^, Field.ValueVarBytesLen, pbDecodeFieldLoginGateMsgRecord_CallbackProc, @Value);
end;



end.

