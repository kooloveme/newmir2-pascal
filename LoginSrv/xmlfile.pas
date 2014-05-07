unit XMLFile;
{
           XML Cofiguration File support               
                                                       
           Copyright (C) 2012 Victor Rull              

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

last modified: 05/11/2012

******************************************************************************
* usage example:
*
* unit uGlobal; // keeps common functions/procedures for the whole application
*
* interface
*  uses ...., XMLFile;
*
* var
*  xConf : TXMLFile; // local/user configuration
*  gConf : TXMLFile; // global configuration file - read-only
*
*  .....
*************************************
* unit uMain; // application main form
*
* .....
* procedure TfrmMain.FormCreate(Sender: TObject);
* begin
*   gConf := TXMLFile.Create(GetAppConfigFile(true),  'Configuration', true);
*   xConf := TXMLFile.Create(GetAppConfigFile(False), 'Configuration', false);
*   ReadGlobalConfig; // a single place to fill variables from gConf
* .....
*
* procedure TfrmMain.ReadGlobalConfig;
* const
*  secOption = 'Options';
* begin
*  keepPosition := gConf.ReadBoolean(secOption, 'rememberPosition', true);
*  previewLength := gConf.ReadInteger(secOption, 'previewLength', 24);
*  actShowAttr.Checked := gConf.ReadBoolean(secOption, 'showAttributes', false);
*  errorColor := gConf.ReadInteger(secOption, 'errorColor', clRed);
*  dlgOpen.Filter := gConf.ReadString(secOption, 'fileExtentions', 'All Files|*.*');
* .....
**************************************
*  // saving form position on onClose event; unit uGlobal
* procedure SaveFormPosition(aForm: TForm);
* var
*   secName : string;
* begin
*   if not keepPosition then exit;
*   if aForm.WindowState <> wsNormal then exit;  // don't save if the window is maximized or minimized
*   secName := posTag + '.' + aForm.Name; // posTag is a parent tag name, like "Position"
*   xConf.WriteInteger(secName, 'Left',   aForm.Left);
*   xConf.WriteInteger(secName, 'Top',    aForm.Top);
*   xConf.WriteInteger(secName, 'Width',  aForm.Width);
*   xConf.WriteInteger(secName, 'Height', aForm.Height);
* end;
**************************************
*   enjoy  :)
*******************************************************************************
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, LazFileUtils, Laz2_DOM, Laz2_XMLRead, Laz2_XMLWrite;

const
 // end of Escapes ---- escapes were removed as they are handled internally
  emptyString = '';

type

  { TXMLFile }

  TXMLFile = class(TObject)
  private
    FFileName: string;
    FConf : TXMLDocument;
    FReadOnly : boolean;
    FReadError : boolean;
    FSectionsDelimiter : char;
    function FindChildSection(secName : string; parentNode : TDOMNode; forceCreate : boolean = false) : TDOMNode;
    function FindXMLSection(aSection : string; forceCreate : boolean = false) : TDOMNode;
    function FindXMLKey(aSection, aKey : string) : TDOMNode;
    procedure SetSectionsDelimiter(Value : char);
    procedure SetFileName(fName : string);
    procedure SetReadOnly(Value : boolean);
  public
    constructor Create(const FileName: string; const topNodeName : string; readOnly : boolean = true);
    destructor Destroy; override;
//  --- basic operations ------------------------------
    function ReadString(aSection, aKey : string; aDefault : string = emptyString) : string;
    function ReadInteger(aSection, aKey : string; iDefault : integer = 0) : integer;
    function ReadFloat(aSection, aKey : string; rDefault : double = 0) : double;
    function ReadBoolean(aSection, aKey : string; bDefault : boolean = false; yesValue : string = '1') : boolean;
    function ReadBinaryStream(aSection, aKey : string; aStream : TStream) : integer;
    procedure WriteString(aSection, aKey, aValue : string; suspend : boolean = false);
    procedure WriteInteger(aSection, aKey : string; aValue : integer; suspend : boolean = false);
    procedure WriteFloat(aSection, aKey : string; aValue : double; suspend : boolean = false);
    procedure WriteBoolean(aSection, aKey : string; aValue : boolean;
                           yesValue : string = '1'; noValue : string = '0'; suspend : boolean = false);
    procedure WriteBinaryStream(aSection, aKey : string; aStream : TStream);
    procedure DeleteKey(aSection, aKey : string; suspend : boolean = false);
  // --- Attributes ----------------------------------------
    function ReadAttribute(aSection, aKey, aAttribute : string; aDefault : string = emptyString) : string;
    procedure WriteAttribute(aSection, aKey, aAttribute : string; aValue : string; suspend : boolean = false);
    procedure DeleteAttribute(aSection, aKey, aAttribute : string; suspend : boolean = false);
  // --- Bonus features
    procedure ListChildren(aSection : string; sChildren : TStrings);
    function SectionExists(aSection : string) : boolean;
    procedure AddToList(aSection, aKey : string; aValue : string; capacity : integer = 0); // see remarks in the implementation
    procedure ReadList(aSection, aKey : string; aList : TStrings);
 // --- properties ------------------------------------------
    property FileName: string read FFileName write SetFileName;
    property ReadOnly: boolean read FReadOnly write SetReadOnly; // Write... methods are not executed
    property ReadError: boolean read FReadError; // could not read data from XML, default value used
    property SectionsDelimiter : char read FSectionsDelimiter write SetSectionsDelimiter;
  end;

  function Empty(s : string) : boolean;

  function AddLeadingZeroes(const aNumber, Length : integer) : string;

  procedure FreeChildNodes(xNode : TDOMNode);

implementation

function Empty(s : string) : boolean;
begin
  Result := Length(trim(s)) = 0;
end;

function AddLeadingZeroes(const aNumber, Length : integer) : string;
begin
   result := SysUtils.Format('%.*d', [Length, aNumber]) ;
end;

procedure FreeChildNodes(xNode: TDOMNode);
var
  chdNode : TDOMNode;
begin
  while xNode.HasChildNodes do
    begin
      chdNode := xNode.RemoveChild(xNode.FirstChild);
      FreeChildNodes(chdNode);
      chdNode.Free;
    end;
end;

{ TXMLFile }

constructor TXMLFile.Create(const FileName: string; const topNodeName : string; readOnly: boolean);
var
  rootNode : TDOMNode;
begin
  if Assigned(FConf) then FreeAndNil(FConf);
  FReadError := true;
  FreadOnly := readOnly;
  FSectionsDelimiter := '.';
  FFileName := FileName;
  if FileExistsUTF8(FFileName) then
    try
      ReadXMLFile(FConf, FFileName);
      FReadError := false;
    finally
    end
  else
    begin
      FConf := TXMLDocument.Create;
      FConf.XMLVersion := '1.0';
      FConf.Encoding := 'UTF-8';
      rootNode := FConf.CreateElement(topNodeName);
      FConf.AppendChild(RootNode);
      if not FReadOnly then
        try
          WriteXMLFile(FConf, FFileName);
          FReadError := false;
        except
          on E: Exception do FReadOnly := true;
        end
      else
        FReadError := false;
    end;
end;

destructor TXMLFile.Destroy;
begin
  inherited Destroy;
  FConf.Free;
end;

function TXMLFile.FindChildSection(secName: string; parentNode: TDOMNode;
  forceCreate: boolean = false): TDOMNode;
begin
  Result := parentNode.FindNode(secName);
  if (not Assigned(Result)) and forceCreate then
    Result := parentNode.AppendChild(parentNode.OwnerDocument.CreateElement(secName));
end;

function TXMLFile.FindXMLSection(aSection: string; forceCreate: boolean = false): TDOMNode;
var
  secName : string;
  parentNode : TDOMNode;
begin
  Result := nil;
  parentNode := FConf.DocumentElement;
  while Assigned(parentNode) and (pos(FSectionsDelimiter, aSection) > 0) do
     begin
       secName := copy(aSection, 1, pos(FSectionsDelimiter, aSection)-1);
       aSection := copy(aSection, pos(FSectionsDelimiter, aSection)+1, length(aSection));
       parentNode := FindChildSection(secName, parentNode, forceCreate);
     end;
  if Assigned(parentNode) then
    if Empty(aSection) then Result := parentNode
    else
      Result := FindChildSection(aSection, parentNode, forceCreate);
end;

function TXMLFile.FindXMLKey(aSection, aKey: string): TDOMNode;
var
  sectionNode : TDOMNode;
begin
  Result := nil;
  sectionNode := FindXMLSection(aSection);
  if Assigned(sectionNode) then
    if Empty(aKey) then Result := sectionNode
    else Result := sectionNode.FindNode(aKey);
end;

procedure TXMLFile.SetSectionsDelimiter(Value: char);
begin
  if Value <> FSectionsDelimiter then
    FSectionsDelimiter := Value;
end;

procedure TXMLFile.SetFileName(fName: string);
begin
  if FFileName <> fName then
    begin
      FFileName := fName;
      if not FReadOnly then
        WriteXMLFile(FConf, FFileName);
    end;
end;

procedure TXMLFile.SetReadOnly(Value: boolean);
begin
  if Value <> FReadOnly then
    FReadOnly := Value;
end;

function TXMLFile.ReadString(aSection, aKey: string; aDefault: string): string;
var
  aNode : TDOMNode;
begin
  Result := aDefault;
  FReadError := true;
  aNode := FindXMLKey(aSection, aKey);
  if Assigned(aNode) then
    if (aNode.ChildNodes.Count = 1) and // ChildNodes is potential memory leak
       (aNode.FirstChild.NodeType = TEXT_NODE) then
      begin
         Result := aNode.TextContent;
         FReadError := false;
       end;
  // freeing ChildNodes causes "external SIGSEGV" crash
end;

function TXMLFile.ReadInteger(aSection, aKey: string; iDefault: integer): integer;
var
  sValue : string;
  iValue : integer;
begin
  Result := iDefault;
  FReadError := true;
  sValue := ReadString(aSection, aKey, emptyString);
  if TryStrToInt(sValue, iValue) then
    begin
      Result := iValue;
      FReadError := false;
    end;
end;

function TXMLFile.ReadFloat(aSection, aKey: string; rDefault: double): double;
var
  sValue : string;
  rValue : double;
begin
  Result := rDefault;
  FReadError := true;
  sValue := ReadString(aSection, aKey, emptyString);
  if TryStrToFloat(sValue, rValue) then
    begin
      Result := rValue;
      FReadError := false;
    end;
end;

function TXMLFile.ReadBoolean(aSection, aKey: string; bDefault: boolean;
                              yesValue: string): boolean;
var
  sValue : string;
begin
  Result := bDefault;
  FReadError := true;
  sValue := ReadString(aSection, aKey, emptyString);
  if not Empty(sValue) then
    begin
      Result := CompareText(sValue, yesValue) = 0;
      FReadError := false;
    end;
end;

function TXMLFile.ReadBinaryStream(aSection, aKey: string; aStream: TStream): integer;
var
  streamText: string;
  Stream: TMemoryStream;
  Pos: Integer;
begin
  FReadError := true;
  streamText := ReadString(aSection, aKey, emptyString);
  if streamText <> emptyString then
    begin
      if aStream is TMemoryStream then
        Stream := TMemoryStream(aStream)
      else
        Stream := TMemoryStream.Create;
      try
        Pos := Stream.Position;
        Stream.SetSize(Stream.Size + Length(streamText) div 2);
        HexToBin(PChar(streamText), PChar(PtrUInt(Stream.Memory) + Stream.Position), Length(streamText) div 2);
        Stream.Position := Pos;
        if aStream <> Stream then
          aStream.CopyFrom(Stream, Length(streamText) div 2);
        Result := Stream.Size - Pos;
        FReadError := false;
      finally
        if aStream <> Stream then
          Stream.Free;
      end;
    end
  else
    Result := 0;
end;

procedure TXMLFile.WriteString(aSection, aKey, aValue: string; suspend : boolean = false);
var
  secNode, keyNode : TDOMNode;
begin
  FReadError := true;
  if FReadOnly or Empty(aKey) then exit;
  secNode := FindXMLSection(aSection, true);
  if Assigned(secNode) then
    begin
      keyNode := secNode.FindNode(aKey);
      if not Assigned(keyNode) then
        begin
          keyNode := FConf.CreateElement(aKey);
          keyNode := secNode.AppendChild(keyNode);
        end;
    end;
  keyNode.TextContent := aValue;
  if not suspend then WriteXMLFile(FConf, FFileName);
  FReadError := false;
end;

procedure TXMLFile.WriteInteger(aSection, aKey: string; aValue: integer; suspend : boolean = false);
begin
  WriteString(aSection, aKey, IntToStr(aValue), suspend);
end;

procedure TXMLFile.WriteFloat(aSection, aKey: string; aValue: double; suspend : boolean = false);
begin
  WriteString(aSection, aKey, FloatToStr(aValue), suspend);
end;

procedure TXMLFile.WriteBoolean(aSection, aKey: string; aValue: boolean;
                                yesValue: string; noValue: string; suspend : boolean = false);
var
  boolText : string;
begin
  if aValue then boolText := yesValue
  else boolText := noValue;
  WriteString(aSection, aKey, boolText, suspend);
end;

procedure TXMLFile.WriteBinaryStream(aSection, aKey: string; aStream: TStream);
var
  streamText: string;
  Stream: TMemoryStream;
begin
  FReadError := true;
  SetLength(streamText, (aStream.Size - aStream.Position) * 2);
  if Length(streamText) > 0 then
    begin
      if aStream is TMemoryStream then
        Stream := TMemoryStream(aStream)
      else
        Stream := TMemoryStream.Create;
      try
        if Stream <> aStream then
          begin
            Stream.CopyFrom(aStream, aStream.Size - aStream.Position);
            Stream.Position := 0;
          end;
        BinToHex(PChar(PtrUInt(Stream.Memory) + Stream.Position),
                 PChar(streamText), Stream.Size - Stream.Position);
        FReadError := false;
      finally
        if aStream <> Stream then
          Stream.Free;
      end;
    end;
  WriteString(aSection, aKey, streamText);
end;

procedure TXMLFile.DeleteKey(aSection, aKey: string; suspend : boolean = false);
var
  secNode, keyNode : TDOMNode;
begin
  FReadError := true;
  if FReadOnly or Empty(aKey) then exit;
  secNode := FindXMLSection(aSection, false);
  if Assigned(secNode) then
    begin
      keyNode := secNode.FindNode(aKey);
      if Assigned(keyNode) then
        begin
          keyNode := secNode.RemoveChild(keyNode);
          FreeChildNodes(keyNode); // these two lines have not been tested
          keyNode.Free;            //
          if not suspend then WriteXMLFile(FConf, FFileName);
          FReadError := false;
        end;
    end;
end;

function TXMLFile.ReadAttribute(aSection, aKey, aAttribute: string;
  aDefault: string): string;
var
  aNode : TDOMNode;
begin
  Result := aDefault;
  FReadError := true;
  aNode := FindXMLKey(aSection, aKey);
  if Assigned(aNode) and (aNode.NodeType = ELEMENT_NODE) then
    begin
      aNode := aNode.Attributes.GetNamedItem(aAttribute);
      if Assigned(aNode) then
        begin
          Result := aNode.NodeValue;
          FReadError := false;
        end;
    end;
end;

procedure TXMLFile.WriteAttribute(aSection, aKey, aAttribute: string;
                                  aValue: string; suspend : boolean = false);
var
  secNode, keyNode : TDOMNode;
  attrNode : TDOMNode;
begin
  FReadError := true;
  if FReadOnly or Empty(aKey) or Empty(aAttribute) then exit;
  secNode := FindXMLSection(aSection, true);
  if Assigned(secNode) then
    begin
      keyNode := secNode.FindNode(aKey);
      if not Assigned(keyNode) then
        begin
          keyNode := FConf.CreateElement(aKey);
          keyNode := secNode.AppendChild(keyNode);
        end;
    end;
  attrNode := keyNode.Attributes.GetNamedItem(aAttribute);
  if not Assigned(attrNode) then
    attrNode := FConf.CreateAttribute(aAttribute);
  attrNode.NodeValue := aValue;
  keyNode.Attributes.SetNamedItem(attrNode);
  if not suspend then WriteXMLFile(FConf, FFileName);
  FReadError := false;
end;

procedure TXMLFile.DeleteAttribute(aSection, aKey, aAttribute: string; suspend : boolean = false);
var
  aNode : TDOMNode;
begin
  FReadError := true;
  aNode := FindXMLKey(aSection, aKey);
  if Assigned(aNode) then
    begin
      aNode.Attributes.RemoveNamedItem(aAttribute);
      if not suspend then WriteXMLFile(FConf, FFileName);
      FReadError := false;
    end;
end;

procedure TXMLFile.ListChildren(aSection: string; sChildren: TStrings);
var
  secNode : TDOMNode;
  aNode : TDOMNode;
begin
  sChildren.Clear;
  secNode := FindXMLSection(aSection);
  if Assigned(secNode) then
    if secNode.HasChildNodes then
      begin
        aNode := secNode.FirstChild;
        while Assigned(aNode) do
          begin
            if not (aNode.NodeType in [TEXT_NODE, COMMENT_NODE])  then
              sChildren.Append(aNode.NodeName);
            aNode := aNode.NextSibling;
          end;
      end;
end;

function TXMLFile.SectionExists(aSection: string): boolean;
begin
  Result := Assigned(FindXMLSection(aSection));
end;

procedure TXMLFile.AddToList(aSection, aKey : string; aValue: string; capacity : integer = 0);
const
  defCapacity = 9; // if capacity = 0 and attribute "capacity" is not set then defCapacity is used
  attrCapacity = 'capacity';
var
  lCapacity : integer; // list capacity - max number of elements
  s : string;
  k : integer;
  intList : TStringList;
begin
  intList := TStringList.Create;
  intList.Text := ReadString(aSection, aKey);
  k := intList.IndexOf(aValue);
  while k >= 0 do
    begin
      intList.Delete(k); // remove matching lines
      k := intList.IndexOf(aValue);
    end;
  intList.Insert(0, aValue);
  if capacity = 0 then // read aSection's "capacity" attribute
    begin
      s := ReadAttribute(aSection, aKey, attrCapacity, '9'); // 9 is hard-coded default value
      lCapacity := StrToIntDef(s, defCapacity); // trying to convert attribute value
     end
  else
    lCapacity := capacity;
  // remove excessive elements from the list
  while intList.Count > lCapacity do intList.Delete(intList.Count - 1);
  WriteString(aSection, aKey, intList.Text);
  WriteAttribute(aSection, aKey, attrCapacity, IntToStr(lCapacity));
  intList.Free;
end;

procedure TXMLFile.ReadList(aSection, aKey : string; aList: TStrings);
begin
  aList.Clear;
  aList.Text :=  ReadString(aSection, aKey);
end;

end.

