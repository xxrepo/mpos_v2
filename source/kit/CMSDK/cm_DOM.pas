{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_DOM

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_DOM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type

  TCMDOMNode = class;

  { TCMDOMNodeList }

  TCMDOMNodeList = class
  private
    FList: TFPObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TCMDOMNode;
    procedure SetCount(AValue: Integer);
    procedure SetItem(Index: Integer; AValue: TCMDOMNode);
  public
    constructor Create;
    destructor Destroy; override;
    Procedure Clear;
    Function Add(ANode: TCMDOMNode): Integer;
    Procedure Delete(Index: Integer);
    Function Remove(ANode: TCMDOMNode): Integer;
    Function IndexOf(ANode: TCMDOMNode): Integer;
    Procedure Insert(Index: Integer; ANode: TCMDOMNode);
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TCMDOMNode read GetItem write SetItem; default;
  end;

  TCMDOMNode = class(TPersistent)
  private
    FLevel: Integer;
    FName: string;
    FText: string;
    FData: Pointer;
    FAttributes: TStrings;
    FParentNode: TCMDOMNode;
    FChildNodeList: TCMDOMNodeList;
  protected
    function GetChildCount: Integer;
    function GetFirstChild: TCMDOMNode;
    function GetLastChild: TCMDOMNode;
    function GetIndex: Integer;
    function GetPreviousSibling: TCMDOMNode;
    function GetNextSibling: TCMDOMNode;
  public
    constructor Create(const AName: string; AParentNode: TCMDOMNode=nil); overload;
    constructor Create(const AName, AText: string; AParentNode: TCMDOMNode=nil); virtual; overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    //
    property Level: Integer read FLevel;
    property Name: string read FName;
    property Text: string read FText write FText;
    property Data: Pointer read FData write FData;
    property Attributes: TStrings read FAttributes write FAttributes;
    procedure SetAttribute(const AName, AValue: string); overload;
    procedure SetAttribute(const AName: string; const AValue: Integer); overload;
    function GetAttribute(const AName: string): string;
    function AttributeExists(const AName: string): Boolean;
    function DeleteAttribute(const AName: string): Boolean;
    //
    property ParentNode: TCMDOMNode read FParentNode;
    function FindNode(const ANodeName: string): TCMDOMNode;
    function FindChildNode(const ANodeName: string): TCMDOMNode;
    function HasChildNodes: Boolean;
    property ChildCount: Integer read GetChildCount;
    property FirstChild: TCMDOMNode read GetFirstChild;
    property LastChild: TCMDOMNode read GetLastChild;
    property ChildNodes: TCMDOMNodeList read FChildNodeList;
    function AppendChild(NewChild: TCMDOMNode): Integer;
    function RemoveChild(OldChild: TCMDOMNode): Integer; overload;
    function RemoveChild(const OldChildName: string): Integer; overload;
    //
    property Index: Integer read GetIndex;
    property PreviousSibling: TCMDOMNode read GetPreviousSibling;
    property NextSibling: TCMDOMNode read GetNextSibling;
  end;

  function FindNodeText(ANode: TCMDOMNode; const ANodeName: string; AThenStr: string=''): string;
  procedure SetChildNodeText(ANode: TCMDOMNode; const ANodeName, AText: string);

implementation

function FindNodeText(ANode: TCMDOMNode; const ANodeName: string; AThenStr: string=''): string;
begin
  Result := AThenStr;
  if Assigned(ANode) then
    if Assigned(ANode.FindNode(ANodeName)) then
      Result := ANode.FindNode(ANodeName).Text;
end;

procedure SetChildNodeText(ANode: TCMDOMNode; const ANodeName, AText: string);
var
  cNode: TCMDOMNode;
begin
  cNode := ANode.FindChildNode(ANodeName);
  if Assigned(cNode) then
    cNode.Text := AText
  else
    TCMDOMNode.Create(ANodeName, AText, ANode);
end;

{ TCMDOMNodeList }

function TCMDOMNodeList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCMDOMNodeList.GetItem(Index: Integer): TCMDOMNode;
begin
  Result := TCMDOMNode(FList[Index]);
end;

procedure TCMDOMNodeList.SetCount(AValue: Integer);
begin
  FList.Count := AValue;
end;

procedure TCMDOMNodeList.SetItem(Index: Integer; AValue: TCMDOMNode);
begin
  FList[Index] := AValue;
end;

constructor TCMDOMNodeList.Create;
begin
  FList := TFPObjectList.Create(True);
end;

destructor TCMDOMNodeList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TCMDOMNodeList.Clear;
begin
  FList.Clear;
end;

function TCMDOMNodeList.Add(ANode: TCMDOMNode): Integer;
begin
  Result := FList.Add(ANode);
end;

procedure TCMDOMNodeList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

function TCMDOMNodeList.Remove(ANode: TCMDOMNode): Integer;
begin
  Result := FList.Remove(ANode);
end;

function TCMDOMNodeList.IndexOf(ANode: TCMDOMNode): Integer;
begin
  Result := FList.IndexOf(ANode);
end;

procedure TCMDOMNodeList.Insert(Index: Integer; ANode: TCMDOMNode);
begin
  FList.Insert(Index, ANode);
end;

{TCMDOMNode}

constructor TCMDOMNode.Create(const AName: string; AParentNode: TCMDOMNode=nil);
begin
  Self.Create(AName, '', AParentNode);
end;

constructor TCMDOMNode.Create(const AName, AText: string; AParentNode: TCMDOMNode=nil);
begin
  inherited Create;
  FParentNode := AParentNode;
  FName := AName;
  FText := AText;
  FAttributes := TStringList.Create;
  FChildNodeList := TCMDOMNodeList.Create;
  //
  if Assigned(FParentNode) then
    begin
      FParentNode.AppendChild(Self);
      FLevel := FParentNode.Level + 1;
    end
  else
    FLevel := 1;
  FData := nil;
end;

destructor TCMDOMNode.Destroy;
begin
  if Assigned(FParentNode) then
    Exit;
  //
  if Assigned(FChildNodeList) then
    FreeAndNil(FChildNodeList);
  if Assigned(FAttributes) then
    FreeAndNil(FAttributes);
  inherited Destroy;
end;

procedure TCMDOMNode.Assign(Source: TPersistent);
begin
  if Source is TCMDOMNode then
    begin
      Self.FLevel := TCMDOMNode(Source).FLevel;
      Self.FName := TCMDOMNode(Source).FName;
      Self.FText := TCMDOMNode(Source).FText;
      Self.FData := TCMDOMNode(Source).FData;
      Self.FParentNode := TCMDOMNode(Source).FParentNode;
      Self.FChildNodeList := TCMDOMNode(Source).FChildNodeList;
    end
  else
    inherited Assign(Source);
end;

function TCMDOMNode.FindNode(const ANodeName: string): TCMDOMNode;
var
  i: Integer;
begin
  Result := nil;
  if ANodeName = Self.Name then
    begin
      Result := Self;
      Exit;
    end;
  if Assigned(FChildNodeList) then
    begin
      for i:=0 to FChildNodeList.Count-1 do
        begin
          if FChildNodeList[i].Name = ANodeName then
            begin
              Result := FChildNodeList[i];
              Exit;
            end;
        end;
    end;
  if Assigned(FParentNode) and Assigned(FParentNode.ChildNodes) then
    begin
      for i:=0 to FParentNode.ChildNodes.Count-1 do
        begin
          if FParentNode.ChildNodes[i].Name = ANodeName then
            begin
              Result := FParentNode.ChildNodes[i];
              Exit;
            end;
        end;
    end;
end;

function TCMDOMNode.FindChildNode(const ANodeName: string): TCMDOMNode;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(FChildNodeList) then
    begin
      for i:=0 to FChildNodeList.Count-1 do
        begin
          if FChildNodeList[i].Name = ANodeName then
            begin
              Result := FChildNodeList[i];
              Exit;
            end;
        end;
    end;
end;

function TCMDOMNode.HasChildNodes: Boolean;
begin
  Result := Assigned(FChildNodeList) and (FChildNodeList.Count > 0);
end;

function TCMDOMNode.AppendChild(NewChild: TCMDOMNode): Integer;
begin
  Result := -1;
  if Assigned(FChildNodeList) then
    begin
      NewChild.FParentNode := Self;
      Result := FChildNodeList.Add(NewChild);
      if Result >= 0 then
        NewChild.FLevel := Self.FLevel + 1;
    end;
end;

function TCMDOMNode.RemoveChild(OldChild: TCMDOMNode): Integer;
begin
  Result := -1;
  if Assigned(FChildNodeList) then
    Result := FChildNodeList.Remove(OldChild);
end;

function TCMDOMNode.RemoveChild(const OldChildName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Assigned(FChildNodeList) then
    begin
      for i:=0 to FChildNodeList.Count-1 do
        begin
          if FChildNodeList[i].Name = OldChildName then
            begin
              Result := Self.RemoveChild(FChildNodeList[i]);
              Exit;
            end;
        end;
    end;
end;

function TCMDOMNode.GetChildCount: Integer;
begin
  Result := 0;
  if Assigned(FChildNodeList) then
    Result := FChildNodeList.Count;
end;

function TCMDOMNode.GetFirstChild: TCMDOMNode;
begin
  Result := nil;
  if GetChildCount > 0 then
    Result := FChildNodeList[0];
end;

function TCMDOMNode.GetLastChild: TCMDOMNode;
begin
  Result := nil;
  if GetChildCount > 0 then
    Result := FChildNodeList[GetChildCount - 1];
end;

function TCMDOMNode.GetIndex: Integer;
begin
  Result := -1;
  if Assigned(FParentNode) then
    if Assigned(FParentNode.ChildNodes) then
      Result := FParentNode.ChildNodes.IndexOf(Self);
end;

function TCMDOMNode.GetPreviousSibling: TCMDOMNode;
begin
  Result := nil;
  if GetIndex > 0 then
    if Assigned(FParentNode) then
      if Assigned(FParentNode.ChildNodes) then
        Result := FParentNode.ChildNodes[GetIndex - 1];
end;

function TCMDOMNode.GetNextSibling: TCMDOMNode;
begin
  Result := nil;
  if Assigned(FParentNode) then
    if Assigned(FParentNode.ChildNodes) then
      if GetIndex < FParentNode.ChildNodes.Count - 1 then
        Result := FParentNode.ChildNodes[GetIndex + 1];
end;

procedure TCMDOMNode.SetAttribute(const AName, AValue: string);
var
  i: Integer;
begin
  i := Self.Attributes.IndexOfName(AName);
  if i>=0 then
    Self.Attributes.ValueFromIndex[i] := AValue
  else
    Self.Attributes.Add(AName + Self.Attributes.NameValueSeparator + AValue);
end;

procedure TCMDOMNode.SetAttribute(const AName: string; const AValue: Integer);
begin
  SetAttribute(AName, IntToStr(AValue));
end;

function TCMDOMNode.GetAttribute(const AName: string): string;
var
  i: Integer;
begin
  Result := '';
  i := Self.Attributes.IndexOfName(AName);
  if i>=0 then
    Result := Self.Attributes.ValueFromIndex[i];
end;

function TCMDOMNode.AttributeExists(const AName: string): Boolean;
begin
  Result := False;
  Result := Self.Attributes.IndexOfName(AName) >= 0;
end;

function TCMDOMNode.DeleteAttribute(const AName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  i := Self.Attributes.IndexOfName(AName);
  if i >= 0 then
    begin
      Self.Attributes.Delete(i);
      Result := not AttributeExists(AName);
    end;
end;

end.

