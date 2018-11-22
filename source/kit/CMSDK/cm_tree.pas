unit cm_tree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TTreeNode }

  TTreeNode = class(TInterfacedObject)
  private
    FName: string;
    FSubsidiary: Pointer;
    FParentNode: TTreeNode;
    FLevel: Integer;
    FChildrenList: TList;
    function GetChildren(AIndex: Integer): TTreeNode;
    function GetChildrenCount: Integer;
    function GetFirstChild: TTreeNode;
    function GetLastChild: TTreeNode;
    function GetNextSibling: TTreeNode;
    function GetPreviousSibling: TTreeNode;
  public
    constructor Create(const AName: string; ASubsidiary: Pointer=nil; AParentNode: TTreeNode=nil);
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Subsidiary: Pointer read FSubsidiary write FSubsidiary;
    property ParentNode: TTreeNode read FParentNode;
    property Level: Integer read FLevel;
  protected
    property ChildrenList: TList read FChildrenList;
  public
    function HasChildren: Boolean;
    property ChildrenCount: Integer read GetChildrenCount;
    property Children[AIndex: Integer]: TTreeNode read GetChildren;
    property FirstChild: TTreeNode read GetFirstChild;
    property LastChild: TTreeNode read GetLastChild;
    function AppendChild(NewChild: TTreeNode): Integer;
    function RemoveChild(OldChild: TTreeNode): Integer; overload;
    function RemoveChild(const OldChildName: string): Integer; overload;
    function FindChildNode(const AName: string): TTreeNode;
  public
    property PreviousSibling: TTreeNode read GetPreviousSibling;
    property NextSibling: TTreeNode read GetNextSibling;
    function FindNode(const AName: string): TTreeNode;
  end;

implementation

{ TTreeNode }

function TTreeNode.GetChildren(AIndex: Integer): TTreeNode;
begin
  Result := TTreeNode(FChildrenList[AIndex]);
end;

function TTreeNode.GetChildrenCount: Integer;
begin
  Result := FChildrenList.Count;
end;

function TTreeNode.GetFirstChild: TTreeNode;
begin
  Result := nil;
  if FChildrenList.Count > 0 then
    Result := TTreeNode(FChildrenList[0]);
end;

function TTreeNode.GetLastChild: TTreeNode;
begin
  Result := nil;
  if FChildrenList.Count > 0 then
    Result := TTreeNode(FChildrenList[FChildrenList.Count-1]);
end;

function TTreeNode.GetNextSibling: TTreeNode;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(Self.ParentNode) then
    begin
      i := Self.ParentNode.FChildrenList.IndexOf(Self);
      if i < Self.ParentNode.FChildrenList.Count - 1 then
        Result := TTreeNode(Self.ParentNode.FChildrenList[i + 1]);
    end;
end;

function TTreeNode.GetPreviousSibling: TTreeNode;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(Self.ParentNode) then
    begin
      i := Self.ParentNode.FChildrenList.IndexOf(Self);
      if i > 0 then
        Result := TTreeNode(Self.ParentNode.FChildrenList[i - 1]);
    end;
end;

constructor TTreeNode.Create(const AName: string; ASubsidiary: Pointer; AParentNode: TTreeNode);
begin
  FName := AName;
  FSubsidiary := ASubsidiary;
  FParentNode := AParentNode;
  if Assigned(FParentNode) then
    begin
      FParentNode.AppendChild(Self);
      FLevel := FParentNode.Level + 1;
    end
  else
    FLevel := 1;
  FChildrenList := TList.Create;
end;

destructor TTreeNode.Destroy;
var
  i: Integer;
begin
  for i:=0 to FChildrenList.Count-1 do
    TTreeNode(FChildrenList[i]).Free;
  FChildrenList.Free;
  inherited Destroy;
end;

function TTreeNode.HasChildren: Boolean;
begin
  Result := FChildrenList.Count > 0;
end;

function TTreeNode.FindChildNode(const AName: string): TTreeNode;
var
  i: Integer;
begin
  Result := nil;
  for i:=0 to FChildrenList.Count-1 do
    if TTreeNode(FChildrenList[i]).Name = AName then
      begin
        Result := TTreeNode(FChildrenList[i]);
        Exit;
      end;
end;

function TTreeNode.AppendChild(NewChild: TTreeNode): Integer;
begin
  Result := FChildrenList.Add(NewChild);
end;

function TTreeNode.RemoveChild(OldChild: TTreeNode): Integer;
begin
  Result := FChildrenList.Remove(OldChild);
end;

function TTreeNode.RemoveChild(const OldChildName: string): Integer;
var
  node: TTreeNode;
begin
  Result := -1;
  node := FindChildNode(OldChildName);
  if Assigned(node) then
    Result := RemoveChild(node);
end;

function TTreeNode.FindNode(const AName: string): TTreeNode;
var
  root: TTreeNode;
  function f(n: TTreeNode): TTreeNode;
  var tmp: TTreeNode;
  begin
    Result := nil;
    if not Assigned(n) then
      Exit;
    Result := n.FindChildNode(AName);
    if Assigned(Result) then
      Exit;
    tmp := n.FirstChild;
    while Assigned(tmp) do
      begin
        Result := f(tmp);
        if Assigned(Result) then
          Exit;
        tmp := tmp.NextSibling;
      end;
  end;
begin
  Result := nil;
  root := Self;
  while Assigned(root.ParentNode) do
    root := root.ParentNode;
  if root.Name = AName then
    begin
      Result := root;
      Exit;
    end;
  Result := f(root);
end;

end.

