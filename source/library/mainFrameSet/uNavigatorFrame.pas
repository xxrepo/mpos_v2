unit uNavigatorFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_AWT, cm_freegenerics, cm_tree, cm_AWTLayoutUtils,
  uAForm, uNavigator;

type

  { TNavNode }

  TNavNode = class(TTreeNode, INavNode)
  private
    FCfg: INavNodeCfg;
    FListener: INavNodeListener;
    FGridLayout: TAGridLayout;
  public
    constructor Create(ACfg: INavNodeCfg; ASubsidiary: TAPanel; AParentNode: TNavNode);
    destructor Destroy; override;
    function GetBtn: TAPanel;
    procedure ReLayoutChildren;
  public
    function GetName: string;
    function GetLevel: Integer;
    function GetParentName: string;
    function GetChildrenNames: TStrings;
    procedure SetListener(l: INavNodeListener);
    function GetCfg: INavNodeCfg;
  end;

  TNavNodeList = specialize TFGFPHashObjectList<TNavNode>;

  { TNavigatorFrame }

  TNavigatorFrame = class(TAPOSFrame, INavigator)
  private
    FNavNodeList: TNavNodeList;
    FGridLayout: TAGridLayout;
  public
    constructor Create(AOwner: TAComponent); override;
    destructor Destroy; override;
  public //INavigator
    function AddNode(const AParentNodeName: string; INavNodeCfg: INavNodeCfg): Boolean;
    function RemoveNode(const ANodeName: string): Boolean;
    function FindNode(const ANodeName: string): INavNode;
    function GetRootNames: TStrings;
  end;

  { TCfg }

  TCfg = class(TInterfacedObject, INavNodeCfg)
  public
    function GetName: string;
    function GetCaption: string;
    function GetCol: Integer;
    function GetRow: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
  end;

implementation

{ TNavNode }

constructor TNavNode.Create(ACfg: INavNodeCfg; ASubsidiary: TAPanel; AParentNode: TNavNode);
begin
  inherited Create(ACfg.GetName, ASubsidiary, AParentNode);
  FCfg := ACfg;
  FListener := nil;
  FGridLayout := TAGridLayout.Create(nil, ASubsidiary);
end;

destructor TNavNode.Destroy;
begin
  FListener := nil;
  FGridLayout.Free;
  inherited Destroy;
end;

function TNavNode.GetBtn: TAPanel;
begin
  Result := TAPanel(Self.Subsidiary);
end;

procedure TNavNode.ReLayoutChildren;
begin
  FGridLayout.ReLayout;
end;

function TNavNode.GetName: string;
begin
  Result := Self.Name;
end;

function TNavNode.GetLevel: Integer;
begin
  Result := Self.Level;
end;

function TNavNode.GetParentName: string;
begin
  Result := '';
  if Assigned(Self.ParentNode) then
    Result := Self.ParentNode.Name;
end;

function TNavNode.GetChildrenNames: TStrings;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i:=0 to Self.ChildrenCount-1 do
    Result.Add(Self.Children[i].Name);
end;

procedure TNavNode.SetListener(l: INavNodeListener);
begin
  FListener := l;
end;

function TNavNode.GetCfg: INavNodeCfg;
begin
  Result := FCfg;
end;

{ TNavigatorFrame }

constructor TNavigatorFrame.Create(AOwner: TAComponent);
var
  e: TAEdit;
begin
  inherited Create(AOwner);
  FNavNodeList := TNavNodeList.Create(False);
  FGridLayout := TAGridLayout.Create(nil, Self);
  //
  Self.Caption := 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
  e := TAEdit.Create(Self);
  e.Parent := Self;
  e.Left := 0;

  Self.AddNode('', TCfg.Create);
end;

destructor TNavigatorFrame.Destroy;
begin
  FNavNodeList.Free;
  FGridLayout.Free;
  inherited Destroy;
end;

function TNavigatorFrame.AddNode(const AParentNodeName: string; INavNodeCfg: INavNodeCfg): Boolean;
var
  navPanel: TAPanel;
  pNav, nav: TNavNode;
begin
  Result := False;
  navPanel := TAPanel.Create(Self);
  pNav := nil;
  if (AParentNodeName <> '') then
    begin
      pNav := FNavNodeList.Find(AParentNodeName);
      if not Assigned(pNav) then
        begin
          navPanel.Free;
          Exit;
        end;
      navPanel.Parent := pNav.GetBtn;
    end
  else
    navPanel.Parent := Self;
  //
  nav := TNavNode.Create(INavNodeCfg, navPanel, pNav);
  FNavNodeList.Add(INavNodeCfg.GetName, nav);
  navPanel.Caption := INavNodeCfg.GetCaption;
  if (INavNodeCfg.GetWidth > -1) and (INavNodeCfg.GetHeight > -1) then
    begin
      navPanel.Width := INavNodeCfg.GetWidth;
      navPanel.Height := INavNodeCfg.GetHeight;
    end;
end;

function TNavigatorFrame.RemoveNode(const ANodeName: string): Boolean;
begin
  Result := FNavNodeList.Remove(FNavNodeList.Find(ANodeName)) >= 0;
end;

function TNavigatorFrame.FindNode(const ANodeName: string): INavNode;
begin
  Result := FNavNodeList.Find(ANodeName);
end;

function TNavigatorFrame.GetRootNames: TStrings;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i:=0 to FNavNodeList.Count-1 do
    if Assigned(FNavNodeList[i].ParentNode) then
      Result.Add(FNavNodeList[i].GetName);
end;

{ TCfg }

function TCfg.GetName: string;
begin
  Result := 'haha';
end;

function TCfg.GetCaption: string;
begin
  Result := 'haha vv';
end;

function TCfg.GetCol: Integer;
begin
  Result := 1;
end;

function TCfg.GetRow: Integer;
begin
  Result := 1;
end;

function TCfg.GetWidth: Integer;
begin
  Result := 60;
end;

function TCfg.GetHeight: Integer;
begin
  Result := 40;
end;



end.

