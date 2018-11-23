unit uNavigatorFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_sysutils, cm_interfaces, cm_parameter, cm_AWT, cm_freegenerics, cm_tree, cm_AWTLayoutUtils,
  uAForm, uNavigator, uSystem;

type

  { TNavNode }

  TNavNode = class(TTreeNode, INavNode)
  private
    FCfg: INavNodeCfg;
    FListener: INavNodeListener;
    FNodesStyleCfg: INodesStyleCfg;
  public
    constructor Create(ACfg: INavNodeCfg; ASubsidiary: TAPanel; AParentNode: TNavNode);
    destructor Destroy; override;
    function GetBtn: TAPanel;
  public
    function GetName: string;
    function GetLevel: Integer;
    function GetParentName: string;
    function GetChildrenNames: TStrings;
    procedure SetListener(l: INavNodeListener);
    function GetCfg: INavNodeCfg;
    procedure SetNodesStyle(nsc: INodesStyleCfg);
  end;

  TNavNodeList = specialize TFGFPHashObjectList<TNavNode>;

  { TNavigatorFrame
    以 root node 作最顶层

  }

  TNavigatorFrame = class(TAPOSFrame, INavigator, IControlListener)
  private
    FRootNav: TNavNode;
    FNavNodeList: TNavNodeList;
    FGridLayout: TAGridLayout;
  private
    FBackBtn: TAPanel;
  public
    constructor Create(AOwner: TAComponent); override;
    destructor Destroy; override;
  private
    function LoadNavCfg(p: ICMParameter): INavNodeCfg;
    function LoadNodesStyleCfg(p: ICMParameter): INodesStyleCfg;
    procedure SetLayoutStyle(nsc: INodesStyleCfg);
  public
    procedure LoadConfig;
  public //INavigator
    function AddNode(const AParentNodeName: string; ANodeCfg: INavNodeCfg): Boolean;
    function RemoveNode(const ANodeName: string): Boolean;
    function FindNode(const ANodeName: string): INavNode;
    function GetRootNames: TStrings;
    procedure RefreshDisplay;
  public //IControlListener
    procedure ControlClick(e: IControlEvent);
    procedure ControlDblClick(e: IControlEvent);
    procedure ControlResize(e: IControlEvent);
  end;

  { TNavNodeEvent }

  TNavNodeEvent = class(TCMEvent, INavNodeEvent)
  private
    FNavigator: INavigator;
    FNode: INavNode;
  public
    constructor Create(ASource: TObject; ANavtor: INavigator; ANode: INavNode);
    destructor Destroy; override;
  public
    function GetNavigator: INavigator;
    function GetNode: INavNode;
  end;

const
  DEF_BtnWidth: Integer=160;
  DEF_BtnHeight: Integer=40;

implementation

{ TNavNode }

constructor TNavNode.Create(ACfg: INavNodeCfg; ASubsidiary: TAPanel; AParentNode: TNavNode);
begin
  inherited Create(ACfg.GetName, ASubsidiary, AParentNode);
  FCfg := ACfg;
  FListener := nil;
  FNodesStyleCfg := nil;
end;

destructor TNavNode.Destroy;
begin
  FCfg := nil;
  FListener := nil;
  FNodesStyleCfg := nil;
  inherited Destroy;
end;

function TNavNode.GetBtn: TAPanel;
begin
  Result := TAPanel(Self.Subsidiary);
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

procedure TNavNode.SetNodesStyle(nsc: INodesStyleCfg);
begin
  FNodesStyleCfg := nsc;
end;

{ TNavigatorFrame }

constructor TNavigatorFrame.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FNavNodeList := TNavNodeList.Create(False);
  FGridLayout := TAGridLayout.Create(nil, Self, 2, 10);
  FGridLayout.SetColsWidth(160);
  //
  FBackBtn := TAPanel.Create(Self);
  FBackBtn.Parent := Self;
  FBackBtn.Visible := False;
  FBackBtn.Color := $cfc76c;
  FGridLayout.PutLayoutControl(FBackBtn);
  FBackBtn.Width := DEF_BtnWidth;
  FBackBtn.Height := DEF_BtnHeight;
  FBackBtn.Caption := '返回';
  FBackBtn.AddControlListener(Self);
  //
  FRootNav := TNavNode.Create(TNavNodeCfg.Create('root', ''), nil, nil);
  FNavNodeList.Add('root', FRootNav);
  //load config
  LoadConfig;
end;

destructor TNavigatorFrame.Destroy;
begin
  FNavNodeList.Free;
  FGridLayout.Free;
  FRootNav.Free;
  inherited Destroy;
end;

function TNavigatorFrame.LoadNavCfg(p: ICMParameter): INavNodeCfg;
var
  nc: TNavNodeCfg;
begin
  Result := nil;
  nc := TNavNodeCfg.Create(p.Get('name').AsString, p.Get('caption').AsString);
  Result := nc;
  if (not p.Get('col').IsNull) and (not p.Get('row').IsNull) then
    nc.SetPos(p.Get('col').AsInteger, p.Get('row').AsInteger);
  if (not p.Get('width').IsNull) and (not p.Get('height').IsNull) then
    nc.SetSize(p.Get('width').AsInteger, p.Get('height').AsInteger);
  if (not p.Get('color').IsNull) then
    nc.SetColor(p.Get('color').AsInteger);
end;

function TNavigatorFrame.LoadNodesStyleCfg(p: ICMParameter): INodesStyleCfg;
var
  nsc: TNodesStyleCfg;
begin
  Result := nil;
  nsc := TNodesStyleCfg.Create;
  Result := nsc;
  if (not p.Get('colCount').IsNull) and (not p.Get('rowCount').IsNull) then
    nsc.SetCount(p.Get('colCount').AsInteger, p.Get('rowCount').AsInteger);
  if (not p.Get('colWidth').IsNull) and (not p.Get('rowHeight').IsNull) then
    nsc.SetSize(p.Get('colWidth').AsInteger, p.Get('rowHeight').AsInteger);
  nsc.SetAlign(p.Get('align').AsBoolean);
end;

procedure TNavigatorFrame.SetLayoutStyle(nsc: INodesStyleCfg);
begin
  if not Assigned(nsc) then
    Exit;
  if nsc.GetColCount > -1 then
    FGridLayout.ColCount := nsc.GetColCount;
  if nsc.GetRowCount > -1 then
    FGridLayout.RowCount := nsc.GetRowCount;
  if nsc.GetColWidth > -1 then
    FGridLayout.SetColsWidth(nsc.GetColWidth);
  if nsc.GetRowHeight > -1 then
    FGridLayout.SetRowsHeight(nsc.GetRowHeight);
  FGridLayout.AlignAtGrid := nsc.GetAlign;
  //AppSystem.GetMsgBox.ShowMessage( BoolToStr(FGridLayout.AlignAtGrid, True) );
end;

procedure TNavigatorFrame.LoadConfig;
var
  navParam: ICMParameter;
  nodesStyleCfg: INodesStyleCfg;
  i: Integer;
  procedure load(const parentName: string; nodeParam: ICMParameter);
  var
    nodeCfg: INavNodeCfg;
    nav: TNavNode;
    j: Integer;
  begin
    if nodeParam.Name <> 'node' then
      Exit;
    nodeCfg := Self.LoadNavCfg(nodeParam);
    Self.AddNode(parentName, nodeCfg);
    if not nodeParam.Get('nodes').IsNull then
      begin
        nav := FNavNodeList.Find(nodeCfg.GetName);
        if Assigned(nav) then
          begin
            nodesStyleCfg := Self.LoadNodesStyleCfg(nodeParam.Get('nodes'));
            nav.FNodesStyleCfg := nodesStyleCfg;
          end;
        for j:=0 to nodeParam.Get('nodes').ItemCount-1 do
          load(nodeCfg.GetName, nodeParam.Get('nodes').GetItem(j));
      end;
  end;
begin
  navParam := AppSystem.GetParameter.Get('navigator.nodes');
  if not navParam.IsNull then
    begin
      nodesStyleCfg := Self.LoadNodesStyleCfg(navParam);
      FRootNav.FNodesStyleCfg := nodesStyleCfg;
      Self.SetLayoutStyle(nodesStyleCfg);
      //
      for i:=0 to navParam.ItemCount-1 do
        load('', navParam.GetItem(i));
    end;
end;

function TNavigatorFrame.AddNode(const AParentNodeName: string; ANodeCfg: INavNodeCfg): Boolean;
var
  navPanel: TAPanel;
  pNav, nav: TNavNode;
begin
  Result := False;
  //找父节点
  if AParentNodeName = '' then
    pNav := FRootNav
  else
    begin
      pNav := FNavNodeList.Find(AParentNodeName);
      if not Assigned(pNav) then
        Exit;
    end;
  //创建板
  navPanel := TAPanel.Create(Self);
  if AParentNodeName <> '' then
    navPanel.Visible := False;
  navPanel.Parent := Self;
  navPanel.Mark := ANodeCfg.GetName;
  navPanel.Caption := ANodeCfg.GetCaption;
  navPanel.AddControlListener(Self);
  //创建节点
  nav := TNavNode.Create(ANodeCfg, navPanel, pNav);
  FNavNodeList.Add(ANodeCfg.GetName, nav);
  //使用配置
  if (ANodeCfg.GetCol > -1) and (ANodeCfg.GetRow > -1) then
    FGridLayout.PutLayoutControl(navPanel, ANodeCfg.GetCol, ANodeCfg.GetRow)
  else
    FGridLayout.PutLayoutControl(navPanel);
  if (ANodeCfg.GetWidth > -1) and (ANodeCfg.GetHeight > -1) then
    begin
      navPanel.Width := ANodeCfg.GetWidth;
      navPanel.Height := ANodeCfg.GetHeight;
    end
  else
    begin
      navPanel.Width := DEF_BtnWidth;
      navPanel.Height := DEF_BtnHeight;
    end;
  if ANodeCfg.GetColor > -1 then
    navPanel.Color := ANodeCfg.GetColor;
  //
  if Self.Showing then
    Self.RefreshDisplay;
end;

function TNavigatorFrame.RemoveNode(const ANodeName: string): Boolean;
begin
  Result := FNavNodeList.Remove(FNavNodeList.Find(ANodeName)) >= 0;
  //
  if Self.Showing then
    Self.RefreshDisplay;
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
  for i:=0 to FRootNav.ChildrenCount-1 do
    Result.Add(TNavNode(FRootNav.Children[i]).GetName);
end;

procedure TNavigatorFrame.RefreshDisplay;
begin
  FGridLayout.ReLayout;
end;

procedure TNavigatorFrame.ControlClick(e: IControlEvent);
var
  nav: TNavNode;
  i: Integer;
begin
  nav := FNavNodeList.Find(e.GetAControl.Mark);
  if Assigned(nav) then
    begin
      if Assigned(nav.FListener) then
        nav.FListener.Selected(TNavNodeEvent.Create(Self, Self, nav));
      //
      if e.GetAControl = FBackBtn then
        begin
          //AppSystem.GetMsgBox.ShowMessage('<<' + IntToStr(TNavNode(nav.ParentNode).FNodesStyleCfg.GetColCount)
          //                              + #10  + IntToStr(TNavNode(nav.ParentNode).FNodesStyleCfg.GetColWidth));
          Self.SetLayoutStyle(TNavNode(nav.ParentNode).FNodesStyleCfg);
          //1、显示兄弟们
          FBackBtn.Mark := TNavNode(nav.ParentNode).GetName;
          if FBackBtn.Mark = 'root' then
            FBackBtn.Visible := False;
          FBackBtn.Caption := Format('%s返回%s', [StringOfChar('.', TNavNode(nav.ParentNode).Level-1), TNavNode(nav.ParentNode).GetCfg.GetCaption]);
          for i:=0 to nav.ParentNode.ChildrenCount-1 do
            TNavNode(nav.ParentNode.Children[i]).GetBtn.Visible := True;
          //2、隐藏孩子们
          for i:=0 to nav.ChildrenCount-1 do
            TNavNode(nav.Children[i]).GetBtn.Visible := False;
        end
      else if nav.HasChildren then
        begin
          Self.SetLayoutStyle(nav.FNodesStyleCfg);
          //1、隐藏兄弟们
          for i:=0 to nav.ParentNode.ChildrenCount-1 do
            TNavNode(nav.ParentNode.Children[i]).GetBtn.Visible := False;
          //2、显示孩子们
          FBackBtn.Mark := nav.GetName;
          FBackBtn.Caption := Format('%s返回%s', [StringOfChar('.', nav.Level-1), nav.GetCfg.GetCaption]);
          FBackBtn.Visible := True;
          for i:=0 to nav.ChildrenCount-1 do
            TNavNode(nav.Children[i]).GetBtn.Visible := True;
          //
          if Assigned(nav.FListener) then
            nav.FListener.Opened(TNavNodeEvent.Create(Self, Self, nav));
        end;
      //重新布局
      FGridLayout.ReLayout;
    end;
end;

procedure TNavigatorFrame.ControlDblClick(e: IControlEvent);
begin

end;

procedure TNavigatorFrame.ControlResize(e: IControlEvent);
begin

end;

{ TNavNodeEvent }

constructor TNavNodeEvent.Create(ASource: TObject; ANavtor: INavigator; ANode: INavNode);
begin
  inherited Create(ASource);
  FNavigator := ANavtor;
  FNode := ANode;
end;

destructor TNavNodeEvent.Destroy;
begin
  FNavigator := nil;
  FNode := nil;
  inherited Destroy;
end;

function TNavNodeEvent.GetNavigator: INavigator;
begin
  Result := FNavigator;
end;

function TNavNodeEvent.GetNode: INavNode;
begin
  Result := FNode;
end;



end.

