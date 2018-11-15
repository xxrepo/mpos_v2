unit uFrameNavigator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics,  Dialogs,
  cm_interfaces, cm_theme, cm_controlutils, cm_parameter, cm_messager, cm_freegenerics, cm_plat,
  uFrame, uSystem,
  uNavigator;

type

  TNavigationListPanel = class;

  { TNavigatorFrame }

  TNavigatorFrame = class(TPOSFrame, IThemeable, INavigator)
    PanelBottom: TPanel;
    PanelTop: TPanel;
    procedure FrameResize(Sender: TObject);
  private
    FListPanel: TNavigationListPanel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadConfig;
  public
    procedure SetTheme(ATheme: ITheme); override;
  public
    function AddNode(AParentNode: INavigationNode; ANewNodeConfig: INavigationNodeConfig): Boolean;
    function RemoveNode(ANode: INavigationNode): Boolean;
    function GetNode(const ANodeName: string): INavigationNode;
    function GetRootNames: TStrings;
  end;

  { TNavigationNodeConfig }

  TNavigationNodeConfig = class(TCMBase, INavigationNodeConfig)
  private
    FName: string;
    FCaption: string;
    FCol: Integer;
    FRow: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FCall: string;
    FChildrenColWidth: Integer;
    FChildrenRowHeight: Integer;
    FChildrenColCount: Integer;
    FChildrenRowCount: Integer;
    FChildrenAlignAtGrid: Boolean;
  public
    constructor Create(const AName, ACaption: string);
    procedure SetPos(ACol, ARow: Integer);
    procedure SetSize(w, h: Integer);
    procedure SetChildrenSize(w, h: Integer);
    procedure SetChildrenCount(c, r: Integer);
    procedure SetChildrenAlignAtGrid(b: Boolean);
  public
    function GetName: string;  //名字应唯一
    function GetCaption: string;
    function GetCol: Integer;  //未配置时返回实现分配
    function GetRow: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetCall: string;  //保留
    function GetChildrenColWidth: Integer;
    function GetChildrenRowHeight: Integer;
    function GetChildrenColCount: Integer;
    function GetChildrenRowCount: Integer;
    function GetChildrenAlignAtGrid: Boolean;
  end;

  { TNavigatorNodeEvent }

  TNavigatorNodeEvent = class(TCMEvent, INavigatorNodeEvent)
  private
    FNavigator: INavigator;
    FNode: INavigationNode;
  public
    constructor Create(ASource: TObject; ANavigator: INavigator; ANode: INavigationNode); reintroduce;
  public
    function GetNavigator: INavigator;
    function GetNode: INavigationNode;
  end;

  TNavigationNodeList = specialize TFGInterfaceList<INavigationNode>;

  { TNavigationNodePanel }

  TNavigationNodePanel = class(TPanel, INavigationNode)
  private
    FConfig: INavigationNodeConfig;
    FLevel: Integer;
    FParent: INavigationNode;
    FChildrenPanel: TNavigationListPanel;
    FIsDblOpenChildren: Boolean;
    FListener: INavigatorNodeListener;
    procedure ClickEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; AParent: INavigationNode; AConfig: INavigationNodeConfig); reintroduce;
    destructor Destroy; override;
  public
    function GetConfig: INavigationNodeConfig;
    function GetLevel: Integer;
    function GetParentName: string;
    function GetChildrenNames: TStrings;
    procedure SetDblOpenChildren(b: Boolean); //设置双击打开子项
    function IsDblOpenChildren: Boolean;
    procedure Show;
    procedure SetListener(l: INavigatorNodeListener);
  end;

  { TNavigationListPanel }

  TNavigationListPanel = class(TPanel)
  private
    FNodeList: TNavigationNodeList;
    FNodeNames: TStrings;
    FHostingMenu: TCMGridLayoutHostingMenu;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodeNames: TStrings;
    procedure AddNodePanel(np: TNavigationNodePanel);
  end;


implementation

{$R *.frm}

var
  _NavigatorFrame: TNavigatorFrame = nil;

{ TNavigatorFrame }

procedure TNavigatorFrame.FrameResize(Sender: TObject);
begin
  FListPanel.FHostingMenu.ReLayout;
end;

constructor TNavigatorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListPanel := TNavigationListPanel.Create(Self);
  FListPanel.Parent := Self;
  FListPanel.Align := alClient;
  FListPanel.BevelOuter := bvNone;
  //
  _NavigatorFrame := Self;
  InterfaceRegister.PutInterface('INavigator', INavigator, Self);
end;

procedure SetStyle(menu: TCMGridLayoutMenu; param: ICMParameter);
begin
  menu.TopSpacing := 4;
  menu.LeftSpacing := 4;
  menu.BorderSpacing := 4;
  menu.SetColWidth(param.Get('ColWidth').AsInteger);
  menu.SetRowHeight(param.Get('RowHeight').AsInteger);
  menu.ColCount := param.Get('ColCount').AsInteger;
  menu.RowCount := param.Get('RowCount').AsInteger;
  menu.SelectBoxSpacing := 1;
  menu.AlignAtGrid := param.Get('AlignAtGrid').AsBoolean;
end;

procedure SetChildren(listPanel: TNavigationListPanel; param: ICMParameter);
var
  i: Integer;
  ip, cp: ICMParameter;
  np: TNavigationNodePanel;
  cfg: TNavigationNodeConfig;
begin
  for i:=0 to param.ItemCount-1 do
    begin
      ip := param.GetItem(i);
      if ip.Name <> 'Item' then
        Continue;
      cfg := TNavigationNodeConfig.Create(ip.Get('Name').AsString, ip.Get('Caption').AsString);
      //cfg.SetPos(ip.Get('Col').AsInteger, ip.Get('Row').AsInteger);
      cfg.SetSize(ip.Get('Width').AsInteger, ip.Get('Height').AsInteger);
      cp := ip.Get('Items.Style');
      if not cp.IsNull then
        begin
          cfg.SetChildrenSize(cp.Get('ColWidth').AsInteger, cp.Get('RowHeight').AsInteger);
          cfg.SetChildrenCount(cp.Get('ColCount').AsInteger, cp.Get('RowCount').AsInteger);
          cfg.SetChildrenAlignAtGrid(cp.Get('AlignAtGrid').AsBoolean);
        end;
      np := TNavigationNodePanel.Create(listPanel, nil, cfg);
      listPanel.AddNodePanel(np);
      //
      cp := ip.Get('Items');
      if not cp.IsNull then
        begin
          SetChildren(np.FChildrenPanel, cp);
        end;
    end;
  listPanel.FHostingMenu.ReLayout;
end;

procedure TNavigatorFrame.LoadConfig;
var
  p: ICMParameter;
begin
  p := AppSystem.GetParameter.Get('Navigator.Items.Style');
  if not p.IsNull then
    SetStyle(FListPanel.FHostingMenu, p);
  p := AppSystem.GetParameter.Get('Navigator.Items');
  if not p.IsNull then
    begin
      SetChildren(FListPanel, p);
    end;
end;

procedure TNavigatorFrame.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  PanelTop.Color := ATheme.GetParameter.Get('navigator.topColor').AsInteger;
  //PanelClient.Color := ATheme.GetParameter.Get('panelColor').AsInteger;
  PanelBottom.Color := ATheme.GetParameter.Get('navigator.bottomColor').AsInteger;
end;

function TNavigatorFrame.AddNode(AParentNode: INavigationNode; ANewNodeConfig: INavigationNodeConfig): Boolean;
begin
  Result := False;
end;

function TNavigatorFrame.RemoveNode(ANode: INavigationNode): Boolean;
begin
  Result := False;
end;

function TNavigatorFrame.GetNode(const ANodeName: string): INavigationNode;
var
  i: Integer;
begin
  // TODO
  for i:=0 to FListPanel.FNodeList.Count-1 do
    begin
      DefaultMessager.Debug('222222--%s', [FListPanel.FNodeList[i].GetConfig.GetName]);
      if FListPanel.FNodeList[i].GetConfig.GetName = ANodeName then
        begin
          Result := FListPanel.FNodeList[i];
          Exit;
        end;
    end;
end;

function TNavigatorFrame.GetRootNames: TStrings;
begin
  Result := FListPanel.GetNodeNames;
end;

{ TNavigationNodeConfig }

constructor TNavigationNodeConfig.Create(const AName, ACaption: string);
begin
  inherited Create;
  FName := AName;
  FCaption := ACaption;
  FWidth := 80;
  FHeight := 40;
  FChildrenColWidth := 80;
  FChildrenRowHeight := 40;
  FChildrenColCount := 10;
  FChildrenRowCount := 1;
  FChildrenAlignAtGrid := False;
end;

procedure TNavigationNodeConfig.SetPos(ACol, ARow: Integer);
begin
  // TODO 加入菜单后重设
  FCol := ACol;
  FRow := ARow;
end;

procedure TNavigationNodeConfig.SetSize(w, h: Integer);
begin
  FWidth := w;
  FHeight := h;
end;

procedure TNavigationNodeConfig.SetChildrenSize(w, h: Integer);
begin
  FChildrenColWidth := w;
  FChildrenRowHeight := h;
end;

procedure TNavigationNodeConfig.SetChildrenCount(c, r: Integer);
begin
  FChildrenColCount := c;
  FChildrenRowCount := r;
end;

procedure TNavigationNodeConfig.SetChildrenAlignAtGrid(b: Boolean);
begin
  FChildrenAlignAtGrid := b;
end;

function TNavigationNodeConfig.GetName: string;
begin
  Result := FName;
end;

function TNavigationNodeConfig.GetCaption: string;
begin
  Result := FCaption;
end;

function TNavigationNodeConfig.GetCol: Integer;
begin
  Result := FCol;
end;

function TNavigationNodeConfig.GetRow: Integer;
begin
  Result := FRow;
end;

function TNavigationNodeConfig.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TNavigationNodeConfig.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TNavigationNodeConfig.GetCall: string;
begin
  Result := FCall;
end;

function TNavigationNodeConfig.GetChildrenColWidth: Integer;
begin
  Result := FChildrenColWidth;
end;

function TNavigationNodeConfig.GetChildrenRowHeight: Integer;
begin
  Result := FChildrenRowHeight;
end;

function TNavigationNodeConfig.GetChildrenColCount: Integer;
begin
  Result := FChildrenColCount;
end;

function TNavigationNodeConfig.GetChildrenRowCount: Integer;
begin
  Result := FChildrenRowCount;
end;

function TNavigationNodeConfig.GetChildrenAlignAtGrid: Boolean;
begin
  Result := FChildrenAlignAtGrid;
end;

{ TNavigationNodePanel }

procedure TNavigationNodePanel.ClickEvent(Sender: TObject);
begin
  if Assigned(FListener) then
    FListener.Click(TNavigatorNodeEvent.Create(Self, _NavigatorFrame, Self));
end;

procedure TNavigationNodePanel.DblClickEvent(Sender: TObject);
begin
  if FChildrenPanel.FNodeList.Count > 0 then
    begin
      FChildrenPanel.Parent := Self.Parent;
      FChildrenPanel.Align := alClient;
      FChildrenPanel.FHostingMenu.ReLayout;
    end;
  if Assigned(FListener) then
    FListener.DblClick(TNavigatorNodeEvent.Create(Self, _NavigatorFrame, Self));
end;

constructor TNavigationNodePanel.Create(AOwner: TComponent; AParent: INavigationNode; AConfig: INavigationNodeConfig);
begin
  inherited Create(AOwner);
  FConfig := AConfig;
  if Assigned(AParent) then
    FLevel := AParent.GetLevel + 1
  else
    FLevel := 1;
  FParent := AParent;
  FChildrenPanel := TNavigationListPanel.Create(Self);
  FIsDblOpenChildren := False;
  FListener := nil;
  //
  Self.Caption := AConfig.GetCaption;
  Self.Width := AConfig.GetWidth;
  Self.Height := AConfig.GetHeight;
  //
  FChildrenPanel.FHostingMenu.TopSpacing := 4;
  FChildrenPanel.FHostingMenu.LeftSpacing := 4;
  FChildrenPanel.FHostingMenu.BorderSpacing := 4;
  FChildrenPanel.FHostingMenu.SetColWidth(AConfig.GetChildrenColWidth);
  FChildrenPanel.FHostingMenu.SetRowHeight(AConfig.GetChildrenRowHeight);
  FChildrenPanel.FHostingMenu.ColCount := AConfig.GetChildrenColCount;
  FChildrenPanel.FHostingMenu.RowCount := AConfig.GetChildrenRowCount;
  FChildrenPanel.FHostingMenu.SelectBoxSpacing := 1;
  FChildrenPanel.FHostingMenu.AlignAtGrid := AConfig.GetChildrenAlignAtGrid;
  //
  Self.OnClick := @ClickEvent;
  Self.OnDblClick := @DblClickEvent;
end;

destructor TNavigationNodePanel.Destroy;
begin
  FChildrenPanel.Free;
  inherited Destroy;
end;

function TNavigationNodePanel.GetConfig: INavigationNodeConfig;
begin
  Result := FConfig;
end;

function TNavigationNodePanel.GetLevel: Integer;
begin
  Result := FLevel;
end;

function TNavigationNodePanel.GetParentName: string;
begin
  Result := FParent.GetConfig.GetName;
end;

function TNavigationNodePanel.GetChildrenNames: TStrings;
begin
  Result := FChildrenPanel.GetNodeNames;
end;

procedure TNavigationNodePanel.SetDblOpenChildren(b: Boolean);
begin
  FIsDblOpenChildren := b;
end;

function TNavigationNodePanel.IsDblOpenChildren: Boolean;
begin
  Result := FIsDblOpenChildren;
end;

procedure TNavigationNodePanel.Show;
begin
  //TODO
end;

procedure TNavigationNodePanel.SetListener(l: INavigatorNodeListener);
begin
  FListener := l;
end;

{ TNavigationListPanel }

constructor TNavigationListPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNodeList := TNavigationNodeList.Create;
  FNodeNames := nil;
  FHostingMenu := TCMGridLayoutHostingMenu.Create(Self);
end;

destructor TNavigationListPanel.Destroy;
begin
  FNodeList.Free;
  inherited Destroy;
end;

function TNavigationListPanel.GetNodeNames: TStrings;
var
  i: Integer;
begin
  Result := nil;
  if not Assigned(FNodeNames) then
    FNodeNames := TStringList.Create;
  FNodeNames.Clear;
  for i:=0 to FNodeList.Count-1 do
    FNodeNames.Add(FNodeList[i].GetConfig.GetName);
  Result := FNodeNames;
end;

procedure TNavigationListPanel.AddNodePanel(np: TNavigationNodePanel);
begin
  np.Parent := Self;
  FHostingMenu.PutLayoutControl(np);
  FNodeList.Add(np);
end;

{ TNavigatorNodeEvent }

constructor TNavigatorNodeEvent.Create(ASource: TObject; ANavigator: INavigator; ANode: INavigationNode);
begin
  inherited Create(ASource);
  FNavigator := ANavigator;
  FNode := ANode;
end;

function TNavigatorNodeEvent.GetNavigator: INavigator;
begin
  Result := FNavigator;
end;

function TNavigatorNodeEvent.GetNode: INavigationNode;
begin
  Result := FNode;
end;

end.

