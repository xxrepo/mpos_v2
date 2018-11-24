unit uNavigator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces;

type

  INavigator = interface;
  INavNode = interface;

  { INodesStyleCfg
    用于设置节点下的子节点的样式（可行）。
  }

  INodesStyleCfg = interface
    ['{035B3A37-B02B-4336-8805-DEFD56B04BFF}']
    function GetColCount: Integer;
    function GetRowCount: Integer;
    function GetColWidth: Integer;
    function GetRowHeight: Integer;
    function GetAlign: Boolean;
    function GetLeftSpacing: Integer;
    function GetTopSpacing: Integer;
  end;

  { INavNodeCfg
    一个节点的配置，动态新增需要。
  }

  INavNodeCfg = interface
    ['{CAFC4D36-F850-402C-B9BC-043DC96A952F}']
    function GetName: string;  //名字应唯一
    function GetCaption: string;
    function GetCol: Integer;  //未配置时为-1
    function GetRow: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetColor: Integer;
  end;

  { INavNodeEvent }

  INavNodeEvent = interface(ICMEvent)
    ['{93409BB9-432E-4452-B9AD-6CB57218FACD}']
    function GetNavigator: INavigator;
    function GetNode: INavNode;
  end;

  { INavNodeListener }

  INavNodeListener = interface(ICMListener)
    ['{FAF46D80-DD92-4620-9ABD-2E70DF640547}']
    procedure Selected(e: INavNodeEvent);
    procedure Opened(e: INavNodeEvent);
  end;

  { INavNode
    代表一个导航节点，它是树形结构的。
  }

  INavNode = interface
    ['{65BE3B79-9C92-4667-927B-2FB4DC70E26E}']
    function GetName: string;   //名字应唯一
    function GetLevel: Integer;
    function GetParentName: string;
    function GetChildrenNames: TStrings;
    procedure SetListener(l: INavNodeListener);
    function GetCfg: INavNodeCfg;
    procedure SetNodesStyle(nsc: INodesStyleCfg);
  end;

  { INavigator }

  INavigator = interface(ICMBase)
    ['{422F5AEA-5FC0-44ED-A28E-5CD4FB2D526F}']
    function AddNode(const AParentNodeName: string; ANodeCfg: INavNodeCfg): Boolean;
    function RemoveNode(const ANodeName: string): Boolean;
    function FindNode(const ANodeName: string): INavNode;
    function GetRootNames: TStrings;
    procedure RefreshDisplay; //刷新显示，当更改样式后需要。
  end;

  { TNodesStyleCfg }

  TNodesStyleCfg = class(TInterfacedObject, INodesStyleCfg)
  private
    FColCount: Integer;
    FRowCount: Integer;
    FColWidth: Integer;
    FRowHeight: Integer;
    FAlign: Boolean;
    FLeftSpacing: Integer;
    FTopSpacing: Integer;
  public
    constructor Create;
    property ColCount: Integer write FColCount;
    property RowCount: Integer write FRowCount;
    property ColWidth: Integer write FColWidth;
    property RowHeight: Integer write FRowHeight;
    property Align: Boolean write FAlign;
    property LeftSpacing: Integer write FLeftSpacing;
    property TopSpacing: Integer write FTopSpacing;
  public
    function GetColCount: Integer;
    function GetRowCount: Integer;
    function GetColWidth: Integer;
    function GetRowHeight: Integer;
    function GetAlign: Boolean;
    function GetLeftSpacing: Integer;
    function GetTopSpacing: Integer;
  end;

  { TNavNodeCfg }

  TNavNodeCfg = class(TInterfacedObject, INavNodeCfg)
  private
    FName: string;
    FCaption: string;
    FCol: Integer;
    FRow: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FColor: Integer;
  public
    constructor Create(const AName, ACaption: string);
    procedure SetPos(c, r: Integer);
    property Width: Integer write FWidth;
    property Height: Integer write FHeight;
    property Color: Integer write FColor;
  public
    function GetName: string;
    function GetCaption: string;
    function GetCol: Integer;
    function GetRow: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetColor: Integer;
  end;

  { TNavNodeAdapter }

  TNavNodeAdapter = class(TCMListener, ICMListener)
  public
    procedure Selected(e: INavNodeEvent); virtual;
    procedure Opened(e: INavNodeEvent); virtual;
  end;

implementation


{ TNodesStyleCfg }

constructor TNodesStyleCfg.Create;
begin
  inherited Create;
  FColCount := -1;
  FRowCount := -1;
  FColWidth := -1;
  FRowHeight := -1;
  FAlign := False;
  FLeftSpacing := -1;
  FTopSpacing := -1;
end;

function TNodesStyleCfg.GetColCount: Integer;
begin
  Result := FColCount;
end;

function TNodesStyleCfg.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

function TNodesStyleCfg.GetColWidth: Integer;
begin
  Result := FColWidth;
end;

function TNodesStyleCfg.GetRowHeight: Integer;
begin
  Result := FRowHeight;
end;

function TNodesStyleCfg.GetAlign: Boolean;
begin
  Result := FAlign;
end;

function TNodesStyleCfg.GetLeftSpacing: Integer;
begin
  Result := FLeftSpacing;
end;

function TNodesStyleCfg.GetTopSpacing: Integer;
begin
  Result := FTopSpacing;
end;

{ TNavNodeCfg }

constructor TNavNodeCfg.Create(const AName, ACaption: string);
begin
  inherited Create;
  FName := AName;
  FCaption := ACaption;
  FCol := -1;
  FRow := -1;
  FWidth := -1;
  FHeight := -1;
  FColor := -$7FFFFFFF;
end;

procedure TNavNodeCfg.SetPos(c, r: Integer);
begin
  FCol := c;
  FRow := r;
end;

function TNavNodeCfg.GetName: string;
begin
  Result := FName;
end;

function TNavNodeCfg.GetCaption: string;
begin
  Result := FCaption;
end;

function TNavNodeCfg.GetCol: Integer;
begin
  Result := FCol;
end;

function TNavNodeCfg.GetRow: Integer;
begin
  Result := FRow;
end;

function TNavNodeCfg.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TNavNodeCfg.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TNavNodeCfg.GetColor: Integer;
begin
  Result := FColor;
end;

{ TNavNodeAdapter }

procedure TNavNodeAdapter.Selected(e: INavNodeEvent);
begin

end;

procedure TNavNodeAdapter.Opened(e: INavNodeEvent);
begin

end;

end.

