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
  public
    constructor Create;
    procedure SetCount(c, r: Integer);
    procedure SetSize(w, h: Integer);
    procedure SetAlign(b: Boolean);
  public
    function GetColCount: Integer;
    function GetRowCount: Integer;
    function GetColWidth: Integer;
    function GetRowHeight: Integer;
    function GetAlign: Boolean;
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
    procedure SetSize(w, h: Integer);
    procedure SetColor(c: Integer);
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
end;

procedure TNodesStyleCfg.SetCount(c, r: Integer);
begin
  FColCount := c;
  FRowCount := r;
end;

procedure TNodesStyleCfg.SetSize(w, h: Integer);
begin
  FColWidth := w;
  FRowHeight := h;
end;

procedure TNodesStyleCfg.SetAlign(b: Boolean);
begin
  FAlign := b;
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

procedure TNavNodeCfg.SetSize(w, h: Integer);
begin
  FWidth := w;
  FHeight := h;
end;

procedure TNavNodeCfg.SetColor(c: Integer);
begin
  FColor := c;
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

