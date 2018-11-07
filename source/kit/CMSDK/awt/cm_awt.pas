unit cm_awt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_AWTEvent;

type

  { TColor、TCaption 定义是依赖 LCL 的。}

  TAColor = -$7FFFFFFF-1..$7FFFFFFF;
  TACaption = type String;

  TAWinControl = class;

  { IAComponentPeer
    // The peer interface for . This is the top level peer interface for widgets and defines the
    // bulk of methods for AWT component peers. Most component peers have to implement this
    // interface (via one of the subinterfaces).
    // <br/>
    // The peer interfaces are intended only for use in porting the AWT. They are not intended for
    // use by application developers, and developers should not implement peers nor invoke any of
    // the peer methods directly on the peer instances.
  }

  IAPeer = interface
    ['{1ED8E4BF-2896-4971-8485-FA93466075FD}']
    function GetDelegate: TObject;
  end;

  IAComponentPeer = interface(IAPeer)
    ['{F5955633-8025-47AE-87EB-A53E240AC20C}']
    function GetName: TComponentName;
    procedure SetName(AValue: TComponentName);
    function GetTag: PtrInt;
    procedure SetTag(AValue: PtrInt);
  end;

  IAControlPeer = interface(IAComponentPeer)
    ['{3EFC4422-89E2-4997-AE84-911B0B7E4017}']
    function GetText: TACaption;
    procedure SetText(AValue: TACaption);
    function GetColor: TAColor;
    procedure SetColor(AValue: TAColor);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    function GetLeft: Integer;
    procedure SetLeft(AValue: Integer);
    function GetTop: Integer;
    procedure SetTop(AValue: Integer);
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    procedure SetParent(AValue: TAWinControl);
  end;

  IAWinControlPeer = interface(IAControlPeer)
    ['{F34E9CA5-AFE6-4AA0-8D35-FD81DC1A6956}']
    function CanFocus: Boolean;
    function CanSetFocus: Boolean;
    procedure SetFocus;
    procedure AddKeyListener(l: IKeyListener);
  end;

  IACustomControlPeer = interface(IAWinControlPeer)
    ['{78B50173-8CA7-4176-A6BD-404570BDD6DE}']
    //property Canvas: TCanvas read FCanvas write FCanvas;
    //property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  IAPanelPeer = interface(IACustomControlPeer)
    ['{4BA1BB04-559F-46AF-BD05-D5CB2D2DA227}']

  end;

  IAEditPeer = interface(IAWinControlPeer)
    ['{3A9FCF75-9370-4F05-B737-B5A08266C40F}']
    procedure Clear;
    procedure SelectAll;
  end;

  IAFormPeer = interface(IAWinControlPeer)
    ['{5A8A620E-272E-4724-B339-282802CE878E}']
    function ShowModal: Integer;
  end;

  TAObject = class
  end;

  { TAComponent }

  TAComponent = class(TAObject)
  private
    FPeer: IAComponentPeer;
    FOwner: TAComponent;
    FComponents: TFPList;
    function GetComponent(AIndex: Integer): TAComponent;
    function GetComponentCount: Integer;
  public
    constructor Create(AOwner: TAComponent); virtual;
    destructor Destroy; override;
    procedure DestroyComponents;
    procedure InsertComponent(AComponent: TAComponent);
    procedure RemoveComponent(AComponent: TAComponent);
    property Components[AIndex: Integer]: TAComponent read GetComponent;
    property ComponentCount: Integer read GetComponentCount;
    property Owner: TAComponent read FOwner;
    function GetPeer: IAComponentPeer;
  private
    function GetName: TComponentName;
    procedure SetName(AValue: TComponentName);
    function GetTag: PtrInt;
    procedure SetTag(AValue: PtrInt);
  public
    property Name: TComponentName read GetName write SetName;
    property Tag: PtrInt read GetTag write SetTag;
  end;

  { TAControl }

  TAControl = class abstract(TAComponent)
  private
    FParent: TAWinControl;
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IAControlPeer;
  private
    function GetText: TACaption;
    procedure SetText(AValue: TACaption);
    function GetColor: TAColor;
    procedure SetColor(AValue: TAColor);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    function GetLeft: Integer;
    procedure SetLeft(AValue: Integer);
    function GetTop: Integer;
    procedure SetTop(AValue: Integer);
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    function GetParent: TAWinControl;
    procedure SetParent(AValue: TAWinControl);
  public
    property Caption: TACaption read GetText write SetText;
    property Color: TAColor read GetColor write SetColor;
    property Left: Integer read GetLeft write SetLeft;
    property Height: Integer read GetHeight write SetHeight;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Parent: TAWinControl read GetParent write SetParent;
  end;

  { TAWinControl }

  TAWinControl = class abstract(TAControl)
  private
    FControls: TFPList;    // the child controls
    function GetControl(AIndex: Integer): TAControl;
    function GetControlCount: Integer;
  public
    constructor Create(AOwner: TAComponent); override;
    destructor Destroy; override;
    procedure InsertControl(AControl: TAControl);
    procedure RemoveControl(AControl: TAControl);
    property ControlCount: Integer read GetControlCount;
    property Controls[AIndex: Integer]: TAControl read GetControl;
    function GetPeer: IAWinControlPeer;
  public
    function CanFocus: Boolean;
    function CanSetFocus: Boolean;
    procedure SetFocus;
    procedure AddKeyListener(l: IKeyListener); //添加指定的按键侦听器，以接收发自此 WinControl 的按键事件。
  end;

  TACustomControl = class(TAWinControl)
  end;

  { TAPanel }

  TAPanel = class(TACustomControl)
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IAPanelPeer;
  public

  end;

  { TAEdit }

  TAEdit = class(TAWinControl)
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IAEditPeer;
  public
    procedure Clear;
    procedure SelectAll;
    property Text: TACaption read GetText write SetText;
  end;

  { TAForm }

  TAForm = class(TAWinControl)
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IAFormPeer;
  public
    function ShowModal: Integer;
  end;

  { IAToolkit
    // 此接口是所有 Abstract Window Toolkit 实际实现的声明。
    // Toolkit 的实现被用于将各种组件绑定到特定工具包实现。
    // <br/>
    // 大多数应用程序不应直接调用该接口中的任何方法。Toolkit 定义的方法是一种“胶水”，将 awt 中
    // 与实现无关的类与 peer 中的对应物连接起来。Toolkit 定义的一些方法能直接查询本机操作系统。
  }

  IAToolkit = interface
    ['{0BD4F0EE-9C6F-4882-A92E-C4B34D866777}']
    function CreatePanel(ATarget: TAPanel): IAPanelPeer;
    function CreateEdit(ATarget: TAEdit): IAEditPeer;
    function CreateForm(ATarget: TAForm): IAFormPeer;
  end;

var
  Toolkit: IAToolkit;

implementation

{ TAComponent }

function TAComponent.GetComponent(AIndex: Integer): TAComponent;
begin
  if not Assigned(FComponents) then
    Result := nil
  else
    Result := TAComponent(FComponents.Items[AIndex]);
end;

function TAComponent.GetComponentCount: Integer;
begin
  if not Assigned(FComponents) then
    Result := 0
  else
    Result := FComponents.Count;
end;

constructor TAComponent.Create(AOwner: TAComponent);
begin
  inherited Create;
  FOwner := nil;
  FComponents := nil;
  if Assigned(AOwner) then
    AOwner.InsertComponent(Self);
end;

destructor TAComponent.Destroy;
begin
  DestroyComponents;
  if FOwner <> nil then
    FOwner.RemoveComponent(Self);
  FPeer := nil;
  inherited Destroy;
end;

procedure TAComponent.DestroyComponents;
var
  acomponent: TAComponent;
begin
  while Assigned(FComponents) do
    begin
      aComponent := TAComponent(FComponents.Last);
      RemoveComponent(aComponent);
      aComponent.Destroy;
    end;
end;

procedure TAComponent.InsertComponent(AComponent: TAComponent);
begin
  if not Assigned(FComponents) then
    FComponents := TFPList.Create;
  FComponents.Add(AComponent);
  AComponent.FOwner := Self;
end;

procedure TAComponent.RemoveComponent(AComponent: TAComponent);
begin
  AComponent.FOwner := nil;
  if Assigned(FComponents) then
    begin
      FComponents.Remove(AComponent);
      if FComponents.Count = 0 then
        begin
          FComponents.Free;
          FComponents := nil;
        end;
    end;
end;

function TAComponent.GetPeer: IAComponentPeer;
begin
  Result := FPeer;
end;

function TAComponent.GetName: TComponentName;
begin
  Result := FPeer.GetName;
end;

procedure TAComponent.SetName(AValue: TComponentName);
begin
  FPeer.SetName(AValue);
end;

function TAComponent.GetTag: PtrInt;
begin
  Result := FPeer.GetTag;
end;

procedure TAComponent.SetTag(AValue: PtrInt);
begin
  FPeer.SetTag(AValue);
end;

{ TAControl }

constructor TAControl.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FParent := nil;
end;

function TAControl.GetPeer: IAControlPeer;
begin
  FPeer.QueryInterface(IAControlPeer, Result);
end;

function TAControl.GetText: TACaption;
begin
  Result := GetPeer.GetText;
end;

procedure TAControl.SetText(AValue: TACaption);
begin
  GetPeer.SetText(AValue);
end;

function TAControl.GetColor: TAColor;
begin
  Result := GetPeer.GetColor;
end;

procedure TAControl.SetColor(AValue: TAColor);
begin
  GetPeer.SetColor(AValue);
end;

function TAControl.GetHeight: Integer;
begin
  Result := GetPeer.GetHeight;
end;

procedure TAControl.SetHeight(AValue: Integer);
begin
  GetPeer.SetHeight(AValue);
end;

function TAControl.GetLeft: Integer;
begin
  Result := GetPeer.GetLeft;
end;

procedure TAControl.SetLeft(AValue: Integer);
begin
  GetPeer.SetLeft(AValue);
end;

function TAControl.GetTop: Integer;
begin
  Result := GetPeer.GetTop;
end;

procedure TAControl.SetTop(AValue: Integer);
begin
  GetPeer.SetTop(AValue);
end;

function TAControl.GetWidth: Integer;
begin
  Result := GetPeer.GetWidth;
end;

procedure TAControl.SetWidth(AValue: Integer);
begin
  GetPeer.SetWidth(AValue);
end;

function TAControl.GetParent: TAWinControl;
begin
  Result := FParent;
end;

procedure TAControl.SetParent(AValue: TAWinControl);
begin
  if FParent = AValue then
    Exit;
  GetPeer.SetParent(AValue);
  if FParent <> nil then
    FParent.RemoveControl(Self);
  if AValue <> nil then
    AValue.InsertControl(Self);
  FParent := AValue;
end;

{ TAWinControl }

function TAWinControl.GetControl(AIndex: Integer): TAControl;
begin
  Result := TAControl(FControls[AIndex]);
end;

function TAWinControl.GetControlCount: Integer;
begin
  if FControls <> nil then
    Result := FControls.Count
  else
    Result := 0;
end;

constructor TAWinControl.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FControls := TFPList.Create;
end;

destructor TAWinControl.Destroy;
var
  c: TAControl;
begin
  while FControls.Count > 0 do
    begin
      c := TAControl(FControls.Last);
      RemoveControl(c);
    end;
  inherited Destroy;
end;

procedure TAWinControl.InsertControl(AControl: TAControl);
begin
  FControls.Add(AControl);
  AControl.FParent := Self;
end;

procedure TAWinControl.RemoveControl(AControl: TAControl);
begin
  FControls.Remove(AControl);
  AControl.FParent := nil;
end;

function TAWinControl.GetPeer: IAWinControlPeer;
begin
  FPeer.QueryInterface(IAWinControlPeer, Result);
end;

function TAWinControl.CanFocus: Boolean;
begin
  Result := GetPeer.CanFocus;
end;

function TAWinControl.CanSetFocus: Boolean;
begin
  Result := GetPeer.CanSetFocus;
end;

procedure TAWinControl.SetFocus;
begin
  GetPeer.SetFocus;
end;

procedure TAWinControl.AddKeyListener(l: IKeyListener);
begin
  GetPeer.AddKeyListener(l);
end;

{ TAPanel }

constructor TAPanel.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := Toolkit.CreatePanel(Self);
end;

function TAPanel.GetPeer: IAPanelPeer;
begin
  FPeer.QueryInterface(IAPanelPeer, Result);
end;

{ TAEdit }

constructor TAEdit.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := Toolkit.CreateEdit(Self);
end;

function TAEdit.GetPeer: IAEditPeer;
begin
  FPeer.QueryInterface(IAEditPeer, Result);
end;

procedure TAEdit.Clear;
begin
  GetPeer.Clear;
end;

procedure TAEdit.SelectAll;
begin
  GetPeer.SelectAll;
end;

{ TAForm }

constructor TAForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := Toolkit.CreateForm(Self);
end;

function TAForm.GetPeer: IAFormPeer;
begin
  FPeer.QueryInterface(IAFormPeer, Result);
end;

function TAForm.ShowModal: Integer;
begin
  Result := IAFormPeer(FPeer).ShowModal;
end;





end.

