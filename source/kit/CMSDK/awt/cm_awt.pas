unit cm_AWT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_AWTBase, cm_AWTEvent;

type

  // 前置声明
  TAWinControl = class;
  IAToolkit = interface;

  { EAWTException }

  EAWTException = class(Exception);

  { TAObject }

  TAObject = class
  public
    constructor Create;
  end;

  {$I cm_AWTBasePeer.inc}

  { TAFont }

  TAFont = class(TAObject)
  private
    FPeer: IAFontPeer;
    function GetColor: TAColor;
    function GetHeight: Integer;
    function GetName: string;
    function GetSize: Integer;
    procedure SetColor(AValue: TAColor);
    procedure SetHeight(AValue: Integer);
    procedure SetName(AValue: string);
    procedure SetSize(AValue: Integer);
  public
    constructor Create;
    constructor Create(APeer: IAFontPeer); overload;
    function GetPeer: IAFontPeer;
  public
    property Color: TAColor read GetColor write SetColor;
    property Height: Integer read GetHeight write SetHeight;
    property Name: string read GetName write SetName;
    property Size: Integer read GetSize write SetSize;
  end;

  { TACanvas }

  TACanvas = class(TAObject)
  private
    FPeer: IACanvasPeer;
  public
    constructor Create;
    constructor Create(APeer: IACanvasPeer); overload;
    function GetPeer: IACanvasPeer;
  public
    procedure TextOut(X,Y: Integer; const Text: string);
  end;

  {$I cm_AWTPeer.inc}

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
    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    function GetFont: TAFont;
    procedure SetFont(AValue: TAFont);
    function GetLeft: Integer;
    procedure SetLeft(AValue: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    function GetTop: Integer;
    procedure SetTop(AValue: Integer);
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    function GetParent: TAWinControl;
    procedure SetParent(AValue: TAWinControl);
  public
    property Caption: TACaption read GetText write SetText;
    property Color: TAColor read GetColor write SetColor;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Font: TAFont read GetFont write SetFont;
  public
    property Left: Integer read GetLeft write SetLeft;
    property Height: Integer read GetHeight write SetHeight;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Parent: TAWinControl read GetParent write SetParent;
  end;

  TAGraphicControl = class abstract(TAControl)
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

  { TACustomControl }

  TACustomControl = class(TAWinControl)
  private
    function GetCanvas: TACanvas;
    procedure SetCanvas(AValue: TACanvas);
  public
    function GetPeer: IACustomControlPeer;
  public
    property Canvas: TACanvas read GetCanvas write SetCanvas;
  end;

  { TALabel }

  TALabel = class(TAGraphicControl)
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IALabelPeer;
  public

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

  {$I cm_AWTToolkit.inc}

implementation

{ TAObject }

constructor TAObject.Create;
begin
  if not Assigned(TAWTManager.DefaultToolkit) then
    raise EAWTException.Create(NoToolKitError);
end;

{ TACanvas }

constructor TACanvas.Create;
begin
  inherited Create;
  FPeer := TAWTManager.DefaultToolkit.CreateCanvas(Self);
end;

constructor TACanvas.Create(APeer: IACanvasPeer);
begin
  inherited Create;
  FPeer := APeer;
end;

function TACanvas.GetPeer: IACanvasPeer;
begin
  Result := FPeer;
end;

procedure TACanvas.TextOut(X, Y: Integer; const Text: string);
begin
  GetPeer.TextOut(X, Y, Text);
end;

{ TAFont }

function TAFont.GetColor: TAColor;
begin
  Result := GetPeer.GetColor;
end;

function TAFont.GetHeight: Integer;
begin
  Result := GetPeer.GetHeight;
end;

function TAFont.GetName: string;
begin
  Result := GetPeer.GetName;
end;

function TAFont.GetSize: Integer;
begin
  Result := GetPeer.GetSize;
end;

procedure TAFont.SetColor(AValue: TAColor);
begin
  GetPeer.SetColor(AValue);
end;

procedure TAFont.SetHeight(AValue: Integer);
begin
  GetPeer.SetHeight(AValue);
end;

procedure TAFont.SetName(AValue: string);
begin
  GetPeer.SetName(AValue);
end;

procedure TAFont.SetSize(AValue: Integer);
begin
  GetPeer.SetSize(AValue);
end;

constructor TAFont.Create;
begin
  inherited Create;
  FPeer := TAWTManager.DefaultToolkit.CreateFont(Self);
end;

constructor TAFont.Create(APeer: IAFontPeer);
begin
  inherited Create;
  FPeer := APeer;
end;

function TAFont.GetPeer: IAFontPeer;
begin
  Result := FPeer;
end;

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
  Result := GetPeer.GetName;
end;

procedure TAComponent.SetName(AValue: TComponentName);
begin
  GetPeer.SetName(AValue);
end;

function TAComponent.GetTag: PtrInt;
begin
  Result := GetPeer.GetTag;
end;

procedure TAComponent.SetTag(AValue: PtrInt);
begin
  GetPeer.SetTag(AValue);
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

function TAControl.GetEnabled: Boolean;
begin
  Result := GetPeer.GetEnabled;
end;

procedure TAControl.SetEnabled(AValue: Boolean);
begin
  GetPeer.SetEnabled(AValue);
end;

function TAControl.GetFont: TAFont;
begin
  Result := GetPeer.GetFont;
end;

procedure TAControl.SetFont(AValue: TAFont);
begin
  GetPeer.SetFont(AValue);
end;

function TAControl.GetLeft: Integer;
begin
  Result := GetPeer.GetLeft;
end;

procedure TAControl.SetLeft(AValue: Integer);
begin
  GetPeer.SetLeft(AValue);
end;

function TAControl.GetHeight: Integer;
begin
  Result := GetPeer.GetHeight;
end;

procedure TAControl.SetHeight(AValue: Integer);
begin
  GetPeer.SetHeight(AValue);
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
  GetPeer.ReParent(AValue.GetPeer);
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

{ TACustomControl }

function TACustomControl.GetCanvas: TACanvas;
begin
  Result := GetPeer.GetCanvas;
end;

procedure TACustomControl.SetCanvas(AValue: TACanvas);
begin
  GetPeer.SetCanvas(AValue);
end;

function TACustomControl.GetPeer: IACustomControlPeer;
begin
  FPeer.QueryInterface(IACustomControlPeer, Result);
end;

{ TALabel }

constructor TALabel.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreateLabel(Self);
end;

function TALabel.GetPeer: IALabelPeer;
begin
  FPeer.QueryInterface(IALabelPeer, Result);
end;

{ TAPanel }

constructor TAPanel.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreatePanel(Self);
end;

function TAPanel.GetPeer: IAPanelPeer;
begin
  FPeer.QueryInterface(IAPanelPeer, Result);
end;

{ TAEdit }

constructor TAEdit.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreateEdit(Self);
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
  FPeer := TAWTManager.DefaultToolkit.CreateForm(Self);
end;

function TAForm.GetPeer: IAFormPeer;
begin
  FPeer.QueryInterface(IAFormPeer, Result);
end;

function TAForm.ShowModal: Integer;
begin
  Result := IAFormPeer(FPeer).ShowModal;
end;

initialization
  TAWTManager.DefaultToolkit := nil;



end.

