{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_AWTProxy

    This is not a complete unit, for testing

    一种 AWT 的解决方案，通过简单代理的方式

    NOTE:
    20181119 add 鼠标事件、memo、datetimepicker

 **********************************************************************}

unit cm_AWTProxy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Forms, Graphics, DateTimePicker,
  cm_interfaces, cm_messager, cm_dialogs, cm_classes,
  cm_AWT, cm_AWTEventBuilder;

type

  { TProxyPeer }

  TProxyPeer = class(TCMBase, IAPeer)
  private
    FIsSelfCreateDelegate: Boolean;
  protected
    FDelegateObj: TObject;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetDelegate: TObject;
  end;

  { TProxyFontPeer }

  TProxyFontPeer = class(TProxyPeer, IAFontPeer)
  public
    constructor Create;
    constructor Create(TheDelegate: TFont); overload;
    function GetDelegate: TFont;
  public
    function GetColor: TColor;
    function GetHeight: Integer;
    function GetName: string;
    function GetSize: Integer;
    procedure SetColor(AValue: TColor);
    procedure SetHeight(AValue: Integer);
    procedure SetName(AValue: string);
    procedure SetSize(AValue: Integer);
  end;

  { TProxyGraphicPeer }

  TProxyGraphicPeer = class abstract(TProxyPeer, IAGraphicPeer)
  public
    function GetDelegate: TGraphic;
  public
    function GetEmpty: Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure Clear;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
  end;

  { TProxyRasterImagePeer }

  TProxyRasterImagePeer = class abstract(TProxyGraphicPeer, IARasterImagePeer)
  private
    FCanvas: TACanvas;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDelegate: TRasterImage;
  public
    function GetCanvas: TACanvas;
    procedure FreeImage;
  end;

  { TProxyCustomBitmapPeer }

  TProxyCustomBitmapPeer = class(TProxyRasterImagePeer, IACustomBitmapPeer)
  public
    constructor Create(TheDelegate: TCustomBitmap);
    function GetDelegate: TCustomBitmap;
  public
    function GetMonochrome: Boolean;
    procedure SetMonochrome(AValue: Boolean);
    procedure SetSize(AWidth, AHeight: Integer);
  end;

  { TProxyBrushPeer }

  TProxyBrushPeer = class(TProxyPeer, IABrushPeer)
  private
    FCustomBitmap: TACustomBitmap;
  public
    constructor Create(TheDelegate: TBrush);
    destructor Destroy; override;
    function GetDelegate: TBrush;
  public
    function GetBitmap: TACustomBitmap;
    function GetColor: TColor;
    procedure SetBitmap(AValue: TACustomBitmap);
    procedure SetColor(AValue: TColor);
  end;

  { TProxyCanvasPeer }

  TProxyCanvasPeer = class(TProxyPeer, IACanvasPeer)
  private
    FBrush: TABrush;
    FFont: TAFont;
  public
    constructor Create;
    constructor Create(TheDelegate: TCanvas); overload;
    destructor Destroy; override;
    function GetDelegate: TCanvas;
  public
    function GetBrush: TABrush;
    function GetFont: TAFont;
    procedure SetBrush(AValue: TABrush);
    procedure SetFont(AValue: TAFont);
    procedure FillRect(X1,Y1,X2,Y2: Integer);
    procedure TextOut(X,Y: Integer; const Text: string);
  end;

  { TProxyControlBorderSpacingPeer }

  TProxyControlBorderSpacingPeer = class(TProxyPeer, IAControlBorderSpacingPeer)
  public
    constructor Create(OwnerControl: TControl);
    constructor Create(TheDelegate: TControlBorderSpacing); overload;
    function GetDelegate: TControlBorderSpacing;
  public
    function GetAround: Integer;
    function GetBottom: Integer;
    function GetLeft: Integer;
    function GetRight: Integer;
    function GetTop: Integer;
    procedure SetAround(AValue: Integer);
    procedure SetBottom(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetRight(AValue: Integer);
    procedure SetTop(AValue: Integer);
  end;

  { TProxyComponentPeer }

  TProxyComponentPeer = class(TProxyPeer, IAComponentPeer)
  protected
    FTargetObj: TAComponent;
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); virtual;
    function GetDelegate: TComponent;
  public
    function GetName: TComponentName;
    procedure SetName(AValue: TComponentName);
    function GetTag: PtrInt;
    procedure SetTag(AValue: PtrInt);
  end;

  { TProxyControlPeer }

  TProxyControlPeer = class(TProxyComponentPeer, IAControlPeer)
  private
    FFont: TAFont;
    FBorderSpacing: TAControlBorderSpacing;
    FMouseListenerList: TMouseListenerList;
    class var FControlPeerList: TFPList;
  protected
    FControlListenerList: TCMInterfaceList; //同时用于子类扩展
    procedure ControlDblClickEvent(Sender: TObject); //Control 未公开，在公开的子类中使用。
    procedure RegisterControlEvents; virtual;
    procedure RegisterMouseEvents; virtual;
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDelegate: TControl;
    procedure ControlClickEvent(Sender: TObject);
    procedure ControlResizeEvent(Sender: TObject);
    procedure MouseDownEvent(Sender: TObject; Button: Controls.TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseUpEvent(Sender: TObject; Button: Controls.TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseEnterEvent(Sender: TObject);
    procedure MouseLeaveEvent(Sender: TObject);
    procedure MouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseWheelEvent(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    function GetAlign: TAlign;
    function GetAutoSize: Boolean;
    function GetBorderSpacing: TAControlBorderSpacing;
    function GetBoundsRect: TRect;
    function GetColor: TColor;
    function GetEnabled: Boolean;
    function GetFont: TAFont;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetText: TCaption;
    function GetTop: Integer;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    procedure SetAlign(AValue: TAlign);
    procedure SetAutoSize(AValue: Boolean);
    procedure SetBorderSpacing(AValue: TAControlBorderSpacing);
    procedure SetBoundsRect(AValue: TRect);
    procedure SetColor(AValue: TColor);
    procedure SetEnabled(AValue: Boolean);
    procedure SetFont(AValue: TAFont);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetText(AValue: TCaption);
    procedure SetTop(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
    //
    function GetParent: TAWinControl;
    procedure SetParent(AValue: TAWinControl);
    //
    procedure AdjustSize;
    procedure InvalidatePreferredSize;
    procedure BringToFront;
    procedure Hide;
    procedure Invalidate;
    procedure SendToBack;
    procedure Show;
    procedure Update;
    //
    procedure AddControlListener(l: IControlListener);
    procedure RemoveControlListener(l: IControlListener);
    function GetControlListeners: TControlListenerList;
    procedure AddMouseListener(l: IMouseListener);
    procedure RemoveMouseListener(l: IMouseListener);
    function GetMouseListeners: TMouseListenerList;
  end;

  TProxyGraphicControlPeer = class(TProxyControlPeer, IAGraphicControlPeer)

  end;

  { TProxyWinControlPeer }

  TProxyWinControlPeer = class(TProxyControlPeer, IAWinControlPeer)
  private
    FControls: TFPList;    // the child controls
    FKeyListenerList: TKeyListenerList;
  protected
    procedure RegisterWinControlEvents;
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDelegate: TWinControl;
    procedure WinControlEnterEvent(Sender: TObject);
    procedure WinControlExitEvent(Sender: TObject);
    procedure KeyDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure KeyUpEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    function GetBorderStyle: TBorderStyle; virtual;
    function GetControl(AIndex: Integer): TAControl;
    function GetControlCount: Integer;
    function GetShowing: Boolean;
    function GetTabOrder: TTabOrder;
    procedure SetBorderStyle(AValue: TBorderStyle); virtual;
    procedure SetTabOrder(AValue: TTabOrder);
    //
    procedure InsertControl(AControl: TAControl);
    procedure RemoveControl(AControl: TAControl);
    function CanFocus: Boolean;
    function CanSetFocus: Boolean;
    function Focused: Boolean;
    procedure SetFocus;
    //
    procedure AddWinControlListener(l: IWinControlListener);
    procedure RemoveWinControlListener(l: IWinControlListener);
    function GetWinControlListeners: TWinControlListenerList;
    procedure AddKeyListener(l: IKeyListener);
    procedure RemoveKeyListener(l: IKeyListener);
    function GetKeyListeners: TKeyListenerList;
  end;

  { TProxyCustomControlPeer }

  TProxyCustomControlPeer = class(TProxyWinControlPeer, IACustomControlPeer)
  private
    FCanvas: TACanvas;
  protected
    procedure RegisterCustomControlEvents;
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDelegate: TCustomControl;
    procedure CustomControlPaintEvent(Sender: TObject);
    function GetBorderStyle: TBorderStyle; override;
    procedure SetBorderStyle(AValue: TBorderStyle); override;
  public
    function GetCanvas: TACanvas;
    procedure SetCanvas(AValue: TACanvas);
    procedure AddCustomControlListener(l: ICustomControlListener);
    procedure RemoveCustomControlListener(l: ICustomControlListener);
    function GetCustomControlListeners: TCustomControlListenerList;
  end;

  { TProxyLabelPeer }

  TProxyLabelPeer = class(TProxyGraphicControlPeer, IALabelPeer)
  protected
    procedure RegisterControlEvents; override;
    procedure RegisterMouseEvents; override;
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    function GetDelegate: TLabel;
  public
    function GetAlignment: TAlignment;
    function GetLayout: TTextLayout;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetLayout(AValue: TTextLayout);
  end;

  { TProxyPanelPeer }

  TProxyPanelPeer = class(TProxyCustomControlPeer, IAPanelPeer)
  protected
    procedure RegisterControlEvents; override;
    procedure RegisterMouseEvents; override;
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    function GetDelegate: TPanel;
  public
    function GetAlignment: TAlignment;
    function GetBevelColor: TColor;
    function GetBevelInner: TPanelBevel;
    function GetBevelOuter: TPanelBevel;
    function GetBevelWidth: TBevelWidth;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetBevelColor(AValue: TColor);
    procedure SetBevelInner(AValue: TPanelBevel);
    procedure SetBevelOuter(AValue: TPanelBevel);
    procedure SetBevelWidth(AValue: TBevelWidth);
  end;

  { TProxyEditPeer }

  TProxyEditPeer = class(TProxyWinControlPeer, IAEditPeer)
  protected
    procedure RegisterControlEvents; override;
    procedure RegisterMouseEvents; override;
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    function GetDelegate: TEdit;
    procedure EditChangeEvent(Sender: TObject);
  public
    procedure Clear;
    procedure SelectAll;
    function GetMaxLength: Integer;
    function GetNumbersOnly: Boolean;
    function GetPasswordChar: Char;
    function GetReadOnly: Boolean;
    function GetSelLength: integer;
    function GetSelStart: integer;
    function GetSelText: String;
    procedure SetMaxLength(AValue: Integer);
    procedure SetNumbersOnly(AValue: Boolean);
    procedure SetPasswordChar(AValue: Char);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetSelLength(AValue: integer);
    procedure SetSelStart(AValue: integer);
    procedure SetSelText(AValue: String);
    //
    procedure AddEditListener(l: IEditListener);
    procedure RemoveEditListener(l: IEditListener);
    function GetEditListeners: TEditListenerList;
  end;

  { TProxyMemoPeer }

  TProxyMemoPeer = class(TProxyEditPeer, IAMemoPeer)
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    function GetDelegate: TMemo;
  public
    function GetLines: TStrings;
    function GetScrollBars: TScrollStyle;
    procedure SetLines(AValue: TStrings);
    procedure SetScrollBars(AValue: TScrollStyle);
  end;

  { TProxyFormPeer }

  TProxyFormPeer = class(TProxyCustomControlPeer, IAFormPeer)
  protected
    procedure RegisterControlEvents; override;
    procedure RegisterFormControlEvents;
    procedure RegisterMouseEvents; override;
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    function GetDelegate: TForm;
    procedure FormActivateEvent(Sender: TObject);
    procedure FormCloseEvent(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreateEvent(Sender: TObject);
    procedure FormHideEvent(Sender: TObject);
    procedure FormShowEvent(Sender: TObject);
  public
    function GetFormBorderStyle: TFormBorderStyle;
    procedure SetFormBorderStyle(AValue: TFormBorderStyle);
    procedure Close;
    function ShowModal: Integer;
    procedure AddFormListener(l: IFormListener);
    procedure RemoveFormListener(l: IFormListener);
    function GetFormListeners: TFormListenerList;
  end;

  { TProxyDateTimePickerPeer }

  TProxyDateTimePickerPeer = class(TProxyCustomControlPeer, IADateTimePickerPeer)
  protected
    procedure RegisterControlEvents; override;
    procedure RegisterMouseEvents; override;
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    function GetDelegate: TDateTimePicker;
  public
    function GetDate: TDate;
    function GetDateTime: TDateTime;
    function GetMaxDate: TDate;
    function GetMinDate: TDate;
    function GetTime: TTime;
    procedure SetDate(AValue: TDate);
    procedure SetDateTime(AValue: TDateTime);
    procedure SetMaxDate(AValue: TDate);
    procedure SetMinDate(AValue: TDate);
    procedure SetTime(AValue: TTime);
  end;

  { TProxyToolkit }

  TProxyToolkit = class(TCMMessageable, IAToolkit)
  public
    function CreateCustomBitmap(ATarget: TACustomBitmap): IACustomBitmapPeer;
    function CreateFont(ATarget: TAFont): IAFontPeer;
    function CreateCanvas(ATarget: TACanvas): IACanvasPeer;
    function CreateBorderSpacing(ATarget: TAControlBorderSpacing; OwnerControl: TAControl): IAControlBorderSpacingPeer;
    //
    function CreateLabel(ATarget: TALabel): IALabelPeer;
    function CreatePanel(ATarget: TAPanel): IAPanelPeer;
    function CreateEdit(ATarget: TAEdit): IAEditPeer;
    function CreateMemo(ATarget: TAMemo): IAMemoPeer;
    function CreateForm(ATarget: TAForm): IAFormPeer;
    function CreateDateTimePicker(ATarget: TADateTimePicker): IADateTimePickerPeer;
  end;


implementation

uses FPImage;

{$i graphics_proxy.inc}

{ TProxyPeer }

constructor TProxyPeer.Create;
begin
  FIsSelfCreateDelegate := False;
  FDelegateObj := nil;
end;

destructor TProxyPeer.Destroy;
begin
  if FIsSelfCreateDelegate and Assigned(FDelegateObj) then
    FDelegateObj.Free;
  inherited Destroy;
end;

function TProxyPeer.GetDelegate: TObject;
begin
  Result := FDelegateObj;
end;

{ TProxyControlBorderSpacingPeer }

constructor TProxyControlBorderSpacingPeer.Create(OwnerControl: TControl);
begin
  inherited Create;
  FIsSelfCreateDelegate := True;
  FDelegateObj := TControlBorderSpacing.Create(OwnerControl);
end;

constructor TProxyControlBorderSpacingPeer.Create(TheDelegate: TControlBorderSpacing);
begin
  inherited Create;
  FDelegateObj := TheDelegate;
end;

function TProxyControlBorderSpacingPeer.GetDelegate: TControlBorderSpacing;
begin
  Result := TControlBorderSpacing(FDelegateObj);
end;

function TProxyControlBorderSpacingPeer.GetAround: Integer;
begin
  Result := GetDelegate.Around;
end;

function TProxyControlBorderSpacingPeer.GetBottom: Integer;
begin
  Result := GetDelegate.Bottom;
end;

function TProxyControlBorderSpacingPeer.GetLeft: Integer;
begin
  Result := GetDelegate.Left;
end;

function TProxyControlBorderSpacingPeer.GetRight: Integer;
begin
  Result := GetDelegate.Right;
end;

function TProxyControlBorderSpacingPeer.GetTop: Integer;
begin
  Result := GetDelegate.Top;
end;

procedure TProxyControlBorderSpacingPeer.SetAround(AValue: Integer);
begin
  GetDelegate.Around := AValue;
end;

procedure TProxyControlBorderSpacingPeer.SetBottom(AValue: Integer);
begin
  GetDelegate.Bottom := AValue;
end;

procedure TProxyControlBorderSpacingPeer.SetLeft(AValue: Integer);
begin
  GetDelegate.Left := AValue;
end;

procedure TProxyControlBorderSpacingPeer.SetRight(AValue: Integer);
begin
  GetDelegate.Right := AValue;
end;

procedure TProxyControlBorderSpacingPeer.SetTop(AValue: Integer);
begin
  GetDelegate.Top := AValue;
end;

{ TProxyComponentPeer }

constructor TProxyComponentPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create;
  FTargetObj := TheTarget;
end;

function TProxyComponentPeer.GetDelegate: TComponent;
begin
  Result := TComponent(FDelegateObj);
end;

function TProxyComponentPeer.GetName: TComponentName;
begin
  Result := GetDelegate.Name;
end;

procedure TProxyComponentPeer.SetName(AValue: TComponentName);
begin
  GetDelegate.Name := AValue;
end;

function TProxyComponentPeer.GetTag: PtrInt;
begin
  Result := GetDelegate.Tag;
end;

procedure TProxyComponentPeer.SetTag(AValue: PtrInt);
begin
  GetDelegate.Tag := AValue;
end;

{ TProxyControlPeer }

constructor TProxyControlPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FFont := nil;
  FBorderSpacing := nil;
  FControlPeerList.Add(Self);
  FControlListenerList := nil;
  FMouseListenerList := nil;
end;

destructor TProxyControlPeer.Destroy;
begin
  if Assigned(FFont) then
    FFont.Free;
  if Assigned(FBorderSpacing) then
    FBorderSpacing.Free;
  FControlPeerList.Remove(Self);
  if Assigned(FControlListenerList) then
    FControlListenerList.Free;
  if Assigned(FMouseListenerList) then
    FMouseListenerList.Free;
  inherited Destroy;
end;

function TProxyControlPeer.GetDelegate: TControl;
begin
  Result := TControl(FDelegateObj);
end;

procedure TProxyControlPeer.ControlClickEvent(Sender: TObject);
var
  i: Integer;
  ce: IControlEvent;
begin
  if Assigned(FControlListenerList) then
    begin
      ce := TControlEvent.Create(Sender, TAControl(Self.FTargetObj));
      for i:=0 to FControlListenerList.Count-1 do
        IControlListener(FControlListenerList[i]).ControlClick(ce);
    end;
end;

procedure TProxyControlPeer.ControlDblClickEvent(Sender: TObject);
var
  i: Integer;
  ce: IControlEvent;
begin
  if Assigned(FControlListenerList) then
    begin
      ce := TControlEvent.Create(Sender, TAControl(Self.FTargetObj));
      for i:=0 to FControlListenerList.Count-1 do
        IControlListener(FControlListenerList[i]).ControlDblClick(ce);
    end;
end;

procedure TProxyControlPeer.ControlResizeEvent(Sender: TObject);
var
  i: Integer;
  ce: IControlEvent;
begin
  if Assigned(FControlListenerList) then
    begin
      ce := TControlEvent.Create(Sender, TAControl(Self.FTargetObj));
      for i:=0 to FControlListenerList.Count-1 do
        IControlListener(FControlListenerList[i]).ControlResize(ce);
    end;
end;

procedure TProxyControlPeer.MouseDownEvent(Sender: TObject; Button: Controls.TMouseButton; Shift: TShiftState; X, Y: Integer)
  ;
var
  i: Integer;
  me: IMouseEvent;
  p: TPoint;
begin
  if Assigned(FMouseListenerList) then
    begin
      p.X := X;
      p.Y := Y;
      me := TMouseEvent.Create(Sender, TAControl(Self.FTargetObj), p, Mouse.CursorPos, Shift, cm_AWT.TMouseButton(Button));
      for i:=0 to FMouseListenerList.Count-1 do
        FMouseListenerList[i].MousePressed(me);
    end;
end;

procedure TProxyControlPeer.MouseUpEvent(Sender: TObject; Button: Controls.TMouseButton; Shift: TShiftState; X, Y: Integer)
  ;
var
  i: Integer;
  me: IMouseEvent;
  p: TPoint;
begin
  if Assigned(FMouseListenerList) then
    begin
      p.X := X;
      p.Y := Y;
      me := TMouseEvent.Create(Sender, TAControl(Self.FTargetObj), p, Mouse.CursorPos, Shift, cm_AWT.TMouseButton(Button));
      for i:=0 to FMouseListenerList.Count-1 do
        FMouseListenerList[i].MouseReleased(me);
    end;
end;

procedure TProxyControlPeer.MouseEnterEvent(Sender: TObject);
var
  i: Integer;
  me: IMouseEvent;
  p: TPoint;
begin
  if Assigned(FMouseListenerList) then
    begin
      p := GetDelegate.ScreenToClient(Mouse.CursorPos);
      me := TMouseEvent.Create(Sender, TAControl(Self.FTargetObj), p, Mouse.CursorPos, []);
      for i:=0 to FMouseListenerList.Count-1 do
        FMouseListenerList[i].MouseEntered(me);
    end;
end;

procedure TProxyControlPeer.MouseLeaveEvent(Sender: TObject);
var
  i: Integer;
  me: IMouseEvent;
  p: TPoint;
begin
  if Assigned(FMouseListenerList) then
    begin
      p := GetDelegate.ScreenToClient(Mouse.CursorPos);
      me := TMouseEvent.Create(Sender, TAControl(Self.FTargetObj), p, Mouse.CursorPos, []);
      for i:=0 to FMouseListenerList.Count-1 do
        FMouseListenerList[i].MouseExited(me);
    end;
end;

procedure TProxyControlPeer.MouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  me: IMouseEvent;
  p: TPoint;
begin
  if Assigned(FMouseListenerList) then
    begin
      p.X := X;
      p.Y := Y;
      me := TMouseEvent.Create(Sender, TAControl(Self.FTargetObj), p, Mouse.CursorPos, Shift);
      for i:=0 to FMouseListenerList.Count-1 do
        FMouseListenerList[i].MouseMoved(me);
    end;
end;

procedure TProxyControlPeer.MouseWheelEvent(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  i: Integer;
  me: IMouseEvent;
begin
  if Assigned(FMouseListenerList) then
    begin
      me := TMouseEvent.Create(Sender, TAControl(Self.FTargetObj), MousePos, Mouse.CursorPos, Shift, WheelDelta);
      for i:=0 to FMouseListenerList.Count-1 do
        FMouseListenerList[i].MouseWheeled(me);
    end;
end;

function TProxyControlPeer.GetAlign: TAlign;
begin
  Result := cm_AWT.TAlign(GetDelegate.Align);
end;

function TProxyControlPeer.GetAutoSize: Boolean;
begin
  Result := GetDelegate.AutoSize;
end;

function TProxyControlPeer.GetBorderSpacing: TAControlBorderSpacing;
var
  cbsp: IAControlBorderSpacingPeer;
begin
  Result := nil;
  if not Assigned(FBorderSpacing) then
    begin
      cbsp := TProxyControlBorderSpacingPeer.Create(GetDelegate.BorderSpacing);
      FBorderSpacing := TAControlBorderSpacing.Create(cbsp);
    end;
  Result := FBorderSpacing;
end;

function TProxyControlPeer.GetBoundsRect: TRect;
begin
  Result := GetDelegate.BoundsRect;
end;

function TProxyControlPeer.GetText: TCaption;
begin
  Result := GetDelegate.Caption;
end;

procedure TProxyControlPeer.SetText(AValue: TCaption);
begin
  GetDelegate.Caption := AValue;
end;

function TProxyControlPeer.GetColor: TColor;
begin
  Result := GetDelegate.Color;
end;

procedure TProxyControlPeer.SetColor(AValue: TColor);
begin
  GetDelegate.Color := AValue;
end;

function TProxyControlPeer.GetEnabled: Boolean;
begin
  Result := GetDelegate.Enabled;
end;

procedure TProxyControlPeer.SetEnabled(AValue: Boolean);
begin
  GetDelegate.Enabled := AValue;
end;

function TProxyControlPeer.GetFont: TAFont;
var
  fp: IAFontPeer;
begin
  Result := nil;
  if not Assigned(FFont) then
    begin
      fp := TProxyFontPeer.Create(GetDelegate.Font);
      FFont := TAFont.Create(fp);
    end;
  Result := FFont;
end;

procedure TProxyControlPeer.SetFont(AValue: TAFont);
begin
  if FFont = AValue then
    Exit;
  if Assigned(FFont) then
    FreeAndNil(FFont);
  FFont := TAFont.Create(AValue.GetPeer);
  if FFont.GetPeer.GetDelegate is TFont then
    GetDelegate.Font := TFont(FFont.GetPeer.GetDelegate);
end;

function TProxyControlPeer.GetLeft: Integer;
begin
  Result := GetDelegate.Left;
end;

procedure TProxyControlPeer.SetLeft(AValue: Integer);
begin
  GetDelegate.Left := AValue;
end;

function TProxyControlPeer.GetHeight: Integer;
begin
  Result := GetDelegate.Height;
end;

procedure TProxyControlPeer.SetHeight(AValue: Integer);
begin
  GetDelegate.Height := AValue;
end;

function TProxyControlPeer.GetTop: Integer;
begin
  Result := GetDelegate.Top;
end;

function TProxyControlPeer.GetVisible: Boolean;
begin
  Result := GetDelegate.Visible;
end;

procedure TProxyControlPeer.SetTop(AValue: Integer);
begin
  GetDelegate.Top := AValue;
end;

procedure TProxyControlPeer.SetVisible(AValue: Boolean);
begin
  GetDelegate.Visible := AValue;
end;

function TProxyControlPeer.GetWidth: Integer;
begin
  Result := GetDelegate.Width;
end;

procedure TProxyControlPeer.SetAlign(AValue: TAlign);
begin
  GetDelegate.Align := Controls.TAlign(AValue);
end;

procedure TProxyControlPeer.SetAutoSize(AValue: Boolean);
begin
  GetDelegate.AutoSize := AValue;
end;

procedure TProxyControlPeer.SetBorderSpacing(AValue: TAControlBorderSpacing);
begin
  if FBorderSpacing = AValue then
    Exit;
  if Assigned(FBorderSpacing) then
    FreeAndNil(FBorderSpacing);
  FBorderSpacing := TAControlBorderSpacing.Create(AValue.GetPeer);
  if FBorderSpacing.GetPeer.GetDelegate is TControlBorderSpacing then
    GetDelegate.BorderSpacing := TControlBorderSpacing(FBorderSpacing.GetPeer.GetDelegate);
end;

procedure TProxyControlPeer.SetBoundsRect(AValue: TRect);
begin
  GetDelegate.BoundsRect := AValue;
end;

procedure TProxyControlPeer.SetWidth(AValue: Integer);
begin
  GetDelegate.Width := AValue;
end;

function TProxyControlPeer.GetParent: TAWinControl;
var
  i: Integer;
  cp: TProxyControlPeer;
begin
  // TODO 改进实现
  Result := nil;
  try
    //Result := FParent;
    //if Assigned(FParent) and (GetDelegate.Parent = FParent.GetPeer.GetDelegate) then
    //  Result := FParent;
    for i:=0 to FControlPeerList.Count-1 do
      begin
        cp := TProxyControlPeer(FControlPeerList[i]);
        if cp.GetDelegate = GetDelegate.Parent then
          Result := TAWinControl(cp.FTargetObj);
      end;
  except
    on e: Exception do
      DefaultMsgBox.MessageBox(e.ClassName + #10 + e.Message, Self.ClassName + ' output error');
  end;
end;

procedure TProxyControlPeer.SetParent(AValue: TAWinControl);
var
  parent: TObject;
begin
  // 之前在外面的代码，这里已直接代理外部了，不再需要这层关系。
  //if FParent = AValue then
  //  Exit;
  //GetPeer.ReParent(AValue.GetPeer);
  //if FParent <> nil then
  //  FParent.RemoveControl(Self);
  //if AValue <> nil then
  //  AValue.InsertControl(Self);
  //FParent := AValue;
  try
    parent := AValue.GetPeer.GetDelegate;
    if parent is TWinControl then
      begin
        GetDelegate.Parent := TWinControl(parent);
        //FParent := AValue;
      end;
  except
    on e: Exception do
      DefaultMsgBox.MessageBox(e.ClassName + #10 + e.Message, Self.ClassName + ' output error');
  end;
end;

procedure TProxyControlPeer.AdjustSize;
begin
  GetDelegate.AdjustSize;
end;

procedure TProxyControlPeer.InvalidatePreferredSize;
begin
  GetDelegate.InvalidatePreferredSize;
end;

procedure TProxyControlPeer.BringToFront;
begin
  GetDelegate.BringToFront;
end;

procedure TProxyControlPeer.Hide;
begin
  GetDelegate.Hide;
end;

procedure TProxyControlPeer.Invalidate;
begin
  GetDelegate.Invalidate;
end;

procedure TProxyControlPeer.SendToBack;
begin
  GetDelegate.SendToBack;
end;

procedure TProxyControlPeer.Show;
begin
  GetDelegate.Show;
end;

procedure TProxyControlPeer.Update;
begin
  GetDelegate.Update;
end;

procedure TProxyControlPeer.RegisterControlEvents;
begin
  GetDelegate.OnClick := @ControlClickEvent;
  GetDelegate.OnResize := @ControlResizeEvent;
end;

procedure TProxyControlPeer.RegisterMouseEvents;
begin
  // Control 未公开
end;

procedure TProxyControlPeer.AddControlListener(l: IControlListener);
begin
  if not Assigned(FControlListenerList) then
    begin
      FControlListenerList := TCMInterfaceList.Create;
      RegisterControlEvents;
    end;
  FControlListenerList.Add(l);
end;

procedure TProxyControlPeer.RemoveControlListener(l: IControlListener);
begin
  if Assigned(FControlListenerList) then
    FControlListenerList.Remove(l);
end;

function TProxyControlPeer.GetControlListeners: TControlListenerList;
begin
  if not Assigned(FControlListenerList) then
    begin
      FControlListenerList := TCMInterfaceList.Create;
      RegisterControlEvents;
    end;
  Result := TControlListenerList(FControlListenerList).Clone;
end;

procedure TProxyControlPeer.AddMouseListener(l: IMouseListener);
begin
  if not Assigned(FMouseListenerList) then
    begin
      FMouseListenerList := TMouseListenerList.Create;
      RegisterMouseEvents;
    end;
  FMouseListenerList.Add(l);
end;

procedure TProxyControlPeer.RemoveMouseListener(l: IMouseListener);
begin
  if Assigned(FMouseListenerList) then
    FMouseListenerList.Remove(l);
end;

function TProxyControlPeer.GetMouseListeners: TMouseListenerList;
begin
  if not Assigned(FMouseListenerList) then
    begin
      FMouseListenerList := TMouseListenerList.Create;
      RegisterMouseEvents;
    end;
  Result := FMouseListenerList.Clone;
end;

{ TProxyWinControlPeer }

constructor TProxyWinControlPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FControls := TFPList.Create;
  FKeyListenerList := nil;
end;

destructor TProxyWinControlPeer.Destroy;
var
  c: TAControl;
begin
  while FControls.Count > 0 do
    begin
      c := TAControl(FControls.Last);
      RemoveControl(c);
    end;
  if Assigned(FKeyListenerList) then
    FKeyListenerList.Free;
  inherited Destroy;
end;

function TProxyWinControlPeer.GetDelegate: TWinControl;
begin
  Result := TWinControl(FDelegateObj);
end;

procedure TProxyWinControlPeer.WinControlEnterEvent(Sender: TObject);
var
  i: Integer;
  wce: IWinControlEvent;
  wcl: IWinControlListener;
begin
  if Assigned(FControlListenerList) then
    begin
      wce := TWinControlEvent.Create(Sender, TAWinControl(Self.FTargetObj));
      for i:=0 to FControlListenerList.Count-1 do
        if Supports(FControlListenerList[i], IWinControlListener, wcl) then
          wcl.ControlEnter(wce);
    end;
end;

procedure TProxyWinControlPeer.WinControlExitEvent(Sender: TObject);
var
  i: Integer;
  wce: IWinControlEvent;
  wcl: IWinControlListener;
begin
  if Assigned(FControlListenerList) then
    begin
      wce := TWinControlEvent.Create(Sender, TAWinControl(Self.FTargetObj));
      for i:=0 to FControlListenerList.Count-1 do
        if Supports(FControlListenerList[i], IWinControlListener, wcl) then
          wcl.ControlExit(wce);
    end;
end;

procedure TProxyWinControlPeer.KeyDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  ke: IKeyEvent;
begin
  if Assigned(FKeyListenerList) then
    begin
      ke := TKeyEvent.Create(Sender, TAWinControl(Self.FTargetObj), Key, Shift);
      for i:=0 to FKeyListenerList.Count-1 do
        begin
          FKeyListenerList[i].KeyPressed(ke);
          Key := ke.GetKeyCode;
        end;
    end;
end;

procedure TProxyWinControlPeer.KeyPressEvent(Sender: TObject; var Key: Char);
var
  i: Integer;
  ke: IKeyEvent;
begin
  if Assigned(FKeyListenerList) then
    begin
      ke := TKeyEvent.Create(Sender, TAWinControl(Self.FTargetObj), Key, []);
      for i:=0 to FKeyListenerList.Count-1 do
        begin
          FKeyListenerList[i].KeyTyped(ke);
          Key := ke.GetKeyChar;
        end;
    end;
end;

procedure TProxyWinControlPeer.KeyUpEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  ke: IKeyEvent;
begin
  if Assigned(FKeyListenerList) then
    begin
      ke := TKeyEvent.Create(Sender, TAWinControl(Self.FTargetObj), Key, Shift);
      for i:=0 to FKeyListenerList.Count-1 do
        begin
          FKeyListenerList[i].KeyReleased(ke);
          Key := ke.GetKeyCode;
        end;
    end;
end;

function TProxyWinControlPeer.GetBorderStyle: TBorderStyle;
begin
  Result := bsNone;
end;

function TProxyWinControlPeer.GetControl(AIndex: Integer): TAControl;
var
  c: TControl;
  i: Integer;
  cp: TProxyControlPeer;
begin
  // TODO 改进实现
  c := GetDelegate.Controls[AIndex];
  if Assigned(c) then
    begin
      for i:=0 to FControlPeerList.Count-1 do
        begin
          cp := TProxyControlPeer(FControlPeerList[i]);
          if cp.GetDelegate = c then
            Result := TAControl(cp.FTargetObj);
        end;
    end;
end;

function TProxyWinControlPeer.GetControlCount: Integer;
begin
  Result := GetDelegate.ControlCount;
end;

function TProxyWinControlPeer.GetShowing: Boolean;
begin
  Result := GetDelegate.Showing;
end;

function TProxyWinControlPeer.GetTabOrder: TTabOrder;
begin
  Result := GetDelegate.TabOrder;
end;

procedure TProxyWinControlPeer.SetBorderStyle(AValue: TBorderStyle);
begin

end;

procedure TProxyWinControlPeer.SetTabOrder(AValue: TTabOrder);
begin
  GetDelegate.TabOrder := AValue;
end;

procedure TProxyWinControlPeer.InsertControl(AControl: TAControl);
var
  c: TObject;
begin
  c := AControl.GetPeer.GetDelegate;
  if Assigned(c) then
    GetDelegate.InsertControl(TControl(c));
  //AControl.FParent := Self;
end;

procedure TProxyWinControlPeer.RemoveControl(AControl: TAControl);
var
  c: TObject;
begin
  c := AControl.GetPeer.GetDelegate;
  if Assigned(c) then
    GetDelegate.RemoveControl(TControl(c));
  //AControl.FParent := nil;
end;

function TProxyWinControlPeer.CanFocus: Boolean;
begin
  Result := GetDelegate.CanFocus;
end;

function TProxyWinControlPeer.CanSetFocus: Boolean;
begin
  Result := GetDelegate.CanSetFocus;
end;

function TProxyWinControlPeer.Focused: Boolean;
begin
  Result := GetDelegate.Focused;
end;

procedure TProxyWinControlPeer.SetFocus;
begin
  GetDelegate.SetFocus;
end;

procedure TProxyWinControlPeer.RegisterWinControlEvents;
begin
  GetDelegate.OnEnter := @WinControlEnterEvent;
  GetDelegate.OnExit := @WinControlExitEvent;
end;

procedure TProxyWinControlPeer.AddWinControlListener(l: IWinControlListener);
begin
  if not Assigned(FControlListenerList) then
    begin
      FControlListenerList := TCMInterfaceList.Create;
      RegisterControlEvents;
      RegisterWinControlEvents;
    end;
  FControlListenerList.Add(l);
end;

procedure TProxyWinControlPeer.RemoveWinControlListener(l: IWinControlListener);
begin
  inherited RemoveControlListener(l);
end;

function TProxyWinControlPeer.GetWinControlListeners: TWinControlListenerList;
begin
  if not Assigned(FControlListenerList) then
    begin
      FControlListenerList := TCMInterfaceList.Create;
      RegisterControlEvents;
      RegisterWinControlEvents;
    end;
  Result := TWinControlListenerList(FControlListenerList).Clone;
end;

procedure TProxyWinControlPeer.AddKeyListener(l: IKeyListener);
begin
  if not Assigned(FKeyListenerList) then
    begin
      FKeyListenerList := TKeyListenerList.Create;
      GetDelegate.OnKeyDown := @KeyDownEvent;
      GetDelegate.OnKeyPress := @KeyPressEvent;
      GetDelegate.OnKeyUp := @KeyUpEvent;
    end;
  FKeyListenerList.Add(l);
end;

procedure TProxyWinControlPeer.RemoveKeyListener(l: IKeyListener);
begin
  if Assigned(FKeyListenerList) then
    FKeyListenerList.Remove(l);
end;

function TProxyWinControlPeer.GetKeyListeners: TKeyListenerList;
begin
  if not Assigned(FKeyListenerList) then
    begin
      FKeyListenerList := TKeyListenerList.Create;
      GetDelegate.OnKeyDown := @KeyDownEvent;
      GetDelegate.OnKeyPress := @KeyPressEvent;
      GetDelegate.OnKeyUp := @KeyUpEvent;
    end;
  Result := FKeyListenerList;
end;

{ TProxyCustomControlPeer }

constructor TProxyCustomControlPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FCanvas := nil;
end;

destructor TProxyCustomControlPeer.Destroy;
begin
  if Assigned(FCanvas) then
    FCanvas.Free;
  inherited Destroy;
end;

function TProxyCustomControlPeer.GetDelegate: TCustomControl;
begin
  Result := TCustomControl(FDelegateObj);
end;

procedure TProxyCustomControlPeer.CustomControlPaintEvent(Sender: TObject);
var
  i: Integer;
  cce: ICustomControlEvent;
  ccl: ICustomControlListener;
begin
  if Assigned(FControlListenerList) then
    begin
      cce := TCustomControlEvent.Create(Sender, TACustomControl(Self.FTargetObj));
      for i:=0 to FControlListenerList.Count-1 do
        if Supports(FControlListenerList[i], ICustomControlListener, ccl) then
          ccl.ControlPaint(cce);
    end;
end;

function TProxyCustomControlPeer.GetBorderStyle: TBorderStyle;
begin
  Result := cm_AWT.TBorderStyle(GetDelegate.BorderStyle);
end;

function TProxyCustomControlPeer.GetCanvas: TACanvas;
var
  cp: IACanvasPeer;
begin
  Result := nil;
  if not Assigned(FCanvas) then
    begin
      cp := TProxyCanvasPeer.Create(GetDelegate.Canvas);
      FCanvas := TACanvas.Create(cp);
    end;
  Result := FCanvas;
end;

procedure TProxyCustomControlPeer.SetBorderStyle(AValue: TBorderStyle);
begin
  GetDelegate.BorderStyle := Controls.TBorderStyle(AValue);
end;

procedure TProxyCustomControlPeer.SetCanvas(AValue: TACanvas);
begin
  if FCanvas = AValue then
    Exit;
  if Assigned(FCanvas) then
    FreeAndNil(FCanvas);
  FCanvas := TACanvas.Create(AValue.GetPeer);
  if FCanvas.GetPeer.GetDelegate is TCanvas then
    GetDelegate.Canvas := TCanvas(FCanvas.GetPeer.GetDelegate);
end;

procedure TProxyCustomControlPeer.RegisterCustomControlEvents;
begin
  GetDelegate.OnPaint := @CustomControlPaintEvent;
end;

procedure TProxyCustomControlPeer.AddCustomControlListener(l: ICustomControlListener);
begin
  if not Assigned(FControlListenerList) then
    begin
      FControlListenerList := TCMInterfaceList.Create;
      RegisterControlEvents;
      RegisterWinControlEvents;
      RegisterCustomControlEvents;
    end;
  FControlListenerList.Add(l);
end;

procedure TProxyCustomControlPeer.RemoveCustomControlListener(l: ICustomControlListener);
begin
  inherited RemoveControlListener(l);
end;

function TProxyCustomControlPeer.GetCustomControlListeners: TCustomControlListenerList;
begin
  if not Assigned(FControlListenerList) then
    begin
      FControlListenerList := TCMInterfaceList.Create;
      RegisterControlEvents;
      RegisterWinControlEvents;
      RegisterCustomControlEvents;
    end;
  Result := TCustomControlListenerList(FControlListenerList).Clone;
end;

{ TProxyLabelPeer }

procedure TProxyLabelPeer.RegisterControlEvents;
begin
  inherited RegisterControlEvents;
  GetDelegate.OnDblClick := @ControlDblClickEvent;
end;

procedure TProxyLabelPeer.RegisterMouseEvents;
begin
  inherited RegisterMouseEvents;
  GetDelegate.OnMouseDown := @MouseDownEvent;
  GetDelegate.OnMouseUp := @MouseUpEvent;
  GetDelegate.OnMouseEnter := @MouseEnterEvent;
  GetDelegate.OnMouseLeave := @MouseLeaveEvent;
  GetDelegate.OnMouseMove := @MouseMoveEvent;
  GetDelegate.OnMouseWheel:= @MouseWheelEvent;
end;

constructor TProxyLabelPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FDelegateObj := TLabel.Create(AOwner);
end;

function TProxyLabelPeer.GetDelegate: TLabel;
begin
  Result := TLabel(FDelegateObj);
end;

function TProxyLabelPeer.GetAlignment: TAlignment;
begin
  Result := GetDelegate.Alignment;
end;

function TProxyLabelPeer.GetLayout: TTextLayout;
begin
  Result := cm_AWT.TTextLayout(GetDelegate.Layout);
end;

procedure TProxyLabelPeer.SetAlignment(AValue: TAlignment);
begin
  GetDelegate.Alignment := AValue;
end;

procedure TProxyLabelPeer.SetLayout(AValue: TTextLayout);
begin
  GetDelegate.Layout := Graphics.TTextLayout(AValue);
end;

{ TProxyPanelPeer }

procedure TProxyPanelPeer.RegisterControlEvents;
begin
  inherited RegisterControlEvents;
  GetDelegate.OnDblClick := @ControlDblClickEvent;
end;

procedure TProxyPanelPeer.RegisterMouseEvents;
begin
  inherited RegisterMouseEvents;
  GetDelegate.OnMouseDown := @MouseDownEvent;
  GetDelegate.OnMouseUp := @MouseUpEvent;
  GetDelegate.OnMouseEnter := @MouseEnterEvent;
  GetDelegate.OnMouseLeave := @MouseLeaveEvent;
  GetDelegate.OnMouseMove := @MouseMoveEvent;
  GetDelegate.OnMouseWheel:= @MouseWheelEvent;
end;

constructor TProxyPanelPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FDelegateObj := TPanel.Create(AOwner);
end;

function TProxyPanelPeer.GetDelegate: TPanel;
begin
  Result := TPanel(FDelegateObj);
end;

function TProxyPanelPeer.GetAlignment: TAlignment;
begin
  Result := GetDelegate.Alignment;
end;

function TProxyPanelPeer.GetBevelColor: TColor;
begin
  Result := GetDelegate.BevelColor;
end;

function TProxyPanelPeer.GetBevelInner: TPanelBevel;
begin
  Result := cm_AWT.TPanelBevel(GetDelegate.BevelInner);
end;

function TProxyPanelPeer.GetBevelOuter: TPanelBevel;
begin
  Result := cm_AWT.TPanelBevel(GetDelegate.BevelOuter);
end;

function TProxyPanelPeer.GetBevelWidth: TBevelWidth;
begin
  Result := GetDelegate.BevelWidth;
end;

procedure TProxyPanelPeer.SetAlignment(AValue: TAlignment);
begin
  GetDelegate.Alignment := AValue;
end;

procedure TProxyPanelPeer.SetBevelColor(AValue: TColor);
begin
  GetDelegate.BevelColor := AValue;
end;

procedure TProxyPanelPeer.SetBevelInner(AValue: TPanelBevel);
begin
  GetDelegate.BevelInner := ExtCtrls.TPanelBevel(AValue);
end;

procedure TProxyPanelPeer.SetBevelOuter(AValue: TPanelBevel);
begin
  GetDelegate.BevelOuter := ExtCtrls.TPanelBevel(AValue);
end;

procedure TProxyPanelPeer.SetBevelWidth(AValue: TBevelWidth);
begin
  GetDelegate.BevelWidth := AValue;
end;

{ TProxyEditPeer }

procedure TProxyEditPeer.RegisterControlEvents;
begin
  inherited RegisterControlEvents;
  GetDelegate.OnDblClick := @ControlDblClickEvent;
end;

procedure TProxyEditPeer.RegisterMouseEvents;
begin
  inherited RegisterMouseEvents;
  GetDelegate.OnMouseDown := @MouseDownEvent;
  GetDelegate.OnMouseUp := @MouseUpEvent;
  GetDelegate.OnMouseEnter := @MouseEnterEvent;
  GetDelegate.OnMouseLeave := @MouseLeaveEvent;
  GetDelegate.OnMouseMove := @MouseMoveEvent;
  GetDelegate.OnMouseWheel:= @MouseWheelEvent;
end;

constructor TProxyEditPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FDelegateObj := TEdit.Create(AOwner);
end;

function TProxyEditPeer.GetDelegate: TEdit;
begin
  Result := TEdit(FDelegateObj);
end;

procedure TProxyEditPeer.EditChangeEvent(Sender: TObject);
var
  i: Integer;
  ee: IEditEvent;
  el: IEditListener;
begin
  if Assigned(FControlListenerList) then
    begin
      ee := TEditEvent.Create(Sender, TACustomEdit(Self.FTargetObj));
      for i:=0 to FControlListenerList.Count-1 do
        if Supports(FControlListenerList[i], IEditListener, el) then
          el.EditChanged(ee);
    end;
end;

procedure TProxyEditPeer.Clear;
begin
  GetDelegate.Clear;
end;

procedure TProxyEditPeer.SelectAll;
begin
  GetDelegate.SelectAll;
end;

function TProxyEditPeer.GetMaxLength: Integer;
begin
  Result := GetDelegate.MaxLength;
end;

function TProxyEditPeer.GetNumbersOnly: Boolean;
begin
  Result := GetDelegate.NumbersOnly;
end;

function TProxyEditPeer.GetPasswordChar: Char;
begin
  Result := GetDelegate.PasswordChar;
end;

function TProxyEditPeer.GetReadOnly: Boolean;
begin
  Result := GetDelegate.ReadOnly;
end;

function TProxyEditPeer.GetSelLength: integer;
begin
  Result := GetDelegate.SelLength;
end;

function TProxyEditPeer.GetSelStart: integer;
begin
  Result := GetDelegate.SelStart;
end;

function TProxyEditPeer.GetSelText: String;
begin
  Result := GetDelegate.SelText;
end;

procedure TProxyEditPeer.SetMaxLength(AValue: Integer);
begin
  GetDelegate.MaxLength := AValue;
end;

procedure TProxyEditPeer.SetNumbersOnly(AValue: Boolean);
begin
  GetDelegate.NumbersOnly := AValue;
end;

procedure TProxyEditPeer.SetPasswordChar(AValue: Char);
begin
  GetDelegate.PasswordChar := AValue;
end;

procedure TProxyEditPeer.SetReadOnly(AValue: Boolean);
begin
  GetDelegate.ReadOnly := AValue;
end;

procedure TProxyEditPeer.SetSelLength(AValue: integer);
begin
  GetDelegate.SelLength := AValue;
end;

procedure TProxyEditPeer.SetSelStart(AValue: integer);
begin
  GetDelegate.SelStart := AValue;
end;

procedure TProxyEditPeer.SetSelText(AValue: String);
begin
  GetDelegate.SelText := AValue;
end;

procedure TProxyEditPeer.AddEditListener(l: IEditListener);
begin
  if not Assigned(FControlListenerList) then
    begin
      FControlListenerList := TCMInterfaceList.Create;
      RegisterControlEvents;
      RegisterWinControlEvents;
      GetDelegate.OnChange := @EditChangeEvent;
    end;
  FControlListenerList.Add(l);
end;

procedure TProxyEditPeer.RemoveEditListener(l: IEditListener);
begin
  inherited RemoveControlListener(l);
end;

function TProxyEditPeer.GetEditListeners: TEditListenerList;
begin
  if not Assigned(FControlListenerList) then
    begin
      FControlListenerList := TCMInterfaceList.Create;
      RegisterControlEvents;
      RegisterWinControlEvents;
      GetDelegate.OnChange := @EditChangeEvent;
    end;
  Result := TEditListenerList(FControlListenerList).Clone;
end;

{ TProxyMemoPeer }

constructor TProxyMemoPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FDelegateObj := TMemo.Create(AOwner);
end;

function TProxyMemoPeer.GetDelegate: TMemo;
begin
  Result := TMemo(FDelegateObj);
end;

function TProxyMemoPeer.GetLines: TStrings;
begin
  Result := GetDelegate.Lines;
end;

function TProxyMemoPeer.GetScrollBars: TScrollStyle;
begin
  Result := cm_AWT.TScrollStyle(GetDelegate.ScrollBars);
end;

procedure TProxyMemoPeer.SetLines(AValue: TStrings);
begin
  GetDelegate.Lines := AValue;
end;

procedure TProxyMemoPeer.SetScrollBars(AValue: TScrollStyle);
begin
  GetDelegate.ScrollBars := StdCtrls.TScrollStyle(AValue);
end;

{ TProxyFormPeer }

procedure TProxyFormPeer.RegisterControlEvents;
begin
  inherited RegisterControlEvents;
  GetDelegate.OnDblClick := @ControlDblClickEvent;
end;

constructor TProxyFormPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FDelegateObj := TForm.Create(AOwner);
  TForm(FDelegateObj).KeyPreview := True;
end;

function TProxyFormPeer.GetDelegate: TForm;
begin
  Result := TForm(FDelegateObj);
end;

procedure TProxyFormPeer.FormActivateEvent(Sender: TObject);
var
  i: Integer;
  fe: IFormEvent;
  fl: IFormListener;
begin
  if Assigned(FControlListenerList) then
    begin
      fe := TFormEvent.Create(Sender, TAForm(Self.FTargetObj));
      for i:=0 to FControlListenerList.Count-1 do
        if Supports(FControlListenerList[i], IFormListener, fl) then
          fl.FormActivate(fe);
    end;
end;

procedure TProxyFormPeer.FormCloseEvent(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
  fe: IFormEvent;
  fl: IFormListener;
begin
  if Assigned(FControlListenerList) then
    begin
      fe := TFormEvent.Create(Sender, TAForm(Self.FTargetObj));
      for i:=0 to FControlListenerList.Count-1 do
        if Supports(FControlListenerList[i], IFormListener, fl) then
          fl.FormClose(fe);
    end;
end;

procedure TProxyFormPeer.FormCreateEvent(Sender: TObject);
var
  i: Integer;
  fe: IFormEvent;
  fl: IFormListener;
begin
  if Assigned(FControlListenerList) then
    begin
      fe := TFormEvent.Create(Sender, TAForm(Self.FTargetObj));
      for i:=0 to FControlListenerList.Count-1 do
        if Supports(FControlListenerList[i], IFormListener, fl) then
          fl.FormCreate(fe);
    end;
end;

procedure TProxyFormPeer.FormHideEvent(Sender: TObject);
var
  i: Integer;
  fe: IFormEvent;
  fl: IFormListener;
begin
  if Assigned(FControlListenerList) then
    begin
      fe := TFormEvent.Create(Sender, TAForm(Self.FTargetObj));
      for i:=0 to FControlListenerList.Count-1 do
        if Supports(FControlListenerList[i], IFormListener, fl) then
          fl.FormHide(fe);
    end;
end;

procedure TProxyFormPeer.FormShowEvent(Sender: TObject);
var
  i: Integer;
  fe: IFormEvent;
  fl: IFormListener;
begin
  if Assigned(FControlListenerList) then
    begin
      fe := TFormEvent.Create(Sender, TAForm(Self.FTargetObj));
      for i:=0 to FControlListenerList.Count-1 do
        if Supports(FControlListenerList[i], IFormListener, fl) then
          fl.FormShow(fe);
    end;
end;

function TProxyFormPeer.GetFormBorderStyle: TFormBorderStyle;
begin
  Result := cm_AWT.TFormBorderStyle(GetDelegate.BorderStyle);
end;

procedure TProxyFormPeer.SetFormBorderStyle(AValue: TFormBorderStyle);
begin
  GetDelegate.BorderStyle := Controls.TFormBorderStyle(AValue);
end;

procedure TProxyFormPeer.Close;
begin
  GetDelegate.Close;
end;

function TProxyFormPeer.ShowModal: Integer;
begin
  Result := GetDelegate.ShowModal;
end;

procedure TProxyFormPeer.RegisterFormControlEvents;
begin
  GetDelegate.OnActivate := @FormActivateEvent;
  GetDelegate.OnClose := @FormCloseEvent;
  GetDelegate.OnCreate := @FormCreateEvent;
  GetDelegate.OnHide := @FormHideEvent;
  GetDelegate.OnShow := @FormShowEvent;
end;

procedure TProxyFormPeer.RegisterMouseEvents;
begin
  inherited RegisterMouseEvents;
  GetDelegate.OnMouseDown := @MouseDownEvent;
  GetDelegate.OnMouseUp := @MouseUpEvent;
  GetDelegate.OnMouseEnter := @MouseEnterEvent;
  GetDelegate.OnMouseLeave := @MouseLeaveEvent;
  GetDelegate.OnMouseMove := @MouseMoveEvent;
  GetDelegate.OnMouseWheel:= @MouseWheelEvent;
end;

procedure TProxyFormPeer.AddFormListener(l: IFormListener);
begin
  if not Assigned(FControlListenerList) then
    begin
      FControlListenerList := TCMInterfaceList.Create;
      RegisterControlEvents;
      RegisterWinControlEvents;
      RegisterCustomControlEvents;
      RegisterFormControlEvents;
    end;
  FControlListenerList.Add(l);
end;

procedure TProxyFormPeer.RemoveFormListener(l: IFormListener);
begin
  inherited RemoveControlListener(l);
end;

function TProxyFormPeer.GetFormListeners: TFormListenerList;
begin
  if not Assigned(FControlListenerList) then
    begin
      FControlListenerList := TCMInterfaceList.Create;
      RegisterControlEvents;
      RegisterWinControlEvents;
      RegisterCustomControlEvents;
      RegisterFormControlEvents;
    end;
  Result := TFormListenerList(FControlListenerList).Clone;
end;

{ TProxyDateTimePickerPeer }

procedure TProxyDateTimePickerPeer.RegisterControlEvents;
begin
  inherited RegisterControlEvents;
  GetDelegate.OnDblClick := @ControlDblClickEvent;
end;

procedure TProxyDateTimePickerPeer.RegisterMouseEvents;
begin
  inherited RegisterMouseEvents;
  GetDelegate.OnMouseDown := @MouseDownEvent;
  GetDelegate.OnMouseUp := @MouseUpEvent;
  GetDelegate.OnMouseEnter := @MouseEnterEvent;
  GetDelegate.OnMouseLeave := @MouseLeaveEvent;
  GetDelegate.OnMouseMove := @MouseMoveEvent;
  GetDelegate.OnMouseWheel:= @MouseWheelEvent;
end;

constructor TProxyDateTimePickerPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FDelegateObj := TDateTimePicker.Create(AOwner);
  GetDelegate.Kind := dtkDateTime;
  GetDelegate.DateMode := dmUpDown;
end;

function TProxyDateTimePickerPeer.GetDelegate: TDateTimePicker;
begin
  Result := TDateTimePicker(FDelegateObj);
end;

function TProxyDateTimePickerPeer.GetDate: TDate;
begin
  Result := GetDelegate.Date;
end;

function TProxyDateTimePickerPeer.GetDateTime: TDateTime;
begin
  Result := GetDelegate.DateTime;
end;

function TProxyDateTimePickerPeer.GetMaxDate: TDate;
begin
  Result := GetDelegate.MaxDate;
end;

function TProxyDateTimePickerPeer.GetMinDate: TDate;
begin
  Result := GetDelegate.MinDate;
end;

function TProxyDateTimePickerPeer.GetTime: TTime;
begin
  Result := GetDelegate.Time;
end;

procedure TProxyDateTimePickerPeer.SetDate(AValue: TDate);
begin
  GetDelegate.Date := AValue;
end;

procedure TProxyDateTimePickerPeer.SetDateTime(AValue: TDateTime);
begin
  GetDelegate.DateTime := AValue;
end;

procedure TProxyDateTimePickerPeer.SetMaxDate(AValue: TDate);
begin
  GetDelegate.MaxDate := AValue;
end;

procedure TProxyDateTimePickerPeer.SetMinDate(AValue: TDate);
begin
  GetDelegate.MinDate := AValue;
end;

procedure TProxyDateTimePickerPeer.SetTime(AValue: TTime);
begin
  GetDelegate.Time := AValue;
end;

{ TProxyToolkit }

type

  // TGIFImage 未实现抽象方法，为消除警告，声明此私有类型
  TGIFImageEx = class(TGIFImage)
  protected
    class function GetWriterClass: TFPCustomImageWriterClass; override;
  end;
  class function TGIFImageEx.GetWriterClass: TFPCustomImageWriterClass;
  begin
    Result := nil;
  end;

function TProxyToolkit.CreateCustomBitmap(ATarget: TACustomBitmap): IACustomBitmapPeer;
var
  classStr: string;
begin
  Result := nil;
  // 无法判断类型信息
  classStr := Format('%s.%s', [ATarget.UnitName, ATarget.ClassName]);
  if classStr = 'cm_AWT.TABitmap' then
    Result := TProxyCustomBitmapPeer.Create(TBitmap.Create)
  else if classStr = 'cm_AWT.TAJPEGImage' then
    Result := TProxyCustomBitmapPeer.Create(TJPEGImage.Create)
  else if classStr = 'cm_AWT.TAGIFImage' then
    Result := TProxyCustomBitmapPeer.Create(TGIFImageEx.Create)
  else if classStr = 'cm_AWT.TAPortableNetworkGraphic' then
    Result := TProxyCustomBitmapPeer.Create(TPortableNetworkGraphic.Create);
end;

function TProxyToolkit.CreateFont(ATarget: TAFont): IAFontPeer;
begin
  Result := TProxyFontPeer.Create;
end;

function TProxyToolkit.CreateCanvas(ATarget: TACanvas): IACanvasPeer;
begin
  Result := TProxyCanvasPeer.Create;
end;

function TProxyToolkit.CreateBorderSpacing(ATarget: TAControlBorderSpacing; OwnerControl: TAControl): IAControlBorderSpacingPeer;
var
  c: TObject;
begin
  Result := nil;
  c := OwnerControl.GetPeer.GetDelegate;
  if c is TControl then
    Result := TProxyControlBorderSpacingPeer.Create(TControl(c));
end;

function TProxyToolkit.CreateLabel(ATarget: TALabel): IALabelPeer;
begin
  Result := TProxyLabelPeer.Create(ATarget, nil);
end;

function TProxyToolkit.CreatePanel(ATarget: TAPanel): IAPanelPeer;
begin
  Result := TProxyPanelPeer.Create(ATarget, nil);
end;

function TProxyToolkit.CreateEdit(ATarget: TAEdit): IAEditPeer;
begin
  Result := TProxyEditPeer.Create(ATarget, nil);
end;

function TProxyToolkit.CreateMemo(ATarget: TAMemo): IAMemoPeer;
begin
  Result := TProxyMemoPeer.Create(ATarget, nil);
end;

function TProxyToolkit.CreateForm(ATarget: TAForm): IAFormPeer;
begin
  Result := TProxyFormPeer.Create(ATarget, nil);
end;

function TProxyToolkit.CreateDateTimePicker(ATarget: TADateTimePicker): IADateTimePickerPeer;
begin
  Result := TProxyDateTimePickerPeer.Create(ATarget, nil);
end;


initialization
  TProxyControlPeer.FControlPeerList := TFPList.Create;

finalization
  TProxyControlPeer.FControlPeerList.Free;

end.

