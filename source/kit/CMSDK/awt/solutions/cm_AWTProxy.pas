{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_AWTProxy

    This is not a complete unit, for testing

    一种 AWT 的解决方案，通过简单代理的方式

 **********************************************************************}

unit cm_AWTProxy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Forms, Graphics,
  cm_interfaces, cm_messager, cm_dialogs,
  cm_AWTBase, cm_awt, cm_AWTEvent, cm_AWTEventUtils,
  uSystem;

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
    function GetColor: TAColor;
    function GetHeight: Integer;
    function GetName: string;
    function GetSize: Integer;
    procedure SetColor(AValue: TAColor);
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
    function GetColor: TAColor;
    procedure SetBitmap(AValue: TACustomBitmap);
    procedure SetColor(AValue: TAColor);
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
    class var FControlPeerList: TFPList;
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDelegate: TControl;
  public
    function GetAlign: TAAlign;
    function GetBorderSpacing: TAControlBorderSpacing;
    function GetBoundsRect: TRect;
    function GetColor: TAColor;
    function GetEnabled: Boolean;
    function GetFont: TAFont;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetText: TACaption;
    function GetTop: Integer;
    function GetWidth: Integer;
    procedure SetAlign(AValue: TAAlign);
    procedure SetBorderSpacing(AValue: TAControlBorderSpacing);
    procedure SetBoundsRect(AValue: TRect);
    procedure SetColor(AValue: TAColor);
    procedure SetEnabled(AValue: Boolean);
    procedure SetFont(AValue: TAFont);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetText(AValue: TACaption);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    //
    function GetParent: TAWinControl;
    procedure SetParent(AValue: TAWinControl);
  end;

  TProxyGraphicControlPeer = class(TProxyControlPeer, IAGraphicControlPeer)

  end;

  { TProxyWinControlPeer }

  TProxyWinControlPeer = class(TProxyControlPeer, IAWinControlPeer)
  private
    FControls: TFPList;    // the child controls
    FKeyListenerList: TKeyListenerList;
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDelegate: TWinControl;
    procedure KeyDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure KeyUpEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    function GetControl(AIndex: Integer): TAControl;
    function GetControlCount: Integer;
    procedure InsertControl(AControl: TAControl);
    procedure RemoveControl(AControl: TAControl);
    property ControlCount: Integer read GetControlCount;
    //
    function CanFocus: Boolean;
    function CanSetFocus: Boolean;
    procedure SetFocus;
    procedure AddKeyListener(l: IKeyListener);
    procedure RemoveKeyListener(l: IKeyListener);
  end;

  { TProxyCustomControlPeer }

  TProxyCustomControlPeer = class(TProxyWinControlPeer, IACustomControlPeer)
  private
    FCanvas: TACanvas;
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDelegate: TCustomControl;
  public
    function GetBorderStyle: TABorderStyle;
    function GetCanvas: TACanvas;
    procedure SetBorderStyle(AValue: TABorderStyle);
    procedure SetCanvas(AValue: TACanvas);
  end;

  { TProxyLabelPeer }

  TProxyLabelPeer = class(TProxyGraphicControlPeer, IALabelPeer)
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    function GetDelegate: TLabel;
  end;

  { TProxyPanelPeer }

  TProxyPanelPeer = class(TProxyCustomControlPeer, IAPanelPeer)
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    function GetDelegate: TPanel;
  end;

  { TProxyEditPeer }

  TProxyEditPeer = class(TProxyWinControlPeer, IAEditPeer)
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    function GetDelegate: TEdit;
  public
    procedure Clear;
    procedure SelectAll;
  end;

  { TProxyFormPeer }

  TProxyFormPeer = class(TProxyWinControlPeer, IAFormPeer)
  public
    constructor Create(TheTarget: TAComponent; AOwner: TComponent); override;
    function GetDelegate: TForm;
  public
    function GetFormBorderStyle: TAFormBorderStyle;
    procedure SetFormBorderStyle(AValue: TAFormBorderStyle);
    function ShowModal: Integer;
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
    function CreateForm(ATarget: TAForm): IAFormPeer;
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
end;

destructor TProxyControlPeer.Destroy;
begin
  if Assigned(FFont) then
    FFont.Free;
  if Assigned(FBorderSpacing) then
    FBorderSpacing.Free;
  FControlPeerList.Remove(Self);
  inherited Destroy;
end;

function TProxyControlPeer.GetDelegate: TControl;
begin
  Result := TControl(FDelegateObj);
end;

function TProxyControlPeer.GetAlign: TAAlign;
begin
  Result := TAAlign(GetDelegate.Align);
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

function TProxyControlPeer.GetText: TACaption;
begin
  Result := GetDelegate.Caption;
end;

procedure TProxyControlPeer.SetText(AValue: TACaption);
begin
  GetDelegate.Caption := AValue;
end;

function TProxyControlPeer.GetColor: TAColor;
begin
  Result := GetDelegate.Color;
end;

procedure TProxyControlPeer.SetColor(AValue: TAColor);
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

procedure TProxyControlPeer.SetTop(AValue: Integer);
begin
  GetDelegate.Top := AValue;
end;

function TProxyControlPeer.GetWidth: Integer;
begin
  Result := GetDelegate.Width;
end;

procedure TProxyControlPeer.SetAlign(AValue: TAAlign);
begin
  GetDelegate.Align := TAlign(AValue);
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
  parent := AValue.GetPeer.GetDelegate;
  if parent is TWinControl then
    begin
      GetDelegate.Parent := TWinControl(parent);
      //FParent := AValue;
    end;
end;

{ TProxyWinControlPeer }

constructor TProxyWinControlPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FControls := TFPList.Create;
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
  inherited Destroy;
end;

function TProxyWinControlPeer.GetDelegate: TWinControl;
begin
  Result := TWinControl(FDelegateObj);
end;

procedure TProxyWinControlPeer.KeyDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  ke: IKeyEvent;
begin
  if Assigned(FKeyListenerList) then
    begin
      ke := TKeyEvent.BuildKeyEvent(Sender, Char(Key), Key);
      for i:=0 to FKeyListenerList.Count-1 do
        FKeyListenerList[i].KeyPressed(ke);
    end;
end;

procedure TProxyWinControlPeer.KeyPressEvent(Sender: TObject; var Key: Char);
var
  i: Integer;
  ke: IKeyEvent;
begin
  if Assigned(FKeyListenerList) then
    begin
      ke := TKeyEvent.BuildKeyEvent(Sender, Key, Ord(Key));
      for i:=0 to FKeyListenerList.Count-1 do
        FKeyListenerList[i].KeyTyped(ke);
    end;
end;

procedure TProxyWinControlPeer.KeyUpEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  ke: IKeyEvent;
begin
  if Assigned(FKeyListenerList) then
    begin
      ke := TKeyEvent.BuildKeyEvent(Sender, Char(Key), Key);
      for i:=0 to FKeyListenerList.Count-1 do
        FKeyListenerList[i].KeyReleased(ke);
    end;
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

procedure TProxyWinControlPeer.SetFocus;
begin
  GetDelegate.SetFocus;
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

function TProxyCustomControlPeer.GetBorderStyle: TABorderStyle;
begin
  Result := TABorderStyle(GetDelegate.BorderStyle);
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

procedure TProxyCustomControlPeer.SetBorderStyle(AValue: TABorderStyle);
begin
  GetDelegate.BorderStyle := TBorderStyle(AValue);
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

{ TProxyLabelPeer }

constructor TProxyLabelPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FDelegateObj := TLabel.Create(AOwner);
end;

function TProxyLabelPeer.GetDelegate: TLabel;
begin
  Result := TLabel(FDelegateObj);
end;

{ TProxyPanelPeer }

constructor TProxyPanelPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FDelegateObj := TPanel.Create(AOwner);
end;

function TProxyPanelPeer.GetDelegate: TPanel;
begin
  Result := TPanel(FDelegateObj);
end;

{ TProxyEditPeer }

constructor TProxyEditPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FDelegateObj := TEdit.Create(AOwner);
end;

function TProxyEditPeer.GetDelegate: TEdit;
begin
  Result := TEdit(FDelegateObj);
end;

procedure TProxyEditPeer.Clear;
begin
  GetDelegate.Clear;
end;

procedure TProxyEditPeer.SelectAll;
begin
  GetDelegate.SelectAll;
end;

{ TProxyFormPeer }

constructor TProxyFormPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FDelegateObj := TForm.Create(AOwner);
end;

function TProxyFormPeer.GetDelegate: TForm;
begin
  Result := TForm(FDelegateObj);
end;

function TProxyFormPeer.GetFormBorderStyle: TAFormBorderStyle;
begin
  Result := TAFormBorderStyle(GetDelegate.BorderStyle);
end;

procedure TProxyFormPeer.SetFormBorderStyle(AValue: TAFormBorderStyle);
begin
  GetDelegate.BorderStyle := TFormBorderStyle(AValue);
end;

function TProxyFormPeer.ShowModal: Integer;
begin
  Result := GetDelegate.ShowModal;
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

function TProxyToolkit.CreateForm(ATarget: TAForm): IAFormPeer;
begin
  Result := TProxyFormPeer.Create(ATarget, nil);
end;


initialization
  TProxyControlPeer.FControlPeerList := TFPList.Create;

finalization
  TProxyControlPeer.FControlPeerList.Free;

end.

