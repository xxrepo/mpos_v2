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
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Forms, Graphics, Dialogs,
  cm_interfaces, cm_messager,
  cm_AWTBase, cm_awt, cm_AWTEvent, cm_AWTEventUtils;

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

  { TProxyComponentPeer }

  TProxyComponentPeer = class(TProxyPeer, IAComponentPeer)
  public
    constructor Create(AOwner: TComponent); virtual;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDelegate: TControl;
  public
    function GetAlign: TAAlign;
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
    procedure ReParent(AValue: IAWinControlPeer);
  end;

  TProxyGraphicControlPeer = class(TProxyControlPeer, IAGraphicControlPeer)

  end;

  { TProxyWinControlPeer }

  TProxyWinControlPeer = class(TProxyControlPeer, IAWinControlPeer)
  private
    FKeyListenerList: TKeyListenerList;
  public
    function GetDelegate: TWinControl;
    procedure KeyDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure KeyUpEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
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
    constructor Create(AOwner: TComponent); override;
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
    constructor Create(AOwner: TComponent); override;
    function GetDelegate: TLabel;
  end;

  { TProxyPanelPeer }

  TProxyPanelPeer = class(TProxyCustomControlPeer, IAPanelPeer)
  public
    constructor Create(AOwner: TComponent); override;
    function GetDelegate: TPanel;
  end;

  { TProxyEditPeer }

  TProxyEditPeer = class(TProxyWinControlPeer, IAEditPeer)
  public
    constructor Create(AOwner: TComponent); override;
    function GetDelegate: TEdit;
  public
    procedure Clear;
    procedure SelectAll;
  end;

  { TProxyFormPeer }

  TProxyFormPeer = class(TProxyWinControlPeer, IAFormPeer)
  public
    constructor Create(AOwner: TComponent); override;
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
    function CreateCanvas(ATarget: TACanvas): IACanvasPeer;
    function CreateFont(ATarget: TAFont): IAFontPeer;
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

{ TProxyComponentPeer }

constructor TProxyComponentPeer.Create(AOwner: TComponent);
begin
  inherited Create;
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

constructor TProxyControlPeer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := nil;
end;

destructor TProxyControlPeer.Destroy;
begin
  if Assigned(FFont) then
    FFont.Free;
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

procedure TProxyControlPeer.SetBoundsRect(AValue: TRect);
begin
  GetDelegate.BoundsRect := AValue;
end;

procedure TProxyControlPeer.SetWidth(AValue: Integer);
begin
  GetDelegate.Width := AValue;
end;

procedure TProxyControlPeer.ReParent(AValue: IAWinControlPeer);
var
  parent: TObject;
begin
  parent := AValue.GetDelegate;
  if parent is TWinControl then
    begin
      GetDelegate.Parent := TWinControl(parent);
    end;
end;

{ TProxyWinControlPeer }

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

constructor TProxyCustomControlPeer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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

constructor TProxyLabelPeer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDelegateObj := TLabel.Create(AOwner);
end;

function TProxyLabelPeer.GetDelegate: TLabel;
begin
  Result := TLabel(FDelegateObj);
end;

{ TProxyPanelPeer }

constructor TProxyPanelPeer.Create(AOwner: TComponent);
begin
  FDelegateObj := TPanel.Create(AOwner);
end;

function TProxyPanelPeer.GetDelegate: TPanel;
begin
  Result := TPanel(FDelegateObj);
end;

{ TProxyEditPeer }

constructor TProxyEditPeer.Create(AOwner: TComponent);
begin
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

constructor TProxyFormPeer.Create(AOwner: TComponent);
begin
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

function TProxyToolkit.CreateCanvas(ATarget: TACanvas): IACanvasPeer;
begin
  Result := TProxyCanvasPeer.Create;
end;

function TProxyToolkit.CreateFont(ATarget: TAFont): IAFontPeer;
begin
  Result := TProxyFontPeer.Create;
end;

function TProxyToolkit.CreateLabel(ATarget: TALabel): IALabelPeer;
begin
  Result := TProxyLabelPeer.Create(nil);
end;

function TProxyToolkit.CreatePanel(ATarget: TAPanel): IAPanelPeer;
begin
  Result := TProxyPanelPeer.Create(nil);
end;

function TProxyToolkit.CreateEdit(ATarget: TAEdit): IAEditPeer;
begin
  Result := TProxyEditPeer.Create(nil);
end;

function TProxyToolkit.CreateForm(ATarget: TAForm): IAFormPeer;
begin
  Result := TProxyFormPeer.Create(nil);
end;



end.

