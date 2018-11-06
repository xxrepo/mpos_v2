unit cm_AProxy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Forms, Dialogs,
  cm_interfaces,
  cm_awt, cm_AWTEvent, cm_AWTEventAdapter;

type

  { TAPeer }

  TAPeer = class(TCMBase, IAPeer)
  protected
    FDelegateObj: TObject;
  public
    constructor Create;
  public
    function GetDelegate: TObject;
  end;

  { TAComponentPeer }

  TAComponentPeer = class(TAPeer, IAComponentPeer)
  public
    constructor Create(AOwner: TComponent); virtual;
    function GetDelegate: TComponent;
  public
    function GetName: TComponentName;
    procedure SetName(AValue: TComponentName);
    function GetTag: PtrInt;
    procedure SetTag(AValue: PtrInt);
  end;

  { TAControlPeer }

  TAControlPeer = class(TAComponentPeer, IAControlPeer)
  private
    FParent: TAWinControl;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDelegate: TControl;
  public
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
  end;

  { TAWinControlPeer }

  TAWinControlPeer = class(TAControlPeer, IAWinControlPeer)
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

  TACustomControlPeer = class(TAWinControlPeer, IACustomControlPeer)

  end;

  { TAPanelPeer }

  TAPanelPeer = class(TACustomControlPeer, IAPanelPeer)
  public
    constructor Create(AOwner: TComponent); override;
    function GetDelegate: TPanel;
  end;

  { TAEditPeer }

  TAEditPeer = class(TAWinControlPeer, IAEditPeer)
  public
    constructor Create(AOwner: TComponent); override;
    function GetDelegate: TEdit;
  public
    procedure Clear;
    procedure SelectAll;
  end;

  { TAFormPeer }

  TAFormPeer = class(TAWinControlPeer, IAFormPeer)
  public
    constructor Create(AOwner: TComponent); override;
    function GetDelegate: TForm;
  public
    function ShowModal: Integer;
  end;


implementation

{ TAPeer }

constructor TAPeer.Create;
begin
  FDelegateObj := nil;
end;

function TAPeer.GetDelegate: TObject;
begin
  Result := FDelegateObj;
end;

{ TAComponentPeer }

constructor TAComponentPeer.Create(AOwner: TComponent);
begin
  inherited Create;
end;

function TAComponentPeer.GetDelegate: TComponent;
begin
  Result := TComponent(FDelegateObj);
end;

function TAComponentPeer.GetName: TComponentName;
begin
  Result := GetDelegate.Name;
end;

procedure TAComponentPeer.SetName(AValue: TComponentName);
begin
  GetDelegate.Name := AValue;
end;

function TAComponentPeer.GetTag: PtrInt;
begin
  Result := GetDelegate.Tag;
end;

procedure TAComponentPeer.SetTag(AValue: PtrInt);
begin
  GetDelegate.Tag := AValue;
end;

{ TAControlPeer }

constructor TAControlPeer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParent := nil;
end;

function TAControlPeer.GetDelegate: TControl;
begin
  Result := TControl(FDelegateObj);
end;

function TAControlPeer.GetText: TACaption;
begin
  Result := GetDelegate.Caption;
end;

procedure TAControlPeer.SetText(AValue: TACaption);
begin
  GetDelegate.Caption := AValue;
end;

function TAControlPeer.GetColor: TAColor;
begin
  Result := GetDelegate.Color;
end;

procedure TAControlPeer.SetColor(AValue: TAColor);
begin
  GetDelegate.Color := AValue;
end;

function TAControlPeer.GetHeight: Integer;
begin
  Result := GetDelegate.Height;
end;

procedure TAControlPeer.SetHeight(AValue: Integer);
begin
  GetDelegate.Height := AValue;
end;

function TAControlPeer.GetLeft: Integer;
begin
  Result := GetDelegate.Left;
end;

procedure TAControlPeer.SetLeft(AValue: Integer);
begin
  GetDelegate.Left := AValue;
end;

function TAControlPeer.GetTop: Integer;
begin
  Result := GetDelegate.Top;
end;

procedure TAControlPeer.SetTop(AValue: Integer);
begin
  GetDelegate.Top := AValue;
end;

function TAControlPeer.GetWidth: Integer;
begin
  Result := GetDelegate.Width;
end;

procedure TAControlPeer.SetWidth(AValue: Integer);
begin
  GetDelegate.Width := AValue;
end;

function TAControlPeer.GetParent: TAWinControl;
begin
  Result := FParent;
end;

procedure TAControlPeer.SetParent(AValue: TAWinControl);
var
  parent: TObject;
begin
  parent := AValue.GetPeer.GetDelegate;
  if parent is TWinControl then
    begin
      GetDelegate.Parent := TWinControl(parent);
      FParent := AValue;
    end;
end;

{ TAWinControlPeer }

function TAWinControlPeer.GetDelegate: TWinControl;
begin
  Result := TWinControl(FDelegateObj);
end;

procedure TAWinControlPeer.KeyDownEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TAWinControlPeer.KeyPressEvent(Sender: TObject; var Key: Char);
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

procedure TAWinControlPeer.KeyUpEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
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

function TAWinControlPeer.CanFocus: Boolean;
begin
  Result := GetDelegate.CanFocus;
end;

function TAWinControlPeer.CanSetFocus: Boolean;
begin
  Result := GetDelegate.CanSetFocus;
end;

procedure TAWinControlPeer.SetFocus;
begin
  GetDelegate.SetFocus;
end;

procedure TAWinControlPeer.AddKeyListener(l: IKeyListener);
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

procedure TAWinControlPeer.RemoveKeyListener(l: IKeyListener);
begin
  if Assigned(FKeyListenerList) then
    FKeyListenerList.Remove(l);
end;

{ TAPanelPeer }

constructor TAPanelPeer.Create(AOwner: TComponent);
begin
  FDelegateObj := TPanel.Create(AOwner);
end;

function TAPanelPeer.GetDelegate: TPanel;
begin
  Result := TPanel(FDelegateObj);
end;

{ TAEditPeer }

constructor TAEditPeer.Create(AOwner: TComponent);
begin
  FDelegateObj := TEdit.Create(AOwner);
end;

function TAEditPeer.GetDelegate: TEdit;
begin
  Result := TEdit(FDelegateObj);
end;

procedure TAEditPeer.Clear;
begin
  GetDelegate.Clear;
end;

procedure TAEditPeer.SelectAll;
begin
  GetDelegate.SelectAll;
end;

{ TAFormPeer }

constructor TAFormPeer.Create(AOwner: TComponent);
begin
  FDelegateObj := TForm.Create(AOwner);
end;

function TAFormPeer.GetDelegate: TForm;
begin
  Result := TForm(FDelegateObj);
end;

function TAFormPeer.ShowModal: Integer;
begin
  Result := GetDelegate.ShowModal;
end;



end.

