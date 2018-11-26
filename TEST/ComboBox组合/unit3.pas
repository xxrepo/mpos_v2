unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LResources,
  LCLType, WSStdCtrls,
  StdCtrls;

type

  { TX }

  TX = class(TWinControl)
  private
    FStyle: TComboBoxStyle;
    procedure SetStyle(AValue: TComboBoxStyle);
  protected
    class procedure WSRegisterClass; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Style: TComboBoxStyle read FStyle write SetStyle default csDropDown;
  end;

implementation

{ TX }

procedure TX.SetStyle(AValue: TComboBoxStyle);
begin
  if AValue <> FStyle then
  begin
    FStyle:= AValue;
    if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
      //TWSCustomComboBoxClass(WidgetSetClass).SetStyle(Self, AValue);
  end;
end;

class procedure TX.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomComboBox;
  RegisterPropertyToSkip(TX, 'BevelInner', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TX, 'BevelKind',  'VCL compatibility property', '');
  RegisterPropertyToSkip(TX, 'BevelOuter', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TX, 'ImeMode',    'VCL compatibility property', '');
end;

procedure TX.CreateParams(var Params: TCreateParams);
const
  ComboBoxStyles: array[TComboBoxStyle] of dword = (
    CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST,
    CBS_OWNERDRAWFIXED or CBS_DROPDOWNLIST, CBS_OWNERDRAWVARIABLE or CBS_DROPDOWNLIST,
    CBS_OWNERDRAWFIXED or CBS_DROPDOWN, CBS_OWNERDRAWVARIABLE or CBS_DROPDOWN);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or (WS_VSCROLL or CBS_AUTOHSCROLL or CBS_HASSTRINGS) or
    ComboBoxStyles[Style];

  //if Sorted then
  //  Params.Style := Params.Style or CBS_SORT;
end;

constructor TX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csComboBox;
  ControlStyle := ControlStyle - [csCaptureMouse];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  AutoSize := True;
end;

end.

