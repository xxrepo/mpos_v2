unit cm_AWTExt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_AWT;

type

  IACMComboEdit = interface(IACustomEditPeer)
    ['{9EBC6ADC-124B-4829-9873-97E4B7893B2B}']
    function GetItemHeight: Integer;
    function GetItemIndex: Integer;
    function GetItems: TStrings;
    function GetSelectOnly: Boolean;
    function GetSorted: Boolean;
    procedure SetItemHeight(AValue: Integer);
    procedure SetItemIndex(AValue: Integer);
    procedure SetItems(AValue: TStrings);
    procedure SetSelectOnly(AValue: Boolean);
    procedure SetSorted(AValue: Boolean);
  end;

  { TACMComboEdit }

  TACMComboEdit = class(TACustomEdit)
  private
    function GetItemHeight: Integer;
    function GetItemIndex: Integer;
    function GetItems: TStrings;
    function GetSelectOnly: Boolean;
    function GetSorted: Boolean;
    procedure SetItemHeight(AValue: Integer);
    procedure SetItemIndex(AValue: Integer);
    procedure SetItems(AValue: TStrings);
    procedure SetSelectOnly(AValue: Boolean);
    procedure SetSorted(AValue: Boolean);
  public
    constructor Create(AOwner: TAComponent); override;
    function GetPeer: IACMComboEdit;
  public
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Items: TStrings read GetItems write SetItems;
    property Sorted: Boolean read GetSorted write SetSorted;
    property SelectOnly: Boolean read GetSelectOnly write SetSelectOnly;
  published
    property ParentColor;
    property ParentFont;
    property Text;
  end;

  IAToolkitExt = interface(IAToolkit)
    ['{21297FC2-991E-4998-9584-AE28A1EF1877}']
    function CreateCMComboEdit(ATarget: TACMComboEdit): IACMComboEdit;
  end;

implementation

{ TACMComboEdit }

function TACMComboEdit.GetItemHeight: Integer;
begin
  Result := GetPeer.GetItemHeight;
end;

function TACMComboEdit.GetItemIndex: Integer;
begin
  Result := GetPeer.GetItemIndex;
end;

function TACMComboEdit.GetItems: TStrings;
begin
  Result := GetPeer.GetItems;
end;

function TACMComboEdit.GetSelectOnly: Boolean;
begin
  Result := GetPeer.GetSelectOnly;
end;

function TACMComboEdit.GetSorted: Boolean;
begin
  Result := GetPeer.GetSorted;
end;

procedure TACMComboEdit.SetItemHeight(AValue: Integer);
begin
  GetPeer.SetItemHeight(AValue);
end;

procedure TACMComboEdit.SetItemIndex(AValue: Integer);
begin
  GetPeer.SetItemIndex(AValue);
end;

procedure TACMComboEdit.SetItems(AValue: TStrings);
begin
  GetPeer.SetItems(AValue);
end;

procedure TACMComboEdit.SetSelectOnly(AValue: Boolean);
begin
  GetPeer.SetSelectOnly(AValue);
end;

procedure TACMComboEdit.SetSorted(AValue: Boolean);
begin
  GetPeer.SetSorted(AValue);
end;

constructor TACMComboEdit.Create(AOwner: TAComponent);
var
  tkext: IAToolkitExt;
begin
  inherited Create(AOwner);
  if Supports(TAWTManager.DefaultToolkit, IAToolkitExt, tkext) then
    FPeer := tkext.CreateCMComboEdit(Self);
end;

function TACMComboEdit.GetPeer: IACMComboEdit;
begin
  FPeer.QueryInterface(IACMComboEdit, Result);
end;

end.

