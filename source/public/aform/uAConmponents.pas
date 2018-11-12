unit uAConmponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_AWT;

type

  { TAQueryEdit }

  TAQueryEdit = class(TAEdit)
  public
    constructor Create(AOwner: TAComponent); override;
  end;

  { TAQueryItem }

  TAQueryItem = class(TAPanel)
  private
    FLabel: TALabel;
    FEdt: TAQueryEdit;
    function GetLabelWidth: Integer;
    procedure SetLabelWidth(AValue: Integer);
  public
    constructor Create(AOwner: TAComponent); override;
    procedure Invalidate; override;
    property LabelWidth: Integer read GetLabelWidth write SetLabelWidth;
  end;

implementation

{ TAQueryEdit }

constructor TAQueryEdit.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  Self.Font.Size := 14;
  Self.Width := 140;
end;

{ TAQueryItem }

function TAQueryItem.GetLabelWidth: Integer;
begin
  Result := FEdt.Left;
end;

procedure TAQueryItem.SetLabelWidth(AValue: Integer);
begin
  FEdt.Left := AValue;
end;

constructor TAQueryItem.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  Self.BevelOuter := bvNone;
  FLabel := TALabel.Create(Self);
  FLabel.Parent := Self;
  FEdt := TAQueryEdit.Create(Self);
  FEdt.Parent := Self;
  FEdt.Left := 100;
end;

procedure TAQueryItem.Invalidate;
begin
  inherited Invalidate;
end;

end.

