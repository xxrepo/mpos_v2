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

implementation

{ TAQueryEdit }

constructor TAQueryEdit.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  Self.Font.Size := 14;
  Self.Width := 140;
end;

end.

