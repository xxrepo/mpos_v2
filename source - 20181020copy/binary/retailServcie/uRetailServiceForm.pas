unit uRetailServiceForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFormService;

type

  { TRetailServiceForm }

  TRetailServiceForm = class(TServiceForm)
  private

  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{ TRetailServiceForm }

constructor TRetailServiceForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Self.SetTitle('零售业务');
end;

end.

