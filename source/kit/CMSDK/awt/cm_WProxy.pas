unit cm_WProxy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  cm_LCL,
  cm_awt;

type



  { TWPForm }

  TWPForm = class(TWPObject)
  private
    FForm: TForm;
  public
    constructor Create;
    function ShowModal: Integer;
  end;

var
  LCLGenerator: ICMLCLGenerator = nil;

implementation

{ TWPForm }

constructor TWPForm.Create;
begin
  FForm := TForm(LCLGenerator.NewComponent('TForm', nil));
end;

function TWPForm.ShowModal: Integer;
begin
  Result := FForm.ShowModal;
end;

end.

