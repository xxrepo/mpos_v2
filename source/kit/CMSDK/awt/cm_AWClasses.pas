unit cm_AWClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_awt;

type

  { TAWObject }

  TAWObject = class
  private
    FWPObject: TWPObject;
  public
    constructor Create;
  end;

  { TAWForm }

  TAWForm = class(TAWObject)
  private
    //FForm: TForm;
  public
    constructor Create;
    function ShowModal: Integer;
  end;

implementation

{ TAWObject }

constructor TAWObject.Create;
var
  t: TSimpleThread;
begin
         classes.vaCurrency;
         tthread.CurrentThread;
end;

{ TAWForm }

constructor TAWForm.Create;
begin
  //FForm := TForm(LCLGenerator.NewComponent('TForm', nil));
end;

function TAWForm.ShowModal: Integer;
begin
  //Result := FForm.ShowModal;
end;



end.

