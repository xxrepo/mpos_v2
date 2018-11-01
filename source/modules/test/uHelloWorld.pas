unit uHelloWorld;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_messager,
  uTest, cm_Plat;

type

  { THelloWorld }

  THelloWorld = class(TCMMessageable, ITest)
  public
    procedure Test;
    procedure UseTest;
  end;

implementation

{ THelloWorld }

procedure THelloWorld.Test;
begin
  Messager.Info('hello world');
end;

procedure THelloWorld.UseTest;
var
  test: ITest;
begin
  if InterfaceRegister.OutInterface(ITest, test) then
    test.Test;
end;

end.

