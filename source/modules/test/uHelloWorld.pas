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
var
  c: TComponent;
begin
  Messager.Debug('Test()...');
  // Do something you need to do.

  Messager.Debug('Test().');
end;

procedure THelloWorld.UseTest;
//var
//  test: ITest;
begin
  //if InterfaceRegister.OutInterface(ITest, test) then
  //  test.Test;
end;

end.

