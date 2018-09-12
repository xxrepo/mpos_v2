unit uMPOS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_InterfaceRegister,
  uSystem;


var
  POSSystem: IPOSSystem = nil;
  InterfaceRegister: ICMInterfaceRegister = nil;

function FetchInterfaceRegisterPOSystem: Boolean;

implementation

function FetchInterfaceRegisterPOSystem: Boolean;
begin
  Result := False;
  if Assigned(InterfaceRegister) then
    begin
      Result := InterfaceRegister.OutInterface(IPOSSystem, POSSystem);
    end;
end;

finalization
  POSSystem := nil;
  InterfaceRegister := nil;

end.

