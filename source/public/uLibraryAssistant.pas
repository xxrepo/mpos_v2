unit uLibraryAssistant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_InterfaceRegister,
  cm_LCLLibraryPlat, uMPOS;


function LibraryInitialize(AInterfaceRegister: ICMInterfaceRegister): Boolean;
function LibraryFinalize: Boolean;

implementation

function LibraryInitialize(AInterfaceRegister: ICMInterfaceRegister): Boolean;
begin
  Result := False;
  InitLibraryPlat(AInterfaceRegister);
  uMPOS.InterfaceRegister := AInterfaceRegister;
  Result := uMPOS.FetchInterfaceRegisterPOSystem;
end;

function LibraryFinalize: Boolean;
begin
  Result := False;
  uMPOS.POSSystem := nil;
  uMPOS.InterfaceRegister := nil;
  Result := True;
end;

end.

