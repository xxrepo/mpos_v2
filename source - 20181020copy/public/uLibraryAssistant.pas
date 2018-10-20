{

 库工程的助手单元

 ---------------------------------------}

unit uLibraryAssistant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_InterfaceRegister,
  cm_LibraryPlat,
  uApp;


function LibraryInitialize(AInterfaceRegister: ICMInterfaceRegister): Boolean;
function LibraryFinalize: Boolean;

implementation

function LibraryInitialize(AInterfaceRegister: ICMInterfaceRegister): Boolean;
begin
  Result := False;
  InitLibraryPlat(AInterfaceRegister);
  uApp.InterfaceRegister := AInterfaceRegister;
  Result := uApp.FetchAppSystem;
end;

function LibraryFinalize: Boolean;
begin
  Result := False;
  uApp.NilAppSystem;
  uApp.InterfaceRegister := nil;
  Result := True;
end;

end.

