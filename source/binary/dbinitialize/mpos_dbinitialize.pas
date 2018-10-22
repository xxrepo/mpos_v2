library mpos_dbinitialize;

{$mode objfpc}{$H+}

uses
  Classes, Forms, Interfaces, cm_InterfaceRegister, cm_InterfaceLoader,
  uLibraryAssistant, uDBInitialize, uRemoteService, uDBInitializeImpl,
  uDBInitDAO, uDBInitDAOImpl, uSqlScriptProcess, uDBInitForm;

function LoadExport(ARegister: ICMInterfaceRegister; const AInfo: ICMLibInfo): Boolean; stdcall;
begin
  Result := False;
  if not LibraryInitialize(ARegister) then
    Exit;
  //
  Result := ARegister.PutInterface(IDBInitialize, TDBInitialize.Create) >= 0;
end;

function UnloadExport(AStatus: Integer): Boolean; stdcall;
begin
  Result := LibraryFinalize;
end;

exports
  LoadExport, UnloadExport;

begin
  Application.Initialize;
end.

