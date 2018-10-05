{

  这里声明了一个 InterfaceRegister 的全局变量，要使用它应指定实例

 ---------------------------------------}

unit uApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_InterfaceRegister,
  uSystem;

var
  InterfaceRegister: ICMInterfaceRegister = nil;

function FetchAppSystem: Boolean;
procedure NilAppSystem;
function AppSystem: IAppSystem;

implementation

var
  _appSystem: IAppSystem = nil;

function FetchAppSystem: Boolean;
begin
  Result := False;
  if Assigned(InterfaceRegister) then
    begin
      Result := InterfaceRegister.OutInterface(IAppSystem, _appSystem);
    end;
end;

procedure NilAppSystem;
begin
  _appSystem := nil;
end;

function AppSystem: IAppSystem;
begin
  Result := nil;
  if not Assigned(_appSystem) then
    FetchAppSystem;
  Result := _appSystem;
end;


finalization
  _appSystem := nil;
  InterfaceRegister := nil;

end.

