{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_LibraryPlat

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_LibraryPlat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager,
  cm_InterfaceRegister, cm_InterfaceLoader,
  cm_PlatConstant;

procedure SetDefaultMessageHandler(AHandler: ICMMessageHandler);
//
procedure InitLibraryPlat(AInterfaceRegister: ICMInterfaceRegister);
procedure FetchInterfaceRegisterDefaultMessageHandler;


var
  InterfaceRegister: ICMInterfaceRegister = nil;

implementation

procedure SetDefaultMessageHandler(AHandler: ICMMessageHandler);
begin
  cm_messager.TCMMessageManager.DefaultHandler := AHandler;
end;

procedure InitLibraryPlat(AInterfaceRegister: ICMInterfaceRegister);
begin
  InterfaceRegister := AInterfaceRegister;
  FetchInterfaceRegisterDefaultMessageHandler;
end;

procedure FetchInterfaceRegisterDefaultMessageHandler;
var
  mh: ICMMessageHandler;
begin
  if Assigned(InterfaceRegister) then
    begin
      if InterfaceRegister.OutInterface(ICMMessageHandler, mh, DefaultMessageHandlerCode) then
        SetDefaultMessageHandler(mh);
    end;
end;


initialization


finalization
  InterfaceRegister := nil;

end.

