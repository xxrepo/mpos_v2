{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_LCLLibraryPlat

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_LCLLibraryPlat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager,
  cm_InterfaceRegister, cm_InterfaceLoader,
  cm_LCL, cm_LCLGlobalSet;

procedure SetDefaultMessageHandler(AHandler: ICMMessageHandler);
//
procedure InitLibraryPlat(AInterfaceRegister: ICMInterfaceRegister);
procedure FetchInterfaceRegisterDefaultMessageHandler;
procedure AddLCLGlobalSetToLCLManager;


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
  AddLCLGlobalSetToLCLManager;
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

procedure AddLCLGlobalSetToLCLManager;
var
  lclManager: ICMLCLManager;
begin
  if Assigned(InterfaceRegister) then
    begin
      if InterfaceRegister.OutInterface(ICMLCLManager, lclManager) then
        lclManager.AddLCLGlobalSet(TLCLGlobalSet.Create);
    end;
end;


initialization


finalization
  InterfaceRegister := nil;

end.

