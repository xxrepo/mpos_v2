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
  cm_InterfaceRegister, cm_InterfaceLoader
  {$IFDEF LCL},
  cm_LCL, cm_LCLGlobalSet
  {$ENDIF};

procedure SetDefaultMessageHandler(AHandler: ICMMessageHandler);
//
procedure InitLibraryPlat(AInterfaceRegister: ICMInterfaceRegister);
procedure FetchInterfaceRegisterDefaultMessageHandler;
{$IFDEF LCL}
procedure AddLCLGlobalSetToLCLManager;
{$ENDIF}


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
  {$IFDEF LCL}
  AddLCLGlobalSetToLCLManager;
  {$ENDIF}
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

{$IFDEF LCL}
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
{$ENDIF}

finalization
  InterfaceRegister := nil;

end.

