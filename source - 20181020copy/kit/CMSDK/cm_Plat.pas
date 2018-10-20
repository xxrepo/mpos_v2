{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_Plat

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_Plat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager,
  {$IFDEF LCL}
  cm_LCL, cm_LCLGlobalSet,
  {$ENDIF}
  cm_InterfaceRegister;

type
  EPlatException = class(Exception);

ResourceString
  PlatUninitializedStr = 'Platform is not initialized.';

//用于库中设置 InterfaceRegister 使用
procedure SetInterfaceRegister(AInterfaceRegister: ICMInterfaceRegister);

var
  InterfaceRegister: ICMInterfaceRegister = nil;

implementation

procedure FetchDefaultMessageHandler;
var
  mh: ICMMessageHandler;
begin
  if InterfaceRegister.OutInterface(ICMMessageHandler, mh, DefaultMessageHandlerCode) then
    cm_messager.TCMMessageManager.DefaultHandler := mh;
end;

{$IFDEF LCL}
procedure RegisterLCLGlobalSet;
var
  lgm: ICMLCLGlobalManager;
begin
  if InterfaceRegister.OutInterface(ICMLCLGlobalManager, lgm) then
    lgm.AddLCLGlobalSet(TCMLCLGlobalSet.Create);
end;
{$ENDIF}

procedure SetInterfaceRegister(AInterfaceRegister: ICMInterfaceRegister);
begin
  if InterfaceRegister = nil then
    begin
      InterfaceRegister := AInterfaceRegister;
      FetchDefaultMessageHandler;
      {$IFDEF LCL}
      RegisterLCLGlobalSet;
      {$ENDIF}
    end;
end;



end.

