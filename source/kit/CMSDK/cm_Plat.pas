{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_plat

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_plat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager, cm_dialogs,
  {$IFDEF LCL}
  cm_LCL, cm_LCLGlobalSet,
  {$ENDIF}
  {$IFDEF UseAWT}
  cm_AWT,
  {$ENDIF}
  cm_InterfaceRegister;

type
  EPlatException = class(Exception);

ResourceString
  PlatUninitializedStr = 'Platform is not initialized.';

// 用于库中设置 InterfaceRegister 使用
function SetInterfaceRegister(AInterfaceRegister: ICMInterfaceRegister): Boolean;

var
  // 系统唯一全局变量
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

{$IFDEF UseAWT}
procedure FetchDefaultToolKit;
var
  tk: IAToolkit;
begin
  if InterfaceRegister.OutInterface(IAToolkit, tk, DefaultToolkitCode) then
    TAWTManager.DefaultToolkit := tk;
end;
{$ENDIF}

procedure FetchDefaultDefultMsgBox;
begin
  InterfaceRegister.OutInterface(ICMMsgBox, DefaultMsgBox, DefaultMsgBoxCode);
end;

function SetInterfaceRegister(AInterfaceRegister: ICMInterfaceRegister): Boolean;
begin
  Result := False;
  if InterfaceRegister = nil then
    begin
      InterfaceRegister := AInterfaceRegister;
      FetchDefaultMessageHandler;
      {$IFDEF LCL}
      RegisterLCLGlobalSet;
      {$ENDIF}
      {$IFDEF UseAWT}
      FetchDefaultToolKit;
      {$ENDIF}
      FetchDefaultDefultMsgBox;
      Result := True;
    end;
end;



end.

