{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_Plat

    This is not a complete unit, for testing

    此单元用于 Application 。

 **********************************************************************}

unit cm_Plat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager,
  cm_InterfaceRegister,
  cm_InterfaceLoader;

procedure SetDefaultMessageHandler(AHandler: ICMMessageHandler);

var
  InterfaceRegister: ICMInterfaceRegister = nil;

implementation



procedure SetDefaultMessageHandler(AHandler: ICMMessageHandler);
begin
  //TODO

  //aRegister.Messager.RemoveAllMessageHandler;
  //aRegister.Messager.AddMessageHandler(AHandler);
  //aLoader.Messager.RemoveAllMessageHandler;
  //aLoader.Messager.AddMessageHandler(AHandler);
  cm_messager.TCMMessageManager.DefaultHandler := AHandler;
  InterfaceRegister.PutInterface('默认信息处理器', ICMMessageHandler, AHandler, DefaultMessageHandlerCode);
  {$IFDEF LCL}
  //TODO
  //以下更换默认信息处理器时，不进行替换了
  //if Assigned(aLCLManager) then
  //  if aLCLManager.Messager.GetMessageHandlerCount = 0 then
  //    aLCLManager.Messager.AddMessageHandler(AHandler);
  //if Assigned(aLCLGenerator) then
  //  if aLCLGenerator.Messager.GetMessageHandlerCount = 0 then
  //    aLCLGenerator.Messager.AddMessageHandler(AHandler);
  {$ENDIF}
end;




end.

