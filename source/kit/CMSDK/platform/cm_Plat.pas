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
  cm_InterfaceRegister, cm_InterfaceRegisterImpl,
  cm_InterfaceLoader, cm_InterfaceLoaderImpl,
  cm_PlatConstant;

procedure SetDefaultMessageHandler(AHandler: ICMMessageHandler);

var
  InterfaceRegister: ICMInterfaceRegister = nil;

implementation

var
  aRegister: TCMInterfaceRegister = nil;
  aLoader: TCMInterfaceLoader = nil;

procedure SetDefaultMessageHandler(AHandler: ICMMessageHandler);
begin
  aRegister.Messager.RemoveAllMessageHandler;
  aRegister.Messager.AddMessageHandler(AHandler);
  aLoader.Messager.RemoveAllMessageHandler;
  aLoader.Messager.AddMessageHandler(AHandler);
  cm_messager.TCMMessageManager.DefaultHandler := AHandler;
  InterfaceRegister.PutInterface('默认信息处理器', ICMMessageHandler, AHandler, DefaultMessageHandlerCode);
end;

initialization
  aRegister := TCMInterfaceRegister.Create(nil);
  InterfaceRegister := aRegister;
  aLoader := TCMInterfaceLoader.Create(nil, aRegister);
  //
  InterfaceRegister.PutInterface('默认接口加载器', ICMInterfaceLoader, ICMInterfaceLoader(aLoader));

finalization
  InterfaceRegister := nil;


end.

