{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_Plat

    This is not a complete unit, for testing

    // This unit initializes the platform. You only need to use it in the main project.
    // Don't to use it in the library project.

 **********************************************************************}

unit cm_PlatInitialize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_InterfaceRegister, cm_InterfaceRegisterImpl,
  cm_InterfaceLoader, cm_InterfaceLoaderImpl,
  {$IFDEF LCL}
  Forms, Controls, StdCtrls, ExtCtrls, Graphics, Grids, Menus, ColorBox, DBGrids, ComCtrls,
  cm_LCL, cm_LCLUtils, cm_LCLGlobalSet,
  cm_type, cm_TypeUtils,
  {$ENDIF}
  cm_messager, cm_Plat, cm_GlobalSet;


procedure InitPlat(AHandler: ICMMessageHandler);
function GetInterfaceLoader: TCMInterfaceLoader;
{$IFDEF LCL}
function GetLCLGlobalManager: TCMLCLGlobalManager;
function GetObjectGenerator: TCMObjectGenerator;
{$ENDIF}

implementation

var
  aRegister: TCMInterfaceRegister = nil;
  aLoader: TCMInterfaceLoader = nil;

{$IFDEF LCL}
  aLCLManager: TCMLCLGlobalManager = nil;
  aObjectGenerator: TCMObjectGenerator = nil;
  aObjectPropertyReaderWriter: TCMObjectPropertyReaderWriter = nil;

procedure InitLCLGlobalManager;
var
  lclgs: TCMLCLGlobalSet;
begin
  DefaultMessager.Info('开始初始化LCL管理器...');
  if not Assigned(aLCLManager) then
    begin
      aLCLManager := TCMLCLGlobalManager.Create;
      InterfaceRegister.PutInterface('LCL全局数据管理器', ICMLCLGlobalManager, aLCLManager);
      lclgs := TCMLCLGlobalSet.Create;
      aLCLManager.SetMainLCLGlobalSet(lclgs);
    end;
end;

function InitObjectGenerator: TCMObjectGenerator;
begin
  Result := nil;
  DefaultMessager.Info('开始初始化LCL组件构造工具...');
  if not Assigned(aObjectGenerator) then
    begin
      aObjectGenerator := TCMObjectGenerator.Create;
      InterfaceRegister.PutInterface('ICMObjectGenerator', ICMObjectGenerator, aObjectGenerator);
      aObjectGenerator.RegisterClass('TPersistent', TPersistent);
      aObjectGenerator.RegisterClass('TFont', TFont);
      //
      aObjectGenerator.RegisterClass('TComponent', TComponent);
      aObjectGenerator.RegisterClass('TControl', TControl);
      aObjectGenerator.RegisterClass('TWinControl', TWinControl);
      aObjectGenerator.RegisterClass('TCustomEdit', TCustomEdit);
      aObjectGenerator.RegisterClass('TCustomGrid', TCustomGrid);
      //
      aObjectGenerator.RegisterClass('TForm', TForm);
      aObjectGenerator.RegisterClass('TMainMenu', TMainMenu);
      aObjectGenerator.RegisterClass('TButton', TButton);
      aObjectGenerator.RegisterClass('TLabel', TLabel);
      aObjectGenerator.RegisterClass('TEdit', TEdit);
      aObjectGenerator.RegisterClass('TMemo', TMemo);
      aObjectGenerator.RegisterClass('TCheckBox', TCheckBox);
      aObjectGenerator.RegisterClass('TRadioButton', TRadioButton);
      aObjectGenerator.RegisterClass('TListBox', TListBox);
      aObjectGenerator.RegisterClass('TComboBox', TComboBox);
      aObjectGenerator.RegisterClass('TPanel', TPanel);
      aObjectGenerator.RegisterClass('TFrame', TFrame);
      aObjectGenerator.RegisterClass('TImage', TImage);
      aObjectGenerator.RegisterClass('TBevel', TBevel);
      aObjectGenerator.RegisterClass('TSplitter', TSplitter);
      aObjectGenerator.RegisterClass('TScrollBox', TScrollBox);
      aObjectGenerator.RegisterClass('TStringGrid', TStringGrid);
      aObjectGenerator.RegisterClass('TDrawGrid', TDrawGrid);
      aObjectGenerator.RegisterClass('TColorBox', TColorBox);
      aObjectGenerator.RegisterClass('TProgressBar', TProgressBar);
      aObjectGenerator.RegisterClass('TTreeView', TTreeView);
      aObjectGenerator.RegisterClass('TPageControl', TPageControl);
      //aObjectGenerator.RegisterClass('TDateTimePicker', TDateTimePicker);   //Need additional reference the package
      aObjectGenerator.RegisterClass('TTimer', TTimer);
      aObjectGenerator.RegisterClass('TDBGrid', TDBGrid);
    end;
  Result := aObjectGenerator;
end;

function InitObjectPropertyReaderWriter: TCMObjectPropertyReaderWriter;
begin
  Result := nil;
  DefaultMessager.Info('开始初始化LCL属性读写器...');
  if not Assigned(aObjectPropertyReaderWriter) then
    begin
      aObjectPropertyReaderWriter := TCMObjectPropertyReaderWriter.Create;
      InterfaceRegister.PutInterface('ICMObjectPropertyReaderWriter', ICMObjectPropertyReaderWriter, aObjectPropertyReaderWriter);
    end;
  Result := aObjectPropertyReaderWriter;
end;

{$ENDIF}

procedure InitPlat(AHandler: ICMMessageHandler);
begin
  cm_messager.TCMMessageManager.DefaultHandler := AHandler;
  {$IFDEF LCL}
  aRegister := TCMInterfaceRegister.Create(Application);
  {$ELSE}
  aRegister := TCMInterfaceRegister.Create(nil);
  {$ENDIF}
  InterfaceRegister := aRegister;
  InterfaceRegister.PutInterface('默认信息处理器', ICMMessageHandler, AHandler, DefaultMessageHandlerCode);
  {$IFDEF LCL}
  aLoader := TCMInterfaceLoader.Create(Application, aRegister);
  {$ELSE}
  aLoader := TCMInterfaceLoader.Create(nil, aRegister);
  {$ENDIF}
  InterfaceRegister.PutInterface('默认接口加载器', ICMInterfaceLoader, ICMInterfaceLoader(aLoader));
  {$IFDEF LCL}
  //TODO 分开初始化
  InitLCLGlobalManager;
  InterfaceRegister.PutInterface('IGlobalSet', IGlobalSet, IGlobalSet(TCMGlobalSet.Create));
  InitObjectGenerator;
  InitObjectPropertyReaderWriter;
  {$ENDIF}
end;

function GetInterfaceLoader: TCMInterfaceLoader;
begin
  Result := nil;
  if not Assigned(InterfaceRegister) then
    EPlatException.Create(PlatUninitializedStr);
  Result := aLoader;
end;

{$IFDEF LCL}

function GetLCLGlobalManager: TCMLCLGlobalManager;
begin
  Result := nil;
  if not Assigned(InterfaceRegister) then
    EPlatException.Create(PlatUninitializedStr);
  Result := aLCLManager;
end;

function GetObjectGenerator: TCMObjectGenerator;
begin
  Result := nil;
  if not Assigned(InterfaceRegister) then
    EPlatException.Create(PlatUninitializedStr);
  Result := aObjectGenerator;
end;

{$ENDIF}


end.

