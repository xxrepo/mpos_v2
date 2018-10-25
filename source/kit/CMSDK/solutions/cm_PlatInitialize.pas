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
  {$ENDIF}
  cm_messager, cm_Plat;


procedure InitPlat(AHandler: ICMMessageHandler);
function GetInterfaceLoader: TCMInterfaceLoader;
{$IFDEF LCL}
function GetLCLGlobalManager: TCMLCLGlobalManager;
function GetLCLGenerator: TCMLCLGenerator;
{$ENDIF}

implementation

var
  aRegister: TCMInterfaceRegister = nil;
  aLoader: TCMInterfaceLoader = nil;

{$IFDEF LCL}
  aLCLManager: TCMLCLGlobalManager = nil;
  aLCLGenerator: TCMLCLGenerator = nil;
  aLCLPropertyReaderWriter: TCMLCLPropertyReaderWriter = nil;

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

function InitLCLGenerator: TCMLCLGenerator;
begin
  Result := nil;
  DefaultMessager.Info('开始初始化LCL组件构造工具...');
  if not Assigned(aLCLGenerator) then
    begin
      aLCLGenerator := TCMLCLGenerator.Create;
      InterfaceRegister.PutInterface('ICMLCLGenerator', ICMLCLGenerator, aLCLGenerator);
    end;
  Result := aLCLGenerator;
  aLCLGenerator.RegisterClass('TPersistent', TPersistent);
  aLCLGenerator.RegisterClass('TFont', TFont);
  //
  aLCLGenerator.RegisterClass('TComponent', TComponent);
  aLCLGenerator.RegisterClass('TControl', TControl);
  aLCLGenerator.RegisterClass('TWinControl', TWinControl);
  aLCLGenerator.RegisterClass('TCustomEdit', TCustomEdit);
  aLCLGenerator.RegisterClass('TCustomGrid', TCustomGrid);
  //
  aLCLGenerator.RegisterClass('TForm', TForm);
  aLCLGenerator.RegisterClass('TMainMenu', TMainMenu);
  aLCLGenerator.RegisterClass('TButton', TButton);
  aLCLGenerator.RegisterClass('TLabel', TLabel);
  aLCLGenerator.RegisterClass('TEdit', TEdit);
  aLCLGenerator.RegisterClass('TMemo', TMemo);
  aLCLGenerator.RegisterClass('TCheckBox', TCheckBox);
  aLCLGenerator.RegisterClass('TRadioButton', TRadioButton);
  aLCLGenerator.RegisterClass('TListBox', TListBox);
  aLCLGenerator.RegisterClass('TComboBox', TComboBox);
  aLCLGenerator.RegisterClass('TPanel', TPanel);
  aLCLGenerator.RegisterClass('TFrame', TFrame);
  aLCLGenerator.RegisterClass('TImage', TImage);
  aLCLGenerator.RegisterClass('TBevel', TBevel);
  aLCLGenerator.RegisterClass('TSplitter', TSplitter);
  aLCLGenerator.RegisterClass('TScrollBox', TScrollBox);
  aLCLGenerator.RegisterClass('TStringGrid', TStringGrid);
  aLCLGenerator.RegisterClass('TDrawGrid', TDrawGrid);
  aLCLGenerator.RegisterClass('TColorBox', TColorBox);
  aLCLGenerator.RegisterClass('TProgressBar', TProgressBar);
  aLCLGenerator.RegisterClass('TTreeView', TTreeView);
  aLCLGenerator.RegisterClass('TPageControl', TPageControl);
  //aLCLGenerator.RegisterClass('TDateTimePicker', TDateTimePicker);   //Need additional reference the package
  aLCLGenerator.RegisterClass('TTimer', TTimer);
  aLCLGenerator.RegisterClass('TDBGrid', TDBGrid);
end;

function InitLCLPropertyReaderWriter: TCMLCLPropertyReaderWriter;
begin
  Result := nil;
  DefaultMessager.Info('开始初始化LCL属性读写器...');
  if not Assigned(aLCLPropertyReaderWriter) then
    begin
      aLCLPropertyReaderWriter := TCMLCLPropertyReaderWriter.Create;
      InterfaceRegister.PutInterface('ICMLCLPropertyReaderWriter', ICMLCLPropertyReaderWriter, aLCLPropertyReaderWriter);
    end;
  Result := aLCLPropertyReaderWriter;
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
  InitLCLGenerator;
  InitLCLPropertyReaderWriter;
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

function GetLCLGenerator: TCMLCLGenerator;
begin
  Result := nil;
  if not Assigned(InterfaceRegister) then
    EPlatException.Create(PlatUninitializedStr);
  Result := aLCLGenerator;
end;

{$ENDIF}


end.

