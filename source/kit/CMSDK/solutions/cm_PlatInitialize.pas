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
  cm_LCL, cm_LCLUtils, cm_LCLGlobalSet, cm_LCLPropertyReaderWriter,
  {$ENDIF}
  cm_messager, cm_Plat;

{$IFDEF LCL}
procedure InitLCLSuite;
function GetLCLManager: TCMLCLManager;
function GetLCLGenerator: TCMLCLGenerator;

//////////////////////////////////////////////////////////////


//function InitLCLManager: TCMLCLManager;
//function InitLCLGenerator: TCMLCLGenerator;
//function InitLCLPropertyReaderWriter: TCMLCLPropertyReaderWriter;
{$ENDIF}

implementation



var
  aRegister: TCMInterfaceRegister = nil;
  aLoader: TCMInterfaceLoader = nil;
  aLCLManager: TCMLCLManager = nil;
  aLCLGenerator: TCMLCLGenerator = nil;
  aLCLPropertyReaderWriter: TCMLCLPropertyReaderWriter = nil;

{$IFDEF LCL}


function InitLCLManager: TCMLCLManager;
var
  lclgs: TLCLGlobalSet;
begin
  Result := nil;
  DefaultMessager.Info('开始初始化LCL管理器...');
  if not Assigned(aLCLManager) then
    begin
      aLCLManager := TCMLCLManager.Create;
      InterfaceRegister.PutInterface('LCL管理器', ICMLCLManager, aLCLManager);
    end;
  lclgs := TLCLGlobalSet.Create;
  aLCLManager.SetMainLCLGlobalSet(lclgs);
  Result := aLCLManager;
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


procedure InitLCLSuite;
begin
  InitLCLManager;
  InitLCLGenerator;
  InitLCLPropertyReaderWriter;
end;

function GetLCLManager: TCMLCLManager;
begin
  if not Assigned(aLCLManager) then
    InitLCLManager;
  Result := aLCLManager;
end;

function GetLCLGenerator: TCMLCLGenerator;
begin
  if not Assigned(aLCLGenerator) then
    InitLCLGenerator;
  Result := aLCLGenerator;
end;

{$ENDIF}

initialization
  aRegister := TCMInterfaceRegister.Create(nil);
  InterfaceRegister := aRegister;
  aLoader := TCMInterfaceLoader.Create(nil, aRegister);
  //
  InterfaceRegister.PutInterface('默认接口加载器', ICMInterfaceLoader, ICMInterfaceLoader(aLoader));

finalization
  InterfaceRegister := nil;

end.

