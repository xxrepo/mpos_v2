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
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Grids, Menus, ColorBox, DBGrids, ComCtrls,
  cm_messager,
  cm_InterfaceRegister, cm_InterfaceRegisterImpl,
  cm_InterfaceLoader, cm_InterfaceLoaderImpl
  {$IFDEF LCL},
  cm_LCL, cm_LCLUtils, cm_LCLGlobalSet, cm_LCLPropertyReaderWriter
  {$ENDIF};

procedure SetDefaultMessageHandler(AHandler: ICMMessageHandler);
{$IFDEF LCL}
function InitLCLManager: TCMLCLManager; //Suggested that the main form to create then call.
function InitLCLGenerator: TCMLCLGenerator;
function InitLCLPropertyReaderWriter: TCMLCLPropertyReaderWriter;
{$ENDIF}

var
  InterfaceRegister: ICMInterfaceRegister = nil;

implementation

var
  aRegister: TCMInterfaceRegister = nil;
  aLoader: TCMInterfaceLoader = nil;
  aLCLManager: TCMLCLManager = nil;
  aLCLGenerator: TCMLCLGenerator = nil;
  aLCLPropertyReaderWriter: TCMLCLPropertyReaderWriter = nil;

procedure SetDefaultMessageHandler(AHandler: ICMMessageHandler);
begin
  aRegister.Messager.RemoveAllMessageHandler;
  aRegister.Messager.AddMessageHandler(AHandler);
  aLoader.Messager.RemoveAllMessageHandler;
  aLoader.Messager.AddMessageHandler(AHandler);
  cm_messager.TCMMessageManager.DefaultHandler := AHandler;
  InterfaceRegister.PutInterface('默认信息处理器', ICMMessageHandler, AHandler, DefaultMessageHandlerCode);
  {$IFDEF LCL}
  //以下更换默认信息处理器时，不进行替换了
  if Assigned(aLCLManager) then
    if aLCLManager.Messager.GetMessageHandlerCount = 0 then
      aLCLManager.Messager.AddMessageHandler(AHandler);
  if Assigned(aLCLGenerator) then
    if aLCLGenerator.Messager.GetMessageHandlerCount = 0 then
      aLCLGenerator.Messager.AddMessageHandler(AHandler);
  {$ENDIF}
end;

{$IFDEF LCL}
function InitLCLManager: TCMLCLManager;
var
  lclgs: TLCLGlobalSet;
begin
  Result := nil;
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
  if not Assigned(aLCLPropertyReaderWriter) then
    begin
      aLCLPropertyReaderWriter := TCMLCLPropertyReaderWriter.Create;
      InterfaceRegister.PutInterface('ICMLCLPropertyReaderWriter', ICMLCLPropertyReaderWriter, aLCLPropertyReaderWriter);
    end;
  Result := aLCLPropertyReaderWriter;
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

