{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_LCLGlobalSet

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_LCLGlobalSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, CustApp, InterfaceBase, ActnList, ClipBrd, Controls,
  cm_interfaces,
  cm_LCL;

type

  { TLCLGlobalSet }

  TLCLGlobalSet = class(TCMBase, ILCLGlobalSet)
  public
    function GetApplication: TApplication;
    function GetCustomApplication: TCustomApplication;
    function GetScreen: TScreen;
    function GetWidgetSet: TWidgetSet;
    function GetApplicationActionComponent: TComponent;
    function GetClipboard: TClipboard;
    function GetDragManager: TDragManager;
    function GetExceptionObject: TExceptObject;
    function GetMouse: TMouse;
    function GetMainThreadID: TThreadID;
    function GetGlobalNameSpace: IReadWriteSync;
    function GetRequireDerivedFormResource: Boolean;
    procedure SetRequireDerivedFormResource(AValue: Boolean);
    function GetMessageBoxFunction: TMessageBoxFunction;
    procedure SetMessageBoxFunction(AValue: TMessageBoxFunction);
  end;

implementation

{ TLCLGlobalSet }

function TLCLGlobalSet.GetApplication: TApplication;
begin
  Result := Forms.Application;
end;

function TLCLGlobalSet.GetCustomApplication: TCustomApplication;
begin
  Result := CustApp.CustomApplication;
end;

function TLCLGlobalSet.GetScreen: TScreen;
begin
  Result := Forms.Screen;
end;

function TLCLGlobalSet.GetWidgetSet: TWidgetSet;
begin
  Result := InterfaceBase.WidgetSet;
end;

function TLCLGlobalSet.GetApplicationActionComponent: TComponent;
begin
  Result := ActnList.ApplicationActionComponent;
end;

function TLCLGlobalSet.GetClipboard: TClipboard;
begin
  Result := ClipBrd.Clipboard;
end;

function TLCLGlobalSet.GetDragManager: TDragManager;
begin
  Result := Controls.DragManager;
end;

function TLCLGlobalSet.GetExceptionObject: TExceptObject;
begin
  Result := Forms.ExceptionObject;
end;

function TLCLGlobalSet.GetMouse: TMouse;
begin
  Result := Controls.Mouse;
end;

function TLCLGlobalSet.GetMainThreadID: TThreadID;
begin
  Result := Classes.MainThreadID;
end;

function TLCLGlobalSet.GetGlobalNameSpace: IReadWriteSync;
begin
  Result := Classes.GlobalNameSpace;
end;

function TLCLGlobalSet.GetRequireDerivedFormResource: Boolean;
begin
  Result := Forms.RequireDerivedFormResource;
end;

procedure TLCLGlobalSet.SetRequireDerivedFormResource(AValue: Boolean);
begin
  Forms.RequireDerivedFormResource := AValue;
end;

function TLCLGlobalSet.GetMessageBoxFunction: TMessageBoxFunction;
begin
  Result := Forms.MessageBoxFunction;
end;

procedure TLCLGlobalSet.SetMessageBoxFunction(AValue: TMessageBoxFunction);
begin
  Forms.MessageBoxFunction := AValue;
end;

end.

