unit cm_LCLGlobalSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, CustApp, InterfaceBase, ActnList, ClipBrd, Controls,
  cm_interfaces, cm_LCL;

type

  { TCMLCLGlobalSet }

  TCMLCLGlobalSet = class(TCMBase, ILCLGlobalSet)
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
    function GetRequireDerivedFormResource: Boolean;
    procedure SetRequireDerivedFormResource(AValue: Boolean);
    function GetMessageBoxFunction: TMessageBoxFunction;
    procedure SetMessageBoxFunction(AValue: TMessageBoxFunction);
  end;

implementation

{ TCMLCLGlobalSet }

function TCMLCLGlobalSet.GetApplication: TApplication;
begin
  Result := Forms.Application;
end;

function TCMLCLGlobalSet.GetCustomApplication: TCustomApplication;
begin
  Result := CustApp.CustomApplication;
end;

function TCMLCLGlobalSet.GetScreen: TScreen;
begin
  Result := Forms.Screen;
end;

function TCMLCLGlobalSet.GetWidgetSet: TWidgetSet;
begin
  Result := InterfaceBase.WidgetSet;
end;

function TCMLCLGlobalSet.GetApplicationActionComponent: TComponent;
begin
  Result := ActnList.ApplicationActionComponent;
end;

function TCMLCLGlobalSet.GetClipboard: TClipboard;
begin
  Result := ClipBrd.Clipboard;
end;

function TCMLCLGlobalSet.GetDragManager: TDragManager;
begin
  Result := Controls.DragManager;
end;

function TCMLCLGlobalSet.GetExceptionObject: TExceptObject;
begin
  Result := Forms.ExceptionObject;
end;

function TCMLCLGlobalSet.GetMouse: TMouse;
begin
  Result := Controls.Mouse;
end;

function TCMLCLGlobalSet.GetRequireDerivedFormResource: Boolean;
begin
  Result := Forms.RequireDerivedFormResource;
end;

procedure TCMLCLGlobalSet.SetRequireDerivedFormResource(AValue: Boolean);
begin
  Forms.RequireDerivedFormResource := AValue;
end;

function TCMLCLGlobalSet.GetMessageBoxFunction: TMessageBoxFunction;
begin
  Result := Forms.MessageBoxFunction;
end;

procedure TCMLCLGlobalSet.SetMessageBoxFunction(AValue: TMessageBoxFunction);
begin
  Forms.MessageBoxFunction := AValue;
end;

end.

