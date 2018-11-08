{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_LCL

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_LCL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, CustApp, InterfaceBase, ActnList, ClipBrd, Controls,
  cm_interfaces;

type

  { ILCLGlobalSet
    // LCL 常用全局数据获取的声明�
  }
  ILCLGlobalSet = interface(ICMBase)
    ['{B8A119F3-000A-447B-84EE-C5E0882F3B10}']
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

  { ICMLCLGlobalManager
    // LCL 全局数据管理器�
  }
  ICMLCLGlobalManager = interface(ICMBase)
    ['{38F96762-4D07-4609-8A9F-4EFEDD020A6D}']
    function SetMainLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
    function GetMainLCLGlobalSet: ILCLGlobalSet;
    function AddLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
    function RemoveLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
  end;

  { ICMLCLWidgetSet
    // LCL 工具集�
  }
  ICMLCLWidgetSet = interface(ICMBase)
    ['{E5C003F1-3FB7-42EC-AFA7-2D7B42F73876}']
    procedure ThreadSynchronize(AThread: TThread; AMethod: TThreadMethod);
    procedure ThreadQueue(AThread: TThread; AMethod: TThreadMethod);
  end;


implementation

end.

