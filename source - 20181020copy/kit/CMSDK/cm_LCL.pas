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
    // LCL 常用全局数据获取的声明。
  }
  ILCLGlobalSet = interface(ICMBase)
    ['{46FAB2C6-A6A4-4E95-9AFC-B37B431166ED}']
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

  { ICMLCLGlobalManager
    // LCL 全局数据管理器�a�
  }
  ICMLCLGlobalManager = interface(ICMBase)
    ['{38F96762-4D07-4609-8A9F-4EFEDD020A6D}']
    function SetMainLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
    function GetMainLCLGlobalSet: ILCLGlobalSet;
    function AddLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
    function RemoveLCLGlobalSet(ASet: ILCLGlobalSet): Boolean;
  end;

  { ICMLCLGenerator
    // LCL 产生器
    //    在实际中可能并不能直接使用比 TControl 更原始的类型，但其下属性难免不会有更原始的类型，故在
    //这里也提供后两方法。
  }

  ICMLCLGenerator = interface(ICMBase)
    ['{AB724EDE-A5A1-4994-A728-E5B001E6D6C0}']
    function NewComponent(const AClassName: string; AOwner: TComponent): TComponent;
    function GetComponentClass(const AClassName: string): TComponentClass;
    function NewPersistent(const AClassName: string): TPersistent;
    function GetPersistentClass(const AClassName: string): TPersistentClass;
  end;

  { ICMLCLPropertyReaderWriter }

  ICMLCLPropertyReaderWriter = interface(ICMBase)
    ['{DB9F9229-C965-4910-9F70-9B538C0C965F}']
    function  GetOrdProp(Instance: TObject; const PropName: string): Int64;
    procedure SetOrdProp(Instance: TObject; const PropName: string; Value: Int64);
    function  GetEnumProp(Instance: TObject; const PropName: string): string;
    procedure SetEnumProp(Instance: TObject; const PropName: string;const Value: string);
    function  GetSetProp(Instance: TObject; const PropName: string): string;
    procedure SetSetProp(Instance: TObject; const PropName: string; const Value: string);
    function  GetStrProp(Instance: TObject; const PropName: string): string;
    procedure SetStrProp(Instance: TObject; const PropName: string; const Value: AnsiString);
    function GetWideStrProp(Instance: TObject; const PropName: string): WideString;
    procedure SetWideStrProp(Instance: TObject; const PropName: string; const Value: WideString);
    //
    function GetUnicodeStrProp(Instance: TObject; const PropName: string): UnicodeString;
    procedure SetUnicodeStrProp(Instance: TObject; const PropName: string; const Value: UnicodeString);
    {$IFNDEF FPUNONE}
    function  GetFloatProp(Instance: TObject; const PropName: string): Extended;
    procedure SetFloatProp(Instance: TObject; const PropName: string; Value: Extended);
    {$ENDIF}
    function  GetObjectProp(Instance: TObject; const PropName: string): TObject;
    function  GetObjectProp(Instance: TObject; const PropName: string; MinClass: TClass): TObject;
    procedure SetObjectProp(Instance: TObject; const PropName: string; Value: TObject);
    function  GetObjectPropClass(Instance: TObject; const PropName: string): TClass;
    function  GetObjectPropClass(AClass: TClass; const PropName: string): TClass;
    //
    function  GetMethodProp(Instance: TObject; const PropName: string): TMethod;
    procedure SetMethodProp(Instance: TObject; const PropName: string; const Value: TMethod);
    //
    function  GetInt64Prop(Instance: TObject; const PropName: string): Int64;
    procedure SetInt64Prop(Instance: TObject; const PropName: string;  const Value: Int64);
    //
    function GetPropValue(Instance: TObject; const PropName: string): Variant;
    function GetPropValue(Instance: TObject; const PropName: string; PreferStrings: Boolean): Variant;
    procedure SetPropValue(Instance: TObject; const PropName: string; const Value: Variant);
    function  GetVariantProp(Instance: TObject; const PropName: string): Variant;
    procedure SetVariantProp(Instance: TObject; const PropName: string; const Value: Variant);
    //
    function GetInterfaceProp(Instance: TObject; const PropName: string): IInterface;
    procedure SetInterfaceProp(Instance: TObject; const PropName: string; const Value: IInterface);
    //
    function GetRawInterfaceProp(Instance: TObject; const PropName: string): Pointer;
    procedure SetRawInterfaceProp(Instance: TObject; const PropName: string; const Value: Pointer);
  end;

  { ICMLCLWidgetSet
    // LCL 工具集。
  }
  ICMLCLWidgetSet = interface(ICMBase)
    ['{E5C003F1-3FB7-42EC-AFA7-2D7B42F73876}']
    procedure ThreadSynchronize(AThread: TThread; AMethod: TThreadMethod);
    procedure ThreadQueue(AThread: TThread; AMethod: TThreadMethod);
  end;


implementation

end.

