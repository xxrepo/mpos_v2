{
    This file is part of the CM SDK.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_LCL

    This is not a complete unit, for testing

 **********************************************************************}

unit cm_type;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces;

type

  { IGlobalSet
    // 常用全局数据获取的声明
  }
  IGlobalSet = interface(ICMBase)
    ['{9FA77CBB-5892-4121-93BD-E958A93D9C48}']
    function GetMainThreadID: TThreadID;
    function GetCurrentThreadVar: TThread;
    function GetGlobalNameSpace: IReadWriteSync;
  end;

  { ICMObjectGenerator
    // LCL 产生器
    //    在实际中參数并不能直接使用比 TControl 更原始的类型，但其下属难免不会有更原始的类型，故在
    //这里也提供后两方法
  }

  ICMObjectGenerator = interface(ICMBase)
    ['{01E97410-3288-4551-BFDE-5485DE8F09D3}']
    function NewComponent(const AClassName: string; AOwner: TComponent): TComponent;
    function GetComponentClass(const AClassName: string): TComponentClass;
    function NewObject(const AClassName: string): TObject;
    function GetClass(const AClassName: string): TClass;
  end;

  { ICMObjectPropertyReaderWriter }

  ICMObjectPropertyReaderWriter = interface(ICMBase)
    ['{4719167B-AADD-48B9-8E21-C11A02320722}']
    function  GetOrdProp(Instance: TObject; const PropName: string): Int64;
    procedure SetOrdProp(Instance: TObject; const PropName: string; Value: Int64);
    function  GetEnumProp(Instance: TObject; const PropName: string): string;
    procedure SetEnumProp(Instance: TObject; const PropName: string; const Value: string);
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

implementation

end.

