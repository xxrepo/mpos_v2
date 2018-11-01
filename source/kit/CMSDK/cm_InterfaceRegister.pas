unit cm_InterfaceRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces;


type

  ICMInterfaceIterator = interface(ICMBase)
    ['{2AC5C523-DF05-4F5A-813B-21989D228597}']
    function HasNext: Boolean;
    function Next(out theDescription: string; out theIID: TGUID; out theIntf; out theCode: string): Boolean; overload;
    function Next(out theIID: TGUID; out theIntf; out theCode: string): Boolean; overload;
    function Next(out theIID: TGUID; out theIntf): Boolean; overload;
  end;

  { ICMInterfaceRegisterEvent
    // 表示接口寄存器事件
  }
  ICMInterfaceRegisterEvent = interface(ICMEvent)
    ['{5122621A-43F9-40EA-BB30-D47236F2EB57}']
    function GetDescription: string;
    function GetIID: TGUID;
    function GetIntf: IUnknown;
    function GetCode: string;
  end;

  ICMInterfaceRegisterListener = interface(ICMListener)
    ['{EF41F64D-5472-4ED2-B322-470561041A01}']
    procedure Putting(e: ICMInterfaceRegisterEvent);
    procedure Outting(e: ICMInterfaceRegisterEvent);
    procedure Cutting(e: ICMInterfaceRegisterEvent);
  end;

  ICMInterfaceRegister = interface(ICMBase)
    ['{5FA35DFF-5227-4B60-B8D4-7BDD6453FF18}']
    function PutInterface(const ADescription: string; const AIID: TGUID; const AIntf: IUnknown; const ACode: string=''): Integer; overload;
    function PutInterface(const AIID: TGUID; const AIntf: IUnknown; const ACode: string=''): Integer; overload;
    function OutInterface(const AIID: TGUID; out theIntf; const ACode: string=''): Boolean;
    function CutInterface(const AIID: TGUID; const ACode: string=''): Integer;
    function Iterator: ICMInterfaceIterator;
  end;

implementation


end.

