unit cm_InterfaceRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_freegenerics;


type

  ICMInterfaceIterator = interface(ICMBase)
    ['{2AC5C523-DF05-4F5A-813B-21989D228597}']
    function HasNext: Boolean;
    function Next(out theDescription: string; out theIID: TGUID; out theIntf; out theCode: string): Boolean; overload;
    function Next(out theIID: TGUID; out theIntf; out theCode: string): Boolean; overload;
    function Next(out theIID: TGUID; out theIntf): Boolean; overload;
  end;

  { ICMInterfaceRegisterEvent }
  ICMInterfaceRegisterEvent = interface(ICMEvent)
    ['{5122621A-43F9-40EA-BB30-D47236F2EB57}']
    function GetDescription: string;
    function GetIID: TGUID;
    function GetIntf: IUnknown;
    function GetCode: string;
    function IsAble: Boolean;
    procedure SetAble(b: Boolean);
  end;

  ICMInterfaceRegisterListener = interface(ICMListener)
    ['{EF41F64D-5472-4ED2-B322-470561041A01}']
    procedure Putting(e: ICMInterfaceRegisterEvent);
    procedure Outting(e: ICMInterfaceRegisterEvent);
    procedure Cutting(e: ICMInterfaceRegisterEvent);
  end;

  TCMInterfaceRegisterListener = specialize TFGInterfaceList<ICMInterfaceRegisterListener>;

  ICMInterfaceRegister = interface(ICMBase)
    ['{CEF63385-A1C0-485A-96A8-612A4B9464B2}']
    function PutInterface(const ADescription: string; const AIID: TGUID; const AIntf: IUnknown; const ACode: string=''): Integer; overload;
    function PutInterface(const AIID: TGUID; const AIntf: IUnknown; const ACode: string=''): Integer; overload;
    function OutInterface(const AIID: TGUID; out theIntf; const ACode: string=''): Boolean;
    function CutInterface(const AIID: TGUID; const ACode: string=''): Integer;
    function Iterator: ICMInterfaceIterator;
    //----------------------------------------------------------------------------------------------
    function IsExist(const AIID: TGUID; const ACode: string=''): Boolean;
    procedure AddInterfaceRegisterListener(l: ICMInterfaceRegisterListener);
    procedure RemoveInterfaceRegisterListener(l: ICMInterfaceRegisterListener);
    function GetInterfaceRegisterListeners: TCMInterfaceRegisterListener;
  end;

  { ICMInterfaceRegisterAdapter }

  ICMInterfaceRegisterAdapter  = class(TCMListener, ICMInterfaceRegisterListener)
  public
    procedure Putting(e: ICMInterfaceRegisterEvent); virtual;
    procedure Outting(e: ICMInterfaceRegisterEvent); virtual;
    procedure Cutting(e: ICMInterfaceRegisterEvent); virtual;
  end;

implementation


{ ICMInterfaceRegisterAdapter }

procedure ICMInterfaceRegisterAdapter.Putting(e: ICMInterfaceRegisterEvent);
begin

end;

procedure ICMInterfaceRegisterAdapter.Outting(e: ICMInterfaceRegisterEvent);
begin

end;

procedure ICMInterfaceRegisterAdapter.Cutting(e: ICMInterfaceRegisterEvent);
begin

end;

end.

