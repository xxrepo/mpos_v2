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

