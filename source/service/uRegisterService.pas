unit uRegisterService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces, cm_parameter;

type
  IRegisterService = interface(ICMBase)
    ['{E6AF0B7A-7183-4A6B-8200-8E19B1D7FD47}']
    function DoPOSRegister(): boolean;
    function DoAuthorityCheck(): boolean;
    function DoRegisterSubmit(const ShopCode, TermCode, TermUUID: string): boolean;
  end;

implementation

end.


