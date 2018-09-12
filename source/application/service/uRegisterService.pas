unit uRegisterService;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cm_interfaces;

type
  IRegisterService = interface(ICMBase)
    ['{4B3C4A4B-1891-4CF2-9C5A-74E9C92E698A}']
    function POSRegister(): boolean;
    function DoRegisterSubmit(const ShopCode, TermCode, TermUUID: string; out RetMsg: string): boolean;
  end;


implementation

end.

