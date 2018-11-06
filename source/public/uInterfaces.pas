unit uInterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager, cm_interfaces;

type

  IService = interface(ICMBase)
    ['{2C96F8FA-1CC4-4E41-A385-3AC5134085D9}']
    function GetServiceCode: string;
  end;

  IBoard = interface(IService)
    ['{4329327A-A1C0-4012-8EAF-97370F7DACF0}']
  end;

  IPromptableBoard = interface(IBoard)
    ['{1E310A8F-E277-4B20-807B-C4B8A01B60C9}']
    procedure PromptMessage(AEventType: TEventType; const AMsg: string);
  end;

  IBusinessService = interface(IService)
    ['{1ADC49D1-B4B8-4C7E-B0EE-3BF43CFFCA9F}']
    function GetBusinessCode: string;
  end;


implementation


end.





