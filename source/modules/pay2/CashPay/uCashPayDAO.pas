unit uCashPayDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uDAO, uCashPayPO;

type


  ICashPayRecordDAO = interface(IPOSDAO)
    ['{C0F57320-08E4-4BE7-82E8-0C1A0DD733C0}']
    function Save(APayRequestCash: TPayRecordCash): boolean;
    function Delete(APayUUID: string): boolean;
    function Update(APayRequestCash: TPayRecordCash): boolean;
  end;

  ICashPayInfoDAO = interface(IPOSDAO)
    ['{6CC5C8BE-9C4D-4201-8484-471C10B0164E}']
    function Save(APayInfoCash: TPayInfoCash): boolean;
    function Delete(AServiceUUID: string): boolean;
    function Update(APayInfoCash: TPayInfoCash): boolean;
  end;

implementation

end.


