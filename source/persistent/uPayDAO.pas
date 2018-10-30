unit uPayDAO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_Interfaces,
  uPayPO, uDAO;

type



  { IPayRecordDAO }

  IPayRecordDAO = interface(IPOSDAO)
    ['{A7421611-9264-4586-86C2-C2BEF832944C}']
    function ExistPayRecord(APayRecordPO: TPayRecordPO): boolean;
    function SavePayRecord(APayRecordPO: TPayRecordPO): boolean;
    function GetPayRecord(const APayUUID: string): TPayRecordPO;
    function GetPayRecordList(const AOrderUUID: string): TPayRecordPOList;
    function GetPayedAmountByOrderUUID(const AOrderUUID: string): currency;
  end;

  { IPayAssignDAO }
  IPayAssignDAO = interface(IPOSDAO)
    ['{6E6BB666-C8E3-44EE-8611-5AC78EFC4DCE}']
    function ExistPayAssign(APayAssignPO: TPayAssignPO): boolean;
    function SavePayAssign(APayAssignPO: TPayAssignPO): boolean;
    function GetPayAssign(const AAssignUUID: string): TPayAssignPO;
    function GetPayAssignList(const APayUUID: string): TPayAssignPOList;
    function GetPayedAmountByPayUUID(const APayUUID: string): currency;
  end;


  { IPayInfoDAO }

  IPayInfoDAO = interface(IPOSDAO)
    ['{C64BF0F8-F90B-4124-9EDE-8451751766DE}']
    function ExistPayInfo(APayInfoPO: TPayInfoPO): boolean;
    function SavePayInfo(APayInfoPO: TPayInfoPO): boolean;
    function GetPayInfo(const AServiceUUID: string): TPayInfoPO;
    function GetPayInfoList(const AAssignUUID: string): TPayInfoPOList;
  end;

  IPayTypeDAO = interface(IPOSDAO)
    ['{A9322A95-EF07-49F5-9B93-15E62F235083}']
    function ExistPayType(const APayCode: string): boolean;
    function GetPayType(const APayCode: string): TPayTypePO;
    function GetPayTypeList(): TPayTypePOList;
  end;

implementation

end.

