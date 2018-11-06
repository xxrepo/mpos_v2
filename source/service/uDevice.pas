unit uDevice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPJson,
  cm_messager,
  uInterfaces;

type

  IPrint = interface(IService)
    ['{1F6B3BBD-657D-43DC-8CED-C219E7DB1E2E}']
    function DoPrint(strList: TStringList): boolean;
  end;

  IPOSPrint = interface(IPrint)
    ['{D17E3763-D24B-46B6-A12C-D0BDFE4B4E2B}']
    function DoPrint(Template, Param: TJSONObject): boolean;
    function DoDBPrint(TemplateName: string; Param: TJSONObject): boolean;
    function GetPrintText(Template, Param: TJSONObject): string;
    function GetDBPrintText(TemplateName: string; Param: TJSONObject): string;
    function GetPrintState(Param: TJSONObject): TJSONObject;
  end;

  ICashBox = interface(IService)
    ['{1761CEE3-5873-42DB-AF84-62ED7A22BB3E}']
    function Open(): boolean;
  end;

  ICustScreen = interface(IService)
    ['{A41BD6CF-800F-4DFC-AA18-61B081AFB056}']
    function ShowProductPrice(AAmount: currency): boolean;
    function SetReceAmount(AAmount: currency): boolean;
    function SetRealAmount(AAmount: currency): boolean;
    function SetGiveAmount(AAmount: currency): boolean;
  end;

  IMultiScreen = interface(IService)
    ['{C6A27DC7-B00C-43BD-994C-FFD36F263DBB}']
    function SetShowPayInfo(ProdName, Price, Qty, Amt, Sum: string): boolean;
    function SetShowPayInfo(ProdName, Price, Qty, Amt, Sum, ProdAd: string): boolean;
    function SetShowSellteInfo(ReceAmt, RealAmt, GiveAmt, DistAmt: string): boolean;
    function SetShowNoticeInfo(vNotice: TStringList): boolean;
    function SetShowBMInfo(BMBus: TJSONObject): boolean;
    function SetShowBMInfo(str_left1: string = ''; str_right1: string = ''; str_left2: string = ''; str_right2: string = '';
      str_left3: string = ''; str_right3: string = ''): boolean;
  end;

implementation

end.

