unit uSale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_messager,
  uInterfaces, uSaleDTO, uSaleBO, uSalePO, uProductPO;

type


  { ISaleBoard }//销售面板，可对其进行的操作

  ISaleBoard = interface(ICMBase)
    ['{2497D987-DF1D-4B1F-BDC1-DBC3045E52AA}']
    function AddShowItem(AVO: TShowItem): boolean;
    function DeleteShowItem(const AUUID: string): boolean;
    function UpdateShowItem(AVO: TShowItem): boolean;
    function SetShowItemList(AVOs: TShowItemList): boolean;
    function Clear: boolean;
    procedure AddListener(AListener: ICMListener);
    procedure PromptMessage(et: TEventType; const msg: string);
  end;

  IProductSelectBoard = interface(ICMBase)
    ['{2C77D8E1-E4E5-434D-9328-57F5EDD5E9F0}']
    function ToSelect(list: TProductList): TProduct;
  end;

  ISaleOrderSelectBoard = interface(ICMBase)
    ['{D50C5834-38B7-4E95-ABA5-387BDAD21400}']
    function ToSelect(list: TSaleOrderList): TSaleOrder;
  end;


  ISaleQueryListener = interface(ICMListener)
    ['{B01F745A-F78C-4981-BB4C-08FE02839B6E}']
    function Search(const beginTime, endTime, orderNo, prodCode, barCode: string; amount: currency; orderType, payType: integer): boolean;
    function Print(const AOrderUUID: string): boolean;
    function Return(const AOrderUUID: string): boolean;
    function ShowDetails(const AOrderUUID: string): boolean;
  end;


  ISaleQueryBoard = interface(IPromptableBoard)
    ['{B1C69B27-1D4E-435D-B2E0-31790E05C075}']
    procedure SetListener(AListener: ISaleQueryListener);
    procedure StartQuery();

    function AddShowItem(AVO: TSaleQueryShowItem): boolean;
    function DeleteShowItem(const AUUID: string): boolean;
    function UpdateShowItem(AVO: TSaleQueryShowItem): boolean;
    function ShowOrderList(list: TSaleOrderList): boolean;
  end;



  { ISaleBoardListener }//用于接收销售操作的侦听器接口

  ISaleBoardListener = interface(ICMListener)
    ['{0A6442FD-56C8-45A4-9679-4D0A95FB322C}']
    procedure Deleting(const AUUID: string; var CanDelete: boolean);
    procedure Updating(AVO: TShowItem; var CanUpdate: boolean);
    procedure Cleared;
    procedure Inputted(const ACode: string);
    procedure Settle;  //结算
    procedure Recovery;  //恢复
    procedure Query;  //恢复
  end;

  { ISaleBillCenter }//销售单据中心，统一管理以方便处理（对象跨库类型不等和生命周期等问题） TODO
  ISaleBillCenter = interface(ICMBase)
    ['{5CC442C5-7166-43B2-8CD9-F502032E8742}']
    function GetCurrBill: TSaleBill;
    function NewBill: boolean;
    procedure AddListener(AListener: ICMListener);
    //function GetShowItemList: TShowItemList;
  end;

  { ICurrSaleBillListener }//当前销售单据侦听器

  ISaleBillListener = interface(ICMListener)
    ['{5C5E5312-B747-4D79-B9F3-60763EBB5419}']
    procedure BillCreated(ABill: TSaleBill);
    procedure CommodityAdding(ABill: TSaleBill; ACommodity: TSaleCommodity; var CanAdd: boolean);
    procedure CommodityAdded(ABill: TSaleBill; ACommodity: TSaleCommodity);
    procedure CommodityRemoving(ABill: TSaleBill; ACommodity: TSaleCommodity; var CanDelete: boolean);
    procedure CommodityRemoved(ABill: TSaleBill; ACommodity: TSaleCommodity);
    procedure CommodityUpdating(ABill: TSaleBill; ACommodity: TSaleCommodity; var CanUpdate: boolean);
    procedure CommodityUpdated(ABill: TSaleBill; ACommodity: TSaleCommodity);
    procedure BillCleared(ABill: TSaleBill);
  end;

  { ISaleHandler }//销售处理接口

  ISaleHandler = interface(ICMBase)
    ['{1FA20EBA-6923-42C2-AEBD-C8775C54E29C}']
    function Handle(ABill: TSaleBill): boolean;
    procedure AddHandler(AHandler: ISaleHandler);
  end;

  ISalePrintHandler = interface(ICMBase)
    ['{E4EC0067-6688-4644-B7BE-6D0BD4863A20}']
    //procedure doPrintByOrder(OrderNo);
  end;

  { TSaleHandler }

  TSaleHandler = class(TCMMessageable, ISaleHandler)
  private
    FHandlers: TInterfaceList;
  protected
    function DoBeforeHandle(ABill: TSaleBill): boolean; virtual;
    function DoAfterHandle(ABill: TSaleBill): boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Handle(ABill: TSaleBill): boolean;
    procedure AddHandler(AHandler: ISaleHandler);
  end;

implementation

{ TSaleHandler }

constructor TSaleHandler.Create;
begin
  inherited Create;
  FHandlers := TInterfaceList.Create;
end;

destructor TSaleHandler.Destroy;
begin
  FHandlers := nil;
  inherited Destroy;
end;

function TSaleHandler.DoBeforeHandle(ABill: TSaleBill): boolean;
begin
  Result := True;
end;

function TSaleHandler.DoAfterHandle(ABill: TSaleBill): boolean;
begin
  Result := True;
end;

function TSaleHandler.Handle(ABill: TSaleBill): boolean;
var
  i: integer;
begin
  Result := DoBeforeHandle(ABill);

  for i := 0 to FHandlers.Count - 1 do
  begin
    if not ISaleHandler(FHandlers[i]).Handle(ABill) then
    begin
      Messager.Debug('Handler[%d]: False...', [i]);
      Result := False;
      Exit;
    end;
    Messager.Debug('Handler[%d]: True...', [i]);
  end;
  Result := DoAfterHandle(ABill);
end;

procedure TSaleHandler.AddHandler(AHandler: ISaleHandler);
begin
  if Supports(AHandler, ISaleHandler) then
  begin
    Messager.Debug('AddHandler:[%s]...', [AHandler.GetImplementorName]);
    FHandlers.Add(AHandler);
  end;
end;


end.


