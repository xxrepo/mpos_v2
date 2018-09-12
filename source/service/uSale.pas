unit uSale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_messager,
  uSaleDTO, uSaleBO;


type


  { ISaleBoard } //销售面板，可对其进行的操作

  ISaleBoard = interface(ICMBase)
    ['{2497D987-DF1D-4B1F-BDC1-DBC3045E52AA}']
    function AddShowItem(AVO: TShowItem): Boolean;
    function DeleteShowItem(const AUUID: string): Boolean;
    function UpdateShowItem(AVO: TShowItem): Boolean;
    function SetShowItemList(AVOs: TShowItemList): Boolean;
    function Clear: Boolean;
    procedure AddListener(AListener: ICMListener);
    procedure PromptMessage(et: TEventType; const msg: string);
  end;

  { ISaleBoardListener } //用于接收销售操作的侦听器接口

  ISaleBoardListener = interface(ICMListener)
    ['{AC40A1E7-1132-41EB-89BD-438A19F4B2E2}']
    procedure Deleting(const AUUID: string; var CanDelete: Boolean);
    procedure Updating(AVO: TShowItem; var CanUpdate: Boolean);
    procedure Cleared;
    procedure Inputted(const ACode: string);
    procedure Settle;  //结算
  end;

  { ISaleBillCenter } //销售单据中心，统一管理以方便处理（对象跨库类型不等和生命周期等问题） TODO
  ISaleBillCenter = interface(ICMBase)
    ['{5CC442C5-7166-43B2-8CD9-F502032E8742}']
    function GetCurrBill: TSaleBill;
    function NewBill: Boolean;
    procedure AddListener(AListener: ICMListener);
    //function GetShowItemList: TShowItemList;
  end;

  { ICurrSaleBillListener } //当前销售单据侦听器

  ISaleBillListener = interface(ICMListener)
    ['{5C5E5312-B747-4D79-B9F3-60763EBB5419}']
    procedure BillCreated(ABill: TSaleBill);
    procedure CommodityAdding(ABill: TSaleBill; ACommodity: TSaleCommodity; var CanAdd: Boolean);
    procedure CommodityAdded(ABill: TSaleBill; ACommodity: TSaleCommodity);
    procedure CommodityRemoving(ABill: TSaleBill; ACommodity: TSaleCommodity; var CanDelete: Boolean);
    procedure CommodityRemoved(ABill: TSaleBill; ACommodity: TSaleCommodity);
    procedure CommodityUpdating(ABill: TSaleBill; ACommodity: TSaleCommodity; var CanUpdate: Boolean);
    procedure CommodityUpdated(ABill: TSaleBill; ACommodity: TSaleCommodity);
    procedure BillCleared(ABill: TSaleBill);
  end;

  { ISaleHandler } //销售处理接口

  ISaleHandler = interface(ICMBase)
    ['{3ED1039D-36B7-4F88-82C4-F4DFD29D41D8}']
    function Handle(ABill: TSaleBill): Boolean;
    procedure AddHandler(AHandler: ISaleHandler);
  end;

  { TSaleHandler }

  TSaleHandler = class(TCMMessageable, ISaleHandler)
  private
    FHandlers: TInterfaceList;
  protected
    function DoBeforeHandle(ABill: TSaleBill): Boolean; virtual; abstract;
    function DoAfterHandle(ABill: TSaleBill): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Handle(ABill: TSaleBill): Boolean;
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

function TSaleHandler.DoAfterHandle(ABill: TSaleBill): Boolean;
begin
  Result := True;
end;

function TSaleHandler.Handle(ABill: TSaleBill): Boolean;
var
  i: Integer;
begin
  Result := DoBeforeHandle(ABill);
  for i:=0 to FHandlers.Count-1 do
    begin
      Result := ISaleHandler(FHandlers[i]).Handle(ABill) and Result;
    end;
  Result := DoAfterHandle(ABill) and Result;
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


