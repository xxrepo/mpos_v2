{




}

unit uSaleDeal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs,
  cm_interfaces, cm_messager,
  uSale,
  uSaleDTO, uSaleBO,
  uMPOS;

type

  TCommodityDealEvent = procedure(Sender: TObject; ACommodity: TSaleCommodity; var CanDeal: Boolean) of object;

  TCommodityNotifyEvent = procedure(Sender: TObject; ACommodity: TSaleCommodity) of object;

  { TDealSaleBill }

  TDealSaleBill = class(TSaleBill)
  private
    FOnAddingCommodity: TCommodityDealEvent;
    FOnAddedCommodity: TCommodityNotifyEvent;
    FOnRemovingCommodity: TCommodityDealEvent;
    FOnRemovedCommodity: TCommodityNotifyEvent;
    FOnUpdatingCommodity: TCommodityDealEvent;
    FOnUpdatedCommodity: TCommodityNotifyEvent;
    FOnCleared: TNotifyEvent;
  public
    constructor Create;
    function AddCommodity(ASampleCommodity: TSaleCommodity): Integer; override;
    function RemoveCommodity(const ACommodityUUID: string): Integer; override;
    function UpdateCommodity(ASampleCommodity: TSaleCommodity): Boolean; override;
    procedure NotifyUpdate(const ACommodityUUID: string); override;
    procedure Clear; override;
  public
    property OnAddingCommodity: TCommodityDealEvent read FOnAddingCommodity write FOnAddingCommodity;
    property OnAddedCommodity: TCommodityNotifyEvent read FOnAddedCommodity write FOnAddedCommodity;
    property OnRemovingCommodity: TCommodityDealEvent read FOnRemovingCommodity write FOnRemovingCommodity;
    property OnRemovedCommodity: TCommodityNotifyEvent read FOnRemovedCommodity write FOnRemovedCommodity;
    property OnUpdatingCommodity: TCommodityDealEvent read FOnUpdatingCommodity write FOnUpdatingCommodity;
    property OnUpdatedCommodity: TCommodityNotifyEvent read FOnUpdatedCommodity write FOnUpdatedCommodity;
    property OnCleared: TNotifyEvent read FOnCleared write FOnCleared;
  end;

  { TSaleDeal }

  TSaleDeal = class(TCMMessageable, ISaleBoardListener, ISaleBillCenter)
  private
    //FShowItemList: TShowItemList;
    FCurrBill: TDealSaleBill;
    FSaleBoard: ISaleBoard;
    FBillListenerList: TInterfaceList;
    FSaleHandler: ISaleHandler;
  public
    constructor Create;
    destructor Destroy; override;
  private
    procedure doBillEvent(doType: Word);
    procedure billAddingCommodity(Sender: TObject; ACommodity: TSaleCommodity; var CanDeal: Boolean);
    procedure billAddedCommodity(Sender: TObject; ACommodity: TSaleCommodity);
    procedure billRemovingCommodity(Sender: TObject; ACommodity: TSaleCommodity; var CanDeal: Boolean);
    procedure billRemovedCommodity(Sender: TObject; ACommodity: TSaleCommodity);
    procedure billUpdatingCommodity(Sender: TObject; ACommodity: TSaleCommodity; var CanDeal: Boolean);
    procedure billUpdatedCommodity(Sender: TObject; ACommodity: TSaleCommodity);
    procedure billClear(Sender: TObject);
  public //ISaleBoardListener
    procedure Deleting(const AUUID: string; var CanDelete: Boolean);
    procedure Updating(AVO: TShowItem; var CanUpdate: Boolean);
    procedure Cleared;
    procedure Inputted(const ACode: string);
    procedure Settle;
  public //ISaleBillCenter
    function GetCurrBill: TSaleBill;
    function NewBill: Boolean;
    procedure AddListener(AListener: ICMListener);
  end;

  procedure OutVOByDTO(ACommodity: TSaleCommodity; out VO: TShowItem);
  procedure SetDTOByVO(AVO: TShowItem; ACommodity: TSaleCommodity);

implementation

procedure OutVOByDTO(ACommodity: TSaleCommodity; out VO: TShowItem);
begin
  VO := TShowItem.Create(ACommodity.UUID);
  VO.Name := ACommodity.Name;
  VO.BarCode := ACommodity.BarCode;
  VO.Price := ACommodity.SettlementPrice;
  VO.Quantity := ACommodity.Quantity;
  VO.Remark := ACommodity.Remark;
end;

procedure SetDTOByVO(AVO: TShowItem; ACommodity: TSaleCommodity);
begin
  ACommodity.SettlementPrice := AVO.Price;
  ACommodity.Quantity := AVO.Quantity;
end;

{ TDealSaleBill }

constructor TDealSaleBill.Create;
begin
  inherited Create;
  FOnAddingCommodity := nil;
  FOnAddedCommodity := nil;
  FOnRemovingCommodity := nil;
  FOnRemovedCommodity := nil;
  FOnUpdatingCommodity := nil;
  FOnUpdatedCommodity := nil;
  FOnCleared := nil;
end;

function TDealSaleBill.AddCommodity(ASampleCommodity: TSaleCommodity): Integer;
var
  canAdd: Boolean;
begin
  Result := -1;
  if Assigned(FOnAddingCommodity) then
    begin
      canAdd := True;
      FOnAddingCommodity(Self, ASampleCommodity, canAdd);
      if not canAdd then
        Exit;
    end;
  Result := inherited AddCommodity(ASampleCommodity);
  if (Result >= 0) and Assigned(FOnAddedCommodity) then
    FOnAddedCommodity(Self, ASampleCommodity);
end;

function TDealSaleBill.RemoveCommodity(const ACommodityUUID: string): Integer;
var
  c, cc: TSaleCommodity;
  canRemove: Boolean;
begin
  Result := -1;
  c := FCommodityList.Find(ACommodityUUID);
  if Assigned(c) then
    begin
      cc := TSaleCommodity.Create(c.UUID);
      try
        cc.Assign(c);
        if Assigned(FOnRemovingCommodity) then
          begin
            canRemove := True;
            FOnRemovingCommodity(Self, cc, canRemove);
            if not canRemove then
              Exit;
          end;
        Result := inherited RemoveCommodity(ACommodityUUID);
        if (Result >= 0) and Assigned(FOnRemovedCommodity) then
          FOnRemovedCommodity(Self, cc);
      finally
        cc.Free;
      end;
    end;
end;

function TDealSaleBill.UpdateCommodity(ASampleCommodity: TSaleCommodity): Boolean;
var
  canUpdate: Boolean;
begin
  Result := False;
  if Assigned(FOnUpdatingCommodity) then
    begin
      canUpdate := True;
      FOnUpdatingCommodity(Self, ASampleCommodity, canUpdate);
      if not canUpdate then
        Exit;
    end;
  Result := inherited UpdateCommodity(ASampleCommodity);
  if Result and Assigned(FOnUpdatedCommodity) then
    FOnUpdatedCommodity(Self, ASampleCommodity);
end;

procedure TDealSaleBill.NotifyUpdate(const ACommodityUUID: string);
var
  c: TSaleCommodity;
  canUpdate: Boolean;
begin
  if Assigned(FOnUpdatingCommodity) then
    begin
      c := FCommodityList.Find(ACommodityUUID);
      if Assigned(c) then
        begin
          canUpdate := True;
          FOnUpdatingCommodity(Self, c, canUpdate);
        end;
    end;
end;

procedure TDealSaleBill.Clear;
begin
  inherited Clear;
  if Assigned(FOnCleared) then
    FOnCleared(Self);
end;

{ TSaleDeal }

constructor TSaleDeal.Create;
begin
  inherited Create;
  FBillListenerList := TInterfaceList.Create;
  FCurrBill := nil;
  FSaleBoard := nil;
  FSaleHandler := nil;
end;

destructor TSaleDeal.Destroy;
begin
  if Assigned(FCurrBill) then
    FCurrBill.Free;
  FBillListenerList := nil;
  inherited Destroy;
end;

//private
procedure TSaleDeal.doBillEvent(doType: Word);
var
  i: Integer;
begin
  Messager.Debug('doBillEvent(%d, TSaleCommodity)...', [doType]);
  for i:=0 to FBillListenerList.Count-1 do
    begin
      case doType of
      1: ISaleBillListener(FBillListenerList[i]).BillCreated(FCurrBill);
      5: ISaleBillListener(FBillListenerList[i]).BillCleared(FCurrBill);
      end;
    end;
end;

procedure TSaleDeal.billAddingCommodity(Sender: TObject; ACommodity: TSaleCommodity; var CanDeal: Boolean);
var
  vo: TShowItem;
  i: Integer;
  canAdd: Boolean;
begin
  //触发单据事件
  for i:=0 to FBillListenerList.Count-1 do
    begin
      canAdd := True;
      ISaleBillListener(FBillListenerList[i]).CommodityAdding(FCurrBill, ACommodity, canAdd);
      CanDeal := CanDeal and canAdd;
    end;
  //往销售板增加项目
  if CanDeal then
    begin
      OutVOByDTO(ACommodity, vo);
      if Assigned(FSaleBoard) or InterfaceRegister.OutInterface(ISaleBoard, FSaleBoard) then
        FSaleBoard.AddShowItem(vo);
      vo.Free;
    end;
end;

procedure TSaleDeal.billAddedCommodity(Sender: TObject; ACommodity: TSaleCommodity);
var
  i: Integer;
begin
  for i:=0 to FBillListenerList.Count-1 do
    ISaleBillListener(FBillListenerList[i]).CommodityAdded(FCurrBill, ACommodity);
end;

procedure TSaleDeal.billRemovingCommodity(Sender: TObject; ACommodity: TSaleCommodity; var CanDeal: Boolean);
var
  i: Integer;
  canRemove: Boolean;
begin
  //触发单据事件
  for i:=0 to FBillListenerList.Count-1 do
    begin
      canRemove := True;
      ISaleBillListener(FBillListenerList[i]).CommodityRemoving(FCurrBill, ACommodity, canRemove);
      CanDeal := CanDeal and canRemove;
    end;
  //删除销售板项目
  if CanDeal then
    begin
      if Assigned(FSaleBoard) or InterfaceRegister.OutInterface(ISaleBoard, FSaleBoard) then
        FSaleBoard.DeleteShowItem(ACommodity.UUID);
    end;
end;

procedure TSaleDeal.billRemovedCommodity(Sender: TObject; ACommodity: TSaleCommodity);
var
  i: Integer;
begin
  for i:=0 to FBillListenerList.Count-1 do
    ISaleBillListener(FBillListenerList[i]).CommodityRemoved(FCurrBill, ACommodity);
end;

procedure TSaleDeal.billUpdatingCommodity(Sender: TObject; ACommodity: TSaleCommodity; var CanDeal: Boolean);
var
  i: Integer;
  vo: TShowItem;
  CanUpdate: Boolean;
begin
  //触发单据事件
  for i:=0 to FBillListenerList.Count-1 do
    begin
      CanUpdate := True;
      ISaleBillListener(FBillListenerList[i]).CommodityUpdating(FCurrBill, ACommodity, CanUpdate);
      CanDeal := CanDeal and CanUpdate;
    end;
  //更新销售板项目
  if CanDeal then
    begin
      OutVOByDTO(ACommodity, vo);
      if Assigned(FSaleBoard) or InterfaceRegister.OutInterface(ISaleBoard, FSaleBoard) then
        FSaleBoard.UpdateShowItem(vo);
      vo.Free;
    end;
end;

procedure TSaleDeal.billUpdatedCommodity(Sender: TObject; ACommodity: TSaleCommodity);
var
  i: Integer;
begin
  for i:=0 to FBillListenerList.Count-1 do
    ISaleBillListener(FBillListenerList[i]).CommodityUpdated(FCurrBill, ACommodity);
end;

procedure TSaleDeal.billClear(Sender: TObject);
begin
  if (not Assigned(FSaleBoard)) and (not InterfaceRegister.OutInterface(ISaleBoard, FSaleBoard)) then
    Exit;
  FSaleBoard.Clear;
  doBillEvent(5);
end;

function TSaleDeal.GetCurrBill: TSaleBill;
begin
  if not Assigned(FCurrBill) then
    Self.NewBill;
  Result := FCurrBill;
end;

function TSaleDeal.NewBill: Boolean;
begin
  Result := False;
  try
    if Assigned(FCurrBill) then
      FCurrBill.Free;
  finally
    FCurrBill := TDealSaleBill.Create;
    FCurrBill.OnAddingCommodity := @billAddingCommodity;
    FCurrBill.OnAddedCommodity := @billAddedCommodity;
    FCurrBill.OnRemovingCommodity := @billRemovingCommodity;
    FCurrBill.OnRemovedCommodity := @billRemovedCommodity;
    FCurrBill.OnUpdatingCommodity := @billUpdatingCommodity;
    FCurrBill.OnUpdatedCommodity := @billUpdatedCommodity;
    FCurrBill.OnCleared := @billClear;
    doBillEvent(1);
    Result := True;
  end;
end;

procedure TSaleDeal.AddListener(AListener: ICMListener);
begin
  if Supports(AListener, ISaleBillListener) then
    FBillListenerList.Add(AListener);
end;


(**************************** ISaleBoardListener ***************************************)

procedure TSaleDeal.Deleting(const AUUID: string; var CanDelete: Boolean);
begin
  Messager.Debug('Delete()...');
  CanDelete := False;
  if GetCurrBill.ExistCommodity(AUUID) then
    CanDelete := GetCurrBill.RemoveCommodity(AUUID) >= 0
  else
    begin
      Messager.Debug('Delete() 没有commodity: %s', [AUUID]);
      CanDelete := True;
    end;
end;

procedure TSaleDeal.Updating(AVO: TShowItem; var CanUpdate: Boolean);
var
  commodity, c: TSaleCommodity;
begin
  Messager.Debug('Update()...');
  CanUpdate := False;
  commodity := GetCurrBill.FindCommodity(AVO.UUID);
  if Assigned(commodity) then
    begin
      c := TSaleCommodity.Create(commodity.UUID);
      SetDTOByVO(AVO, c);
      CanUpdate := GetCurrBill.UpdateCommodity(c);
      c.Free;
    end;
end;

procedure TSaleDeal.Cleared;
begin
  Messager.Debug('Clear()...');
  GetCurrBill.Clear;
end;

procedure TSaleDeal.Inputted(const ACode: string);
var
  commodity: TSaleCommodity;
begin
  Messager.Info('InputCode(%s)...', [ACode]);
  //
  commodity := TSaleCommodity.Create;
  commodity.Name := '椰树椰子汁 2000ml/罐';
  commodity.BarCode := '632' + ACode;
  commodity.Price := 10.68;
  commodity.SettlementPrice := 10.68;
  commodity.Quantity := 2;
  //
  GetCurrBill.AddCommodity(commodity);
  commodity.Free;
end;

procedure TSaleDeal.Settle;
begin
  Messager.Info('开始结算...');
  if Assigned(FSaleBoard) or InterfaceRegister.OutInterface(ISaleBoard, FSaleBoard) then
    begin
      if GetCurrBill.CommodityCount < 1 then
        begin
          FSaleBoard.PromptMessage(etError, '请先输入商品！');
        end;
      if Assigned(FSaleHandler) or InterfaceRegister.OutInterface(ISaleHandler, FSaleHandler) then
        begin
          Messager.Info('handler.......................');
          FSaleHandler.Handle(GetCurrBill);
        end;
      Messager.Info('handle over.');
      //FSaleBoard 须在前
      FSaleBoard.Clear;
      Self.NewBill;
    end;
end;


end.


