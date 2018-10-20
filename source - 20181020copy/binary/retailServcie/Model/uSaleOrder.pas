unit uSaleOrder;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, uStoreSaleOrder, cm_messager;

type
  TSaleOrder = class;


  { TSaleMaster }

  TSaleMaster = class(TPersistent)
  private
    FCanModify: boolean;
    FCardCode: string;
    FCardNeedDestory: boolean;
    FCardNo: string;
    FCardType: integer;
    FCasher: string;
    FCompanyCode: string;
    FCustPayCash: extended;
    FDiscountAmount: extended;
    FDiscountID: string;
    FFixDiscAmt: extended;
    FFixRealAmt: extended;
    FGiveZeroAmt: extended;
    FGUID: string;
    FCode: string;
    FDate: TDatetime;
    FMemberId: string;
    FOrderType: integer;
    FPayedSum: extended;
    FPosNo: string;
    FRealAmt: extended;
    FRecCount: extended;
    FScore: integer;
    FShopCode: string;
    FSrcGUID: string;
    FTotalAmt: extended;
    FTotalQty: extended;
  public
    //..
    property GUID: string read FGUID write FGUID;
    property Code: string read FCode write FCode;
    property Date: TDatetime read FDate write FDate;
    property ShopCode: string read FShopCode write FShopCode;
    property CompanyCode: string read FCompanyCode write FCompanyCode;
    property SrcGUID: string read FSrcGUID write FSrcGUID;
    property PosNo: string read FPosNo write FPosNo;
    property OrderType: integer read FOrderType write FOrderType;
    //优惠卡
    property CardType: integer read FCardType write FCardType;
    property CardCode: string read FCardCode write FCardCode;
    property CardNo: string read FCardNo write FCardNo;
    property CardNeedDestory: boolean read FCardNeedDestory write FCardNeedDestory;
    //会员信息
    property Score: integer read FScore write FScore;
    property MemberId: string read FMemberId write FMemberId;
    property DiscountID: string read FDiscountID write FDiscountID;
    //数量,价格,金额
    property TotalAmt: extended read FTotalAmt write FTotalAmt;                    //订单总金额（不含折扣）
    property TotalQty: extended read FTotalQty write FTotalQty;                    //汇总数量
    property RealAmt: extended read FRealAmt write FRealAmt;                       //应收金额 ＝ 订单总金额－折扣金额
    property RecCount: extended read FRecCount write FRecCount;                    //记录数
    property DiscountAmount: extended read FDiscountAmount write FDiscountAmount;  //折扣金额
    property GiveZeroAmt: extended read FGiveZeroAmt write FGiveZeroAmt;           //找零
    property CustPayCash: extended read FCustPayCash write FCustPayCash;           //用户实付金额
    property PayedSum: extended read FPayedSum write FPayedSum;                    //已支付金额
    property FixDiscAmt: extended read FFixDiscAmt write FFixDiscAmt;              //实际优惠金额
    property FixRealAmt: extended read FFixRealAmt write FFixRealAmt;              //实际应收金额
    //..
    property Casher: string read FCasher write FCasher;                            //收银员
    property CanModify: boolean read FCanModify write FCanModify;                  //是否可修改
  end;

  //销售单库存
  TSaleSubInv = class(TPersistent)
  private
    FGUID: string;
    FITEMNO: integer;
    FGID: integer;
    FEGID: integer;
    FQuantity: extended;
    FPrice: extended;
    FParentGUID: string;
  published
    property GUID: string read FGUID write FGUID;                              //GUID
    property ParentGUID: string read FParentGUID write FParentGUID;            //ParentGUID
    property ItemNo: integer read FItemNo write FItemNo;                       //序号
    property GID: integer read FGID write FGID;
    property EGID: integer read FEGID write FEGID;
    property Quantity: extended read FQuantity write FQuantity;
    property Price: extended read FPRICE write FPRICE;
  end;

  TSaleSubInvs = TObjectList<TSaleSubInv>;


  { TSaleSub }

  TSaleSub = class(TPersistent)
  private
    FAvgPrice: extended;
    FGDCODE: string;
    FGID: integer;
    FITEMNO: integer;
    FInputCode: string;
    FIsChengePrice: boolean;
    FPrice: extended;
    FProdPrice: extended;
    FQuantity: extended;
    FSrcItemNo: integer;
    FGUID: string;
    FParentGUID: string;
    //..
    FProduct: TShopProduct;
    FInnerPackings: TSaleSubInvs;
  published
    property GUID: string read FGUID write FGUID;                                    //GUID
    property ParentGUID: string read FParentGUID write FParentGUID;                  //ParentGUID
    property ItemNo: integer read FItemNo write FItemNo;                             //序号
    property GID: integer read FGID write FGID;                                      //商品GID =>商品条码     ???
    property GDCODE: string read FGDCODE write FGDCODE;                              //条码
    property Quantity: extended read FQuantity write FQuantity;                      //数量
    property Price: extended read FPrice write FPrice;                               //折后价格 零售价
    property IsChengePrice: boolean read FIsChengePrice write FIsChengePrice;        //零售价格是否修改
    property ProdPrice: extended read FProdPrice write FProdPrice;                   //原价
    property AvgPrice: extended read FAvgPrice write FAvgPrice;                      //分滩价格
    property InputCode: string read FInputCode write FInputCode;                     //输入码
    property SrcItemNo: integer read FSrcItemNo write FSrcItemNo;                    //退单商品的原itemno
    //..
    property Product: TShopProduct read FProduct write FProduct;                     //商品
    property InnerPackings: TSaleSubInvs read FInnerPackings write FInnerPackings; //内包装商品清单
  end;

  TSaleSubs = TObjectList<TSaleSub>;

  TSalePay = class(TPersistent)
  private
    FGUID: string;                  //ID
    FParentGUID: string;            //主表ID
    FItemNo: integer;               //序号
    FPayType: integer;              //付款方式
    FPayOrderID: string;            //支付订单ID
    FAmount: extended;              //付款金额
    FDiscount: extended;            //优惠金额
  published
    property GUID: string read FGUID write FGUID;                                    //GUID
    property ParentGUID: string read FParentGUID write FParentGUID;                  //ParentGUID
    property ItemNo: integer read FItemNo write FItemNo;                             //序号
    property PayType: integer read FPayType write FPayType;                          //付款方式
    property PayOrderID: string read FPayOrderID write FPayOrderID;                  //支付订单ID
    property Amount: extended read FAmount write FAmount;                            //付款金额
    property Discount: extended read FDiscount write FDiscount;                      //优惠金额
  end;

  TSalePays = TObjectList<TSalePay>;


  TMemberOrder = class(TPersistent)

  end;

  TMemberOrders = TObjectList<TMemberOrder>;

  TSalePromotion = class(TPersistent)
  private
    FGUID: string;           //ID
    FParentGUID: string;     //主表ID
    FItemNo: integer;        //序号
    FPromType: integer;
    //促销类型代码   促销类型【外卖促销：0】 【单品促销：1】【单品组合：2】【组合促销：3】【总额促销：4】【客单价：5】【卡券:6】【临近商品:7】【赠品:8】【捆绑商品(用于分解):9】【会员:10】【微信:11】【支付宝:12】 【捆绑促销:13】
    FFlowNo: string;         //促销号或着劵号
    FAmount: extended;       //优惠金额
    FPromCode: string;        // billnum
  published
    property GUID: string read FGUID write FGUID;                                    //GUID
    property ParentGUID: string read FParentGUID write FParentGUID;                  //ParentGUID
    property ItemNo: integer read FItemNo write FItemNo;                             //序号
    property FlowNo: string read FFlowNo write FFlowNo;
    property Amount: extended read FAmount write FAmount;
    property PromType: integer read FPromType write FPromType;
    property PromCode: string read FPromCode write FPromCode;
  end;

  TSalePromotions = TObjectList<TSalePromotion>;
  { TSaleOrder }

  TSaleOrder = class(TCMMessageableComponent)
  private
    FPays: TSalePays;
    FMaster: TSaleMaster;
    FDetails: TSaleSubs;
    FMemberOrders: TMemberOrders;
    FPromotions: TSalePromotions;
  public
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(AOwner: TComponent; AMaster: TSaleMaster; ADetails: TSaleSubs; APays: TSalePays;
      AMemberOrders: TMemberOrders; APromotions: TSalePromotions); overload;
    destructor Destroy(); override;
  published
    property Pays: TSalePays read FPays write FPays;
    property Master: TSaleMaster read FMaster write FMaster;
    property Details: TSaleSubs read FDetails write FDetails;
    property Promotions: TSalePromotions read FPromotions write FPromotions;
    property MemberOrders: TMemberOrders read FMemberOrders write FMemberOrders;
  end;

  TSaleOrders = TObjectList<TSaleOrder>;

implementation

{ TSaleSub }

{ TSaleOrder }

constructor TSaleOrder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPays := TSalePays.Create;
  FMaster := TSaleMaster.Create;
  FDetails := TSaleSubs.Create;
  FPromotions := TSalePromotions.Create;
  FMemberOrders := TMemberOrders.Create;
end;

constructor TSaleOrder.Create(AOwner: TComponent; AMaster: TSaleMaster; ADetails: TSaleSubs; APays: TSalePays;
  AMemberOrders: TMemberOrders; APromotions: TSalePromotions);
begin
  inherited Create(AOwner);
  FPays := APays;
  FMaster := AMaster;
  FDetails := ADetails;
  FPromotions := APromotions;
  FMemberOrders := AMemberOrders;
end;

destructor TSaleOrder.Destroy();
begin
  if Assigned(FPays) then
    FreeAndNil(FPays);
  if Assigned(FMaster) then
    FreeAndNil(FMaster);
  if Assigned(FDetails) then
    FreeAndNil(FDetails);
  if Assigned(FMemberOrders) then
    FreeAndNil(FMemberOrders);
  if Assigned(FPromotions) then
    FreeAndNil(FPromotions);
  inherited Destroy();
end;

end.
