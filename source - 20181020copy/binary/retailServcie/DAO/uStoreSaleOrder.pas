unit uStoreSaleOrder;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type

  { TStoreSaleMain }

  TStoreSaleMain = class(TPersistent)
  private
    FCompanyCode: string;
    FShopCode: string;
    //   _TerminalNo:string;
    FUUID: string;       //ID
    FSRCUUID: string;    //ID
    FFLOWNO: string;            //流水号
    FPOSNO: string;             //POS编号
    FFILDATE: TDatetime;        //销售时间
    FCASHIER: string;           //收银员
    FTOTAL: currency;           //订单总金额（不含折扣）
    FREALAMT: currency;         //应收金额 ＝ 订单总金额－折扣金额
    FRECCNT: currency;          //记录数
    FSCORE: integer;            //积分
    FCARDCODE: string;          //会员卡号
    FDiscountAmount: currency;  //折扣金额
    FGiveZeroAmt: currency;     //找零
    FCustPayCash: currency;     //用户实付金额
    FMEMO: string;       //备注
    FPayedSum: currency;       //已支付金额
    //------------表没有的字段----------
    FCASHIERIDANDNAME: string;    //挂单显示专用用户名和编号
    FOrderType: integer;        //类型  0 销售单 1 冲单 2 提货单 3 凭码兑奖-- 11 外卖订单
    //_SrcFLOWNO: UnicodeString; //原单号
    //_bPaySucc: boolean;
    //冲单用临时变量
    FSrcFlowNo: string;
    FDiscountID: string;//优惠信息唯一编码 {会员价专用}
    FMemberId: string;//后台返回的会员编号  {会员价专用}
    //2016 10 28 hxl 新增订单是否处于锁定状态
    FBoSaleLock: boolean;
    //-----------------
    FCardNo: string;            //卡劵号
    FCardType: integer;  //卡劵类型  1 ,提货劵 ， 2 拆扣劵  3，代金劵
    FisDestory: boolean;    //卡劵情况下是否核销此劵
    //--------------------------------
    FixDiscAmt: currency; //实际优惠金额
    FixRealAmt: currency; //实际应收金额

    FStreamMsg: string;

    FTotalQty: currency;//汇总数量

    FCode: string;
  published

    property _CompanyCode: string read FCompanyCode write FCompanyCode;
    property _ShopCode: string read FShopCode write FShopCode;
    //   _TerminalNo:string;
    property _UUID: string read FUUID write FUUID;
    property _SrcUUID: string read FSRCUUID write FSRCUUID;
    property _FlowNo: string read FFLOWNO write FFLOWNO;
    property _PosNo: string read FPOSNO write FPOSNO;
    property _FilDate: TDatetime read FFILDATE write FFILDATE;
    property _Cashier: string read FCASHIER write FCASHIER;
    property _Total: currency read FTOTAL write FTOTAL;
    property _RealAmt: currency read FREALAMT write FREALAMT;
    property _RecCnt: currency read FRECCNT write FRECCNT;
    property _SCore: integer read FSCORE write FSCORE;
    property _CardCode: string read FCARDCODE write FCARDCODE;
    property _DiscountAmount: currency read FDiscountAmount write FDiscountAmount;
    property _GiveZeroAmt: currency read FGiveZeroAmt write FGiveZeroAmt;
    property _CustPayCash: currency read FCustPayCash write FCustPayCash;
    property _Memo: string read FMEMO write FMEMO;
    property _PayedSum: currency read FPayedSum write FPayedSum;
    //------------表没有的字段----------
    property _CashierIdAndName: string read FCASHIERIDANDNAME write FCASHIERIDANDNAME;
    property _OrderType: integer read FOrderType write FOrderType;
    property _SrcFlowNo: string read FSrcFlowNo write FSrcFlowNo;
    property _DiscountID: string read FDiscountID write FDiscountID;
    property _MemberId: string read FMemberId write FMemberId;
    //-----------------
    property _CardNo: string read FCardNo write FCardNo;
    property _CardType: integer read FCardType write FCardType;
    property _isDestory: boolean read FisDestory write FisDestory;

    property _FixDiscAmt: currency read FixDiscAmt write FixDiscAmt;
    property _FixRealAmt: currency read FixRealAmt write FixRealAmt;
    property _BoSaleLock: boolean read FBoSaleLock write FBoSaleLock;
    property _TotalQty: currency read FTotalQty write FTotalQty;
  end;

  TStoreSaleMains = TObjectList<TStoreSaleMain>;




  { TStoreSaleSub }

  TStoreSaleSub = class(TPersistent)
  private
    FGUID: string;
    FParentGUID: string;    //ID
    FITEMNO: integer;           //序号
    FGID: integer;              //商品GID =>商品条码     ???
    FQTY: currency;             //数量
    FPRICE: currency;           // 折后价格 零售价
    FProdPrice: currency;       //原价
    FAvgPrice: currency;        //分滩价格
    FGDCODE: string;            //条码
    FWRH: string;               //仓位
    FSRCITEMNO: integer;        //退单商品的原itemno
    FIsChangePrice: boolean;    //是否改价格
    FInputBarCode: string;      //输入码
    //----------表没有的字段----------------
    //FProductCode: string;
    //FPrcType: integer;          //0- 门店不可变价  1-门店可以变价
    //FProdName: string;          //商品名称
    //FNetWeight: string;         //净含量
    //FProdUnit: string;          //单位;
    //FStockAmount: currency;     //库存数量
    //FPriceRate: string;  //拆扣
    //FSettleAmount: currency;    //总价
    //FProdSettleAmt: currency;   //原价总计
    //FAvgAmount: currency;       //分摊价总计
    //FRemark: string;            //备注
    //FMemberGetMTicketInfo: boolean;//是否取会员广告信息
    //FIsPkg: boolean;       //是否有大小包//新增大小包
    //FIsBind: boolean;      //是否绑定商品//新增大小包
    //FIsZengPing: boolean;           //1为赠品 0 为正常商品 //促销用的变量
    //FPromotionAD: string;           //促销广告语     //促销用的变量
    //FDiscountAmt: currency;         //促销优惠金额  //促销用的变量
    //FSortCode: string;
    //FIsHuanGou: boolean;            //是否换购       //促销用的变量
    //FbillNum: string;               //促销单号billnum //促销用的变量
    //FFlag: string;                  //促销类型        //促销用的变量
    //FShortExpProduct: boolean;       //是否临保商品     //促销用的变量
    //FPiShortExpType: string;         //参与短保中的促销类型  //促销用的变量
    //FPRMNO: integer;                  // 赠品参与的活动序号    //促销用的变量
    //FCLS: string;                    // 赠品类种：总额。客单。单品。捆绑 //促销用的变量
    //FDisRatio: integer;              // 分摊比率 数值*100 为保存   //促销用的变量
    //FPromotionType: integer;         // 赠送该商品所发生的促销种类，仅在赠品使用   //促销用的变量
    //FBillLIne: string;              //促销用的变量
    //FTablePrice: currency;          //改价格用                //商品在本地数据库中的原价
    //FGuarDisCount: currency;         //商品的折扣，默认是1 （100%），如果是临期商品，则会小于1，改价时自动按照此折扣计算新的价格
    //FBGuar: boolean;                //是否是临期改价商品
    //FIsDisp: boolean;   //电子称
  published
    property _GUID: string read FGUID write FGUID;
    property _ParentGUID: string read FParentGUID write FParentGUID;
    property _ITEMNO: integer read FITEMNO write FITEMNO;
    property _GID: integer read FGID write FGID;
    property _QTY: currency read FQTY write FQTY;
    property _PRICE: currency read FPRICE write FPRICE;
    property _ProdPrice: currency read FProdPrice write FProdPrice;
    property _AvgPrice: currency read FAvgPrice write FAvgPrice;
    property _GDCODE: string read FGDCODE write FGDCODE;
    property _WRH: string read FWRH write FWRH;
    property _SRCITEMNO: integer read FSRCITEMNO write FSRCITEMNO;
    property _IsChangePrice: boolean read FIsChangePrice write FIsChangePrice;
    property _InputBarCode: string read FInputBarCode write FInputBarCode;
    //----------表没有的字段----------------
    //property _ProductCode: string read FProductCode write FProductCode;
    //property _PrcType: integer read FPrcType write FPrcType;
    //property _ProdName: string read FProdName write FProdName;
    //property _NetWeight: string read FNetWeight write FNetWeight;
    //property _ProdUnit: string read FProdUnit write FProdUnit;
    //property _StockAmount: currency read FStockAmount write FStockAmount;
    //property _avgAmount: currency read FAvgAmount write FAvgAmount;
    //property _PriceRate: string read FPriceRate write FPriceRate;
    //property _SettleAmount: currency read FSettleAmount write FSettleAmount;
    //property _ProdSettleAmt: currency read FProdSettleAmt write FProdSettleAmt;
    //property _Remark: string read FRemark write FRemark;
    //property _MemberGetMTicketInfo: boolean read FMemberGetMTicketInfo write FMemberGetMTicketInfo;
    //property _IsPkg: boolean read FIsPkg write FIsPkg;                             //新增大小包
    //property _IsBind: boolean read FIsBind write FIsBind;
    //property _IsZengPing: boolean read FIsZengPing write FIsZengPing;              //促销用的变量
    //property _PromotionAD: string read FPromotionAD write FPromotionAD;
    //property _DiscountAmt: currency read FDiscountAmt write FDiscountAmt;
    //property _SortCode: string read FSortCode write FSortCode;
    //property _IsHuanGou: boolean read FIsHuanGou write FIsHuanGou;
    //property _billNum: string read FbillNum write FbillNum;
    //property _Flag: string read FFlag write FFlag;
    //property _ShortExpProduct: boolean read FShortExpProduct write FShortExpProduct;
    //property _PiShortExpType: string read FPiShortExpType write FPiShortExpType;
    //property _PRMNO: integer read FPRMNO write FPRMNO;
    //property _CLS: string read FCLS write FCLS;
    //property _DisRatio: integer read FDisRatio write FDisRatio;
    //property _PromotionType: integer read FPromotionType write FPromotionType;
    //property _BillLine: string read FBillLine write FBillLine;
    //property _TablePrice: currency read FTablePrice write FTablePrice;
    //property _GuarDisCount: currency read FGuarDisCount write FGuarDisCount;
    //property _BGuar: boolean read FBGuar write FBGuar;
    //property _IsDisp: boolean read FIsDisp write FIsDisp;
  end;



  TStoreSaleSubs = TObjectList<TStoreSaleSub>;


  { TSalePayInfo }
  TStoreSalePay = class(TPersistent)
  private
    FUUID: UnicodeString;     //ID
    FITEMNO: integer;         //序号
    FCURRENCY: integer;       //付款方式
    FAMOUNT: currency;        //付款金额
    FCARDCODE: string;        //付款消费卡
    FFAVTYPE: string;         //优惠标志
    FFAVAMT: currency;        //优惠金额
    FCASHIER: string;
    FCREATETIME: string;
  published
    property _UUID: UnicodeString read FUUID write FUUID;
    property _ITEMNO: integer read FITEMNO write FITEMNO;                //序号
    property _CURRENCY: integer read FCURRENCY write FCURRENCY;          //付款方式
    property _AMOUNT: currency read FAMOUNT write FAMOUNT;               //付款金额
    property _CARDCODE: string read FCARDCODE write FCARDCODE;           //付款消费卡
    property _FAVTYPE: string read FFAVTYPE write FFAVTYPE;              //优惠标志
    property _FAVAMT: currency read FFAVAMT write FFAVAMT;               //优惠金额
    property _CASHIER: string read FCASHIER write FCASHIER;
    property _CREATETIME: string read FCREATETIME write FCREATETIME;
  end;


  TStoreSalePays = TObjectList<TStoreSalePay>;


  TStoreSalePromotion = class(TPersistent)
  private
    FUUID: string;           //ID
    FFLOWNO: string;         //促销号或着劵号
    FITEMNO: integer;        //序号
    FFAVTYPE: integer;
    //促销类型代码   促销类型【外卖促销：0】 【单品促销：1】【单品组合：2】【组合促销：3】【总额促销：4】【客单价：5】【卡券:6】【临近商品:7】【赠品:8】【捆绑商品(用于分解):9】【会员:10】【微信:11】【支付宝:12】 【捆绑促销:13】
    FFAVAMT: currency;       //优惠金额
    FPROMNUM: string;        // billnum
  published
    property _UUID: string read FUUID write FUUID;
    property _FLOWNO: string read FFLOWNO write FFLOWNO;
    property _ITEMNO: integer read FITEMNO write FITEMNO;
    property _FAVTYPE: integer read FFAVTYPE write FFAVTYPE;
    property _FAVAMT: currency read FFAVAMT write FFAVAMT;
    property _PROMNUM: string read FPROMNUM write FPROMNUM;
  end;

  TStoreSalePromotions = TObjectList<TStoreSalePromotion>;




  //销售单库存
  TStoreSaleInv = class(TPersistent)
  private
    FUUID: string;
    FITEMNO: integer;
    FGID: integer;
    FEGID: integer;
    FQTY: currency;
    FPRICE: currency;
  published
    property _UUID: string read FUUID write FUUID;
    property _ITEMNO: integer read FITEMNO write FITEMNO;
    property _GID: integer read FGID write FGID;
    property _EGID: integer read FEGID write FEGID;
    property _QTY: currency read FQTY write FQTY;
    property _PRICE: currency read FPRICE write FPRICE;
  end;


  TStoreSaleInvs = TObjectList<TStoreSaleInv>;

  { TMemberOrder }
  TStoreMemberOrder = class(TPersistent)
  private
    FUUID: UnicodeString;     //UUID
    FMemberId: integer;         //会员ID
    FSrcOrderNo: string;       //冲帐订单流水号
    FPayType: string;        //支付方式中文名称
    FDiscountId: integer; //优惠方式编码
  published
    property _UUID: UnicodeString read FUUID write FUUID;
    property _MemberId: integer read FMemberId write FMemberId;
    property _SrcOrderNo: string read FSrcOrderNo write FSrcOrderNo;
    property _PayType: string read FPayType write FPayType;
    property _DiscountId: integer read FDiscountId write FDiscountId;
  end;

  TStoreMemberOrders = TObjectList<TStoreMemberOrder>;



  { TShopProduct }

  TShopProduct = class(TPersistent)
  private
    F_BarCode: string;
    F_GID: integer;
    F_IsBigPack: boolean;
    F_IsDisp: integer;
    F_MemberPrice: extended;
    F_PrcType: integer;
    F_ProductCode: string;
    F_ProductID: integer;
    F_ProductName: string;
    F_Py: string;
    F_RelativeQty: integer;
    F_Remark: string;
    F_RetailPrice: extended;
    F_ShortCode: string;
    F_ShortName: string;
    F_SortCode: string;
    F_SortName: string;
    F_SPEC: string;
    F_SPProductID: string;
    F_Status: string;
    F_Unit: string;
    F_UpdateState: integer;
  published
    property _GID: integer read F_GID write F_GID;
    property _ProductCode: string read F_ProductCode write F_ProductCode;
    property _ShortCode: string read F_ShortCode write F_ShortCode;
    property _ProductName: string read F_ProductName write F_ProductName;
    property _ShortName: string read F_ShortName write F_ShortName;
    property _SortCode: string read F_SortCode write F_SortCode;
    property _SortName: string read F_SortName write F_SortName;
    property _BarCode: string read F_BarCode write F_BarCode;
    property _SPEC: string read F_SPEC write F_SPEC;
    property _Unit: string read F_Unit write F_Unit;
    property _RetailPrice: extended read F_RetailPrice write F_RetailPrice;
    property _Remark: string read F_Remark write F_Remark;
    property _Status: string read F_Status write F_Status;
    property _PrcType: integer read F_PrcType write F_PrcType;
    property _ProductID: integer read F_ProductID write F_ProductID;
    property _MemberPrice: extended read F_MemberPrice write F_MemberPrice;
    property _IsBigPack: boolean read F_IsBigPack write F_IsBigPack;
    property _IsDisp: integer read F_IsDisp write F_IsDisp;
    property _SPProductID: string read F_SPProductID write F_SPProductID;
    property _RelativeQty: integer read F_RelativeQty write F_RelativeQty;
    property _Py: string read F_Py write F_Py;
    property _UpdateState: integer read F_UpdateState write F_UpdateState;
  end;

  TShopProducts = TObjectList<TShopProduct>;



implementation



{ TStoreSaleSub }


{ TShopProduct }
end.
