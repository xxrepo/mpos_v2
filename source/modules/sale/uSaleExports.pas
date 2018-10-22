unit uSaleExports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_messager, cm_InterfaceRegister,
  uDAO,
  uSale, uSaleDAO, uSaleDAOImpl,
  uSaleDeal, uSaleServicePoxy, uSalePersistant;

type

  { TExports }

  TExports = class(TCMMessageable)
  public
    procedure LoadExport(ARegister: ICMInterfaceRegister);
  end;

implementation

{ TExports }

procedure TExports.LoadExport(ARegister: ICMInterfaceRegister);
var
  sd: TSaleDeal;
  dao: ISaleDAO;
  sb: ISaleBoard;
  sh: ISaleHandler;
  tp: TTestPromotion;
begin
  Messager.Debug('开始加载销售业务...');
  //BillCenter
  sd := TSaleDeal.Create;
  ARegister.PutInterface('ISaleBillCenter', ISaleBillCenter, sd);
  //DAO
  if TPOSDAOFactory.GetInstance.OutDAO(TSaleDAO, ISaleDAO, dao) then
    ARegister.PutInterface('ISaleDAO', ISaleDAO, dao)
  else
    Messager.Error('销售DAO实例化失败！');
  //
  Messager.Debug('开始向销售面板添加侦听器...');
  if ARegister.OutInterface(ISaleBoard, sb) then
    sb.AddListener(ISaleBoardListener(sd));
  //
  sh := TTestSaleService.Create;
  Messager.Debug('开始注册销售业务处理器...');
  ARegister.PutInterface('ISaleHandler', ISaleHandler, sh);
  //
  tp := TTestPromotion.Create;
  Messager.Debug('开始向销售业务处理器添加促销处理器...');
  sh.AddHandler(tp);
  Messager.Debug('开始向销售业务处理器添加持久化处理器...');
  sh.AddHandler(ISaleHandler(TTestSalePersistant.Create));

  //---------------------------
  Messager.Debug('开始向销售业务处理器添加结算处理器...');
  sh.AddHandler(ISaleHandler(TTestSalePay.Create));

  Messager.Debug('开始向ISaleBillCenter添加销售单据侦听器...');
  sd.AddListener(tp);
end;

end.

