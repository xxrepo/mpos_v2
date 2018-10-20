unit uServiceFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uSaleMasterService,
  uSaleMasterServiceImpl,
  uSalePayService,
  uSalePayServiceImpl,
  uSaleSubService,
  uSaleSubServiceImpl,
  uSaleSubInvService,
  uSaleSubInvServiceImpl,
  uSalePromotionService,
  uSalePromotionServiceImpl,
  uMemberOrderService,
  uMemberOrderServiceImpl,
  uDAO;

type

  { TServiceFactory }

  TServiceFactory = class(TComponent)
    FSaleMasterService: TSaleMasterService;
    FSaleSubService: TSaleSubService;
    FSalePromotionService: TSalePromotionService;
    FSalePayService: TSalePayService;
    FSaleSubInvService: TSaleSubInvService;
    FMemberOrderService: TMemberOrderService;
  public
    function GetSaleMasterService(): ISaleMasterService;
    function GetSalePayService(): ISalePayService;
    function GetSaleSubService(): ISaleSubService;
    function GetSaleSubInvService(): ISaleSubInvService;
    function GetSalePromotionService(): ISalePromotionService;
    function GetMemberOrderService(): IMemberOrderService;

  end;

var
  ServiceFactory: TServiceFactory;

implementation

{ TServiceFactory }

function TServiceFactory.GetSaleMasterService(): ISaleMasterService;
begin
  if not Assigned(FSaleMasterService) then
    FSaleMasterService := TSaleMasterService.Create(self);
  Result := ISaleMasterService(FSaleMasterService);
end;

function TServiceFactory.GetSaleSubService(): ISaleSubService;
begin
  if not Assigned(FSaleSubService) then
    FSaleSubService := TSaleSubService.Create(self);
  Result := ISaleSubService(FSaleSubService);
end;

function TServiceFactory.GetSaleSubInvService(): ISaleSubInvService;
begin
  if not Assigned(FSaleSubInvService) then
    FSaleSubInvService := TSaleSubInvService.Create(self);
  Result := ISaleSubInvService(FSaleSubInvService);
end;

function TServiceFactory.GetSalePayService(): ISalePayService;
begin
  if not Assigned(FSalePayService) then
    FSalePayService := TSalePayService.Create(self);
  Result := ISalePayService(FSalePayService);
end;

function TServiceFactory.GetSalePromotionService(): ISalePromotionService;
begin
  if not Assigned(FSalePromotionService) then
    FSalePromotionService := TSalePromotionService.Create(self);
  Result := ISalePromotionService(FSalePromotionService);
end;

function TServiceFactory.GetMemberOrderService(): IMemberOrderService;
begin
  if not Assigned(FMemberOrderService) then
    FMemberOrderService := TMemberOrderService.Create(self);
  Result := IMemberOrderService(FMemberOrderService);
end;


initialization
  ServiceFactory := TServiceFactory.Create(nil);

finalization
  ServiceFactory.Free;

end.
