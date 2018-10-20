unit uRetailControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_InterfaceLoader,
  cm_InterfaceRegister,
  //--------------------------------------------
  uStoreSaleMainDAO,
  uStoreSaleMainDAOImpl,
  //--------------------------------------------
  uStoreSaleSubDAO,
  uStoreSaleSubDAOImpl,
  //--------------------------------------------
  uStoreSalePayDAO,
  uStoreSalePayDAOImpl,
  //--------------------------------------------
  uStoreSaleInvDAO,
  uStoreSaleInvDAOImpl,
  //--------------------------------------------
  uStoreSalePromotionDAO,
  uStoreSalePromotionDAOImpl,
  //--------------------------------------------
  uStoreMemberOrderDAO,
  uStoreMemberOrderDAOImpl,
  //--------------------------------------------
  uShopProductDAO,
  uShopProductDAOImpl;

type

  { TRetailManage }

  TRetailManage = class
  private
    FRegister: ICMInterfaceRegister;
    FInfo: ICMLibInfo;
  public
    constructor Create(ARegister: ICMInterfaceRegister; const AInfo: ICMLibInfo); override;
    destructor Destroy; override;
    procedure RegisterRetailService;
  end;

implementation

{ TRetailManage }

constructor TRetailManage.Create(ARegister: ICMInterfaceRegister; const AInfo: ICMLibInfo);
begin
  inherited Create(ARegister, AInfo);
  FRegister := ARegister;
  FInfo := AInfo;
end;

destructor TRetailManage.Destroy;
begin
  inherited Destroy;
end;

procedure TRetailManage.RegisterRetailService;
begin

end;

end.
