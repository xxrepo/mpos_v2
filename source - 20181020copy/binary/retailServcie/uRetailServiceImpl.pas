unit uRetailServiceImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, cm_messager, uSaleOrder, uFormService, uRetailServiceForm, cm_interfaces, uRetailService;

type

  { TRetailService }

  TRetailService = class(TCMMessageableComponent, IRetailService)
  private
  public
  public
    //----- IRetailService Impl ----------------------------------
    function Settlement(ASaleOrder: TSaleOrder): boolean;
    function ChangePrice(ASaleOrder: TSaleOrder; ASubRowIndex: integer): boolean;
    function ChangeQuantity(ASaleOrder: TSaleOrder; ASubRowIndex: integer): boolean;
    function RemoveRow(ASaleOrder: TSaleOrder; ASubRowIndex: integer): boolean;
    function ClearRows(ASaleOrder: TSaleOrder): boolean;
    function AddTicketCode(ASaleOrder: TSaleOrder): boolean;
    function AddMemberCode(ASaleOrder: TSaleOrder): boolean;
    function Backup(ASaleOrder: TSaleOrder): boolean;
    function Recovery(ASaleOrder: TSaleOrder): boolean;
  end;

implementation

{ TRetailService }


function TRetailService.Settlement(ASaleOrder: TSaleOrder): boolean;
begin
  Messager.Info('开始结算');

end;

function TRetailService.ChangePrice(ASaleOrder: TSaleOrder; ASubRowIndex: integer): boolean;
begin
  Messager.Info('修改价格 %s', [ASubRowIndex]);
end;

function TRetailService.ChangeQuantity(ASaleOrder: TSaleOrder; ASubRowIndex: integer): boolean;
begin
  Messager.Info('修改数量 %s', [ASubRowIndex]);
end;

function TRetailService.RemoveRow(ASaleOrder: TSaleOrder; ASubRowIndex: integer): boolean;
begin
  Messager.Info('删行 %s', [ASubRowIndex]);
end;

function TRetailService.ClearRows(ASaleOrder: TSaleOrder): boolean;
begin
  Messager.Info('清空 ');
end;

function TRetailService.AddTicketCode(ASaleOrder: TSaleOrder): boolean;
begin
  Messager.Info('加入优惠券 ');
end;

function TRetailService.AddMemberCode(ASaleOrder: TSaleOrder): boolean;
begin
  Messager.Info('清空优惠券 ');
end;

function TRetailService.Backup(ASaleOrder: TSaleOrder): boolean;
begin
  Messager.Info('挂单 ');
end;

function TRetailService.Recovery(ASaleOrder: TSaleOrder): boolean;
begin
  Messager.Info('取回 ');
end;

end.





