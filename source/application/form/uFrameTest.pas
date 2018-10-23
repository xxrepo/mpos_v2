unit uFrameTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls,
  cm_theme, cm_Plat,
  uSale, uSaleDTO,
  uSystem;

type

  { TTestFrame }

  TTestFrame = class(TFrame)
    Panel1: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    procedure Panel1Click(Sender: TObject);
    procedure Panel6Click(Sender: TObject);
    procedure Panel7Click(Sender: TObject);
  private

  public

  end;

implementation

{$R *.frm}

{ TTestFrame }

procedure TTestFrame.Panel1Click(Sender: TObject);
var
  ic: IThemeController;
  currName: string;
  tnl: TStrings;
  index: Integer;
begin
  if InterfaceRegister.OutInterface(IThemeController, ic) then
    begin
      currName := ic.GetCurrTheme.GetName;
      tnl := ic.GetThemeNames;
      index := tnl.IndexOf(currName);
      if (index >= 0) and (index < tnl.Count-1) then
        ic.SwitchTheme(tnl[index + 1])
      else if tnl.Count > 0 then
        ic.SwitchTheme(tnl[0]);
    end;
end;

procedure TTestFrame.Panel6Click(Sender: TObject);
var
  vo: TShowItem;
  vos: TShowItemList;
  i: Integer;
  sb: ISaleBoard;
begin
  vos := TShowItemList.Create(False);
  for i:=0 to 800 do
    begin
      vo := TShowItem.Create;
      vo.Name := '椰树椰子汁 1000ml/罐';
      vo.BarCode := '632014412014';
      vo.Price := 6.98;
      vo.Quantity := i;
      vos.Add(vo.UUID, vo);
    end;
  if InterfaceRegister.OutInterface(ISaleBoard, sb) then
    sb.SetShowItemList(vos);
  vos.Free;
end;

procedure TTestFrame.Panel7Click(Sender: TObject);
var
  vo: TShowItem;
  sb: ISaleBoard;
begin
  vo := TShowItem.Create;
  vo.Name := '椰树椰子汁 1000ml/罐';
  vo.BarCode := '632014412014';
  vo.Price := 6.98;
  vo.Quantity := Random(88);
  if InterfaceRegister.OutInterface(ISaleBoard, sb) then
    sb.AddShowItem(vo);
end;

end.

