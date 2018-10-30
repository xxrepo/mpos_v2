unit uFrameTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls,
  cm_theme, cm_Plat,
  uSale, uSaleDTO,
  uSystem;

type

  { TTestFrame }

  TTestFrame = class(TFrame)
    edtShopCode: TEdit;
    edtTermCode: TEdit;
    edtTermUUID: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure Panel5Click(Sender: TObject);
    procedure Panel6Click(Sender: TObject);
    procedure Panel7Click(Sender: TObject);
  private

  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  uDBInitialize, cm_messager, uPOS;

{$R *.frm}

{ TTestFrame }
constructor TTestFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TTestFrame.Panel1Click(Sender: TObject);
var
  ic: IThemeController;
  currName: string;
  tnl: TStrings;
  index: integer;
begin
  if InterfaceRegister.OutInterface(IThemeController, ic) then
  begin
    currName := ic.GetCurrTheme.GetName;
    tnl := ic.GetThemeNames;
    index := tnl.IndexOf(currName);
    if (index >= 0) and (index < tnl.Count - 1) then
      ic.SwitchTheme(tnl[index + 1])
    else if tnl.Count > 0 then
      ic.SwitchTheme(tnl[0]);
  end;
end;


procedure TTestFrame.Panel2Click(Sender: TObject);
var
  DBInit: IDBInitialize;
begin
  if InterfaceRegister.OutInterface(IDBInitialize, DBInit) then
  begin
    DBInit.DBInitialize;
  end;
end;

procedure TTestFrame.Panel5Click(Sender: TObject);
begin
  edtShopCode.Enabled := not edtShopCode.Enabled;
  edtTermCode.Enabled := not edtTermCode.Enabled;
  edtTermUUID.Enabled := not edtTermUUID.Enabled;
end;

procedure TTestFrame.Panel6Click(Sender: TObject);
var
  vo: TShowItem;
  vos: TShowItemList;
  i: integer;
  sb: ISaleBoard;
begin
  vos := TShowItemList.Create(False);
  for i := 0 to 800 do
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
