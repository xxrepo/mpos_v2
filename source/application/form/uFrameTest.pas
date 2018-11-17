unit uFrameTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics,
  cm_theme, cm_plat,
  uSale, uSaleDTO,
  uSystem,
  uAForm, cm_AWT, cm_AWTEventUtils;

type

  { TTestFrame }

  TTestFrame = class(TFrame)
    Button1: TButton;
    Panel1: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FrameClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel6Click(Sender: TObject);
    procedure Panel7Click(Sender: TObject);
  private

  public

  end;

  { TX }

  TX = class(TControlAdapter)
  public
    procedure ControlClick(e: IControlEvent); override;
  end;


implementation

uses
  cm_messager;

{$R *.frm}



{ TTestFrame }

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

procedure TTestFrame.FrameClick(Sender: TObject);
begin
  //Panel1.TabOrder := ;

end;

procedure TTestFrame.Button1Click(Sender: TObject);
var
  f: TACustomServiceForm;
  e: TAEdit;
begin
  f := TACustomServiceForm.Create(nil);

  e := TAEdit.Create(f);
  e.Parent := f;

  e.AddControlListener(TX.Create);
  f.Left := 10;
  f.Top := 10;
  f.ShowModal;
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

{ TX }

procedure TX.ControlClick(e: IControlEvent);
begin
  AppSystem.GetMsgBar.ShowMessage(etInfo, 'aaaaaaaaaa');
end;



end.
