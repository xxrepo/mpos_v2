unit uFrameTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics, Grids, DateTimePicker,
  cm_theme, cm_plat,
  uSale, uSaleDTO,
  uSystem,
  uAForm, cm_AWT, cm_AWTEventUtils;

type

  { TTestFrame }

  TTestFrame = class(TFrame)
    Button1: TButton;
    DateTimePicker1: TDateTimePicker;
    Label1: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure FrameClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel6Click(Sender: TObject);
    procedure Panel7Click(Sender: TObject);
  private

  public

  end;

  { TX }

  TX = class(TWinControlAdapter)
  public
    procedure ControlClick(e: IControlEvent); override;
    procedure ControlEnter(e: IWinControlEvent); override;
  end;

  { TMouseL }

  TMouseL = class(TMouseAdapter)
  public
    procedure MouseMoved(e: IMouseEvent); override;
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
  //Panel1.OnMouseDown := ;
  //Label1.OnMouseDown := ;
  //Memo1.
  //Memo1.ReadOnly := ;
  //Memo1.NumbersOnly := ;
end;

procedure TTestFrame.Button1Click(Sender: TObject);
var
  f: TACustomServiceForm;
  e, e2: TAEdit;
  wl: IWinControlListener;
  p: TADateTimePicker;
begin
  f := TACustomServiceForm.Create(nil);

  e := TAEdit.Create(f);
  e.Parent := f;
  e.GetPeer.GetName;

  e2 := TAEdit.Create(f);
  e2.Parent := f;
  e2.Left := 200;

  wl := TX.Create;
  e.AddWinControlListener(wl);
  e2.AddWinControlListener(wl);

  p := TADateTimePicker.Create(f);
  p.Parent := f;
  p.AddMouseListener(TMouseL.Create);


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
  //AppSystem.GetMsgBar.ShowMessage(etInfo, 'aaaaaaaaaa');
end;

procedure TX.ControlEnter(e: IWinControlEvent);
begin
  AppSystem.GetMsgBar.ShowMessage(etInfo, 'bbbbbb' + IntToStr(e.GetAControl.GetHashCode));
end;

{ TMouseL }

procedure TMouseL.MouseMoved(e: IMouseEvent);
begin
  AppSystem.GetMsgBar.ShowMessage(etInfo, Format('%d - %d', [e.GetX, e.GetY]));
end;



end.
