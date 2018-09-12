unit uFrameSale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Contnrs, Graphics,
  uSale, uMPOS, Types;

type

  { TSaleControl }

  TSaleControl = class(TPanel)
  private
    FVO: TSaleItem;
    FNOLab: TLabel;
    FCommodityLab: TLabel;
    FCommodityCodeLab: TLabel;
    FPriceLab: TLabel;
    FQuantity: TLabel;
    FCost: TLabel;
    FRemark: TLabel;
    FSelected: Boolean;
    procedure ReSizeEvent(Sender: TObject);
    procedure SetSelected(AValue: Boolean);
    procedure SetVO(AValue: TSaleItem);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetNO(ANO: Integer);
    property VO: TSaleItem read FVO write SetVO;
    property Selected: Boolean read FSelected write SetSelected;
  end;

  { TSaleFrame }

  TSaleFrame = class(TFrame, ISaleBoard)
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Lab_sumCost: TLabel;
    Lab_count: TLabel;
    LabelCost: TLabel;
    LabelCommodity: TLabel;
    LabelPrice: TLabel;
    LabelQuantity: TLabel;
    PanelAdjustment: TPanel;
    PanelBottom: TPanel;
    PanelTitle: TPanel;
    Panel3: TPanel;
    PanelItems: TPanel;
    PanelSale: TPanel;
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FrameResize(Sender: TObject);
    procedure PanelItemsMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FControlList: TFPObjectList;
    FSaleList: TSaleList;
    FCurrIndex: Integer;
    FCurrSelect: TSaleControl;
    procedure ControlClick(Sender: TObject);
    procedure ReSetControlVO(AStartIndex: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CurrIndex: Integer read FCurrIndex;
    procedure Up;
    procedure Down;
    procedure SelectVO(AVO: TSaleItem);
  public //ISaleBoard
    function Add(AVO: TSaleItem): Boolean; overload;
    function Add(AVOs: TSaleList): Boolean; overload;
    function Delete(AVO: TSaleItem): Boolean;
    function DeleteCurrent: Boolean;
    function GetVOList: TSaleList;
    function GetCurrentVO: TSaleItem;
    function Clear: Boolean;
  end;

implementation

{$R *.frm}

{ TSaleControl }

constructor TSaleControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.BevelOuter := bvNone;
  Self.Height := 40;
  Self.OnResize := @ReSizeEvent;
  FNOLab := TLabel.Create(Self);
  FNOLab.Parent := Self;
  FCommodityLab := TLabel.Create(Self);
  FCommodityLab.Parent := Self;
  FCommodityCodeLab := TLabel.Create(Self);
  FCommodityCodeLab.Parent := Self;
  FPriceLab := TLabel.Create(Self);
  FPriceLab.Parent := Self;
  FQuantity := TLabel.Create(Self);
  FQuantity.Parent := Self;
  FCost := TLabel.Create(Self);
  FCost.Parent := Self;
  FRemark := TLabel.Create(Self);
  FRemark.Parent := Self;
  //
  FNOLab.Left := 10;
  FNOLab.Top := 11;
  FNOLab.Width := 30;
  FNOLab.AutoSize := False;
  FCommodityLab.Left := 46;
  FCommodityLab.Top := 4;
  FCommodityCodeLab.Left := 48;
  FCommodityCodeLab.Top := 22;
  FPriceLab.Top := 11;
  FQuantity.Top := 11;
  FCost.Top := 11;
  //
  FNOLab.Alignment := taRightJustify;
  FCommodityCodeLab.Font.Color := clGrayText;
  FPriceLab.Font.Size := 12;
  FQuantity.Font.Size := 12;
  FCost.Font.Color := $025EE8;
  FCost.Font.Size := 12;
  //
  FSelected := False;
end;

procedure TSaleControl.SetNO(ANO: Integer);
begin
  FNOLab.Caption := IntToStr(ANO);
end;

procedure TSaleControl.ReSizeEvent(Sender: TObject);
begin
  FPriceLab.Left := Self.Width div 6 * 3;
  FQuantity.Left := Self.Width div 6 * 4;
  FCost.Left := Self.Width div 6 * 5;
end;

procedure TSaleControl.SetSelected(AValue: Boolean);
begin
  if FSelected = AValue then
    Exit;
  FSelected := AValue;
  Invalidate;
end;

procedure TSaleControl.SetVO(AValue: TSaleItem);
begin
  FVO := AValue;
  FCommodityLab.Caption := AValue.CommodityName;
  FCommodityCodeLab.Caption := AValue.CommodityCode;
  FPriceLab.Caption := FormatCurr('￥0.00', AValue.Price);
  FQuantity.Caption := CurrToStr(AValue.Quantity);
  FCost.Caption := FormatCurr('￥0.00', AValue.Cost);
end;

procedure TSaleControl.Paint;
begin
  inherited Paint;
  if Selected then
    begin
      Canvas.Pen.Color := clBlue;
      Canvas.Pen.Width := 2;
      Canvas.RoundRect(8, 1, Width-8, Height-1, 10, 10);
    end
  else
    begin
      Canvas.Pen.Color := clGrayText;
      Canvas.Pen.Width := 1;
      Canvas.Line(10, Height-1, Width-10, Height-1);
    end;
end;

{ TSaleFrame }

procedure TSaleFrame.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  l: ISaleInputListener;
begin
  case Key of
  13: begin
      if InterfaceRegister.OutInterface(ISaleInputListener, l) then
        l.InputCode(Edit1.Text);
      Edit1.Clear;
    end;
  34: begin
      if InterfaceRegister.OutInterface(ISaleInputListener, l) then
        l.SettleAccount;
    end;
  38: Self.Up;
  40: Self.Down;
  46: Self.DeleteCurrent;
  end;
end;

constructor TSaleFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlList := TFPObjectList.Create(True);
  FSaleList := TSaleList.Create(False);
  FCurrIndex := -1;
  FCurrSelect := nil;
end;

destructor TSaleFrame.Destroy;
begin
  FControlList.Free;
  FSaleList.Free;
  inherited Destroy;
end;

procedure TSaleFrame.FrameResize(Sender: TObject);
var
  i: Integer;
  c: TSaleControl;
begin
  LabelPrice.Left := PanelTitle.Width div 6 * 3;
  LabelQuantity.Left := PanelTitle.Width div 6 * 4;
  LabelCost.Left := PanelTitle.Width div 6 * 5;
  Lab_sumCost.Top := Lab_count.Top + Lab_count.Height - Lab_sumCost.Height;
  PanelAdjustment.Height := (PanelItems.Height - (PanelItems.Height div 40 * 40)) div 2 - 8;
  //for i:=0 to FControlList.Count-1 do
  //  begin
  //    c := TSaleControl(FControlList[i]);
  //    c.Width := PanelItems.Width;
  //  end;
  if not Self.Showing then
    Exit;
  FControlList.Clear;
  while FControlList.Count * 40 + 40 < PanelItems.Height do
    begin
      if FControlList.Count >= FSaleList.Count then
        Break;
      c := TSaleControl.Create(Self);
      c.Parent := PanelItems;
      c.Top := FControlList.Count * 40;
      c.Width := PanelItems.Width;
      c.OnClick := @ControlClick;
      FControlList.Add(c);
    end;
  if FCurrIndex >= 0 then
    SelectVO(FSaleList[FCurrIndex]);
end;

procedure TSaleFrame.PanelItemsMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    Self.Up
  else
    Self.Down;
end;

procedure TSaleFrame.ControlClick(Sender: TObject);
begin
  SelectVO(TSaleControl(Sender).VO);
end;

procedure TSaleFrame.Up;
begin
  if CurrIndex > 0 then
    SelectVO(FSaleList[CurrIndex-1]);
end;

procedure TSaleFrame.Down;
begin
  if (CurrIndex >= 0) and (CurrIndex < FSaleList.Count - 1) then
    SelectVO(FSaleList[CurrIndex+1]);
end;

procedure TSaleFrame.SelectVO(AVO: TSaleItem);
var
  index: Integer;
  c: TSaleControl;
  i: Integer;
  procedure SelectControl(ASC: TSaleControl);
  begin
    ASC.Selected := True;
    FCurrIndex := FSaleList.IndexOf(ASC.VO);
    if Assigned(FCurrSelect) and (FCurrSelect <> ASC) then
      FCurrSelect.Selected := False;
    FCurrSelect := ASC;
  end;
begin
  index := FSaleList.IndexOf(AVO);
  if (index < 0) or (index = CurrIndex) then
    Exit;
  for i:=0 to FControlList.Count-1 do
    begin
      c := TSaleControl(FControlList[i]);
      if c.VO = AVO then
        begin
          SelectControl(c);
          Exit;
        end;
    end;
  if CurrIndex > index then
    begin
      for i:=0 to FControlList.Count-1 do
        begin
          c := TSaleControl(FControlList[i]);
          c.VO := FSaleList[index + i];
          c.SetNO(index + i + 1);
          if i = 0 then
            SelectControl(c);
        end;
    end
  else
    begin
      for i:=FControlList.Count-1 downto 0 do
        begin
          c := TSaleControl(FControlList[i]);
          c.VO := FSaleList[index];
          c.SetNO(index + 1);
          index := index - 1;
          if i = FControlList.Count - 1 then
            SelectControl(c);
        end;
    end;
end;

function TSaleFrame.Add(AVO: TSaleItem): Boolean;
var
  c: TSaleControl;
begin
  Result := False;
  if FSaleList.Count >= 9999 then
    Exit;
  FSaleList.Add(AVO);
  if FControlList.Count * 40 + 40 < PanelItems.Height then
    begin
      c := TSaleControl.Create(Self);
      c.Parent := PanelItems;
      c.Top := FControlList.Count * 40;
      c.Width := PanelItems.Width;
      c.OnClick := @ControlClick;
      FControlList.Add(c);
    end;
  SelectVO(FSaleList[FSaleList.Count-1]);
  Lab_count.Caption := IntToStr(FSaleList.Count);
  Lab_sumCost.Caption := FormatCurr('￥0.00', FSaleList.SumCost);
  Result := True;
end;

function TSaleFrame.Add(AVOs: TSaleList): Boolean;
var
  i: Integer;
  c: TSaleControl;
begin
  Result := False;
  if FSaleList.Count + AVOs.Count > 9999 then
    Exit;
  for i:=0 to AVOs.Count-1 do
    FSaleList.Add(AVOs[i]);
  while FControlList.Count * 40 + 40 < PanelItems.Height do
    begin
      if FControlList.Count >= FSaleList.Count then
        Break;
      c := TSaleControl.Create(Self);
      c.Parent := PanelItems;
      c.Top := FControlList.Count * 40;
      c.Width := PanelItems.Width;
      c.OnClick := @ControlClick;
      FControlList.Add(c);
    end;
  SelectVO(FSaleList[FSaleList.Count-1]);
  Lab_count.Caption := IntToStr(FSaleList.Count);
  Lab_sumCost.Caption := FormatCurr('￥0.00', FSaleList.SumCost);
  Result := True;
end;

function TSaleFrame.Delete(AVO: TSaleItem): Boolean;
var
  index, i: Integer;
  c: TSaleControl;
begin
  Result := False;
  index := FSaleList.Remove(AVO);
  for i:=0 to FControlList.Count-1 do
    begin
      c := TSaleControl(FControlList[i]);
      if c.VO = AVO then
        begin
          //c.SetVO();
        end;
    end;
end;

procedure TSaleFrame.ReSetControlVO(AStartIndex: Integer);
var
  i, index: Integer;
begin
  index := AStartIndex;
  for i:=0 to FControlList.Count-1 do
    begin
      if index < FSaleList.Count then
        begin
          TSaleControl(FControlList[i]).VO := TSaleItem(FSaleList[index]);
          index := index + 1;
        end
      else
        TSaleControl(FControlList[i]).Visible := False;
    end;
end;

function TSaleFrame.DeleteCurrent: Boolean;
var
  sIndex: Integer;
begin
  Result := False;
  sIndex := FSaleList.IndexOf(TSaleControl(FControlList[0]).VO);
  FSaleList.Remove(FCurrSelect.VO);
  ReSetControlVO(sIndex);
end;

function TSaleFrame.GetVOList: TSaleList;
begin
  Result := FSaleList;
end;

function TSaleFrame.GetCurrentVO: TSaleItem;
begin
  Result := nil;
  Result := FCurrSelect.VO;
end;

function TSaleFrame.Clear: Boolean;
begin
  Result := False;
  FCurrIndex := -1;
  FSaleList.Clear;
  FControlList.Clear;
  Result := True;
end;


end.

