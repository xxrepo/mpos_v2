unit uFrameSale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Contnrs, Graphics, Types, Dialogs,
  cm_interfaces,
  uFrame, cm_theme,
  uSale, uSaleDTO;

type

  { TSaleControl }

  TSaleControl = class(TPanel)
  private
    FVO: TShowItem;
    FNOLab: TLabel;
    FCommodityLab: TLabel;
    FCommodityCodeLab: TLabel;
    FPriceLab: TLabel;
    FQuantity: TLabel;
    FCost: TLabel;
    FRemark: TLabel;
    FSelected: Boolean;
    procedure SetSelected(AValue: Boolean);
    procedure SetVO(AValue: TShowItem);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetNO(ANO: Integer);
    property VO: TShowItem read FVO write SetVO;
    property Selected: Boolean read FSelected write SetSelected;
  end;

  { TSaleFrame }

  TSaleFrame = class(TPOSFrame, ISaleBoard)
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LabelScan: TLabel;
    LabelNO: TLabel;
    Lab_sumCost: TLabel;
    Lab_count: TLabel;
    LabelCost: TLabel;
    LabelCommodity: TLabel;
    LabelPrice: TLabel;
    LabelQuantity: TLabel;
    PanelSum: TPanel;
    PanelMsg: TPanel;
    PanelAdjustment: TPanel;
    PanelBottom: TPanel;
    PanelTitle: TPanel;
    PanelItems: TPanel;
    PanelSale: TPanel;
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FrameResize(Sender: TObject);
    procedure PanelItemsMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FControlList: TFPObjectList;
    FVOList: TShowItemList;
    FCurrSelectVO: TShowItem;
    FBoardListener: TInterfaceList;
    procedure ControlClick(Sender: TObject);
    function GetCurrIndex: Integer;
    procedure SetSum;
    procedure SelectControl(AVO: TShowItem);
    function DeleteControl(AVO: TShowItem): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CurrIndex: Integer read GetCurrIndex;
    procedure Up;
    procedure Down;
  public //ISaleBoard
    function AddShowItem(AVO: TShowItem): Boolean;
    function DeleteShowItem(const AUUID: string): Boolean;
    function UpdateShowItem(AVO: TShowItem): Boolean;
    function SetShowItemList(AVOs: TShowItemList): Boolean;
    function Clear: Boolean;
    procedure AddListener(AListener: ICMListener);
    procedure PromptMessage(et: TEventType; const msg: string);
  public
    procedure SetTheme(ATheme: ITheme); override;
  end;

const
  _LabPriceLeft: Integer=100;
  _LabQuantityLeft: Integer=200;
  _LabCostLeft: Integer=300;

implementation

{$R *.frm}

{ TSaleControl }

constructor TSaleControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.BevelOuter := bvNone;
  Self.Height := 40;
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
  FNOLab.Width := 34;
  FNOLab.AutoSize := False;
  FCommodityLab.Left := 60;
  FCommodityLab.Top := 4;
  FCommodityCodeLab.Left := 62;
  FCommodityCodeLab.Top := 22;
  FPriceLab.Top := 11;
  FPriceLab.Left := _LabPriceLeft;
  FPriceLab.Width := 80;
  FPriceLab.AutoSize := False;
  FQuantity.Top := 11;
  FQuantity.Left := _LabQuantityLeft;
  FQuantity.Width := 60;
  FQuantity.AutoSize := False;
  FCost.Top := 11;
  FCost.Left := _LabCostLeft;
  FCost.Width := 100;
  FCost.AutoSize := False;
  //
  FNOLab.Alignment := taRightJustify;
  FCommodityLab.Font.Name := '黑体';
  FCommodityCodeLab.Font.Color := clGrayText;
  FPriceLab.Alignment := taRightJustify;
  FPriceLab.Font.Size := 12;
  FPriceLab.Font.Name := '黑体';
  FQuantity.Alignment := taRightJustify;
  FQuantity.Font.Size := 12;
  FQuantity.Font.Name := '黑体';
  FCost.Alignment := taRightJustify;
  FCost.Font.Color := $025EE8;
  FCost.Font.Size := 12;
  FCost.Font.Name := '黑体';
  //
  FSelected := False;
end;

procedure TSaleControl.SetNO(ANO: Integer);
begin
  FNOLab.Caption := IntToStr(ANO);
end;

procedure TSaleControl.SetSelected(AValue: Boolean);
begin
  if FSelected = AValue then
    Exit;
  FSelected := AValue;
  Invalidate;
end;

procedure TSaleControl.SetVO(AValue: TShowItem);
begin
  FVO := AValue;
  FCommodityLab.Caption := AValue.Name;
  FCommodityCodeLab.Caption := AValue.BarCode;
  FPriceLab.Caption := FormatCurr('￥0.00', AValue.Price);
  FQuantity.Caption := CurrToStr(AValue.Quantity);
  FCost.Caption := FormatCurr('￥0.00', AValue.GetAmount);
end;

procedure TSaleControl.Paint;
begin
  inherited Paint;
  //scheme 1
  {if Selected then
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
    end;}
  //scheme 2
  if Selected then
    begin
      Canvas.Brush.Color := $eeeeff;
      Canvas.FillRect(10, 0, Width-10, Height-1);
    end
  else
    begin
      Canvas.Brush.Color := $ffffff;
      Canvas.FillRect(0, 10, Width-10, Height-1);
    end;
  Canvas.Pen.Color := clGrayText;
  Canvas.Pen.Width := 1;
  Canvas.Line(10, Height-1, Width-10, Height-1);
end;

{ TSaleFrame }

constructor TSaleFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlList := TFPObjectList.Create(True);
  FVOList := TShowItemList.Create(True);
  FCurrSelectVO := nil;
  FBoardListener := TInterfaceList.Create;
end;

destructor TSaleFrame.Destroy;
begin
  FControlList.Free;
  FVOList.Free;
  FBoardListener := nil;
  inherited Destroy;
end;

procedure TSaleFrame.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  l: ISaleBoardListener;
  i: Integer;
  del, canDelete: Boolean;
  uuid: string;
  uv: TShowItem;
begin
  if PanelMsg.Visible then
    PanelMsg.Visible := False;
  case Key of
  13: begin
      for i:=0 to FBoardListener.Count-1 do
        begin
          l := ISaleBoardListener(FBoardListener[i]);
          l.Inputted(Edit1.Text);
        end;
      Edit1.Clear;
    end;
  34: begin
      for i:=0 to FBoardListener.Count-1 do
        begin
          l := ISaleBoardListener(FBoardListener[i]);
          l.Settle;
        end;
    end;
  38: Self.Up;
  40: Self.Down;
  46: begin //delete
        if Assigned(FCurrSelectVO) then
          begin
            uuid := FCurrSelectVO.UUID;
            del := True;
            for i:=0 to FBoardListener.Count-1 do
              begin
                canDelete := True;
                ISaleBoardListener(FBoardListener[i]).Deleting(FCurrSelectVO.UUID, canDelete);
                del := del and canDelete;
              end;
            //实际已经在单据处理时已删除
            if del then
              Self.DeleteShowItem(uuid);
          end;
      end;
  112:begin //test update
        if Assigned(FCurrSelectVO) then
          begin
            uv := TShowItem.Create(FCurrSelectVO.UUID);
            uv.Assign(FCurrSelectVO);
            uv.Quantity := 101;
            del := True;
            for i:=0 to FBoardListener.Count-1 do
              begin
                canDelete := True;
                ISaleBoardListener(FBoardListener[i]).Updating(uv, canDelete);
                del := del and canDelete;
              end;
            //实际已经在单据处理时已作更改
            if del then
              Self.UpdateShowItem(uv);
            uv.Free;
          end;
       end;
  113:begin //test update
        if Assigned(FCurrSelectVO) then
          begin
            uv := TShowItem.Create(FCurrSelectVO.UUID);
            uv.Assign(FCurrSelectVO);
            uv.Price := 88.00;
            uv.Quantity := 10;
            del := True;
            for i:=0 to FBoardListener.Count-1 do
              begin
                canDelete := True;
                ISaleBoardListener(FBoardListener[i]).Updating(uv, canDelete);
                del := del and canDelete;
              end;
            //实际已经在单据处理时已作更改
            if del then
              Self.UpdateShowItem(uv);
            uv.Free;
          end;
       end;
  end;
end;

procedure TSaleFrame.FrameResize(Sender: TObject);
var
  i: Integer;
  c: TSaleControl;
begin
  Lab_sumCost.Top := Lab_count.Top + Lab_count.Height - Lab_sumCost.Height;
  PanelAdjustment.Height := (PanelItems.Height - (PanelItems.Height div 40 * 40)) div 2 - 8;
  _LabPriceLeft := LabelPrice.Left;
  _LabQuantityLeft := LabelQuantity.Left;
  _LabCostLeft := LabelCost.Left;
  for i:=0 to FControlList.Count-1 do
    begin
      c := TSaleControl(FControlList[i]);
      c.Width := PanelItems.Width;
      c.FPriceLab.Left := _LabPriceLeft;
      c.FQuantity.Left := _LabQuantityLeft;
      c.FCost.Left := _LabCostLeft;
    end;
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
  SelectControl(TSaleControl(Sender).VO);
end;

function TSaleFrame.GetCurrIndex: Integer;
begin
  Result := FVOList.IndexOf(FCurrSelectVO);
end;

procedure TSaleFrame.Up;
begin
  if CurrIndex > 0 then
    SelectControl(FVOList[CurrIndex-1]);
end;

procedure TSaleFrame.Down;
begin
  if (CurrIndex >= 0) and (CurrIndex < FVOList.Count - 1) then
    SelectControl(FVOList[CurrIndex+1]);
end;

procedure TSaleFrame.SetSum;
begin
  Lab_count.Caption := IntToStr(FVOList.Count);
  Lab_sumCost.Caption := FormatCurr('￥0.00', FVOList.SumAmount);
end;

procedure TSaleFrame.SelectControl(AVO: TShowItem);
var
  oldIndex, curIndex: Integer;
  c: TSaleControl;
  i: Integer;
  b: Boolean;
begin
  oldIndex := CurrIndex;
  FCurrSelectVO := AVO;
  curIndex := CurrIndex;
  if (curIndex < 0) or (oldIndex = curIndex) then
    Exit;
  //
  while FControlList.Count * 40 + 40 < PanelItems.Height do
    begin
      if FControlList.Count >= FVOList.Count then
        Break;
      c := TSaleControl.Create(Self);
      c.Parent := PanelItems;
      c.Top := FControlList.Count * 40;
      c.Width := PanelItems.Width;
      c.OnClick := @ControlClick;
      FControlList.Add(c);
    end;
  //
  b := False;
  for i:=0 to FControlList.Count-1 do
    begin
      c := TSaleControl(FControlList[i]);
      if Assigned(c.VO) and (c.VO.UUID = AVO.UUID) then
        begin
          c.Selected := True;
          b := True;
        end
      else
        c.Selected := False;
    end;
  if b then
    Exit;
  if oldIndex > curIndex then
    begin
      for i:=0 to FControlList.Count-1 do
        begin
          c := TSaleControl(FControlList[i]);
          c.VO := FVOList[curIndex + i];
          c.SetNO(curIndex + i + 1);
          if i = 0 then
            c.Selected := True;
        end;
    end
  else
    begin
      for i:=FControlList.Count-1 downto 0 do
        begin
          c := TSaleControl(FControlList[i]);
          c.VO := FVOList[curIndex];
          c.SetNO(curIndex + 1);
          curIndex := curIndex - 1;
          if i = FControlList.Count - 1 then
            c.Selected := True;
        end;
    end;
end;

function TSaleFrame.DeleteControl(AVO: TShowItem): Boolean;
var
  isDeleteCurr: Boolean;
  oldCurr, startIndex, i: Integer;
  c: TSaleControl;
begin
  Result := False;
  if not Assigned(AVO) then
    Exit;
  startIndex := 0;
  if FControlList.Count > 0 then
    startIndex := FVOList.IndexOf(TSaleControl(FControlList[0]).VO);
  //
  isDeleteCurr := Assigned(FCurrSelectVO) and (FCurrSelectVO.UUID = AVO.UUID);
  oldCurr := CurrIndex;
  FVOList.Remove(FVOList.Find(AVO.UUID));
  while FVOList.Count < FControlList.Count do
    FControlList.Remove(FControlList.Last);
  //
  if startIndex + FControlList.Count > FVOList.Count then
    startIndex := FVOList.Count - FControlList.Count;
  for i:=0 to FControlList.Count-1 do
    begin
      c := TSaleControl(FControlList[i]);
      c.VO := FVOList[startIndex + i];
      c.SetNO(startIndex + i + 1);
    end;
  //
  if isDeleteCurr then
    begin
      if (oldCurr >= 0) and (FVOList.Count > 0) then
        begin
          if oldCurr >= FVOList.Count then
            SelectControl(FVOList[FVOList.Count-1])
          else
            SelectControl(FVOList[oldCurr])
        end
      else
        SelectControl(nil);
    end
  else
    SelectControl(FCurrSelectVO);
  SetSum;
  Result := True;
end;

(**************************************************************************************************)

function TSaleFrame.AddShowItem(AVO: TShowItem): Boolean;
var
  vo: TShowItem;
begin
  Result := False;
  if FVOList.Count >= 9999 then
    Exit;
  vo := TShowItem.Create;
  vo.Assign(AVO);
  FVOList.Add(vo.UUID, vo);
  //
  SelectControl(FVOList[FVOList.Count-1]);
  SetSum;
  Result := True;
end;

function TSaleFrame.DeleteShowItem(const AUUID: string): Boolean;
var
  vo: TShowItem;
begin
  Result := False;
  vo := FVOList.Find(AUUID);
  if Assigned(vo) then
    begin
      Result := Self.DeleteControl(vo);
    end;
end;

function TSaleFrame.UpdateShowItem(AVO: TShowItem): Boolean;
var
  i: Integer;
  c: TSaleControl;
  cv: TShowItem;
begin
  Result := False;
  for i:=0 to FControlList.Count-1 do
    begin
      c := TSaleControl(FControlList[i]);
      if c.VO.UUID = AVO.UUID then
        begin
          cv := TShowItem.Create(AVO.UUID);
          cv.Assign(AVO);
          c.VO.Free;
          c.VO := cv;
          SetSum;
          Result := True;
          Exit;
        end;
    end;
end;

function TSaleFrame.SetShowItemList(AVOs: TShowItemList): Boolean;
var
  i: Integer;
  vo: TShowItem;
begin
  Result := False;
  if FVOList.Count + AVOs.Count > 9999 then
    Exit;
  for i:=0 to AVOs.Count-1 do
    begin
      vo := TShowItem.Create;
      vo.Assign(AVOs[i]);
      FVOList.Add(vo.UUID, vo);
    end;
  SelectControl(FVOList[FVOList.Count-1]);
  SetSum;
  Result := True;
end;

function TSaleFrame.Clear: Boolean;
begin
  Result := False;
  FCurrSelectVO := nil;
  FVOList.Clear;
  FControlList.Clear;
  SetSum;
  Result := True;
end;

procedure TSaleFrame.AddListener(AListener: ICMListener);
begin
  if Supports(AListener, ISaleBoardListener) then
    FBoardListener.Add(AListener);
end;

procedure TSaleFrame.PromptMessage(et: TEventType; const msg: string);
begin
  case et of
  etInfo: PanelMsg.Font.Color := clGreen;
  etWarning: PanelMsg.Font.Color := clYellow;
  etError: PanelMsg.Font.Color := clRed;
  else PanelMsg.Font.Color := clBlue;
  end;
  PanelMsg.Caption := msg;
  PanelMsg.Visible := True;
  PanelMsg.Left := (PanelItems.Width - PanelMsg.Width) div 2;
end;

procedure TSaleFrame.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  LabelScan.Color := ATheme.GetParameter.Get('sale.labelScanColor').AsInteger;
end;


end.

