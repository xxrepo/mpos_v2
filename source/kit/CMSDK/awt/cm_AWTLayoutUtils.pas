unit cm_AWTLayoutUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  cm_AWT;

type

  { TALayoutManager }

  TALayoutManager = class abstract(TComponent)
  private
    FContainer: TAControl;
    FBorderSpacing: Integer;
    FLeftSpacing: Integer;
    FTopSpacing: Integer;
    procedure SetBorderSpacing(AValue: Integer);
    procedure SetContainer(AValue: TAControl);
    procedure SetLeftSpacing(AValue: Integer);
    procedure SetTopSpacing(AValue: Integer);
  protected
    FItems: TFPHashObjectList;
  public
    constructor Create(AOwner: TComponent; AContainer: TAControl); virtual; reintroduce;
    destructor Destroy; override;
    property Container: TAControl read FContainer write SetContainer; //布置指定容器
  protected type
    TLayoutItem = class
      FControl: TAControl;
    end;
    TLayoutItemClass = class of TLayoutItem;
  protected
    FItemClass: TLayoutItemClass;
    function CreateItem(AControl: TAControl): TLayoutItem;
    function AddItem(AItem: TLayoutItem): Integer;
    procedure BeforeRemoveItem(AControl: TAControl; AItem: TLayoutItem); virtual;
  protected
    property BorderSpacing: Integer read FBorderSpacing write SetBorderSpacing;
    property LeftSpacing: Integer read FLeftSpacing write SetLeftSpacing;
    property TopSpacing: Integer read FTopSpacing write SetTopSpacing;
  public
    function AddLayoutControl(AControl: TAControl): Boolean;
    function PutLayoutControl(AControl: TAControl): Boolean; virtual;
    procedure PutLayoutControls(AControls: array of TAControl);
    procedure RemoveLayoutControl(AControl: TAControl);
    function Count: Integer;
    procedure Clear; virtual;
    procedure ReLayout; virtual; abstract;
  end;

  TControlOrientation = (coLeftToRight, coRightToLeft, coLeftToBottom, coRightToBottom);

  { TAFlowLayout
    // 流布局用于安排有向流中的组件，这非常类似于段落中的文本行。流的方向取决于容器的 ControlOrientation 属性
  }

  TAFlowLayout = class(TALayoutManager)
  private
    FControlOrientation: TControlOrientation;
    FLineMaxCount: Integer;
    FItemLength: Integer;
    procedure SetControlOrientation(AValue: TControlOrientation);
    procedure SetItemLength(AValue: Integer);
    procedure SetLineMaxCount(AValue: Integer);
  public
    constructor Create(AOwner: TComponent; AContainer: TAControl); override;
  protected type
    TFlowLayoutItem = class(TLayoutItem)
      FNewLine: Boolean; //用于指定新行
      procedure AfterConstruction; override;
    end;
  public
    property BorderSpacing;
    property LeftSpacing;
    property TopSpacing;
    property ControlOrientation: TControlOrientation read FControlOrientation write SetControlOrientation;
    property LineMaxCount: Integer read FLineMaxCount write SetLineMaxCount; //流动线中的最大布局数量
    property ItemLength: Integer read FItemLength write SetItemLength; //当值大于-1时布局为指定长度
    function PutLayoutControlToNewLine(AControl: TAControl): Boolean;
    procedure PutLayoutControlsToNewLine(AControls: array of TAControl);
    procedure ReLayout; override;
  end;

  { TAGridLayout }

  TAGridLayout = class(TALayoutManager)
  private
    FDefaultColWidth: Integer;
    FDefaultRowHeight: Integer;
    FColWidths: array of Integer;
    FRowHeights: array of Integer;
    FAlignAtGrid: Boolean;
    FSetPosSL: TStrings;
    //FLayoutRecList: TFPHashObjectList;
    function GetColCount: Integer;
    function GetColWidth(ACol: Integer): Integer;
    //function GetItemAlignAtGrid(ACol, ARow: Integer): Boolean;
    function GetRowCount: Integer;
    function GetRowHeight(ARow: Integer): Integer;
    procedure SetAlignAtGrid(AValue: Boolean);
    procedure SetColCount(AValue: Integer);
    procedure SetColWidth(ACol: Integer; AValue: Integer);
    //procedure SetItemAlignAtGrid(ACol, ARow: Integer; AValue: Boolean);
    procedure SetRowCount(AValue: Integer);
    procedure SetRowHeight(ARow: Integer; AValue: Integer);
    function GetColRowRecStr(ACol, ARow: Integer): string;
  protected type
    TGridLayoutItem = class(TLayoutItem)
      FCol: Integer;    //实现分配的位置
      FRow: Integer;
      FControlWidth: Integer;  //AtGrid 前的尺寸
      FControlHeight: Integer;
      procedure AfterConstruction; override;
    private
      FSetCol: Integer; //放入时设置的位置，未设置为-1
      FSetRow: Integer;
      FSizeStored: Boolean;
      FIsSetAlign: Boolean;
      FAlignValue: Boolean;
    end;
  public
    constructor Create(AOwner: TComponent; AContainer: TAControl); override;
    constructor Create(AOwner: TComponent; AContainer: TAControl; ACols, ARows: Integer); overload;
    destructor Destroy; override;
    property ColCount: Integer read GetColCount write SetColCount;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property ColWidths[ACol: Integer]: Integer read GetColWidth write SetColWidth;
    property RowHeights[ARow: Integer]: Integer read GetRowHeight write SetRowHeight;
    property DefaultColWidth: Integer read FDefaultColWidth write FDefaultColWidth;
    property DefaultRowHeight: Integer read FDefaultRowHeight write FDefaultRowHeight;
    procedure SetColsWidth(AValue: Integer);
    procedure SetRowsHeight(AValue: Integer);
    property AlignAtGrid: Boolean read FAlignAtGrid write SetAlignAtGrid;
    //property ItemAlignAtGrid[ACol, ARow: Integer]: Boolean read GetItemAlignAtGrid write SetItemAlignAtGrid;
    function PutLayoutControl(AControl: TAControl): Boolean; override;
    function PutLayoutControl(AControl: TAControl; ACol, ARow: Integer): Boolean; overload;
    function PutLayoutControl(AControl: TAControl; IsAlign: Boolean): Boolean; overload;
    procedure ReLayout; override;
  end;

implementation

{ TALayoutManager }

procedure TALayoutManager.SetContainer(AValue: TAControl);
begin
  if FContainer = AValue then
    Exit;
  FContainer := AValue;
  ReLayout;
end;

procedure TALayoutManager.SetBorderSpacing(AValue: Integer);
begin
  if FBorderSpacing = AValue then
    Exit;
  FBorderSpacing := AValue;
  ReLayout;
end;

procedure TALayoutManager.SetLeftSpacing(AValue: Integer);
begin
  if FLeftSpacing = AValue then
    Exit;
  FLeftSpacing := AValue;
  ReLayout;
end;

procedure TALayoutManager.SetTopSpacing(AValue: Integer);
begin
  if FTopSpacing = AValue then
    Exit;
  FTopSpacing := AValue;
  ReLayout;
end;

constructor TALayoutManager.Create(AOwner: TComponent; AContainer: TAControl);
begin
  inherited Create(AOwner);
  FContainer := AContainer;
  FItemClass := TLayoutItem;
  FItems := TFPHashObjectList.Create(True);
  FBorderSpacing := 10;
  FLeftSpacing := 10;
  FTopSpacing := 10;
end;

destructor TALayoutManager.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TALayoutManager.CreateItem(AControl: TAControl): TLayoutItem;
begin
  Result := nil;
  if not Assigned(AControl) then
    Exit;
  if FItems.FindIndexOf(IntToStr(AControl.GetHashCode)) >= 0 then
    Exit;
  Result := FItemClass.Create;
  Result.FControl := AControl;
end;

function TALayoutManager.AddItem(AItem: TLayoutItem): Integer;
begin
  Result := FItems.Add(IntToStr(AItem.FControl.GetHashCode), AItem);
end;

procedure TALayoutManager.BeforeRemoveItem(AControl: TAControl; AItem: TLayoutItem);
begin
  //
end;

function TALayoutManager.AddLayoutControl(AControl: TAControl): Boolean;
begin
  Result := PutLayoutControl(AControl);
  if Result then
    ReLayout;
end;

function TALayoutManager.PutLayoutControl(AControl: TAControl): Boolean;
var
  item: TLayoutItem;
begin
  Result := False;
  item := Self.CreateItem(AControl);
  if Assigned(item) then
    begin
      Result := Self.AddItem(item) >= 0;
    end;
end;

procedure TALayoutManager.PutLayoutControls(AControls: array of TAControl);
var
  i: Integer;
begin
  for i:=Low(AControls) to High(AControls) do
    PutLayoutControl(AControls[i]);
end;

procedure TALayoutManager.RemoveLayoutControl(AControl: TAControl);
var
  item: TLayoutItem;
begin
  item := TLayoutItem(FItems.Find(IntToStr(AControl.GetHashCode)));
  if Assigned(item) then
    begin
      BeforeRemoveItem(AControl, item);
      FItems.Remove(item);
    end;
end;

function TALayoutManager.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TALayoutManager.Clear;
begin
  FItems.Clear;
  ReLayout;
end;

{ TAFlowLayout.TFlowLayoutItem }

procedure TAFlowLayout.TFlowLayoutItem.AfterConstruction;
begin
  inherited AfterConstruction;
  FNewLine := False;
end;

{ TAFlowLayout }

procedure TAFlowLayout.SetControlOrientation(AValue: TControlOrientation);
begin
  if FControlOrientation = AValue then
    Exit;
  FControlOrientation := AValue;
  ReLayout;
end;

procedure TAFlowLayout.SetItemLength(AValue: Integer);
begin
  if FItemLength = AValue then
    Exit;
  FItemLength := AValue;
  ReLayout;
end;

procedure TAFlowLayout.SetLineMaxCount(AValue: Integer);
begin
  if FLineMaxCount = AValue then
    Exit;
  FLineMaxCount := AValue;
  ReLayout;
end;

constructor TAFlowLayout.Create(AOwner: TComponent; AContainer: TAControl);
begin
  inherited Create(AOwner, AContainer);
  FItemClass := TFlowLayoutItem;
  FControlOrientation := coLeftToRight;
  FLineMaxCount := 256;
  FItemLength := -1;
end;

function TAFlowLayout.PutLayoutControlToNewLine(AControl: TAControl): Boolean;
var
  item: TFlowLayoutItem;
begin
  Result := False;
  item := TFlowLayoutItem(Self.CreateItem(AControl));
  if Assigned(item) then
    begin
      item.FNewLine := True;
      Result := Self.AddItem(item) >= 0;
    end;
end;

procedure TAFlowLayout.PutLayoutControlsToNewLine(AControls: array of TAControl);
var
  i: Integer;
begin
  if Length(AControls) > 0 then
    begin
      Self.PutLayoutControlToNewLine(AControls[Low(AControls)]);
      for i:= Low(AControls) + 1 to High(AControls) do
        Self.PutLayoutControl(AControls[i]);
    end;
end;

procedure TAFlowLayout.ReLayout;
var
  i: Integer;
  theItem: TFlowLayoutItem;
  theControl: TAControl;
  tabOrderInt: Integer;
  //位置相关
  nextLineX, nextLineY: Integer;
  x, y: Integer;
  //
  currMaxLineMark: Integer;
  lineControlCount: Integer;
begin
  if not Assigned(FContainer) then
    Exit;
  if (FContainer is TAWinControl) and (not TAWinControl(FContainer).Showing) then
    Exit;
  tabOrderInt := 0;
  currMaxLineMark := 0;
  lineControlCount := 0;
  //
  for i:=0 to FItems.Count-1 do
    begin
      theItem := TFlowLayoutItem(FItems[i]);
      theControl := theItem.FControl;
      if Assigned(theControl) then
        begin
          if not theControl.Visible then
            Continue;
          if theControl is TAWinControl then
            begin
              TAWinControl(theControl).TabOrder := tabOrderInt;
              tabOrderInt := tabOrderInt + 1;
            end;
          //
          if ControlOrientation = coLeftToRight then
            begin
              if i = 0 then
                begin
                  nextLineX := BorderSpacing;
                  nextLineY := BorderSpacing;
                end
              else if theItem.FNewLine or (nextLineX + theControl.Width + BorderSpacing > FContainer.Width) or (lineControlCount mod LineMaxCount = 0) then
                begin
                  nextLineX := BorderSpacing;
                  nextLineY := nextLineY + currMaxLineMark + TopSpacing;
                  currMaxLineMark := 0;
                  lineControlCount := 0;
                end;
              x := nextLineX;
              if FItemLength > -1 then
                nextLineX := x + FItemLength + LeftSpacing
              else
                nextLineX := x + theControl.Width + LeftSpacing;
              y := nextLineY;
              //
              if theControl.Height > currMaxLineMark then
                currMaxLineMark := theControl.Height;
            end
          else if ControlOrientation = coRightToLeft then
            begin
              if i = 0 then
                begin
                  nextLineX := FContainer.Width - BorderSpacing;
                  nextLineY := BorderSpacing;
                end
              else if theItem.FNewLine or (nextLineX < theControl.Width + BorderSpacing) or (lineControlCount mod LineMaxCount = 0) then
                begin
                  nextLineX := FContainer.Width - BorderSpacing;
                  nextLineY := nextLineY + currMaxLineMark + TopSpacing;
                  currMaxLineMark := 0;
                  lineControlCount := 0;
                end;
              if FItemLength > -1 then
                x := nextLineX - FItemLength
              else
                x := nextLineX - theControl.Width;
              nextLineX := x - LeftSpacing;
              y := nextLineY;
              //
              if theControl.Height > currMaxLineMark then
                currMaxLineMark := theControl.Height;
            end
          else if ControlOrientation = coLeftToBottom then
            begin
              if i = 0 then
                begin
                  nextLineX := BorderSpacing;
                  nextLineY := BorderSpacing;
                end
              else if theItem.FNewLine or (nextLineY + theControl.Height + BorderSpacing > FContainer.Height) or (lineControlCount mod LineMaxCount = 0) then
                begin
                  nextLineX := nextLineX + currMaxLineMark + LeftSpacing;
                  nextLineY := BorderSpacing;
                  currMaxLineMark := 0;
                  lineControlCount := 0;
                end;
              x := nextLineX;
              y := nextLineY;
              if FItemLength > -1 then
                nextLineY := y + FItemLength + TopSpacing
              else
                nextLineY := y + theControl.Height + TopSpacing;
              //
              if theControl.Width > currMaxLineMark then
                currMaxLineMark := theControl.Width;
            end
          else if ControlOrientation = coRightToBottom then
            begin
              if i = 0 then
                begin
                  nextLineX := FContainer.Width - BorderSpacing;
                  nextLineY := BorderSpacing;
                end
              else if theItem.FNewLine or (nextLineY + theControl.Height + BorderSpacing > FContainer.Height) or (lineControlCount mod LineMaxCount = 0) then
                begin
                  nextLineX := nextLineX - currMaxLineMark - LeftSpacing;
                  nextLineY := BorderSpacing;
                  currMaxLineMark := 0;
                  lineControlCount := 0;
                end;
              x := nextLineX - theControl.Width;
              y := nextLineY;
              if FItemLength > -1 then
                nextLineY := y + FItemLength + TopSpacing
              else
                nextLineY := y + theControl.Height + TopSpacing;
              //
              if theControl.Width > currMaxLineMark then
                currMaxLineMark := theControl.Width;
            end;
          theControl.Left := x;
          theControl.Top := y;
          lineControlCount := lineControlCount + 1;
        end;
    end;
end;

{ TAGridLayout.TGridLayoutItem }

procedure TAGridLayout.TGridLayoutItem.AfterConstruction;
begin
  inherited AfterConstruction;
  FCol := -1;
  FRow := -1;
  FSetCol := -1;
  FSetRow := -1;
  FSizeStored := False;
  FIsSetAlign := False;
end;

{ TAGridLayout }

constructor TAGridLayout.Create(AOwner: TComponent; AContainer: TAControl);
begin
  inherited Create(AOwner, AContainer);
  FItemClass := TGridLayoutItem;
  //FLayoutRecList := TFPHashObjectList.Create(False);
  FDefaultColWidth := 80;
  FDefaultRowHeight := 40;
  FAlignAtGrid := False;
  //
  ColCount := 4;
  RowCount := 2;
  //
  FSetPosSL := TStringList.Create;
end;

constructor TAGridLayout.Create(AOwner: TComponent; AContainer: TAControl; ACols, ARows: Integer);
begin
  Self.Create(AOwner, AContainer);
  ColCount := ACols;
  RowCount := ARows;
end;

destructor TAGridLayout.Destroy;
begin
  FSetPosSL.Free;
  //FLayoutRecList.Free;
  inherited Destroy;
end;

function TAGridLayout.GetColCount: Integer;
begin
  Result := Length(FColWidths);
end;

function TAGridLayout.GetColWidth(ACol: Integer): Integer;
begin
  Result := FColWidths[ACol];
end;

//function TAGridLayout.GetItemAlignAtGrid(ACol, ARow: Integer): Boolean;
//var
//  item: TGridLayoutItem;
//begin
//  Result := False;
//  item := TGridLayoutItem(FLayoutRecList.Find(GetColRowRecStr(ACol, ARow)));
//  if Assigned(item) and item.FIsSetAlign then
//    Result := item.FAlignValue;
//end;

function TAGridLayout.GetRowCount: Integer;
begin
  Result := Length(FRowHeights);
end;

function TAGridLayout.GetRowHeight(ARow: Integer): Integer;
begin
  Result := FRowHeights[ARow];
end;

procedure TAGridLayout.SetAlignAtGrid(AValue: Boolean);
begin
  if FAlignAtGrid = AValue then
    Exit;
  FAlignAtGrid := AValue;
  ReLayout;
end;

procedure TAGridLayout.SetColCount(AValue: Integer);
var
  l, i: Integer;
begin
  if AValue >= 0 then
    begin
      l := Length(FColWidths);
      SetLength(FColWidths, AValue);
      for i:=l to High(FColWidths) do
        FColWidths[i] := FDefaultColWidth;
      ReLayout;
    end;
end;

procedure TAGridLayout.SetColWidth(ACol: Integer; AValue: Integer);
begin
  if FColWidths[ACol] = AValue then
    Exit;
  FColWidths[ACol] := AValue;
  ReLayout;
end;

//procedure TAGridLayout.SetItemAlignAtGrid(ACol, ARow: Integer; AValue: Boolean);
//var
//  item: TGridLayoutItem;
//begin
//  item := TGridLayoutItem(FLayoutRecList.Find(GetColRowRecStr(ACol, ARow)));
//  if Assigned(item) then
//    begin
//      item.FIsSetAlign := True;
//      item.FAlignValue := AValue;
//      ReLayout;
//    end;
//end;

procedure TAGridLayout.SetRowCount(AValue: Integer);
var
  l, i: Integer;
begin
  if AValue >= 0 then
    begin
      l := Length(FRowHeights);
      SetLength(FRowHeights, AValue);
      for i:=l to High(FRowHeights) do
        FRowHeights[i] := FDefaultRowHeight;
      ReLayout;
    end;
end;

procedure TAGridLayout.SetRowHeight(ARow: Integer; AValue: Integer);
begin
  if FRowHeights[ARow] = AValue then
    Exit;
  FRowHeights[ARow] := AValue;
  ReLayout;
end;

function TAGridLayout.GetColRowRecStr(ACol, ARow: Integer): string;
begin
  Result := Format('%d-%d', [ACol, ARow]);
end;

procedure TAGridLayout.SetColsWidth(AValue: Integer);
var
  i: Integer;
begin
  FDefaultColWidth := AValue;
  for i:=Low(FColWidths) to High(FColWidths) do
    FColWidths[i] := AValue;
  ReLayout;
end;

procedure TAGridLayout.SetRowsHeight(AValue: Integer);
var
  i: Integer;
begin
  FDefaultRowHeight := AValue;
  for i:=Low(FRowHeights) to High(FRowHeights) do
    FRowHeights[i] := AValue;
  ReLayout;
end;

function TAGridLayout.PutLayoutControl(AControl: TAControl): Boolean;
var
  item: TGridLayoutItem;
begin
  Result := False;
  item := TGridLayoutItem(Self.CreateItem(AControl));
  if Assigned(item) then
    begin
      item.FControlWidth := AControl.Width;
      item.FControlHeight := AControl.Height;
      Result := Self.AddItem(item) >= 0;
    end;
end;

function TAGridLayout.PutLayoutControl(AControl: TAControl; ACol, ARow: Integer): Boolean;
var
  item: TGridLayoutItem;
begin
  Result := False;
  item := TGridLayoutItem(Self.CreateItem(AControl));
  if Assigned(item) then
    begin
      item.FControlWidth := AControl.Width;
      item.FControlHeight := AControl.Height;
      item.FSetCol := ACol;
      item.FSetRow := ARow;
      Result := Self.AddItem(item) >= 0;
    end;
end;

function TAGridLayout.PutLayoutControl(AControl: TAControl; IsAlign: Boolean): Boolean;
var
  item: TGridLayoutItem;
begin
  Result := False;
  item := TGridLayoutItem(Self.CreateItem(AControl));
  if Assigned(item) then
    begin
      item.FControlWidth := AControl.Width;
      item.FControlHeight := AControl.Height;
      item.FIsSetAlign := True;
      item.FAlignValue := IsAlign;
      Result := Self.AddItem(item) >= 0;
    end;
end;

procedure TAGridLayout.ReLayout;
var
  tabOrderInt: Integer;
  i, tempInt: Integer;
  theItem: TGridLayoutItem;
  theControl: TAControl;
  lefts, tops: array of Integer;
  rCol, rRow: Integer;
  procedure SetPos(c, r: Integer);
  var needAlign: Boolean;
  begin
    theItem.FCol := c;
    theItem.FRow := r;
    theControl.Left := lefts[c];
    theControl.Top := tops[r];
    //FLayoutRecList.Add(GetColRowRecStr(c,r), theItem);
    needAlign := AlignAtGrid;
    if theItem.FIsSetAlign then
      needAlign := theItem.FAlignValue;
    if needAlign then
      begin
        if not theItem.FSizeStored then
          begin
            theItem.FControlWidth := theControl.Width;
            theItem.FControlHeight := theControl.Height;
            theItem.FSizeStored := True;
          end;
        theControl.Width := FColWidths[c] - LeftSpacing;
        theControl.Height := FRowHeights[r] - TopSpacing;
      end
    else if theItem.FSizeStored then
      begin
        theControl.Width := theItem.FControlWidth;
        theControl.Height := theItem.FControlHeight;
        theItem.FSizeStored := False;
      end;
  end;
begin
  if not Assigned(FContainer) then
    Exit;
  if (FContainer is TAWinControl) and (not TAWinControl(FContainer).Showing) then
    Exit;
  if (ColCount <= 0) or (RowCount <= 0) then
    Exit;
  //先计算出所有行列的起点
  SetLength(lefts, ColCount);
  SetLength(tops, RowCount);
  tempInt := BorderSpacing;
  for i:=Low(FColWidths) to High(FColWidths) do
    begin
      lefts[i] := tempInt;
      tempInt := tempInt + FColWidths[i];
    end;
  tempInt := BorderSpacing;
  for i:=Low(FRowHeights) to High(FRowHeights) do
    begin
      tops[i] := tempInt;
      tempInt := tempInt + FRowHeights[i];
    end;
  //FLayoutRecList.Clear;
  //先布局指定位置的
  FSetPosSL.Clear;
  for i:=0 to FItems.Count-1 do
    begin
      theItem := TGridLayoutItem(FItems[i]);
      theControl := theItem.FControl;
      if not theControl.Visible then
        Continue;
      if (theItem.FSetCol >= 0) or (theItem.FSetRow >= 0) then
        begin
          FSetPosSL.Add(GetColRowRecStr(theItem.FSetCol, theItem.FSetRow));
          if (theItem.FSetCol < ColCount) and (theItem.FSetRow < RowCount) then
            SetPos(theItem.FSetCol, theItem.FSetRow);
        end;
    end;
  //其他展开布局
  tabOrderInt := 0;
  rCol := 0;
  rRow := 0;
  for i:=0 to FItems.Count-1 do
    begin
      theItem := TGridLayoutItem(FItems[i]);
      theControl := theItem.FControl;
      if not Assigned(theControl) then
        Continue;
      if not theControl.Visible then
        Continue;
      //TabOrder的设置包含了指定位置的
      if theControl is TAWinControl then
        begin
          TAWinControl(theControl).TabOrder := tabOrderInt;
          tabOrderInt := tabOrderInt + 1;
        end;
      //如果是已指定位置的则跳过，进行下一操作
      if (theItem.FSetCol >= 0) or (theItem.FSetRow >= 0) then
        if FSetPosSL.IndexOf(GetColRowRecStr(theItem.FSetCol, theItem.FSetRow)) >= 0 then
          Continue;
      //如果当前位置被指定位置占用，则换下一个位置
      while FSetPosSL.IndexOf(GetColRowRecStr(rCol, rRow)) >= 0 do
        begin
          rCol := rCol + 1;
          if rCol >= ColCount then
            begin
              rCol := 0;
              rRow := rRow + 1;
              if rRow >= RowCount then
                begin
                  rRow := 0;
                  Break; //避免死循环
                end;
            end;
        end;
      SetPos(rCol, rRow);
      //计算下一步位置
      rCol := rCol + 1;
      if rCol >= ColCount then
        begin
          rCol := 0;
          rRow := rRow + 1;
          if rRow >= RowCount then
            rRow := 0;
        end;
    end;
end;

end.

