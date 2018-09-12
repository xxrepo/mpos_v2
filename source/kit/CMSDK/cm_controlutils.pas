{
    This file is part of the CM Software Development Kit.
    Copyright (c) 2013-2018 by the ChenMeng studio

    cm_controlutils

    This is not a complete unit,
    Here is the copy part of the CMSDK, for demo testing


 **********************************************************************}

unit cm_controlutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Contnrs, Graphics, ExtCtrls;

type

  { TCMLayoutItem }

  TCMLayoutItem = class
  private
    FControl: TControl;
  public
    constructor Create;
    property Control: TControl read FControl;
  end;

  TCMLayoutItemClass = class of TCMLayoutItem;

  { TCMLayoutManager }

  TCMLayoutManager = class(TComponent)
  private
    FContainer: TControl;
    FItems: TFPHashObjectList;
    FBorderSpacing: Integer;
    FLeftSpacing: Integer;
    FTopSpacing: Integer;
    procedure setContainer(AValue: TControl);
    procedure setBorderSpacing(AValue: Integer);
    procedure setLeftSpacing(AValue: Integer);
    procedure setTopSpacing(AValue: Integer);
  protected
    FItemClass: TCMLayoutItemClass;
    function GetItem(AIndex: Integer): TCMLayoutItem;
    function GetItem(AControl: TControl): TCMLayoutItem;
    function GetItemIndex(AControl: TControl): Integer; overload;
    function GetItemIndex(AItem: TCMLayoutItem): Integer; overload;
    function CreateItem(AControl: TControl): TCMLayoutItem;
    function AddItem(AItem: TCMLayoutItem): Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Container: TControl read FContainer write setContainer;
    property BorderSpacing: Integer read FBorderSpacing write setBorderSpacing;
    property LeftSpacing: Integer read FLeftSpacing write setLeftSpacing;
    property TopSpacing: Integer read FTopSpacing write setTopSpacing;
    function AddLayoutControl(AControl: TControl): Boolean;
    function PutLayoutControl(AControl: TControl): TCMLayoutItem; virtual;
    procedure RemoveLayoutControl(AControl: TControl); virtual;
    function Count: Integer;
    procedure Clear; virtual;
    procedure ReLayout; virtual; abstract;
  end;

  TCMFlowLayoutOrientation = (floLeftToRight, floRightToLeft, floLeftToBottom, floRightToBottom);

  { TCMFlowLayout }

  TCMFlowLayout = class(TCMLayoutManager)
  private
    FFlowLayoutOrientation: TCMFlowLayoutOrientation;
    FLineMaxCount: Integer;
    procedure setLineMaxCount(AValue: Integer);
    procedure setFlowLayoutOrientation(AValue: TCMFlowLayoutOrientation);
  protected type
    TCMFlowLayoutItem = class(TCMLayoutItem)
    private
      FNewLine: Boolean;
    public
      constructor Create;
      property NewLine: Boolean read FNewLine write FNewLine;
    end;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FlowLayoutOrientation: TCMFlowLayoutOrientation read FFlowLayoutOrientation write setFlowLayoutOrientation;
    property LineMaxCount: Integer read FLineMaxCount write setLineMaxCount;
    function PutLayoutControl(AControl: TControl; NewLine: Boolean): TCMLayoutItem; overload; inline;
    procedure ReLayout; override;
  end;

  { TCMGridLayout }

  TCMGridLayout = class(TCMLayoutManager)
  private
    FColWidths: array of Integer;
    FRowHeights: array of Integer;
    FDefaultColWidth: Integer;
    FDefaultRowHeight: Integer;
    FAlignAtGrid: Boolean;
    FColRowRecList: TFPHashObjectList;
    function getColCount: Integer;
    function getRowCount: Integer;
    function getColWidths(ACol: Integer): Integer;
    function getRowHeights(ARow: Integer): Integer;
    procedure setColCount(AValue: Integer);
    procedure setRowCount(AValue: Integer);
    procedure setColWidths(ACol: Integer; AValue: Integer);
    procedure setRowHeights(ARow: Integer; AValue: Integer);
    procedure setAlignAtGrid(AValue: Boolean);
  protected type
    TCMGridLayoutItem = class(TCMLayoutItem)
    private
      FCol: Integer;
      FRow: Integer;
      FSizeStored: Boolean;
      FControlWidth: Integer;
      FControlHeight: Integer;
    public
      constructor Create;
      property Col: Integer read FCol;
      property Row: Integer read FRow;
      property SizeStored: Boolean read FSizeStored write FSizeStored;
      property ControlWidth: Integer read FControlWidth write FControlWidth;
      property ControlHeight: Integer read FControlHeight write FControlHeight;
    end;
    function ColRowRecStr(ACol, ARow: Integer): string;
  public
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(AOwner: TComponent; ACols, ARows: Integer); virtual; overload;
    destructor Destroy; override;
    property ColCount: Integer read getColCount write setColCount;
    property RowCount: Integer read getRowCount write setRowCount;
    property ColWidths[ACol: Integer]: Integer read getColWidths write setColWidths;
    property RowHeights[ARow: Integer]: Integer read getRowHeights write setRowHeights;
    property DefaultColWidth: Integer read FDefaultColWidth write FDefaultColWidth;
    property DefaultRowHeight: Integer read FDefaultRowHeight write FDefaultRowHeight;
    procedure SetColWidth(AValue: Integer);
    procedure SetRowHeight(AValue: Integer);
    property AlignAtGrid: Boolean read FAlignAtGrid write setAlignAtGrid;
    function PutLayoutControl(AControl: TControl): TCMLayoutItem; override;
    function PutLayoutControl(AControl: TControl; ACol, ARow: Integer): TCMLayoutItem; overload;
    procedure RemoveLayoutControl(AControl: TControl); override;
    procedure ReLayout; override;
  end;

  TMenuCellSelectEvent = procedure(Sender: TControl; ACol, ARow: Integer) of object;

  { TCMGridLayoutMenu }

  TCMGridLayoutMenu = class(TCMGridLayout)
  private
    FSelectedControl: TControl;
    FOnCellSelectEnter: TMenuCellSelectEvent;
    FOnCellSelectLeave: TMenuCellSelectEvent;
    function getSelectedCol: Integer;
    procedure setSelectedCol(AValue: Integer);
    function getSelectedRow: Integer;
    procedure setSelectedRow(AValue: Integer);
    function getSelectedIndex: Integer;
    procedure setSelectedIndex(AValue: Integer);
    procedure setSelectedControl(AValue: TControl);   (****)
  private
    FSelectedBoxPanel: TPanel;
    FSelectBoxWidth: Integer;
    FSelectBoxSize: Integer;
    FSelectBoxSpacing: Integer;
    FSelectedColor: TColor;
    FWhenSelectSetColor: Boolean;
    function getSelectBoxColor: TColor;
    procedure setSelectBoxColor(AValue: TColor);
    procedure setSelectBoxWidth(AValue: Integer);
    procedure setSelectBoxSize(AValue: Integer);
    procedure setSelectBoxSpacing(AValue: Integer);
    procedure SelectedBoxPanelOnPaint(Sender: TObject);
    procedure setSelectedColor(AValue: TColor);
    procedure setWhenSelectSetColor(AValue: Boolean);
    procedure reSelectedBoxPanel;
    procedure reSelectedColor;
  protected type
    TCMGridLayoutMenuItem = class(TCMGridLayoutItem)
    private
      FControlColor: TColor;
    public
      property ControlColor: TColor read FControlColor write FControlColor;
    end;
  protected
    function AddItem(AItem: TCMLayoutItem): Integer; override;
    function SelectCell(ACol, ARow: Integer): Boolean;
    property SelectedBoxPanel: TPanel read FSelectedBoxPanel;
    procedure SetToSelected(AControl: TControl); virtual;
    procedure SetToOriginal(AControl: TControl); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RemoveLayoutControl(AControl: TControl); override;
    procedure ReLayout; override;
    property SelectedCol: Integer read getSelectedCol write setSelectedCol;
    property SelectedRow: Integer read getSelectedRow write setSelectedRow;
    property SelectedIndex: Integer read getSelectedIndex write setSelectedIndex;
    property SelectedControl: TControl read FSelectedControl write setSelectedControl;
    property OnCellSelectEnter: TMenuCellSelectEvent read FOnCellSelectEnter write FOnCellSelectEnter;
    property OnCellSelectLeave: TMenuCellSelectEvent read FOnCellSelectLeave write FOnCellSelectLeave;
  public
    property SelectBoxColor: TColor read getSelectBoxColor write setSelectBoxColor;
    property SelectBoxWidth: Integer read FSelectBoxWidth write setSelectBoxWidth;
    property SelectBoxSize: Integer read FSelectBoxSize write setSelectBoxSize;
    property SelectBoxSpacing: Integer read FSelectBoxSpacing write setSelectBoxSpacing;
    property SelectedColor: TColor read FSelectedColor write setSelectedColor;
    property WhenSelectSetColor: Boolean read FWhenSelectSetColor write setWhenSelectSetColor;
    procedure Up;
    procedure Down;
    procedure Left;
    procedure Right;
  end;

  { TCMGridLayoutHostingMenu }

  TCMGridLayoutHostingMenu = class(TCMGridLayoutMenu)
  protected type
    TCMGridLayoutHostingMenuItem = class(TCMGridLayoutMenuItem)
    private
      FControlOnClick: TNotifyEvent;
    public
      constructor Create;
      property ControlOnClick: TNotifyEvent read FControlOnClick write FControlOnClick;
    end;
  protected
    function AddItem(AItem: TCMLayoutItem): Integer; override;
    procedure MenuCellClick(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DelClick(AControl: TControl); overload;
    procedure DelClick; overload;
  end;

  function GetKeyCodeCHName(const AKeyCode: Word; IsNeedCode: Boolean=True): string;

implementation

uses LCLType, TypInfo;

{ TCMLayoutItem }

constructor TCMLayoutItem.Create;
begin
  FControl := nil;
end;

{ TCMLayoutManager }

procedure TCMLayoutManager.setContainer(AValue: TControl);
begin
  if FContainer = AValue then
    Exit;
  FContainer := AValue;
  ReLayout;
end;

procedure TCMLayoutManager.setBorderSpacing(AValue: Integer);
begin
  if FBorderSpacing = AValue then
    Exit;
  FBorderSpacing := AValue;
  ReLayout;
end;

procedure TCMLayoutManager.setLeftSpacing(AValue: Integer);
begin
  if FLeftSpacing = AValue then
    Exit;
  FLeftSpacing := AValue;
  ReLayout;
end;

procedure TCMLayoutManager.setTopSpacing(AValue: Integer);
begin
  if FTopSpacing = AValue then
    Exit;
  FTopSpacing := AValue;
  ReLayout;
end;

function TCMLayoutManager.GetItem(AIndex: Integer): TCMLayoutItem;
begin
  Result := TCMLayoutItem(FItems[AIndex]);
end;

function TCMLayoutManager.GetItem(AControl: TControl): TCMLayoutItem;
begin
  Result := nil;
  if Assigned(AControl) then
    Result := TCMLayoutItem(FItems.Find(IntToStr(AControl.GetHashCode)));
end;

function TCMLayoutManager.GetItemIndex(AControl: TControl): Integer;
begin
  Result := -1;
  if Assigned(AControl) then
    Result := FItems.FindIndexOf(IntToStr(AControl.GetHashCode));
end;

function TCMLayoutManager.GetItemIndex(AItem: TCMLayoutItem): Integer;
begin
  Result := FItems.IndexOf(AItem);
end;

function TCMLayoutManager.CreateItem(AControl: TControl): TCMLayoutItem;
begin
  Result := nil;
  if FItems.FindIndexOf(IntToStr(AControl.GetHashCode)) >= 0 then
    Exit;
  Result := FItemClass.Create;
  Result.FControl := AControl;
end;

function TCMLayoutManager.AddItem(AItem: TCMLayoutItem): Integer;
begin
  Result := FItems.Add(IntToStr(AItem.Control.GetHashCode), AItem);
end;

constructor TCMLayoutManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TControl then
    FContainer := TControl(AOwner);
  FItemClass := TCMLayoutItem;
  FItems := TFPHashObjectList.Create(True);
  FBorderSpacing := 10;
  FLeftSpacing := 10;
  FTopSpacing := 10;
end;

destructor TCMLayoutManager.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TCMLayoutManager.AddLayoutControl(AControl: TControl): Boolean;
begin
  Result := Assigned(PutLayoutControl(AControl));
  ReLayout;
end;

function TCMLayoutManager.PutLayoutControl(AControl: TControl): TCMLayoutItem;
begin
  Result := nil;
  if Assigned(AControl) then
    begin
      Result := Self.CreateItem(AControl);
      if Assigned(Result) then
        Self.AddItem(Result);
    end;
end;

procedure TCMLayoutManager.RemoveLayoutControl(AControl: TControl);
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do
    begin
      if TCMLayoutItem(FItems[i]).FControl = AControl then
        begin
          FItems.Delete(i);
          Break;
        end;
    end;
end;

function TCMLayoutManager.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TCMLayoutManager.Clear;
begin
  FItems.Clear;
  ReLayout;
end;

{ TCMFlowLayout.TCMFlowLayoutItem }

constructor TCMFlowLayout.TCMFlowLayoutItem.Create;
begin
  inherited Create;
  FNewLine := False;
end;

{ TCMFlowLayout }

procedure TCMFlowLayout.setLineMaxCount(AValue: Integer);
begin
  if FLineMaxCount = AValue then
    Exit;
  FLineMaxCount := AValue;
  ReLayout;
end;

procedure TCMFlowLayout.setFlowLayoutOrientation(AValue: TCMFlowLayoutOrientation);
begin
  if FFlowLayoutOrientation = AValue then
    Exit;
  FFlowLayoutOrientation := AValue;
  ReLayout;
end;

constructor TCMFlowLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemClass := TCMFlowLayoutItem;
  FFlowLayoutOrientation := floLeftToRight;
  FLineMaxCount := 256;
end;

destructor TCMFlowLayout.Destroy;
begin
  inherited Destroy;
end;

function TCMFlowLayout.PutLayoutControl(AControl: TControl; NewLine: Boolean): TCMLayoutItem;
begin
  Result := nil;
  if Assigned(AControl) then
    begin
      Result := Self.CreateItem(AControl);
      if Assigned(Result) then
        begin
          TCMFlowLayoutItem(Result).NewLine := NewLine;
          Self.AddItem(Result);
        end;
    end;
end;

procedure TCMFlowLayout.ReLayout;
var
  i: Integer;
  theItem: TCMFlowLayoutItem;
  theControl: TControl;
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
  if (FContainer is TWinControl) and (not TWinControl(FContainer).Showing) then
    Exit;
  tabOrderInt := 0;
  currMaxLineMark := 0;
  lineControlCount := 0;
  //
  for i:=0 to Self.Count-1 do
    begin
      theItem := TCMFlowLayoutItem(Self.GetItem(i));
      theControl := theItem.Control;
      if Assigned(theControl) then
        begin
          if theControl is TWinControl then
            begin
              TWinControl(theControl).TabOrder := tabOrderInt;
              tabOrderInt := tabOrderInt + 1;
            end;
          //
          if FlowLayoutOrientation = floLeftToRight then
            begin
              if i = 0 then
                begin
                  nextLineX := BorderSpacing;
                  nextLineY := BorderSpacing;
                end
              else if theItem.NewLine or (nextLineX + theControl.Width + BorderSpacing > FContainer.Width) or (lineControlCount mod LineMaxCount = 0) then
                begin
                  nextLineX := BorderSpacing;
                  nextLineY := nextLineY + currMaxLineMark + TopSpacing;
                  currMaxLineMark := 0;
                  lineControlCount := 0;
                end;
              x := nextLineX;
              nextLineX := x + theControl.Width + LeftSpacing;
              y := nextLineY;
              //
              if theControl.Height > currMaxLineMark then
                currMaxLineMark := theControl.Height;
            end
          else if FlowLayoutOrientation = floRightToLeft then
            begin
              if i = 0 then
                begin
                  nextLineX := FContainer.Width - BorderSpacing;
                  nextLineY := BorderSpacing;
                end
              else if theItem.NewLine or (nextLineX < theControl.Width + BorderSpacing) or (lineControlCount mod LineMaxCount = 0) then
                begin
                  nextLineX := FContainer.Width - BorderSpacing;
                  nextLineY := nextLineY + currMaxLineMark + TopSpacing;
                  currMaxLineMark := 0;
                  lineControlCount := 0;
                end;
              x := nextLineX - theControl.Width;
              nextLineX := x - LeftSpacing;
              y := nextLineY;
              //
              if theControl.Height > currMaxLineMark then
                currMaxLineMark := theControl.Height;
            end
          else if FlowLayoutOrientation = floLeftToBottom then
            begin
              if i = 0 then
                begin
                  nextLineX := BorderSpacing;
                  nextLineY := BorderSpacing;
                end
              else if theItem.NewLine or (nextLineY + theControl.Height + BorderSpacing > FContainer.Height) or (lineControlCount mod LineMaxCount = 0) then
                begin
                  nextLineX := nextLineX + currMaxLineMark + LeftSpacing;
                  nextLineY := BorderSpacing;
                  currMaxLineMark := 0;
                  lineControlCount := 0;
                end;
              x := nextLineX;
              y := nextLineY;
              nextLineY := y + theControl.Height + TopSpacing;
              //
              if theControl.Width > currMaxLineMark then
                currMaxLineMark := theControl.Width;
            end
          else if FlowLayoutOrientation = floRightToBottom then
            begin
              if i = 0 then
                begin
                  nextLineX := FContainer.Width - BorderSpacing;
                  nextLineY := BorderSpacing;
                end
              else if theItem.NewLine or (nextLineY + theControl.Height + BorderSpacing > FContainer.Height) or (lineControlCount mod LineMaxCount = 0) then
                begin
                  nextLineX := nextLineX - currMaxLineMark - LeftSpacing;
                  nextLineY := BorderSpacing;
                  currMaxLineMark := 0;
                  lineControlCount := 0;
                end;
              x := nextLineX - theControl.Width;
              y := nextLineY;
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

{ TCMGridLayout.TCMGridLayoutItem }

constructor TCMGridLayout.TCMGridLayoutItem.Create;
begin
  inherited Create;
  FCol := -1;
  FRow := -1;
  FSizeStored := False;
  FControlWidth := -1;
  FControlHeight := -1;
end;

{ TCMGridLayout }

function TCMGridLayout.getColCount: Integer;
begin
  Result := Length(FColWidths);
end;

function TCMGridLayout.getRowCount: Integer;
begin
  Result := Length(FRowHeights);
end;

function TCMGridLayout.getColWidths(ACol: Integer): Integer;
begin
  Result := FColWidths[ACol];
end;

function TCMGridLayout.getRowHeights(ARow: Integer): Integer;
begin
  Result := FRowHeights[ARow];
end;

procedure TCMGridLayout.setColCount(AValue: Integer);
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

procedure TCMGridLayout.setRowCount(AValue: Integer);
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

procedure TCMGridLayout.setColWidths(ACol: Integer; AValue: Integer);
begin
  if FColWidths[ACol] = AValue then
    Exit;
  FColWidths[ACol] := AValue;
  ReLayout;
end;

procedure TCMGridLayout.setRowHeights(ARow: Integer; AValue: Integer);
begin
  if FRowHeights[ARow] = AValue then
    Exit;
  FRowHeights[ARow] := AValue;
  ReLayout;
end;

procedure TCMGridLayout.setAlignAtGrid(AValue: Boolean);
begin
  if FAlignAtGrid = AValue then
    Exit;
  FAlignAtGrid := AValue;
  ReLayout;
end;

constructor TCMGridLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemClass := TCMGridLayoutItem;
  FColRowRecList := TFPHashObjectList.Create(False);
  FDefaultColWidth := 100;
  FDefaultRowHeight := 100;
  ColCount := 0;
  RowCount := 0;
end;

constructor TCMGridLayout.Create(AOwner: TComponent; ACols, ARows: Integer);
begin
  Self.Create(AOwner);
  ColCount := ACols;
  RowCount := ARows;
end;

destructor TCMGridLayout.Destroy;
begin
  FColRowRecList.Free;
  inherited Destroy;
end;

procedure TCMGridLayout.SetColWidth(AValue: Integer);
var
  i: Integer;
begin
  DefaultColWidth := AValue;
  for i:=Low(FColWidths) to High(FColWidths) do
    FColWidths[i] := AValue;
  ReLayout;
end;

procedure TCMGridLayout.SetRowHeight(AValue: Integer);
var
  i: Integer;
begin
  DefaultRowHeight := AValue;
  for i:=Low(FRowHeights) to High(FRowHeights) do
    FRowHeights[i] := AValue;
  ReLayout;
end;

function TCMGridLayout.PutLayoutControl(AControl: TControl): TCMLayoutItem;
var
  i, j: Integer;
  str: string;
begin
  Result := nil;
  if Assigned(AControl) then
    begin
      Result := Self.CreateItem(AControl);
      if Assigned(Result) then
        begin
          for i:=0 to RowCount-1 do
            begin
              for j:=0 to ColCount-1 do
                begin
                  str := ColRowRecStr(j, i);
                  if FColRowRecList.FindIndexOf(str) < 0 then
                    begin
                      TCMGridLayoutItem(Result).FCol := j;
                      TCMGridLayoutItem(Result).FRow := i;
                      if Self.AddItem(Result) >= 0 then
                        FColRowRecList.Add(str, Result);
                      Exit;
                    end;
                end;
            end;
          Self.AddItem(Result);
        end;
    end;
end;

function TCMGridLayout.PutLayoutControl(AControl: TControl; ACol, ARow: Integer): TCMLayoutItem;
var
  str: string;
begin
  Result := nil;
  if (ACol >= 0) and (ACol < ColCount) then
    if (ARow >= 0) and (ARow < RowCount) then
      if Assigned(AControl) then
        begin
          str := ColRowRecStr(ACol, ARow);
          if FColRowRecList.FindIndexOf(str) < 0 then
            begin
              Result := TCMGridLayoutItem(Self.CreateItem(AControl));
              if Assigned(Result) then
                begin
                  TCMGridLayoutItem(Result).FCol := ACol;
                  TCMGridLayoutItem(Result).FRow := ARow;
                  if Self.AddItem(Result) >= 0 then
                    FColRowRecList.Add(str, Result);
                end;
            end;
        end;
end;

procedure TCMGridLayout.RemoveLayoutControl(AControl: TControl);
var
  item: TCMGridLayoutItem;
  str: string;
  i: Integer;
begin
  item := TCMGridLayoutItem(Self.GetItem(AControl));
  if Assigned(item) then
    str := ColRowRecStr(item.Col, item.Row);
  inherited RemoveLayoutControl(AControl);
  i := FColRowRecList.FindIndexOf(str);
  if i >= 0 then
    FColRowRecList.Delete(i);  //去掉多余的行列检索
end;

function TCMGridLayout.ColRowRecStr(ACol, ARow: Integer): string;
begin
  Result := Format('%d-%d', [ACol, ARow]);
end;

procedure TCMGridLayout.ReLayout;
var
  tabOrderInt: Integer;
  i, tempInt: Integer;
  theItem: TCMGridLayoutItem;
  theControl: TControl;
  lefts, tops: array of Integer;
begin
  if not Assigned(FContainer) then
    Exit;
  if (FContainer is TWinControl) and (not TWinControl(FContainer).Showing) then
    Exit;
  if (ColCount <= 0) or (RowCount <= 0) then
    Exit;
  tabOrderInt := 0;
  //
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
  //
  for i:=0 to Self.Count-1 do
    begin
      theItem := TCMGridLayoutItem(Self.GetItem(i));
      theControl := theItem.Control;
      if Assigned(theControl) then
        begin
          if theControl is TWinControl then
            begin
              TWinControl(theControl).TabOrder := tabOrderInt;
              tabOrderInt := tabOrderInt + 1;
            end;
          if (theItem.Col >= 0) and (theItem.Row >= 0) then
            begin
              if (theItem.Col < ColCount) and (theItem.Row < RowCount) then
                begin
                  theControl.Left := lefts[theItem.Col];
                  theControl.Top := tops[theItem.Row];
                  if AlignAtGrid then
                    begin
                      if not theItem.SizeStored then
                        begin
                          theItem.ControlWidth := theControl.Width;
                          theItem.ControlHeight := theControl.Height;
                          theItem.SizeStored := True;
                        end;
                      theControl.Width := FColWidths[theItem.Col] - LeftSpacing;
                      theControl.Height := FRowHeights[theItem.Row] - TopSpacing;
                    end;
                end
              else
                begin
                  theControl.Left := 0;
                  theControl.Top := 0;
                end;
              //置此，因可能中间更改行列数量
              if not AlignAtGrid then
                begin
                  if theItem.SizeStored then
                    begin
                      theControl.Width := theItem.ControlWidth;
                      theControl.Height := theItem.ControlHeight;
                      theItem.SizeStored := False;
                    end;
                end;
            end;
        end;
    end;
end;

{ TCMGridLayoutMenu }

constructor TCMGridLayoutMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectedBoxPanel := TPanel.Create(Self);
  FSelectedBoxPanel.BevelOuter := bvNone;
  FSelectedBoxPanel.Color := clYellow;
  FSelectedBoxPanel.OnPaint := @SelectedBoxPanelOnPaint;
  FSelectedControl := nil;
  FOnCellSelectEnter := nil;
  FOnCellSelectLeave := nil;
  FSelectBoxWidth := 2;
  SelectBoxSize := 10;
  FSelectBoxSpacing := 2;
  FSelectedColor := clPurple;
  FWhenSelectSetColor := False;
end;

function TCMGridLayoutMenu.getSelectedCol: Integer;
var
  item: TCMGridLayoutItem;
begin
  Result := -1;
  item := TCMGridLayoutItem(Self.GetItem(SelectedControl));
  if Assigned(item) then
    Result := item.Col;
end;

procedure TCMGridLayoutMenu.setSelectedCol(AValue: Integer);
begin
  SelectCell(AValue, SelectedRow);
end;

function TCMGridLayoutMenu.getSelectedRow: Integer;
var
  item: TCMGridLayoutItem;
begin
  Result := -1;
  item := TCMGridLayoutItem(Self.GetItem(SelectedControl));
  if Assigned(item) then
    Result := item.Row;
end;

procedure TCMGridLayoutMenu.setSelectedRow(AValue: Integer);
begin
  SelectCell(SelectedCol, AValue);
end;

function TCMGridLayoutMenu.getSelectedIndex: Integer;
begin
  Result := Self.GetItemIndex(SelectedControl);
end;

procedure TCMGridLayoutMenu.setSelectedIndex(AValue: Integer);
var
  item: TCMLayoutItem;
begin
  if (AValue >= 0) and (AValue < Self.Count) then
    begin
      item := Self.GetItem(AValue);
      if Assigned(item) then
        begin
          SelectedControl := item.Control;
          Exit;
        end;
    end;
  SelectedControl := nil;
end;

//选择 item 以此为基准(设置col、row、index)
procedure TCMGridLayoutMenu.setSelectedControl(AValue: TControl);
var
  item: TCMGridLayoutItem;
begin
  if FSelectedControl = AValue then
    Exit;
  try
    //设置为先前状态
    if Assigned(FSelectedControl) then
      begin
        SetToOriginal(FSelectedControl);
        item := TCMGridLayoutItem(Self.GetItem(FSelectedControl));
        if Assigned(FOnCellSelectLeave) then
          FOnCellSelectLeave(item.Control, item.Col, item.Row);
      end;
  finally
    if Assigned(AValue) and (GetItemIndex(AValue) >= 0) then
      begin
        FSelectedControl := AValue;
        SetToSelected(FSelectedControl);
        item := TCMGridLayoutItem(Self.GetItem(FSelectedControl));
        if Assigned(FOnCellSelectEnter) then
          FOnCellSelectEnter(item.Control, item.Col, item.Row);
      end
    else
      begin
        FSelectedControl := nil;
      end;
  end;
end;

function TCMGridLayoutMenu.getSelectBoxColor: TColor;
begin
  Result := FSelectedBoxPanel.Color;
end;

procedure TCMGridLayoutMenu.setSelectBoxColor(AValue: TColor);
begin
  FSelectedBoxPanel.Color := AValue;
end;

procedure TCMGridLayoutMenu.setSelectBoxWidth(AValue: Integer);
begin
  if FSelectBoxWidth = AValue then
    Exit;
  FSelectBoxWidth := AValue;
  if SelectBoxSize < AValue then
    SelectBoxSize := AValue;
  reSelectedBoxPanel;
end;

procedure TCMGridLayoutMenu.setSelectBoxSize(AValue: Integer);
begin
  if FSelectBoxSize = AValue then
    Exit;
  if SelectBoxWidth > AValue then
    Exit;
  FSelectBoxSize := AValue;
  reSelectedBoxPanel;
  FSelectedBoxPanel.Repaint;
end;

procedure TCMGridLayoutMenu.setSelectBoxSpacing(AValue: Integer);
begin
  if FSelectBoxSpacing = AValue then
    Exit;
  FSelectBoxSpacing := AValue;
  reSelectedBoxPanel;
end;

procedure TCMGridLayoutMenu.SelectedBoxPanelOnPaint(Sender: TObject);
var
  p: TPanel;
begin
  p := TPanel(Sender);
  if Assigned(Container) then
    begin
      p.Canvas.Brush.Color := Container.Color;
      if SelectBoxSpacing > 0 then
        p.Canvas.FillRect(SelectBoxWidth, SelectBoxWidth, p.Width-SelectBoxWidth, p.Height-SelectBoxWidth);
      if SelectBoxSize > 0 then
        begin
          p.Canvas.FillRect(0, SelectBoxSize, p.Width, p.Height-SelectBoxSize);
          p.Canvas.FillRect(SelectBoxSize, 0, p.Width-SelectBoxSize, p.Height);
        end;
    end;
end;

procedure TCMGridLayoutMenu.setSelectedColor(AValue: TColor);
begin
  if FSelectedColor = AValue then
    Exit;
  FSelectedColor := AValue;
  if Assigned(SelectedControl) then
    SelectedControl.Color := SelectedColor;
end;

procedure TCMGridLayoutMenu.setWhenSelectSetColor(AValue: Boolean);
begin
  if FWhenSelectSetColor = AValue then
    Exit;
  FWhenSelectSetColor := AValue;
  reSelectedColor;
end;

procedure TCMGridLayoutMenu.reSelectedBoxPanel;
var
  i: Integer;
begin
  if Assigned(SelectedControl) then
    begin
      FSelectedBoxPanel.Parent := SelectedControl.Parent;
      i := SelectBoxWidth + SelectBoxSpacing;
      FSelectedBoxPanel.Left := SelectedControl.Left -i;
      FSelectedBoxPanel.Top := SelectedControl.Top - i;
      FSelectedBoxPanel.Width := SelectedControl.Width + i + i;
      FSelectedBoxPanel.Height := SelectedControl.Height + i + i;
    end;
end;

procedure TCMGridLayoutMenu.reSelectedColor;
var
  item: TCMGridLayoutMenuItem;
begin
  if Assigned(SelectedControl) then
    begin
      if WhenSelectSetColor then
        SelectedControl.Color := SelectedColor
      else
        begin
          item := TCMGridLayoutMenuItem(Self.GetItem(SelectedControl));
          SelectedControl.Color := item.ControlColor;
        end;
    end;
end;

function TCMGridLayoutMenu.AddItem(AItem: TCMLayoutItem): Integer;
begin
  Result := inherited AddItem(AItem);
  if Result < 0 then
    Exit;
  TCMGridLayoutMenuItem(AItem).ControlColor := AItem.Control.Color;
end;

function TCMGridLayoutMenu.SelectCell(ACol, ARow: Integer): Boolean;
var
  str: string;
  i: Integer;
  item: TCMLayoutItem;
begin
  Result := False;
  str := ColRowRecStr(ACol, ARow);
  i := FColRowRecList.FindIndexOf(str);
  if i >= 0 then
    begin
      item := TCMLayoutItem(FColRowRecList[i]);
      if Assigned(item) then
        begin
          SelectedControl := item.Control;
          Result := True;
          Exit;
        end;
    end;
  SelectedControl := nil;
end;

procedure TCMGridLayoutMenu.SetToSelected(AControl: TControl);
begin
  if WhenSelectSetColor then
    AControl.Color := SelectedColor;
  reSelectedBoxPanel;
  FSelectedBoxPanel.Visible := True;
  FSelectedBoxPanel.SendToBack;
end;

procedure TCMGridLayoutMenu.SetToOriginal(AControl: TControl);
var
  item: TCMGridLayoutMenuItem;
begin
  if WhenSelectSetColor then
    begin
      item := TCMGridLayoutMenuItem(Self.GetItem(AControl));
      if Assigned(item) then
        item.Control.Color := item.ControlColor;
    end;
  FSelectedBoxPanel.Visible := False;
end;

procedure TCMGridLayoutMenu.RemoveLayoutControl(AControl: TControl);
begin
  if SelectedControl = AControl then
    SelectedControl := nil;
  inherited RemoveLayoutControl(AControl);
end;

procedure TCMGridLayoutMenu.ReLayout;
begin
  inherited ReLayout;
  reSelectedBoxPanel;
end;

procedure TCMGridLayoutMenu.Up;
var
  item: TCMGridLayoutItem;
  i: Integer;
begin
  item := TCMGridLayoutItem(Self.GetItem(SelectedControl));
  if Assigned(item) then
    begin
      for i:=item.Row - 1 downto 0 do
        if SelectCell(item.Col, i) then
          Exit;
      SelectedControl := item.Control;
    end;
end;

procedure TCMGridLayoutMenu.Down;
var
  item: TCMGridLayoutItem;
  i: Integer;
begin
  item := TCMGridLayoutItem(Self.GetItem(SelectedControl));
  if Assigned(item) then
    begin
      for i:=item.Row + 1 to Self.RowCount-1 do
        if SelectCell(item.Col, i) then
          Exit;
      SelectedControl := item.Control;
    end;
end;

procedure TCMGridLayoutMenu.Left;
var
  item: TCMGridLayoutItem;
  i, j: Integer;
begin
  item := TCMGridLayoutItem(Self.GetItem(SelectedControl));
  if Assigned(item) then
    begin
      if (item.Col=0) and (item.Row=0) then
        Exit;
      for i:=item.Col-1 downto 0 do
        begin
          if SelectCell(i, item.Row) then
            Exit;
        end;
      for j:= item.Row-1 downto 0 do
        for i:=Self.ColCount-1 downto 0 do
          begin
            if SelectCell(i, j) then
              Exit;
          end;
      SelectedControl := item.Control;
    end;
end;

procedure TCMGridLayoutMenu.Right;
var
  item: TCMGridLayoutItem;
  i, j: Integer;
begin
  item := TCMGridLayoutItem(Self.GetItem(SelectedControl));
  if Assigned(item) then
    begin
      for i:=item.Col+1 to Self.ColCount-1 do
        begin
          if SelectCell(i, item.Row) then
            Exit;
        end;
      for j:= item.Row+1 to Self.RowCount-1 do
        for i:=0 to Self.ColCount-1 do
          begin
            if SelectCell(i, j) then
              Exit;
          end;
      SelectedControl := item.Control;
    end;
end;

{ TCMGridLayoutHostingMenu.TCMGridLayoutHostingMenuItem }

constructor TCMGridLayoutHostingMenu.TCMGridLayoutHostingMenuItem.Create;
begin
  inherited Create;
  FControlOnClick := nil;
end;

{ TCMGridLayoutHostingMenu }

constructor TCMGridLayoutHostingMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemClass := TCMGridLayoutHostingMenuItem;
end;

function TCMGridLayoutHostingMenu.AddItem(AItem: TCMLayoutItem): Integer;
begin
  Result := inherited AddItem(AItem);
  if Result < 0 then
    Exit;
  TCMGridLayoutHostingMenuItem(AItem).ControlOnClick := AItem.Control.OnClick;
  AItem.Control.OnClick := @MenuCellClick;
  //默认选一个
  if Result >= 0 then
    if SelectedIndex < 0 then
      SelectedIndex := 0;
end;

procedure TCMGridLayoutHostingMenu.MenuCellClick(Sender: TObject);
var
  item: TCMGridLayoutHostingMenuItem;
begin
  Self.SelectedControl := TControl(Sender);
  item := TCMGridLayoutHostingMenuItem(Self.GetItem(TControl(Sender)));
  if Assigned(item.ControlOnClick) then
    item.ControlOnClick(item.Control);
end;

procedure TCMGridLayoutHostingMenu.DelClick(AControl: TControl);
var
  item: TCMGridLayoutHostingMenuItem;
  m: TMethod;
  dcEvent: TNotifyEvent;
begin
  item := TCMGridLayoutHostingMenuItem(Self.GetItem(AControl));
  if Assigned(item) then
    begin
      Self.SelectedControl := item.Control;
      m := TypInfo.GetMethodProp(item.Control, 'OnDblClick');
      if m.Code <> nil then
        begin
          dcEvent := TNotifyEvent(m);
          dcEvent(item.Control);
        end;
    end;
end;

procedure TCMGridLayoutHostingMenu.DelClick;
begin
  DelClick(SelectedControl);
end;



{-------------------------------------------------------------------------------------------------}

function GetKeyCodeCHName(const AKeyCode: Word; IsNeedCode: Boolean=True): string;
begin
  case AKeyCode of
  VK_LBUTTON: Result := '鼠标左键';     //	01	1	鼠标的左键
  VK_RBUTTON: Result := '鼠标右键';     //	02	2	鼠标的右键
  VK_CANCEL: Result := 'Ctrl+Break';    //	03	3	Ctrl+Break(通常不需要处理)
  VK_MBUTTON: Result := '鼠标中键';     //	04	4	鼠标的中键（三按键鼠标)
  VK_BACK: Result := 'Backspace键';     //	08	8	Backspace键
  VK_TAB: Result := 'Tab键';            //	09	9	Tab键
  VK_CLEAR: Result := 'Clear键（Num Lock关闭时的数字键盘5）'; //	0C	12	Clear键（Num Lock关闭时的数字键盘5）
  VK_RETURN: Result := 'Enter键';       //	0D	13	Enter键
  VK_SHIFT: Result := 'Shift键';        //	10	16	Shift键
  VK_CONTROL: Result := 'Ctrl键';       //	11	17	Ctrl键
  VK_MENU: Result := 'Alt键';           //	12	18	Alt键
  VK_PAUSE: Result := 'Pause键';        //	13	19	Pause键
  VK_CAPITAL: Result := 'CapsLock键';   //	14	20	Caps Lock键
  VK_ESCAPE: Result := 'Esc键';         //	1B	27	Ese键
  VK_SPACE: Result := 'Spacebar键';     //	20	32	Spacebar键
  VK_PRIOR: Result := 'Page Up键';      //	21	33	Page Up键
  VK_NEXT: Result := 'Page Down键';     //	22	34	Page Domw键
  VK_END: Result := 'End键';            //	23	35	End键
  VK_HOME: Result := 'Home键';          //	24	36	Home键
  VK_LEFT: Result := 'LEFT ARROW 键(←)'; //	25	37	LEFT ARROW 键(←)
  VK_UP: Result := 'UP ARROW键(↑)';     //	26	38	UP ARROW键(↑)
  VK_RIGHT: Result := 'RIGHT ARROW键(→)'; //	27	39	RIGHT ARROW键(→)
  VK_DOWN: Result := 'DOWN ARROW键(↓)'; //	28	40	DOWN ARROW键(↓)
  VK_Select: Result := 'Select键';      //	29	41	Select键
  VK_PRINT: Result := 'PrintScreen key'; //	2A	42
  VK_EXECUTE: Result := 'EXECUTE键';    //	2B	43	EXECUTE键
  VK_SNAPSHOT: Result := 'Print Screen键（抓屏）'; //	2C	44	Print Screen键（抓屏）
  VK_Insert: Result := 'Ins键(Num Lock关闭时的数字键盘0)'; //	2D	45	Ins键(Num Lock关闭时的数字键盘0)
  VK_Delete: Result := 'Del键(Num Lock关闭时的数字键盘.)'; //	2E	46	Del键(Num Lock关闭时的数字键盘.)
  VK_HELP: Result := 'Help键';          //	2F	47	Help键
  VK_0: Result := '0键'; //	30	48	0键
  VK_1: Result := '1键'; //	31	49	1键
  VK_2: Result := '2键'; //	32	50	2键
  VK_3: Result := '3键'; //	33	51	3键
  VK_4: Result := '4键'; //	34	52	4键
  VK_5: Result := '5键'; //	35	53	5键
  VK_6: Result := '6键'; //	36	54	6键
  VK_7: Result := '7键'; //	37	55	7键
  VK_8: Result := '8键'; //	38	56	8键
  VK_9: Result := '9键'; //	39	57	9键
  VK_A: Result := 'A键'; //	41	65	A键
  VK_B: Result := 'B键'; //	42	66	B键
  VK_C: Result := 'C键'; //	43	67	C键
  VK_D: Result := 'D键'; //	44	68	D键
  VK_E: Result := 'E键'; //	45	69	E键
  VK_F: Result := 'F键'; //	46	70	F键
  VK_G: Result := 'G键'; //	47	71	G键
  VK_H: Result := 'H键'; //	48	72	H键
  VK_I: Result := 'I键'; //	49	73	I键
  VK_J: Result := 'J键'; //	4A	74	J键
  VK_K: Result := 'K键'; //	4B	75	K键
  VK_L: Result := 'L键'; //	4C	76	L键
  VK_M: Result := 'M键'; //	4D	77	M键
  VK_N: Result := 'N键'; //	4E	78	N键
  VK_O: Result := 'O键'; //	4F	79	O键
  VK_P: Result := 'P键'; //	50	80	P键
  VK_Q: Result := 'Q键'; //	51	81	Q键
  VK_R: Result := 'R键'; //	52	82	R键
  VK_S: Result := 'S键'; //	53	83	S键
  VK_T: Result := 'T键'; //	54	84	T键
  VK_U: Result := 'U键'; //	55	85	U键
  VK_V: Result := 'V键'; //	56	86	V键
  VK_W: Result := 'W键'; //	57	87	W键
  VK_X: Result := 'X键'; //	58	88	X键
  VK_Y: Result := 'Y键'; //	59	89	Y键
  VK_Z: Result := 'Z键'; //	5A	90	Z键
  VK_NUMPAD0: Result := '数字键盘0键'; //	60	96	数字键0键
  VK_NUMPAD1: Result := '数字键盘1键'; //	61	97	数字键1键
  VK_NUMPAD2: Result := '数字键盘2键'; //	62	98	数字键2键
  VK_NUMPAD3: Result := '数字键盘3键'; //	62	99	数字键3键
  VK_NUMPAD4: Result := '数字键盘4键'; //	64	100	数字键4键
  VK_NUMPAD5: Result := '数字键盘5键'; //	65	101	数字键5键
  VK_NUMPAD6: Result := '数字键盘6键'; //	66	102	数字键6键
  VK_NUMPAD7: Result := '数字键盘7键'; //	67	103	数字键7键
  VK_NUMPAD8: Result := '数字键盘8键'; //	68	104	数字键8键
  VK_NUMPAD9: Result := '数字键盘9键'; //	69	105	数字键9键
  VK_MULTIPLY: Result := '数字键盘*键'; //	6A	106	数字键盘上的*键
  VK_ADD: Result := '数字键盘+键';      //	6B	107	数字键盘上的+键
  VK_SEPARATOR: Result := 'Separator键';    //	6C	108	Separator键
  VK_SUBTRACT: Result := '数字键盘-键'; //	6D	109	数字键盘上的-键
  VK_DECIMAL: Result := '数字键盘.键';  //	6E	110	数字键盘上的.键
  VK_DIVIDE: Result := '数字键盘/键';   //	6F	111	数字键盘上的/键
  VK_F1: Result := 'F1键';   //	70	112	F1键
  VK_F2: Result := 'F2键';   //	71	113	F2键
  VK_F3: Result := 'F3键';   //	72	114	F3键
  VK_F4: Result := 'F4键';   //	73	115	F4键
  VK_F5: Result := 'F5键';   //	74	116	F5键
  VK_F6: Result := 'F6键';   //	75	117	F6键
  VK_F7: Result := 'F7键';   //	76	118	F7键
  VK_F8: Result := 'F8键';   //	77	119	F8键
  VK_F9: Result := 'F9键';   //	78	120	F9键
  VK_F10: Result := 'F10键'; //	79	121	F10键
  VK_F11: Result := 'F11键'; //	7A	122	F11键
  VK_F12: Result := 'F12键'; //	7B	123	F12键
  VK_NUMLOCK: Result := 'Num Lock键';  //	90	144	Num Lock 键
  VK_SCROLL: Result := 'Scroll Lock键'; //	91	145	Scroll Lock键
  //上面没有提到的：（都在大键盘）
  VK_LWIN: Result := '左win键';   //	 	91	左win键
  VK_RWIN: Result := '右win键';   //	 	92	右win键
  VK_APPS: Result := 'PopUp键';   //	 	93	右Ctrl左边键，点击相当于点击鼠标右键，会弹出快捷菜单
  else begin
         if not IsNeedCode then
           Result := '键值' + IntToStr(AKeyCode) + '键';
       end;
  end;
  if IsNeedCode then
    Result := Result + '(KeyCode:' + IntToStr(AKeyCode) + ')';
end;

end.

