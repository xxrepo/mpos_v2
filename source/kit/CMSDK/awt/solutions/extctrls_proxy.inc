

{ TProxyDateTimePickerPeer }

procedure TProxyDateTimePickerPeer.RegisterControlEvents;
begin
  inherited RegisterControlEvents;
  GetDelegate.OnDblClick := @ControlDblClickEvent;
end;

procedure TProxyDateTimePickerPeer.RegisterMouseEvents;
begin
  inherited RegisterMouseEvents;
  GetDelegate.OnMouseDown := @MouseDownEvent;
  GetDelegate.OnMouseUp := @MouseUpEvent;
  GetDelegate.OnMouseEnter := @MouseEnterEvent;
  GetDelegate.OnMouseLeave := @MouseLeaveEvent;
  GetDelegate.OnMouseMove := @MouseMoveEvent;
  GetDelegate.OnMouseWheel:= @MouseWheelEvent;
end;

constructor TProxyDateTimePickerPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FDelegateObj := TDateTimePicker.Create(AOwner);
  GetDelegate.Kind := dtkDateTime;
  GetDelegate.DateMode := dmUpDown;
end;

function TProxyDateTimePickerPeer.GetDelegate: TDateTimePicker;
begin
  Result := TDateTimePicker(FDelegateObj);
end;

function TProxyDateTimePickerPeer.GetParentColor: Boolean;
begin
  Result := GetDelegate.ParentColor;
end;

function TProxyDateTimePickerPeer.GetParentFont: Boolean;
begin
  Result := GetDelegate.ParentFont;
end;

procedure TProxyDateTimePickerPeer.SetParentColor(AValue: Boolean);
begin
  GetDelegate.ParentColor := AValue;
end;

procedure TProxyDateTimePickerPeer.SetParentFont(AValue: Boolean);
begin
  GetDelegate.ParentFont := AValue;
end;

function TProxyDateTimePickerPeer.GetDate: TDate;
begin
  Result := GetDelegate.Date;
end;

function TProxyDateTimePickerPeer.GetDateTime: TDateTime;
begin
  Result := GetDelegate.DateTime;
end;

function TProxyDateTimePickerPeer.GetMaxDate: TDate;
begin
  Result := GetDelegate.MaxDate;
end;

function TProxyDateTimePickerPeer.GetMinDate: TDate;
begin
  Result := GetDelegate.MinDate;
end;

function TProxyDateTimePickerPeer.GetTime: TTime;
begin
  Result := GetDelegate.Time;
end;

procedure TProxyDateTimePickerPeer.SetDate(AValue: TDate);
begin
  GetDelegate.Date := AValue;
end;

procedure TProxyDateTimePickerPeer.SetDateTime(AValue: TDateTime);
begin
  GetDelegate.DateTime := AValue;
end;

procedure TProxyDateTimePickerPeer.SetMaxDate(AValue: TDate);
begin
  GetDelegate.MaxDate := AValue;
end;

procedure TProxyDateTimePickerPeer.SetMinDate(AValue: TDate);
begin
  GetDelegate.MinDate := AValue;
end;

procedure TProxyDateTimePickerPeer.SetTime(AValue: TTime);
begin
  GetDelegate.Time := AValue;
end;

{ TProxyCustomGridPeer }

function TProxyCustomGridPeer.GetDelegate: TCustomGrid;
begin
  Result := TCustomGrid(FDelegateObj);
end;

procedure TProxyCustomGridPeer.BeginUpdate;
begin
  GetDelegate.BeginUpdate;
end;

function TProxyCustomGridPeer.CellRect(ACol, ARow: Integer): TRect;
begin
  Result := GetDelegate.CellRect(ACol, ARow);
end;

procedure TProxyCustomGridPeer.Clear;
begin
  GetDelegate.Clear;
end;

procedure TProxyCustomGridPeer.EndUpdate(ARefresh: Boolean);
begin
  GetDelegate.EndUpdate(ARefresh);
end;

procedure TProxyCustomGridPeer.InvalidateCell(ACol, ARow: Integer);
begin
  GetDelegate.InvalidateCell(ACol, ARow);
end;

procedure TProxyCustomGridPeer.InvalidateCol(ACol: Integer);
begin
  GetDelegate.InvalidateCol(ACol);
end;

procedure TProxyCustomGridPeer.InvalidateRow(ARow: Integer);
begin
  GetDelegate.InvalidateRow(ARow);
end;

{ TProxyCustomDrawGridPeer }

procedure TProxyCustomDrawGridPeer.RegisterControlEvents;
begin
  inherited RegisterControlEvents;
  GetDelegate.OnDblClick := @ControlDblClickEvent;
end;

procedure TProxyCustomDrawGridPeer.CheckCustomDrawGridControlEvents;
begin
  if GetDelegate.OnDrawCell <> @CustomDrawGridDrawCellEvent then
    GetDelegate.OnDrawCell := @CustomDrawGridDrawCellEvent;
  if GetDelegate.OnSelection <> @CustomDrawGridSelectionEvent then
    GetDelegate.OnSelection := @CustomDrawGridSelectionEvent;
  if GetDelegate.OnSelectCell <> @CustomDrawGridSelectCellEvent then
    GetDelegate.OnSelectCell := @CustomDrawGridSelectCellEvent;
end;

procedure TProxyCustomDrawGridPeer.RegisterMouseEvents;
begin
  inherited RegisterMouseEvents;
  GetDelegate.OnMouseDown := @MouseDownEvent;
  GetDelegate.OnMouseUp := @MouseUpEvent;
  GetDelegate.OnMouseEnter := @MouseEnterEvent;
  GetDelegate.OnMouseLeave := @MouseLeaveEvent;
  GetDelegate.OnMouseMove := @MouseMoveEvent;
  GetDelegate.OnMouseWheel:= @MouseWheelEvent;
end;

function TProxyCustomDrawGridPeer.GetDelegate: TCustomDrawGrid;
begin
  Result := TCustomDrawGrid(FDelegateObj);
end;

procedure TProxyCustomDrawGridPeer.CustomDrawGridDrawCellEvent(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  i: Integer;
  dge: IGridEvent;
  dgl: IDrawGridListener;
begin
  if Assigned(FControlListenerList) then
    begin
      dge := TGridEvent.Create(Sender, TACustomGrid(Self.FTargetObj), aCol, aRow);
      for i:=0 to FControlListenerList.Count-1 do
        if Supports(FControlListenerList[i], IDrawGridListener, dgl) then
          dgl.GridDrawCell(dge);
    end;
end;

procedure TProxyCustomDrawGridPeer.CustomDrawGridSelectionEvent(Sender: TObject; aCol, aRow: Integer);
var
  i: Integer;
  dge: IGridEvent;
  dgl: IDrawGridListener;
begin
  if Assigned(FControlListenerList) then
    begin
      dge := TGridEvent.Create(Sender, TACustomGrid(Self.FTargetObj), aCol, aRow);
      for i:=0 to FControlListenerList.Count-1 do
        if Supports(FControlListenerList[i], IDrawGridListener, dgl) then
          dgl.GridSelection(dge);
    end;
end;

procedure TProxyCustomDrawGridPeer.CustomDrawGridSelectCellEvent(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var
  i: Integer;
  dge: IGridEvent;
  dgl: IDrawGridListener;
begin
  if Assigned(FControlListenerList) then
    begin
      dge := TGridEvent.Create(Sender, TACustomGrid(Self.FTargetObj), aCol, aRow);
      for i:=0 to FControlListenerList.Count-1 do
        if Supports(FControlListenerList[i], IDrawGridListener, dgl) then
          dgl.GridSelectCell(dge);
    end;
end;

function TProxyCustomDrawGridPeer.GetBorderColor: TColor;
begin
  Result := GetDelegate.BorderColor;
end;

function TProxyCustomDrawGridPeer.GetCol: Integer;
begin
  Result := GetDelegate.Col;
end;

function TProxyCustomDrawGridPeer.GetColCount: Integer;
begin
  Result := GetDelegate.ColCount;
end;

function TProxyCustomDrawGridPeer.GetColWidths(ACol: Integer): Integer;
begin
  Result := GetDelegate.ColWidths[ACol];
end;

function TProxyCustomDrawGridPeer.GetDefColWidth: Integer;
begin
  Result := GetDelegate.DefaultColWidth;
end;

function TProxyCustomDrawGridPeer.GetDefRowHeight: Integer;
begin
  Result := GetDelegate.DefaultRowHeight;
end;

function TProxyCustomDrawGridPeer.GetFixedColor: TColor;
begin
  Result := GetDelegate.FixedColor;
end;

function TProxyCustomDrawGridPeer.GetFixedCols: Integer;
begin
  Result := GetDelegate.FixedCols;
end;

function TProxyCustomDrawGridPeer.GetFixedRows: Integer;
begin
  Result := GetDelegate.FixedRows;
end;

function TProxyCustomDrawGridPeer.GetGridBorderStyle: TBorderStyle;
begin
  Result := cm_AWT.TBorderStyle(GetDelegate.BorderStyle);
end;

function TProxyCustomDrawGridPeer.GetOptions: TGridOptions;
begin
  Result := cm_AWT.TGridOptions(GetDelegate.Options);
end;

function TProxyCustomDrawGridPeer.GetRow: Integer;
begin
  Result := GetDelegate.Row;
end;

function TProxyCustomDrawGridPeer.GetRowCount: Integer;
begin
  Result := GetDelegate.RowCount;
end;

function TProxyCustomDrawGridPeer.GetRowHeights(ARow: Integer): Integer;
begin
  Result := GetDelegate.RowHeights[ARow];
end;

function TProxyCustomDrawGridPeer.GetScrollBars: TScrollStyle;
begin
  Result := cm_AWT.TScrollStyle(GetDelegate.ScrollBars);
end;

procedure TProxyCustomDrawGridPeer.SetBorderColor(AValue: TColor);
begin
  GetDelegate.BorderColor := AValue;
end;

procedure TProxyCustomDrawGridPeer.SetCol(AValue: Integer);
begin
  GetDelegate.Col := AValue;
end;

procedure TProxyCustomDrawGridPeer.SetColCount(AValue: Integer);
begin
  GetDelegate.ColCount := AValue;
end;

procedure TProxyCustomDrawGridPeer.SetColWidths(ACol: Integer; AValue: Integer);
begin
  GetDelegate.ColWidths[ACol] := AValue;
end;

procedure TProxyCustomDrawGridPeer.SetDefColWidth(AValue: Integer);
begin
  GetDelegate.DefaultColWidth := AValue;
end;

procedure TProxyCustomDrawGridPeer.SetDefRowHeight(AValue: Integer);
begin
  GetDelegate.DefaultRowHeight := AValue;
end;

procedure TProxyCustomDrawGridPeer.SetFixedcolor(AValue: TColor);
begin
  GetDelegate.Fixedcolor := AValue;
end;

procedure TProxyCustomDrawGridPeer.SetFixedCols(AValue: Integer);
begin
  GetDelegate.FixedCols := AValue;
end;

procedure TProxyCustomDrawGridPeer.SetFixedRows(AValue: Integer);
begin
  GetDelegate.FixedRows := AValue;
end;

procedure TProxyCustomDrawGridPeer.SetGridBorderStyle(AValue: TBorderStyle);
begin
  GetDelegate.BorderStyle := Controls.TBorderStyle(AValue);
end;

procedure TProxyCustomDrawGridPeer.SetOptions(AValue: TGridOptions);
begin
  GetDelegate.Options := Grids.TGridOptions(AValue);
end;

procedure TProxyCustomDrawGridPeer.SetRow(AValue: Integer);
begin
  GetDelegate.Row := AValue;
end;

procedure TProxyCustomDrawGridPeer.SetRowCount(AValue: Integer);
begin
  GetDelegate.RowCount := AValue;
end;

procedure TProxyCustomDrawGridPeer.SetRowHeights(ARow: Integer; AValue: Integer);
begin
  GetDelegate.RowHeights[ARow] := AValue;
end;

procedure TProxyCustomDrawGridPeer.SetScrollBars(AValue: TScrollStyle);
begin
  GetDelegate.ScrollBars := StdCtrls.TScrollStyle(AValue);
end;

procedure TProxyCustomDrawGridPeer.AddDrawGridListener(l: IDrawGridListener);
begin
  CheckCustomDrawGridControlEvents;
  Self.AddCustomControlListener(l);
end;

procedure TProxyCustomDrawGridPeer.RemoveDrawGridListener(l: IDrawGridListener);
begin
  Self.RemoveControlListener(l);
end;

function TProxyCustomDrawGridPeer.GetDrawGridListeners: TDrawGridListenerList;
var
  i: Integer;
begin
  // TODO 线程安全
  Result := TDrawGridListenerList.Create;
  if Assigned(FControlListenerList) then
    for i:=0 to FControlListenerList.Count-1 do
      if Supports(FControlListenerList[i], IDrawGridListener) then
        Result.Add(IDrawGridListener(FControlListenerList[i]));
end;

{ TProxyStringGridPeer }

constructor TProxyStringGridPeer.Create(TheTarget: TAComponent; AOwner: TComponent);
begin
  inherited Create(TheTarget, AOwner);
  FTitleFont := nil;
  FDelegateObj := TStringGrid.Create(AOwner);
end;

destructor TProxyStringGridPeer.Destroy;
begin
  if Assigned(FTitleFont) then
    FTitleFont.Free;
  inherited Destroy;
end;

function TProxyStringGridPeer.GetDelegate: TStringGrid;
begin
  Result := TStringGrid(FDelegateObj);
end;

function TProxyStringGridPeer.GetParentColor: Boolean;
begin
  Result := GetDelegate.ParentColor;
end;

function TProxyStringGridPeer.GetParentFont: Boolean;
begin
  Result := GetDelegate.ParentFont;
end;

procedure TProxyStringGridPeer.SetParentColor(AValue: Boolean);
begin
  GetDelegate.ParentColor := AValue;
end;

procedure TProxyStringGridPeer.SetParentFont(AValue: Boolean);
begin
  GetDelegate.ParentFont := AValue;
end;

function TProxyStringGridPeer.GetTitleFont: TAFont;
var
  fp: IAFontPeer;
begin
  Result := nil;
  if not Assigned(FTitleFont) then
    begin
      fp := TProxyFontPeer.Create(GetDelegate.TitleFont);
      FTitleFont := TAFont.Create(fp);
    end;
  Result := FTitleFont;
end;

procedure TProxyStringGridPeer.SetTitleFont(AValue: TAFont);
begin
  if FFont = AValue then
    Exit;
  if Assigned(FTitleFont) then
    FreeAndNil(FTitleFont);
  FTitleFont := TAFont.Create(AValue.GetPeer);
  if FTitleFont.GetPeer.GetDelegate is TFont then
    GetDelegate.TitleFont := TFont(FTitleFont.GetPeer.GetDelegate);
end;

function TProxyStringGridPeer.GetCells(ACol, ARow: Integer): string;
begin
  Result := GetDelegate.Cells[ACol, ARow];
end;

function TProxyStringGridPeer.GetCols(Index: Integer): TStrings;
begin
  Result := GetDelegate.Cols[Index];
end;

function TProxyStringGridPeer.GetRows(Index: Integer): TStrings;
begin
  Result := GetDelegate.Rows[Index];
end;

procedure TProxyStringGridPeer.SetCells(ACol, ARow: Integer; AValue: string);
begin
  GetDelegate.Cells[ACol, ARow] := AValue;
end;

procedure TProxyStringGridPeer.SetCols(Index: Integer; AValue: TStrings);
begin
  GetDelegate.Cols[Index] := AValue;
end;

procedure TProxyStringGridPeer.SetRows(Index: Integer; AValue: TStrings);
begin
  GetDelegate.Rows[Index] := AValue;
end;

procedure TProxyStringGridPeer.AutoSizeColumn(ACol: Integer);
begin
  GetDelegate.AutoSizeColumn(ACol);
end;

procedure TProxyStringGridPeer.AutoSizeColumns;
begin
  GetDelegate.AutoSizeColumns;
end;
