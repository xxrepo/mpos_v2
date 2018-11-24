

procedure TNavNode.SetNodesStyle(nsc: INodesStyleCfg);
begin
  //复制至实现分配对象FAssignedNodesStyleCfg，是因为只用一个布局管理，当切换时默认可能在上一次使用时是指定的。
  FNodesStyleCfg := nsc;
  if nsc.GetColCount > 0 then
    FAssignedNodesStyleCfg.ColCount := nsc.GetColCount
  else
    FAssignedNodesStyleCfg.ColCount := DEF_GridColCount;
  if nsc.GetRowCount > 0 then
    FAssignedNodesStyleCfg.RowCount := nsc.GetRowCount
  else
    FAssignedNodesStyleCfg.RowCount := DEF_GridRowCount;

  if nsc.GetColWidth > 0 then
    FAssignedNodesStyleCfg.ColWidth := nsc.GetColWidth
  else
    FAssignedNodesStyleCfg.ColWidth := INIT_GridColWidth;

  if nsc.GetRowHeight > 0 then
    FAssignedNodesStyleCfg.RowHeight := nsc.GetRowHeight
  else
    FAssignedNodesStyleCfg.RowHeight := INIT_GridRowHeight;
  FAssignedNodesStyleCfg.Align := nsc.GetAlign;
  if nsc.GetLeftSpacing > 0 then
    FAssignedNodesStyleCfg.LeftSpacing := nsc.GetLeftSpacing
  else
    FAssignedNodesStyleCfg.LeftSpacing := DEF_LeftSpacing;
  if nsc.GetTopSpacing > 0 then
    FAssignedNodesStyleCfg.TopSpacing := nsc.GetTopSpacing
  else
    FAssignedNodesStyleCfg.TopSpacing := DEF_Toppacing;
end;

function TNavigatorFrame.LoadNavCfg(p: ICMParameter): INavNodeCfg;
var
  nc: TNavNodeCfg;
begin
  Result := nil;
  nc := TNavNodeCfg.Create(p.Get('name').AsString, p.Get('caption').AsString);
  Result := nc;
  if (not p.Get('col').IsNull) and (not p.Get('row').IsNull) then
    nc.SetPos(p.Get('col').AsInteger, p.Get('row').AsInteger);
  if not p.Get('width').IsNull then
    nc.Width := p.Get('width').AsInteger;
  if not p.Get('height').IsNull then
    nc.Height := p.Get('height').AsInteger;
  if not p.Get('color').IsNull then
    nc.Color := p.Get('color').AsInteger;
end;

function TNavigatorFrame.LoadNodesStyleCfg(p: ICMParameter): INodesStyleCfg;
var
  nsc: TNodesStyleCfg;
begin
  Result := nil;
  nsc := TNodesStyleCfg.Create;
  Result := nsc;
  if not p.Get('colCount').IsNull then
    nsc.ColCount := p.Get('colCount').AsInteger;
  if not p.Get('rowCount').IsNull then
    nsc.RowCount := p.Get('rowCount').AsInteger;
  if not p.Get('colWidth').IsNull then
    nsc.ColWidth := p.Get('colWidth').AsInteger;
  if not p.Get('rowHeight').IsNull then
    nsc.RowHeight := p.Get('rowHeight').AsInteger;
  nsc.Align := p.Get('align').AsBoolean;
  if not p.Get('leftSpacing').IsNull then
    nsc.LeftSpacing := p.Get('leftSpacing').AsInteger;
  if not p.Get('topSpacing').IsNull then
    nsc.TopSpacing := p.Get('topSpacing').AsInteger;
end;

procedure TNavigatorFrame.SetLayoutStyle(nsc: TNodesStyleCfg);
begin
  if not Assigned(nsc) then
    Exit;
  if nsc.GetColCount > -1 then
    FGridLayout.ColCount := nsc.GetColCount;
  if nsc.GetRowCount > -1 then
    FGridLayout.RowCount := nsc.GetRowCount;
  if nsc.GetColWidth > -1 then
    FGridLayout.SetColsWidth(nsc.GetColWidth);
  if nsc.GetRowHeight > -1 then
    FGridLayout.SetRowsHeight(nsc.GetRowHeight);
  FGridLayout.AlignAtGrid := nsc.GetAlign;
  if nsc.GetLeftSpacing > -1 then
    FGridLayout.LeftSpacing := nsc.GetLeftSpacing;
  if nsc.GetTopSpacing > -1 then
    FGridLayout.TopSpacing := nsc.GetTopSpacing;
end;

procedure TNavigatorFrame.LoadConfig;
var
  navParam: ICMParameter;
  nodesStyleCfg: INodesStyleCfg;
  i: Integer;
  procedure load(const parentName: string; nodeParam: ICMParameter);
  var
    nodeCfg: INavNodeCfg;
    nav: TNavNode;
    j: Integer;
  begin
    if nodeParam.Name <> 'node' then
      Exit;
    nodeCfg := Self.LoadNavCfg(nodeParam);
    Self.AddNode(parentName, nodeCfg);
    if not nodeParam.Get('nodes').IsNull then
      begin
        nav := TNavNode(FNavNodeList.Find(nodeCfg.GetName));
        if Assigned(nav) then
          begin
            nodesStyleCfg := Self.LoadNodesStyleCfg(nodeParam.Get('nodes'));
            nav.SetNodesStyle(nodesStyleCfg);
          end;
        for j:=0 to nodeParam.Get('nodes').ItemCount-1 do
          load(nodeCfg.GetName, nodeParam.Get('nodes').GetItem(j));
      end;
  end;
begin
  navParam := AppSystem.GetParameter.Get('navigator.nodes');
  if not navParam.IsNull then
    begin
      nodesStyleCfg := Self.LoadNodesStyleCfg(navParam);
      FRootNav.SetNodesStyle(nodesStyleCfg);
      Self.SetLayoutStyle(FRootNav.FAssignedNodesStyleCfg);
      //
      for i:=0 to navParam.ItemCount-1 do
        load('', navParam.GetItem(i));
    end;
end;
