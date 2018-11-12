
{ TProxyGraphicPeer }

function TProxyGraphicPeer.GetDelegate: TGraphic;
begin
  Result := TGraphic(FDelegateObj);
end;

function TProxyGraphicPeer.GetEmpty: Boolean;
begin
  Result := GetDelegate.Empty;
end;

function TProxyGraphicPeer.GetHeight: Integer;
begin
  Result := GetDelegate.Height;
end;

function TProxyGraphicPeer.GetWidth: Integer;
begin
  Result := GetDelegate.Width;
end;

procedure TProxyGraphicPeer.SetHeight(AValue: Integer);
begin
  GetDelegate.Height := AValue;
end;

procedure TProxyGraphicPeer.SetWidth(AValue: Integer);
begin
  GetDelegate.Width := AValue;
end;

procedure TProxyGraphicPeer.Clear;
begin
  GetDelegate.Clear;
end;

procedure TProxyGraphicPeer.LoadFromFile(const AFileName: string);
begin
  GetDelegate.LoadFromFile(AFileName);
end;

procedure TProxyGraphicPeer.LoadFromStream(AStream: TStream);
begin
  GetDelegate.LoadFromStream(AStream);
end;

procedure TProxyGraphicPeer.SaveToFile(const AFileName: string);
begin
  GetDelegate.SaveToFile(AFileName);
end;

procedure TProxyGraphicPeer.SaveToStream(AStream: TStream);
begin
  GetDelegate.SaveToStream(AStream);
end;

{ TProxyRasterImagePeer }

constructor TProxyRasterImagePeer.Create;
begin
  inherited Create;
  FCanvas := nil;
end;

destructor TProxyRasterImagePeer.Destroy;
begin
  if Assigned(FCanvas) then
    FCanvas.Free;
  inherited Destroy;
end;

function TProxyRasterImagePeer.GetDelegate: TRasterImage;
begin
  Result := TRasterImage(FDelegateObj);
end;

function TProxyRasterImagePeer.GetCanvas: TACanvas;
var
  cp: IACanvasPeer;
begin
  Result := nil;
  if not Assigned(FCanvas) then
    begin
      cp := TProxyCanvasPeer.Create(GetDelegate.Canvas);
      FCanvas := TACanvas.Create(cp);
    end;
  Result := FCanvas;
end;

procedure TProxyRasterImagePeer.FreeImage;
begin
  GetDelegate.FreeImage;
end;

{ TProxyCustomBitmapPeer }

constructor TProxyCustomBitmapPeer.Create(TheDelegate: TCustomBitmap);
begin
  inherited Create;
  FDelegateObj := TheDelegate;
end;

function TProxyCustomBitmapPeer.GetDelegate: TCustomBitmap;
begin
  Result := TCustomBitmap(FDelegateObj);
end;

function TProxyCustomBitmapPeer.GetMonochrome: Boolean;
begin
  Result := GetDelegate.Monochrome;
end;

procedure TProxyCustomBitmapPeer.SetMonochrome(AValue: Boolean);
begin
  GetDelegate.Monochrome := AValue;
end;

procedure TProxyCustomBitmapPeer.SetSize(AWidth, AHeight: Integer);
begin
  GetDelegate.SetSize(AWidth, AHeight);
end;

{ TProxyFontPeer }

constructor TProxyFontPeer.Create;
begin
  inherited Create;
  FIsSelfCreateDelegate := True;
  FDelegateObj := TFont.Create;
end;

constructor TProxyFontPeer.Create(TheDelegate: TFont);
begin
  inherited Create;
  FDelegateObj := TheDelegate;
end;

function TProxyFontPeer.GetDelegate: TFont;
begin
  Result := TFont(FDelegateObj);
end;

function TProxyFontPeer.GetColor: TColor;
begin
  Result := cm_AWT.TColor(GetDelegate.Color);
end;

function TProxyFontPeer.GetHeight: Integer;
begin
  Result := GetDelegate.Height;
end;

function TProxyFontPeer.GetName: string;
begin
  Result := GetDelegate.Name;
end;

function TProxyFontPeer.GetSize: Integer;
begin
  Result := GetDelegate.Size;
end;

procedure TProxyFontPeer.SetColor(AValue: TColor);
begin
  GetDelegate.Color := AValue;
end;

procedure TProxyFontPeer.SetHeight(AValue: Integer);
begin
  GetDelegate.Height := AValue;
end;

procedure TProxyFontPeer.SetName(AValue: string);
begin
  GetDelegate.Name := AValue;
end;

procedure TProxyFontPeer.SetSize(AValue: Integer);
begin
  GetDelegate.Size := AValue;
end;

{ TProxyBrushPeer }

constructor TProxyBrushPeer.Create(TheDelegate: TBrush);
begin
  inherited Create;
  FDelegateObj := TheDelegate;
  FCustomBitmap := nil;
end;

destructor TProxyBrushPeer.Destroy;
begin
  if Assigned(FCustomBitmap) then
    FCustomBitmap.Free;
  inherited Destroy;
end;

function TProxyBrushPeer.GetDelegate: TBrush;
begin
  Result := TBrush(FDelegateObj);
end;

function TProxyBrushPeer.GetBitmap: TACustomBitmap;
var
  cbp: IACustomBitmapPeer;
begin
  Result := nil;
  //GetDelegate.Bitmap 不是默认存在的
  if not Assigned(FCustomBitmap) and Assigned(GetDelegate.Bitmap) then
    begin
      cbp := TProxyCustomBitmapPeer.Create(GetDelegate.Bitmap);
      FCustomBitmap := TACustomBitmap.Create(cbp);
    end;
  Result := FCustomBitmap;
end;

function TProxyBrushPeer.GetColor: TColor;
begin
  Result := GetDelegate.Color;
end;

procedure TProxyBrushPeer.SetBitmap(AValue: TACustomBitmap);
begin
  if FCustomBitmap = AValue then
    Exit;
  if Assigned(FCustomBitmap) then
    FreeAndNil(FCustomBitmap);
  FCustomBitmap := TACustomBitmap.Create(AValue.GetPeer);
  if FCustomBitmap.GetPeer.GetDelegate is TCustomBitmap then
    GetDelegate.Bitmap := TCustomBitmap(FCustomBitmap.GetPeer.GetDelegate);
end;

procedure TProxyBrushPeer.SetColor(AValue: TColor);
begin
  GetDelegate.Color := Graphics.TColor(AValue);
end;

{ TProxyCanvasPeer }

constructor TProxyCanvasPeer.Create;
begin
  inherited Create;
  FIsSelfCreateDelegate := True;
  FDelegateObj := TCanvas.Create;
  FBrush := nil;
  FFont := nil;
end;

constructor TProxyCanvasPeer.Create(TheDelegate: TCanvas);
begin
  inherited Create;
  FDelegateObj := TheDelegate;
  FBrush := nil;
  FFont := nil;
end;

destructor TProxyCanvasPeer.Destroy;
begin
  if Assigned(FBrush) then
    FBrush.Free;
  if Assigned(FFont) then
    FFont.Free;
  inherited Destroy;
end;

function TProxyCanvasPeer.GetDelegate: TCanvas;
begin
  Result := TCanvas(FDelegateObj);
end;

function TProxyCanvasPeer.GetBrush: TABrush;
var
  bp: IABrushPeer;
begin
  Result := nil;
  if not Assigned(FBrush) then
    begin
      bp := TProxyBrushPeer.Create(GetDelegate.Brush);
      FBrush := TABrush.Create(bp);
    end;
  Result := FBrush;
end;

function TProxyCanvasPeer.GetFont: TAFont;
var
  fp: IAFontPeer;
begin
  Result := nil;
  if not Assigned(FFont) then
    begin
      fp := TProxyFontPeer.Create(GetDelegate.Font);
      FFont := TAFont.Create(fp);
    end;
  Result := FFont;
end;

procedure TProxyCanvasPeer.SetBrush(AValue: TABrush);
begin
  if FBrush = AValue then
    Exit;
  if Assigned(FBrush) then
    FreeAndNil(FBrush);
  FBrush := TABrush.Create(AValue.GetPeer);
  if FBrush.GetPeer.GetDelegate is TBrush then
    GetDelegate.Brush := TBrush(FBrush.GetPeer.GetDelegate);
end;

procedure TProxyCanvasPeer.SetFont(AValue: TAFont);
begin
  if FFont = AValue then
    Exit;
  if Assigned(FFont) then
    FreeAndNil(FFont);
  FFont := TAFont.Create(AValue.GetPeer);
  if FFont.GetPeer.GetDelegate is TFont then
    GetDelegate.Font := TFont(FFont.GetPeer.GetDelegate);
end;

procedure TProxyCanvasPeer.FillRect(X1, Y1, X2, Y2: Integer);
begin
  GetDelegate.FillRect(X1, Y1, X2, Y2);
end;

procedure TProxyCanvasPeer.TextOut(X, Y: Integer; const Text: string);
begin
  GetDelegate.TextOut(X, Y, Text);
end;


