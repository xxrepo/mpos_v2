

{ TAFont }

function TAFont.GetColor: TColor;
begin
  Result := GetPeer.GetColor;
end;

function TAFont.GetHeight: Integer;
begin
  Result := GetPeer.GetHeight;
end;

function TAFont.GetName: string;
begin
  Result := GetPeer.GetName;
end;

function TAFont.GetSize: Integer;
begin
  Result := GetPeer.GetSize;
end;

procedure TAFont.SetColor(AValue: TColor);
begin
  GetPeer.SetColor(AValue);
end;

procedure TAFont.SetHeight(AValue: Integer);
begin
  GetPeer.SetHeight(AValue);
end;

procedure TAFont.SetName(AValue: string);
begin
  GetPeer.SetName(AValue);
end;

procedure TAFont.SetSize(AValue: Integer);
begin
  GetPeer.SetSize(AValue);
end;

constructor TAFont.Create;
begin
  inherited Create;
  FPeer := TAWTManager.DefaultToolkit.CreateFont(Self);
end;

constructor TAFont.Create(APeer: IAFontPeer);
begin
  inherited Create;
  FPeer := APeer;
end;

function TAFont.GetPeer: IAFontPeer;
begin
  FPeer.QueryInterface(IAFontPeer, Result);
end;

{ TAGraphic }

function TAGraphic.GetEmpty: Boolean;
begin
  Result := GetPeer.GetEmpty;
end;

function TAGraphic.GetHeight: Integer;
begin
  Result := GetPeer.GetHeight;
end;

function TAGraphic.GetWidth: Integer;
begin
  Result := GetPeer.GetWidth;
end;

procedure TAGraphic.SetHeight(AValue: Integer);
begin
  GetPeer.SetHeight(AValue);
end;

procedure TAGraphic.SetWidth(AValue: Integer);
begin
  GetPeer.SetWidth(AValue);
end;

constructor TAGraphic.Create;
begin
  FPeer := nil;
end;

destructor TAGraphic.Destroy;
begin
  FPeer := nil;
  inherited Destroy;
end;

function TAGraphic.GetPeer: IAGraphicPeer;
begin
  FPeer.QueryInterface(IAGraphicPeer, Result);
end;

procedure TAGraphic.Clear;
begin
  GetPeer.Clear;
end;

procedure TAGraphic.LoadFromFile(const AFilename: string);
begin
  GetPeer.LoadFromFile(AFilename);
end;

procedure TAGraphic.LoadFromStream(AStream: TStream);
begin
  GetPeer.LoadFromStream(AStream);
end;

procedure TAGraphic.SaveToFile(const AFilename: string);
begin
  GetPeer.SaveToFile(AFilename);
end;

procedure TAGraphic.SaveToStream(AStream: TStream);
begin
  GetPeer.SaveToStream(AStream);
end;

{ TARasterImage }

function TARasterImage.GetCanvas: TACanvas;
begin
  Result := GetPeer.GetCanvas;
end;

function TARasterImage.GetPeer: IARasterImagePeer;
begin
  FPeer.QueryInterface(IARasterImagePeer, Result);
end;

procedure TARasterImage.FreeImage;
begin
  GetPeer.FreeImage;
end;

{ TACustomBitmap }

function TACustomBitmap.GetMonochrome: Boolean;
begin
  Result := GetPeer.GetMonochrome;
end;

procedure TACustomBitmap.SetMonochrome(AValue: Boolean);
begin
  GetPeer.SetMonochrome(AValue);
end;

constructor TACustomBitmap.Create(APeer: IACustomBitmapPeer);
begin
  inherited Create;
  FPeer := APeer;
end;

function TACustomBitmap.GetPeer: IACustomBitmapPeer;
begin
  FPeer.QueryInterface(IACustomBitmapPeer, Result);
end;

procedure TACustomBitmap.SetSize(AWidth, AHeight: Integer);
begin
  GetPeer.SetSize(AWidth, AHeight);
end;

{ TABitmap }

constructor TABitmap.Create;
begin
  inherited Create(TAWTManager.DefaultToolkit.CreateCustomBitmap(Self));
end;

{ TAJPEGImage }

constructor TAJPEGImage.Create;
begin
  inherited Create(TAWTManager.DefaultToolkit.CreateCustomBitmap(Self));
end;

{ TAGIFImage }

constructor TAGIFImage.Create;
begin
  inherited Create(TAWTManager.DefaultToolkit.CreateCustomBitmap(Self));
end;

{ TAPortableNetworkGraphic }

constructor TAPortableNetworkGraphic.Create;
begin
  inherited Create(TAWTManager.DefaultToolkit.CreateCustomBitmap(Self));
end;

{ TABrush }

function TABrush.GetBitmap: TACustomBitmap;
begin
  Result := GetPeer.GetBitmap;
end;

function TABrush.GetColor: TColor;
begin
  Result := GetPeer.GetColor;
end;

procedure TABrush.SetBitmap(AValue: TACustomBitmap);
begin
  GetPeer.SetBitmap(AValue);
end;

procedure TABrush.SetColor(AValue: TColor);
begin
  GetPeer.SetColor(AValue);
end;

constructor TABrush.Create(APeer: IABrushPeer);
begin
  inherited Create;
  FPeer := APeer;
end;

function TABrush.GetPeer: IABrushPeer;
begin
  FPeer.QueryInterface(IABrushPeer, Result);
end;

{ TACanvas }

function TACanvas.GetBrush: TABrush;
begin
  Result := GetPeer.GetBrush;
end;

function TACanvas.GetFont: TAFont;
begin
  Result := GetPeer.GetFont;
end;

procedure TACanvas.SetBrush(AValue: TABrush);
begin
  GetPeer.SetBrush(AValue);
end;

procedure TACanvas.SetFont(AValue: TAFont);
begin
  GetPeer.SetFont(AValue);
end;

constructor TACanvas.Create;
begin
  inherited Create;
  FPeer := TAWTManager.DefaultToolkit.CreateCanvas(Self);
end;

constructor TACanvas.Create(APeer: IACanvasPeer);
begin
  inherited Create;
  FPeer := APeer;
end;

function TACanvas.GetPeer: IACanvasPeer;
begin
  Result := IACanvasPeer(FPeer);
end;

procedure TACanvas.FillRect(X1, Y1, X2, Y2: Integer);
begin
  GetPeer.FillRect(X1, Y1, X2, Y2);
end;

procedure TACanvas.TextOut(X, Y: Integer; const Text: string);
begin
  GetPeer.TextOut(X, Y, Text);
end;

{ TAControlBorderSpacing }

function TAControlBorderSpacing.GetAround: Integer;
begin
  Result := GetPeer.GetAround;
end;

function TAControlBorderSpacing.GetBottom: Integer;
begin
  Result := GetPeer.GetBottom;
end;

function TAControlBorderSpacing.GetLeft: Integer;
begin
  Result := GetPeer.GetLeft;
end;

function TAControlBorderSpacing.GetRight: Integer;
begin
  Result := GetPeer.GetRight;
end;

function TAControlBorderSpacing.GetTop: Integer;
begin
  Result := GetPeer.GetTop;
end;

procedure TAControlBorderSpacing.SetAround(AValue: Integer);
begin
  GetPeer.SetAround(AValue);
end;

procedure TAControlBorderSpacing.SetBottom(AValue: Integer);
begin
  GetPeer.SetBottom(AValue);
end;

procedure TAControlBorderSpacing.SetLeft(AValue: Integer);
begin
  GetPeer.SetLeft(AValue);
end;

procedure TAControlBorderSpacing.SetRight(AValue: Integer);
begin
  GetPeer.SetRight(AValue);
end;

procedure TAControlBorderSpacing.SetTop(AValue: Integer);
begin
  GetPeer.SetTop(AValue);
end;

constructor TAControlBorderSpacing.Create(OwnerControl: TAControl);
begin
  inherited Create;
  FPeer := TAWTManager.DefaultToolkit.CreateBorderSpacing(Self, OwnerControl);
end;

constructor TAControlBorderSpacing.Create(APeer: IAControlBorderSpacingPeer);
begin
  inherited Create;
  FPeer := APeer;
end;

function TAControlBorderSpacing.GetPeer: IAControlBorderSpacingPeer;
begin
  Result := IAControlBorderSpacingPeer(FPeer);
end;

{ TAComponent }

function TAComponent.GetComponent(AIndex: Integer): TAComponent;
begin
  if not Assigned(FComponents) then
    Result := nil
  else
    Result := TAComponent(FComponents.Items[AIndex]);
end;

function TAComponent.GetComponentCount: Integer;
begin
  if not Assigned(FComponents) then
    Result := 0
  else
    Result := FComponents.Count;
end;

constructor TAComponent.Create(AOwner: TAComponent);
begin
  inherited Create;
  FOwner := nil;
  FComponents := nil;
  if Assigned(AOwner) then
    AOwner.InsertComponent(Self);
end;

destructor TAComponent.Destroy;
begin
  DestroyComponents;
  if FOwner <> nil then
    FOwner.RemoveComponent(Self);
  FPeer := nil;
  inherited Destroy;
end;

procedure TAComponent.DestroyComponents;
var
  acomponent: TAComponent;
begin
  while Assigned(FComponents) do
    begin
      aComponent := TAComponent(FComponents.Last);
      RemoveComponent(aComponent);
      aComponent.Destroy;
    end;
end;

procedure TAComponent.InsertComponent(AComponent: TAComponent);
begin
  if not Assigned(FComponents) then
    FComponents := TFPList.Create;
  FComponents.Add(AComponent);
  AComponent.FOwner := Self;
end;

procedure TAComponent.RemoveComponent(AComponent: TAComponent);
begin
  AComponent.FOwner := nil;
  if Assigned(FComponents) then
    begin
      FComponents.Remove(AComponent);
      if FComponents.Count = 0 then
        begin
          FComponents.Free;
          FComponents := nil;
        end;
    end;
end;

function TAComponent.GetPeer: IAComponentPeer;
begin
  Result := IAComponentPeer(FPeer);
end;

function TAComponent.GetName: TComponentName;
begin
  Result := GetPeer.GetName;
end;

procedure TAComponent.SetName(AValue: TComponentName);
begin
  GetPeer.SetName(AValue);
end;

function TAComponent.GetTag: PtrInt;
begin
  Result := GetPeer.GetTag;
end;

procedure TAComponent.SetTag(AValue: PtrInt);
begin
  GetPeer.SetTag(AValue);
end;

{ TAControl }

function TAControl.GetPeer: IAControlPeer;
begin
  FPeer.QueryInterface(IAControlPeer, Result);
end;

function TAControl.GetParent: TAWinControl;
begin
  Result := GetPeer.GetParent;
end;

procedure TAControl.SetParent(AValue: TAWinControl);
begin
  GetPeer.SetParent(AValue);
end;

function TAControl.GetWidth: Integer;
begin
  Result := GetPeer.GetWidth;
end;

procedure TAControl.SetWidth(AValue: Integer);
begin
  GetPeer.SetWidth(AValue);
end;

procedure TAControl.Click;
begin
  GetPeer.Click;
end;

procedure TAControl.DblClick;
begin
  GetPeer.DblClick;
end;

procedure TAControl.AdjustSize;
begin
  GetPeer.AdjustSize;
end;

procedure TAControl.InvalidatePreferredSize;
begin
  GetPeer.InvalidatePreferredSize;
end;

procedure TAControl.BringToFront;
begin
  GetPeer.BringToFront;
end;

procedure TAControl.Hide;
begin
  GetPeer.Hide;
end;

procedure TAControl.Invalidate;
begin
  GetPeer.Invalidate;
end;

procedure TAControl.SendToBack;
begin
  GetPeer.SendToBack;
end;

procedure TAControl.Show;
begin
  GetPeer.Show;
end;

procedure TAControl.Update;
begin
  GetPeer.Update;
end;

procedure TAControl.AddControlListener(l: IControlListener);
begin
  GetPeer.AddControlListener(l);
end;

procedure TAControl.RemoveControlListener(l: IControlListener);
begin
  GetPeer.RemoveControlListener(l);
end;

function TAControl.GetControlListeners: TControlListenerList;
begin
  Result := GetPeer.GetControlListeners;
end;

procedure TAControl.AddMouseListener(l: IMouseListener);
begin
  GetPeer.AddMouseListener(l);
end;

procedure TAControl.RemoveMouseListener(l: IMouseListener);
begin
  GetPeer.RemoveMouseListener(l);
end;

function TAControl.GetMouseListeners: TMouseListenerList;
begin
  Result := GetPeer.GetMouseListeners;
end;

function TAControl.GetBoundsRect: TRect;
begin
  Result := GetPeer.GetBoundsRect;
end;

function TAControl.GetAlign: TAlign;
begin
  Result := GetPeer.GetAlign;
end;

function TAControl.GetAutoSize: Boolean;
begin
  Result := GetPeer.GetAutoSize;
end;

function TAControl.GetBorderSpacing: TAControlBorderSpacing;
begin
  Result := GetPeer.GetBorderSpacing;
end;

function TAControl.GetColor: TColor;
begin
  Result := GetPeer.GetColor;
end;

function TAControl.GetEnabled: Boolean;
begin
  Result := GetPeer.GetEnabled;
end;

function TAControl.GetFont: TAFont;
begin
  Result := GetPeer.GetFont;
end;

function TAControl.GetHeight: Integer;
begin
  Result := GetPeer.GetHeight;
end;

function TAControl.GetHint: TCaption;
begin
  Result := GetPeer.GetHint;
end;

function TAControl.GetLeft: Integer;
begin
  Result := GetPeer.GetLeft;
end;

function TAControl.GetMark: TCaption;
begin
  Result := GetPeer.GetMark;
end;

function TAControl.GetParentColor: Boolean;
begin
  Result := GetPeer.GetParentColor;
end;

function TAControl.GetParentFont: Boolean;
begin
  Result := GetPeer.GetParentFont;
end;

function TAControl.GetText: TCaption;
begin
  Result := GetPeer.GetText;
end;

function TAControl.GetTop: Integer;
begin
  Result := GetPeer.GetTop;
end;

function TAControl.GetVisible: Boolean;
begin
  Result := GetPeer.GetVisible;
end;

procedure TAControl.SetAlign(AValue: TAlign);
begin
  GetPeer.SetAlign(AValue);
end;

procedure TAControl.SetAutoSize(AValue: Boolean);
begin
  GetPeer.SetAutoSize(AValue);
end;

procedure TAControl.SetBorderSpacing(AValue: TAControlBorderSpacing);
begin
  GetPeer.SetBorderSpacing(AValue);
end;

procedure TAControl.SetColor(AValue: TColor);
begin
  GetPeer.SetColor(AValue);
end;

procedure TAControl.SetEnabled(AValue: Boolean);
begin
  GetPeer.SetEnabled(AValue);
end;

procedure TAControl.SetFont(AValue: TAFont);
begin
  GetPeer.SetFont(AValue);
end;

procedure TAControl.SetHeight(AValue: Integer);
begin
  GetPeer.SetHeight(AValue);
end;

procedure TAControl.SetHint(AValue: TCaption);
begin
  GetPeer.SetHint(AValue);
end;

procedure TAControl.SetLeft(AValue: Integer);
begin
  GetPeer.SetLeft(AValue);
end;

procedure TAControl.SetMark(AValue: TCaption);
begin
  GetPeer.SetMark(AValue);
end;

procedure TAControl.SetParentColor(AValue: Boolean);
begin
  GetPeer.SetParentColor(AValue);
end;

procedure TAControl.SetParentFont(AValue: Boolean);
begin
  GetPeer.SetParentFont(AValue);
end;

procedure TAControl.SetText(AValue: TCaption);
begin
  GetPeer.SetText(AValue);
end;

procedure TAControl.SetTop(AValue: Integer);
begin
  GetPeer.SetTop(AValue);
end;

procedure TAControl.SetVisible(AValue: Boolean);
begin
  GetPeer.SetVisible(AValue);
end;

procedure TAControl.SetBoundsRect(AValue: TRect);
begin
  GetPeer.SetBoundsRect(AValue);
end;

{ TAWinControl }

function TAWinControl.GetBorderStyle: TBorderStyle;
begin
  Result := GetPeer.GetBorderStyle;
end;

function TAWinControl.GetControl(AIndex: Integer): TAControl;
begin
  Result := GetPeer.GetControl(AIndex);
end;

function TAWinControl.GetControlCount: Integer;
begin
  Result := GetPeer.GetControlCount;
end;

function TAWinControl.GetShowing: Boolean;
begin
  Result := GetPeer.GetShowing;
end;

function TAWinControl.GetTabOrder: TTabOrder;
begin
  Result := GetPeer.GetTabOrder;
end;

procedure TAWinControl.SetBorderStyle(AValue: TBorderStyle);
begin
  GetPeer.SetBorderStyle(AValue);
end;

procedure TAWinControl.SetTabOrder(AValue: TTabOrder);
begin
  GetPeer.SetTabOrder(AValue);
end;

function TAWinControl.GetPeer: IAWinControlPeer;
begin
  Result := IAWinControlPeer(FPeer);
end;

procedure TAWinControl.InsertControl(AControl: TAControl);
begin
  GetPeer.InsertControl(AControl);
end;

procedure TAWinControl.RemoveControl(AControl: TAControl);
begin
  GetPeer.RemoveControl(AControl);
end;

function TAWinControl.CanFocus: Boolean;
begin
  Result := GetPeer.CanFocus;
end;

function TAWinControl.CanSetFocus: Boolean;
begin
  Result := GetPeer.CanSetFocus;
end;

function TAWinControl.Focused: Boolean;
begin
  Result := GetPeer.Focused;
end;

procedure TAWinControl.SetFocus;
begin
  GetPeer.SetFocus;
end;

procedure TAWinControl.AddWinControlListener(l: IWinControlListener);
begin
  GetPeer.AddWinControlListener(l);
end;

procedure TAWinControl.RemoveWinControlListener(l: IWinControlListener);
begin
  GetPeer.RemoveWinControlListener(l);
end;

function TAWinControl.GetWinControlListeners: TWinControlListenerList;
begin
  Result := GetPeer.GetWinControlListeners;
end;

procedure TAWinControl.AddKeyListener(l: IKeyListener);
begin
  GetPeer.AddKeyListener(l);
end;

procedure TAWinControl.RemoveKeyListener(l: IKeyListener);
begin
  GetPeer.RemoveKeyListener(l);
end;

function TAWinControl.GetKeyListeners: TKeyListenerList;
begin
  Result := GetPeer.GetKeyListeners;
end;

{ TACustomControl }

function TACustomControl.GetCanvas: TACanvas;
begin
  Result := GetPeer.GetCanvas;
end;

procedure TACustomControl.SetCanvas(AValue: TACanvas);
begin
  GetPeer.SetCanvas(AValue);
end;

function TACustomControl.GetPeer: IACustomControlPeer;
begin
  FPeer.QueryInterface(IACustomControlPeer, Result);
end;

procedure TACustomControl.AddCustomControlListener(l: ICustomControlListener);
begin
  GetPeer.AddCustomControlListener(l);
end;

procedure TACustomControl.RemoveCustomControlListener(l: ICustomControlListener);
begin
  GetPeer.RemoveCustomControlListener(l);
end;

function TACustomControl.GetCustomControlListeners: TCustomControlListenerList;
begin
  Result := GetPeer.GetCustomControlListeners;
end;

{ TAListBox }

function TAListBox.GetCanvas: TACanvas;
begin
  Result := GetPeer.GetCanvas;
end;

function TAListBox.GetCount: Integer;
begin
  Result := GetPeer.GetCount;
end;

function TAListBox.GetItemHeight: Integer;
begin
  Result := GetPeer.GetItemHeight;
end;

function TAListBox.GetItemIndex: Integer;
begin
  Result := GetPeer.GetItemIndex;
end;

function TAListBox.GetItems: TStrings;
begin
  Result := GetPeer.GetItems;
end;

function TAListBox.GetSelected(Index: Integer): Boolean;
begin
  Result := GetPeer.GetSelected(Index);
end;

function TAListBox.GetSorted: Boolean;
begin
  Result := GetPeer.GetSorted;
end;

procedure TAListBox.SetItemHeight(AValue: Integer);
begin
  GetPeer.SetItemHeight(AValue);
end;

procedure TAListBox.SetItemIndex(AValue: Integer);
begin
  GetPeer.SetItemIndex(AValue);
end;

procedure TAListBox.SetItems(AValue: TStrings);
begin
  GetPeer.SetItems(AValue);
end;

procedure TAListBox.SetSelected(Index: Integer; AValue: Boolean);
begin
  GetPeer.SetSelected(Index, AValue);
end;

procedure TAListBox.SetSorted(AValue: Boolean);
begin
  GetPeer.SetSorted(AValue);
end;

constructor TAListBox.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreateListBox(Self);
end;

function TAListBox.GetPeer: IAListBoxPeer;
begin
  Result := IAListBoxPeer(FPeer);
end;

procedure TAListBox.Click;
begin
  GetPeer.Click; //继承关系应由实现者维护，这时只管调用
end;

procedure TAListBox.Clear;
begin
  GetPeer.Clear;
end;

function TAListBox.GetSelectedText: string;
begin
  Result := GetPeer.GetSelectedText;
end;

function TAListBox.ItemRect(Index: Integer): TRect;
begin
  Result := GetPeer.ItemRect(Index);
end;

procedure TAListBox.AddListBoxListener(l: IListBoxListener);
begin
  GetPeer.AddListBoxListener(l);
end;

procedure TAListBox.RemoveListBoxListener(l: IListBoxListener);
begin
  GetPeer.RemoveListBoxListener(l);
end;

function TAListBox.GetListBoxListeners: TListBoxListenerList;
begin
  Result := GetPeer.GetListBoxListeners;
end;

{ TALabel }

function TALabel.GetAlignment: TAlignment;
begin
  Result := GetPeer.GetAlignment;
end;

function TALabel.GetLayout: TTextLayout;
begin
  Result := GetPeer.GetLayout;
end;

procedure TALabel.SetAlignment(AValue: TAlignment);
begin
  GetPeer.SetAlignment(AValue);
end;

procedure TALabel.SetLayout(AValue: TTextLayout);
begin
  GetPeer.SetLayout(AValue);
end;

constructor TALabel.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreateLabel(Self);
end;

function TALabel.GetPeer: IALabelPeer;
begin
  FPeer.QueryInterface(IALabelPeer, Result);
end;

{ TAPanel }

function TAPanel.GetAlignment: TAlignment;
begin
  Result := GetPeer.GetAlignment;
end;

function TAPanel.GetBevelColor: TColor;
begin
  Result := GetPeer.GetBevelColor;
end;

function TAPanel.GetBevelInner: TPanelBevel;
begin
  Result := GetPeer.GetBevelInner;
end;

function TAPanel.GetBevelOuter: TPanelBevel;
begin
  Result := GetPeer.GetBevelOuter;
end;

function TAPanel.GetBevelWidth: TBevelWidth;
begin
  Result := GetPeer.GetBevelWidth;
end;

procedure TAPanel.SetAlignment(AValue: TAlignment);
begin
  GetPeer.SetAlignment(AValue);
end;

procedure TAPanel.SetBevelColor(AValue: TColor);
begin
  GetPeer.SetBevelColor(AValue);
end;

procedure TAPanel.SetBevelInner(AValue: TPanelBevel);
begin
  GetPeer.SetBevelInner(AValue);
end;

procedure TAPanel.SetBevelOuter(AValue: TPanelBevel);
begin
  GetPeer.SetBevelOuter(AValue);
end;

procedure TAPanel.SetBevelWidth(AValue: TBevelWidth);
begin
  GetPeer.SetBevelWidth(AValue);
end;

constructor TAPanel.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreatePanel(Self);
end;

function TAPanel.GetPeer: IAPanelPeer;
begin
  FPeer.QueryInterface(IAPanelPeer, Result);
end;

{ TACustomEdit }

function TACustomEdit.GetMaxLength: Integer;
begin
  Result := GetPeer.GetMaxLength;
end;

function TACustomEdit.GetNumbersOnly: Boolean;
begin
  Result := GetPeer.GetNumbersOnly;
end;

function TACustomEdit.GetPasswordChar: Char;
begin
  Result := GetPeer.GetPasswordChar;
end;

function TACustomEdit.GetReadOnly: Boolean;
begin
  Result := GetPeer.GetReadOnly;
end;

function TACustomEdit.GetSelLength: integer;
begin
  Result := GetPeer.GetSelLength;
end;

function TACustomEdit.GetSelStart: integer;
begin
  Result := GetPeer.GetSelStart;
end;

function TACustomEdit.GetSelText: String;
begin
  Result := GetPeer.GetSelText;
end;

procedure TACustomEdit.SetMaxLength(AValue: Integer);
begin
  GetPeer.SetMaxLength(AValue);
end;

procedure TACustomEdit.SetNumbersOnly(AValue: Boolean);
begin
  GetPeer.SetNumbersOnly(AValue);
end;

procedure TACustomEdit.SetPasswordChar(AValue: Char);
begin
  GetPeer.SetPasswordChar(AValue);
end;

procedure TACustomEdit.SetReadOnly(AValue: Boolean);
begin
  GetPeer.SetReadOnly(AValue);
end;

procedure TACustomEdit.SetSelLength(AValue: integer);
begin
  GetPeer.SetSelLength(AValue);
end;

procedure TACustomEdit.SetSelStart(AValue: integer);
begin
  GetPeer.SetSelStart(AValue);
end;

procedure TACustomEdit.SetSelText(AValue: String);
begin
  GetPeer.SetSelText(AValue);
end;

function TACustomEdit.GetPeer: IACustomEditPeer;
begin
  FPeer.QueryInterface(IACustomEditPeer, Result);
end;

procedure TACustomEdit.Clear;
begin
  GetPeer.Clear;
end;

procedure TACustomEdit.SelectAll;
begin
  GetPeer.SelectAll;
end;

procedure TACustomEdit.AddEditListener(l: IEditListener);
begin
  GetPeer.AddEditListener(l);
end;

procedure TACustomEdit.RemoveEditListener(l: IEditListener);
begin
  GetPeer.RemoveEditListener(l);
end;

function TACustomEdit.GetEditListeners: TEditListenerList;
begin
  Result := GetPeer.GetEditListeners;
end;

{ TAEdit }

constructor TAEdit.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreateEdit(Self);
end;

function TAEdit.GetPeer: IAEditPeer;
begin
  Result := IAEditPeer(FPeer);
end;

{ TAMemo }

function TAMemo.GetLines: TStrings;
begin
  Result := GetPeer.GetLines;
end;

function TAMemo.GetScrollBars: TScrollStyle;
begin
  Result := GetPeer.GetScrollBars;
end;

procedure TAMemo.SetLines(AValue: TStrings);
begin
  GetPeer.SetLines(AValue);
end;

procedure TAMemo.SetScrollBars(AValue: TScrollStyle);
begin
  GetPeer.SetScrollBars(AValue);
end;

constructor TAMemo.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreateMemo(Self);
end;

function TAMemo.GetPeer: IAMemoPeer;
begin
  Result := IAMemoPeer(FPeer);
end;

{ TAForm }

function TAForm.GetFormBorderStyle: TFormBorderStyle;
begin
  Result := GetPeer.GetFormBorderStyle;
end;

procedure TAForm.SetFormBorderStyle(AValue: TFormBorderStyle);
begin
  GetPeer.SetFormBorderStyle(AValue);
end;

constructor TAForm.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreateForm(Self);
end;

function TAForm.GetPeer: IAFormPeer;
begin
  Result := IAFormPeer(FPeer);
end;

procedure TAForm.Close;
begin
  GetPeer.Close;
end;

function TAForm.ShowModal: Integer;
begin
  Result := GetPeer.ShowModal;
end;

procedure TAForm.AddFormListener(l: IFormListener);
begin
  GetPeer.AddFormListener(l);
end;

procedure TAForm.RemoveFormListener(l: IFormListener);
begin
  GetPeer.RemoveFormListener(l);
end;

function TAForm.GetFormListeners: TFormListenerList;
begin
  Result := GetPeer.GetFormListeners;
end;

{ TAFrame }

constructor TAFrame.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreateFrame(Self);
end;

function TAFrame.GetPeer: IAFramePeer;
begin
  FPeer.QueryInterface(IAFramePeer, Result);
end;

{ TADateTimePicker }

function TADateTimePicker.GetDate: TDate;
begin
  Result := GetPeer.GetDate;
end;

function TADateTimePicker.GetDateTime: TDateTime;
begin
  Result := GetPeer.GetDateTime;
end;

function TADateTimePicker.GetMaxDate: TDate;
begin
  Result := GetPeer.GetMaxDate;
end;

function TADateTimePicker.GetMinDate: TDate;
begin
  Result := GetPeer.GetMinDate;
end;

function TADateTimePicker.GetTime: TTime;
begin
  Result := GetPeer.GetTime;
end;

procedure TADateTimePicker.SetDate(AValue: TDate);
begin
  GetPeer.SetDate(AValue);
end;

procedure TADateTimePicker.SetDateTime(AValue: TDateTime);
begin
  GetPeer.SetDateTime(AValue);
end;

procedure TADateTimePicker.SetMaxDate(AValue: TDate);
begin
  GetPeer.SetMaxDate(AValue);
end;

procedure TADateTimePicker.SetMinDate(AValue: TDate);
begin
  GetPeer.SetMinDate(AValue);
end;

procedure TADateTimePicker.SetTime(AValue: TTime);
begin
  GetPeer.SetTime(AValue);
end;

constructor TADateTimePicker.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreateDateTimePicker(Self);
end;

function TADateTimePicker.GetPeer: IADateTimePickerPeer;
begin
  Result := IADateTimePickerPeer(FPeer);
end;

{ TACustomGrid }

function TACustomGrid.GetBorderColor: TColor;
begin
  Result := GetPeer.GetBorderColor;
end;

function TACustomGrid.GetCol: Integer;
begin
  Result := GetPeer.GetCol;
end;

function TACustomGrid.GetColCount: Integer;
begin
  Result := GetPeer.GetColCount;
end;

function TACustomGrid.GetColWidths(ACol: Integer): Integer;
begin
  Result := GetPeer.GetColWidths(ACol);
end;

function TACustomGrid.GetDefColWidth: Integer;
begin
  Result := GetPeer.GetDefColWidth;
end;

function TACustomGrid.GetDefRowHeight: Integer;
begin
  Result := GetPeer.GetDefRowHeight;
end;

function TACustomGrid.GetFixedColor: TColor;
begin
  Result := GetPeer.GetFixedColor;
end;

function TACustomGrid.GetFixedCols: Integer;
begin
  Result := GetPeer.GetFixedCols;
end;

function TACustomGrid.GetFixedRows: Integer;
begin
  Result := GetPeer.GetFixedRows;
end;

function TACustomGrid.GetGridBorderStyle: TBorderStyle;
begin
  Result := GetPeer.GetGridBorderStyle;
end;

function TACustomGrid.GetOptions: TGridOptions;
begin
  Result := GetPeer.GetOptions;
end;

function TACustomGrid.GetRow: Integer;
begin
  Result := GetPeer.GetRow;
end;

function TACustomGrid.GetRowCount: Integer;
begin
  Result := GetPeer.GetRowCount;
end;

function TACustomGrid.GetRowHeights(ARow: Integer): Integer;
begin
  Result := GetPeer.GetRowHeights(ARow);
end;

function TACustomGrid.GetScrollBars: TScrollStyle;
begin
  Result := GetPeer.GetScrollBars;
end;

function TACustomGrid.GetTitleFont: TAFont;
begin
  Result := GetPeer.GetTitleFont;
end;

procedure TACustomGrid.SetBorderColor(AValue: TColor);
begin
  GetPeer.SetBorderColor(AValue);
end;

procedure TACustomGrid.SetCol(AValue: Integer);
begin
  GetPeer.SetCol(AValue);
end;

procedure TACustomGrid.SetColCount(AValue: Integer);
begin
  GetPeer.SetColCount(AValue);
end;

procedure TACustomGrid.SetColWidths(ACol: Integer; AValue: Integer);
begin
  GetPeer.SetColWidths(ACol, AValue);
end;

procedure TACustomGrid.SetDefColWidth(AValue: Integer);
begin
  GetPeer.SetDefColWidth(AValue);
end;

procedure TACustomGrid.SetDefRowHeight(AValue: Integer);
begin
  GetPeer.SetDefRowHeight(AValue);
end;

procedure TACustomGrid.SetFixedcolor(AValue: TColor);
begin
  GetPeer.SetFixedcolor(AValue);
end;

procedure TACustomGrid.SetFixedCols(AValue: Integer);
begin
  GetPeer.SetFixedCols(AValue);
end;

procedure TACustomGrid.SetFixedRows(AValue: Integer);
begin
  GetPeer.SetFixedRows(AValue);
end;

procedure TACustomGrid.SetGridBorderStyle(AValue: TBorderStyle);
begin
  GetPeer.SetGridBorderStyle(AValue);
end;

procedure TACustomGrid.SetOptions(AValue: TGridOptions);
begin
  GetPeer.SetOptions(AValue);
end;

procedure TACustomGrid.SetRow(AValue: Integer);
begin
  GetPeer.SetRow(AValue);
end;

procedure TACustomGrid.SetRowCount(AValue: Integer);
begin
  GetPeer.SetRowCount(AValue);
end;

procedure TACustomGrid.SetRowHeights(ARow: Integer; AValue: Integer);
begin
  GetPeer.SetRowHeights(ARow, AValue);
end;

procedure TACustomGrid.SetScrollBars(AValue: TScrollStyle);
begin
  GetPeer.SetScrollBars(AValue);
end;

procedure TACustomGrid.SetTitleFont(AValue: TAFont);
begin
  GetPeer.SetTitleFont(AValue);
end;

function TACustomGrid.GetPeer: IACustomGridPeer;
begin
  Result := IACustomGridPeer(FPeer);
end;

procedure TACustomGrid.BeginUpdate;
begin
  GetPeer.BeginUpdate;
end;

function TACustomGrid.CellRect(ACol, ARow: Integer): TRect;
begin
  Result := GetPeer.CellRect(ACol, ARow);
end;

procedure TACustomGrid.Clear;
begin
  GetPeer.Clear;
end;

procedure TACustomGrid.EndUpdate(ARefresh: Boolean);
begin
  GetPeer.EndUpdate(ARefresh);
end;

procedure TACustomGrid.InvalidateCell(ACol, ARow: Integer);
begin
  GetPeer.InvalidateCell(ACol, ARow);
end;

procedure TACustomGrid.InvalidateCol(ACol: Integer);
begin
  GetPeer.InvalidateCol(ACol);
end;

procedure TACustomGrid.InvalidateRow(ARow: Integer);
begin
  GetPeer.InvalidateRow(ARow);
end;

{ TACustomDrawGrid }

function TACustomDrawGrid.GetPeer: IACustomDrawGridPeer;
begin
  Result := IACustomDrawGridPeer(FPeer);
end;

procedure TACustomDrawGrid.AddDrawGridListener(l: IDrawGridListener);
begin
  GetPeer.AddDrawGridListener(l);
end;

procedure TACustomDrawGrid.RemoveDrawGridListener(l: IDrawGridListener);
begin
  GetPeer.RemoveDrawGridListener(l);
end;

function TACustomDrawGrid.GetDrawGridListeners: TDrawGridListenerList;
begin
  Result := GetPeer.GetDrawGridListeners;
end;

{ TAStringGrid }

function TAStringGrid.GetCells(ACol, ARow: Integer): string;
begin
  Result := GetPeer.GetCells(ACol, ARow);
end;

function TAStringGrid.GetCols(Index: Integer): TStrings;
begin
  Result := GetPeer.GetCols(Index);
end;

function TAStringGrid.GetRows(Index: Integer): TStrings;
begin
  Result := GetPeer.GetRows(Index);
end;

procedure TAStringGrid.SetCells(ACol, ARow: Integer; AValue: string);
begin
  GetPeer.SetCells(ACol, ARow, AValue);
end;

procedure TAStringGrid.SetCols(Index: Integer; AValue: TStrings);
begin
  GetPeer.SetCols(Index, AValue);
end;

procedure TAStringGrid.SetRows(Index: Integer; AValue: TStrings);
begin
  GetPeer.SetRows(Index, AValue);
end;

constructor TAStringGrid.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreateStringGrid(Self);
end;

function TAStringGrid.GetPeer: IAStringGridPeer;
begin
  Result := IAStringGridPeer(FPeer);
end;

procedure TAStringGrid.AutoSizeColumn(ACol: Integer);
begin
  GetPeer.AutoSizeColumn(ACol);
end;

procedure TAStringGrid.AutoSizeColumns;
begin
  GetPeer.AutoSizeColumns;
end;

{ TAComboBox }

function TAComboBox.GetCanvas: TACanvas;
begin
  Result := GetPeer.GetCanvas;
end;

function TAComboBox.GetDropDownCount: Integer;
begin
  Result := GetPeer.GetDropDownCount;
end;

function TAComboBox.GetItemIndex: Integer;
begin
  Result := GetPeer.GetItemIndex;
end;

function TAComboBox.GetItems: TStrings;
begin
  Result := GetPeer.GetItems;
end;

function TAComboBox.GetMaxLength: Integer;
begin
  Result := GetPeer.GetMaxLength;
end;

function TAComboBox.GetSelLength: Integer;
begin
  Result := GetPeer.GetSelLength;
end;

function TAComboBox.GetSelStart: Integer;
begin
  Result := GetPeer.GetSelStart;
end;

function TAComboBox.GetSelText: string;
begin
  Result := GetPeer.GetSelText;
end;

function TAComboBox.GetSorted: Boolean;
begin
  Result := GetPeer.GetSorted;
end;

function TAComboBox.GetStyle: TComboBoxStyle;
begin
  Result := GetPeer.GetStyle;
end;

procedure TAComboBox.SetDropDownCount(AValue: Integer);
begin
  GetPeer.SetDropDownCount(AValue);
end;

procedure TAComboBox.SetItemIndex(AValue: Integer);
begin
  GetPeer.SetItemIndex(AValue);
end;

procedure TAComboBox.SetItems(AValue: TStrings);
begin
  GetPeer.SetItems(AValue);
end;

procedure TAComboBox.SetMaxLength(AValue: Integer);
begin
  GetPeer.SetMaxLength(AValue);
end;

procedure TAComboBox.SetSelLength(AValue: Integer);
begin
  GetPeer.SetSelLength(AValue);
end;

procedure TAComboBox.SetSelStart(AValue: Integer);
begin
  GetPeer.SetSelStart(AValue);
end;

procedure TAComboBox.SetSelText(AValue: string);
begin
  GetPeer.SetSelText(AValue);
end;

procedure TAComboBox.SetSorted(AValue: Boolean);
begin
  GetPeer.SetSorted(AValue);
end;

procedure TAComboBox.SetStyle(AValue: TComboBoxStyle);
begin
  GetPeer.SetStyle(AValue);
end;

constructor TAComboBox.Create(AOwner: TAComponent);
begin
  inherited Create(AOwner);
  FPeer := TAWTManager.DefaultToolkit.CreateComboBox(Self);
end;

function TAComboBox.GetPeer: IAComboBoxPeer;
begin
  Result := IAComboBoxPeer(FPeer);
end;

procedure TAComboBox.AddComboBoxListener(l: IComboBoxListener);
begin
  GetPeer.AddComboBoxListener(l);
end;

procedure TAComboBox.RemoveComboBoxListener(l: IComboBoxListener);
begin
  GetPeer.RemoveComboBoxListener(l);
end;

function TAComboBox.GetComboBoxListeners: TComboBoxListenerList;
begin
  Result := GetPeer.GetComboBoxListeners;
end;

procedure TAComboBox.Clear;
begin
  GetPeer.Clear;
end;

procedure TAComboBox.SelectAll;
begin
  GetPeer.SelectAll;
end;




