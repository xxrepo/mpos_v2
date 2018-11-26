unit cm_controls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics, Dialogs,
  Themes, LCLProc;

type

  { TCMComboEdit }

  TCMComboEdit = class(TCustomEdit)
  private
    FItemsListBox: TListBox;
    FIgnoreExit: Boolean;
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxExit(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: Boolean);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FSelectOnly: Boolean;
    function GetItemHeight: Integer;
    function GetItemIndex: Integer;
    function GetItems: TStrings;
    function GetSorted: Boolean;
    procedure SetItemHeight(AValue: Integer);
    procedure SetItemIndex(AValue: Integer);
    procedure SetItems(AValue: TStrings);
    procedure SetSelectOnly(AValue: Boolean);
    procedure SetSorted(AValue: Boolean);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoOnChangeBounds; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Items: TStrings read GetItems write SetItems;
    property Sorted: Boolean read GetSorted write SetSorted;
    property SelectOnly: Boolean read FSelectOnly write SetSelectOnly;
  end;

  { TCMCustomPanel }

  //后继再写
  TCMCustomPanel = class(TCustomPanel)
  protected
    procedure Paint; override;
  end;

  { TCMImagePanel }

  TCMImagePanel = class(TCMCustomPanel)
  private
    FPicture: TPicture;
    FCenter: Boolean;
    FProportional: Boolean;
    FStretch: Boolean;
    procedure SetPicture(AValue: TPicture);
    procedure SetCenter(AValue: Boolean);
    procedure SetProportional(AValue: Boolean);
    procedure SetStretch(AValue: Boolean);
  private
    FImageAlign: TAlign;
    FRecImagePos: TPoint;
    FRecImageSize: TPoint;
    FSetImageRect: Boolean;
    procedure SetImageAlign(AValue: TAlign);
    function GetImageRect: TRect;
    procedure SetImageRect(AValue: TRect);
  protected
    procedure PictureChanged(Sender: TObject); virtual;
    function ImageDestRect: TRect; virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Picture: TPicture read FPicture write SetPicture;
    property Center: Boolean read FCenter write SetCenter;
    property Proportional: Boolean read FProportional write SetProportional;
    property Stretch: Boolean read FStretch write SetStretch;
    property ImageAlign: TAlign read FImageAlign write SetImageAlign;
    property ImageRect: TRect read GetImageRect write SetImageRect;
  end;

implementation

{ TCMComboEdit }

constructor TCMComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemsListBox := TListBox.Create(Self);
  FItemsListBox.Hide;
  FIgnoreExit := False;
  FSelectOnly := False;
  //
  FItemsListBox.OnClick := @ListBoxClick;
  FItemsListBox.OnExit := @ListBoxExit;
  FItemsListBox.OnSelectionChange := @ListBoxSelectionChange;
  FItemsListBox.OnKeyDown := @ListBoxKeyDown;
end;

procedure TCMComboEdit.ListBoxClick(Sender: TObject);
begin
  FItemsListBox.Show;
  FItemsListBox.BringToFront;
end;

procedure TCMComboEdit.ListBoxExit(Sender: TObject);
begin
  FItemsListBox.Hide;
end;

procedure TCMComboEdit.ListBoxSelectionChange(Sender: TObject; User: Boolean);
begin
  Self.Text := FItemsListBox.GetSelectedText;
end;

procedure TCMComboEdit.ListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 38) and (FItemsListBox.ItemIndex = 0) then
    begin
      if Self.CanFocus then
        begin
          Self.SetFocus;
        end;
    end;
end;

function TCMComboEdit.GetItemHeight: Integer;
begin
  Result := FItemsListBox.ItemHeight;
end;

function TCMComboEdit.GetItemIndex: Integer;
begin
  Result := FItemsListBox.ItemIndex;
end;

function TCMComboEdit.GetItems: TStrings;
begin
  Result := FItemsListBox.Items;
end;

function TCMComboEdit.GetSorted: Boolean;
begin
  Result := FItemsListBox.Sorted;
end;

procedure TCMComboEdit.SetItemHeight(AValue: Integer);
begin
  FItemsListBox.ItemHeight := AValue;
end;

procedure TCMComboEdit.SetItemIndex(AValue: Integer);
begin
  FItemsListBox.ItemIndex := AValue;
end;

procedure TCMComboEdit.SetItems(AValue: TStrings);
begin
  FItemsListBox.Items := AValue;
end;

procedure TCMComboEdit.SetSelectOnly(AValue: Boolean);
begin
  if FSelectOnly = AValue then
    Exit;
  FSelectOnly := AValue;
  if FSelectOnly then
    begin
      if FItemsListBox.Items.Count > 0 then
        begin
          if FItemsListBox.ItemIndex < 0 then
            FItemsListBox.ItemIndex := 0;
          Self.Text := FItemsListBox.Items[FItemsListBox.ItemIndex];
        end;
    end;
end;

procedure TCMComboEdit.SetSorted(AValue: Boolean);
begin
  FItemsListBox.Sorted := AValue;
end;

procedure TCMComboEdit.DoEnter;
begin
  inherited DoEnter;
  FItemsListBox.Show;
  FItemsListBox.BringToFront;
end;

procedure TCMComboEdit.DoExit;
var
  p, sp: TPoint;
begin
  inherited DoExit;
  p.SetLocation(0, 0);
  sp := FItemsListBox.ControlToScreen(p);
  p := Mouse.CursorPos;
  if not FIgnoreExit then
    if not ((p.X > sp.X) and (p.X < sp.X + FItemsListBox.Width) and (p.Y > sp.Y) and (p.Y < sp.Y + FItemsListBox.Height)) then
      FItemsListBox.Hide;
end;

procedure TCMComboEdit.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  if Assigned(FItemsListBox) then
    begin
      FItemsListBox.Parent := Self.Parent;
      FItemsListBox.Left := Self.Left;
      FItemsListBox.Top := Self.Top + Self.Height;
      FItemsListBox.Width := Self.Width;
    end;
end;

procedure TCMComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = 40) and (FItemsListBox.Items.Count > 0) then
    begin
      FItemsListBox.ItemIndex := 0;
      if FItemsListBox.CanFocus then
        begin
          FIgnoreExit := True;
          FItemsListBox.SetFocus;
          FIgnoreExit := False;
        end;
    end;
  if FSelectOnly then
    if not (Key in [9, 13, 16..20, 27, 32..36, 39, 40, 112..123]) then
      Key := 0;
end;

{ TCMCustomPanel }

procedure TCMCustomPanel.Paint;
begin
  inherited Paint;
end;

{ TCMImagePanel }

constructor TCMImagePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCenter := False;
  FProportional := False;
  FStretch := False;
  FImageAlign := alNone;
  FSetImageRect := False;
  //
  FPicture := TPicture.Create;
  FPicture.OnChange := @PictureChanged;
end;

procedure TCMImagePanel.SetPicture(AValue: TPicture);
begin
  if FPicture = AValue then
    Exit;
  FPicture.Assign(AValue);
end;

procedure TCMImagePanel.SetCenter(AValue: Boolean);
begin
  if FCenter = AValue then
    Exit;
  FCenter := AValue;
  PictureChanged(Self);
end;

procedure TCMImagePanel.SetProportional(AValue: Boolean);
begin
  if FProportional = AValue then
    Exit;
  FProportional := AValue;
  PictureChanged(Self);
end;

procedure TCMImagePanel.SetStretch(AValue: Boolean);
begin
  if FStretch = AValue then
    Exit;
  FStretch := AValue;
  PictureChanged(Self);
end;

procedure TCMImagePanel.SetImageAlign(AValue: TAlign);
begin
  if FImageAlign = AValue then
    Exit;
  FImageAlign := AValue;
  FSetImageRect := False;
  PictureChanged(Self);
end;

function TCMImagePanel.GetImageRect: TRect;
begin
  case ImageAlign of
  alLeft: begin FRecImagePos.X := 0; FRecImagePos.Y := 0; FRecImageSize.X := Picture.Width; FRecImageSize.Y := Self.Height; end;
  alTop: begin FRecImagePos.X := 0; FRecImagePos.Y := 0; FRecImageSize.X := Self.Width; FRecImageSize.Y := Picture.Height; end;
  alRight: begin FRecImagePos.X := Self.Width - Picture.Width; FRecImagePos.Y := 0; FRecImageSize.X := Picture.Width; FRecImageSize.Y := Self.Height; end;
  alBottom: begin FRecImagePos.X := 0; FRecImagePos.Y := Self.Height - Picture.Height; FRecImageSize.X := Self.Width; FRecImageSize.Y := Picture.Height; end;
  alClient: begin FRecImagePos.X := 0; FRecImagePos.Y := 0; FRecImageSize.X := Self.Width; FRecImageSize.Y := Self.Height; end;
  else if not FSetImageRect then
    begin FRecImageSize.X := Picture.Width; FRecImageSize.Y := Picture.Height; end;
  end;
  Result := Bounds(FRecImagePos.X, FRecImagePos.Y, FRecImageSize.X, FRecImageSize.Y);
end;

procedure TCMImagePanel.SetImageRect(AValue: TRect);
begin
  FImageAlign := alNone;
  FStretch := True;
  FSetImageRect := True;
  FRecImagePos.X := AValue.Left;
  FRecImagePos.Y := AValue.Top;
  FRecImageSize.X := AValue.Width;
  FRecImageSize.Y := AValue.Height;
  PictureChanged(Self);
end;

procedure TCMImagePanel.PictureChanged(Sender: TObject);
begin
  if Picture.Graphic <> nil then
    begin
      if AutoSize then
        begin
          InvalidatePreferredSize;
          AdjustSize;
        end;
      Picture.Graphic.Transparent := True;
    end;
  Invalidate;
end;

function TCMImagePanel.ImageDestRect: TRect;
var
  PicWidth: Integer;
  PicHeight: Integer;
  ImgWidth: Integer;
  ImgHeight: Integer;
  w: Integer;
  h: Integer;
  ChangeX, ChangeY: Integer;
begin
  PicWidth := Picture.Width;
  PicHeight := Picture.Height;
  ImgWidth := ImageRect.Width;
  ImgHeight := ImageRect.Height;
  //
  if (PicWidth=0) or (PicHeight=0) then
    Exit(Rect(0, 0, 0, 0));
  //
  if Stretch or (Proportional) then
    if Proportional then
      begin
        w := ImgWidth;
        h := (PicHeight*w) div PicWidth;
        if h>ImgHeight then
          begin
            h := ImgHeight;
            w := (PicWidth*h) div PicHeight;
          end;
        PicWidth := w;
        PicHeight := h;
      end
    else
      begin
        PicWidth := ImgWidth;
        PicHeight := ImgHeight;
      end;
  //
  Result := Bounds(ImageRect.Left, ImageRect.Top, PicWidth, PicHeight);
  //
  if Center then
    begin
      ChangeX := (ImgWidth-PicWidth) div 2;
      ChangeY := (ImgHeight-PicHeight) div 2;
      OffsetRect(Result, ChangeX, ChangeY);
    end;
end;

procedure TCMImagePanel.Paint;
var
  ARect: TRect;
  TS : TTextStyle;
  pro: TThreadMethod;
begin
  ARect := GetClientRect;

  //PaintBevel(ARect, BevelOuter);
  //InflateRect(ARect, -BorderWidth, -BorderWidth);
  //PaintBevel(ARect, BevelInner);

  if Picture.Graphic <> nil then
    begin
      Canvas.StretchDraw(ImageDestRect, Picture.Graphic);

    end;

  //
  if Caption <> '' then
  begin
    TS := Canvas.TextStyle;
    TS.Alignment := BidiFlipAlignment(Self.Alignment, UseRightToLeftAlignment);
    if BiDiMode<>bdLeftToRight then
      TS.RightToLeft:= True;
    TS.Layout := tlCenter;
    TS.Opaque := false;
    TS.Clipping := false;
    TS.SystemFont :=Canvas.Font.IsDefault;
    TS.Wordbreak := WordWrap;
    TS.SingleLine := not WordWrap;
    if not Enabled then
      if ThemeServices.ThemesEnabled then
        Canvas.Font.Color := clGrayText
      else
        begin
          Canvas.Font.Color := clBtnHighlight;
          OffsetRect(ARect, 1, 1);
          Canvas.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
          Canvas.Font.Color := clBtnShadow;
          OffsetRect(ARect, -1, -1);
        end
    else
      Canvas.Font.Color := Font.Color;
    Canvas.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
  end;

  //inherited Paint;
  TMethod(pro).Code := @TCustomControl.Paint;
  TMethod(pro).Data := Self;
  pro();
end;



end.

