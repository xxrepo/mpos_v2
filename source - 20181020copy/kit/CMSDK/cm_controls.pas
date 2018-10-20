unit cm_controls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics, Dialogs,
  Themes, LCLProc;

type

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

