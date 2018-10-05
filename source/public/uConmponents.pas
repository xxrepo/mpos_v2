unit uConmponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls, Graphics, Dialogs,
  cm_ThemeControls, cm_theme;


type

  { TPOSTitlePanel }

  TPOSTitlePanel = class(TCMThemePanel)
  private
    FImage: TImage;
    FTitleLab: TLabel;
    FVersionLab: TLabel;
    FReLab: TLabel;
    procedure SelfResize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetTheme(ATheme: ITheme); override;
    procedure SetTitle(const ATitle: string);
    procedure SetVersion(const AVersion: string);
    procedure SetImage(const AFileName: string);
  end;

implementation

{ TPOSTitlePanel }

procedure TPOSTitlePanel.SelfResize(Sender: TObject);
begin
  FVersionLab.Left := Self.Width - FVersionLab.Width - 6;
  FVersionLab.Top := Self.Height - FVersionLab.Height - 4;
  //
  while FTitleLab.Width > (Self.Width - FImage.Width - FImage.Width) do
    begin
      FTitleLab.Font.Size := FTitleLab.Font.Size - 1;
      if FTitleLab.Font.Size < 10 then
        Break;
    end;
  //
  FTitleLab.Left := (Self.Width - FTitleLab.Width) div 2;
  FTitleLab.Top := (Self.Height - FTitleLab.Height) div 2;
end;

constructor TPOSTitlePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage := TImage.Create(Self);
  FTitleLab := TLabel.Create(Self);
  FVersionLab := TLabel.Create(Self);
  FReLab := TLabel.Create(Self);
  Self.OnResize := @SelfResize;
  //
  Self.BevelOuter := bvNone;
  Self.Height := 50;
  Self.ParentFont := False;
  Self.Font.Size := 12;
  FImage.Parent := Self;
  FImage.Align := alLeft;
  FImage.Width := 176;
  FImage.Proportional := True;
  FImage.Stretch := True;
  FImage.Transparent := True;
  //
  FTitleLab.Parent := Self;
  FTitleLab.ParentFont := False;
  FTitleLab.Alignment := taCenter;
  FTitleLab.AutoSize := True;
  //
  FVersionLab.Parent := Self;
  FVersionLab.ParentFont := False;
  FVersionLab.Font.Size := 9;
  FVersionLab.Font.Name := '黑体';
  FVersionLab.Font.Color := $0;
  FVersionLab.Font.Style := FVersionLab.Font.Style + [fsBold, fsItalic];
end;

procedure TPOSTitlePanel.SetTheme(ATheme: ITheme);
begin
  inherited SetTheme(ATheme);
  Self.Color := ATheme.GetParameter.Get('header.color').AsInteger;
  FTitleLab.Font.Size := ATheme.GetParameter.Get('header.titleFont').Get('size').AsInteger;
  FTitleLab.Font.Color := ATheme.GetParameter.Get('header.titleFont').Get('color').AsInteger;
  //FTitleLab.Font.Color := clLime;
  FTitleLab.Font.Name := ATheme.GetParameter.Get('header.titleFont').Get('name').AsString;
  if Self.Showing then
    SelfResize(Self);
end;

procedure TPOSTitlePanel.SetTitle(const ATitle: string);
begin
  if ATitle = '' then
    FTitleLab.Caption := '美宜佳便利店有限公司'
  else
    FTitleLab.Caption := ATitle;
  if Self.Showing then
    SelfResize(Self);
end;

procedure TPOSTitlePanel.SetVersion(const AVersion: string);
begin
  FVersionLab.Caption := AVersion;
  if Self.Showing then
    SelfResize(Self);
end;

procedure TPOSTitlePanel.SetImage(const AFileName: string);
begin
  if FileExists(AFileName) then
    try
      FImage.Picture.LoadFromFile(AFileName);
    except
      on e: Exception do
        //
    end;
end;


end.

