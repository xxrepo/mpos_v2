unit uFormLoading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  cm_messager, uSystem;

type

  { TLoadingForm }

  TLoadingForm = class(TForm)
    Image1: TImage;
    LabelMsg2: TLabel;
    LabelRight: TLabel;
    LabelAppName: TLabel;
    Lab_Version: TLabel;
    LabelMsg: TLabel;
    PanelProgressBar: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCreateTime: TDateTime;
    FDev: Double;
    FLastPosition: Integer;
    procedure SetProgressBar(APosition: Integer);
  public
    procedure SetLoadMsg(const AMsg: string); overload;
    procedure SetLoadMsg(const AMsg: string; APosition: Integer); overload;
  end;

var
  LoadingForm: TLoadingForm;

resourceString
  RightStr = 'Copyright © 2016-2018,All rights reserved';

implementation

uses DateUtils, uVersion;

{$R *.frm}

{ TLoadingForm }

procedure TLoadingForm.FormCreate(Sender: TObject);
begin
  FCreateTime := now;
  if not AppSystem.IsTestMode then
    Self.FormStyle := fsStayOnTop;
  LabelMsg.Caption := '.';
  LabelMsg2.Caption := '.';
  LabelRight.Caption := RightStr;
  Lab_Version.Caption := 'v' + VersionStr;
end;

procedure TLoadingForm.FormShow(Sender: TObject);
begin
  FLastPosition := 0;
  PanelProgressBar.Top := LabelMsg.Top + LabelMsg.Height + 2;
  PanelProgressBar.Width := 0;
  FDev := (Self.Width - 16) / 100;
end;

procedure TLoadingForm.SetProgressBar(APosition: Integer);
begin
  PanelProgressBar.Width := Round(FDev * APosition);
end;

procedure TLoadingForm.SetLoadMsg(const AMsg: string);
begin
  if FLastPosition + 1 > FLastPosition then
    FLastPosition := FLastPosition + 1;
  SetLoadMsg(AMsg, FLastPosition);
end;

procedure TLoadingForm.SetLoadMsg(const AMsg: string; APosition: Integer);
begin
  FLastPosition := APosition;
  SetProgressBar(FLastPosition);
  LabelMsg2.Caption := LabelMsg.Caption;
  LabelMsg.Caption := AMsg;
  DefaultMessager.Debug('设置加载信息: [pos:%.2d msg:%s]', [FLastPosition, AMsg]);
  //Sleep(300);
  Application.ProcessMessages;
  if APosition >= 100 then
    begin
      if DateUtils.SecondsBetween(now, FCreateTime) <= 2 then
        begin
          Application.ProcessMessages;
          Sleep(400);
        end;
    end;
end;

end.

