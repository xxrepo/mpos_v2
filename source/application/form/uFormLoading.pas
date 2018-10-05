unit uFormLoading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls,
  uApp;

type

  { TLoadingForm }

  TLoadingForm = class(TForm)
    LabelMsg2: TLabel;
    LabelRight: TLabel;
    LabelTitle: TLabel;
    LabelMsg: TLabel;
    ProgressBar: TProgressBar;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure Step(const AMsg: string; APosition: Integer);
  end;

var
  LoadingForm: TLoadingForm;

resourceString
  RightStr = 'Copyright © 2016-2018,All rights reserved';

implementation

{$R *.frm}

{ TLoadingForm }

procedure TLoadingForm.FormCreate(Sender: TObject);
begin
  if not AppSystem.IsTestMode then
    Self.FormStyle := fsStayOnTop;
  LabelMsg.Caption := '';
  LabelMsg2.Caption := '';
  LabelRight.Caption := RightStr;
end;

procedure TLoadingForm.Step(const AMsg: string; APosition: Integer);
begin
  ProgressBar.Position := APosition;
  LabelMsg2.Caption := LabelMsg.Caption;
  LabelMsg.Caption := AMsg;
  Application.ProcessMessages;
end;

end.

