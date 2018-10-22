unit uDBInitForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uFormService;

type

  { TDBInitForm }

  TDBInitForm = class(TServiceForm)
    MemoLog : TMemo;
    Btn_Cancel : TButton;
    procedure CancelClick(Sender: TObject);
  public
    procedure printLog(s:String);
  end;

implementation

{ TDBInitForm }

procedure TDBInitForm.CancelClick(Sender: TObject);
begin
  //printlog('点击取消按钮');
  showmessage('单击取消按钮');
end;

procedure TDBInitForm.printLog(s: String);
begin
  if Assigned(MemoLog) then
    MemoLog.Lines.Add(s);
end;

end.

