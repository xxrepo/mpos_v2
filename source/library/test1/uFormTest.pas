unit uFormTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  cm_LCL;

type

  { TFormTest }

  TFormTest = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure appActive(Sender: TObject);
    procedure appUnactive(Sender: TObject);
  public

  end;

var
  FormTest: TFormTest;
  LCLManager: ICMLCLManager;

implementation

{$R *.frm}

{ TFormTest }

procedure TFormTest.FormCreate(Sender: TObject);
begin
  Application.OnActivate := @appActive;
  Application.OnDeactivate := @appUnactive;
end;

procedure TFormTest.FormDeactivate(Sender: TObject);
begin
  Memo1.Lines.Add('FormDeactivate');
end;

procedure TFormTest.FormShow(Sender: TObject);
begin
  Memo1.Lines.Add('FormShow');
end;

procedure TFormTest.FormActivate(Sender: TObject);
begin
  Memo1.Lines.Add('FormActivate');
end;

procedure TFormTest.Button1Click(Sender: TObject);
const
  wsa: array[TWindowState] of string = ('wsNormal', 'wsMinimized', 'wsMaximized', 'wsFullScreen');
begin
  Sleep(1000);
  //LCLManager.GetMainLCLGlobalSet.GetApplication.IntfAppRestore;
  Memo1.Lines.Add(wsa[LCLManager.GetMainLCLGlobalSet.GetApplication.MainForm.WindowState]);
  //
  if LCLManager.GetMainLCLGlobalSet.GetApplication.MainForm.WindowState = wsMinimized then
    begin
      LCLManager.GetMainLCLGlobalSet.GetApplication.MainForm.WindowState := wsMaximized;
      Self.BringToFront;

    end;
end;

procedure TFormTest.Button2Click(Sender: TObject);
begin
  Sleep(1000);
  //LCLManager.GetMainLCLGlobalSet.GetApplication.BringToFront;
  LCLManager.GetMainLCLGlobalSet.GetScreen.MoveFormToFocusFront(LCLManager.GetMainLCLGlobalSet.GetApplication.MainForm);
end;

procedure TFormTest.Button3Click(Sender: TObject);
begin
  ShowMessage('aaaaaaaaaaaaaaaaaaaaa');
end;

procedure TFormTest.appActive(Sender: TObject);
begin
  Memo1.Lines.Add('appActive');
end;

procedure TFormTest.appUnactive(Sender: TObject);
begin
  Memo1.Lines.Add('appActive');
end;

end.

