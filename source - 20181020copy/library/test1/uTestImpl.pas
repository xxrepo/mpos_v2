unit uTestImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs,
  cm_interfaces, cm_messager,
  uTest,
  uFormTest,
  uApp, cm_LCL,
  LCLType, InterfaceBase, LCLIntf;

type

  { TTest }

  TTest = class(TCMBasePersistent, IUnknown, ITest)
    procedure testClick(Sender: TObject);
  public
    procedure Test;
    procedure Test2;
    procedure Test3;
  end;

implementation

{ TTest }

procedure TTest.Test;
begin
  //Messager.Info('1111111111111111');
  //Messager.Info('re:%d', [Application.MessageBox('aa','bb',2)]);
  //Messager.Info('2222222222222222');
  raise Exception.Create('lib raise exception test');
  //Application.HandleException(Exception.Create('lib raise exception test'));
end;

procedure TTest.Test2;
var
  m: ICMLCLManager;

  DisabledList: TList;
  SavedFocusState: TFocusState;
  ActiveWindow: HWnd;
  SavedCursor: TCursor;
begin
  FormTest := TFormTest.Create(nil);
  if InterfaceRegister.OutInterface(ICMLCLManager, m) then
    begin
      LCLManager := m;

      InterfaceBase.WidgetSet.AppHandle := LCLManager.GetMainLCLGlobalSet.GetWidgetSet.AppHandle;

      DisabledList := m.GetMainLCLGlobalSet.GetScreen.DisableForms(nil);
      try
        //Messager.Info('--ShowInTaskBar');
        FormTest.ShowInTaskBar := stNever;
        Application.TaskBarBehavior := tbSingleButton;

        //
        ActiveWindow := GetActiveWindow;
        SavedFocusState := SaveFocusState;
        SavedCursor := Screen.Cursor;
        //Screen.FSaveFocusedList.Insert(0, Screen.FFocusedForm);
        //Screen.FFocusedForm := Self;
        Screen.MoveFormToFocusFront(FormTest);
        Screen.Cursor := crDefault;

        //
        FormTest.ShowModal;

        //Messager.Error('bbbbb %s', [booltostr(Assigned(Application.MainForm), True)]);

        //RestoreFocusedForm;
        RestoreFocusState(SavedFocusState);
        Screen.Cursor := SavedCursor;
        if LCLIntf.IsWindow(ActiveWindow) then
          SetActiveWindow(ActiveWindow);

      finally
        m.GetMainLCLGlobalSet.GetScreen.EnableForms(DisabledList);
      end;
    end;
  FormTest.Free;
end;

procedure TTest.Test3;
var
  prw: ICMLCLPropertyReaderWriter;
  lclg: ICMLCLGenerator;
  lclm: ICMLCLManager;
  f: TForm;
  m: TMethod;
  //tm : TNotifyEvent;
begin
  if InterfaceRegister.OutInterface(ICMLCLPropertyReaderWriter, prw) then
    if InterfaceRegister.OutInterface(ICMLCLGenerator, lclg) then
      if InterfaceRegister.OutInterface(ICMLCLManager, lclm) then
        begin
          f := TForm(lclg.NewComponent('TForm', nil));
          lclm.GetMainLCLGlobalSet.SetRequireDerivedFormResource(False);
          //f.Caption := '1234';

          try
            prw.SetStrProp(f, 'Caption', 'test');
            prw.SetInt64Prop(f, 'Left', 200);
            prw.SetInt64Prop(f, 'Top', 200);
            prw.SetInt64Prop(f, 'Width', 700);
            prw.SetInt64Prop(f, 'Height', 100);

            //Messager.Debug(Self.MethodName(Self.MethodAddress('testClick')));
            //tm := @testClick;

            FormTest := TFormTest.Create(nil);

            m.Code := Self.MethodAddress('testClick');
            //m.Data := FormTest;
            prw.SetMethodProp(f, 'OnClick', m);

          except
            on e: Exception do
              ShowMessage(e.ClassName + #10 + e.Message);
          end;
          f.ShowModal;
        end;
end;

procedure TTest.testClick(Sender: TObject);
begin
  //Messager.Error('44444444444444444444444444444444444');
  Application.MessageBox('aaaaaaaaaaaaaaaaaaaaa  vvv', 'aa');
end;

end.

