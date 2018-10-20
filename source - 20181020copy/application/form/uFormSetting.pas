unit uFormSetting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uFormService,
  cm_controlutils;

type

  { TSettingForm }

  TSettingForm = class(TServiceForm)
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FGridLayoutMenu: TCMGridLayoutHostingMenu;
    procedure test(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.frm}

{ TSettingForm }

procedure TSettingForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
    begin
      Close;
      Exit;
    end;
  case Key of
  37: FGridLayoutMenu.Left;
  38: FGridLayoutMenu.Up;
  39: FGridLayoutMenu.Right;
  40: FGridLayoutMenu.Down;
  end;
end;

procedure TSettingForm.FormShow(Sender: TObject);
var
  p: TPanel;
begin
  Self.SetTitle('设置');
  FGridLayoutMenu.SetColWidth((PanelClient.Width - 160) div 6);
  FGridLayoutMenu.SetRowHeight((PanelClient.Height - 160) div 4);
  //
  p := TPanel.Create(Self);
  p.Parent := PanelClient;
  p.Caption := '设置主题';
  p.OnDblClick := @test;
  FGridLayoutMenu.PutLayoutControl(p, 1, 1);
  p := TPanel.Create(Self);
  p.Parent := PanelClient;
  p.Caption := '设置???A';
  FGridLayoutMenu.PutLayoutControl(p, 2, 1);
  p := TPanel.Create(Self);
  p.Parent := PanelClient;
  p.Caption := '设置???B';
  FGridLayoutMenu.PutLayoutControl(p, 2, 2);
  //
  FGridLayoutMenu.ReLayout;
end;

procedure TSettingForm.test(Sender: TObject);
begin
  ShowMessage('test'#10 + TControl(Sender).Name);
end;

constructor TSettingForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGridLayoutMenu := TCMGridLayoutHostingMenu.Create(PanelClient, 6, 4);
  FGridLayoutMenu.BorderSpacing := 80;
  FGridLayoutMenu.LeftSpacing := 10;
  FGridLayoutMenu.TopSpacing := 10;
  FGridLayoutMenu.SelectBoxSize := 15;
  FGridLayoutMenu.SelectBoxWidth := 5;
  FGridLayoutMenu.AlignAtGrid := True;
end;

destructor TSettingForm.Destroy;
begin
  inherited Destroy;
end;

end.

