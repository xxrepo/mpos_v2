unit uFormMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Contnrs, ExtCtrls,
  cm_controlutils,
  uFormService;

type

  { TMenuForm }

  TMenuForm = class(TServiceForm)
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FHostingMenu: TCMGridLayoutHostingMenu;
    FSubMenuFormList: TFPHashObjectList;
    procedure MenuItemDblClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetTitle(const ATitle: string); override;
    property HostingMenu: TCMGridLayoutHostingMenu read FHostingMenu;
    function AddMenuItem(const ACaption: string): TPanel;
    function AddItem(const ACaption: string; AEvent: TNotifyEvent): TPanel;
  end;

implementation

{$R *.frm}

{ TMenuForm }

procedure TMenuForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
    begin
      Close;
      Exit;
    end;
  case Key of
  37: FHostingMenu.Left;
  38: FHostingMenu.Up;
  39: FHostingMenu.Right;
  40: FHostingMenu.Down;
  13: FHostingMenu.DelClick;
  end;
end;

procedure TMenuForm.FormShow(Sender: TObject);
begin
  FHostingMenu.ReLayout;
end;

constructor TMenuForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSubMenuFormList := TFPHashObjectList.Create(False);
  FHostingMenu := TCMGridLayoutHostingMenu.Create(PanelClient, 6, 4);
  FHostingMenu.BorderSpacing := 80;
  FHostingMenu.LeftSpacing := 20;
  FHostingMenu.TopSpacing := 20;
  FHostingMenu.SelectBoxSize := 15;
  FHostingMenu.SelectBoxWidth := 4;
  FHostingMenu.AlignAtGrid := True;
  //
  FHostingMenu.SetColWidth(100);
  FHostingMenu.SetRowHeight(100);
end;

procedure TMenuForm.SetTitle(const ATitle: string);
begin
  inherited SetTitle(ATitle);
end;

procedure TMenuForm.MenuItemDblClick(Sender: TObject);
var
  f: TMenuForm;
begin
  f := TMenuForm(FSubMenuFormList.Find(IntToStr(Sender.GetHashCode)));
  if Assigned(f) then
    f.ShowModal;
end;

function TMenuForm.AddMenuItem(const ACaption: string): TPanel;
var
  f: TMenuForm;
begin
  Result := TPanel.Create(Self);
  Result.Caption := ACaption;
  Result.Parent := PanelClient;
  Result.OnDblClick := @MenuItemDblClick;
  HostingMenu.AddLayoutControl(Result);
  f := TMenuForm.Create(Result);
  f.BorderStyle := bsNone;
  f.BoundsRect := Self.BoundsRect;
  f.SetTitle(ACaption);
  FSubMenuFormList.Add(IntToStr(Result.GetHashCode), f);
end;

function TMenuForm.AddItem(const ACaption: string; AEvent: TNotifyEvent): TPanel;
begin
  Result := TPanel.Create(Self);
  Result.Caption := ACaption;
  Result.Parent := PanelClient;
  Result.OnDblClick := AEvent;
  HostingMenu.AddLayoutControl(Result);
end;

end.

