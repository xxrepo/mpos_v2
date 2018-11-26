unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Dialogs;

type

  TReProc = procedure(msg: string) of object;

  { TCMComboEdit }

  TCMComboEdit = class(TEdit)
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

var
  PrintMsg: TReProc;

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

end.

