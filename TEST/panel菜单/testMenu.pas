unit testMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, GraphType, Dialogs;

type
  TMenuBtnRec = record
    btnPanel: TPanel;
    intoClickEvent: TNotifyEvent;
    intoDblClickEvent: TNotifyEvent;
  end;

  TCMPanelMenu = class
  private
    FMenuPanel: TPanel;
    FIndex: Integer;
    FBtnHeight: Integer;
    FColCount: Integer;
    FInterval: Integer;
    FBtnAlignment: TAlignment;
    FFont: TFont;
    FColor: TColor;
    //
    FBtns: array of TMenuBtnRec;
    function getCount: Integer;
    procedure btnClick(Sender: TObject);
    procedure btnDblClick(Sender: TObject);
  protected
    FSelectedBtnBackgroundColor: TColor;
    procedure setIndex(const Index: Integer);
  public
    constructor Create(AMenuPanel: TPanel; ColCount: Integer);
    procedure addBtn(const ACaption: string; ClickEvent: TNotifyEvent; ATag: Integer; dblClickEvent: TNotifyEvent=nil);
    procedure clearBtn;
    procedure up;
    procedure down;
    procedure left;
    procedure right;
    procedure clickIndex(Index: Integer);
  published
    property Count: Integer read getCount;
    property Index: Integer read FIndex write setIndex;
    property BtnHeight: Integer read FBtnHeight write FBtnHeight default 42;
    property ColCount: Integer read FColCount write FColCount default 3;
    property Interval: Integer read FInterval write FInterval default 0;
    property Font: TFont read FFont write FFont;
    property Color: TColor read FColor write FColor;
    property BtnAlignment: TAlignment read FBtnAlignment write FBtnAlignment default taLeftJustify;
    property SelectedBtnBackgroundColor: TColor write FSelectedBtnBackgroundColor default clPurple;
  end;

implementation

constructor TCMPanelMenu.Create(AMenuPanel: TPanel; ColCount: Integer);
begin
  FMenuPanel := AMenuPanel;
  FIndex := -1;
  FBtnHeight := 42;
  FBtnAlignment := taLeftJustify;
  FColCount := ColCount;
  //
  FSelectedBtnBackgroundColor := clPurple;
  FFont := TFont.Create;
  FFont.Name := string('宋体');
  FFont.Size := 14;
  FFont.Color := FMenuPanel.Font.Color;
  FColor := AMenuPanel.Color;
end;

function TCMPanelMenu.getCount: Integer;
begin
  Result := Length(FBtns);
end;

procedure TCMPanelMenu.addBtn(const ACaption: string; ClickEvent: TNotifyEvent; ATag: Integer; dblClickEvent: TNotifyEvent=nil);
var
  sPanel: TPanel;
begin
  SetLength(FBtns, Length(FBtns)+1);
  //
  sPanel := TPanel.Create(FMenuPanel);
  sPanel.Parent := FMenuPanel;
  sPanel.Height := FBtnHeight;
  sPanel.TabOrder := Length(FBtns)-1;
  sPanel.Width := (FMenuPanel.Width - FInterval) div FColCount - FInterval ;
  //sPanel.Left := ((getCount - 1) mod FColCount) * (FMenuPanel.Width div FColCount) + FInterval;
  sPanel.Left := ((getCount - 1) mod FColCount) * (sPanel.Width + FInterval) + FInterval;

  sPanel.Top := (getCount-1) div FColCount * (FBtnHeight + FInterval) + FInterval;
  sPanel.Color := FColor;
  sPanel.BevelOuter := bvRaised;
  sPanel.BevelWidth := 1;
  sPanel.Caption := ACaption;
  sPanel.Alignment := FBtnAlignment;
  sPanel.Font := FFont;
  sPanel.Tag := ATag;
  //
  FMenuPanel .Height := sPanel.Top + sPanel.Height + FInterval;
  //
  sPanel.OnClick := @btnClick;
  sPanel.OnDblClick := @btnDblClick;
  FBtns[Length(FBtns)-1].btnPanel := sPanel;
  FBtns[Length(FBtns)-1].intoClickEvent := ClickEvent;
  FBtns[Length(FBtns)-1].intoDblClickEvent := dblClickEvent;
  if FIndex<0 then
    setIndex(0);
end;

procedure TCMPanelMenu.clickIndex(Index: Integer);
var
  i: Integer;
begin
  for i:=Low(FBtns) to High(FBtns) do
    begin
      if i = Index then
        begin
          setIndex(i);
          if Assigned(FBtns[i].intoClickEvent) then
            FBtns[i].intoClickEvent(FBtns[i].btnPanel);
        end;
    end;
end;

procedure TCMPanelMenu.btnClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=Low(FBtns) to High(FBtns) do
    begin
      if FBtns[i].btnPanel.Tag = TPanel(Sender).Tag then
        begin
          clickIndex(i);
        end;
    end;
end;

procedure TCMPanelMenu.btnDblClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=Low(FBtns) to High(FBtns) do
    begin
      if FBtns[i].btnPanel.Tag = TPanel(Sender).Tag then
        begin
          setIndex(i);
          if Assigned(FBtns[i].intoDblClickEvent) then
            FBtns[i].intoDblClickEvent(FBtns[i].btnPanel);
        end;
    end;
end;

procedure TCMPanelMenu.clearBtn;
var
  i: Integer;
begin
  for i:=Low(FBtns) to High(FBtns) do
    begin
      FBtns[i].btnPanel.Free;
    end;
  SetLength(FBtns, 0);
  FIndex := -1;
end;

procedure TCMPanelMenu.setIndex(const Index: Integer);
var
  i: Integer;
begin
    begin
      FIndex := Index;
      for i:=Low(FBtns) to High(FBtns) do
        begin
          if i = FIndex then
            begin
              FBtns[i].btnPanel.Color := FSelectedBtnBackgroundColor;
              FBtns[i].btnPanel.Font.Color := clWhite;
            end
          else
            begin
              FBtns[i].btnPanel.Color := FColor;
              FBtns[i].btnPanel.Font := FFont;
            end;
        end;
    end;
end;

procedure TCMPanelMenu.up;
begin
  if FIndex - FColCount >= 0 then
    setIndex(FIndex - FColCount);
end;

procedure TCMPanelMenu.down;
begin
  if FIndex + FColCount < getCount then
    setIndex(FIndex + FColCount);
end;

procedure TCMPanelMenu.left;
begin
  if FIndex - 1 >= 0 then
    begin
      setIndex(FIndex - 1);
    end;
end;

procedure TCMPanelMenu.right;
begin
  if FIndex + 1 < getCount then
    begin
      setIndex(FIndex + 1);
    end;
end;

end.

