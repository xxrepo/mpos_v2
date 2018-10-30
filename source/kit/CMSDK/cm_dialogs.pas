{
    This file is part of the CM Kit.
    Copyright (c) 2013-2017 by the ChenMeng studio

    dialogs

    This is not a complete unit,
    Here is the copy part of the CMKit, for testing

 **********************************************************************}

unit cm_dialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF LCL},Controls, StdCtrls, ExtCtrls, Graphics, Forms, LCLType, contnrs{$ENDIF}
  ;

type

  { ICMMsgBar
    // 信息提示条。
  }
  ICMMsgBar = interface
    ['{4A3D21D0-7929-49B8-B401-1F99F5C14675}']
    procedure ShowMessage(AEventType: TEventType; const AMsg: string); overload;
  end;

  { ICMMsgBox
    // 信息提示框，遵循 Application.MessageBox() 规范。
    // MB_OK = 0
  }
  ICMMsgBox = interface
    ['{637ECDF7-86EF-4C4B-83B0-C88BD061ABD5}']
    procedure ShowMessage(const AMsg: string);
    function MessageBox(const AText, ACaption: string; AFlags: Longint=0): Integer;
    function InputBox(const ACaption, APrompt, ADefault: string): string;
  end;

  {$IFDEF LCL}

  { TCMMsgBar }

  TCMMsgBar = class(TCustomPanel, ICMMsgBar)
  private
    FMsgLabel: TLabel;
    FPrefix: string;
    FInherentHeight: Integer;
  protected
    procedure toShowMsg(const AMsg: string); overload; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowInfo(const AMsg: string); overload;
    procedure ShowInfo(const Fmt: string; const Args: array of const); overload;
    procedure ShowError(const AMsg: string); overload;
    procedure ShowError(const Fmt: string; const Args: array of const); overload;
    procedure ShowMessage(AEventType: TEventType; const AMsg: string); overload;
    procedure ShowMessage(const AMsg: string; AFontColor, ABackgroundColor: TColor); overload;
    property Prefix: string read FPrefix write FPrefix;
    property InherentHeight: Integer read FInherentHeight write FInherentHeight;
  end;

  TCMMsgButtonPanel = class(TCustomPanel)
  private
    FKeyCode: Word;
    FReturnValue: Integer;
  public
    property KeyCode: Word read FKeyCode write FKeyCode;
    property ReturnValue: Integer read FReturnValue write FReturnValue;
  end;

  { TCMBaseMsgBoard }

  TCMBaseMsgBoard = class(TCustomPanel)
  private
    FTitleLabel: TLabel;
    FTopPanel, FMainPanel, FBottomPanel: TPanel;
    FMsgPanel: TPanel;
    FDrawPanel: TPanel;
  protected
    property MsgPanel: TPanel read FMsgPanel;    //FMsgPanel于FMsgPanel居右
    property DrawPanel: TPanel read FDrawPanel;  //FDrawPanel于FMsgPanel居左,一般不用宽度为0
    property BottomPanel: TPanel read FBottomPanel;
  (*** button *****************************************************************************)
  private
    FBtnList: TFPObjectList;    //TCMMsgButtonPanel
    FBtnIndex: Integer;
    FBtnWidth: Integer;
    FBtnHeight: Integer;
    FOnButtonClick: TNotifyEvent;
    procedure setBtnIndex(AValue: Integer);
    procedure setBtnHeight(AValue: Integer);
    procedure setBtnWidth(AValue: Integer);
    function getBtn(Index: Integer): TCMMsgButtonPanel;
    function getSelectedButton: TCMMsgButtonPanel;
    function getSelectedButtonReturnValue: Integer;
    procedure setSelectedButtonReturnValue(AValue: Integer);
  protected
    property BtnIndex: Integer read FBtnIndex write setBtnIndex;
    property Btn[Index: Integer]: TCMMsgButtonPanel read getBtn;
    procedure reBtnTheme; virtual;
    procedure reBtnSize; virtual;
    procedure btnClick(Sender: TObject); virtual;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  public
    property ButtonWidth: Integer read FBtnWidth write setBtnWidth;
    property ButtonHeight: Integer read FBtnHeight write setBtnHeight;
    procedure ClearButton;
    function AddButton(const ACaption: string; const AKeyCode: Word; const AReturnValue: Integer): Integer;
    property SelectedButton: TCMMsgButtonPanel read getSelectedButton;
    property SelectedButtonReturnValue: Integer read getSelectedButtonReturnValue write setSelectedButtonReturnValue;
  (*** end button **********************************************************************)
  (*** message area -------------------------------------------------------------------*)
  private
    FMsgAreaControlList: TFPObjectList; //TGraphicControl、TWinControl
    FMsgAreaFocusControlList: TFPList;  //TWinControl
    function getMsgAreaFocusControlCount: Integer;
    function getMsgAreaFocusControl(Item: Integer): TWinControl;
    function AddMsgLabel(const AMsg: string): TLabel;
  protected
    procedure reMsgAreaSize; virtual;
    procedure AddMsgAreaControl(AControl: TGraphicControl); overload;
    procedure AddMsgAreaControl(AControl: TWinControl); overload;
    procedure RegisterMsgAreaFocusControl(AControl: TWinControl);
    property MsgAreaFocusControlCount: Integer read getMsgAreaFocusControlCount;
    property MsgAreaFocusControl[Item: Integer]: TWinControl read getMsgAreaFocusControl;
  public
    procedure ClearMsg;
    procedure AddMsg(const AMsg: string; AFont: TFont); overload;
    procedure AddMsg(const AMsg: string; const ASize: Integer; const AFontName: string; AColor: TColor); overload;
  (*** end message area ---------------------------------------------------------------*)
  private
    function getTitle: string;
    procedure setTitle(const ATitle: string);
  public
    property Title: string read getTitle write setTitle;
  (*** theme *****************************************************************************)
  private
    FSelectedButtonFontColor: TColor;
    FTitleColor: TColor;
    function getTitleFont: TFont;
    procedure setTitleColor(AColor: TColor);
    function getDefaultMsgFont: TFont;
    function getButtonFont: TFont;
  public
    property TitleFont: TFont read getTitleFont;
    property TitleColor: TColor read FTitleColor write setTitleColor;
    property DefaultMsgFont: TFont read getDefaultMsgFont;
    property ButtonFont: TFont read getButtonFont;
    property SelectedButtonFontColor: TColor read FSelectedButtonFontColor write FSelectedButtonFontColor;
  (*** end theme ***********************************************************************)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetInitFocus;
  end;

  TCMMsgBoard = class(TCMBaseMsgBoard)
  public
    procedure AddMsg(const AMsg: string); overload;
  end;

  TCMBaseMsgBox = class(TCustomForm)
  private
    FReturnValue: Integer;
    FClearMsgOnClose: Boolean;
    FClearButtonOnClose: Boolean;
  protected
    procedure DoShow; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
  protected
    FMsgBoard: TCMMsgBoard;
    procedure boardResize(Sender: TObject);
    procedure boardButtonClick(Sender: TObject);
    procedure boxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure boxModalBefore; virtual;
    procedure boxModalAfter; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RePosition; overload;
    procedure RePosition(APoint: TPoint); overload;
    property MsgBoard: TCMMsgBoard read FMsgBoard;
    property ReturnValue: Integer read FReturnValue;
    property ClearMsgOnClose: Boolean read FClearMsgOnClose write FClearMsgOnClose;
    property ClearButtonOnClose: Boolean read FClearButtonOnClose write FClearButtonOnClose;
  public
    procedure ShowMessage(const AMsg: string);
    function MessageBox(const AText, ACaption: string; AFlags: Longint=MB_OK): Integer; virtual;
  end;

  TCMMsgBox = class(TCMBaseMsgBox, ICMMsgBox)
  private
    FInputEdt: TEdit;
    FInputText: string;
    FDefRe: Integer;
  protected
    procedure DoShow; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function MessageBox(const AText, ACaption: string; AFlags: Longint; ADefRe: Integer): Integer; overload;
    function InputBox(const ACaption, APrompt, ADefault: string): string; virtual;
  end;

  {$ELSE}
  //没有使用 LCL 时无法知晓 LCLType 的定义
const
  MB_OK = $00000000;
  MB_OKCANCEL = $00000001;
  MB_ABORTRETRYIGNORE = $00000002;
  MB_YESNOCANCEL = $00000003;
  MB_YESNO = $00000004;
  MB_RETRYCANCEL = $00000005;
  //
  IDOK = 1;     ID_OK = IDOK;
  IDCANCEL = 2; ID_CANCEL = IDCANCEL;
  IDABORT = 3;  ID_ABORT = IDABORT;
  IDRETRY = 4;  ID_RETRY = IDRETRY;
  IDIGNORE = 5; ID_IGNORE = IDIGNORE;
  IDYES = 6;    ID_YES = IDYES;
  IDNO = 7;     ID_NO = IDNO;
  IDCLOSE = 8;  ID_CLOSE = IDCLOSE;
  IDHELP = 9;   ID_HELP = IDHELP;
  {$ENDIF}


const
  DefaultBoardHeight: Integer = 220;
  DefaultBoardWidth: Integer = 450;
  DefaultButtonHeight: Integer = 40;
  DefaultButtonWidth: Integer = 120;

implementation

{$IFDEF LCL}

{TCMMsgBar}

constructor TCMMsgBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FPrefix := '    ';
  FPrefix := ' ';
  if AOwner is TWinControl then
    Self.Parent := TWinControl(AOwner);
  FInherentHeight := 20;
  Self.Height := FInherentHeight;
  Self.Align := alBottom;
  Self.Alignment := taLeftJustify;
  Self.BevelInner := bvNone;
  Self.BevelOuter := bvNone;
  Self.ParentColor := False;
  Self.Caption := '';
  //
  FMsgLabel :=  TLabel.Create(Self);
  FMsgLabel.Parent := Self;
  Self.Font.Name := string('黑体');
  Self.Font.Size := 14;
  FMsgLabel.Font := Self.Font;
  FMsgLabel.Color := clNone;
  //FMsgLabel.Height := Self.Height;
  if (Self.BevelOuter <> bvNone) or (Self.BevelInner <> bvNone) then
    FMsgLabel.Font.Height := FMsgLabel.Height - Self.BevelWidth - Self.BevelWidth
  else
    FMsgLabel.Font.Height := Self.Height;
  FMsgLabel.Align := alTop;
  FMsgLabel.Alignment := taLeftJustify;
  FMsgLabel.AutoSize := True;
  FMsgLabel.WordWrap := True;
  Self.Hide;
end;

procedure TCMMsgBar.toShowMsg(const AMsg: string);
var
  extra: Integer;
begin
  FMsgLabel.Font := Self.Font;
  FMsgLabel.Alignment := Self.Alignment;
  FMsgLabel.Caption := FPrefix + AMsg;
  Self.Show;
  //
  if (Self.BevelOuter <> bvNone) or (Self.BevelInner <> bvNone) then
    extra := Self.BevelWidth + Self.BevelWidth
  else
    extra := 0;
  if FMsgLabel.Height < FInherentHeight - extra  then
    Self.Height := FInherentHeight - extra
  else
    Self.Height := FMsgLabel.Height + extra;
  //
  Self.BringToFront;
  Self.Update;
end;

procedure TCMMsgBar.ShowInfo(const AMsg: string);
begin
  ShowMessage(AMsg, clYellow, clGreen);
end;

procedure TCMMsgBar.ShowInfo(const Fmt: string; const Args: array of const);
begin
  ShowInfo(Format(Fmt, Args));
end;

procedure TCMMsgBar.ShowError(const AMsg: string);
begin
  ShowMessage(AMsg, clYellow, clRed);
end;

procedure TCMMsgBar.ShowError(const Fmt: string; const Args: array of const);
begin
  ShowError(Format(Fmt, Args));
end;

procedure TCMMsgBar.ShowMessage(AEventType: TEventType; const AMsg: string);
begin
  case AEventType of
  etInfo: ShowInfo(AMsg);
  etError: ShowError(AMsg);
  etWarning: ShowMessage(AMsg, clRed, clYellow);
  else ShowMessage(AMsg, clBlack, clWhite);
  end;
end;

procedure TCMMsgBar.ShowMessage(const AMsg: string; AFontColor, ABackgroundColor: TColor);
begin
  Self.Color := ABackgroundColor;
  Self.Font.Color := AFontColor;
  toShowMsg(AMsg);
end;

{TCMBaseMsgBoard}

constructor TCMBaseMsgBoard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Width := DefaultBoardWidth;
  Self.Height := DefaultBoardHeight;
  Self.Align := alNone;
  Self.BorderStyle := bsNone;
  Self.BevelOuter := bvRaised;
  Self.BevelWidth := 1;
  //
  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTitleLabel := TLabel.Create(FTopPanel);
  FTitleLabel.Parent := FTopPanel;
  FMainPanel := TPanel.Create(Self);
  FMainPanel.Parent := Self;
  FBottomPanel := TPanel.Create(Self);
  FBottomPanel.Parent := Self;
  FBottomPanel.Width := Self.Width;
  //
  FTopPanel.Height := 50;
  FBottomPanel.Height := 51;
  //
  FTopPanel.Align := alTop;
  FMainPanel.Align := alClient;
  FBottomPanel.Align := alBottom;
  //title
  FTopPanel.Color := clGray;
  FTitleLabel.Align := alClient;
  FTitleLabel.Alignment := taCenter;
  FTitleLabel.Layout := tlCenter;
  FTitleLabel.Caption := '温馨提示';
  FTitleLabel.Font.Size := 14;
  FTitleLabel.Font.Name := '黑体';
  FTitleLabel.Font.Color := clYellow;
  //
  FBottomPanel.Font.Size := 11;
  FBottomPanel.Font.Name := '黑体';
  (**** message area ****)
  FMsgPanel := TPanel.Create(Self);
  FMsgPanel.Parent := FMainPanel;
  FMsgPanel.BevelInner := bvNone;
  FMsgPanel.BevelOuter := bvNone;
  FMsgPanel.BorderSpacing.Around := 2;
  FMsgPanel.Align := alClient;
  FMsgPanel.Font.Size := 11;
  FMsgPanel.Font.Name := '黑体';
  (**** draw area ****)
  FDrawPanel := TPanel.Create(Self);
  FDrawPanel.Parent := FMainPanel;
  FDrawPanel.BevelInner := bvNone;
  FDrawPanel.BevelOuter := bvNone;
  FDrawPanel.BorderSpacing.Around := 2;
  FDrawPanel.Align := alLeft;
  FDrawPanel.Width := 0;
  //
  FSelectedButtonFontColor := clYellow;
  FTitleColor := FTopPanel.Color;
  //背景色
  if AOwner is TWinControl then
    Self.Color := TWinControl(AOwner).Color
  else
    Self.Color := clSkyBlue;
  //
  FBtnIndex := -1;
  FBtnHeight := DefaultButtonHeight;
  FBtnWidth := DefaultButtonWidth;
  FBtnList := TFPObjectList.Create;
  //
  FMsgAreaControlList := TFPObjectList.Create;
  FMsgAreaFocusControlList := TFPList.Create;
end;

destructor TCMBaseMsgBoard.Destroy;
begin
  FBtnList.Free;
  FMsgAreaControlList.Free;
  FMsgAreaFocusControlList.Free;
  inherited Destroy;
end;

procedure TCMBaseMsgBoard.setBtnHeight(AValue: Integer);
begin
  FBtnHeight := AValue;
  reBtnSize;
end;

procedure TCMBaseMsgBoard.setBtnWidth(AValue: Integer);
begin
  FBtnWidth := AValue;
  reBtnSize;
end;

procedure TCMBaseMsgBoard.reBtnTheme;
var
  i: Integer;
begin
  for i:=FBtnList.Count-1 downto 0 do
    begin
      TCMMsgButtonPanel(FBtnList[i]).Color := FBottomPanel.Color;
      if i = BtnIndex then
        begin
          TCMMsgButtonPanel(FBtnList[i]).Font.Color := FSelectedButtonFontColor;
          TCMMsgButtonPanel(FBtnList[i]).BevelWidth := 2;
        end
      else
        begin
          TCMMsgButtonPanel(FBtnList[i]).Font.Color := FBottomPanel.Font.Color;
          TCMMsgButtonPanel(FBtnList[i]).BevelWidth := 1;
        end;
    end;
end;

procedure TCMBaseMsgBoard.setBtnIndex(AValue: Integer);
begin
  if AValue < FBtnList.Count then
    begin
      if AValue >= 0 then
        begin
          FBtnIndex := AValue;
          //没有Show时，不能SetFocus
          if TCMMsgButtonPanel(FBtnList[FBtnIndex]).CanSetFocus then
            TCMMsgButtonPanel(FBtnList[FBtnIndex]).SetFocus;
        end
      else
        FBtnIndex := -1;
      Self.reBtnTheme;
    end;
end;

procedure TCMBaseMsgBoard.btnClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=FBtnList.Count-1 downto 0 do
    begin
      if Sender = FBtnList[i] then
        begin
          BtnIndex := i;
          if Assigned(FOnButtonClick) then
            FOnButtonClick(Sender);
          Exit;
        end;
    end;
end;

procedure TCMBaseMsgBoard.ClearButton;
begin
  FBtnList.Clear;
  FBtnIndex := -1;
end;

procedure TCMBaseMsgBoard.reBtnSize;
var
  i: Integer;
  topInt, interval: Integer;
  reserved: Integer;
  areaWidth: Integer;
begin
  if FBtnList.Count <= 0 then
    Exit;
  //算顶
  if FBtnHeight > FBottomPanel.Height  - FBottomPanel.BevelWidth then
    begin
      FBtnHeight := FBottomPanel.Height - FBottomPanel.BevelWidth;
      topInt := 0;
    end
  else
    topInt := (FBottomPanel.Height - FBottomPanel.BevelWidth - FBtnHeight) div 2;
  //左右预留空间
  reserved := FBottomPanel.Width div 10;
  //分布区域宽度
  areaWidth := FBottomPanel.Width - FBottomPanel.BevelWidth - reserved;
  //算间隔
  if FBtnWidth * FBtnList.Count > areaWidth then
    begin
      FBtnWidth := areaWidth div FBtnList.Count;
      interval := 0;
    end
  else
    interval := (areaWidth - FBtnWidth * FBtnList.Count) div (FBtnList.Count * 2);
  //
  for i:=FBtnList.Count-1 downto 0 do
    begin
      TCMMsgButtonPanel(FBtnList[i]).Parent := FBottomPanel;
      TCMMsgButtonPanel(FBtnList[i]).OnClick := @btnClick;
      TCMMsgButtonPanel(FBtnList[i]).Height := FBtnHeight;
      TCMMsgButtonPanel(FBtnList[i]).Width := FBtnWidth;
      TCMMsgButtonPanel(FBtnList[i]).Left := reserved div 2 + ((2 * i) + 1) * interval + i * FBtnWidth;
      TCMMsgButtonPanel(FBtnList[i]).Top := topInt;
    end;
end;

function TCMBaseMsgBoard.getBtn(Index: Integer): TCMMsgButtonPanel;
begin
  Result := nil;
  if (Index >= 0) and (Index < FBtnList.Count) then
    Result := TCMMsgButtonPanel(FBtnList[Index]);
end;

function TCMBaseMsgBoard.getSelectedButton: TCMMsgButtonPanel;
begin
  Result := nil;
  if FBtnIndex>=0 then
    Result := TCMMsgButtonPanel(FBtnList[FBtnIndex]);
end;

function TCMBaseMsgBoard.getSelectedButtonReturnValue: Integer;
begin
  Result := -1;
  if FBtnIndex >= 0 then
    Result := TCMMsgButtonPanel(FBtnList[FBtnIndex]).ReturnValue;
end;

procedure TCMBaseMsgBoard.setSelectedButtonReturnValue(AValue: Integer);
var
  i: Integer;
begin
  if AValue >= 0 then
    begin
      for i:=FBtnList.Count-1 downto 0 do
        begin
          if TCMMsgButtonPanel(FBtnList[i]).ReturnValue = AValue then
            begin
              BtnIndex := i;
              Break;
            end;
        end;
    end;
end;

function TCMBaseMsgBoard.AddButton(const ACaption: string; const AKeyCode: Word; const AReturnValue: Integer): Integer;
var
  abtn: TCMMsgButtonPanel;
begin
  abtn := TCMMsgButtonPanel.Create(Self);
  abtn.Caption := ACaption;
  abtn.KeyCode := AKeyCode;
  abtn.ReturnValue := AReturnValue;
  abtn.BevelOuter := bvRaised;
  abtn.Font := FBottomPanel.Font;
  abtn.Color := FBottomPanel.Color;
  Result := FBtnList.Add(abtn);
  //
  if Self.Showing then
    begin
      Self.reBtnSize;
      Self.reBtnTheme;
    end;
end;

procedure TCMBaseMsgBoard.SetInitFocus;
begin
  if FMsgAreaFocusControlList.Count>0 then
    begin
      if TWinControl(FMsgAreaFocusControlList[0]).CanSetFocus then
        TWinControl(FMsgAreaFocusControlList[0]).SetFocus;
    end
  else if BtnIndex < 0 then
    BtnIndex := 0
end;

procedure TCMBaseMsgBoard.AddMsgAreaControl(AControl: TGraphicControl);
begin
  FMsgAreaControlList.Add(AControl);
  reMsgAreaSize;
end;

procedure TCMBaseMsgBoard.AddMsgAreaControl(AControl: TWinControl);
begin
  FMsgAreaControlList.Add(AControl);
  reMsgAreaSize;
end;

procedure TCMBaseMsgBoard.RegisterMsgAreaFocusControl(AControl: TWinControl);
begin
  FMsgAreaFocusControlList.Add(AControl);
end;

function TCMBaseMsgBoard.getMsgAreaFocusControlCount: Integer;
begin
  Result := 0;
  Result := FMsgAreaFocusControlList.Count;
end;

function TCMBaseMsgBoard.getMsgAreaFocusControl(Item: Integer): TWinControl;
begin
  Result := nil;
  if Item < FMsgAreaFocusControlList.Count then
    Result := TWinControl(FMsgAreaFocusControlList[Item]);
end;

procedure TCMBaseMsgBoard.ClearMsg;
begin
  FMsgAreaControlList.Clear;
  FMsgAreaFocusControlList.Clear;
end;

procedure TCMBaseMsgBoard.reMsgAreaSize;
var
  itemsHeight, sumHeight: Integer;
  i: Integer;
  control: TControl;
  c: TClass;
  x: Integer;
begin
  itemsHeight := 0;
  c := nil;
  x := 0;
  for i:=0 to FMsgAreaControlList.Count-1 do
    begin
      control := TControl(FMsgAreaControlList[i]);
      control.Parent := nil;   //必不可少
      control.Align := alNone;
      //特殊现象
      if c <> control.ClassType then
        x := x + 1;
      c := control.ClassType;
    end;
  for i:=FMsgAreaControlList.Count-1 downto 0 do
    begin
      control := TControl(FMsgAreaControlList[i]);
      control.Parent := FMsgPanel;
      control.Align := alTop;
      itemsHeight := itemsHeight + control.Height + control.BorderSpacing.Around + control.BorderSpacing.Top + control.BorderSpacing.Bottom;
      //特殊现象
      if control is TCustomEdit then
        if x > 1 then
          itemsHeight := itemsHeight + 1;
    end;
  itemsHeight := itemsHeight + FMsgPanel.BorderSpacing.Around * 2;
  //
  sumHeight := itemsHeight
               + FTopPanel.Height + FBottomPanel.Height
               + FTopPanel.BevelWidth * 2
               + FBottomPanel.BevelWidth * 2
               + FMsgPanel.BevelWidth * 2;
  //
  if sumHeight < DefaultBoardHeight then
    Self.Height := DefaultBoardHeight
  else
    Self.Height := sumHeight;
end;

function TCMBaseMsgBoard.getTitle: string;
begin
  Result := FTitleLabel.Caption;
end;

procedure TCMBaseMsgBoard.setTitle(const ATitle: string);
begin
  FTitleLabel.Caption := ATitle;
end;

function TCMBaseMsgBoard.getTitleFont: TFont;
begin
  Result := FTitleLabel.Font;
end;

procedure TCMBaseMsgBoard.setTitleColor(AColor: TColor);
begin
  FTopPanel.Color := AColor;
  FTitleColor := AColor;
end;

function TCMBaseMsgBoard.getDefaultMsgFont: TFont;
begin
  Result := FMsgPanel.Font;
end;

function TCMBaseMsgBoard.getButtonFont: TFont;
begin
  Result := FBottomPanel.Font;
end;

function TCMBaseMsgBoard.AddMsgLabel(const AMsg: string): TLabel;
var
  p: TPanel;
begin
  Result := nil;
  if AMsg = '' then
    Exit;
  p := TPanel.Create(Self);
  p.BevelInner := bvNone;
  p.BevelOuter := bvNone;
  p.BorderSpacing.Around := 2;
  p.Font := Self.DefaultMsgFont;
  p.ParentFont := True;
  Result :=  TLabel.Create(p);
  Result.Parent := p;
  //
  Result.Font := Self.DefaultMsgFont;
  Result.ParentFont := False;
  Result.Caption := AMsg;
  Result.Color := clNone;
  Result.BorderSpacing.Around := 0;
  Result.Alignment := taLeftJustify;
  Result.AutoSize := True;
  Result.WordWrap := True;
  //
  Result.Align := alTop;
  p.Height := 15;
  p.AutoSize := True; //必不可少
  AddMsgAreaControl(p);
end;

procedure TCMBaseMsgBoard.AddMsg(const AMsg: string; AFont: TFont);
var
  aLab: TLabel;
begin
  aLab := AddMsgLabel(AMsg);
  if not Assigned(aLab) then
    Exit;
  aLab.Font := AFont;
  reMsgAreaSize;
end;

procedure TCMBaseMsgBoard.AddMsg(const AMsg: string; const ASize: Integer; const AFontName: string; AColor: TColor);
var
  aLab: TLabel;
begin
  aLab := AddMsgLabel(AMsg);
  if not Assigned(aLab) then
    Exit;
  aLab.Font.Size := ASize;
  aLab.Font.Name := AFontName;
  aLab.Font.Color := AColor;
  reMsgAreaSize;
end;

{TCMMsgBoard}

procedure TCMMsgBoard.AddMsg(const AMsg: string);
var
  aLab: TLabel;
begin
  aLab := AddMsgLabel(AMsg);
  if not Assigned(aLab) then
    Exit;
  aLab.ParentFont := True;
end;

{TCMBaseMsgBox}

constructor TCMBaseMsgBox.Create(AOwner: TComponent);
var
  oldRequireDerivedFormResource: Boolean;
begin
  oldRequireDerivedFormResource := RequireDerivedFormResource;
  try
    RequireDerivedFormResource := False;
    inherited Create(AOwner);
    Self.KeyPreview := True;
    Self.BorderStyle := bsNone;
  finally
    RequireDerivedFormResource := oldRequireDerivedFormResource;
  end;
  Self.OnKeyDown := @boxKeyDown;
  Self.Width := DefaultBoardWidth;
  Self.Height := DefaultBoardHeight;
  //
  FReturnValue := -1;
  FClearMsgOnClose := True;
  FClearButtonOnClose := True;
  //
  FMsgBoard := TCMMsgBoard.Create(Self);
  FMsgBoard.Parent := Self;
  FMsgBoard.Align := alNone;
  FMsgBoard.Top := 0;
  FMsgBoard.Left := 0;
end;

destructor TCMBaseMsgBox.Destroy;
begin
  inherited Destroy;
end;

procedure TCMBaseMsgBox.DoShow;
begin
  try
    Self.Width := FMsgBoard.Width;
    Self.Height := FMsgBoard.Height;
    FMsgBoard.OnResize := @boardResize;
    FMsgBoard.OnButtonClick := @boardButtonClick;
    //
    FMsgBoard.reMsgAreaSize;
    //if FMsgBoard.FMainPanel.Height - FMsgBoard.MsgPanel.Height > 0 then
    //  FMsgBoard.Height := FMsgBoard.Height + (FMsgBoard.FMainPanel.Height - FMsgBoard.MsgPanel.Height);
    //20180514
    FMsgBoard.reBtnSize;
  finally
    inherited DoShow;
    FMsgBoard.reMsgAreaSize;
    Self.RePosition;
    FMsgBoard.SetInitFocus;
  end;
end;

procedure TCMBaseMsgBox.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  if FClearMsgOnClose then
    FMsgBoard.ClearMsg;
  if FClearButtonOnClose then
    FMsgBoard.ClearButton;
end;

procedure TCMBaseMsgBox.RePosition;
var
  p1, p2: TPoint;
  cp: TWinControl;
begin
  if Self.Owner is TControl then
    begin
      p1.x := TControl(Self.Owner).Left;
      p1.y := TControl(Self.Owner).Top;
      cp := TControl(Self.Owner).Parent;
      if Assigned(cp) then
        begin
          p2 := cp.ControlToScreen(p1);
          Self.Left := p2.x + abs(TControl(Self.Owner).Width - Self.Width) div 2;
          Self.Top := p2.y + abs(TControl(Self.Owner).Height - Self.Height) div 2;
        end
      else
        begin
          Self.Left := p1.x + abs(TControl(Self.Owner).Width - Self.Width) div 2;
          Self.Top := p1.y + abs(TControl(Self.Owner).Height - Self.Height) div 2;
        end;
    end
  else
    begin
      FMsgBoard.FTopPanel.Caption := '';
      if Assigned(Application.MainForm) then
        begin
          Self.Left := (Application.MainForm.Width - Self.Width) div 2;
          Self.Top := (Application.MainForm.Height - Self.Height) div 2;
        end
      else
        begin
          Self.Left := (Screen.Width - Self.Width) div 2;
          Self.Top := (Screen.Height - Self.Height) div 2;
        end;
    end;
end;

procedure TCMBaseMsgBox.RePosition(APoint: TPoint);
begin
  Self.Left := APoint.x;
  Self.Top := APoint.y;
end;

procedure TCMBaseMsgBox.boardResize(Sender: TObject);
begin
  if Sender is TCMMsgBoard then
    begin
      Self.Width := TCMMsgBoard(Sender).Width;
      Self.Height := TCMMsgBoard(Sender).Height;
    end;
end;

procedure TCMBaseMsgBox.boardButtonClick(Sender: TObject);
begin
  FReturnValue := TCMMsgButtonPanel(Sender).ReturnValue;
  Close;
end;

procedure TCMBaseMsgBox.boxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i, tempInt: Integer;
  wc: TWinControl;
  procedure checkKey;
  var j: Integer;
  begin
    for j:=FMsgBoard.FBtnList.Count-1 downto 0 do
      begin
        if TCMMsgButtonPanel(FMsgBoard.FBtnList[j]).KeyCode = Key then
          begin
            FReturnValue := TCMMsgButtonPanel(FMsgBoard.FBtnList[j]).ReturnValue;
            Close;
            Exit;
          end;
      end;
  end;
begin
  for i:=0 to FMsgBoard.MsgAreaFocusControlCount-1 do
    begin
      wc := TWinControl(FMsgBoard.MsgAreaFocusControl[i]);
      if wc.Focused then
        begin
          if Key = 13 then
            begin
              if i = FMsgBoard.MsgAreaFocusControlCount-1 then
                FMsgBoard.BtnIndex := 0
              else
                TWinControl(FMsgBoard.MsgAreaFocusControl[i+1]).SetFocus;
            end
          else
            checkKey;
          //
          Exit;
        end;
    end;

  if FMsgBoard.BtnIndex < 0 then
    Exit;
  //
  if Key = 37 then
    begin
      tempInt := FMsgBoard.BtnIndex - 1;
      if tempInt >= 0 then
        FMsgBoard.BtnIndex := tempInt;
      Exit;
    end
  else if Key = 39 then
    begin
      tempInt := FMsgBoard.BtnIndex + 1;
      if tempInt < FMsgBoard.FBtnList.Count then
        FMsgBoard.BtnIndex := tempInt
      else if FMsgBoard.MsgAreaFocusControlCount>0 then
        begin
          TWinControl(FMsgBoard.MsgAreaFocusControl[0]).SetFocus;
          FMsgBoard.BtnIndex := -1;
        end;
      Exit;
    end
  else if Key = 13 then
    begin
      if FMsgBoard.BtnIndex >= 0 then
        begin
          FReturnValue := FMsgBoard.Btn[FMsgBoard.BtnIndex].ReturnValue;
          Close;
          Exit;
        end;
    end
  else
    begin
      checkKey;
    end;
end;

function TCMBaseMsgBox.MessageBox(const AText, ACaption: string; AFlags: Longint=MB_OK): Integer;
begin
  FMsgBoard.Title := ACaption;
  FMsgBoard.ClearButton;
  if AText <> '' then
    FMsgBoard.AddMsg(AText);
  //
  if (AFlags and MB_OKCANCEL) = MB_OKCANCEL then
    begin
      FMsgBoard.AddButton('确定 [Enter]', 13, IDOK);
      FMsgBoard.AddButton('取消 [Esc]', 27, IDCANCEL);
    end
  else if (AFlags and MB_ABORTRETRYIGNORE) = MB_ABORTRETRYIGNORE then
    begin
      FMsgBoard.AddButton('异常终止 [F1]', 112, IDABORT);
      FMsgBoard.AddButton('重试 [F2]', 113, IDRETRY);
      FMsgBoard.AddButton('忽略 [F3]', 114, IDIGNORE);
    end
  else if (AFlags and MB_YESNOCANCEL) = MB_YESNOCANCEL then
    begin
      FMsgBoard.AddButton('是 [Y]', 89, IDYES);
      FMsgBoard.AddButton('否 [N]', 78, IDNO);
      FMsgBoard.AddButton('取消 [Esc]', 27, IDCANCEL);
    end
  else if (AFlags and MB_YESNO) = MB_YESNO then
    begin
      FMsgBoard.AddButton('是 [Y]', 89, IDYES);
      FMsgBoard.AddButton('否 [N]', 78, IDNO);
    end
  else if (AFlags and MB_RETRYCANCEL) = MB_RETRYCANCEL then
    begin
      FMsgBoard.AddButton('重试 [R]', 82, IDRETRY);
      FMsgBoard.AddButton('取消 [Esc]', 27, IDCANCEL);
    end
  else if (AFlags and MB_OK) = MB_OK then
    begin
      FMsgBoard.AddButton('确定 [Enter]', 13, IDOK);
    end
  else
    {%H-}FMsgBoard.AddButton('确定 [Enter]', 13, IDOK);
  //
  boxModalBefore;
  try
    Self.ShowModal;
    boxModalAfter;
  finally
    Result := FReturnValue;
  end;
end;

procedure TCMBaseMsgBox.boxModalBefore;
begin
end;

procedure TCMBaseMsgBox.boxModalAfter;
begin
end;

procedure TCMBaseMsgBox.ShowMessage(const AMsg: string);
begin
  Self.MessageBox(AMsg, '温馨提示', MB_OK);
end;

{TCMMsgBox}

constructor TCMMsgBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInputEdt := nil;
  FInputText := '';
end;

destructor TCMMsgBox.Destroy;
begin
  inherited Destroy;
end;

procedure TCMMsgBox.DoShow;
begin
  inherited;
  FMsgBoard.SelectedButtonReturnValue := FDefRe;
end;

procedure TCMMsgBox.DoClose(var CloseAction: TCloseAction);
begin
  if Assigned(FInputEdt) then
    FInputText := FInputEdt.Text;
  inherited DoClose(CloseAction);
end;

function TCMMsgBox.MessageBox(const AText, ACaption: string; AFlags: Longint; ADefRe: Integer): Integer;
begin
  FDefRe := ADefRe;
  try
    Result := inherited MessageBox(AText, ACaption, AFlags);
  finally
    FDefRe := -1;
  end;
end;

function TCMMsgBox.InputBox(const ACaption, APrompt, ADefault: string): string;
begin
  Result := ADefault;
  FInputEdt := TEdit.Create(Self);
  FInputEdt.Font := FMsgBoard.DefaultMsgFont;
  FInputEdt.Text := ADefault;
  FMsgBoard.AddMsg(APrompt);
  FMsgBoard.RegisterMsgAreaFocusControl(FInputEdt);
  FMsgBoard.AddMsgAreaControl(FInputEdt);
  if Self.MessageBox('', ACaption, MB_OKCANCEL) = IDOK then
    Result := FInputText;
end;

{$ENDIF}




end.




