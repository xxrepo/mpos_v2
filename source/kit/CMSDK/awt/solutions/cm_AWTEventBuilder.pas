unit cm_AWTEventBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_controlutils,
  cm_AWT;

type

  { TControlEvent }

  TControlEvent = class(TCMEvent, IControlEvent)
  private
    FAControl: TAControl;
  public
    constructor Create(ASource: TObject; AAControl: TAControl);
    function GetAControl: TAControl;
  end;

  { TWinControlEvent }

  TWinControlEvent = class(TControlEvent, IWinControlEvent)
  public
    constructor Create(ASource: TObject; AAWinControl: TAWinControl);
    function GetAWinControl: TAWinControl;
  end;

  { TCustomControlEvent }

  TCustomControlEvent = class(TWinControlEvent, ICustomControlEvent)
  public
    constructor Create(ASource: TObject; AACustomControl: TACustomControl);
    function GetACustomControl: TACustomControl;
  end;

  { TListBoxEvent }

  TListBoxEvent = class(TWinControlEvent, IListBoxEvent)
  public
    constructor Create(ASource: TObject; AAListBox: TAListBox);
    function GetAListBox: TAListBox;
  end;

  { TComboBoxEvent }

  TComboBoxEvent = class(TWinControlEvent, IComboBoxEvent)
  public
    constructor Create(ASource: TObject; AAComboBox: TAComboBox);
    function GetAComboBox: TAComboBox;
  end;

  { TEditEvent }

  TEditEvent = class(TWinControlEvent, IEditEvent)
  public
    constructor Create(ASource: TObject; AAEdit: TACustomEdit);
    function GetAEdit: TACustomEdit;
  end;

  { TFormEvent }

  TFormEvent = class(TCustomControlEvent, IFormEvent)
  public
    constructor Create(ASource: TObject; AAForm: TAForm);
    function GetAForm: TAForm;
  end;

  { TGridEvent }

  TGridEvent = class(TCustomControlEvent, IGridEvent)
  private
    FCol, FRow: Integer;
  public
    constructor Create(ASource: TObject; AAGrid: TACustomGrid; ACol, ARow: Integer);
    function GetAGrid: TACustomGrid;
    function GetCol: Integer;
    function GetRow: Integer;
  end;

  //------------------------------------- 以上为控件事件 -------------------------------------------

  { TKeyEvent }

  TKeyEvent = class(TControlEvent, IKeyEvent)
  private
    FKeyChar: Char;
    FKeyCode: Word;
    FKeyText: string;
    FShiftState: TShiftState;
  public
    constructor Create(ASource: TObject; AAControl: TAControl; AChar: Char; AShift: TShiftState);
    constructor Create(ASource: TObject; AAControl: TAControl; ACode: Word; AShift: TShiftState);
    function GetKeyChar: Char;
    function GetKeyCode: Word;
    procedure SetKeyChar(AKeyChar: Char);
    procedure SetKeyCode(AKeyCode: Word);
    function GetKeyText: string; //返回按键的描述。
    function GetShiftState: TShiftState;
  end;

  { TMouseEvent }

  TMouseEvent = class(TControlEvent, IMouseEvent)
  private
    FPoint: TPoint;
    FPointOnScreen: TPoint;
    FWheelDelta: Integer;
    FButton: TMouseButton;
    FShiftState: TShiftState;
  public
    constructor Create(ASource: TObject; AAControl: TAControl; APoint, APointOnScreen: TPoint; AShift: TShiftState);
    constructor Create(ASource: TObject; AAControl: TAControl; APoint, APointOnScreen: TPoint; AShift: TShiftState; AMouseButton: TMouseButton);
    constructor Create(ASource: TObject; AAControl: TAControl; APoint, APointOnScreen: TPoint; AShift: TShiftState; AWheelDelta: Integer);
  public
    function GetPoint: TPoint;
    function GetX: Integer;
    function GetY: Integer;
    function GetXOnScreen: Integer;
    function GetYOnScreen: Integer;
    function GetWheelDelta: Integer;
    function GetButton: TMouseButton;
    function GetShiftState: TShiftState;
  end;

implementation

{ TControlEvent }

constructor TControlEvent.Create(ASource: TObject; AAControl: TAControl);
begin
  inherited Create(ASource);
  FAControl := AAControl;
end;

function TControlEvent.GetAControl: TAControl;
begin
  Result := FAControl;
end;

{ TWinControlEvent }

constructor TWinControlEvent.Create(ASource: TObject; AAWinControl: TAWinControl);
begin
  inherited Create(ASource, AAWinControl);
end;

function TWinControlEvent.GetAWinControl: TAWinControl;
begin
  Result := TAWinControl(FAControl);
end;

{ TCustomControlEvent }

constructor TCustomControlEvent.Create(ASource: TObject; AACustomControl: TACustomControl);
begin
  inherited Create(ASource, AACustomControl);
end;

function TCustomControlEvent.GetACustomControl: TACustomControl;
begin
  Result := TACustomControl(FAControl);
end;

{ TListBoxEvent }

constructor TListBoxEvent.Create(ASource: TObject; AAListBox: TAListBox);
begin
  inherited Create(ASource, AAListBox);
end;

function TListBoxEvent.GetAListBox: TAListBox;
begin
  Result := TAListBox(FAControl);
end;

{ TComboBoxEvent }

constructor TComboBoxEvent.Create(ASource: TObject; AAComboBox: TAComboBox);
begin
  inherited Create(ASource, AAComboBox);
end;

function TComboBoxEvent.GetAComboBox: TAComboBox;
begin
  Result := TAComboBox(FAControl);
end;

{ TEditEvent }

constructor TEditEvent.Create(ASource: TObject; AAEdit: TACustomEdit);
begin
  inherited Create(ASource, AAEdit);
end;

function TEditEvent.GetAEdit: TACustomEdit;
begin
  Result := TACustomEdit(FAControl);
end;

{ TFormEvent }

constructor TFormEvent.Create(ASource: TObject; AAForm: TAForm);
begin
  inherited Create(ASource, AAForm);
end;

function TFormEvent.GetAForm: TAForm;
begin
  Result := TAForm(FAControl);
end;

{ TGridEvent }

constructor TGridEvent.Create(ASource: TObject; AAGrid: TACustomGrid; ACol, ARow: Integer);
begin
  inherited Create(ASource, AAGrid);
  FCol := ACol;
  FRow := ARow;
end;

function TGridEvent.GetAGrid: TACustomGrid;
begin
  Result := TACustomGrid(FAControl);
end;

function TGridEvent.GetCol: Integer;
begin
  Result := FCol;
end;

function TGridEvent.GetRow: Integer;
begin
  Result := FRow;
end;

{ TKeyEvent }

constructor TKeyEvent.Create(ASource: TObject; AAControl: TAControl; AChar: Char; AShift: TShiftState);
begin
  inherited Create(ASource, AAControl);
  SetKeyChar(AChar);
  FShiftState := AShift;
end;

constructor TKeyEvent.Create(ASource: TObject; AAControl: TAControl; ACode: Word; AShift: TShiftState);
begin
  inherited Create(ASource, AAControl);
  SetKeyCode(ACode);
  FShiftState := AShift;
end;

function TKeyEvent.GetKeyChar: Char;
begin
  Result := FKeyChar;
end;

function TKeyEvent.GetKeyCode: Word;
begin
  Result := FKeyCode;
end;

// TODO 轮换
procedure TKeyEvent.SetKeyChar(AKeyChar: Char);
begin
  FKeyChar := AKeyChar;
  FKeyCode := Ord(AKeyChar);
  if (FKeyChar in ['a'..'z']) or (FKeyChar in ['A'..'Z']) or (FKeyChar in ['0'..'9']) or (FKeyChar in [#0, #7..#13]) then
    FKeyText := GetKeyCodeText(FKeyCode)
  else
    FKeyText := '';
end;

// TODO 轮换
procedure TKeyEvent.SetKeyCode(AKeyCode: Word);
begin
  FKeyCode := AKeyCode;
  FKeyChar := Char(AKeyCode);
  FKeyText := GetKeyCodeText(FKeyCode);
end;

function TKeyEvent.GetKeyText: string;
begin
  Result := FKeyText;
end;

function TKeyEvent.GetShiftState: TShiftState;
begin
  Result := FShiftState;
end;

{ TMouseEvent }

constructor TMouseEvent.Create(ASource: TObject; AAControl: TAControl; APoint, APointOnScreen: TPoint; AShift: TShiftState)
  ;
begin
  inherited Create(ASource, AAControl);
  FPoint := APoint;
  FPointOnScreen := APointOnScreen;
  FWheelDelta := 0;
  FButton := mbUnknown;
  FShiftState := AShift;
end;

constructor TMouseEvent.Create(ASource: TObject; AAControl: TAControl; APoint, APointOnScreen: TPoint; AShift: TShiftState; AMouseButton: TMouseButton);
begin
  inherited Create(ASource, AAControl);
  FPoint := APoint;
  FPointOnScreen := APointOnScreen;
  FWheelDelta := 0;
  FButton := AMouseButton;
  FShiftState := AShift;
end;

constructor TMouseEvent.Create(ASource: TObject; AAControl: TAControl; APoint, APointOnScreen: TPoint; AShift: TShiftState; AWheelDelta: Integer);
begin
  FPoint := APoint;
  FPointOnScreen := APointOnScreen;
  FWheelDelta := AWheelDelta;
  FButton := mbUnknown;
  FShiftState := AShift;
end;

function TMouseEvent.GetPoint: TPoint;
begin
  Result := FPoint;
end;

function TMouseEvent.GetX: Integer;
begin
  Result := FPoint.X;
end;

function TMouseEvent.GetY: Integer;
begin
  Result := FPoint.Y;
end;

function TMouseEvent.GetXOnScreen: Integer;
begin
  Result := FPointOnScreen.X;
end;

function TMouseEvent.GetYOnScreen: Integer;
begin
  Result := FPointOnScreen.Y;
end;

function TMouseEvent.GetWheelDelta: Integer;
begin
  Result := FWheelDelta;
end;

function TMouseEvent.GetButton: TMouseButton;
begin
  Result := FButton;
end;

function TMouseEvent.GetShiftState: TShiftState;
begin
  Result := FShiftState;
end;




end.

