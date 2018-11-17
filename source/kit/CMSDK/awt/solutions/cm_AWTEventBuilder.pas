unit cm_AWTEventBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_interfaces, cm_AWT;

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

  { TFormEvent }

  TFormEvent = class(TCustomControlEvent, IFormEvent)
  public
    constructor Create(ASource: TObject; AAForm: TAForm);
    function GetAForm: TAForm;
  end;

  { TKeyEvent }

  TKeyEvent = class(TControlEvent, IKeyEvent)
  private
    FKeyChar: Char;
    FKeyCode: Word;
  public
    constructor Create(ASource: TObject; AAControl: TAControl; AChar: Char);
    constructor Create(ASource: TObject; AAControl: TAControl; ACode: Word);
    function GetKeyChar: Char;
    function GetKeyCode: Word;
    procedure SetKeyChar(AKeyChar: Char);
    procedure SetKeyCode(AKeyCode: Word);
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

{ TFormEvent }

constructor TFormEvent.Create(ASource: TObject; AAForm: TAForm);
begin
  inherited Create(ASource, AAForm);
end;

function TFormEvent.GetAForm: TAForm;
begin
  Result := TAForm(FAControl);
end;

{ TKeyEvent }

constructor TKeyEvent.Create(ASource: TObject; AAControl: TAControl; AChar: Char);
begin
  inherited Create(ASource, AAControl);
  SetKeyChar(AChar);
end;

constructor TKeyEvent.Create(ASource: TObject; AAControl: TAControl; ACode: Word);
begin
  inherited Create(ASource, AAControl);
  SetKeyCode(ACode);
end;

function TKeyEvent.GetKeyChar: Char;
begin
  Result := FKeyChar;
end;

function TKeyEvent.GetKeyCode: Word;
begin
  Result := FKeyCode;
end;

procedure TKeyEvent.SetKeyChar(AKeyChar: Char);
begin
  FKeyChar := AKeyChar;
  FKeyCode := Ord(AKeyChar);
end;

procedure TKeyEvent.SetKeyCode(AKeyCode: Word);
begin
  FKeyCode := AKeyCode;
  FKeyChar := Char(AKeyCode)
end;

end.

