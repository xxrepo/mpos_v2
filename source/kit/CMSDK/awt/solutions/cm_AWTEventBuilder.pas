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
    class function BuildControlEvent(ASource: TObject; AAControl: TAControl): TControlEvent;
    function GetAControl: TAControl;
  end;

  { TKeyEvent }

  TKeyEvent = class(TCMEvent, IKeyEvent)
  private
    FKeyChar: Char;
    FKeyCode: Word;
  public
    class function BuildKeyEvent(ASource: TObject; AChar: Char; ACode: Word): TKeyEvent;
    function GetKeyChar: Char;
    function GetKeyCode: Word;
    procedure SetKeyChar(AKeyChar: Char);
    procedure SetKeyCode(AKeyCode: Word);
  end;

implementation

{ TControlEvent }

class function TControlEvent.BuildControlEvent(ASource: TObject; AAControl: TAControl): TControlEvent;
begin
  Result := TControlEvent.Create(ASource);
  Result.FAControl := AAControl;
end;

function TControlEvent.GetAControl: TAControl;
begin
  Result := FAControl;
end;

{ TKeyEvent }

class function TKeyEvent.BuildKeyEvent(ASource: TObject; AChar: Char; ACode: Word): TKeyEvent;
begin
  Result := TKeyEvent.Create(ASource);
  Result.SetKeyChar(AChar);
  Result.SetKeyCode(ACode);
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
end;

procedure TKeyEvent.SetKeyCode(AKeyCode: Word);
begin
  FKeyCode := AKeyCode;
end;

end.

