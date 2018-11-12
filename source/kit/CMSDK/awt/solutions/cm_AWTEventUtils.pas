unit cm_AWTEventUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, //Generics.Defaults,
  cm_interfaces,
  cm_AWTEvent;

type

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

  { TKeyAdapter }

  TKeyAdapter = class(TCMBase, IKeyListener)
  public
    procedure KeyPressed(e: IKeyEvent); virtual;
    procedure KeyReleased(e: IKeyEvent); virtual;
    procedure KeyTyped(e: IKeyEvent); virtual;
  end;

implementation

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

{ TKeyAdapter }

procedure TKeyAdapter.KeyPressed(e: IKeyEvent);
begin
  //There's nothing to do here.
end;

procedure TKeyAdapter.KeyReleased(e: IKeyEvent);
begin
  //There's nothing to do here.
end;

procedure TKeyAdapter.KeyTyped(e: IKeyEvent);
begin
  //There's nothing to do here.
end;

end.

