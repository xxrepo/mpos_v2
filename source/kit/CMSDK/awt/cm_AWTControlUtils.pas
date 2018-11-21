unit cm_AWTControlUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  cm_AWT;


procedure SetLabelHeightAndToBottom(h: Integer; labs: array of TALabel);

implementation

procedure SetLabelHeightAndToBottom(h: Integer; labs: array of TALabel);
var
  i: Integer;
begin
  for i:=Low(labs) to High(labs) do
    begin
      labs[i].AutoSize := False;
      labs[i].Layout := tlBottom;
      labs[i].Height := h;
    end;
end;

end.

