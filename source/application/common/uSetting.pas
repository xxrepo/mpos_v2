unit uSetting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, ExtCtrls,
  cm_messager,
  uApp, uFormMenu;


type

  { TPOSSetting }

  TPOSSetting = class(TCMMessageableComponent)
  private
    FMenuForm: TMenuForm;
    procedure DblClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property MenuForm: TMenuForm read FMenuForm;
  end;

implementation

{ TPOSSetting }

constructor TPOSSetting.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMenuForm := TMenuForm.Create(Self);
  FMenuForm.BorderStyle := bsNone;
  FMenuForm.BoundsRect := AppSystem.GetWorkRect;
  FMenuForm.SetTitle('设置');

  FMenuForm.AddMenuItem('主题设置');
  FMenuForm.AddItem('设置2', nil);
  FMenuForm.AddItem('设置3', nil);
  FMenuForm.AddItem('设置4', nil);
  FMenuForm.AddItem('设置5', nil);
  FMenuForm.AddItem('设置6', nil);
  FMenuForm.AddItem('设置7', nil);
end;

procedure TPOSSetting.DblClick(Sender: TObject);
begin
  //
end;



end.

