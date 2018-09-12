unit uAuthorization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, cm_messager, cm_Parameter, uMPOS;

type

  { TSystemRegister }

  TSystemRegister = class(TCMMessageableComponent)
  private
    FCompanyArray: TStrings;
    class function DoVerification(AParam: ICMParameter): boolean;
  public
    class function Verification(AParam: ICMParameter): boolean;
    class function DoRegedit(): boolean;
  end;

const
  NodeName = 'Authlicense';
  {$IFDEF UNIX}
  FileName = 'config\Authlicense.xml';
  {$ELSE}
  FileName = 'config/Authlicense.xml';
  {$ENDIF}

implementation

uses
  uFormRegister;

{ TSystemRegister }

class function TSystemRegister.Verification(AParam: ICMParameter): boolean;
var
  AuthParam: ICMParameter;
begin
  Result := False;
  AuthParam := AParam.Get(NodeName);

  if not (AuthParam.IsNull) then
    if Self.DoVerification(AuthParam) then
    begin
      Result := True;
      Exit;
    end;

  Result := Self.DoRegedit();
end;

class function TSystemRegister.DoVerification(AParam: ICMParameter): boolean;
var
  AuthParam: ICMParameter;
  compCode, shopCode, termCode, termUUID, autoCode: string;
begin
  Result := False;
  //注册,然后保存注册信息
  //ap.AddString('compCode', 'GD');
  //ap.AddString('shopCode', '0198');
  //ap.AddString('termCode', '019828');
  //ap.AddString('termUUID', '3261343236386135346263623430356239383866383564663131386631316161');
  //ap.AddString('autoCode', '11a4de6039507da7452e17062ffe7a24');

  if AParam.IsNull then
    Self.DoRegedit();

end;

class function TSystemRegister.DoRegedit(): boolean;
var
  Form: TFormRegister;
begin
  Result := False;
  Form := TFormRegister.Create(nil);
  try
    if (Form.ShowModal() = mrOk) then
      Result := True;
  finally
    FreeAndNil(Form);
  end;

end;

end.














