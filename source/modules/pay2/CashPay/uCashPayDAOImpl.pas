unit uCashPayDAOImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCashPayDAO;

type

  { TCashPayRecordDAO }

  TCashPayRecordDAO = class(TPOSDAO, ICashPayRecordDAO)
  public
    function Save(APayRequestCash: TPayRecordCash): boolean;
    function Get(AAssignUUID: string): TPayRecordCash;
  end;

implementation

{ TCashPayRecordDAO }


function TCashPayRecordDAO.Save(APayRequestCash: TPayRecordCash): boolean;
begin

end;

function TCashPayRecordDAO.Get(AAssignUUID: string): TPayRecordCash;
begin

end;

end.

