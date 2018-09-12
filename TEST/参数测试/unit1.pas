unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  cm_parameter,
  cm_ParameterUtils,
  cm_SqliteDB, cm_DB, DB;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure println(const msg: string);
  public

  end;

var
  Form1: TForm1;
  pObj: TCMParameter;
  pIntf: ICMParameter;



implementation

{$R *.frm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  Self.Left := 0;
end;

procedure TForm1.println(const msg: string);
begin
  Memo1.Lines.Add(msg);
end;

//////////////////////

procedure TForm1.Button1Click(Sender: TObject);
var
  i, j: Integer;
begin
  pObj := TCMParameter.Create(nil, 'aa', 'aavvvvvvvvvvvv');
  for j:=0 to 9  do
    begin
      for i:=0 to 10000 do
        begin
          pIntf := TCMParameter.Create(pObj, 'bb' + IntToStr(i), 'aavvvvvvvvvvvv');
          pIntf.AddString('xx', 'aaa').AddString('vv', 'vvvvvv');
          pIntf.RemoveItems;
        end;
      //TCMParameter.Create(pObj, 'xx', 'xxxxvvv');
      //pObj.AddString('xx2', 'xx22 vv').ReDateTime(now);
      //println( pObj.Get('xx').AsString );
      //println( pObj.Get('xx2').AsString );
      //
      //println( pObj.Get('xx2.cc').AsString );

      println(IntToStr(pObj.ItemCount));
      pObj.RemoveItems;
      println(IntToStr(pObj.ItemCount));
    end;
  pObj.Free;
  println('-------------------');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i, j: Integer;
  tp: ICMParameter;
begin
  pIntf := TCMParameter.Create(nil, 'aa', 'aa v');
  for i:=0 to 200 do
    begin
      tp := pIntf.AddString('bb' + IntToStr(i), 'bb' + IntToStr(i) + ' v');
      for j:=0 to 200 do
        begin
          tp.AddString('cc' + IntToStr(j), 'cc' + IntToStr(j) + ' v');
        end;
    end;
  println( pIntf.Get('bb1').AsString );
  println( pIntf.Get('bb1').GetItem(1).AsString );

  println(IntToStr(pIntf.Get('bb1').ItemCount));
  println( pIntf.Get('bb1').AddString('xx', 'xx v').AsString );
  println(IntToStr(pIntf.Get('bb1').ItemCount));
  println('------------------->');
  tp := pIntf.Get('bb1');
  println( tp.AsString );
  //pIntf.RemoveItems;
  println( pIntf.Get('bb1').AsString );

  println('-------------------');
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  conn: TCMSQLiteConnection;
  sm: ICMStatement;
  ds: TDataSet;
  i: Integer;
begin
  conn := TCMSQLiteConnection.Create(Self);
  sm := conn.CreateStatement;
  ds := sm.Query('select * from parameter;');
  while not ds.EOF do
    begin
      println(ds.FieldByName('value').AsString);
      ds.Next;
    end;
  pObj := TCMParameter.Create(nil, 'zz', 'zz v');
  pObj.AddString('qq', 'qq v');
  println(pObj.Get('qq').AsString);

  i := pObj.ParameterSet.AddParameters(pObj.Get('qq'), ds);
  println(IntToStr(i));
  println('>>-----------------');
  println( pObj.Get('w').AsString );
  println( pObj.Get('w.aa').AsString );
  println( pObj.Get('bb1').AsString );
  println( pObj.Get('w.aa.bb1').AsString );
  println( pObj.Get('qq.aa.bb2').AsString );
  println( pObj.Get('w.aa.bb2.cc1').AsString );
  println( pObj.Get('qq.xx').AsString );

  println( pObj.ParameterSet.GetParameter('xx').AsString );

  //for i:=0 to pObj.ItemCount-1 do
  //  begin
  //    println(pObj.GetItem(i).AsString);
  //  end;
  //println(IntToStr(pObj.Get('w').ItemCount));
  //println(IntToStr(pObj.ItemCount));

  println('-------------------');
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  ps: ICMParameterDataList;
begin
  ps := TCMParameterDataList.Create;
  ps.SetString('aaa', 'aaa v');
  ps.SetDateTime('bb', now);

  println(IntToStr(ps.Count));
  println(ps.Get('aaa').AsString);
  println(ps.Get('bb').AsString);
  println('-------------------');
end;


end.

