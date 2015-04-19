unit edit_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, connection_transaction, metadata, DBGrids, sqldb, Dialogs;

procedure DeleteRecord(ATable: TDBTable; AGrid: TDBGrid);
procedure UpdateRecord(ATable: TDBTable; AGrid: TDBGrid);

implementation

procedure DeleteRecord(ATable: TDBTable; AGrid: TDBGrid);
var
  SQLQuery: TSQLQuery;
  i: integer;
begin
  SQLQuery := AGrid.DataSource.DataSet as TSQLQuery;

  ConTran.CommonSQLQuery.Close;

  with ConTran.CommonSQLQuery.SQL do begin
    Clear;
    Append('delete from ' + ATable.Name);
    Append('where 1 = 1');
    for i := 0 to High(ATable.Fields) do
      Append('and ' + ATable.Name + '.' + ATable.Fields[i].Name + ' = :P' + IntToStr(i));
	end;

	for i := 0 to High(ATable.Fields) do
    ConTran.CommonSQLQuery.Params[i].Value :=
      (SQLQuery.FieldByName(ATable.Name + ATable.Fields[i].Name).Value);

  ConTran.CommonSQLQuery.ExecSQL;
end;

procedure UpdateRecord(ATable: TDBTable; AGrid: TDBGrid);
begin

end;

end.

