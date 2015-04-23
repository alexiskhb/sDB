unit edit_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, connection_transaction, metadata, DBGrids, sqldb, Dialogs;

procedure DeleteRecord(ATable: TDBTable; AGrid: TDBGrid);
procedure UpdateRecord(ATable: TDBTable; OldValues, NewValues: TVariantDynArray);
procedure InsertRecord(ATable: TDBTable; AValues: TVariantDynArray);

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

	for i := 0 to High(ATable.Fields) do begin
    ConTran.CommonSQLQuery.Params[i].Value :=
      SQLQuery.Fields.FieldByName(ATable.Name + ATable.Fields[i].Name).Value;
	end;

  ConTran.CommonSQLQuery.ExecSQL;
  ConTran.DBTransaction.Commit;
end;

procedure UpdateRecord(ATable: TDBTable; OldValues, NewValues: TVariantDynArray);
var
  i: integer;
begin
  ConTran.CommonSQLQuery.Close;
  with ConTran.CommonSQLQuery.SQL do begin
    Clear;
    Append('update ' + ATable.Name);
    Append('set ');

    for i := 0 to High(NewValues) do begin
      Append(ATable.Fields[i].Name + ' = :new_value' + IntToStr(i));
      Append(',');
		end;
    Delete(Count - 1);

    Append('where 1 = 1 ');
    for i := 0 to High(OldValues) do
      Append('and ' + ATable.Fields[i].Name + ' = :old_value' + IntToStr(i));

    Append(';');
	end;

  with ConTran.CommonSQLQuery do begin
    for i := 0 to High(NewValues) do
      ParamByName('new_value' + IntToStr(i)).Value := NewValues[i];
    for i := 0 to High(OldValues) do
      ParamByName('old_value' + IntToStr(i)).Value := OldValues[i];
	end;

  ConTran.CommonSQLQuery.ExecSQL;
  ConTran.DBTransaction.Commit;
end;

procedure InsertRecord(ATable: TDBTable; AValues: TVariantDynArray);
var
  i: integer;
begin
  ConTran.CommonSQLQuery.Close;
  with ConTran.CommonSQLQuery.SQL do begin
    Clear;
    Append('insert into  ' + ATable.Name + ' values');
    Append('(');
    for i := 0 to High(AValues) do begin
      Append(':P' + IntToStr(i));
      Append(',');
		end;
    Delete(Count - 1);
    Append(');');
	end;
	for i := 0 to High(AValues) do
    ConTran.CommonSQLQuery.Params[i].Value := AValues[i];

  ConTran.CommonSQLQuery.ExecSQL;
  ConTran.DBTransaction.Commit;
end;

end.










