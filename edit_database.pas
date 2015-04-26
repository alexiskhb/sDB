unit edit_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, connection_transaction, metadata, DBGrids, sqldb, Dialogs;

procedure DeleteRecord(ATable: TDBTable; AGrid: TDBGrid);
procedure UpdateRecord(ATable: TDBTable; APrimaryKey: integer; NewValues: TVariantDynArray);
procedure InsertRecord(ATable: TDBTable; APrimaryKey: integer; AValues: TVariantDynArray);

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

  //try
    ConTran.CommonSQLQuery.ExecSQL;
  //except
  //  on EIBDatabaseError: Exception do
  //    MessageDlg('Невозможно удалить запись.' + #13+#10
  //             + 'Возможно, она используется в:' + #13+#10
  //             + TDBTable.TablesUsingTable(ATable), mtError, [mbOk], 0);
  //end;

	ConTran.DBTransaction.Commit;
end;

procedure UpdateRecord(ATable: TDBTable; APrimaryKey: integer; NewValues: TVariantDynArray);
var
  i: integer;
begin
  ConTran.CommonSQLQuery.Close;
  with ConTran.CommonSQLQuery.SQL do begin
    Clear;
    Append('update ' + ATable.Name);
    Append('set ');

    for i := 1 to High(NewValues) do begin
      Append(ATable.Fields[i].Name + ' = :new_value' + IntToStr(i));
      Append(',');
		end;
    Delete(Count - 1);

    Append('where ' + ATable.Name + '.id = :primary_key');


    Append(';');
	end;

  with ConTran.CommonSQLQuery do begin
    for i := 1 to High(NewValues) do
      ParamByName('new_value' + IntToStr(i)).Value := NewValues[i];

    ParamByName('primary_key').Value := APrimaryKey;
  end;

  //try
    ConTran.CommonSQLQuery.ExecSQL;
	//except
 //   on EDatabaseError: Exception do
 //     MessageDlg('Ошибка.' + #13+#10
 //              + 'Возможно, такая запись уже существует.', mtError, [mbOk], 0);
	//end;

  ConTran.DBTransaction.Commit;
end;

procedure InsertRecord(ATable: TDBTable; APrimaryKey: integer; AValues: TVariantDynArray);
var
  i: integer;
begin
  //ShowMessage(IntToStr(Length(AValues)));
  ConTran.CommonSQLQuery.Close;
  with ConTran.CommonSQLQuery.SQL do begin
    Clear;
    Append('insert into  ' + ATable.Name + ' values');
    Append('(');
    for i := 0 to High(AValues) do begin
      Append(' :par' + IntToStr(i));
      Append(',');
		end;
    Delete(Count - 1);
    Append(');');
	end;

	for i := 0 to High(AValues) do
    ConTran.CommonSQLQuery.ParamByName('par'+IntToStr(i)).Value := AValues[i];

  //try
    ConTran.CommonSQLQuery.ExecSQL;
	//except
 //   on EDatabaseError: Exception do
 //     MessageDlg('Невозможно добавить запись.' + #13 + #10
 //                + 'Такая запись уже существует' + #13 + #10
 //                + 'либо данные введены некорректно.', mtError, [mbOk], 0);
	//end;

  ConTran.DBTransaction.Commit;
end;

end.










