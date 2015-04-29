unit edit_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, connection_transaction, metadata, DBGrids, sqldb, Dialogs;

procedure DeleteRecord(ATable: TDBTable; APrimaryKey: integer);
procedure UpdateRecord(ATable: TDBTable; APrimaryKey: integer; NewValues: TVariantDynArray);
procedure InsertRecord(ATable: TDBTable; APrimaryKey: integer; AValues: TVariantDynArray);

implementation

procedure DeleteRecord(ATable: TDBTable; APrimaryKey: integer);
begin
  ConTran.CommonSQLQuery.Close;
  with ConTran.CommonSQLQuery do begin
    SQL.Clear;
    SQL.Append('delete from ' + ATable.Name);
    SQL.Append('where ' + ATable.Name + '.id = :primary_key');
    ParamByName('primary_key').Value := APrimaryKey;
	end;

  try
    ConTran.CommonSQLQuery.ExecSQL;
  except
    on EIBDatabaseError: Exception do
      MessageDlg('Невозможно удалить запись.' + #13+#10
               + 'Возможно, она используется в:' + #13+#10
               + TDBTable.TablesUsingTable(ATable), mtError, [mbOk], 0);
  end;

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

  try
    ConTran.CommonSQLQuery.ExecSQL;
	except
    on EDatabaseError: Exception do
      MessageDlg('Ошибка.' + #13 + #10
                 + 'Возможно, такая запись уже существует.' + #13 + #10
                 , mtError, [mbOk], 0);
    on EVariantError: Exception do
      MessageDlg('Невозможно добавить запись.' + #13 + #10
                 + 'Данные введены некорректно.' + #13 + #10
                 , mtError, [mbOk], 0);
	end;

  ConTran.DBTransaction.Commit;
end;

procedure InsertRecord(ATable: TDBTable; APrimaryKey: integer; AValues: TVariantDynArray);
var
  i: integer;
begin
  AValues[0] := APrimaryKey;
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

  try
    ConTran.CommonSQLQuery.ExecSQL;
	except
    on EVariantError: Exception do
      MessageDlg('Невозможно добавить запись.' + #13 + #10
                 + 'Данные введены некорректно.' + #13 + #10
                 , mtError, [mbOk], 0);
    on EDatabaseError: Exception do
      MessageDlg('Невозможно добавить запись.' + #13 + #10
                 + 'Такая запись уже существует.' + #13 + #10
                 , mtError, [mbOk], 0);
	end;

  ConTran.DBTransaction.Commit;
end;

end.










