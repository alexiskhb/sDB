unit edit_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, connection_transaction, metadata, DBGrids, sqldb, Dialogs;

function DeleteRecord(ATable: TDBTable; APrimaryKey: integer): integer;
function UpdateRecord(ATable: TDBTable; APrimaryKey: integer; AValues: TVariantDynArray): integer;
function InsertRecord(ATable: TDBTable; APrimaryKey: integer; AValues: TVariantDynArray): integer;

implementation

function DeleteRecord(ATable: TDBTable; APrimaryKey: integer): integer;
begin
  Result := 0;
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
    on EIBDatabaseError: Exception do begin
      MessageDlg('Невозможно удалить запись.' + #13+#10
               + 'Возможно, она используется в:' + #13+#10
               + TDBTable.TablesUsingTable(ATable), mtError, [mbOk], 0);
      Result := 1;
    end;
  end;

  ConTran.DBTransaction.Commit;
end;

function UpdateRecord(ATable: TDBTable; APrimaryKey: integer; AValues: TVariantDynArray): integer;
var
  i: integer;
begin
  Result := 0;
  ConTran.CommonSQLQuery.Close;
  with ConTran.CommonSQLQuery.SQL do begin
    Clear;
    Append('update ' + ATable.Name + ' set');
    for i := 1 to High(AValues) do begin
      Append(ATable.Fields[i].Name + ' = :value' + IntToStr(i));
      Append(',');
		end;
    Delete(Count - 1);
    Append('where ' + ATable.Name + '.id = :primary_key ;');
  end;

  with ConTran.CommonSQLQuery do begin
    for i := 1 to High(AValues) do
      ParamByName('value' + IntToStr(i)).Value := AValues[i];
    ParamByName('primary_key').Value := APrimaryKey;
  end;

  try
    ConTran.CommonSQLQuery.ExecSQL;
  except
    on EVariantError: Exception do begin
      MessageDlg('Невозможно изменить запись.' + #13 + #10
                 + 'Данные введены некорректно.' + #13 + #10
                 , mtError, [mbOk], 0);
      Result := 2;
    end;
    on EDatabaseError: Exception do begin
      MessageDlg('Ошибка.' + #13 + #10
                 + 'Возможно, такая запись уже существует.' + #13 + #10
                 , mtError, [mbOk], 0);
      Result := 1;
    end;
  end;

  ConTran.DBTransaction.Commit;
end;

function InsertRecord(ATable: TDBTable; APrimaryKey: integer; AValues: TVariantDynArray): integer;
var
  i: integer;
begin
  Result := 0;
  AValues[0] := APrimaryKey;
  ConTran.CommonSQLQuery.Close;
  with ConTran.CommonSQLQuery.SQL do begin
    Clear;
    Append('insert into  ' + ATable.Name + ' values');
    Append('(');
    for i := 0 to High(AValues) do begin
      Append(' :value' + IntToStr(i));
      Append(',');
    end;
    Delete(Count - 1);
    Append(');');
  end;

  for i := 0 to High(AValues) do
    ConTran.CommonSQLQuery.ParamByName('value'+IntToStr(i)).Value := AValues[i];

  try
    ConTran.CommonSQLQuery.ExecSQL;
  except
    on EVariantError: Exception do begin
      MessageDlg('Невозможно добавить запись.' + #13 + #10
                 + 'Данные введены некорректно.' + #13 + #10
                 , mtError, [mbOk], 0);
      Result := 1;
    end;
    on EDatabaseError: Exception do begin
      MessageDlg('Невозможно добавить запись.' + #13 + #10
                 + 'Такая запись уже существует.' + #13 + #10
                 , mtError, [mbOk], 0);
      Result := 2;
    end;
  end;

  ConTran.DBTransaction.Commit;
end;

end.










