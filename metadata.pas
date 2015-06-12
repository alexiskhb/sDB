unit metadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, dialogs, typinfo, connection_transaction, sqldb, StdCtrls, Types;

type

  TDBTable = class;
  TDBField = class;

  TDBField = class
  private
    FFieldType: TFieldType;
    FName: string;
    FCaption: string;
    FWidth: integer;
    FPrimaryKey: boolean;
    FTableRef: TDBTable;
    FFieldRef: TDBField;
    FOwner: TDBTable;
    FVarCharLimit: integer;
    FSortField: TDBField;
  public
    property Name: string read FName;
    property Caption: string read FCaption;
    property FieldType: TFieldType read FFieldType;
    property Width: integer read FWidth write FWidth;
    property Visible: boolean read FPrimaryKey write FPrimaryKey;
    property TableRef: TDBTable read FTableRef;
    property FieldRef: TDBField read FFieldRef;
    property SortField: TDBField read FSortField write FSortField;
    property VarCharLimit: integer read FVarCharLimit;
    property TableOwner: TDBTable read FOwner;
    procedure RowsTo(AComboBox: TComboBox);
    constructor Create(AOwner: TDBTable; AName, ACaption, ATableRef, AFieldRef: string;
    AWidth: integer; AFieldType: TFieldType; APrimaryKey: boolean; AVarCharLimit: integer); overload;
    constructor Create(AOwner: TDBTable; AName, ACaption: string; AWidth: integer; AFieldType:
        TFieldType; APrimaryKey: boolean; AVarCharLimit: integer); overload;
  end;

  TDBFieldDynArray = array of TDBField;
  TVariantDynArray = array of Variant;

  TDBTable = class
  private
    FName: string;
    FCaption: string;
    FFields: TDBFieldDynArray;
    FFieldsList: TStringList;
    FTag: integer;
  public
    property Fields: TDBFieldDynArray read FFields write FFields;
    property Name: string read FName;
    property Caption: string read FCaption;
    procedure AddField(AName, ACaption: string; AWidth: integer;
      AFieldType: TFieldType; AVisible: boolean; AVarCharLimit: integer); overload;
    procedure AddField(AName, ACaption, ATableRef, AFieldRef: string;
      AWidth: integer; AFieldType: TFieldType; AVisible: boolean; AVarCharLimit: integer); overload;
    class procedure Add(AName, ACaption: string);
    class function TablesUsingTable(ATable: TDBTable): string;
    constructor Create(AName, ACaption: string);
  end;

  TDBTableDynArray = array of TDBTable;

var
  DBTables: TDBTableDynArray;
  DBTablesList: TStringList;
  DBTimeTable: TDBTable;

  procedure SetSQLQuery(ATable: TDBTable; SQLQuery: TSQLQuery);
  procedure AllQueryColumnsToList(ATable: TDBTable; var ASList: TStringList);
  function NextID: integer;

implementation

procedure AddColumnsToQuery(ATable: TDBTable; SQLQuery: TSQLQuery); forward;

procedure TDBField.RowsTo(AComboBox: TComboBox);
begin
  with ConTran.CommonSQLQuery do begin
    Close;
    SetSQLQuery(Self.TableOwner, ConTran.CommonSQLQuery);
    Open;
    First;
    AComboBox.Clear;
    while not EOF do begin
      AComboBox.AddItem(
                    FieldByName(Self.TableOwner.Name + Self.Name).Value,
                    TObject(Pointer(FieldByName(Self.TableOwner.Name + 'id').AsInteger))
                    );
      Next;
    end;
  end;
end;

function NextID: integer;
begin
  with ConTran.CommonSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Append('select next value for global_sequence');
    SQL.Append('from RDB$DATABASE');
    Open;
    Result := Fields[0].Value;
  end;
end;

procedure SetSQLQuery(ATable: TDBTable; SQLQuery: TSQLQuery);
var
  i: integer;
begin
  with SQLQuery.SQL do begin
    Clear;
    Append('select');
    AddColumnsToQuery(ATable, SQLQuery);
    Delete(Count - 1);
    Append('from');
    Append(ATable.Name + ' ');
    with ATable do
      for i := Low(Fields) to High(Fields) do
        if Assigned(Fields[i].TableRef) then begin
          Append('join ' + Fields[i].TableRef.Name + ' on ');
          Append('  ' + Fields[i].TableRef.Name + '.' + Fields[i].FieldRef.Name + ' = ');
          Append('    ' + Name + '.' + Fields[i].Name);
        end;
  end;
end;

procedure AddColumnsToQuery(ATable: TDBTable; SQLQuery: TSQLQuery);
var
  i: integer;
begin
  with SQLQuery.SQL, ATable do
    for i := 0 to High(Fields) do begin
      Append(Name + '.' + Fields[i].Name + ' as ' + Name + Fields[i].Name);
      Append(',');
      if Assigned(Fields[i].TableRef) then
        AddColumnsToQuery(Fields[i].TableRef, SQLQuery);
    end;
end;

procedure AllQueryColumnsToList(ATable: TDBTable; var ASList: TStringList);
var
  i: integer;
begin
  with ATable do
    for i := 0 to High(Fields) do begin
      ASList.AddObject(ATable.Name + Fields[i].Name, Fields[i]);
      if Assigned(Fields[i].TableRef) then
        AllQueryColumnsToList(Fields[i].TableRef, ASList);
    end;
end;

procedure TDBTable.AddField(AName, ACaption: string; AWidth: integer;
  AFieldType: TFieldType; AVisible: boolean; AVarCharLimit: integer);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFieldsList.AddObject(AName, TDBField.Create(Self, AName, ACaption, AWidth, AFieldType, AVisible, AVarCharLimit));
  FFields[High(FFields)] := (FFieldsList.Objects[FFieldsList.Count - 1] as TDBField);
end;

procedure TDBTable.AddField(AName, ACaption, ATableRef, AFieldRef: string;
    AWidth: integer; AFieldType: TFieldType; AVisible: boolean; AVarCharLimit: integer);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFieldsList.AddObject(AName, TDBField.Create(Self, AName, ACaption, ATableRef, AFieldRef,
    AWidth, AFieldType, AVisible, AVarCharLimit));
  FFields[High(FFields)] := (FFieldsList.Objects[FFieldsList.Count - 1] as TDBField);
end;

class procedure TDBTable.Add(AName, ACaption: string);
begin
  SetLength(DBTables, Length(DBTables) + 1);
  DBTablesList.AddObject(AName, TDBTable.Create(AName, ACaption));
  DBTables[High(DBTables)] := (DBTablesList.Objects[DBTablesList.Count - 1] as TDBTable);
  DBTables[High(DBTables)].FTag := High(DBTables);
end;

class function TDBTable.TablesUsingTable(ATable: TDBTable): string;
var
  i, j: integer;
begin
  for i := 0 to High(DBTables) do
    for j := 0 to High(DBTables[i].Fields) do
      if DBTables[i].Fields[j].TableRef = ATable then
        if Pos(DBTables[i].Caption, Result) = 0 then
          Result := Result + DBTables[i].Caption + #13 + #10;
end;

constructor TDBTable.Create(AName, ACaption: string);
begin
  FName := AName;
  FCaption := ACaption;
  FFieldsList := TStringList.Create;
  with FFieldsList do begin
    Sorted := false;
    Duplicates := dupError;
  end;
end;

constructor TDBField.Create(AOwner: TDBTable; AName, ACaption, ATableRef, AFieldRef: string;
    AWidth: integer; AFieldType: TFieldType; APrimaryKey: boolean; AVarCharLimit: integer);
begin
  FName := AName;
  FCaption := ACaption;
  FWidth := AWidth;
  FFieldType := AFieldType;
  FPrimaryKey := APrimaryKey;
  FOwner := AOwner;
  FSortField := Self;
  FVarCharLimit := AVarCharLimit;
  FTableRef := (DBTablesList.Objects[DBTablesList.IndexOf(ATableRef)] as TDBTable);
  FFieldRef := (FTableRef.FFieldsList.Objects[FTableRef.FFieldsList.IndexOf(AFieldRef)] as TDBField);
end;

constructor TDBField.Create(AOwner: TDBTable; AName, ACaption: string;
    AWidth: integer; AFieldType: TFieldType; APrimaryKey: boolean; AVarCharLimit: integer);
begin
  FName := AName;
  FCaption := ACaption;
  FWidth := AWidth;
  FFieldType := AFieldType;
  FPrimaryKey := APrimaryKey;
  FSortField := Self;
  FOwner := AOwner;
  FVarCharLimit := AVarCharLimit;
end;

initialization

  DBTablesList := TStringList.Create;
  with DBTablesList do begin
    Sorted := false;
    Duplicates := dupError;
  end;

  TDBTable.Add('teachers', 'Преподаватели');
  DBTables[0].AddField('id', 'П. ИД', 40, ftInteger, false, 0);
  DBTables[0].AddField('name', 'Преподаватель', 300, ftString, true, 50);

  TDBTable.Add('groups', 'Группы');
  DBTables[1].AddField('id', 'Г. ИД', 40, ftInteger, false, 0);
  DBTables[1].AddField('name', 'Группа', 100, ftString, true, 50);
  DBTables[1].AddField('st_number', 'Количество', 60, ftInteger, true, 0);

  TDBTable.Add('courses', 'Дисциплины');
  DBTables[2].AddField('id', 'ИД', 40, ftInteger, false, 0);
  DBTables[2].AddField('name', 'Дисциплина', 300, ftString, true, 50);

  TDBTable.Add('groups_courses', 'Дисц. групп');
  DBTables[3].AddField('id', 'Ид. отношения', 40, ftInteger, false, 0);
  DBTables[3].AddField('group_id', 'Ид. группы', 'Groups', 'id', 80, ftInteger, false, 0);
  DBTables[3].AddField('course_id', 'Ид. предмета', 'courses', 'id', 80, ftInteger, false, 0);

  TDBTable.Add('classrooms', 'Аудитории');
  DBTables[4].AddField('id', 'ИД', 40, ftInteger, false, 0);
  DBTables[4].AddField('classroom', 'Аудитория', 100, ftString, true, 50);
  DBTables[4].AddField('capacity', 'Вместимость', 60, ftInteger, true, 0);

  TDBTable.Add('weekdays', 'Дни недели');
  DBTables[5].AddField('id', 'День', 40, ftInteger, false, 0);
  DBTables[5].AddField('weekday', 'День недели', 100, ftString, true, 15);
  DBTables[5].Fields[1].SortField := DBTables[5].Fields[0];

  TDBTable.Add('pairs', 'Период зан.');
  DBTables[6].AddField('id', 'Пара', 40, ftInteger, false, 0);
  DBTables[6].AddField('period', 'Время занятия', 100, ftString, true, 50);
  DBTables[6].Fields[1].SortField := DBTables[6].Fields[0];

  TDBTable.Add('teachers_courses', 'Дисц. препод.');
  DBTables[7].AddField('id', 'Ид. отношения', 40, ftInteger, false, 0);
  DBTables[7].AddField('teacher_id', 'Ид. преподавателя', 'teachers', 'id', 80, ftInteger, false, 0);
  DBTables[7].AddField('course_id', 'Ид. предмета', 'courses', 'id', 80, ftInteger, false, 0);

  TDBTable.Add('lessons', 'Расписание');
  DBTimeTable := DBTables[8];
  DBTables[8].AddField('id', 'Ид. записи', 40, ftInteger, false, 0);
  DBTables[8].AddField('pair_id', 'Пара', 'pairs', 'id', 40, ftInteger, false, 0);
  DBTables[8].AddField('weekday_id', 'Ид. дня недели', 'weekdays', 'id', 70, ftInteger, false, 0);
  DBTables[8].AddField('group_id', 'Ид. группы', 'groups', 'id', 50, ftInteger, false, 0);
  DBTables[8].AddField('course_id', 'Ид. предмета', 'courses', 'id', 70, ftInteger, false, 0);
  DBTables[8].AddField('class_id', 'Ид. аудитории', 'classrooms', 'id', 70, ftInteger, false, 0);
  DBTables[8].AddField('teacher_id', 'Ид. преподавателя', 'teachers', 'id', 70, ftInteger, false, 0);

end.

















