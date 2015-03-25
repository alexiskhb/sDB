unit metadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, dialogs, typinfo;

type

  TTableName = (teachers, groups, courses, groups_courses, classrooms, weekdays,
    pairs, teachers_courses, lessons, selfreft);

  TFieldName = (id, name, group_id, course_id, classroom, weekday, period,
    teacher_id, pair_id, weekday_id, class_id, selfreff);

  TFieldNameIndexArray = array [id..class_id] of integer;

  TDBField = class
  private
    FFieldType: TFieldType;
    FName: TFieldName;
    FCaption: string;
    FWidth: integer;
    FVisible: boolean;
    FTableRef: TTableName;
    FFieldRef: TFieldName;
    function GetFieldName: string;
    function GetFieldCaption: string;
    function GetFieldType: TFieldType;
    function GetFieldWidth: integer;
    function GetTableRefName: string;
    function GetFieldRefName: string;
  public
    property Name: string read GetFieldName;
    property EnumName: TFieldName read FName;
    property Caption: string read GetFieldCaption;
    property FieldType: TFieldType read GetFieldType;
    property Width: integer read GetFieldWidth;
    property Visible: boolean read FVisible write FVisible;
    property TableRefStr: string read GetTableRefName;
    property TableRefEnum: TTableName read FTableRef;
    property FieldRefStr: string read GetFieldRefName;
    property FieldRefEnum: TFieldName read FFieldRef;
    constructor Create(aName: TFieldName; aCaption: string; aTableRef: TTableName;
        aFieldRef: TFieldName; aWidth: integer; aType: TFieldType; aVisible: boolean);
  end;

  TDBFieldDynArray = array of TDBField;

  TDBTable = class
  private
    FName: TTableName;
    FCaption: string;
    FFields: TDBFieldDynArray;
    FTag: integer;
    function GetTableName: string;
    function GetTableCaption: string;
    constructor Create(aName: TTableName; aCaption: string);
  public
    property Fields: TDBFieldDynArray read FFields write FFields;
    property Name: string read GetTableName;
    property EnumName: TTableName read FName;
    property Caption: string read GetTableCaption;
    procedure AddField(aName: TFieldName; aCaption: string; aWidth: integer;
      aType: TFieldType; aVisible: boolean); overload;
    procedure AddField(aName: TFieldName; aCaption: string; aTableName: TTableName;
        aFieldName: TFieldName; aWidth: integer; aType: TFieldType; aVisible: boolean); overload;
    class procedure Add(aName: TTableName; aCaption: string);
    class function NumByName(aTableName: TTableName): integer;
  end;

  TDBTableDynArray = array of TDBTable;

function EnumToString(aTableName: TTableName): string; overload;
function EnumToString(aFieldName: TFieldName): string; overload;

var
  DBTables: TDBTableDynArray;

implementation

procedure TDBTable.AddField(aName: TFieldName; aCaption: string; aWidth: integer;
  aType: TFieldType; aVisible: boolean);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := TDBField.Create(aName, aCaption, selfreft, selfreff,
    aWidth, aType, aVisible);
end;

procedure TDBTable.AddField(aName: TFieldName; aCaption: string; aTableName: TTableName;
    aFieldName: TFieldName; aWidth: integer; aType: TFieldType; aVisible: boolean);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := TDBField.Create(aName, aCaption, aTableName, aFieldName,
    aWidth, aType, aVisible);
end;

class procedure TDBTable.Add(aName: TTableName; aCaption: string);
begin
  SetLength(DBTables, Length(DBTables) + 1);
  DBTables[High(DBTables)] := TDBTable.Create(aName, aCaption);
  DBTables[High(DBTables)].FTag := High(DBTables);
end;

constructor TDBTable.Create(aName: TTableName; aCaption: string);
begin
  FName := aName;
  FCaption := aCaption;
end;

function TDBTable.GetTableName: string;
begin
  Result := GetEnumName(TypeInfo(TTableName), integer(FName));
end;

function TDBTable.GetTableCaption: string;
begin
  Result := FCaption;
end;

class function TDBTable.NumByName(aTableName: TTableName): integer;
var
  i: integer;
begin
  for i := Low(DBTables) to High(DBTables) do
    if DBTables[i].EnumName = aTableName then exit(i);
  Result := -1;
end;

constructor TDBField.Create(aName: TFieldName; aCaption: string; aTableRef: TTableName;
    aFieldRef: TFieldName; aWidth: integer; aType: TFieldType; aVisible: boolean);
begin
  FName := aName;
  FCaption := aCaption;
  FWidth := aWidth;
  FFieldType := aType;
  FVisible := aVisible;
  FTableRef := aTableRef;
  FFieldRef := aFieldRef;
end;

function TDBField.GetFieldName: string;
begin
  Result := GetEnumName(TypeInfo(TFieldName), integer(FName));
end;

function TDBField.GetFieldCaption: string;
begin
  Result := FCaption;
end;

function TDBField.GetFieldType: TFieldType;
begin
  Result := FFieldType;
end;

function TDBField.GetFieldWidth: integer;
begin
  Result := FWidth;
end;

function TDBField.GetTableRefName: string;
begin
  Result := GetEnumName(TypeInfo(TTableName), integer(FTableRef));
end;

function TDBField.GetFieldRefName: string;
begin
  Result := GetEnumName(TypeInfo(TFieldName), integer(FFieldRef));
end;

function EnumToString(aTableName: TTableName): string;
begin
  Result := GetEnumName(TypeInfo(TFieldName), integer(aTableName));
end;

function EnumToString(aFieldName: TFieldName): string;
begin
  Result := GetEnumName(TypeInfo(TFieldName), integer(aFieldName));
end;

initialization

  TDBTable.Add(teachers, 'Преподаватели');
  DBTables[0].AddField(id, 'ИД', 40, ftInteger, false);
  DBTables[0].AddField(name, 'Имя', 300, ftString, true);

  TDBTable.Add(Groups, 'Группы');
  DBTables[1].AddField(id, 'ИД', 40, ftInteger, false);
  DBTables[1].AddField(name, 'Группа', 100, ftString, true);

  TDBTable.Add(courses, 'Дисциплины');
  DBTables[2].AddField(id, 'ИД', 40, ftInteger, false);
  DBTables[2].AddField(name, 'Дисциплина', 300, ftString, true);

  TDBTable.Add(Groups_Courses, 'Дисц. групп');
  DBTables[3].AddField(group_id, 'Ид. группы', Groups, id, 80, ftInteger, false);
  DBTables[3].AddField(course_id, 'Ид. предмета', courses, id, 80, ftInteger, false);

  TDBTable.Add(Classrooms, 'Аудитории');
  DBTables[4].AddField(id, 'ИД', 40, ftInteger, false);
  DBTables[4].AddField(classroom, 'Аудитория', 100, ftString, true);

  TDBTable.Add(WeekDays, 'Дни недели');
  DBTables[5].AddField(id, 'ИД', 40, ftInteger, false);
  DBTables[5].AddField(weekday, 'День недели', 100, ftString, true);

  TDBTable.Add(Pairs, 'Период зан.');
  DBTables[6].AddField(ID, 'Пара', 40, ftInteger, true);
  DBTables[6].AddField(period, 'Время занятия', 300, ftString, true);

  TDBTable.Add(Teachers_Courses, 'Дисц. препод.');
  DBTables[7].AddField(teacher_id, 'Ид. преподавателя', teachers, id, 80, ftInteger, false);
  DBTables[7].AddField(course_id, 'Ид. предмета', courses, id, 80, ftInteger, false);

  TDBTable.Add(lessons, 'Расписание');
  DBTables[8].AddField(pair_id, 'Пара', pairs, id, 40, ftInteger, false);
  DBTables[8].AddField(weekday_id, 'День недели', weekdays, id, 70, ftInteger, false);
  DBTables[8].AddField(group_id, 'Группа', groups, id, 50, ftInteger, false);
  DBTables[8].AddField(course_id, 'Предмет', courses, id, 70, ftInteger, false);
  DBTables[8].AddField(class_id, 'Аудитория', classrooms, id, 70, ftInteger, false);
  DBTables[8].AddField(teacher_id, 'Преподаватель', teachers, id, 70, ftInteger, false);

end.
