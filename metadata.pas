unit metadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, dialogs, typinfo;

type

  TTableName = (teachers, groups, courses, groups_courses, classrooms, weekdays,
    pairs, teachers_courses, lessons);

  TFieldName = (id, name, group_id, course_id, classroom, weekday, period,
    teacher_id, pair_id, weekday_id, class_id);

  TDBField = class
  private
    FFieldType: TFieldType;
    FName: TFieldName;
    FCaption: string;
    FWidth: integer;
    FVisible: boolean;
    FTableRef: integer;
    function GetFieldName: string;
    function GetFieldCaption: string;
    function GetFieldType: TFieldType;
    function GetFieldWidth: integer;
  public
    property Name: string read GetFieldName;
    property Caption: string read GetFieldCaption;
    property FieldType: TFieldType read GetFieldType;
    property Width: integer read GetFieldWidth;
    property Visible: boolean read FVisible write FVisible;
    constructor Create(aName: TFieldName; aCaption: string; aTableRef, aWidth: integer;
      aType: TFieldType; aVisible: boolean);
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
    property Caption: string read GetTableCaption;
    function NumberOfTable(aTableName: TTableName): integer;
    procedure AddField(aName: TFieldName; aCaption: string; aWidth: integer;
      aType: TFieldType; aVisible: boolean); overload;
    procedure AddField(aName: TFieldName; aCaption: string; aTableName: TTableName; aWidth: integer;
      aType: TFieldType; aVisible: boolean); overload;
    class procedure Add(aName: TTableName; aCaption: string);
  end;

  TDBTableDynArray = array of TDBTable;

var
  DBTables: TDBTableDynArray;

implementation

procedure TDBTable.AddField(aName: TFieldName; aCaption: string; aWidth: integer;
  aType: TFieldType; aVisible: boolean);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := TDBField.Create(aName, aCaption, Self.FTag, aWidth, aType, aVisible);
end;

procedure TDBTable.AddField(aName: TFieldName; aCaption: string; aTableName: TTableName; aWidth: integer;
  aType: TFieldType; aVisible: boolean);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := TDBField.Create(aName, aCaption, NumberOfTable(aTableName),
    aWidth, aType, aVisible);
end;

function TDBTable.NumberOfTable(aTableName: TTableName): integer;
var
  i: integer;
begin
  for i := Low(DBTables) to High(DBTables) do
    if (DBTables[i].FName = aTableName) then exit(i);
  Result := -1;
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

constructor TDBField.Create(aName: TFieldName; aCaption: string; aTableRef, aWidth: integer;
  aType: TFieldType; aVisible: boolean);
begin
  FName := aName;
  FCaption := aCaption;
  FWidth := aWidth;
  FFieldType := aType;
  FVisible := aVisible;
  FTableRef := aTableRef;
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
  DBTables[3].AddField(group_id, 'Ид. группы', Groups , 80, ftInteger, true);
  DBTables[3].AddField(course_id, 'Ид. предмета', courses ,80, ftInteger, true);

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
  DBTables[7].AddField(teacher_id, 'Ид. преподавателя', teachers, 80, ftInteger, true);
  DBTables[7].AddField(course_id, 'Ид. предмета', courses, 80, ftInteger, true);

  TDBTable.Add(lessons, 'Расписание');
  DBTables[8].AddField(pair_id, 'Пара', pairs, 40, ftInteger, true);
  DBTables[8].AddField(weekday_id, 'День недели', weekdays, 70, ftInteger, true);
  DBTables[8].AddField(group_id, 'Группа', groups, 50, ftInteger, true);
  DBTables[8].AddField(course_id, 'Предмет', courses, 70, ftInteger, true);
  DBTables[8].AddField(class_id, 'Аудитория', classrooms, 70, ftInteger, true);
  DBTables[8].AddField(teacher_id, 'Преподаватель', teachers, 70, ftInteger, true);

end.

{
groups_courses
select g.name gr, c.name cr from groups_courses gc
inner join groups g on g.id = gc.group_id
inner join courses c on c.id = gc.course_id

teachers_courses
select t.name tr, c.name cr from teachers_courses tc
inner join courses c on c.id = tc.course_id
inner join teachers t on t.id = tc.teacher_id

lessons
select wd.weekday, p.id, p.period, g.name, c.name, cr.classroom, t.name
from lessons l
inner join pairs p on p.id = l.pair_id
inner join weekdays wd on wd.id = l.weekday_id
inner join groups g on g.id = l.group_id
inner join courses c on c.id = l.course_id
inner join classrooms cr on cr.id = l.class_id
inner join teachers t on t.id = l.teacher_id
order by wd.id, p.id, g.id
}

