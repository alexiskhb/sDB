unit metadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db;

type

  TDBField = class
  private
    FFieldType: TFieldType;
    FName: string;
    FCaption: string;
    FWidth: integer;
    function GetFieldName: string;
    function GetFieldCaption: string;
    function GetFieldType: TFieldType;
    function GetFieldWidth: integer;
    constructor Create(aName, aCaption: string; aWidth: integer; aType: TFieldType);
  public
    property Name: string read GetFieldName;
    property Caption: string read GetFieldCaption;
    property FieldType: TFieldType read GetFieldType;
    property Width: integer read GetFieldWidth;
  end;

  TDBFieldDynArray = array of TDBField;

  TDBTable = class
  private
    FName: string;
    FCaption: string;
    FFields: TDBFieldDynArray;
    function GetTableName: string;
    function GetTableCaption: string;
    constructor Create(aName, aCaption: string);
  public
    property Fields: TDBFieldDynArray read FFields write FFields;
    property Name: string read GetTableName;
    property Caption: string read GetTableCaption;
    procedure AddField(aName, aCaption: string; aWidth: integer; aType: TFieldType);
    class procedure Add(aName, aCaption: string);
  end;

  TDBTableDynArray = array of TDBTable;

var
  DBTables: TDBTableDynArray;

implementation

procedure TDBTable.AddField(aName, aCaption: string; aWidth: integer; aType: TFieldType);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := TDBField.Create(aName, aCaption, aWidth, aType);
end;

class procedure TDBTable.Add(aName, aCaption: string);
begin
  SetLength(DBTables, Length(DBTables) + 1);
  DBTables[High(DBTables)] := TDBTable.Create(aName, aCaption);
end;

constructor TDBTable.Create(aName, aCaption: string);
begin
  FName := aName;
  FCaption := aCaption;
end;

function TDBTable.GetTableName: string;
begin
  Result := FName;
end;

function TDBTable.GetTableCaption: string;
begin
  Result := FCaption;
end;

constructor TDBField.Create(aName, aCaption: string; aWidth: integer; aType: TFieldType);
begin
  FName := aName;
  FCaption := aCaption;
  FWidth := aWidth;
  FFieldType := aType;
end;

function TDBField.GetFieldName: string;
begin
  Result := FName;
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

  TDBTable.Add('teachers', 'Преподаватели');
  DBTables[0].AddField('ID', 'ИД', 40, ftInteger);
  DBTables[0].AddField('NAME', 'Имя', 300, ftString);

  TDBTable.Add('Groups', 'Группы');
  DBTables[1].AddField('ID', 'ИД', 40, ftInteger);
  DBTables[1].AddField('NAME', 'Группа', 100, ftString);

  TDBTable.Add('Courses', 'Дисциплины');
  DBTables[2].AddField('ID', 'ИД', 40, ftInteger);
  DBTables[2].AddField('NAME', 'Дисциплина', 300, ftString);

  TDBTable.Add('Groups_Courses', 'Дисц. групп');
  DBTables[3].AddField('group_id', 'Ид. группы', 80, ftInteger);
  DBTables[3].AddField('course_id', 'Ид. предмета', 80, ftInteger);

  TDBTable.Add('Classrooms', 'Аудитории');
  DBTables[4].AddField('ID', 'ИД', 40, ftInteger);
  DBTables[4].AddField('classroom', 'Аудитория', 100, ftString);

  TDBTable.Add('WeekDays', 'Дни недели');
  DBTables[5].AddField('ID', 'ИД', 40, ftInteger);
  DBTables[5].AddField('WeekDay', 'День недели', 100, ftString);

  TDBTable.Add('Pairs', 'Период зан.');
  DBTables[6].AddField('ID', 'Пара', 40, ftInteger);
  DBTables[6].AddField('period', 'Время занятия', 300, ftString);

  TDBTable.Add('Teachers_Courses', 'Дисц. препод.');
  DBTables[7].AddField('teacher_id', 'Ид. преподавателя', 80, ftInteger);
  DBTables[7].AddField('course_id', 'Ид. предмета', 80, ftInteger);

  TDBTable.Add('lessons', 'Расписание');
  DBTables[8].AddField('pair_id', 'Пара', 40, ftInteger);
  DBTables[8].AddField('weekday_id', 'День недели', 70, ftInteger);
  DBTables[8].AddField('group_id', 'Группа', 50, ftInteger);
  DBTables[8].AddField('course_id', 'Предмет', 70, ftInteger);
  DBTables[8].AddField('class_id', 'Аудитория', 70, ftInteger);
  DBTables[8].AddField('teacher_id', 'Преподаватель', 70, ftInteger);

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

