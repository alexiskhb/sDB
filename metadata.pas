unit metadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, dialogs, typinfo;

type

  TDBTable = class;
  TDBField = class;

  TDBField = class
  private
    FFieldType: TFieldType;
    FName: string;
    FCaption: string;
    FWidth: integer;
    FVisible: boolean;
    FTableRef: TDBTable;
    FFieldRef: TDBField;
    FOwner: TDBTable;
  public
    property Name: string read FName;
    property Caption: string read FCaption;
    property FieldType: TFieldType read FFieldType;
    property Width: integer read FWidth write FWidth;
    property Visible: boolean read FVisible write FVisible;
    property TableRef: TDBTable read FTableRef;
    property FieldRef: TDBField read FFieldRef;
    property Owner: TDBTable read FOwner;
    constructor Create(AOwner: TDBTable; AName, ACaption, ATableRef, AFieldRef: string;
    AWidth: integer; AFieldType: TFieldType; AVisible: boolean); overload;
    constructor Create(AOwner: TDBTable; AName, ACaption: string; AWidth: integer; AFieldType:
        TFieldType; AVisible: boolean); overload;
  end;

  TDBFieldDynArray = array of TDBField;

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
      AFieldType: TFieldType; AVisible: boolean); overload;
    procedure AddField(AName, ACaption, ATableRef, AFieldRef: string;
      AWidth: integer; AFieldType: TFieldType; AVisible: boolean); overload;
    class procedure Add(AName, ACaption: string);
    constructor Create(AName, ACaption: string);
  end;

  TDBTableDynArray = array of TDBTable;

var
  DBTables: TDBTableDynArray;
  DBTablesList: TStringList;

implementation

procedure TDBTable.AddField(AName, ACaption: string; AWidth: integer;
  AFieldType: TFieldType; AVisible: boolean);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFieldsList.AddObject(AName, TDBField.Create(Self, AName, ACaption, AWidth, AFieldType, AVisible));
  FFields[High(FFields)] := (FFieldsList.Objects[FFieldsList.Count - 1] as TDBField);
end;

procedure TDBTable.AddField(AName, ACaption, ATableRef, AFieldRef: string;
    AWidth: integer; AFieldType: TFieldType; AVisible: boolean);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFieldsList.AddObject(AName, TDBField.Create(Self, AName, ACaption, ATableRef, AFieldRef,
    AWidth, AFieldType, AVisible));
  FFields[High(FFields)] := (FFieldsList.Objects[FFieldsList.Count - 1] as TDBField);
end;

class procedure TDBTable.Add(AName, ACaption: string);
begin
  SetLength(DBTables, Length(DBTables) + 1);
  DBTablesList.AddObject(AName, TDBTable.Create(AName, ACaption));
  DBTables[High(DBTables)] := (DBTablesList.Objects[DBTablesList.Count - 1] as TDBTable);
  DBTables[High(DBTables)].FTag := High(DBTables);
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
    AWidth: integer; AFieldType: TFieldType; AVisible: boolean);
begin
  FName := AName;
  FCaption := ACaption;
  FWidth := AWidth;
  FFieldType := AFieldType;
  FVisible := AVisible;
  FOwner := AOwner;
  FTableRef := (DBTablesList.Objects[DBTablesList.IndexOf(ATableRef)] as TDBTable);
  FFieldRef := (FTableRef.FFieldsList.Objects[FTableRef.FFieldsList.IndexOf(AFieldRef)] as TDBField);
end;

constructor TDBField.Create(AOwner: TDBTable; AName, ACaption: string;
    AWidth: integer; AFieldType: TFieldType; AVisible: boolean);
begin
  FName := AName;
  FCaption := ACaption;
  FWidth := AWidth;
  FFieldType := AFieldType;
  FVisible := AVisible;
  FOwner := AOwner;
end;

initialization

  DBTablesList := TStringList.Create;
  with DBTablesList do begin
    Sorted := false;
    Duplicates := dupError;
  end;

  TDBTable.Add('teachers', 'Преподаватели');
  DBTables[0].AddField('id', 'ИД', 40, ftInteger, false);
  DBTables[0].AddField('name', 'Имя', 300, ftString, true);

  TDBTable.Add('groups', 'Группы');
  DBTables[1].AddField('id', 'ИД', 40, ftInteger, false);
  DBTables[1].AddField('name', 'Группа', 100, ftString, true);

  TDBTable.Add('courses', 'Дисциплины');
  DBTables[2].AddField('id', 'ИД', 40, ftInteger, false);
  DBTables[2].AddField('name', 'Дисциплина', 300, ftString, true);

  TDBTable.Add('groups_courses', 'Дисц. групп');
  DBTables[3].AddField('group_id', 'Ид. группы', 'Groups', 'id', 80, ftInteger, false);
  DBTables[3].AddField('course_id', 'Ид. предмета', 'courses', 'id', 80, ftInteger, false);

  TDBTable.Add('classrooms', 'Аудитории');
  DBTables[4].AddField('id', 'ИД', 40, ftInteger, false);
  DBTables[4].AddField('classroom', 'Аудитория', 100, ftString, true);

  TDBTable.Add('weekdays', 'Дни недели');
  DBTables[5].AddField('id', 'День', 40, ftInteger, false);
  DBTables[5].AddField('weekday', 'День недели', 100, ftString, true);

  TDBTable.Add('pairs', 'Период зан.');
  DBTables[6].AddField('ID', 'Пара', 40, ftInteger, true);
  DBTables[6].AddField('period', 'Время занятия', 100, ftString, true);

  TDBTable.Add('teachers_courses', 'Дисц. препод.');
  DBTables[7].AddField('teacher_id', 'Ид. преподавателя', 'teachers', 'id', 80, ftInteger, false);
  DBTables[7].AddField('course_id', 'Ид. предмета', 'courses', 'id', 80, ftInteger, false);

  TDBTable.Add('lessons', 'Расписание');
  DBTables[8].AddField('pair_id', 'Пара', 'pairs', 'id', 40, ftInteger, false);
  DBTables[8].AddField('weekday_id', 'День недели', 'weekdays', 'id', 70, ftInteger, false);
  DBTables[8].AddField('group_id', 'Группа', 'groups', 'id', 50, ftInteger, false);
  DBTables[8].AddField('course_id', 'Предмет', 'courses', 'id', 70, ftInteger, false);
  DBTables[8].AddField('class_id', 'Аудитория', 'classrooms', 'id', 70, ftInteger, false);
  DBTables[8].AddField('teacher_id', 'Преподаватель', 'teachers', 'id', 70, ftInteger, false);

end.

















