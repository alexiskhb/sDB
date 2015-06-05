unit conflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ValEdit, ExtCtrls, db, sqldb;

type

  TCellConflicts = record
    TeachersConf: array of array of boolean;
    GroupsConf: array of array of boolean;
    ClassroomsConf: array of array of boolean;
  end;

  { TConflictsCheckForm }

  TConflictsCheckForm = class(TForm)
    ListBox1: TListBox;
    ListBox2: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    FTeachersInCell: TStringList;
    FGroupsInCell: TStringList;
    FClassroomsInCell: TStringList;
  public
    SQLQuery: TSQLQuery;
    CellConflicts: array of array of TCellConflicts;
    procedure CheckTeacher(TeacherID, RecordID: Variant; x, y: integer);
    procedure CheckGroup(GroupID, RecordID: Variant; x, y: integer);
    procedure CheckClassroom(ClassroomID, RecordID: Variant; x, y: integer);
    procedure CheckTeachersCourses();
    procedure CheckGroupsCourses();
    procedure SetCurrentCellConflictColors(X, Y: integer);
    function Conflicted(aCol, aRow, Z: integer): boolean;
    procedure Clear;
  end;

var
  ConflictsCheckForm: TConflictsCheckForm;

implementation

{$R *.lfm}

procedure TConflictsCheckForm.FormCreate(Sender: TObject);
begin
  FTeachersInCell := TStringList.Create;
  FGroupsInCell := TStringList.Create;
  FClassroomsInCell := TStringList.Create;
end;

procedure TConflictsCheckForm.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin

end;

procedure TConflictsCheckForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TConflictsCheckForm.Clear;
begin
  FTeachersInCell.Clear;
  FGroupsInCell.Clear;
  FClassroomsInCell.Clear;
end;

procedure TConflictsCheckForm.CheckTeacher(TeacherID, RecordID: Variant; x, y: integer);
var
  i: integer;
begin
  with FTeachersInCell do begin
    SetLength(CellConflicts[y, x].TeachersConf, Count + 1, Count + 1);
    CellConflicts[y, x].TeachersConf[Count, Count] := false;
    for i := 0 to Count - 1 do
      if Strings[i] = string(TeacherID) then begin
        CellConflicts[y, x].TeachersConf[i, Count] := true;
        CellConflicts[y, x].TeachersConf[Count, i] := true;
        CellConflicts[y, x].TeachersConf[i, i] := true;
        CellConflicts[y, x].TeachersConf[Count, Count] := true;
      end else begin
        CellConflicts[y, x].TeachersConf[i, Count] := false;
        CellConflicts[y, x].TeachersConf[Count, i] := false;
      end;
    AddObject(TeacherID, TObject(Pointer(Integer(RecordID))));
  end;
end;

procedure TConflictsCheckForm.CheckGroup(GroupID, RecordID: Variant; x, y: integer);
var
  i: integer;
begin
  with FGroupsInCell do begin
    SetLength(CellConflicts[y, x].GroupsConf, Count + 1, Count + 1);
    CellConflicts[y, x].GroupsConf[Count, Count] := false;
    for i := 0 to Count - 1 do
      if Strings[i] = string(GroupID) then begin
        CellConflicts[y, x].GroupsConf[i, Count] := true;
        CellConflicts[y, x].GroupsConf[Count, i] := true;
        CellConflicts[y, x].GroupsConf[i, i] := true;
        CellConflicts[y, x].GroupsConf[Count, Count] := true;
      end else begin
        CellConflicts[y, x].GroupsConf[i, Count] := false;
        CellConflicts[y, x].GroupsConf[Count, i] := false;
      end;
    AddObject(GroupID, TObject(Pointer(Integer(RecordID))));
  end;
end;

procedure TConflictsCheckForm.CheckClassroom(ClassroomID, RecordID: Variant; x, y: integer);
var
  i: integer;
begin
  with FClassroomsInCell do begin
    SetLength(CellConflicts[y, x].ClassroomsConf, Count + 1, Count + 1);
    CellConflicts[y, x].ClassroomsConf[Count, Count] := false;
    for i := 0 to Count - 1 do
      if Strings[i] = string(ClassroomID) then begin
        CellConflicts[y, x].ClassroomsConf[i, Count] := true;
        CellConflicts[y, x].ClassroomsConf[Count, i] := true;
        CellConflicts[y, x].ClassroomsConf[i, i] := true;
        CellConflicts[y, x].ClassroomsConf[Count, Count] := true;
      end else begin
        CellConflicts[y, x].ClassroomsConf[i, Count] := false;
        CellConflicts[y, x].ClassroomsConf[Count, i] := false;
      end;
    AddObject(ClassroomID, TObject(Pointer(Integer(RecordID))));
  end;
end;

procedure TConflictsCheckForm.CheckTeachersCourses();
begin

end;

procedure TConflictsCheckForm.CheckGroupsCourses();
begin

end;

procedure TConflictsCheckForm.SetCurrentCellConflictColors(X, Y: integer);
begin

end;

function TConflictsCheckForm.Conflicted(aCol, aRow, Z: integer): boolean;
begin
  with CellConflicts[aRow, aCol] do
    exit((Z < Length(TeachersConf)) and (TeachersConf[Z, Z] or GroupsConf[Z, Z] or ClassroomsConf[Z, Z]));
end;

end.

