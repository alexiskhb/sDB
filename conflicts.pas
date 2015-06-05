unit conflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ValEdit, ExtCtrls, db, sqldb, metadata, CheckLst, record_cards;

type

  T3DPoint = record
    X, Y, Z: integer;
  end;

  TCellIdentifier = record
    id: integer;
  end;

  TCellConflicts = record
    TeachersConf: array of array of boolean;
    GroupsConf: array of array of boolean;
    ClassroomsConf: array of array of boolean;
  end;

  { TConflictsCheckForm }

  TConflictsCheckForm = class(TForm)
    LeftListBox: TListBox;
    RightListBox: TListBox;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    Splitter: TSplitter;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LeftListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ListBoxDblClick(Sender: TObject);
  private
    FTeachersInCell: TStringList;
    FGroupsInCell: TStringList;
    FClassroomsInCell: TStringList;
    FRecordCell: array of T3DPoint;
  public
    SQLQuery: TSQLQuery;
    Records: array of array of array of TCellIdentifier;
    CellConflicts: array of array of TCellConflicts;
    Table: TDBTable;
    CheckList: TCheckListBox;
    procedure CheckTeacher(TeacherID, RecordID: Variant; x, y: integer);
    procedure CheckGroup(GroupID, RecordID: Variant; x, y: integer);
    procedure CheckClassroom(ClassroomID, RecordID: Variant; x, y: integer);
    procedure CheckTeachersCourses();
    procedure CheckGroupsCourses();
    procedure SetCurrentCellConflictColors(X, Y: integer);
    procedure AddConfRecord(x, y, z, RecordID: integer);
    function GetRecord(RecordID: integer): string;
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

procedure TConflictsCheckForm.LeftListBoxSelectionChange(Sender: TObject;
  User: boolean);
var
  i, x, y, z, N: integer;
begin
  RightListBox.Clear;
  x := FRecordCell[LeftListBox.ItemIndex].X;
  y := FRecordCell[LeftListBox.ItemIndex].Y;
  z := FRecordCell[LeftListBox.ItemIndex].Z;
  N := Length(CellConflicts[y, x].TeachersConf);
  RightListBox.AddItem('Не поделили преподавателя:', nil);
  for i := 0 to N - 1 do
    if (z <> i) and CellConflicts[y, x].TeachersConf[z, i] then
      RightListBox.AddItem(GetRecord(Records[y, x, i].id), TObject(Pointer(Integer(Records[y, x, i].id))));
  RightListBox.AddItem('Не поделили группу:', nil);
  for i := 0 to N - 1 do
    if (z <> i) and CellConflicts[y, x].GroupsConf[z, i] then
      RightListBox.AddItem(GetRecord(Records[y, x, i].id), TObject(Pointer(Integer(Records[y, x, i].id))));
  RightListBox.AddItem('Не поделили аудиторию:', nil);
  for i := 0 to N - 1 do
    if (z <> i) and CellConflicts[y, x].ClassroomsConf[z, i] then
      RightListBox.AddItem(GetRecord(Records[y, x, i].id), TObject(Pointer(Integer(Records[y, x, i].id))));
  i := 0;
  while i < RightListBox.Count do begin
    if RightListBox.Items.Objects[i] = nil then
      if (i = RightListBox.Count - 1) or (RightListBox.Items.Objects[i + 1] = nil) then begin
        RightListBox.Items.Delete(i);
        dec(i);
      end;
    inc(i);
  end;
end;

procedure TConflictsCheckForm.ListBoxDblClick(Sender: TObject);
var
  ListBox: TListBox;
  Posit, x, y, z: integer;
begin
  ListBox := Sender as TListBox;
  Posit := LeftListBox.Items.IndexOfObject(ListBox.Items.Objects[ListBox.ItemIndex]);
  x := FRecordCell[Posit].X;
  y := FRecordCell[Posit].Y;
  z := FRecordCell[Posit].Z;
  CardsManager.EditTable(Table, Records[y, x, z].id, atUpdate);
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
  SetLength(FRecordCell, 0);
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

procedure TConflictsCheckForm.AddConfRecord(x, y, z, RecordID: integer);
begin
  LeftListBox.AddItem(GetRecord(RecordID), TObject(Pointer(Integer(RecordID))));
  SetLength(FRecordCell, Length(FRecordCell) + 1);
  FRecordCell[High(FRecordCell)].X := x;
  FRecordCell[High(FRecordCell)].Y := y;
  FRecordCell[High(FRecordCell)].Z := z;
end;

function TConflictsCheckForm.GetRecord(RecordID: integer): string;
var
  i: integer;
  res: string;
  Field: TDBField;
begin
  res := ' ';
  SQLQuery.Locate(Table.Name + 'id', Variant(RecordId), []);
  for i := 0 to CheckList.Count - 1 do begin
    if not CheckList.Checked[i] then continue;
    Field := CheckList.Items.Objects[i] as TDBField;
    res := res + SQLQuery.FieldByName(Field.TableOwner.Name + Field.Name).Value + '  ';
  end;
  exit(res);
end;

function TConflictsCheckForm.Conflicted(aCol, aRow, Z: integer): boolean;
begin
  with CellConflicts[aRow, aCol] do
    exit((Z < Length(TeachersConf)) and (TeachersConf[Z, Z] or GroupsConf[Z, Z] or ClassroomsConf[Z, Z]));
end;

end.

