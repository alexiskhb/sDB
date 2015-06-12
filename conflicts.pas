unit conflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ValEdit, ExtCtrls, db, sqldb, metadata, CheckLst, ComCtrls, record_cards,
  connection_transaction, types;

type

  TConflict = class;
  TConflictClass = class of TConflict;

  TConflict = class
  private
    RecordID: integer;
    ConflictID: array of TConflict;
    ConflictType: TConflictClass;
  public
    class procedure Check(LeftTree, RightTree: TTreeView);
    constructor Create(ARecordID: integer; AConflictType: TConflictClass);
  end;

  TTeacherConflict = class(TConflict)
  private
    class var FCaption: string;
  public
    class procedure Check(LeftTree, RightTree: TTreeView);
    class property Caption: string read FCaption write FCaption;
  end;

  TGroupConflict = class(TConflict)
  private
    class var FCaption: string;
  public
    class property Caption: string read FCaption write FCaption;
    class procedure Check(LeftTree, RightTree: TTreeView);
  end;

  TClassroomConflict = class(TConflict)
  private
    class var FCaption: string;
  public
    class property Caption: string read FCaption write FCaption;
    class procedure Check(LeftTree, RightTree: TTreeView);
  end;

  TCapacityConflict = class(TConflict)
  private
    class var FCaption: string;
  public
    class property Caption: string read FCaption write FCaption;
    class procedure Check(LeftTree, RightTree: TTreeView);
  end;

  TTeacherCourseConflict = class(TConflict)
  private
    class var FCaption: string;
  public
    class property Caption: string read FCaption write FCaption;
    class procedure Check(LeftTree, RightTree: TTreeView);
  end;

  TGroupCourseConflict = class(TConflict)
  private
    class var FCaption: string;
  public
    class property Caption: string read FCaption write FCaption;
    class procedure Check(LeftTree, RightTree: TTreeView);
  end;

  TCellIdentifier = record
    id: integer;
    IsConf: boolean;
  end;

  { TConflictsCheckForm }

  TConflictsCheckForm = class(TForm)
    clbVisibleFields: TCheckListBox;
    DataSource: TDataSource;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    Splitter: TSplitter;
    RightTreeView: TTreeView;
    LeftTreeView: TTreeView;
    Splitter1: TSplitter;
    SQLQuery: TSQLQuery;
    procedure clbVisibleFieldsClickCheck(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LeftTreeViewDblClick(Sender: TObject);
    procedure RightTreeViewDblClick(Sender: TObject);
    procedure RightTreeViewSelectionChanged(Sender: TObject);
    procedure clbVisibleFieldsMouseWheelUp(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure clbVisibleFieldsMouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  public
    function GetRecord(RecordID: integer): string;
    procedure AddFieldsToLists(ATable: TDBTable);
    function IsRecConf(RecordID: integer): boolean;
    procedure Refresh;
  end;

var
  ConflictsCheckForm: TConflictsCheckForm;

implementation

{$R *.lfm}

class procedure TConflict.Check(LeftTree, RightTree: TTreeView);
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  RightTree.Items.Clear;
  TTeacherConflict.Check(LeftTree, RightTree);
  TGroupConflict.Check(LeftTree, RightTree);
  TClassroomConflict.Check(LeftTree, RightTree);
  TCapacityConflict.Check(LeftTree, RightTree);
  TTeacherCourseConflict.Check(LeftTree, RightTree);
  TGroupCourseConflict.Check(LeftTree, RightTree);
end;

constructor TConflict.Create(ARecordID: integer; AConflictType: TConflictClass);
begin
  RecordID := ARecordID;
  ConflictType := AConflictType;
end;

procedure NonExistentLinks(Query: TSQLQuery; StringList, ConfObjects: TStringList;
  ConfType: TConflictClass; Field1, Field2, LinkTable: string);
var
  i, j: integer;
  Conf: TConflict;
begin
  Query.Close;
  with Query.SQL do begin
    Clear;
    Append('select l.id as lid, l.' + Field1 + ' as l'+ Field1 +', l.' + Field2 + ' as l' + Field2);
    Append('from lessons l where not exists( select *');
    Append('from ' + LinkTable + ' tab');
    Append('where tab.' + Field1 + ' = l.' + Field1 + ' and tab.' + Field2 + ' = l.' + Field2 + ')');
  end;
  with Query do begin
    Open; First;
    while not EOF do begin
      StringList.AddObject(
        FieldByName('l' + Field1).AsString + '#' +
        FieldByName('l' + Field2).AsString,
        TObject(Pointer(Integer(FieldByName('lid').AsInteger))));
      Next;
    end;
  end;
  for i := 0 to StringList.Count - 1 do
    ConfObjects.AddObject(
      IntToStr(Integer(Pointer(StringList.Objects[i]))),
      TConflict.Create(Integer(Pointer(StringList.Objects[i])), ConfType));
end;

procedure ExceptionalRows(Query: TSQLQuery; StringList, ConfObjects: TStringList;
  ConfType: TConflictClass; Field1, Field2, Field3: string);
var
  i, j: integer;
  Conf: TConflict;
begin
  Query.Close;
  with Query.SQL do begin
    Clear;
    Append('WITH sel AS');
    Append('( SELECT COUNT(*) AS cnt');
    Append(', l.' + Field1 + ' AS sel' + Field1);
    Append(', l.' + Field2 + ' AS sel' + Field2);
    Append(', l.' + Field3 + ' AS sel' + Field3);
    Append('FROM lessons l');
    Append('GROUP BY l.' + Field1 + ', l.' + Field2 + ', l.' + Field3);
    Append('HAVING COUNT(*) > 1');
    Append(') SELECT l.id AS lid');
    Append(', l.' + Field1 + ' AS l' + Field1);
    Append(', l.' + Field2 + ' AS l' + Field2);
    Append(', l.' + Field3 + ' AS l' + Field3);
    Append('FROM lessons l RIGHT JOIN sel ON');
    Append('l.' + Field1+ ' = sel' + Field1 + ' AND');
    Append('l.' + Field2+ ' = sel' + Field2 + ' AND');
    Append('l.' + Field3+ ' = sel' + Field3);
    Append('ORDER BY l.' + Field1 + ', l.' + Field2 + ', l.' + Field3);
    if ConfType = TCapacityConflict then begin
      Strings[1] := '( SELECT SUM(g.st_number) AS stsum';
      Strings[7] := 'JOIN groups g ON g.id = l.group_id';
      Strings[12] := 'FROM lessons l';
      Exchange(6, 7);
      Insert(13, 'JOIN classrooms c ON c.id = l.class_id JOIN sel ON');
      Insert(17, 'AND c.capacity < stsum');
    end;
  end;

  with Query do begin
    Open; First;
    while not EOF do begin
      StringList.AddObject(
        FieldByName('l' + Field1).AsString + '#' +
        FieldByName('l' + Field2).AsString + '##' +
        FieldByName('l' + Field3).AsString,
        TObject(Pointer(Integer(FieldByName('lid').AsInteger))));
      Next;
    end;
  end;

  for i := 0 to StringList.Count - 1 do
    ConfObjects.AddObject(
      IntToStr(Integer(Pointer(StringList.Objects[i]))),
      TConflict.Create(Integer(Pointer(StringList.Objects[i])), ConfType));

  with StringList do
    for i := 0 to Count - 1 do
      for j := 0 to Count - 1 do
        if (Strings[i] = Strings[j]) and (Objects[i] <> Objects[j]) then begin
          Conf := ConfObjects.Objects[i] as TConflict;
          SetLength(Conf.ConflictID, Length(Conf.ConflictID) + 1);
          Conf.ConflictID[High(Conf.ConflictID)] := ConfObjects.Objects[j] as TConflict;
	end;
end;

function ConflictCaption(ConfType: TConflictClass): string;
begin
  if ConfType = TTeacherConflict then Result := TTeacherConflict.Caption;
  if ConfType = TGroupConflict then Result := TGroupConflict.Caption;
  if ConfType = TClassroomConflict then Result := TClassroomConflict.Caption;
  if ConfType = TCapacityConflict then Result := TCapacityConflict.Caption;
  if ConfType = TTeacherCourseConflict then Result := TTeacherCourseConflict.Caption;
  if ConfType = TGroupCourseConflict then Result := TGroupCourseConflict.Caption;
end;

procedure AddToTrees(LeftTree, RightTree: TTreeView; AParentNode: TTreeNode;
  ConfObjects: TStringList);
var
  i, j: integer;
  Node, LeftNode: TTreeNode;
  Conf: TConflict;
begin
  with ConfObjects do begin
    for i := 0 to ConfObjects.Count - 1 do begin
      Conf := Objects[i] as TConflict;
      Node := RightTree.Items.AddChildObject(AParentNode, ConflictsCheckForm.GetRecord(Conf.RecordID), Conf);
      for j := 0 to Length(Conf.ConflictID) - 1 do
        RightTree.Items.AddChildObject(Node, ConflictsCheckForm.GetRecord(Conf.ConflictID[j].RecordID), Conf.ConflictID[j]);

      LeftNode := LeftTree.Items.FindNodeWithData(Pointer(Conf.RecordID));
      if LeftNode = nil then
        LeftNode :=
          LeftTree.Items.AddObject(TTreeNode.Create(LeftTree.Items),
          ConflictsCheckForm.GetRecord(Conf.RecordID), Pointer(Conf.RecordID));
      LeftTree.Items.AddChildObject(
        LeftNode,
        ConflictCaption(Conf.ConflictType),
        Node);
    end;
  end;
end;

class procedure TTeacherConflict.Check(LeftTree, RightTree: TTreeView);
var
  Query: TSQLQuery;
  StringList: TStringList;
  ConfObjects: TStringList;
  Node: TTreeNode;
begin
  StringList := TStringList.Create;
  ConfObjects := TStringList.Create;
  Query := ConTran.CommonSQLQuery;
  ExceptionalRows(
    Query, StringList, ConfObjects, TTeacherConflict,
    'weekday_id', 'pair_id', 'teacher_id');
  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TTeacherConflict.Caption);
  AddToTrees(LeftTree, RightTree, Node, ConfObjects);
end;

class procedure TGroupConflict.Check(LeftTree, RightTree: TTreeView);
var
  Query: TSQLQuery;
  StringList: TStringList;
  ConfObjects: TStringList;
  Node: TTreeNode;
begin
  StringList := TStringList.Create;
  ConfObjects := TStringList.Create;
  Query := ConTran.CommonSQLQuery;
  ExceptionalRows(
    Query, StringList, ConfObjects, TGroupConflict,
    'weekday_id', 'pair_id', 'group_id');
  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TGroupConflict.Caption);
  AddToTrees(LeftTree, RightTree, Node, ConfObjects);
end;

class procedure TClassroomConflict.Check(LeftTree, RightTree: TTreeView);
var
  Query: TSQLQuery;
  StringList: TStringList;
  ConfObjects: TStringList;
  Node: TTreeNode;
begin
  StringList := TStringList.Create;
  ConfObjects := TStringList.Create;
  Query := ConTran.CommonSQLQuery;
  ExceptionalRows(
    Query, StringList, ConfObjects, TClassroomConflict,
    'weekday_id', 'pair_id', 'class_id');
  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TClassroomConflict.Caption);
  AddToTrees(LeftTree, RightTree, Node, ConfObjects);
end;

class procedure TCapacityConflict.Check(LeftTree, RightTree: TTreeView);
var
  Query: TSQLQuery;
  StringList: TStringList;
  ConfObjects: TStringList;
  Node: TTreeNode;
begin
  StringList := TStringList.Create;
  ConfObjects := TStringList.Create;
  Query := ConTran.CommonSQLQuery;
  ExceptionalRows(
    Query, StringList, ConfObjects, TCapacityConflict,
    'weekday_id', 'pair_id', 'class_id');
  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TCapacityConflict.Caption);
  AddToTrees(LeftTree, RightTree, Node, ConfObjects);
end;

class procedure TTeacherCourseConflict.Check(LeftTree, RightTree: TTreeView);
var
  Query: TSQLQuery;
  StringList: TStringList;
  ConfObjects: TStringList;
  Node: TTreeNode;
begin
  StringList := TStringList.Create;
  ConfObjects := TStringList.Create;
  Query := ConTran.CommonSQLQuery;
  NonExistentLinks(
    Query, StringList, ConfObjects, TTeacherCourseConflict,
    'teacher_id', 'course_id', 'teachers_courses');
  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TTeacherCourseConflict.Caption);
  AddToTrees(LeftTree, RightTree, Node, ConfObjects);
end;

class procedure TGroupCourseConflict.Check(LeftTree, RightTree: TTreeView);
var
  Query: TSQLQuery;
  StringList: TStringList;
  ConfObjects: TStringList;
  Node: TTreeNode;
begin
  StringList := TStringList.Create;
  ConfObjects := TStringList.Create;
  Query := ConTran.CommonSQLQuery;
  NonExistentLinks(
    Query, StringList, ConfObjects, TGroupCourseConflict,
    'group_id', 'course_id', 'groups_courses');
  Node := RightTree.Items.Add(TTreeNode.Create(RightTree.Items), TGroupCourseConflict.Caption);
  AddToTrees(LeftTree, RightTree, Node, ConfObjects);
end;

procedure TConflictsCheckForm.FormCreate(Sender: TObject);
begin
  AddFieldsToLists(DBTimeTable);
end;

procedure TConflictsCheckForm.FormShow(Sender: TObject);
begin
  Refresh;
end;

procedure TConflictsCheckForm.LeftTreeViewDblClick(Sender: TObject);
var
  Node: TTreeNode;
  RightNode: TTreeNode;
begin
  Node := LeftTreeView.Selected;
  if Node.GetFirstChild <> nil then begin
    CardsManager.EditTable(DBTimeTable, Integer(Node.Data), atUpdate);
  end else begin
    RightNode := TObject(Node.Data) as TTreeNode;
    RightTreeView.Items.SelectOnlyThis(RightNode);
    RightNode.Expand(true);
    ActiveControl := RightTreeView;
  end;
end;

procedure TConflictsCheckForm.RightTreeViewDblClick(Sender: TObject);
var
  Tree: TTreeView;
  Conf: TConflict;
begin
  Tree := Sender as TTreeView;
  Conf := TObject(Tree.Selected.Data) as TConflict;
  if Conf = nil then exit;
  CardsManager.EditTable(DBTimeTable, Conf.RecordID, atUpdate);
end;

procedure TConflictsCheckForm.RightTreeViewSelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
  LeftNode: TTreeNode;
begin
  Node := RightTreeView.Selected;
  if (Node = nil) or (Node.Data = nil) then exit;
  LeftNode := LeftTreeView.Items.FindNodeWithData(Pointer((TObject(Node.Data) as TConflict).RecordID));
  LeftTreeView.Items.SelectOnlyThis(LeftNode);
end;

procedure TConflictsCheckForm.clbVisibleFieldsMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  i: integer;
begin
  for i := clbVisibleFields.Count - 1 downto 0 do
    with clbVisibleFields do
      if Selected[i] and (i < Count - 1) then begin
        clbVisibleFields.Items.Move(i, i + 1);
        clbVisibleFields.Selected[i + 1] := true;
      end;
end;

procedure TConflictsCheckForm.clbVisibleFieldsMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  i: integer;
begin
  for i := 0 to clbVisibleFields.Count - 1 do
    with clbVisibleFields do
      if Selected[i] and (i > 0) then begin
        clbVisibleFields.Items.Move(i, i - 1);
        clbVisibleFields.Selected[i - 1] := true;
      end;
end;

procedure TConflictsCheckForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TConflictsCheckForm.clbVisibleFieldsClickCheck(Sender: TObject);
begin
  Refresh;
end;

procedure TConflictsCheckForm.Refresh;
begin
  LeftTreeView.Items.Clear;
  RightTreeView.Items.Clear;
  with SQLQuery do begin
    Close;
    SetSQLQuery(DBTimeTable, SQLQuery);
    Open;
  end;
  TConflict.Check(LeftTreeView, RightTreeView);
end;

function TConflictsCheckForm.GetRecord(RecordID: integer): string;
var
  Field: TDBField;
  i: integer;
begin
  Result := '';
  SQLQuery.Locate(DBTimeTable.Name + 'id', Integer(RecordId), []);
  for i := 0 to clbVisibleFields.Count - 1 do begin
    if not clbVisibleFields.Checked[i] then continue;
    Field := clbVisibleFields.Items.Objects[i] as TDBField;
    Result := Result + string(SQLQuery.FieldByName(Field.TableOwner.Name + Field.Name).Value) + '  ';
  end;
end;

procedure TConflictsCheckForm.AddFieldsToLists(ATable: TDBTable);
var
  i: integer;
begin
  with ATable do
    for i := 0 to High(Fields) do begin
      if Fields[i].Visible and not Fields[i].Secondary then begin
        clbVisibleFields.AddItem(Fields[i].Caption, Fields[i]);
        clbVisibleFields.Checked[clbVisibleFields.Count - 1] := true;
      end;
      if Assigned(Fields[i].TableRef) then
        AddFieldsToLists(Fields[i].TableRef);
    end;
end;

function TConflictsCheckForm.IsRecConf(RecordID: integer): boolean;
begin
  Result := LeftTreeView.Items.FindNodeWithData(Pointer(RecordID)) <> nil;
end;

initialization

  TTeacherConflict.Caption := 'ПРЕПОДАВАТЕЛИ';
  TGroupConflict.Caption := 'ГРУППЫ';
  TClassroomConflict.Caption := 'АУДИТОРИИ';
  TCapacityConflict.Caption := 'ВМЕСТИМОСТЬ';
  TTeacherCourseConflict.Caption := 'ПРЕПОД. ДИСЦИПЛИНЫ';
  TGroupCourseConflict.Caption := 'ГРУППЫ ДИСЦИПЛИНЫ';

end.

