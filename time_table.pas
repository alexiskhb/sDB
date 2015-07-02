unit time_table;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, CheckLst, Grids, Buttons, Menus, ActnList, metadata,
  sqldb, types, query_filter, cell_contents, record_cards, sf_export, conflicts,
  ComCtrls, export_form;

type

  TGlyphButton = (gbNone, gbDelete, gbEdit, gbAdd, gbExpand, gbConflict);

  TMyStringGrid = class(TStringGrid)
  public
    CellStrings: TDblStrinListDynArray;
    function CellStringsAssigned(ACol, ARow: integer): boolean;
    function Button(ARect: TRect; APoint: TPoint; RowsCount: integer;
      RowsInSpanCount: integer; var RecordNum: integer): TGlyphButton;
    procedure ExpandCell(ACol, ARow: integer);
    procedure Reset;
  end;

  { TTimeTable }

  TTimeTable = class(TForm)
  private
    FTable: TDBTable;
    FShowAsList: TNotifyEvent;
    FCellContents: TCellContentsForm;
    FRecords: array of array of array of TCellIdentifier;
    IsRightPnlExtended: boolean;
    IsColEmpty: array of boolean;
    IsRowEmpty: array of boolean;
    IsTimeTable: boolean;
    horzids, vertids: array of integer;
    FStringsBuffer: TStringList;
    FOnInsert: TNotifyEvent;
    FOnUpdate: TNotifyEvent;
    FOnDelete: TNotifyEvent;
    FDragID: integer;
    FCheckedCount: integer;
    FReadOnly: boolean;
    FFilterPopup: TNotifyEvent;
    FExportForm: TExportForm;
  public
    Filters: TQueryFilterDynArray;
    property OnInsert: TNotifyEvent read FOnInsert write FOnInsert;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
    property OnDelete: TNotifyEvent read FOnDelete write FOnDelete;
    property Table: TDBTable read FTable;
    property OnShowAsListClick: TNotifyEvent read FShowAsList write FShowAsList;
    property OnFilterPopup: TNotifyEvent read FFilterPopup write FFilterPopup;
    constructor Create(ATable: TDBTable);
  published
    miSaveAs: TMenuItem;
    SaveDialog: TSaveDialog;
    btnApply: TBitBtn;
    btnAddFilter: TButton;
    cbbHorz: TComboBox;
    cbbVert: TComboBox;
    clbVisibleFields: TCheckListBox;
    ImageList: TImageList;
    lbFilters: TLabel;
    lbVert: TLabel;
    lbHorz: TLabel;
    MainMenu: TMainMenu;
    miExpandOnDrag: TMenuItem;
    miEmptyRows: TMenuItem;
    miEmptyCols: TMenuItem;
    miOptions: TMenuItem;
    miShowAsList: TMenuItem;
    miTable: TMenuItem;
    pnlFilterControls: TPanel;
    pnlControlsRight: TPanel;
    pnlContols: TPanel;
    sbxFilters: TScrollBox;
    Splitter: TSplitter;
    CheckSplitter: TSplitter;
    sgTable: TMyStringGrid;
    SQLQuery: TSQLQuery;
    StringGrid1: TStringGrid;
    PopupCaption: TMenuItem;
    pmCopyFilters: TPopupMenu;
    miWatch: TMenuItem;
    miShow: TMenuItem;
    miConflicts: TMenuItem;
    procedure miSaveAsClick(Sender: TObject);
    procedure miConflictsClick(Sender: TObject);
    procedure miShowClick(Sender: TObject);
    procedure ShowConflicts(ACol, ARow, ARecNum, RecordID: integer);
    procedure sgTableClick(Sender: TObject);
    procedure pmCopyFiltersFromClick(Sender: TObject);
    procedure pmCopyFiltersFromPopup(Sender: TObject);
    procedure lbFiltersClick(Sender: TObject);
    procedure btnAddFilterClick(Sender: TObject);
    procedure FilterChangeData(Sender: TObject);
    procedure clbVisibleFieldsClickCheck(Sender: TObject);
    procedure DestroyFilterClick(Sender: TObject);
    procedure AddConditionsToQuery;
    procedure btnApplyClick(Sender: TObject);
    procedure cbbChange(Sender: TObject);
    procedure clbVisibleFieldsMouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure clbVisibleFieldsMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure AddFieldsToLists(ATable: TDBTable);
    procedure FillTable(Horz, Vert: TDBField);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure lbFiltersPanelClick(Sender: TObject);
    procedure miEmptyColsClick(Sender: TObject);
    procedure miEmptyRowsClick(Sender: TObject);
    procedure miShowAsListClick(Sender: TObject);
    procedure sgTableGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure sgTableMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure sgTableMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sgTableDblClick(Sender: TObject);
    procedure RefreshTable;
    procedure sgTableStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure sgTableDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sgTableDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    class procedure RefreshTables;
    class procedure DestroyTimeTable(ATag: integer);
    class procedure FormSetFocus(ATag: integer);
    class function FormExists(ATag: integer): boolean;
  end;

  TTimeTableDynArray = array of TTimeTable;

const
  DefaultRightPanelWidth = 162;
  ExtendedRigthPanelWidth = 362;
  DefaultColumnWidth = 200;
  NarrowColumnWidth = 100;

var
  TimeTables: TTimeTableDynArray;

implementation

{$R *.lfm}

{ TTimeTable }

procedure TMyStringGrid.Reset;
var
  i, j: integer;
begin
  for i := 0 to High(CellStrings) do
    for j := 0 to High(CellStrings[i]) do
      if Assigned(CellStrings[i, j]) then
        FreeAndNil(CellStrings[i, j]);
  while RowCount > 1 do
    DeleteColRow(false, 1);
  while ColCount > 1 do
    DeleteColRow(true, 1);
end;

function TMyStringGrid.Button(ARect: TRect; APoint: TPoint; RowsCount: integer;
  RowsInSpanCount: integer; var RecordNum: integer): TGlyphButton;
var
  X, Y, Ht, Wh, CellRowHeight: integer;
  i: integer;
begin
  X := APoint.X - ARect.Left;
  Y := APoint.Y - ARect.Top;
  Ht := ARect.Bottom - ARect.Top;
  Wh := ARect.Right - ARect.Left;
  CellRowHeight := Canvas.TextHeight('A');

  if (X <= Wh) and (X >= Wh - 16) and (Y <= 16) and (Y >= 0) then exit(gbAdd);
  if RowsCount = 0 then exit(gbNone);
  if (X <= Wh) and (X >= Wh - 16) and
     (Y <= Ht) and (Y >= Ht - 16) and (CellRowHeight*RowsCount > Ht) then exit(gbExpand);

  i := 0;
  while Y > 0 do begin
    RecordNum := i;
    if (X <= 17) and (X >= 1) and
       (Y <= 16) and (Y >= 0) and (i < RowsCount div RowsInSpanCount) then exit(gbDelete);
    if (X <= 49) and (X >= 33) and
       (Y <= 16) and (Y >= 0) and (i < RowsCount div RowsInSpanCount) then exit(gbEdit);
    if (X <= 81) and (X >= 65) and
       (Y <= 16) and (Y >= 0) and (i < RowsCount div RowsInSpanCount) then exit(gbConflict);
    Y := Y - RowsInSpanCount*CellRowHeight;
    inc(i);
  end;

  exit(gbNone);
end;

function TMyStringGrid.CellStringsAssigned(ACol, ARow: integer): boolean;
begin
  Result :=
    (aRow < Length(CellStrings)) and
    (aCol < Length(CellStrings[aRow])) and
    Assigned(CellStrings[aRow, aCol]);
end;

procedure TMyStringGrid.ExpandCell(ACol, ARow: integer);
begin
  if Canvas.TextHeight('A') * CellStrings[ARow, ACol].Count > RowHeights[ARow] then
    RowHeights[ARow] := Canvas.TextHeight('A') * CellStrings[ARow, ACol].Count;
end;

class procedure TTimeTable.DestroyTimeTable(ATag: integer);
begin
  if FormExists(ATag) then
    TimeTables[ATag].Close;
end;

class procedure TTimeTable.FormSetFocus(ATag: integer);
begin
  if FormExists(ATag) then
    TimeTables[ATag].SetFocus;
end;

class function TTimeTable.FormExists(ATag: integer): boolean;
begin
  Result := TimeTables[ATag] <> nil;
end;

procedure TTimeTable.sgTableStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  CurCol, CurRow: integer;
begin
  sgTable.MouseToCell(ScreenToClient(Mouse.CursorPos).X, ScreenToClient(Mouse.CursorPos).Y, CurCol, CurRow);
end;

procedure TTimeTable.sgTableDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  CurCol, CurRow: integer;
  Field1, Field2: TDBField;
  ID1, ID2: integer;
  Shift: TShiftState;
begin
  //showmessage(inttostr(fdragid));
  if (FDragID = -1) or (FReadOnly) then exit;
  with sgTable do
    MouseToCell(ScreenToClient(Mouse.CursorPos).X, ScreenToClient(Mouse.CursorPos).Y, CurCol, CurRow);
  if (CurCol = 0) or (CurRow = 0) then exit;
  Shift := GetKeyShiftState;
  Field1 := cbbHorz.Items.Objects[cbbHorz.ItemIndex] as TDBField;
  Field2 := cbbVert.Items.Objects[cbbVert.ItemIndex] as TDBField;
  ID1 := horzids[CurCol];
  ID2 := vertids[CurRow];
  with CardsManager do begin
    if not (ssCtrl in Shift) then
      ShouldShow := false;
    EditTable(FTable, FDragID, atUpdate, Field1, Field2, ID1, ID2);
    if not (ssCtrl in Shift) then
      OkCloseLast;
    ShouldShow := true;
  end;
  if miExpandOnDrag.Checked then sgTable.ExpandCell(CurCol, CurRow);
end;

procedure TTimeTable.sgTableDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if FDragID = -1 then sgTable.DragDrop(Self, 0, 0);
end;

class procedure TTimeTable.RefreshTables;
var
  i: integer;
begin
  for i := 0 to High(TimeTables) do
    if Assigned(TimeTables[i]) then
      TimeTables[i].RefreshTable;
end;

procedure TTimeTable.sgTableMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurCol, CurRow: longint;
  Point: TPoint;
  Rect: TRect;
  RecordNum: integer;
  GlyphButton: TGlyphButton;
  RecID: integer;
begin
  RecordNum := -1;
  if FReadOnly then exit;;
  with sgTable do begin
    Point := ScreenToClient(Mouse.CursorPos);
    MouseToCell(Point.X, Point.Y, CurCol, CurRow);
    Rect := CellRect(CurCol, CurRow);
    if (CurCol = 0) or (CurRow = 0) then exit;
    if (CurCol > 0) and (CurRow > 0) then
      if CellStringsAssigned(CurCol, CurRow) then
        GlyphButton := Button(Rect, Point, sgTable.CellStrings[CurRow, CurCol].Count, FCheckedCount + 1, RecordNum)
      else
        GlyphButton := Button(Rect, Point, 0, FCheckedCount + 1, RecordNum);
    if (RecordNum <> -1) and (RecordNum < Length(FRecords[CurRow, CurCol])) then
      RecID := FRecords[CurRow, CurCol, RecordNum].id
    else
      RecID := -1;
    if not Assigned(sgTable.CellStrings[CurRow, CurCol]) then
      RecID := -1;
  end;
  FDragID := RecID;
  if GlyphButton <> gbNone then FDragID := -1;
end;

procedure TTimeTable.sgTableDblClick(Sender: TObject);
var
  CurCol, CurRow: integer;
  Point: TPoint;
begin
  with sgTable do begin
    Point := ScreenToClient(Mouse.CursorPos);
    MouseToCell(Point.X, Point.Y, CurCol, CurRow);

    if CurRow = 0 then
      if IsColEmpty[CurCol] then
        ColWidths[CurCol] := NarrowColumnWidth
      else
        ColWidths[CurCol] := DefaultColumnWidth;

    if CurCol = 0 then
      if IsRowEmpty[CurRow] then
        RowHeights[CurRow] := Canvas.TextHeight('A')*2
      else
        RowHeights[CurRow] := Canvas.TextHeight('A')*(FCheckedCount + 1);

    if (CurRow = 0) and (CurCol = 0) then begin
      ColWidths[CurCol] := NarrowColumnWidth;
      RowHeights[CurRow] := Canvas.TextHeight('A')*2
    end;

    if (CurRow > 0) and (CurCol > 0) and CellStringsAssigned(CurCol, CurRow) then begin
      FCellContents := TCellContentsForm.Create(Self);
      FCellContents.Content.Lines.Text := CellStrings[CurRow, CurCol].Text;
      Point.X := CellRect(CurCol, CurRow).Left;
      Point.Y := CellRect(CurCol, CurRow).Top;
      FCellContents.Top := ClientToScreen(Point).Y;
      FCellContents.Left := ClientToScreen(Point).X;
      FCellContents.Show;
    end;
  end;
end;

procedure TTimeTable.miSaveAsClick(Sender: TObject);
//var
  //Stream: TFileStream;
  //StringList: TStringList;
begin
  if not Assigned(FExportForm) then
    FExportForm := TExportForm.Create(
                                      Self,
                                      sgTable.CellStrings,
                                      sgTable.ColCount,
                                      sgTable.RowCount,
                                      clbVisibleFields,
                                      cbbHorz.Items.Objects[cbbHorz.ItemIndex] as TDBField,
                                      cbbVert.Items.Objects[cbbVert.ItemIndex] as TDBField,
                                      Filters,
                                      btnApply,
                                      FCheckedCount);
  FExportForm.Show;
end;

procedure TTimeTable.lbFiltersClick(Sender: TObject);
begin
  pmCopyFilters.PopUp;
end;

procedure TTimeTable.pmCopyFiltersFromPopup(Sender: TObject);
begin
  FFilterPopup(pmCopyFilters);
end;

procedure TTimeTable.pmCopyFiltersFromClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Length(Filters) - 1 do begin
    Filters[i].OnDestroy := @DestroyFilterClick;
    Filters[i].OnFilterAdd := @btnAddFilterClick;
    Filters[i].OnChangeData := @FilterChangeData;
    Filters[i].Tag := i;
    Filters[i].Top := i * (Filters[i].Height + 2);
  end;
  pnlControlsRight.Width := ExtendedRigthPanelWidth;
  if Length(Filters) = 0 then
    pnlControlsRight.Width := DefaultRightPanelWidth;
  btnApply.Enabled := true;
end;

procedure TTimeTable.sgTableClick(Sender: TObject);
var
  CurCol, CurRow: longint;
  Point: TPoint;
  Rect: TRect;
  RecordNum: integer;
  GlyphButton: TGlyphButton;
  Field1, Field2: TDBField;
  RecID, ID1, ID2: integer;
begin
  RecordNum := -1;
  if FReadOnly then exit;
  with sgTable do begin
    Point := ScreenToClient(Mouse.CursorPos);
    MouseToCell(Point.X, Point.Y, CurCol, CurRow);
    Rect := CellRect(CurCol, CurRow);

    if (CurCol = 0) or (CurRow = 0) then exit;

    if (CurCol > 0) and (CurRow > 0) then
      if CellStringsAssigned(CurCol, CurRow) then
        GlyphButton := Button(Rect, Point, sgTable.CellStrings[CurRow, CurCol].Count, FCheckedCount + 1, RecordNum)
      else
        GlyphButton := Button(Rect, Point, 0, FCheckedCount + 1, RecordNum);

    Field1 := cbbHorz.Items.Objects[cbbHorz.ItemIndex] as TDBField;
    Field2 := cbbVert.Items.Objects[cbbVert.ItemIndex] as TDBField;
    if RecordNum <> -1 then
      RecID := FRecords[CurRow, CurCol, RecordNum].id;
    if not Assigned(sgTable.CellStrings[CurRow, CurCol]) then
      RecID := -1;
    ID1 := horzids[CurCol];
    ID2 := vertids[CurRow];

    case GlyphButton of
      gbNone:;
      gbDelete: if MessageDlg('Удалить запись?', mtConfirmation, mbOKCancel, 0) = 1 then begin
        CardsManager.EditTable(FTable, RecID, atDelete);
        RefreshTable;
      end;
      gbEdit: CardsManager.EditTable(FTable, RecID, atUpdate, Field1, Field2, ID1, ID2);
      gbAdd: CardsManager.EditTable(FTable, NextID, atInsert, Field1, Field2, ID1, ID2);
      gbExpand: ExpandCell(CurCol, CurRow);
      gbConflict: ShowConflicts(CurCol, CurRow, RecordNum, RecID);
    end;
  end;
end;

procedure TTimeTable.ShowConflicts(ACol, ARow, ARecNum, RecordID: integer);
var
  Node: TTreeNode;
begin
  with ConflictsCheckForm do begin
    Show;
    Node := LeftTreeView.Items.FindNodeWithData(Pointer(RecordID));
    LeftTreeView.Items.SelectOnlyThis(Node);
    Node.Expand(true);
  end;
end;

procedure TTimeTable.miConflictsClick(Sender: TObject);
begin
  ConflictsCheckForm.Show;
end;

procedure TTimeTable.miShowClick(Sender: TObject);
begin
  ConflictsCheckForm.Show;
end;

constructor TTimeTable.Create(ATable: TDBTable);
begin
  inherited Create(Application);
  FTable := ATable;
  Caption := ATable.Caption;
  AddFieldsToLists(ATable);
  FStringsBuffer := TStringList.Create;
  IsTimeTable := ATable = DBTimeTable;
  FExportForm := nil;

  sgTable := TMyStringGrid.Create(Self);
  with sgTable do begin
    SetLength(CellStrings, 1, 1);
    Parent := Self;
    Align := alClient;
    Options := Options
      + [goRowSizing, goColSizing, goThumbTracking, goFixedColSizing, goCellHints]
      - [goRangeSelect];
    DragMode := dmAutomatic;
    DefaultColWidth := DefaultColumnWidth;
    OnDrawCell := @GridDrawCell;
    ColWidths[0] := NarrowColumnWidth;
    RowHeights[0] := Canvas.TextHeight('A')*2;
    ShowHint := true;
    Visible := false;
    DragCursor := crDrag;
    OnGetCellHint := @sgTableGetCellHint;
    OnMouseMove := @sgTableMouseMove;
    OnMouseDown := @sgTableMouseDown;
    OnDblClick := @sgTableDblClick;
    OnStartDrag := @sgTableStartDrag;
    OnDragDrop := @sgTableDragDrop;
    OnDragOver := @sgTableDragOver;
    OnClick := @sgTableClick;
  end;

  FReadOnly := clbVisibleFields.Count < 2;
  IsRightPnlExtended := false;
  clbVisibleFieldsClickCheck(clbVisibleFields);
  clbVisibleFields.Height := clbVisibleFields.Count*24;
end;

procedure TTimeTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TTimeTable.cbbChange(Sender: TObject);
begin
  if (cbbHorz.ItemIndex <> -1) and (cbbVert.ItemIndex <> -1) then
    btnApply.Enabled := true;
end;

procedure TTimeTable.clbVisibleFieldsMouseWheelDown(Sender: TObject;
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

procedure TTimeTable.clbVisibleFieldsMouseWheelUp(Sender: TObject;
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

procedure TTimeTable.btnApplyClick(Sender: TObject);
begin
  sgTable.Reset;
  sgTable.Visible := true;
  FillTable(
    cbbHorz.Items.Objects[cbbHorz.ItemIndex] as TDBField,
    cbbVert.Items.Objects[cbbVert.ItemIndex] as TDBField);
  pnlControlsRight.Width := DefaultRightPanelWidth;
  btnApply.Enabled := false;
end;

procedure TTimeTable.AddFieldsToLists(ATable: TDBTable);
var
  i: integer;
begin
  with ATable do
    for i := 0 to High(Fields) do begin
      if Fields[i].Visible and not Fields[i].Secondary then begin
        cbbHorz.AddItem(Fields[i].Caption, Fields[i]);
        cbbVert.AddItem(Fields[i].Caption, Fields[i]);
        clbVisibleFields.AddItem(Fields[i].Caption, Fields[i]);
        clbVisibleFields.Checked[clbVisibleFields.Count - 1] := true;
      end;
      if Assigned(Fields[i].TableRef) then
        AddFieldsToLists(Fields[i].TableRef);
    end;
end;

procedure TTimeTable.FillTable(Horz, Vert: TDBField);
var
  x, y, i, j, k: integer;
  Field: TDBField;
  ColsCount, RowsCount: integer;
begin
  with SQLQuery do begin
    Close;
    SQL.Text := 'select count(*) as ColsCount from ' + Horz.TableOwner.Name;
    Open;
    ColsCount := FieldByName('ColsCount').AsInteger;
    Close;
    SQL.Text := 'select count(*) as RowsCount from ' + Vert.TableOwner.Name;
    Open;
    RowsCount := FieldByName('RowsCount').AsInteger;
    Close;
  end;

  SetLength(horzids, ColsCount + 1);
  SetLength(vertids, RowsCount + 1);
  SetLength(FRecords, 0);
  SetLength(FRecords, RowsCount + 1, ColsCount + 1, 0);
  SetLength(sgTable.CellStrings, RowsCount + 1, ColsCount + 1);
  SetLength(IsColEmpty, ColsCount + 1);
  SetLength(IsRowEmpty, RowsCount + 1);
  for i := 1 to High(IsColEmpty) do
    IsColEmpty[i] := true;
  for i := 1 to High(IsRowEmpty) do
    IsRowEmpty[i] := true;
  ConflictsCheckForm.Refresh;

  SetLength(FRecords[0], ColsCount + 1, 1);
  with SQLQuery do begin
    Close;
    SetSQLQuery(Horz.TableOwner, SQLQuery);
    SQL.Append('order by ' + Horz.SortField.TableOwner.Name + Horz.SortField.Name + ' asc');
    Open; First;
    for j := 1 to High(horzids) do begin
      horzids[j] := FieldByName(Horz.TableOwner.Name + 'id').AsInteger;
      if not Assigned(sgTable.CellStrings[0, j]) then
        sgTable.CellStrings[0, j] := TStringList.Create
      else
        sgTable.CellStrings[0, j].Clear;
      sgTable.CellStrings[0, j].Append(FieldByName(Horz.TableOwner.Name + Horz.Name).AsString);
      FRecords[0, j, 0].id := horzids[j];
      Next;
    end;
  end;

  with SQLQuery do begin
    Close;
    SetSQLQuery(Vert.TableOwner, SQLQuery);
    SQL.Append('order by ' + Vert.SortField.TableOwner.Name + Vert.SortField.Name + ' asc');
    Open; First;
    for i := 1 to High(vertids) do begin
      SetLength(FRecords[i, 0], 1);
      vertids[i] := FieldByName(Vert.TableOwner.Name + 'id').Value;
      if not Assigned(sgTable.CellStrings[i, 0]) then
        sgTable.CellStrings[i, 0] := TStringList.Create
      else
        sgTable.CellStrings[i, 0].Clear;
      sgTable.CellStrings[i, 0].Append(FieldByName(Vert.TableOwner.Name + Vert.Name).AsString);
      FRecords[i, 0, 0].id := vertids[i];
      Next;
    end;
  end;

  with SQLQuery do begin
    Close;
    SetSQLQuery(FTable, SQLQuery);
    AddConditionsToQuery;
    SQL.Append(
      'order by ' + Vert.SortField.TableOwner.Name + Vert.SortField.Name +
      ', ' + Horz.SortField.TableOwner.Name + Horz.SortField.Name +
      ', ' + FTable.Name + 'id' + ' asc');
    Open; First;
    y := 0; k := 1;
    x := 1;
    while not EOF do begin
      if
      (FieldByName(Horz.TableOwner.Name + 'id').AsInteger = horzids[x]) and
      (FieldByName(Vert.TableOwner.Name + 'id').AsInteger = vertids[y]) then begin
        SetLength(FRecords[y, x], Length(FRecords[y, x]) + 1);
        if not Assigned(sgTable.CellStrings[y, x]) then
          sgTable.CellStrings[y, x] := TStringList.Create;
        sgTable.CellStrings[y, x].Append(' ');
        for i := 0 to clbVisibleFields.Count - 1 do begin
          if not clbVisibleFields.Checked[i] then continue;
            Field := clbVisibleFields.Items.Objects[i] as TDBField;
            sgTable.CellStrings[y, x].Append(FieldByName(Field.TableOwner.Name + Field.Name).Value);
        end;
        FRecords[y, x, High(FRecords[y, x])].id := FieldByName(FTable.Name + 'id').AsInteger;
        FRecords[y, x, High(FRecords[y, x])].IsConf := ConflictsCheckForm.IsRecConf(FRecords[y, x, High(FRecords[y, x])].id);
        Next; inc(k);
        IsColEmpty[x] := false;
        IsRowEmpty[y] := false;
      end else begin
        inc(x);
        if x > ColsCount then begin
          inc(y);
          x := 1;
          continue;
        end;
      end;
    end;
  end;

  IsColEmpty[0] := false;
  IsRowEmpty[0] := false;
  sgTable.RowCount := RowsCount + 1;
  sgTable.ColCount := ColsCount + 1;
  with sgTable do begin
    DefaultRowHeight := (FCheckedCount + 1)*Canvas.TextHeight('A');
    RowHeights[0] := 30;
  end;
  miEmptyRowsClick(miEmptyRows);
  miEmptyColsClick(miEmptyCols);
end;

procedure TTimeTable.FormDestroy(Sender: TObject);
begin
  TimeTables[(Sender as TTimeTable).Tag] := nil;
end;

procedure TTimeTable.FormShow(Sender: TObject);
begin
  miShowAsList.Tag := Self.Tag;
  cbbVert.ItemIndex := 0 mod clbVisibleFields.Count;
  cbbHorz.ItemIndex := 1 mod clbVisibleFields.Count;
  btnApplyClick(btnApply);
end;

procedure TTimeTable.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i, k: integer;
  ShouldShow: boolean;
  CurCol, CurRow: integer;
begin
  k := 0;
  with sgTable do begin
    MouseToCell(ScreenToClient(Mouse.CursorPos).X, ScreenToClient(Mouse.CursorPos).Y, CurCol, CurRow);
    ShouldShow := (CurCol = aCol) and (CurRow = aRow) and (not FReadOnly);
    if CellStringsAssigned(aCol, aRow) then begin
      for i := 0 to CellStrings[aRow, aCol].Count - 1 do begin
        Canvas.TextOut(aRect.Left, aRect.Top + i*Canvas.TextHeight('A'), CellStrings[aRow, aCol].Strings[i]);
        if (CellStrings[aRow, aCol].Strings[i] = ' ') then begin
          if ShouldShow then begin
            ImageList.Draw(Canvas, aRect.Left + 1, aRect.Top + i*Canvas.TextHeight('A') + 1, 2, True);
            ImageList.Draw(Canvas, aRect.Left + 33, aRect.Top + i*Canvas.TextHeight('A') + 1, 1, True);
            if FRecords[aRow, aCol, k].IsConf then
              ImageList.Draw(Canvas, aRect.Left + 65, aRect.Top + i*Canvas.TextHeight('A') + 1, 5, True);
            end;
          if FRecords[aRow, aCol, k].IsConf and miWatch.Checked then
            ImageList.Draw(Canvas, aRect.Left + 65, aRect.Top + i*Canvas.TextHeight('A') + 1, 5, True);
          inc(k);
        end;
      end;
      if CellStrings[aRow, aCol].Count*Canvas.TextHeight('A') > aRect.Bottom - aRect.Top then
        ImageList.Draw(Canvas, aRect.Left + ColWidths[aCol] - 16, aRect.Top + RowHeights[aRow] - 16, 3, True);

    end;
    if (aCol > 0) and (aRow > 0) and ShouldShow then
      ImageList.Draw(Canvas, aRect.Left + ColWidths[aCol] - 16, aRect.Top, 0, True);
  end;
end;

procedure TTimeTable.lbFiltersPanelClick(Sender: TObject);
begin
  if IsRightPnlExtended then
    pnlControlsRight.Width := DefaultRightPanelWidth
  else
    pnlControlsRight.Width := ExtendedRigthPanelWidth;
  IsRightPnlExtended := not IsRightPnlExtended;
end;

procedure TTimeTable.miEmptyColsClick(Sender: TObject);
var
  i: integer;
begin
  if miEmptyCols.Checked then begin
    for i := 1 to sgTable.ColCount - 1 do
      if IsColEmpty[i] then
        sgTable.ColWidths[i] := 0;
    end else begin
      for i := 1 to sgTable.ColCount - 1 do
        if IsColEmpty[i] then
          sgTable.ColWidths[i] := NarrowColumnWidth
        else
          if sgTable.ColWidths[i] = NarrowColumnWidth then
            sgTable.ColWidths[i] := DefaultColumnWidth;
    end;
end;

procedure TTimeTable.miEmptyRowsClick(Sender: TObject);
var
  i: integer;
begin
  if miEmptyRows.Checked then begin
    for i := 0 to sgTable.RowCount - 1 do
      if IsRowEmpty[i] then
        sgTable.RowHeights[i] := 0;
  end else begin
    for i := 0 to sgTable.RowCount - 1 do
      if IsRowEmpty[i] then
        sgTable.RowHeights[i] := sgTable.Canvas.TextHeight('A')*2
      else
        if sgTable.RowHeights[i] = sgTable.Canvas.TextHeight('A')*2 then
            sgTable.RowHeights[i] := (FCheckedCount + 1)*sgTable.Canvas.TextHeight('A');
  end;
end;

procedure TTimeTable.miShowAsListClick(Sender: TObject);
begin
  FShowAsList(Sender);
end;

procedure TTimeTable.sgTableGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
begin
  with sgTable do
    if (aRow < Length(CellStrings)) and (aCol < Length(CellStrings[aRow])) and Assigned(CellStrings[aRow, aCol]) then
      HintText := CellStrings[aRow, aCol].Text;
end;

procedure TTimeTable.sgTableMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  sgTable.Invalidate;
end;

procedure TTimeTable.RefreshTable;
var
  i, j: integer;
  TempID: TObject;
begin
  with sgTable do
    for i := 0 to High(CellStrings) do
      for j := 0 to High(CellStrings[i]) do
        if Assigned(CellStrings[i, j]) then
          FreeAndNil(CellStrings[i, j]);
  //if ConflictsCheckForm.LeftListBox.ItemIndex >= 0 then
    //TempID := ConflictsCheckForm.LeftListBox.Items.Objects[ConflictsCheckForm.LeftListBox.ItemIndex];
  FillTable(
    cbbHorz.Items.Objects[cbbHorz.ItemIndex] as TDBField,
    cbbVert.Items.Objects[cbbVert.ItemIndex] as TDBField);
  //ConflictsCheckForm.LeftListBox.ItemIndex := ConflictsCheckForm.LeftListBox.Items.IndexOfObject(TempID);
end;

procedure TTimeTable.FilterChangeData(Sender: TObject);
begin
  btnApply.Enabled := true;
end;

procedure TTimeTable.btnAddFilterClick(Sender: TObject);
var
  i, VPos: integer;
begin
  VPos := (Sender as TButton).Tag;

  if VPos = -1 then
    VPos := Length(Filters) - 1;

  SetLength(Filters, Length(Filters) + 1);

  for i := Length(Filters) - 1 downto VPos + 1 do
    Filters[i] := Filters[i - 1];

  Filters[VPos + 1] := TQueryFilter.Create(FTable, VPos + 1, sbxFilters);
  Filters[VPos + 1].OnDestroy := @DestroyFilterClick;
  Filters[VPos + 1].OnFilterAdd := @btnAddFilterClick;
  Filters[VPos + 1].OnChangeData := @FilterChangeData;

  for i := 0 to Length(Filters) - 1 do begin
    Filters[i].Tag := i;
    Filters[i].Top := i * (Filters[i].Height + 2);
  end;

  pnlControlsRight.Width := ExtendedRigthPanelWidth;
end;

procedure TTimeTable.clbVisibleFieldsClickCheck(Sender: TObject);
var
  i: integer;
begin
  FCheckedCount := 0;
  for i := 0 to clbVisibleFields.Count - 1 do
    if clbVisibleFields.Checked[i] then inc(FCheckedCount);
  btnApply.Enabled := true;
end;

procedure TTimeTable.DestroyFilterClick(Sender: TObject);
var
  VPos, i: integer;
begin
  VPos := (Sender as TButton).Tag;

  Filters[VPos].Destroy;

  for i := VPos to Length(Filters) - 2 do
    Filters[i] := Filters[i + 1];

  SetLength(Filters, Length(Filters) - 1);

  for i := 0 to Length(Filters) - 1 do begin
    Filters[i].Tag := i;
    Filters[i].Top := i * (Filters[i].Height + 2);
  end;

  if Length(Filters) = 0 then
    pnlControlsRight.Width := DefaultRightPanelWidth;
  FilterChangeData(Sender);
end;

procedure TTimeTable.AddConditionsToQuery;
var
  i, k: integer;
begin
  SQLQuery.SQL.Append('where 1 = 1');

  with SQLQuery do
    for i := 0 to Length(Filters) - 1 do
      with Filters[i] do
        if Assigned(Filters[i]) and Assigned(ConstantEditor) then
          if (ConstantEditor.Visible) then begin
            SQL.Append('and ' + ChosenField.TableOwner.Name + '.' + ChosenField.Name);
            SQL.Append(Operation.Code + ' :' + ChosenField.TableOwner.Name + ChosenField.Name + IntToStr(i));
          end else
            SQL.Append('or 1 = 1');

  k := 0;
  for i := 0 to Length(Filters) - 1 do
    with Filters[i] do
      if Assigned(Filters[i]) and Assigned(ConstantEditor) and
      (ConstantEditor.Visible) then
        with SQLQuery do begin
          Params[k].Value := Value;
          inc(k);
        end;
end;

initialization

  SetLength(TimeTables, Length(DBTables));

end.
















