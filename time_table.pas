unit time_table;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, CheckLst, Grids, Buttons, Menus, metadata, connection_transaction,
  sqldb, types, query_filter, cell_contents;

type

  TGlyphButton = (gbNone, gbDelete, gbEdit, gbAdd, gbExpand);

  TMyStringGrid = class(TStringGrid)
  public
    CellStrings: array of array of TStringList;
    function CellStringsAssigned(ACol, ARow: integer): boolean;
    function Button(ARect: TRect; APoint: TPoint; RowsCount: integer;
      RowsInSpanCount: integer; var RecordNum: integer): TGlyphButton;
    procedure ExpandCell(ACol, ARow: integer);
    procedure Reset;
  end;

  { TTimeTable }

  TTimeTable = class(TForm)
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
    procedure btnAddFilterClick(Sender: TObject);
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
    procedure lbFiltersClick(Sender: TObject);
    procedure miEmptyColsClick(Sender: TObject);
    procedure miEmptyRowsClick(Sender: TObject);
    procedure miShowAsListClick(Sender: TObject);
    procedure sgTableGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure sgTableMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    class procedure DestroyTimeTable(ATag: integer);
    class procedure FormSetFocus(ATag: integer);
    class function FormExists(ATag: integer): boolean;
    procedure sgTableMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sgTableDblClick(Sender: TObject);
  private
    FTable: TDBTable;
    FShowAsList: TNotifyEvent;
    FCellContents: TCellContents;
    FFilters: array of TQueryFilter;
    //[X, Y, InCell]
    FRecords: array of array of array of integer;
    IsRightPnlExtended: boolean;
    IsColEmpty: array of boolean;
    IsRowEmpty: array of boolean;
  public
    property OnShowAsListClick: TNotifyEvent read FShowAsList write FShowAsList;
    constructor Create(ATable: TDBTable);
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
  RowNum, ColNum: integer;
  X, Y, Ht, Wh, CellRowHeight, CellColWidth: integer;
  i: integer;
begin
  X := APoint.X - ARect.Left;
  Y := APoint.Y - ARect.Top;
  Ht := ARect.Bottom - ARect.Top;
  Wh := ARect.Right - ARect.Left;
  CellRowHeight := Canvas.TextHeight('A');

  if (X <= Wh) and (X >= Wh - 16) and (Y <= 16) and (Y >= 0) then exit(gbAdd);
  if (X <= Wh) and (X >= Wh - 16) and
     (Y <= Ht) and (Y >= Ht - 16) and (CellRowHeight*RowsCount > Ht) then exit(gbExpand);

  i := 0;
  while Y > 0 do begin
    RecordNum := i;
    if (X <= 17) and (X >= 1) and (Y <= 16) and (Y >= 0) then exit(gbDelete);
    if (X <= 49) and (X >= 33) and (Y <= 16) and (Y >= 0) then exit(gbEdit);
    Y := Y - RowsInSpanCount*CellRowHeight;
    inc(i);
  end;

  exit(gbNone);
end;

function TMyStringGrid.CellStringsAssigned(ACol, ARow: integer): boolean;
begin
  Result := (aRow < Length(CellStrings)) and (aCol < Length(CellStrings[aRow])) and
      Assigned(CellStrings[aRow, aCol]);
end;

procedure TMyStringGrid.ExpandCell(ACol, ARow: integer);
begin
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

procedure TTimeTable.sgTableMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurCol, CurRow: longint;
  Point: TPoint;
  Rect: TRect;
  CellRow: integer;
  RecordNum: integer;
  CheckedCount: integer;
  i: integer;
begin
  CheckedCount := 0;
  for i := 0 to clbVisibleFields.Count - 1 do
    if clbVisibleFields.Checked[i] then inc(CheckedCount);
  with sgTable do begin
    Point := ScreenToClient(Mouse.CursorPos);
    MouseToCell(Point.X, Point.Y, CurCol, CurRow);
    Rect := CellRect(CurCol, CurRow);
    if (CurCol > 0) and (CurRow > 0) and CellStringsAssigned(CurCol, CurRow) then
      case Button(Rect, Point, sgTable.CellStrings[CurRow, CurCol].Count, CheckedCount + 1, RecordNum) of
        gbNone:;
        gbDelete: ShowMessage('Delete');
        gbEdit: ShowMessage('Edit');
        gbAdd: ShowMessage('Add');
        gbExpand: ExpandCell(CurCol, CurRow);
      end;
    ShowMessage(IntToStr(FRecords[CurRow, CurCol, 0]));
  end;
end;

procedure TTimeTable.sgTableDblClick(Sender: TObject);
var
  CurCol, CurRow: integer;
  Point: TPoint;
  CheckedCount: integer;
  i: integer;
begin
  CheckedCount := 0;
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
      else begin
        for i := 0 to clbVisibleFields.Count - 1 do
          if clbVisibleFields.Checked[i] then inc(CheckedCount);
        RowHeights[CurRow] := Canvas.TextHeight('A')*(CheckedCount + 1);
      end;

    if (CurRow = 0) and (CurCol = 0) then begin
      ColWidths[CurCol] := NarrowColumnWidth;
      RowHeights[CurRow] := Canvas.TextHeight('A')*2
    end;

    if (CurRow > 0) and (CurCol > 0) and CellStringsAssigned(CurCol, CurRow) then begin
      FCellContents := TCellContents.Create(Self);
      FCellContents.Content.Lines.Text := CellStrings[CurRow, CurCol].Text;
      Point.X := CellRect(CurCol, CurRow).Left;
      Point.Y := CellRect(CurCol, CurRow).Top;
      FCellContents.Top := ClientToScreen(Point).Y;
      FCellContents.Left := ClientToScreen(Point).X;
      FCellContents.Show;
    end;
  end;
end;

constructor TTimeTable.Create(ATable: TDBTable);
begin
  inherited Create(Application);
  FTable := ATable;
  Caption := ATable.Caption;
  AddFieldsToLists(ATable);

  sgTable := TMyStringGrid.Create(Self);
  with sgTable do begin
    SetLength(CellStrings, 1, 1);
    Parent := Self;
    Align := alClient;
    Options := Options + [goRowSizing, goColSizing, goThumbTracking, goFixedColSizing, goCellHints];
    DefaultColWidth := DefaultColumnWidth;
    OnDrawCell := @GridDrawCell;
    ColWidths[0] := NarrowColumnWidth;
    RowHeights[0] := Canvas.TextHeight('A')*2;
    ShowHint := true;
    Visible := false;
    OnGetCellHint := @sgTableGetCellHint;
    OnMouseMove := @sgTableMouseMove;
    OnMouseDown := @sgTableMouseDown;
    OnDblClick := @sgTableDblClick;
  end;

  IsRightPnlExtended := false;
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
  sgTable.Visible := true;
  FillTable(
    cbbHorz.Items.Objects[cbbHorz.ItemIndex] as TDBField,
    cbbVert.Items.Objects[cbbVert.ItemIndex] as TDBField);

  pnlControlsRight.Width := DefaultRightPanelWidth;
end;

procedure TTimeTable.AddFieldsToLists(ATable: TDBTable);
var
  i: integer;
begin
  with ATable do
    for i := 0 to High(Fields) do begin
      if Fields[i].Visible then begin
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
  x, y, i: integer;
  Field: TDBField;
  horzids, vertids: array of integer;
  CheckedCount: integer;
begin
  sgTable.Reset;

  CheckedCount := 0;
  SetLength(horzids, 1);
  SetLength(vertids, 1);
  SetLength(FRecords, 1);

  with SQLQuery do begin
    Close;
    SetSQLQuery(FTable, SQLQuery);
    AddConditionsToQuery;
    SQL.Append(
      'order by ' + Vert.SortField.TableOwner.Name + Vert.SortField.Name +
      ', ' + Horz.SortField.TableOwner.Name + Horz.SortField.Name + ' asc');
    Open;
    First;
  end;

  with ConTran.CommonSQLQuery do begin
    Close;
    SetSQLQuery(Horz.TableOwner, ConTran.CommonSQLQuery);
    SQL.Append('order by ' + Horz.SortField.TableOwner.Name + Horz.SortField.Name + ' asc');
    Open;
    First;
    while not EOF do begin
      SetLength(horzids, Length(horzids) + 1);
      SetLength(sgTable.CellStrings, 1, Length(horzids));
      SetLength(FRecords[0], Length(horzids), 1);
      horzids[High(horzids)] := FieldByName(Horz.TableOwner.Name + 'id').AsInteger;
      sgTable.Columns.Add.Title.Caption := FieldByName(Horz.TableOwner.Name + Horz.Name).Value;
      if not Assigned(sgTable.CellStrings[0, High(horzids)]) then
        sgTable.CellStrings[0, High(horzids)] := TStringList.Create
      else
        sgTable.CellStrings[0, High(horzids)].Clear;
      sgTable.CellStrings[0, High(horzids)].Append(sgTable.Columns[High(horzids) - 1].Title.Caption);
      FRecords[0, High(horzids), 0] := horzids[High(horzids)];
      Next;
    end;

    SetLength(IsColEmpty, Length(horzids));
    for i := 1 to High(IsColEmpty) do
      IsColEmpty[i] := true;

    Close;
    SetSQLQuery(Vert.TableOwner, ConTran.CommonSQLQuery);
    SQL.Append('order by ' + Vert.SortField.TableOwner.Name + Vert.SortField.Name + ' asc');
    Open;
    First;
    y := 0;
    SetLength(IsRowEmpty, 1);
    while not EOF do begin
      inc(y);
      SetLength(vertids, y + 1);
      vertids[y] := FieldByName(Vert.TableOwner.Name + 'id').AsInteger;
      SetLength(IsRowEmpty, y + 1);
      IsRowEmpty[y] := true;

      with sgTable do begin
        RowCount := RowCount + 1;
        SetLength(CellStrings, y + 1, ColCount);
        Cells[0, y] := FieldByName(Vert.TableOwner.Name + Vert.Name).Value;
        if not Assigned(CellStrings[y, 0]) then
          CellStrings[y, 0] := TStringList.Create
        else
          CellStrings[y, 0].Clear;
        CellStrings[y, 0].Append(FieldByName(Vert.TableOwner.Name + Vert.Name).Value);

        SetLength(FRecords, y + 1, ColCount);
        SetLength(FRecords[y, 0], 1);
        FRecords[y, 0, 0] := FieldByName(Vert.TableOwner.Name + 'id').AsInteger;
      end;
      x := 1;
      with SQLQuery do begin
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
            FRecords[y, x, High(FRecords[y, x])] :=
              SQLQuery.FieldByName(FTable.Name + 'id').AsInteger;
            Next;
            IsColEmpty[x] := false;
            IsRowEmpty[y] := false;
          end else begin
            inc(x);
            if x >= sgTable.ColCount then begin break; end;
          end;
        end;
      end;
      Next;
    end;
  end;
  IsColEmpty[0] := false;
  IsRowEmpty[0] := false;

  for i := 0 to clbVisibleFields.Count - 1 do
    if clbVisibleFields.Checked[i] then inc(CheckedCount);
  with sgTable do begin
    DefaultRowHeight := (CheckedCount + 1)*Canvas.TextHeight('A');
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
end;

procedure TTimeTable.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i: integer;
  ShouldShow: boolean;
  CurCol, CurRow: integer;
begin
  if (aCol = 0) or (aRow = 0) then exit;
  with sgTable do begin
    MouseToCell(ScreenToClient(Mouse.CursorPos).X, ScreenToClient(Mouse.CursorPos).Y, CurCol, CurRow);
    ShouldShow := (CurCol = aCol) and (CurRow = aRow);

    if CellStringsAssigned(aCol, aRow) then begin
      for i := 0 to CellStrings[aRow, aCol].Count - 1 do begin
        Canvas.TextOut(aRect.Left, aRect.Top + i*Canvas.TextHeight('A'), CellStrings[aRow, aCol].Strings[i]);
        if (CellStrings[aRow, aCol].Strings[i] = ' ') and ShouldShow then begin
          ImageList.Draw(Canvas, aRect.Left + 1, aRect.Top + i*Canvas.TextHeight('A') + 1, 2, True);
          ImageList.Draw(Canvas, aRect.Left + 33, aRect.Top + i*Canvas.TextHeight('A') + 1, 1, True);
        end;
      end;
      if CellStrings[aRow, aCol].Count*Canvas.TextHeight('A') > aRect.Bottom - aRect.Top then
        ImageList.Draw(Canvas, aRect.Left + ColWidths[aCol] - 16, aRect.Top + RowHeights[aRow] - 16, 3, True);
    end;

    if (aCol > 0) and (aRow > 0) and ShouldShow then
      ImageList.Draw(Canvas, aRect.Left + ColWidths[aCol] - 16, aRect.Top, 0, True);
  end;
end;

procedure TTimeTable.lbFiltersClick(Sender: TObject);
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
    for i := 0 to High(IsColEmpty) do
      if IsColEmpty[i] then
        sgTable.ColWidths[i] := 0;
    end else begin
      for i := 0 to High(IsColEmpty) do
        if IsColEmpty[i] then
          sgTable.ColWidths[i] := NarrowColumnWidth;
    end;
end;

procedure TTimeTable.miEmptyRowsClick(Sender: TObject);
var
  i: integer;
begin
  if miEmptyRows.Checked then begin
    for i := 0 to High(IsRowEmpty) do
      if IsRowEmpty[i] then
        sgTable.RowHeights[i] := 0;
  end else begin
    for i := 0 to High(IsRowEmpty) do
      if IsRowEmpty[i] then
        sgTable.RowHeights[i] := sgTable.Canvas.TextHeight('A')*2;
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

procedure TTimeTable.btnAddFilterClick(Sender: TObject);
var
  i, VPos: integer;
begin
  VPos := (Sender as TButton).Tag;

  if VPos = -1 then
    VPos := Length(FFilters) - 1;

  SetLength(FFilters, Length(FFilters) + 1);

  for i := Length(FFilters) - 1 downto VPos + 1 do
    FFilters[i] := FFilters[i - 1];

  FFilters[VPos + 1] := TQueryFilter.Create(FTable, VPos + 1, sbxFilters);
  FFilters[VPos + 1].OnDestroy := @DestroyFilterClick;
  FFilters[VPos + 1].OnFilterAdd := @btnAddFilterClick;

  for i := 0 to Length(FFilters) - 1 do begin
    FFilters[i].Tag := i;
    FFilters[i].Top := i * (FFilters[i].Height + 2);
  end;

  pnlControlsRight.Width := ExtendedRigthPanelWidth;
end;

procedure TTimeTable.DestroyFilterClick(Sender: TObject);
var
  VPos, i: integer;
begin
  VPos := (Sender as TButton).Tag;

  FFilters[VPos].Destroy;

  for i := VPos to Length(FFilters) - 2 do
    FFilters[i] := FFilters[i + 1];

  SetLength(FFilters, Length(FFilters) - 1);

  for i := 0 to Length(FFilters) - 1 do begin
    FFilters[i].Tag := i;
    FFilters[i].Top := i * (FFilters[i].Height + 2);
  end;

  if Length(FFilters) = 0 then
    pnlControlsRight.Width := DefaultRightPanelWidth;
end;

procedure TTimeTable.AddConditionsToQuery;
var
  i, k: integer;
begin
  SQLQuery.SQL.Append('where 1 = 1');

  with SQLQuery do
    for i := 0 to Length(FFilters) - 1 do
      with FFilters[i] do
        if Assigned(FFilters[i]) and Assigned(ConstantEditor) then
          if (ConstantEditor.Visible) then begin
            SQL.Append('and ' + ChosenField.TableOwner.Name + '.' + ChosenField.Name);
            SQL.Append(Operation + ' :' + ChosenField.TableOwner.Name + ChosenField.Name + IntToStr(i));
          end else
            SQL.Append('or 1 = 1');

  k := 0;
  for i := 0 to Length(FFilters) - 1 do
    with FFilters[i] do
      if Assigned(FFilters[i]) and Assigned(ConstantEditor) and
      (ConstantEditor.Visible) then
        with SQLQuery do begin
          Params[k].Value := Value;
          inc(k);
        end;
end;

initialization

  SetLength(TimeTables, Length(DBTables));

end.















