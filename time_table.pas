unit time_table;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
	StdCtrls, CheckLst, Grids, Buttons, metadata, connection_transaction, sqldb, types;

type

  TMyStringGrid = class(TStringGrid)
  public
    CellStrings: array of array of TStringList;
    procedure Reset;
  end;

  { TTimeTable }

  TTimeTable = class(TForm)
    btnApply: TBitBtn;
    cbbHorz: TComboBox;
    cbbVert: TComboBox;
    clbVisibleFields: TCheckListBox;
    ImageList: TImageList;
    pnlControlsRight: TPanel;
    pnlContols: TPanel;
    Splitter: TSplitter;
    CheckSplitter: TSplitter;
    sgTable: TMyStringGrid;
    SQLQuery: TSQLQuery;
    StringGrid1: TStringGrid;
    procedure btnApplyClick(Sender: TObject);
    procedure cbbChange(Sender: TObject);
    procedure clbVisibleFieldsMouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure clbVisibleFieldsMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure AddFieldsToLists(ATable: TDBTable);
    procedure FillTable(Horz, Vert: TDBField);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure sgTableGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
  private
    FTable: TDBTable;
  public
    constructor Create(ATable: TDBTable);
  end;

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
    DefaultColWidth := 200;
    DefaultRowHeight := 100;
    OnDrawCell := @GridDrawCell;
    ColWidths[0] := 100;
    RowHeights[0] := 30;
    ShowHint := true;
    OnGetCellHint := @sgTableGetCellHint;
  end;

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
  FillTable(
    cbbHorz.Items.Objects[cbbHorz.ItemIndex] as TDBField,
    cbbVert.Items.Objects[cbbVert.ItemIndex] as TDBField);
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

  with SQLQuery do begin
    Close;
    SetSQLQuery(FTable, SQLQuery);
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
      horzids[High(horzids)] := FieldByName(Horz.TableOwner.Name + 'id').AsInteger;
      sgTable.Columns.Add.Title.Caption := FieldByName(Horz.TableOwner.Name + Horz.Name).Value;
      if not Assigned(sgTable.CellStrings[0, High(horzids)]) then
        sgTable.CellStrings[0, High(horzids)] := TStringList.Create
      else
        sgTable.CellStrings[0, High(horzids)].Clear;
      sgTable.CellStrings[0, High(horzids)].Append(sgTable.Columns[High(horzids) - 1].Title.Caption);
      Next;
    end;

    Close;
    SetSQLQuery(Vert.TableOwner, ConTran.CommonSQLQuery);
    SQL.Append('order by ' + Vert.SortField.TableOwner.Name + Vert.SortField.Name + ' asc');
    Open;
    First;
    y := 0;
    while not EOF do begin
      inc(y);
      SetLength(vertids, y + 1);
      SetLength(sgTable.CellStrings, y + 1, sgTable.ColCount);
      vertids[y] := FieldByName(Vert.TableOwner.Name + 'id').AsInteger;
      sgTable.RowCount := sgTable.RowCount + 1;
      sgTable.Cells[0, y] := FieldByName(Vert.TableOwner.Name + Vert.Name).Value;
      if not Assigned(sgTable.CellStrings[y, 0]) then
        sgTable.CellStrings[y, 0] := TStringList.Create
      else
        sgTable.CellStrings[y, 0].Clear;
      sgTable.CellStrings[y, 0].Append(FieldByName(Vert.TableOwner.Name + Vert.Name).Value);
      x := 1;
      with SQLQuery do begin
        while not EOF do begin
          if
          (FieldByName(Horz.TableOwner.Name + 'id').AsInteger = horzids[x]) and
          (FieldByName(Vert.TableOwner.Name + 'id').AsInteger = vertids[y]) then begin
            if not Assigned(sgTable.CellStrings[y, x]) then
              sgTable.CellStrings[y, x] := TStringList.Create;
            sgTable.CellStrings[y, x].Append(' ');
            for i := 0 to clbVisibleFields.Count - 1 do begin
              if not clbVisibleFields.Checked[i] then continue;
                Field := clbVisibleFields.Items.Objects[i] as TDBField;
                sgTable.CellStrings[y, x].Append(FieldByName(Field.TableOwner.Name + Field.Name).Value);
	      end;
            Next;
          end else begin
            inc(x);
            if x >= sgTable.ColCount then begin break; end;
          end;
        end;
      end;
      Next;
    end;
  end;

  for i := 0 to clbVisibleFields.Count - 1 do
    if clbVisibleFields.Checked[i] then inc(CheckedCount);
  with sgTable do begin
    DefaultRowHeight := (CheckedCount + 1)*Canvas.TextHeight('A');
    //ColWidths[0] := 100;
    RowHeights[0] := 30;
    //clbVisibleFields.Items.;
  end;
end;

procedure TTimeTable.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i: integer;
begin
  //pict := TBitmap.Create;
  //ImageList.GetBitmap(1, pict);
  //sgTable.Canvas.Draw(aRect, pict);
  //sgTable.Canvas.CopyRect(aRect, pict.Canvas, aRect);
  //sgTable.Canvas.Draw(10, 10, pict);
  //sgTable.Canvas.Brush.Color := clWhite;
  //sgTable.Canvas.TextOut(aRect.Left, aRect.Top, sgTable.Cells[aCol, aRow]);
  //sgTable.Canvas;
  //if (aCol = 2) and (aRow = 2) then ShowMessage('S');
  with sgTable do begin
    if (aRow < Length(CellStrings)) and (aCol < Length(CellStrings[aRow])) and Assigned(CellStrings[aRow, aCol]) then
      for i := 1 to CellStrings[aRow, aCol].Count - 1 do
        Canvas.TextOut(aRect.Left, aRect.Top + i*Canvas.TextHeight('A'), CellStrings[aRow, aCol].Strings[i]);
  end;
  ImageList.Draw(sgTable.Canvas, aRect.Left + sgTable.ColWidths[aCol] - 16, aRect.Top + sgTable.RowHeights[aRow] - 16, 3, True);
end;

procedure TTimeTable.sgTableGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
begin
  with sgTable do
    if (aRow < Length(CellStrings)) and (aCol < Length(CellStrings[aRow])) and Assigned(CellStrings[aRow, aCol]) then
      HintText := CellStrings[aRow, aCol].Text;
end;

end.
















