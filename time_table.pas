unit time_table;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
	StdCtrls, CheckLst, Grids, Buttons, metadata, connection_transaction, sqldb;

type

	{ TTimeTable }

  TMyStringGrid = class(TStringGrid)
    procedure Reset;
	end;

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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure AddFieldsToLists(ATable: TDBTable);
    procedure FillTable(Horz, Vert: TDBField);
		procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer;
		  aRect: TRect; aState: TGridDrawState);
  private
    FTable: TDBTable;
  public
    constructor Create(ATable: TDBTable);
  end;

implementation

{$R *.lfm}

{ TTimeTable }

procedure TMyStringGrid.Reset;
begin
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
    Parent := Self;
    Align := alClient;
    Options := Options + [goRowSizing, goColSizing, goThumbTracking] - [goFixedColSizing];
    DefaultColWidth := 100;
    DefaultRowHeight := 30;
    OnDrawCell := @GridDrawCell;
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
  count: integer;
  horzids, vertids: array of integer;
begin
  sgTable.Reset;

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
      SetLength(horzids, length(horzids) + 1);
      horzids[High(horzids)] := FieldByName(Horz.TableOwner.Name + 'id').AsInteger;
      sgTable.Columns.Add.Title.Caption := FieldByName(Horz.TableOwner.Name + Horz.Name).Value;
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
      vertids[y] := FieldByName(Vert.TableOwner.Name + 'id').AsInteger;
      sgTable.RowCount := sgTable.RowCount + 1;
      sgTable.Cells[0, y] := FieldByName(Vert.TableOwner.Name + Vert.Name).Value;
      x := 1;
      with SQLQuery do begin
        while not EOF do begin
          if
          (FieldByName(Horz.TableOwner.Name + 'id').AsInteger = horzids[x]) and
          (FieldByName(Vert.TableOwner.Name + 'id').AsInteger = vertids[y]) then begin
            for i := 0 to clbVisibleFields.Count - 1 do begin
              if not clbVisibleFields.Checked[i] then continue;
                Field := clbVisibleFields.Items.Objects[i] as TDBField;
                sgTable.Cells[x, y] :=
                  sgTable.Cells[x, y] +
                  FieldByName(Field.TableOwner.Name + Field.Name).Value + ' | ';
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
end;

procedure TTimeTable.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  pict: TBitmap;
begin
  pict := TBitmap.Create;
  ImageList.GetBitmap(1, pict);
  sgTable.Canvas.Brush.Color := clYellow;
  if (ACol = 1) and (ARow = 1) then
  ;   //sgTable.Canvas.Draw(aRect, pict);
  //sgTable.Canvas.CopyRect(aRect, pict.Canvas, aRect);
  //sgTable.Canvas.Draw(10, 10, pict);
 // sgTable.Canvas.TextOut(aRect.Left, aRect.Top, 'hello i''m a cell');
  ImageList.Draw(sgTable.Canvas, aRect.Left + sgTable.ColWidths[aCol] - 16, aRect.Top + sgTable.RowHeights[aRow] - 16, 3, True);
end;

end.
















