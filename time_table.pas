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
	  pnlControlsRight: TPanel;
	  pnlContols: TPanel;
		Splitter: TSplitter;
		CheckSplitter: TSplitter;
    sgTable: TMyStringGrid;
		SQLQuery: TSQLQuery;
		procedure btnApplyClick(Sender: TObject);
    procedure cbbChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure AddFieldsToLists(ATable: TDBTable);
    procedure FillTitles(Horz, Vert: TDBField);
  private
    FTable: TDBTable;
    FSQLQuery: TSQLQuery;
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
    Options := Options + [goRowSizing, goColSizing] - [goFixedColSizing];
    DefaultColWidth := 100;
    DefaultRowHeight := 30;

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
  FillTitles(
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

procedure TTimeTable.FillTitles(Horz, Vert: TDBField);
var
  x, y, i: integer;
  Field: TDBField;
begin
  y := 0;
  sgTable.Reset;

  with SQLQuery do begin

	end;

  with ConTran.CommonSQLQuery do begin
    Close;
    SetSQLQuery(Horz.TableOwner, ConTran.CommonSQLQuery);
    SQL.Append('order by ' + Horz.SortField.TableOwner.Name + Horz.SortField.Name + ' asc');
    Open;
    First;
    while not EOF do begin
      sgTable.Columns.Add.Title.Caption := FieldByName(Horz.TableOwner.Name + Horz.Name).Value;
      Next;
		end;

    Close;
    SetSQLQuery(Vert.TableOwner, ConTran.CommonSQLQuery);
    SQL.Append('order by ' + Vert.SortField.TableOwner.Name + Vert.SortField.Name + ' asc');
    Open;
    First;
    while not EOF do begin
      inc(y);
      sgTable.RowCount := sgTable.RowCount + 1;
      sgTable.Cells[0, y] := FieldByName(Vert.TableOwner.Name + Vert.Name).Value;
      Next;
		end;

    Close;
    SetSQLQuery(FTable, ConTran.CommonSQLQuery);
    SQL.Append(
      'order by ' + Vert.SortField.TableOwner.Name + Vert.SortField.Name +
      ', ' + Horz.SortField.TableOwner.Name + Horz.SortField.Name);
    Open;
    First;
    x := 1;
    y := 1;
    while not EOF do begin
      if(FieldByName(Horz.TableOwner.Name + Horz.Name).Value = sgTable.Columns[x - 1].Title.Caption) and
        (FieldByName(Vert.TableOwner.Name + Vert.Name).Value = sgTable.Cells[0, y]) then begin
          for i := 0 to clbVisibleFields.Count - 1 do begin
            if not clbVisibleFields.Checked[i] then
              continue;
            Field := clbVisibleFields.Items.Objects[i] as TDBField;
            sgTable.Cells[x, y] :=
              sgTable.Cells[x, y] +
              FieldByName(Field.TableOwner.Name + Field.Name).Value + #13 + #10;
				  end;
			end
			else begin
        inc(x);
        if x >= sgTable.ColCount then begin
          x := 1;
          inc(y);
        end;
      end;
      Next;
		end;
	end;

end;

end.
















