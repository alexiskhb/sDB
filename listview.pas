unit listview;

{$mode objfpc}{$H+}

interface

uses
  connection_transaction, Classes, lcltype, SysUtils, Forms, Menus, DBCtrls, DB, DBGrids,
  ExtCtrls, sqldb, Dialogs, Controls, StdCtrls, metadata, Buttons, record_cards,
  query_filter;

type

  TDBTableForm = class;

  { TDBTableForm }

  TDBTableForm = class(TForm)
    btnEditRecord: TBitBtn;
    btnInsertRecord: TBitBtn;
    btnDeleteRecord: TBitBtn;
    DBNavigator: TDBNavigator;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    miShowAsTable: TMenuItem;
    miReset: TMenuItem;
    SQLQuery: TSQLQuery;
    btnAddFilter: TButton;
    btnFilter: TSpeedButton;
    sbxFilters: TScrollBox;
    pnlControls: TPanel;
    Splitter: TSplitter;
    miList: TMenuItem;
    miCloseOtherTables: TMenuItem;
    MainMenu: TMainMenu;
    miCloseTable: TMenuItem;
    RecordCard: TRecordCard;
    procedure btnEditRecordClick(Sender: TObject);
    procedure DBGridColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
    procedure DBGridDblClick(Sender: TObject);
    procedure btnInsertRecordClick(Sender: TObject);
    procedure btnDeleteRecordClick(Sender: TObject);
    procedure DBNavigatorClick(Sender: TObject; Button: TDBNavButtonType);
    procedure btnAddFilterClick(Sender: TObject);
    procedure btnFilterClick(Sender: TObject);
    procedure miShowAsTableClick(Sender: TObject);
    procedure miCloseOtherTablesClick(Sender: TObject);
    procedure miCloseTableClick(Sender: TObject);
    procedure miResetClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AddConditionsToQuery;
    procedure AddSort;
    procedure RefreshTable;
    procedure ResetGridTitles;
    procedure AddColumnsToGrid(ATable: TDBTable);
    procedure DestroyFilterClick(Sender: TObject);
    procedure FDBGridTitleClick(Column: TColumn);
    procedure FilterDataChanged(Sender: TObject);
    procedure RecordCardOkClick(Sender: TObject);
    procedure RememberCursorPosition(Sender: TObject);
    class procedure DestroyTableForm(ATag: integer);
    class procedure FormSetFocus(ATag: integer);
    class procedure RefreshTables;
    class function FormExists(ATag: integer): boolean;
  private
    FFilters: array of TQueryFilter;
    FFieldsOrder: TStringList;
    OrderIsDesc: boolean;
    FTable: TDBTable;
    FCurPos: integer;
    FShowAsTable: TNotifyEvent;
  public
    property OnShowAsTableClick: TNotifyEvent read FShowAsTable write FShowAsTable;
    constructor Create(ATable: TDBTable);
  end;

  TDBTableFormDynArray = array of TDBTableForm;

var
  DBTableForms: TDBTableFormDynArray;

implementation

{$R *.lfm}

class function TDBTableForm.FormExists(ATag: integer): boolean;
begin
  Result := DBTableForms[ATag] <> nil;
end;

class procedure TDBTableForm.DestroyTableForm(ATag: integer);
begin
  if FormExists(ATag) then
    DBTableForms[ATag].Close;
end;

class procedure TDBTableForm.FormSetFocus(ATag: integer);
begin
  if FormExists(ATag) then
    DBTableForms[ATag].SetFocus;
end;

class procedure TDBTableForm.RefreshTables;
var
  i: integer;
begin
  for i := 0 to High(DBTableForms) do
    if Assigned(DBTableForms[i]) then
      DBTableForms[i].RefreshTable;
end;

constructor TDBTableForm.Create(ATable: TDBTable);
begin
  inherited Create(Application);
  FTable := ATable;
  Caption := ATable.Caption;

  with SQLQuery do begin
    Transaction := ConTran.DBTransaction;
    Database := ConTran.DBConnection;
    SQLQuery.Close;
    SetSQLQuery(FTable, SQLQuery);
  end;

  with DataSource do begin
    DataSet := SQLQuery;
    DataSet.EnableControls;
  end;

  FFieldsOrder := TStringList.Create;
  FFieldsOrder.Sorted := false;

  with DBGrid do begin
    DataSource := DataSource;
    AddColumnsToGrid(FTable);
    Options := Options - [dgEditing] + [dgThumbTracking];
    OnTitleClick := @FDBGridTitleClick;
    OnDblClick := @DBGridDblClick;
  end;

  with DBNavigator do begin
    DataSource := DataSource;
  end;

  SQLQuery.ReadOnly := false;

  AddSort;

  SQLQuery.Open;
  SQLQuery.Last;
  SQLQuery.First;

  CardsManager.OnRequestRefreshTables := @RecordCardOkClick;
end;

procedure TDBTableForm.AddColumnsToGrid(ATable: TDBTable);
var
  i: integer;
begin
  with DBGrid, ATable do
    for i := 0 to High(Fields) do begin
      if Fields[i].Visible then begin
        Columns.Add.FieldName := ATable.Name + Fields[i].Name;
        Columns[Columns.Count - 1].Title.Caption := Fields[i].Caption;
        Columns[Columns.Count - 1].Width := Fields[i].Width + 10;
        Columns[Columns.Count - 1].Visible := Fields[i].Visible;
        FFieldsOrder.AddObject(Columns[Columns.Count - 1].FieldName, Fields[i]);
      end;
      if Assigned(Fields[i].TableRef) then
        AddColumnsToGrid(Fields[i].TableRef);
    end;
end;

procedure TDBTableForm.FDBGridTitleClick(Column: TColumn);
begin
  ResetGridTitles;
  SQLQuery.Close;
  SetSQLQuery(DBTables[Self.Tag], SQLQuery);
  AddConditionsToQuery;
  if OrderIsDesc then
    Column.Title.Caption := '↑ ' + Column.Title.Caption
  else
    Column.Title.Caption := '↓ ' + Column.Title.Caption;
  AddSort;
  OrderIsDesc := not OrderIsDesc;
  SQLQuery.Open;
end;

procedure TDBTableForm.AddSort;
var
  i: integer;
  sortfield: string;
  Field: TDBField;
begin
  for i := 0 to DBGrid.Columns.Count - 1 do begin
    Field := FFieldsOrder.Objects[i] as TDBField;
    if Field.SortField = nil then
      sortfield := Field.TableOwner.Name + Field.Name
    else
      sortfield := Field.SortField.TableOwner.Name + Field.SortField.Name;
    with DBGrid.Columns[i].Title do
      if (Pos('↑', Caption) <> 0) then begin
        SQLQuery.SQL.Append('order by ' + sortfield);
        SQLQuery.SQL.Append('  desc');
        exit;
      end else
        if (Pos('↓', Caption) <> 0) then begin
          SQLQuery.SQL.Append('order by ' + sortfield);
          SQLQuery.SQL.Append('  asc');
          exit;
        end;
  end;

  SQLQuery.SQL.Append('order by ' + FTable.Name + '.id');
  SQLQuery.SQL.Append('  asc');
end;

procedure TDBTableForm.RefreshTable;
begin
  SQLQuery.Close;
  SetSQLQuery(DBTables[Self.Tag], SQLQuery);
  AddConditionsToQuery;
  AddSort;
  SQLQuery.Open;
  SQLQuery.First;
  SQLQuery.MoveBy(FCurPos - 1);
end;

procedure TDBTableForm.RecordCardOkClick(Sender: TObject);
begin
  RefreshTables;
end;

procedure TDBTableForm.RememberCursorPosition(Sender: TObject);
begin
  FCurPos := SQLQuery.RecNo;
end;

procedure TDBTableForm.DBGridDblClick(Sender: TObject);
var
  ID: integer;
begin
  FCurPos := SQLQuery.RecNo;
  ID := SQLQuery.FieldByName(FTable.Name + 'id').Value;
  CardsManager.EditTable(FTable, ID, atUpdate);
end;

procedure TDBTableForm.FormDestroy(Sender: TObject);
begin
  DBTableForms[(Sender as TDBTableForm).Tag] := nil;
end;

procedure TDBTableForm.FormShow(Sender: TObject);
begin
  miShowAsTable.Tag := Self.Tag;
end;

procedure TDBTableForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TDBTableForm.miCloseTableClick(Sender: TObject);
begin
  Close;
end;

procedure TDBTableForm.miResetClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(FFilters) do
    if Assigned(FFilters[i]) then
      FFilters[i].Destroy;
  SetLength(FFilters, 0);
  SQLQuery.Close;
  SetSQLQuery(DBTables[Self.Tag], SQLQuery);
  ResetGridTitles;
  SQLQuery.Open;
  sbxFilters.Height := 0;
  btnFilter.Enabled := false;
end;

procedure TDBTableForm.ResetGridTitles;
var
  i: integer;
begin
  for i := 0 to DBGrid.Columns.Count - 1 do
    with DBGrid.Columns[i].Title do
      if (Pos('↑', Caption) <> 0) or (Pos('↓', Caption) <> 0) then
        Caption := Copy(Caption, Length('↑ ') + 1, Length(Caption));
end;

procedure TDBTableForm.btnFilterClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Enabled := false;
  SQLQuery.Close;
  SetSQLQuery(DBTables[Self.Tag], SQLQuery);
  AddConditionsToQuery;
  AddSort;
  SQLQuery.Open;
end;

procedure TDBTableForm.miShowAsTableClick(Sender: TObject);
begin
  FShowAsTable(Sender);
end;

procedure TDBTableForm.AddConditionsToQuery;
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

procedure TDBTableForm.miCloseOtherTablesClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(DBTableForms) do
    if (i <> Tag) then
      DestroyTableForm(i);
end;

procedure TDBTableForm.DestroyFilterClick(Sender: TObject);
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

  sbxFilters.Constraints.MaxHeight := 3 * Height div 4;
  sbxFilters.Height := Length(FFilters) * (FilterHeight + 2);
end;

procedure TDBTableForm.FilterDataChanged(Sender: TObject);
begin
  btnFilter.Enabled := true;
end;

procedure TDBTableForm.btnDeleteRecordClick(Sender: TObject);
var
  ID: integer;
begin
  FCurPos := SQLQuery.RecNo;
  ID := SQLQuery.FieldByName(FTable.Name + 'id').Value;
  if MessageDlg('Удалить запись?', mtConfirmation, mbOKCancel, 0) = 1 then
    CardsManager.EditTable(FTable, ID, atDelete);
  RefreshTables;
end;

procedure TDBTableForm.btnAddFilterClick(Sender: TObject);
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
  FFilters[VPos + 1].OnChangeData := @FilterDataChanged;
  FFilters[VPos + 1].OnFilterAdd := @btnAddFilterClick;

  for i := 0 to Length(FFilters) - 1 do begin
    FFilters[i].Tag := i;
    FFilters[i].Top := i * (FFilters[i].Height + 2);
	end;

  sbxFilters.Constraints.MaxHeight := 3 * Height div 4;
  sbxFilters.Height := Length(FFilters) * (FilterHeight + 2);
end;

procedure TDBTableForm.btnEditRecordClick(Sender: TObject);
begin
  DBGridDblClick(Sender);
end;

procedure TDBTableForm.DBGridColumnMoved(Sender: TObject; FromIndex,
		ToIndex: Integer);
begin
  FFieldsOrder.Move(FromIndex - 1, ToIndex - 1);
end;

procedure TDBTableForm.btnInsertRecordClick(Sender: TObject);
begin
  FCurPos := SQLQuery.RecNo;
  CardsManager.EditTable(FTable, NextID, atInsert);
end;

procedure TDBTableForm.DBNavigatorClick(Sender: TObject;
  Button: TDBNavButtonType);
begin
  case Button of
    nbEdit: DBGridDblClick(DBGrid);
    else exit;
  end;
end;

initialization

  SetLength(DBTableForms, Length(DBTables));

end.






