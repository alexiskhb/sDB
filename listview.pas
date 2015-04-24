unit listview;
//2 массива -> 1 массив
//fresh - процедуры в 1
//panel align top

{$mode objfpc}{$H+}

interface

uses
  connection_transaction, Classes, lcltype, SysUtils, Forms, Menus, DBCtrls, DB, DBGrids,
  ExtCtrls, sqldb, Dialogs, Controls, StdCtrls, metadata, Spin, Buttons, Messages, record_cards,
  edit_database;

type

  TRelationalOperation = (roGreater, roLess, roNotLess, roNotGreater, roEqual,
    roInequal, roStartsWith, roContaining);

  TDBTableForm = class;

  TRelOperation = class
  private
    FCaption: string;
    FCode: string;
  public
    property Caption: string read FCaption;
    property Code: string read FCode;
    constructor Create(aCaption, aCode: string);
	end;

  TQueryFilter = class
  private
    FTag: integer;
    FDestroying: TNotifyEvent;
    FChangingData: TNotifyEvent;
    FOwner: TDBTableForm;
    FHeight: integer;
    FTop: integer;
    FAddingFilter: TNotifyEvent;
    cbbFields: TComboBox;
    cbbOperations: TComboBox;
    btnDeleteFilter: TButton;
    btnAddFilter: TButton;
    ConstantEditor: TCustomEdit;
    procedure SetFilterTop(Value: integer);
    procedure SetFilterHeight(Value: integer);
    procedure SetFilterTag(Value: integer);
    function GetField: TDBField;
    function GetValue: Variant;
    function GetOperation: string;
  public
    property ChosenField: TDBField read GetField;
    property Tag: integer read FTag write SetFilterTag;
    property OnDestroy: TNotifyEvent read FDestroying write FDestroying;
    property OnChangeData: TNotifyEvent read FChangingData write FChangingData;
    property OnFilterAdd: TNotifyEvent read FAddingFilter write FAddingFilter;
    property Top: integer read FTop write SetFilterTop;
    property Height: integer read FHeight write SetFilterHeight;
    property Value: variant read GetValue;
    property Operation: string read GetOperation;

    procedure ChosenFieldChange(Sender: TObject);
    procedure DeleteFilterClick(Sender: TObject; MouseButton: TMouseButton; ShiftState: TShiftState; X, Y: longint);
    procedure AddFieldsForChoose(ATable: TDBTable);
    procedure EditChange(Sender: TObject);
    procedure OperationChange(Sender: TObject);
    procedure AddFilterClick(Sender: TObject);
    constructor Create(AIndex: integer; AForm: TDBTableForm);
    destructor Destroy;
 	end;

  { TDBTableForm }

  TDBTableForm = class(TForm)
	  btnInsertRecord: TBitBtn;
	  btnDeleteRecord: TBitBtn;
    DBNavigator: TDBNavigator;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
		miReset: TMenuItem;
    SQLQuery: TSQLQuery;
    btnAddFilter: TButton;
    btnFilter: TSpeedButton;
		sbxFilters: TScrollBox;
		pnlControls: TPanel;
	  Splitter: TSplitter;
    miTable: TMenuItem;
    miCloseOtherTables: TMenuItem;
    MainMenu: TMainMenu;
    miCloseTable: TMenuItem;
    RecordCard: TRecordCard;
	  procedure btnAddFilterClick(Sender: TObject);
		procedure btnInsertRecordClick(Sender: TObject);
		procedure DBNavigatorClick(Sender: TObject; Button: TDBNavButtonType);
    procedure miCloseOtherTablesClick(Sender: TObject);
    procedure miCloseTableClick(Sender: TObject);
		procedure miResetClick(Sender: TObject);
		procedure btnFilterClick(Sender: TObject);
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
    procedure DBGridColumnMoved(Sender: TObject; FromIndex,
		ToIndex: Integer);
    procedure FilterDataChanged(Sender: TObject);
    procedure LocateFiltersOnDelete(ATag: integer);
    procedure LocateFiltersOnAdd(ATag, APosition: integer);
    procedure DBGridDblClick(Sender: TObject);
    procedure btnDeleteRecordClick(Sender: TObject);
    procedure SQLQueryAfterDelete(DataSet: TDataSet);
		procedure SQLQueryBeforeDelete(DataSet: TDataSet);
    class procedure CreateTableForm(ATag: integer; aCaption: string);
    class procedure DestroyTableForm(ATag: integer);
    class procedure FormSetFocus(ATag: integer);
    class procedure RefreshTables;
    class function FormExists(ATag: integer): boolean;
  private
    FFilters: array of TQueryFilter;
    InitialFieldsOrder: TStringList;
    OrderIsDesc: boolean;
    FilterInPosition: array of integer;
    FTable: TDBTable;
  end;

  TDBTableFormDynArray = array of TDBTableForm;

implementation

{$R *.lfm}

var
  DBTableForms: TDBTableFormDynArray;
  AvailableOperations: array [Low(TFieldType)..High(TFieldType)] of set of TRelationalOperation;
  Operations: array [Low(TRelationalOperation)..High(TRelationalOperation)] of TRelOperation;

class function TDBTableForm.FormExists(ATag: integer): boolean;
begin
  Result := DBTableForms[ATag] <> nil;
end;

class procedure TDBTableForm.CreateTableForm(ATag: integer; aCaption: string);
begin
  Application.CreateForm(TDBTableForm, DBTableForms[ATag]);
  with DBTableForms[ATag] do begin
    Caption := aCaption;
    Tag := ATag;
    Show;
  end;
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
        InitialFieldsOrder.AddObject(Columns[Columns.Count - 1].FieldName, Fields[i]);
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
begin
  for i := 0 to DBGrid.Columns.Count - 1 do
    with DBGrid.Columns[i].Title do
      if (Pos('↑', Caption) <> 0) then begin
        SQLQuery.SQL.Append('order by ' + DBGrid.Columns[i].DisplayName);
        SQLQuery.SQL.Append('  desc');
        break;
		  end else
        if (Pos('↓', Caption) <> 0) then begin
          SQLQuery.SQL.Append('order by ' + DBGrid.Columns[i].DisplayName);
          SQLQuery.SQL.Append('  asc');
          break;
			  end;
end;

procedure TDBTableForm.RefreshTable;
begin
  SQLQuery.Close;
  SetSQLQuery(DBTables[Self.Tag], SQLQuery);
  AddConditionsToQuery;
  AddSort;
  SQLQuery.Open;
  SQLQuery.Last;
  SQLQuery.First;
end;

procedure TDBTableForm.DBGridColumnMoved(Sender: TObject; FromIndex,
		ToIndex: Integer);
begin
  //InitialFieldsOrder.Move(FromIndex - 1, ToIndex - 1);
end;

procedure TDBTableForm.DBGridDblClick(Sender: TObject);
var
  i: integer;
begin
  RecordCard := TEditRecordCard.Create(FTable, InitialFieldsOrder, DBGrid);
  RecordCard.Hide;
  if RecordCard.ShowModal = mrOK then
    UpdateRecord(FTable, (RecordCard as TEditRecordCard).OldValues, RecordCard.NewValues);
  FreeAndNil(RecordCard);
  RefreshTables;
end;

procedure TDBTableForm.FormDestroy(Sender: TObject);
begin
  DBTableForms[(Sender as TDBTableForm).Tag] := nil;
end;

procedure TDBTableForm.FormShow(Sender: TObject);
begin
  FTable := DBTables[Self.Tag];

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

  InitialFieldsOrder := TStringList.Create;
  InitialFieldsOrder.Sorted := false;

  with DBGrid do begin
    DataSource := DataSource;
    AddColumnsToGrid(FTable);
    Options := Options - [dgEditing];
    OnTitleClick := @FDBGridTitleClick;
    OnColumnMoved := @DBGridColumnMoved;
    OnDblClick := @DBGridDblClick;
  end;

  with DBNavigator do begin
    DataSource := DataSource;
    Controls[Ord(nbDelete)].Enabled := true;
  end;

  SQLQuery.ReadOnly := false;

  SQLQuery.Open;
  SQLQuery.Last;
  SQLQuery.First;

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
  SetLength(FilterInPosition, 0);
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
  ResetGridTitles;
  SQLQuery.Open;
end;

procedure TDBTableForm.AddConditionsToQuery;
var
  i, k: integer;
begin
  SQLQuery.SQL.Append('where 1 = 1');

  with SQLQuery do
    for i := 0 to Length(FilterInPosition) - 1 do
      with FFilters[FilterInPosition[i]] do
        if Assigned(FFilters[FilterInPosition[i]]) and Assigned(ConstantEditor) then
          if (ConstantEditor.Visible) then begin
            SQL.Append('and ' + ChosenField.TableOwner.Name + '.' + ChosenField.Name);
            SQL.Append(Operation + ' :' + ChosenField.TableOwner.Name + ChosenField.Name + IntToStr(i));
          end else
            SQL.Append('or 1 = 1');

  k := 0;
  for i := 0 to Length(FilterInPosition) - 1 do
    with FFilters[FilterInPosition[i]] do
      if Assigned(FFilters[FilterInPosition[i]]) and Assigned(ConstantEditor) and
      (ConstantEditor.Visible) then
        with SQLQuery do begin
          Params[k].Value := Value;
          inc(k);
        end;
  //showmessage(SQLQuery.SQL.Text);
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
  VTag, i: integer;
begin
  VTag := (Sender as TButton).Tag;
  LocateFiltersOnDelete(VTag);
  FFilters[VTag].Destroy;
  //sbxFilters.RemoveControl(Sender as TButton);
  FFilters[VTag] := nil;

  for i := High(FFilters) downto 0 do
    if (FFilters[i] = nil) then
      SetLength(FFilters, Length(FFilters) - 1)
    else
      break;
end;

procedure TDBTableForm.LocateFiltersOnDelete(ATag: integer);
var
  i: integer;
  found: boolean;
begin
  found := false;

  for i := 0 to Length(FilterInPosition) - 2 do begin
    if FilterInPosition[i] = ATag then
      found := true;
    if found then
      FilterInPosition[i] := FilterInPosition[i + 1];
	end;

  SetLength(FilterInPosition, Length(FilterInPosition) - 1);

	sbxFilters.Height := sbxFilters.Height - FFilters[ATag].Height;

  for i := 0 to High(FilterInPosition) do
    FFilters[FilterInPosition[i]].Top := i * (FFilters[FilterInPosition[i]].Height + 2);

  if ClientToScreen(Point(0, sbxFilters.Height + pnlControls.Height)).Y < Mouse.CursorPos.Y then
    sbxFilters.Height := sbxFilters.Height + FFilters[ATag].Height + 2;
end;

procedure TDBTableForm.LocateFiltersOnAdd(ATag, APosition: integer);
var
  i: integer;
begin
  if APosition = -1 then
    APosition := Length(FilterInPosition) - 1;

  SetLength(FilterInPosition, Length(FilterInPosition) + 1);

  for i := Length(FilterInPosition) - 1 downto APosition + 2 do
    FilterInPosition[i] := FilterInPosition[i - 1];

  FilterInPosition[APosition + 1] := ATag;

  for i := 0 to High(FilterInPosition) do
    FFilters[FilterInPosition[i]].Top := i * (FFilters[FilterInPosition[i]].Height + 2);

  if (FFilters[ATag].Top + FFilters[ATag].Height + 2 >= sbxFilters.Height) and
    (FFilters[ATag].Top <= sbxFilters.Height) and
      (sbxFilters.Height <= 4*(Height div 5)) then
        sbxFilters.Height := sbxFilters.Height + FFilters[ATag].Height + 2;
end;

constructor TQueryFilter.Create(AIndex: integer; AForm: TDBTableForm);
begin
  FOwner := AForm;
  FTag := AIndex;

  btnDeleteFilter := TButton.Create(AForm.sbxFilters);
  with btnDeleteFilter do begin
    Parent := AForm.sbxFilters;
    Height := 26;
    Width := Height;
    FHeight := Height;
    Caption := 'X';
    Left := 2;
    Tag := AIndex;
    OnMouseUp := @DeleteFilterClick;
  end;

	cbbFields := TComboBox.Create(AForm.sbxFilters);
  with cbbFields do begin
    Parent := AForm.sbxFilters;
    Left := btnDeleteFilter.Left + btnDeleteFilter.Width + 1;
    AutoSize := false;
    Height := btnDeleteFilter.Height + 2;
    Style := csDropDownList;
    AddFieldsForChoose(DBTables[AForm.Tag]);
    AddItem('ИЛИ', nil);
    OnChange := @ChosenFieldChange;
  end;
end;

procedure TQueryFilter.AddFieldsForChoose(ATable: TDBTable);
var
  i: integer;
begin
  with cbbFields do
    with ATable do
      for i := 0 to High(Fields) do begin
        if Fields[i].Visible then begin
          AddItem(Fields[i].Caption, Fields[i]);
        end;
        if Assigned(Fields[i].TableRef) then
          AddFieldsForChoose(Fields[i].TableRef);
      end;
end;

procedure TQueryFilter.DeleteFilterClick(Sender: TObject; MouseButton: TMouseButton; ShiftState: TShiftState; X, Y: longint);
begin
  FChangingData(Self);
  FDestroying(Sender);
end;

procedure TDBTableForm.FilterDataChanged(Sender: TObject);
begin
  btnFilter.Enabled := true;
end;

procedure TDBTableForm.SQLQueryAfterDelete(DataSet: TDataSet);
begin
  RefreshTables;
end;

procedure TDBTableForm.btnDeleteRecordClick(Sender: TObject);
begin
  if MessageDlg('Удалить запись?', mtConfirmation, mbOKCancel, 0) = 1 then
    DeleteRecord(FTable, DBGrid);
  RefreshTables;
end;

procedure TDBTableForm.SQLQueryBeforeDelete(DataSet: TDataSet);
begin
  DeleteRecord(FTable, DBGrid);
end;

procedure TDBTableForm.btnAddFilterClick(Sender: TObject);
var
  i, VPos: integer;
begin
  VPos := (Sender as TButton).Tag;

  for i := 0 to High(FilterInPosition) do
    if FilterInPosition[i] = VPos then begin
      VPos := i;
      break;
		end;

  for i := 0 to Length(FFilters) - 1 do
    if (FFilters[i] = nil) then begin
      FFilters[i] := TQueryFilter.Create(i, Self);
      LocateFiltersOnAdd(i, VPos);
      with FFilters[i] do begin
        OnDestroy := @DestroyFilterClick;
        OnChangeData := @FilterDataChanged;
        OnFilterAdd := @btnAddFilterClick;
			end;
			exit;
    end;

  SetLength(FFilters, Length(FFilters) + 1);
  FFilters[High(FFilters)] := TQueryFilter.Create(High(FFilters), Self);
  LocateFiltersOnAdd(High(FFilters), VPos);
  FFilters[High(FFilters)].OnDestroy := @DestroyFilterClick;
  FFilters[High(FFilters)].OnChangeData := @FilterDataChanged;
  FFilters[High(FFilters)].OnFilterAdd := @btnAddFilterClick;
end;

procedure TDBTableForm.btnInsertRecordClick(Sender: TObject);
begin
  RecordCard := TRecordCard.Create(DBTables[Self.Tag], InitialFieldsOrder);
  RecordCard.Hide;
  if RecordCard.ShowModal = mrOK then
    InsertRecord(FTable, RecordCard.NewValues);
  RefreshTables;
  FreeAndNil(RecordCard);
end;

procedure TDBTableForm.DBNavigatorClick(Sender: TObject;
		Button: TDBNavButtonType);
begin
  case Button of
    nbEdit: DBGridDblClick(DBGrid);
		else exit;
	end;
end;

procedure TQueryFilter.ChosenFieldChange(Sender: TObject);
var
  VSender: TComboBox;
  tempft: TFieldType;
  ro: TRelationalOperation;
begin
  VSender := (Sender as TComboBox);

  if Assigned(ConstantEditor) then begin
    FreeAndNil(ConstantEditor);
    FreeAndNil(cbbOperations);
    FreeAndNil(btnAddFilter);
	end;

  if Assigned(VSender.Items.Objects[VSender.ItemIndex]) then
    tempft := ((VSender.Items.Objects[VSender.ItemIndex]) as TDBField).FieldType
  else
    tempft := ftUnknown;

  cbbOperations := TComboBox.Create(FOwner.sbxFilters);
  cbbOperations.Parent := FOwner.sbxFilters;
  with cbbOperations do begin
    AutoSize := false;
    Top := btnDeleteFilter.Top;
    Left := cbbFields.Left + cbbFields.Width + 1;
    Height := cbbFields.Height;
    Style := csDropDownList;
    for ro := Low(TRelationalOperation) to High(TRelationalOperation) do
      if ro in AvailableOperations[tempft] then
        AddItem(Operations[ro].Caption, Operations[ro]);
    ItemIndex := 0;
    OnChange := @OperationChange;
	end;

  ConstantEditor := TypeOfEditor[tempft].Create(FOwner.sbxFilters);
  ConstantEditor.Parent := FOwner.sbxFilters;
  with ConstantEditor do begin
    AutoSize := false;
    Height := cbbFields.Height;
    Width := cbbFields.Width;;
    Top := btnDeleteFilter.Top;
    Left := cbbOperations.Left + cbbOperations.Width + 1;
    if TypeOfEditor[tempft] = TSpinEdit then
      with (ConstantEditor as TSpinEdit) do begin
        MaxValue := High(Integer);
        MinValue := Low(Integer);
	  	end;
    if TypeOfEditor[tempft] = TEdit then
      with (ConstantEditor as TEdit) do begin
        MaxLength := ((VSender.Items.Objects[VSender.ItemIndex]) as TDBField).VarCharLimit;
	  	end;
    OnChange := @EditChange;
	end;

  btnAddFilter := TButton.Create(FOwner.sbxFilters);
  btnAddFilter.Parent := FOwner.sbxFilters;
  with btnAddFilter do begin
    Height := btnDeleteFilter.Height;
    Width := Height;
    FHeight := Height;
    Caption := '+';
    Tag := btnDeleteFilter.Tag;
    Top := btnDeleteFilter.Top;
    Left := ConstantEditor.Left + ConstantEditor.Width + 1;
    OnClick := @AddFilterClick;
  end;

  if tempft = ftUnknown then begin
    ConstantEditor.Visible := false;
    cbbOperations.Visible := false;
    btnAddFilter.Visible := false;
	end;

  FChangingData(Self);
end;

procedure TQueryFilter.EditChange(Sender: TObject);
begin
  FChangingData(Self);
end;

procedure TQueryFilter.OperationChange(Sender: TObject);
begin
  FChangingData(Self);
end;

procedure TQueryFilter.AddFilterClick(Sender: TObject);
begin
  FAddingFilter(Sender);
end;

procedure TQueryFilter.SetFilterTop(Value: integer);
begin
  btnDeleteFilter.Top := Value;
  cbbFields.Top := Value;
  if Assigned(ConstantEditor) then begin
    ConstantEditor.Top := Value;
    cbbOperations.Top := Value;
    btnAddFilter.Top := Value;
	end;
  FTop := Value;
end;

procedure TQueryFilter.SetFilterHeight(Value: integer);
begin
  btnDeleteFilter.Height := Value;
  cbbFields.Height := Value;
  if Assigned(ConstantEditor) then begin
    ConstantEditor.Height := Value;
    cbbOperations.Height := Value;
    btnAddFilter.Height := Value;
	end;
	FHeight := Value;
end;

procedure TQueryFilter.SetFilterTag(Value: integer);
begin
  btnDeleteFilter.Tag := Value;
  btnAddFilter.Tag := Value;
  FTag := Value;
end;

function TQueryFilter.GetField: TDBField;
begin
  if Assigned(cbbFields.Items.Objects[cbbFields.ItemIndex] as TDBField) then
    Result := (cbbFields.Items.Objects[cbbFields.ItemIndex] as TDBField)
  else
    Result := nil;
end;

function TQueryFilter.GetOperation: string;
begin
  Result := (cbbOperations.Items.Objects[cbbOperations.ItemIndex] as TRelOperation).Code;
end;

function TQueryFilter.GetValue: Variant;
begin
  if ((cbbFields.Items.Objects[cbbFields.ItemIndex]) as TDBField).FieldType = ftInteger then
    Result := (ConstantEditor as TSpinEdit).Value
  else
    Result := ConstantEditor.Text;
end;

destructor TQueryFilter.Destroy;
begin
  FreeAndNil(btnDeleteFilter);
  FreeAndNil(cbbFields);
  if Assigned(ConstantEditor) then begin
    FreeAndNil(ConstantEditor);
    FreeAndNil(cbbOperations);
    FreeAndNil(btnAddFilter);
	end;
  inherited;
end;

constructor TRelOperation.Create(ACaption, ACode: string);
begin
  FCaption := ACaption;
  FCode := ACode;
end;

initialization

  SetLength(DBTableForms, Length(DBTables));
  TypeOfEditor[ftInteger] := TSpinEdit;
  TypeOfEditor[ftString] := TEdit;
  TypeOfEditor[ftUnknown] := TEdit;
  AvailableOperations[ftInteger] := [roEqual, roInequal, roGreater, roNotGreater, roNotLess, roLess];
  AvailableOperations[ftString] := [roEqual, roInequal, roContaining, roStartsWith];
  Operations[roGreater] := TRelOperation.Create('>', '  > ');
  Operations[roContaining] := TRelOperation.Create('Содержит', '  containing ');
  Operations[roEqual] := TRelOperation.Create('=', '  = ');
  Operations[roInequal] := TRelOperation.Create('<>', '  <> ');
  Operations[roLess] := TRelOperation.Create('<', '  < ');
  Operations[roNotGreater] := TRelOperation.Create('<=', '  <= ');
  Operations[roNotLess] := TRelOperation.Create('>=', '  >= ');
  Operations[roStartsWith] := TRelOperation.Create('Начинается с', '  starts with ');

end.






