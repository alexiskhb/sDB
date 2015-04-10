unit listview;

{$mode objfpc}{$H+}

interface

uses
  connection_transaction, Classes, lcltype, SysUtils, Forms, Menus, DBCtrls, DB, DBGrids,
  ExtCtrls, sqldb, Dialogs, Controls, StdCtrls, metadata, Spin, Buttons, Messages;

type

  TRelationalOperation = (roGreater, roLess, roNotLess, roNotGreater, roEqual,
    roInequal, roStartsWith, roContaining);

  TDBTableForm = class;
  TCustomEditClass = class of TCustomEdit;

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
    procedure SetFilterTop(Value: integer);
    procedure SetFilterHeight(Value: integer);
    procedure SetFilterTag(Value: integer);
    function GetField: TDBField;
    function GetValue: Variant;
    function GetOperation: string;
  public
    cbbFields: TComboBox;
    cbbOperations: TComboBox;
    btnDeleteFilter: TButton;
    btnAddFilter: TButton;
    ConstantEditor: TCustomEdit;
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
    procedure DeleteFilterClick(Sender: TObject);
    procedure AddFieldsForChoose(ATable: TDBTable);
    procedure EditChange(Sender: TObject);
    procedure OperationChange(Sender: TObject);
    procedure AddFilterClick(Sender: TObject);
    constructor Create(AIndex: integer; AForm: TDBTableForm);
    destructor Destroy;
	end;

  { TDBTableForm }

  TDBTableForm = class(TForm)
    DBNavigator: TDBNavigator;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    SQLQuery: TSQLQuery;
    btnAddFilter: TButton;
    sbtnFilter: TSpeedButton;
		sbxFilters: TScrollBox;
		pnlControls: TPanel;
	  Splitter: TSplitter;
    miTable: TMenuItem;
    miCloseOtherTables: TMenuItem;
    MainMenu: TMainMenu;
    miCloseTable: TMenuItem;
	  procedure btnAddFilterClick(Sender: TObject);
    procedure miCloseOtherTablesClick(Sender: TObject);
    procedure miCloseTableClick(Sender: TObject);
		procedure sbtnFilterClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
    procedure SetSQLQuery;
    procedure AddConditionsToQuery;
    procedure AddColumnsToQuery(ATable: TDBTable);
    procedure AddColumnsToGrid(ATable: TDBTable);
    procedure DestroyFilterClick(Sender: TObject);
    procedure FDBGridTitleClick(Column: TColumn);
    procedure DBGridColumnMoved(Sender: TObject; FromIndex,
		ToIndex: Integer);
    procedure FilterDataChanged(Sender: TObject);
    class procedure CreateTableForm(ATag: integer; aCaption: string);
    class procedure DestroyTableForm(ATag: integer);
    class procedure FormSetFocus(ATag: integer);
    class function FormExists(ATag: integer): boolean;
  private
    FFilters: array of TQueryFilter;
    FieldOfColumn: TStringList;
    OrderIsDesc: boolean;
    FilterInPosition: array of integer;
    procedure LocateFiltersOnDelete(ATag: integer);
    procedure LocateFiltersOnAdd(ATag, APosition: integer);
  end;

  TDBTableFormDynArray = array of TDBTableForm;

implementation

{$R *.lfm}

var
  DBTableForms: TDBTableFormDynArray;
  TypeOfEditor: array [Low(TFieldType)..High(TFieldType)] of TCustomEditClass;
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

procedure TDBTableForm.AddColumnsToGrid(ATable: TDBTable);
var
  i: integer;
begin
  with DBGrid do
    with ATable do
      for i := 0 to High(Fields) do begin
        if Fields[i].Visible then begin
          Columns.Add.FieldName := ATable.Name + Fields[i].Name;
          Columns[Columns.Count - 1].Title.Caption := Fields[i].Caption;
          Columns[Columns.Count - 1].Width := Fields[i].Width;
          Columns[Columns.Count - 1].Visible := Fields[i].Visible;
          FieldOfColumn.AddObject(Columns[Columns.Count - 1].FieldName, Fields[i]);
        end;
        if Assigned(Fields[i].TableRef) then
          AddColumnsToGrid(Fields[i].TableRef);
      end;
end;

procedure TDBTableForm.AddColumnsToQuery(ATable: TDBTable);
var
  i: integer;
begin
  with SQLQuery.SQL do
    with ATable do
      for i := 0 to High(Fields) do begin
        if Fields[i].Visible then begin
          Append(Name + '.' + Fields[i].Name + ' as ' + Name + Fields[i].Name);
          Append(',');
        end;
        if Assigned(Fields[i].TableRef) then
          AddColumnsToQuery(Fields[i].TableRef);
      end;
end;

procedure TDBTableForm.SetSQLQuery;
var
  i: integer;
begin
  with SQLQuery.SQL do begin
    Clear;
    Append('select');
    AddColumnsToQuery(DBTables[Self.Tag]);
    Delete(Count - 1);
    Append('from');
    Append(DBTables[Self.Tag].Name + ' ');
    with DBTables[Self.Tag] do
      for i := Low(Fields) to High(Fields) do
        if Assigned(Fields[i].TableRef) then begin
          Append('join ' + Fields[i].TableRef.Name + ' on ');
          Append('  ' + Fields[i].TableRef.Name + '.' + Fields[i].FieldRef.Name + ' = ');
          Append('    ' + Name + '.' + Fields[i].Name);
        end;
  end;
end;

procedure TDBTableForm.FDBGridTitleClick(Column: TColumn);
var
  Field: TDBField;
  i: integer;
begin
  Field := (FieldOfColumn.Objects[Column.Index] as TDBField);

  for i := 0 to DBGrid.Columns.Count - 1 do
    with DBGrid.Columns[i].Title do
      if (Pos('↓', Caption) <> 0) or (Pos('↑', Caption) <> 0) then begin
        Caption := Copy(Caption, Length('↑ ') + 1, Length(Caption));
        break;
		  end;

  SQLQuery.Close;
  SetSQLQuery;
  AddConditionsToQuery;
  SQLQuery.SQL.Append('order by ' + Field.Owner.Name + '.' + Field.Name);
  if OrderIsDesc then begin
    SQLQuery.SQL.Append('  desc');
    Column.Title.Caption := '↑ ' + Column.Title.Caption;
	end
	else begin
    SQLQuery.SQL.Append('  asc');
    Column.Title.Caption := '↓ ' + Column.Title.Caption;
	end;
	OrderIsDesc := not OrderIsDesc;
  SQLQuery.Open;
end;

procedure TDBTableForm.DBGridColumnMoved(Sender: TObject; FromIndex,
		ToIndex: Integer);
begin
  FieldOfColumn.Move(FromIndex - 1, ToIndex - 1);
end;

procedure TDBTableForm.FormDestroy(Sender: TObject);
begin
  DBTableForms[(Sender as TDBTableForm).Tag] := nil;
end;

procedure TDBTableForm.FormShow(Sender: TObject);
begin
  with SQLQuery do begin
    Transaction := ConTran.DBTransaction;
    Database := ConTran.DBConnection;
    SQLQuery.Close;
    SetSQLQuery;
  end;

  with DataSource do
    DataSet := SQLQuery;

  FieldOfColumn := TStringList.Create;
  FieldOfColumn.Sorted := false;

  with DBGrid do begin
    DataSource := DataSource;
    AddColumnsToGrid(DBTables[Self.Tag]);
    OnTitleClick := @FDBGridTitleClick;
    OnColumnMoved := @DBGridColumnMoved;
  end;

  with DBNavigator do begin
    DataSource := DataSource;
    VisibleButtons := [nbFirst, nbNext, nbPrior, nbLast];
  end;

  SQLQuery.Open;
end;

procedure TDBTableForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TDBTableForm.miCloseTableClick(Sender: TObject);
begin
  Close;
end;

procedure TDBTableForm.sbtnFilterClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Enabled := false;
  SQLQuery.Close;
  SetSQLQuery;
  AddConditionsToQuery;
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
            SQL.Append('and ' + ChosenField.Owner.Name + '.' + ChosenField.Name);
            SQL.Append(Operation + ' :' + ChosenField.Owner.Name + ChosenField.Name + IntToStr(i));
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
  sbxFilters.RemoveControl(Sender as TButton);
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
    FFilters[FilterInPosition[i]].Top := i * FFilters[FilterInPosition[i]].Height;

  if ClientToScreen(Point(0, sbxFilters.Height + pnlControls.Height)).Y < Mouse.CursorPos.Y then
    sbxFilters.Height := sbxFilters.Height + FFilters[ATag].Height;
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
    FFilters[FilterInPosition[i]].Top := i * FFilters[FilterInPosition[i]].Height;

  if (FFilters[ATag].Top + FFilters[ATag].Height >= sbxFilters.Height) and
    (FFilters[ATag].Top <= sbxFilters.Height) and
      (sbxFilters.Height <= 4*(Height div 5)) then
        sbxFilters.Height := sbxFilters.Height + FFilters[ATag].Height;
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
    OnClick := @DeleteFilterClick;
  end;

	cbbFields := TComboBox.Create(AForm.sbxFilters);
  with cbbFields do begin
    Parent := AForm.sbxFilters;
    Left := btnDeleteFilter.Left + btnDeleteFilter.Width + 1;
    AutoSize := false;
    Height := btnDeleteFilter.Height;
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

procedure TQueryFilter.DeleteFilterClick(Sender: TObject);
begin
  FChangingData(Self);
  FDestroying(Sender);
end;

procedure TDBTableForm.FilterDataChanged(Sender: TObject);
begin
  sbtnFilter.Enabled := true;
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
  //FreeAndNil(btnDeleteFilter);
  FreeAndNil(cbbFields);
  if Assigned(ConstantEditor) then begin
    FreeAndNil(ConstantEditor);
    FreeAndNil(cbbOperations);
    FreeAndNil(btnAddFilter);
	end;
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






