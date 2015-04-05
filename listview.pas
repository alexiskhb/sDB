unit listview;

{$mode objfpc}{$H+}

interface

uses
  connection_transaction, Classes, lcltype, SysUtils, Forms, Menus, DBCtrls, DB, DBGrids,
  ExtCtrls, sqldb, Dialogs, Controls, StdCtrls, metadata, Spin, Buttons;

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
    FConstantEditor: TCustomEdit;
    FFieldsList: TComboBox;
    FOperationsList: TComboBox;
    FDeleteFilter: TButton;
    FOwner: TDBTableForm;
    FHeight: integer;
    FTop: integer;
    procedure SetFilterTop(Value: integer);
    procedure SetFilterHeight(Value: integer);
    procedure SetFilterTag(Value: integer);
    function GetField: TDBField;
    function GetValue: Variant;
    function GetOperation: string;
  public
    procedure FieldChoose(Sender: TObject);
    procedure DeleteFilterClick(Sender: TObject);
    procedure AddFieldsForChoose(aTable: TDBTable);
    procedure EditChange(Sender: TObject);
    procedure OperationChange(Sender: TObject);
    property ChosenField: TDBField read GetField;
    property Tag: integer read FTag write SetFilterTag;
    property OnDestroy: TNotifyEvent read FDestroying write FDestroying;
    property OnChangeData: TNotifyEvent read FChangingData write FChangingData;
    property Top: integer read FTop write SetFilterTop;
    property Height: integer read FHeight write SetFilterHeight;
    property Value: variant read GetValue;
    property Operation: string read GetOperation;
    constructor Create(AIndex: integer; AForm: TDBTableForm);
    destructor Destroy;
	end;

  { TDBTableForm }

  TDBTableForm = class(TForm)
    AddFilter: TButton;
    FNavigator: TDBNavigator;
    FDataSource: TDataSource;
    FDBGrid: TDBGrid;
    FSQLQuery: TSQLQuery;
    Filter: TSpeedButton;
		FFilterPanel: TScrollBox;
		FButtonPanel: TPanel;
	  Splitter: TSplitter;
    TableMenu: TMenuItem;
    CloseOtherTables: TMenuItem;
    TableMainMenu: TMainMenu;
    CloseTable: TMenuItem;
	  procedure AddFilterClick(Sender: TObject);
    procedure CloseOtherTablesClick(Sender: TObject);
    procedure CloseTableClick(Sender: TObject);
		procedure FilterClick(Sender: TObject);
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
    Filters: array of TQueryFilter;
    FieldOfColumn: TStringList;
    OrderDesc: boolean;
    FiltersOrder: array of integer;
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
  with FDBGrid do
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
  with FSQLQuery.SQL do
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
  with FSQLQuery.SQL do begin
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
          Append(Fields[i].TableRef.Name + '.' + Fields[i].FieldRef.Name + ' = ');
          Append(Name + '.' + Fields[i].Name);
        end;
  end;
end;

procedure TDBTableForm.FDBGridTitleClick(Column: TColumn);
var
  vField: TDBField;
begin
  vField := (FieldOfColumn.Objects[Column.Index] as TDBField);
  FSQLQuery.Close;
  SetSQLQuery;
  AddConditionsToQuery;
  FSQLQuery.SQL.Append('order by ' + vField.Owner.Name + '.' + vField.Name);
  if OrderDesc then
    FSQLQuery.SQL.Append('desc');
  OrderDesc := not OrderDesc;
  FSQLQuery.Open;
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
  with FSQLQuery do begin
    Transaction := ConTran.DBTransaction;
    Database := ConTran.DBConnection;
    FSQLQuery.Close;
    SetSQLQuery;
  end;

  with FDataSource do
    DataSet := FSQLQuery;

  FieldOfColumn := TStringList.Create;
  FieldOfColumn.Sorted := false;

  with FDBGrid do begin
    DataSource := FDataSource;
    AddColumnsToGrid(DBTables[Self.Tag]);
    OnTitleClick := @FDBGridTitleClick;
    OnColumnMoved := @DBGridColumnMoved;
  end;

  with FNavigator do begin
    DataSource := FDataSource;
    VisibleButtons := [nbFirst, nbNext, nbPrior, nbLast];
  end;

  FSQLQuery.Open;
end;

procedure TDBTableForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TDBTableForm.CloseTableClick(Sender: TObject);
begin
  Close;
end;

procedure TDBTableForm.FilterClick(Sender: TObject);
begin
  (Sender as TSpeedButton).Enabled := false;
  FSQLQuery.Close;
  SetSQLQuery;
  AddConditionsToQuery;
  FSQLQuery.Open;
end;

procedure TDBTableForm.AddConditionsToQuery;
var
  i, k: integer;
begin
  FSQLQuery.SQL.Append('where 1 = 1');

  with FSQLQuery do
    for i := 0 to Length(FiltersOrder) - 1 do
      if Assigned(Filters[FiltersOrder[i]]) and Assigned(Filters[FiltersOrder[i]].FConstantEditor)
      and (Filters[FiltersOrder[i]].FConstantEditor.Visible) then begin
        SQL.Append('and ' + Filters[FiltersOrder[i]].ChosenField.Owner.Name+'.'+Filters[FiltersOrder[i]].ChosenField.Name);
        SQL.Append(Filters[FiltersOrder[i]].Operation + ' :' + 'P' + IntToStr(FiltersOrder[i]));
      end else if Assigned(Filters[FiltersOrder[i]]) and Assigned(Filters[FiltersOrder[i]].FConstantEditor) then
        SQL.Append('or 1 = 1');

  k := 0;
  for i := 0 to Length(FiltersOrder) - 1 do
    if Assigned(Filters[FiltersOrder[i]]) and Assigned(Filters[FiltersOrder[i]].FConstantEditor) and
    (Filters[FiltersOrder[i]].FConstantEditor.Visible) then
      with FSQLQuery do begin
        Params[k].Value := Filters[FiltersOrder[i]].Value;
        inc(k);
      end;
end;

procedure TDBTableForm.CloseOtherTablesClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(DBTableForms) do
    if (i <> Tag) then
      DestroyTableForm(i);
end;

procedure TDBTableForm.DestroyFilterClick(Sender: TObject);
var
  vTag, i: integer;
begin
  vTag := (Sender as TQueryFilter).Tag;
  LocateFiltersOnDelete(vTag);
  Filters[vTag].Destroy;
  Filters[vTag] := nil;
  for i := High(Filters) downto 0 do
    if (Filters[i] = nil) then
      SetLength(Filters, Length(Filters) - 1)
    else
      break;
end;

procedure TDBTableForm.LocateFiltersOnDelete(ATag: integer);
var
  i: integer;
  found: boolean;
begin
  found := false;
  for i := 0 to Length(FiltersOrder) - 2 do begin
    if FiltersOrder[i] = ATag then
      found := true;
    if found then
      FiltersOrder[i] := FiltersOrder[i + 1];
	end;
  SetLength(FiltersOrder, Length(FiltersOrder) - 1);
	FFilterPanel.Height := FFilterPanel.Height - Filters[ATag].Height;
  for i := 0 to Length(Filters) - 1 do
    if Assigned(Filters[i]) and (i <> ATag) then
      if Filters[i].Top > Filters[ATag].Top then
        Filters[i].Top := Filters[i].Top - Filters[i].Height;
  if ClientToScreen(Point(0, FFilterPanel.Height + FButtonPanel.Height)).Y < Mouse.CursorPos.Y then
    FFilterPanel.Height := FFilterPanel.Height + Filters[ATag].Height;
end;

procedure TDBTableForm.LocateFiltersOnAdd(ATag, APosition: integer);
var
  i, k: integer;
begin
  if APosition = -1 then
    APosition := Length(FiltersOrder) - 1;
  SetLength(FiltersOrder, Length(FiltersOrder) + 1);
  for i := Length(FiltersOrder) - 1 downto APosition + 2 do
    FiltersOrder[i] := FiltersOrder[i - 1];
  FiltersOrder[APosition + 1] := ATag;
  k := 0;
  for i := 0 to Length(Filters) - 1 do
    if Assigned(Filters[i]) and (i <> ATag) then
      inc(k);
  Filters[ATag].Top := k * Filters[ATag].Height;
  if (Filters[ATag].Top + Filters[ATag].Height >= FFilterPanel.Height) and
    (Filters[ATag].Top <= FFilterPanel.Height) and
      (FFilterPanel.Height <= 4*(Height div 5)) then
        FFilterPanel.Height := FFilterPanel.Height + Filters[ATag].Height;
end;

constructor TQueryFilter.Create(AIndex: integer; AForm: TDBTableForm);
begin
  FOwner := AForm;
  FTag := AIndex;

  FDeleteFilter := TButton.Create(AForm.FFilterPanel);
  with FDeleteFilter do begin
    Parent := AForm.FFilterPanel;
    Height := 26;
    Width := Height;
    FHeight := Height;
    Caption := 'X';
    Left := 2;
    Tag := AIndex;
    OnClick := @DeleteFilterClick;
  end;

	FFieldsList := TComboBox.Create(AForm.FFilterPanel);
  with FFieldsList do begin
    Parent := AForm.FFilterPanel;
    Left := FDeleteFilter.Left + FDeleteFilter.Width + 1;
    AutoSize := false;
    Height := FDeleteFilter.Height;
    Style := csDropDownList;
    AddFieldsForChoose(DBTables[AForm.Tag]);
    AddItem('ИЛИ', nil);
    OnChange := @FieldChoose;
  end;
end;

procedure TQueryFilter.AddFieldsForChoose(aTable: TDBTable);
var
  i: integer;
begin
  with FFieldsList do
    with aTable do
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
  FDestroying(Self);
end;

procedure TDBTableForm.FilterDataChanged(Sender: TObject);
begin
  Filter.Enabled := true;
end;

procedure TDBTableForm.AddFilterClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Length(Filters) - 1 do
    if (Filters[i] = nil) then begin
      Filters[i] := TQueryFilter.Create(i, Self);
      LocateFiltersOnAdd(i, -1);
      Filters[i].OnDestroy := @DestroyFilterClick;
      Filters[i].OnChangeData := @FilterDataChanged;
      exit;
    end;

  SetLength(Filters, Length(Filters) + 1);
  Filters[High(Filters)] := TQueryFilter.Create(High(Filters), Self);
  LocateFiltersOnAdd(High(Filters), -1);
  Filters[High(Filters)].OnDestroy := @DestroyFilterClick;
  Filters[High(Filters)].OnChangeData := @FilterDataChanged;
end;

procedure TQueryFilter.FieldChoose(Sender: TObject);
var
  VSender: TComboBox;
  tempft: TFieldType;
  ro: TRelationalOperation;
begin
  VSender := (Sender as TComboBox);
  if Assigned(FConstantEditor) then begin
    FreeAndNil(FConstantEditor);
    FreeAndNil(FOperationsList);
	end;
  if Assigned(VSender.Items.Objects[VSender.ItemIndex]) then
    tempft := ((VSender.Items.Objects[VSender.ItemIndex]) as TDBField).FieldType
  else
    tempft := ftUnknown;

  FOperationsList := TComboBox.Create(FOwner.FFilterPanel);
  FOperationsList.Parent := FOwner.FFilterPanel;
  with FOperationsList do begin
    AutoSize := false;
    Top := FDeleteFilter.Top;
    Left := FFieldsList.Left + FFieldsList.Width + 1;
    Height := FFieldsList.Height;
    Style := csDropDownList;
    for ro := Low(TRelationalOperation) to High(TRelationalOperation) do
      if ro in AvailableOperations[tempft] then
        AddItem(Operations[ro].Caption, Operations[ro]);
    ItemIndex := 0;
    OnChange := @OperationChange;
	end;

  FConstantEditor := TypeOfEditor[tempft].Create(FOwner.FFilterPanel);
  FConstantEditor.Parent := FOwner.FFilterPanel;
  with FConstantEditor do begin
    AutoSize := false;
    Height := FFieldsList.Height;
    Width := FFieldsList.Width;;
    Top := FDeleteFilter.Top;
    Left := FOperationsList.Left + FOperationsList.Width + 1;
    if TypeOfEditor[tempft] = TSpinEdit then
      with (FConstantEditor as TSpinEdit) do begin
        MaxValue := High(Integer);
        MinValue := Low(Integer);
	  	end;
    OnChange := @EditChange;
	end;

  if tempft = ftUnknown then begin
    FConstantEditor.Visible := false;
    FOperationsList.Visible := false;
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

procedure TQueryFilter.SetFilterTop(Value: integer);
begin
  FDeleteFilter.Top := Value;
  FFieldsList.Top := Value;
  if Assigned(FConstantEditor) then begin
    FConstantEditor.Top := Value;
    FOperationsList.Top := Value;
	end;
  FTop := Value;
end;

procedure TQueryFilter.SetFilterHeight(Value: integer);
begin
  FDeleteFilter.Height := Value;
  FFieldsList.Height := Value;
  if Assigned(FConstantEditor) then begin
    FConstantEditor.Height := Value;
    FOperationsList.Height := Value;
	end;
	FHeight := Value;
end;

procedure TQueryFilter.SetFilterTag(Value: integer);
begin
  FDeleteFilter.Tag := value;
  FTag := value;
end;

function TQueryFilter.GetField: TDBField;
begin
  if Assigned(FFieldsList.Items.Objects[FFieldsList.ItemIndex] as TDBField) then
    Result := (FFieldsList.Items.Objects[FFieldsList.ItemIndex] as TDBField)
  else
    Result := nil;
end;

function TQueryFilter.GetOperation: string;
begin
  Result := (FOperationsList.Items.Objects[FOperationsList.ItemIndex] as TRelOperation).Code;
end;

function TQueryFilter.GetValue: Variant;
begin
  if ((FFieldsList.Items.Objects[FFieldsList.ItemIndex]) as TDBField).FieldType = ftInteger then
    Result := (FConstantEditor as TSpinEdit).Value
  else
    Result := FConstantEditor.Text;
end;

destructor TQueryFilter.Destroy;
begin
  FreeAndNil(FDeleteFilter);
  FreeAndNil(FFieldsList);
  if Assigned(FConstantEditor) then begin
    FreeAndNil(FConstantEditor);
    FreeAndNil(FOperationsList);
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
  Operations[roGreater] := TRelOperation.Create('>', ' > ');
  Operations[roContaining] := TRelOperation.Create('Содержит', ' containing ');
  Operations[roEqual] := TRelOperation.Create('=', ' = ');
  Operations[roInequal] := TRelOperation.Create('<>', ' <> ');
  Operations[roLess] := TRelOperation.Create('<', ' < ');
  Operations[roNotGreater] := TRelOperation.Create('<=', ' <= ');
  Operations[roNotLess] := TRelOperation.Create('>=', ' >= ');
  Operations[roStartsWith] := TRelOperation.Create('Начинается с', ' starts with ');

end.






