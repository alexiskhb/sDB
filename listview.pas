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

  TQueryFilter = class
  private
    FTag: integer;
    FDestroying: TNotifyEvent;
    FConstant: TCustomEdit;
    FFieldChoise: TComboBox;
    FOperationChoise: TComboBox;
    FDeleteFilter: TButton;
    FOwner: TDBTableForm;
    FHeight: integer;
    FTop: integer;
    procedure SetFilterTop(Value: integer);
    procedure SetFilterHeight(Value: integer);
    procedure SetFilterTag(Value: integer);
  public
    procedure FieldChoose(Sender: TObject);
    procedure DeleteFilterClick(Sender: TObject);
    procedure AddFieldsForChoose(aTable: TDBTable);
    property Tag: integer read FTag write SetFilterTag;
    property OnDestroy: TNotifyEvent read FDestroying write FDestroying;
    property Top: integer read FTop write SetFilterTop;
    property Height: integer read FHeight write SetFilterHeight;
    constructor Create(aIndex: integer; aForm: TDBTableForm);
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
    procedure SetSQLQuery;
    procedure AddColumnsToQuery(aTable: TDBTable);
    procedure AddColumnsToGrid(aTable: TDBTable);
    procedure DestroyFilterClick(Sender: TObject);
    procedure FDBGridTitleClick(Column: TColumn);
    procedure DBGridColumnMoved(Sender: TObject; FromIndex,
		ToIndex: Integer);
    class procedure CreateTableForm(aTag: integer; aCaption: string);
    class procedure DestroyTableForm(aTag: integer);
    class procedure FormSetFocus(aTag: integer);
    class function FormExists(aTag: integer): boolean;
  private
    Filters: array of TQueryFilter;
    FieldOfColumn: TStringList;
    OrderDesc: boolean;
    procedure LocateFiltersOnDelete(aTag: integer);
    procedure LocateFiltersOnAdd(aTag: integer);
  end;

  TDBTableFormDynArray = array of TDBTableForm;

implementation

{$R *.lfm}

var
  DBTableForms: TDBTableFormDynArray;
  EditField: array [ftUnknown..ftWideMemo] of TCustomEditClass;
  Operations: array [ftUnknown..ftWideMemo] of set of TRelationalOperation;
  OperCaptions: array [roGreater..roContaining] of string =
    ('>', '<', '>=', '<=', '=', '<>', 'Начинается с', 'Содержит');

class function TDBTableForm.FormExists(aTag: integer): boolean;
begin
  Result := DBTableForms[aTag] <> nil;
end;

class procedure TDBTableForm.CreateTableForm(aTag: integer; aCaption: string);
begin
  Application.CreateForm(TDBTableForm, DBTableForms[aTag]);
  with DBTableForms[aTag] do begin
    Caption := aCaption;
    Tag := aTag;
    Show;
  end;
end;

class procedure TDBTableForm.DestroyTableForm(aTag: integer);
begin
  if FormExists(aTag) then
    DBTableForms[aTag].Close;
end;

class procedure TDBTableForm.FormSetFocus(aTag: integer);
begin
  if FormExists(aTag) then
    DBTableForms[aTag].SetFocus;
end;

procedure TDBTableForm.AddColumnsToGrid(aTable: TDBTable);
var
  i: integer;
begin
  with FDBGrid do
    with aTable do
      for i := 0 to High(Fields) do begin
        if Fields[i].Visible then begin
          Columns.Add.FieldName := aTable.Name + Fields[i].Name;
          Columns[Columns.Count - 1].Title.Caption := Fields[i].Caption;
          Columns[Columns.Count - 1].Width := Fields[i].Width;
          Columns[Columns.Count - 1].Visible := Fields[i].Visible;
          FieldOfColumn.AddObject(Columns[Columns.Count - 1].FieldName, Fields[i]);
        end;
        if Assigned(Fields[i].TableRef) then
          AddColumnsToGrid(Fields[i].TableRef);
      end;
end;

procedure TDBTableForm.AddColumnsToQuery(aTable: TDBTable);
var
  i: integer;
begin
  with FSQLQuery.SQL do
    with aTable do
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

procedure TDBTableForm.LocateFiltersOnDelete(aTag: integer);
var
  i: integer;
begin
  FFilterPanel.Height := FFilterPanel.Height - Filters[aTag].Height;
  for i := 0 to Length(Filters) - 1 do
    if Assigned(Filters[i]) and (i <> aTag) then
      if Filters[i].Top > Filters[aTag].Top then
        Filters[i].Top := Filters[i].Top - Filters[i].Height;
  if ClientToScreen(Point(0, FFilterPanel.Height + FButtonPanel.Height)).Y < Mouse.CursorPos.Y then
    FFilterPanel.Height := FFilterPanel.Height + Filters[aTag].Height;
end;

procedure TDBTableForm.LocateFiltersOnAdd(aTag: integer);
var
  i, k: integer;
begin
  k := 0;
  for i := 0 to Length(Filters) - 1 do
    if Assigned(Filters[i]) and (i <> aTag) then
      inc(k);
  Filters[aTag].Top := k * Filters[aTag].Height;
  if (Filters[aTag].Top + Filters[aTag].Height >= FFilterPanel.Height) and
    (Filters[aTag].Top <= FFilterPanel.Height) and
      (FFilterPanel.Height <= 4*(Height div 5)) then
    FFilterPanel.Height := FFilterPanel.Height + Filters[aTag].Height;
end;

constructor TQueryFilter.Create(aIndex: integer; aForm: TDBTableForm);
begin
  FOwner := aForm;
  FTag := aIndex;

  FDeleteFilter := TButton.Create(aForm.FFilterPanel);
  with FDeleteFilter do begin
    Parent := aForm.FFilterPanel;
    Height := 26;
    Width := Height;
    FHeight := Height;
    Caption := 'X';
    Left := 2;
    Tag := aIndex;
    OnClick := @DeleteFilterClick;
  end;

	FFieldChoise := TComboBox.Create(aForm.FFilterPanel);
  with FFieldChoise do begin
    Parent := aForm.FFilterPanel;
    Left := FDeleteFilter.Left + FDeleteFilter.Width + 1;
    AutoSize := false;
    Height := FDeleteFilter.Height;
    Style := csDropDownList;
    AddFieldsForChoose(DBTables[aForm.Tag]);
    OnChange := @FieldChoose;
  end;
end;

procedure TQueryFilter.AddFieldsForChoose(aTable: TDBTable);
var
  i: integer;
begin
  with FFieldChoise do
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
  FDestroying(Self);
end;

procedure TDBTableForm.AddFilterClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Length(Filters) - 1 do
    if (Filters[i] = nil) then begin
      Filters[i] := TQueryFilter.Create(i, Self);
      LocateFiltersOnAdd(i);
      Filters[i].OnDestroy := @DestroyFilterClick;
      exit();
    end;

  SetLength(Filters, Length(Filters) + 1);
  Filters[High(Filters)] := TQueryFilter.Create(High(Filters), Self);
  LocateFiltersOnAdd(High(Filters));
  Filters[High(Filters)].OnDestroy := @DestroyFilterClick;
end;

procedure TQueryFilter.FieldChoose(Sender: TObject);
var
  vSender: TComboBox;
  tempft: TFieldType;
  ro: TRelationalOperation;
begin
  vSender := (Sender as TComboBox);
  if Assigned(FConstant) then begin
    FreeAndNil(FConstant);
    FreeAndNil(FOperationChoise);
	end;
  tempft := ((vSender.Items.Objects[vSender.ItemIndex]) as TDBField).FieldType;

  FOperationChoise := TComboBox.Create(FOwner.FFilterPanel);
  FOperationChoise.Parent := FOwner.FFilterPanel;
  with FOperationChoise do begin
    AutoSize := false;
    Top := FDeleteFilter.Top;
    Left := FFieldChoise.Left + FFieldChoise.Width + 1;
    Height := FFieldChoise.Height;
    Style := csDropDownList;
    for ro := Low(TRelationalOperation) to High(TRelationalOperation) do
      if ro in Operations[tempft] then
        AddItem(OperCaptions[ro], Self);
	end;

  FConstant := EditField[tempft].Create(FOwner.FFilterPanel);
  FConstant.Parent := FOwner.FFilterPanel;
  with FConstant do begin
    Top := FDeleteFilter.Top;
    Left := FOperationChoise.Left + FOperationChoise.Width + 1;
	end;

end;

procedure TQueryFilter.SetFilterTop(Value: integer);
begin
  FDeleteFilter.Top := Value;
  FFieldChoise.Top := Value;
  if Assigned(FConstant) then begin
    FConstant.Top := Value;
    FOperationChoise.Top := Value;
	end;
  FTop := Value;
end;

procedure TQueryFilter.SetFilterHeight(Value: integer);
begin
  FDeleteFilter.Height := Value;
  FFieldChoise.Height := Value;
  if Assigned(FConstant) then begin
    FConstant.Height := Value;
    FOperationChoise.Height := Value;
	end;
	FHeight := Value;
end;

procedure TQueryFilter.SetFilterTag(Value: integer);
begin
  FDeleteFilter.Tag := value;
  FTag := value;
end;

destructor TQueryFilter.Destroy;
begin
  FreeAndNil(FDeleteFilter);
  FreeAndNil(FFieldChoise);
  if Assigned(FConstant) then begin
    FreeAndNil(FConstant);
    FreeAndNil(FOperationChoise);
	end;
end;

initialization

  SetLength(DBTableForms, Length(DBTables));
  EditField[ftInteger] := TSpinEdit;
  EditField[ftString] := TEdit;
  Operations[ftInteger] := [roEqual, roInequal, roGreater, roNotGreater, roNotLess, roLess];
  Operations[ftString] := [roEqual, roInequal, roContaining, roStartsWith];

end.



