unit listview;

{$mode objfpc}{$H+}

interface

uses
  connection_transaction, Classes, lcltype, SysUtils, Forms, Menus, DBCtrls, DB, DBGrids,
  ExtCtrls, sqldb, Dialogs, Controls, StdCtrls, metadata;

type

  { TDBTableForm }

  TDBTableForm = class(TForm)
    FNavigator: TDBNavigator;
    FDataSource: TDataSource;
    FControlPanel: TPanel;
    FDBGrid: TDBGrid;
    FSQLQuery: TSQLQuery;
    TableMenu: TMenuItem;
    CloseOtherTables: TMenuItem;
    TableMainMenu: TMainMenu;
    CloseTable: TMenuItem;
    procedure CloseOtherTablesClick(Sender: TObject);
    procedure CloseTableClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
    procedure SetSQLQuery(formsender: TWinControl);
    procedure AddColumnsToQuery(aTable: TDBTable);
    procedure AddColumnsToGrid(aTable: TDBTable; var aDuplicates: TFieldNameIndexArray;
      var k: integer);
    function AddFieldName(aFieldName: TFieldName;
      var aDuplicates: TFieldNameIndexArray): string;
    class procedure CreateTableForm(aTag: integer; aCaption: string);
    class procedure DestroyTableForm(aTag: integer);
    class procedure FormSetFocus(aTag: integer);
    class function FormExists(aTag: integer): boolean;
  end;

  TDBTableFormDynArray = array of TDBTableForm;

implementation

{$R *.lfm}

var
  DBTableForms: TDBTableFormDynArray;

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

procedure TDBTableForm.AddColumnsToGrid(aTable: TDBTable;
  var aDuplicates: TFieldNameIndexArray; var k: integer);
var
  i: integer;
begin
  with FDBGrid do
    with aTable do
      for i := Low(Fields) to High(Fields) do begin
        if Fields[i].Visible then begin
          Columns.Add.FieldName := AddFieldName(Fields[i].EnumName, aDuplicates);
          Columns[k].Title.Caption := Fields[i].Caption;
          Columns[k].Width := Fields[i].Width;
          Columns[k].Visible := Fields[i].Visible;
          Inc(k);
        end;
        if Fields[i].TableRefEnum <> selfreft then
          AddColumnsToGrid(DBTables[NumByName(Fields[i].TableRefEnum)], aDuplicates, k);
      end;
end;

function TDBTableForm.AddFieldName(aFieldName: TFieldName;
  var aDuplicates: TFieldNameIndexArray): string;
begin
  if aDuplicates[aFieldName] = 0 then
    Result := EnumToString(aFieldName)
  else
    Result := EnumToString(aFieldName) + '_' + IntToStr(aDuplicates[aFieldName]);
  Inc(aDuplicates[aFieldName]);
end;

procedure TDBTableForm.AddColumnsToQuery(aTable: TDBTable);
var
  i: integer;
begin
  with FSQLQuery.SQL do
    with aTable do
      for i := Low(Fields) to High(Fields) do begin
        if Fields[i].Visible then begin
          Append(Name + '.' + Fields[i].Name);
          Append(',');
        end;
        if Fields[i].TableRefEnum <> selfreft then
          AddColumnsToQuery(DBTables[NumByName(Fields[i].TableRefEnum)]);
      end;
end;

procedure TDBTableForm.SetSQLQuery(formsender: TWinControl);
var
  i: integer;
begin
  with FSQLQuery.SQL do begin
    Append('select');
    AddColumnsToQuery(DBTables[formsender.Tag]);
    Delete(Count - 1);
    Append('from');
    Append(DBTables[formsender.Tag].Name + ' ');
    with DBTables[formsender.Tag] do
      for i := Low(Fields) to High(Fields) do
        if Fields[i].TableRefEnum <> selfreft then begin
          Append('join ' + Fields[i].TableRefStr + ' on ');
          Append(Fields[i].TableRefStr + '.' + Fields[i].FieldRefStr + ' = ');
          Append(Name + '.' + Fields[i].Name);
        end;
  end;
end;

procedure TDBTableForm.FormDestroy(Sender: TObject);
begin
  DBTableForms[(Sender as TDBTableForm).Tag] := nil;
end;

procedure TDBTableForm.FormShow(Sender: TObject);
var
  formsender: TWinControl;
  k: integer;
  duplicatenames: TFieldNameIndexArray = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
begin
  formsender := (Sender as TDBTableForm);

  with FSQLQuery do begin
    Transaction := ConTran.DBTransaction;
    Database := ConTran.DBConnection;
    FSQLQuery.Close;
    SetSQLQuery(formsender);
  end;

  with FDataSource do
    DataSet := FSQLQuery;

  with FDBGrid do begin
    DataSource := FDataSource;
    k := 0;
    AddColumnsToGrid(DBTables[formsender.Tag], duplicatenames, k);
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

initialization

  SetLength(DBTableForms, Length(DBTables));

end.



