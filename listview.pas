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
		Splitter: TSplitter;
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
    procedure AddColumnsToGrid(aTable: TDBTable);
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

procedure TDBTableForm.AddColumnsToGrid(aTable: TDBTable);
var
  i: integer;
begin
  with FDBGrid do
    with aTable do
      for i := 0 to High(Fields) do begin
        if Fields[i].Visible then begin
          Columns.Add.FieldName := aTable.Name + Fields[i].Name ;
          Columns[Columns.Count - 1].Title.Caption := Fields[i].Caption;
          Columns[Columns.Count - 1].Width := Fields[i].Width;
          Columns[Columns.Count - 1].Visible := Fields[i].Visible;
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
        if Assigned(Fields[i].TableRef) then begin
          Append('join ' + Fields[i].TableRef.Name + ' on ');
          Append(Fields[i].TableRef.Name + '.' + Fields[i].FieldRef.Name + ' = ');
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
    AddColumnsToGrid(DBTables[formsender.Tag]);
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



