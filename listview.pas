unit listview;

{$mode objfpc}{$H+}

interface

uses
  connection_transaction, Classes, lcltype, SysUtils, Forms, menus, DBCtrls, db, DBGrids,
  ExtCtrls, sqldb, Dialogs, Controls, metadata;

type

  TDBTableForm = class(TForm)
  private
    FControlPanel: TPanel;
    FNavigator: TDBNavigator;
    FSQLQuery: TSQLQuery;
    FDataSource: TDataSource;
    FDBGrid: TDBGrid;
  public
    TableMainMenu: TMainMenu;
    TableMenu: TMenuItem;
    CloseTable: TMenuItem;
    CloseOtherTables: TMenuItem;
    procedure CloseTableForm(Sender: TObject);
    procedure CloseOtherTableForms(Sender: TObject);
    procedure CreateTableMenu(formsender: TWinControl);
    procedure FormCreate(Sender: TObject);
    procedure FormOnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormOnDestroy(Sender: TObject);
    procedure SetSQLQuery(formsender: TWinControl);
    procedure AddColumnsToQuery(aTable: TDBTable);
    procedure AddColumnsToGrid(aTable: TDBTable; var aDuplicates: TFieldNameIndexArray; var k: integer);
    function AddFieldName(aFieldName: TFieldName; var aDuplicates: TFieldNameIndexArray): string;
    constructor CreateNew(aTag: integer; FCaption: string);
    class procedure CreateTableForm(aTag: integer; FCaption: string);
    class procedure DestroyTableForm(aTag: integer);
    class procedure FormSetFocus(aTag: integer);
    class function FormExists(aTag: integer): boolean;
  end;

  TDBTableFormDynArray = array of TDBTableForm;

implementation

var
  DBTableForms: TDBTableFormDynArray;

class function TDBTableForm.FormExists(aTag: integer): boolean;
begin
  Result := DBTableForms[aTag] <> nil;
end;

class procedure TDBTableForm.CreateTableForm(aTag: integer; FCaption: string);
begin
  DBTableForms[aTag] := TDBTableForm.CreateNew(aTag, FCaption);
  DBTableForms[aTag].Tag := aTag;
  DBTableForms[aTag].Show;
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

constructor TDBTableForm.CreateNew(aTag: integer; FCaption: string);
begin
  inherited CreateNew(Application);
  Position := poScreenCenter;
  Caption := FCaption;
  Width := 400;
  Height := 400;
  Tag := aTag;
  OnCreate := @FormCreate;
  OnClose := @FormOnClose;
  OnDestroy := @FormOnDestroy;
end;

procedure TDBTableForm.FormCreate(Sender: TObject);
var
  formsender: TWinControl;
  i, j, k: integer;
  duplicatenames: TFieldNameIndexArray;
begin
  formsender := (Sender as TDBTableForm);

  FControlPanel := TPanel.Create(formsender);
  with FControlPanel do begin
    Parent := formsender;
    Height := 30;
    Align := alTop;
  end;

  FDBGrid := TDBGrid.Create(formsender);
  FSQLQuery := TSQLQuery.Create(formsender);
  with FSQLQuery do begin
    Transaction := ConTran.DBTransaction;
    Database := ConTran.DBConnection;
    Active := false;
    SetSQLQuery(formsender);
  end;

  FDataSource := TDataSource.Create(formsender);
  with FDataSource do begin
    DataSet := FSQLQuery;
  end;

  with FDBGrid do begin
    Parent := formsender;
    Align := alClient;
    DataSource := FDataSource;
    k := 0;
    AddColumnsToGrid(DBTables[formsender.Tag], duplicatenames, k);
  end;

  FNavigator := TDBNavigator.Create(FControlPanel);
  with FNavigator do begin
    Parent := FControlPanel;
    DataSource := FDataSource;
    Left := 2;
    Top := 2;
    VisibleButtons := [nbFirst, nbNext, nbPrior, nbLast];
  end;

  CreateTableMenu(formsender);

  FSQLQuery.Active := true;
end;

procedure TDBTableForm.AddColumnsToGrid(aTable: TDBTable; var aDuplicates: TFieldNameIndexArray;
  var k: integer);
var
  i: integer;
begin
  with FDBGrid do
	  with aTable do begin
	    for i := Low(Fields) to High(Fields) do begin
	      if Fields[i].Visible then begin
          Columns.Add.FieldName := AddFieldName(Fields[i].EnumName, aDuplicates);
          Columns[k].Title.Caption := Fields[i].Caption;
          Columns[k].Width := Fields[i].Width;
          Columns[k].Visible := Fields[i].Visible;
          inc(k);
				end;
	      if Fields[i].TableRefEnum <> selfreft then
	        AddColumnsToGrid(DBTables[NumByName(Fields[i].TableRefEnum)], aDuplicates, k);
		  end;
	  end;
end;

function TDBTableForm.AddFieldName(aFieldName: TFieldName; var aDuplicates: TFieldNameIndexArray): string;
begin
  if aDuplicates[aFieldName] = 0 then
    Result := EnumToString(aFieldName)
	else
    Result := EnumToString(aFieldName) + '_' + IntToStr(aDuplicates[aFieldName]);
  inc(aDuplicates[aFieldName]);
end;

procedure TDBTableForm.AddColumnsToQuery(aTable: TDBTable);
var
  i: integer;
begin
  with FSQLQuery.SQL do
	  with aTable do begin
	    for i := Low(Fields) to High(Fields) do begin
	      if Fields[i].Visible then begin
	        Append(Name + '.' + Fields[i].Name);
	        Append(',');
	      end;
	      if Fields[i].TableRefEnum <> selfreft then
	        AddColumnsToQuery(DBTables[NumByName(Fields[i].TableRefEnum)]);
		  end;
	  end;
end;

procedure TDBTableForm.SetSQLQuery(formsender: TWinControl);
var
  i, j: integer;
begin
  with FSQLQuery.SQL do begin
    Append('select');
    AddColumnsToQuery(DBTables[formsender.Tag]);
    Delete(Count - 1);
    Append('from');
    Append(DBTables[formsender.Tag].Name + ' ');
    with DBTables[formsender.Tag] do begin
      for i := Low(Fields) to High(Fields) do
        if Fields[i].TableRefEnum <> selfreft then begin
          Append('join ' + Fields[i].TableRefStr + ' on ');
          Append(Fields[i].TableRefStr + '.' + Fields[i].FieldRefStr  + ' = ');
          Append(Name + '.' + Fields[i].Name);
	end;
    end;
  end;
end;

procedure TDBTableForm.CreateTableMenu(formsender: TWinControl);
begin
  TableMainMenu := TMainMenu.Create(formsender);
  TableMenu := TMenuItem.Create(TableMainMenu);
  TableMenu.Caption := 'Таблицы';
  TableMainMenu.Items.Insert(0, TableMenu);

  CloseTable := TMenuItem.Create(TableMenu);
  TableMainMenu.Items[0].Add(CloseTable);
  with CloseTable do begin
    Caption := 'Закрыть';
    ShortCut := KeyToShortCut(87, [ssCtrl]);
    OnClick := @CloseTableForm;
  end;

  CloseOtherTables := TMenuItem.Create(TableMenu);
  TableMainMenu.Items[0].Add(CloseOtherTables);
  with CloseOtherTables do begin
    Caption := 'Закрыть все другие';
    ShortCut := KeyToShortCut(87, [ssCtrl, ssShift]);
    OnClick := @CloseOtherTableForms;
  end;
end;

procedure TDBTableForm.CloseTableForm(Sender: TObject);
begin
  Close;
end;

procedure TDBTableForm.CloseOtherTableForms(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(DBTableForms) do
    if (i <> Tag) then
      DestroyTableForm(i);
end;

procedure TDBTableForm.FormOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TDBTableForm.FormOnDestroy(Sender: TObject);
begin
  DBTableForms[(Sender as TDBTableForm).Tag] := nil;
end;

initialization

  SetLength(DBTableForms, Length(DBTables));

end.

