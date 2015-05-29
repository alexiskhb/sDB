unit DBMain;

{$mode objfpc}{$H+}

interface

uses
  connection_transaction, Classes, SysUtils, sqldb, DB, IBConnection, FileUtil,
  SynHighlighterSQL, SynEdit, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls, ExtCtrls, Menus, Buttons, DBCtrls, FormConnect,
  metadata, listview, aboutsdb, time_table, record_cards, query_filter;

type

  { TMainForm }

  TMainForm = class(TForm)
    DataSourceComponent: TDataSource;
    DBGrid: TDBGrid;
    ControlPanel: TPanel;
    DBQuery: TSQLQuery;
    MainMenu: TMainMenu;
    EntryField: TMemo;
    MenuTables: TMenuItem;
    MenuOpenSQL: TMenuItem;
    MenuDatabase: TMenuItem;
    MenuConnect: TMenuItem;
    MenuDisconnect: TMenuItem;
    MenuHelp: TMenuItem;
    MenuAbout: TMenuItem;
    MenuExecSQL: TMenuItem;
    MenuLists: TMenuItem;
    MenuQuit: TMenuItem;
    MenuStatements: TMenuItem;
    EntryGridSplitter: TSplitter;
    procedure ExecuteStatementsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuConnectClick(Sender: TObject);
    procedure MenuDisconnectClick(Sender: TObject);
    procedure MenuExecSQLClick(Sender: TObject);
    procedure MenuOpenSQLClick(Sender: TObject);
    procedure ExecuteEntryFieldStatements;
    procedure MenuQuitClick(Sender: TObject);
    procedure TryToConnectDB;
    procedure ShowDBTable(Sender: TObject);
    procedure ShowTimeTable(Sender: TObject);
    procedure RecordCardOkClick(Sender: TObject);
    procedure TableFiltersPopup(Sender: TObject);
    procedure TableFiltersPopupClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ExecuteStatementsClick(Sender: TObject);
begin
  ExecuteEntryFieldStatements;
  ActiveControl := EntryField;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
  miLists: array of TMenuItem;
  miTables: array of TMenuItem;
begin
  SetLength(miLists, Length(DBTables));
  SetLength(miTables, Length(DBTables));
  for i := Low(miLists) to High(miLists) do begin
    miLists[i] := TMenuItem.Create(MainMenu);
    miTables[i] := TMenuItem.Create(MainMenu);
    with miLists[i] do begin
      Caption := DBTables[i].Caption;
      Tag := i;
      OnClick := @ShowDBTable;
    end;
    with miTables[i] do begin
      Caption := DBTables[i].Caption;
      Tag := i;
      OnClick := @ShowTimeTable;
    end;
  end;
  MainMenu.Items[0].Add(miLists);
  MainMenu.Items[1].Add(miTables);

  CardsManager.OnRequestRefreshTables := @RecordCardOkClick;
end;

procedure TMainForm.MenuAboutClick(Sender: TObject);
begin
  AboutProg.ShowModal;
end;

procedure TMainForm.MenuConnectClick(Sender: TObject);
begin
  TryToConnectDB;
end;

procedure TMainForm.MenuDisconnectClick(Sender: TObject);
begin
  with ConTran.DBConnection do begin
    Connected := False;
    Password := '';
  end;
end;

procedure TMainForm.MenuExecSQLClick(Sender: TObject);
begin
  DBQuery.Close;
  DBQuery.SQL.Text := EntryField.Lines.Text;
  DBQuery.ExecSQL;
end;

procedure TMainForm.ShowDBTable(Sender: TObject);
var
  MI: TMenuItem;
begin
  MI := (Sender as TMenuItem);
  if TDBTableForm.FormExists(MI.Tag) then
    TDBTableForm.FormSetFocus(MI.Tag)
  else begin
    DBTableForms[MI.Tag] := TDBTableForm.Create(DBTables[MI.Tag]);
    DBTableForms[MI.Tag].Tag := MI.Tag;
    DBTableForms[MI.Tag].OnShowAsTableClick := @ShowTimeTable;
    DBTableForms[MI.Tag].Show;
  end;
end;

procedure TMainForm.ShowTimeTable(Sender: TObject);
var
  MI: TMenuItem;
begin
  MI := (Sender as TMenuItem);
  if TTimeTable.FormExists(MI.Tag) then
    TTimeTable.FormSetFocus(MI.Tag)
  else begin
    TimeTables[MI.Tag] := TTimeTable.Create(DBTables[MI.Tag]);
    TimeTables[MI.Tag].Tag := MI.Tag;
    TimeTables[MI.Tag].OnShowAsListClick := @ShowDBTable;
    TimeTables[MI.Tag].OnFilterPopup := @TableFiltersPopup;
    TimeTables[MI.Tag].Show;
  end;
end;

procedure TMainForm.TryToConnectDB;
var
  i: integer;
begin
  if ConnectForm.ShowModal = mrOk then
    with ConTran.DBConnection do begin
      for i := 0 to High(DBTables) do
        TDBTableForm.DestroyTableForm(i);
      Connected := False;
      DatabaseName := ConnectForm.DBPath.Text;
      UserName := ConnectForm.DBUserName.Text;
      Password := ConnectForm.DBPassword.Text;
      Connected := True;
    end;
end;

procedure TMainForm.MenuOpenSQLClick(Sender: TObject);
begin
  ExecuteEntryFieldStatements;
end;

procedure TMainForm.ExecuteEntryFieldStatements;
begin
  DBQuery.Close;
  DBQuery.SQL.Text := EntryField.Lines.Text;
  DBQuery.Open;
end;

procedure TMainForm.MenuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.RecordCardOkClick(Sender: TObject);
begin
  TDBTableForm.RefreshLists;
  TTimeTable.RefreshTables;
end;

procedure TMainForm.TableFiltersPopup(Sender: TObject);
var
  i: integer;
  PopMenu: TPopupMenu;
  MenuItem: TMenuItem;
  TimeTable: TTimeTable;
begin
  PopMenu := Sender as TPopupMenu;
  TimeTable := PopMenu.Owner as TTimeTable;
  PopMenu.Items.Clear;
  for i := 0 to Length(DBTables) - 1 do begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := DBTables[i].Caption + ' (список)';
    MenuItem.Visible := TDBTableForm.FormExists(i);
    MenuItem.OnClick := @TableFiltersPopupClick;
    MenuItem.Tag := i;
    PopMenu.Items.Add(MenuItem);
  end;
  for i := 0 to Length(DBTables) - 1 do begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := DBTables[i].Caption + ' (таблица)';
    MenuItem.Visible := TTimeTable.FormExists(i) and (TimeTable <> TimeTables[i]);
    MenuItem.OnClick := @TableFiltersPopupClick;
    MenuItem.Tag := i + Length(DBTables);
    PopMenu.Items.Add(MenuItem);
  end;
end;

procedure TMainForm.TableFiltersPopupClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  FiltersFrom: TQueryFilterDynArray;
  TimeTable: TTimeTable;
begin
  MenuItem := Sender as TMenuItem;
  if MenuItem.Tag < Length(DBTables) then
    FiltersFrom := DBTableForms[MenuItem.Tag].Filters
  else
    FiltersFrom := TimeTables[MenuItem.Tag mod Length(DBTables)].Filters;
  TimeTable := (MenuItem.Parent.Owner as TPopupMenu).Owner as TTimeTable;
  TQueryFilter.CopyFilters(
    TimeTable.Table, FiltersFrom, TimeTable.Filters, TimeTable.sbxFilters);
  TimeTable.pmCopyFiltersFromClick(Sender);
end;

end.






