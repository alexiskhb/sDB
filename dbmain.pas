unit DBMain;

{$mode objfpc}{$H+}

interface

uses
  connection_transaction, Classes, SysUtils, sqldb, sqldblib, db, IBConnection, FileUtil,
  SynHighlighterSQL, SynMemo, SynEdit, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls, ExtCtrls, Menus, Buttons, DbCtrls, FormConnect, types,
  metadata, listview, aboutsdb;

type

  { TMainForm }

  TMainForm = class(TForm)
    ExecuteStatements: TBitBtn;
    DataSourceComponent: TDataSource;
    DBGrid: TDBGrid;
    ControlPanel: TPanel;
    DBQuery: TSQLQuery;
    MainMenu: TMainMenu;
    EntryField: TMemo;
    MenuExecuteStatements: TMenuItem;
    MenuDatabase: TMenuItem;
    MenuConnect: TMenuItem;
    MenuDisconnect: TMenuItem;
    MenuCatalog: TMenuItem;
    MenuHelp: TMenuItem;
    MenuAbout: TMenuItem;
    MenuQuit: TMenuItem;
    MenuStatements: TMenuItem;
    EntryGridSplitter: TSplitter;
    procedure ExecuteStatementsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuConnectClick(Sender: TObject);
    procedure MenuDisconnectClick(Sender: TObject);
    procedure MenuExecuteStatementsClick(Sender: TObject);
    procedure ExecuteEntryFieldStatements;
    procedure MenuQuitClick(Sender: TObject);
    procedure TryToConnectDB;
    procedure ShowDBTable(Sender: TObject);
  public
    MenuItemsTables: array of TMenuItem;
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
begin
  SetLength(MenuItemsTables, Length(DBTables));
  for i := Low(MenuItemsTables) to High(MenuItemsTables) do begin
    MenuItemsTables[i] := TMenuItem.Create(MainMenu);
    with MenuItemsTables[i] do begin
      Name := DBTables[i].Name;
      Caption := DBTables[i].Caption;
      Tag := i;
      OnClick := @ShowDBTable;
    end;
  end;
  MainMenu.Items[0].Items[0].Add(MenuItemsTables);
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
    Connected := false;
    Password := '';
  end;
end;

procedure TMainForm.ShowDBTable(Sender: TObject);
var
  MI: TMenuItem;
begin
  MI := (Sender as TMenuItem);
  if TDBTableForm.FormExists(MI.Tag) then
    TDBTableForm.FormSetFocus(MI.Tag)
  else
    TDBTableForm.CreateTableForm(MI.Tag, MI.Caption);
end;

procedure TMainForm.TryToConnectDB;
var
  i: integer;
begin
  if ConnectForm.ShowModal = mrOk then
  with ConTran.DBConnection do begin
    for i := 0 to High(DBTables) do
      TDBTableForm.DestroyTableForm(i);
    Connected := false;
    DatabaseName := ConnectForm.DBPath.Text;
    UserName := ConnectForm.DBUserName.Text;
    Password := ConnectForm.DBPassword.Text;
    Connected := true;
  end;
end;

procedure TMainForm.MenuExecuteStatementsClick(Sender: TObject);
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

end.

