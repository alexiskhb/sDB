unit time_table;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
	StdCtrls, CheckLst, Grids, Buttons, metadata;

type

	{ TTimeTable }

  TTimeTable = class(TForm)
	 btnApply: TBitBtn;
	  cbbHorz: TComboBox;
 	  cbbVert: TComboBox;
	  clbVisibleFields: TCheckListBox;
	  pnlControlsRight: TPanel;
	  pnlContols: TPanel;
		sgTable: TStringGrid;
		Splitter: TSplitter;
		CheckSplitter: TSplitter;
		procedure btnApplyClick(Sender: TObject);
  procedure cbbHorzChange(Sender: TObject);
   procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure AddFieldsToLists(ATable: TDBTable);
  private
    FTable: TDBTable;
  public
    constructor Create(ATable: TDBTable);
  end;

implementation

{$R *.lfm}

{ TTimeTable }

constructor TTimeTable.Create(ATable: TDBTable);
begin
  inherited Create(Application);
  FTable := ATable;
  Caption := ATable.Caption;
  AddFieldsToLists(ATable);


end;

procedure TTimeTable.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TTimeTable.cbbHorzChange(Sender: TObject);
begin
  if (cbbHorz.ItemIndex <> -1) and (cbbVert.ItemIndex <> -1) then
    btnApply.Enabled := true;
end;

procedure TTimeTable.btnApplyClick(Sender: TObject);
begin

end;

procedure TTimeTable.AddFieldsToLists(ATable: TDBTable);
var
  i: integer;
begin
  with ATable do
    for i := 0 to High(Fields) do begin
      if Fields[i].Visible then begin
        cbbHorz.AddItem(Fields[i].Caption, Fields[i]);
        cbbVert.AddItem(Fields[i].Caption, Fields[i]);
        clbVisibleFields.AddItem(Fields[i].Caption, Fields[i]);
        clbVisibleFields.Checked[clbVisibleFields.Count - 1] := true;
      end;
      if Assigned(Fields[i].TableRef) then
        AddFieldsToLists(Fields[i].TableRef);
    end;
end;

end.

