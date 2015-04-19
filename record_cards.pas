unit record_cards;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, metadata, StdCtrls, db, ExtCtrls;

type

  TCustomEditClass = class of TCustomEdit;

  TCellEdit = class
  private
    cbbValues: TComboBox;
    pnlCellEdit: TPanel;
    CellEditor: TCustomEdit;
    lbTitle: TLabel;
  public
    constructor Create(AField: TDBField; APos: integer; ACard: TForm); overload;
    constructor Create(ATableRef: TDBTable; AField: TDBField; APos: integer; ACard: TForm); overload;
	end;

  TRecordCard = class(TForm)
  private
    FCellEdits: array of TCellEdit;
  public
    constructor Create(ATable: TDBTable; ACaption: string);
  end;

var
  TypeOfEditor: array [Low(TFieldType)..High(TFieldType)] of TCustomEditClass;

implementation

{$R *.lfm}

constructor TRecordCard.Create(ATable: TDBTable; ACaption: string);
var
  i: integer;
begin
  inherited Create(Application);
  Caption := ACaption;

  for i := 0 to High(ATable.Fields) do begin
    SetLength(FCellEdits, Length(FCellEdits) + 1);
    if (ATable.Fields[i].TableRef = nil) then begin
      FCellEdits[High(FCellEdits)] := TCellEdit.Create(ATable.Fields[i], i, Self);
    end else begin
      FCellEdits[High(FCellEdits)] := TCellEdit.Create(ATable.Fields[i].TableRef, ATable.Fields[i], i, Self);
		end;
	end;

end;

constructor TCellEdit.Create(AField: TDBField; APos: integer; ACard: TForm);
begin
  pnlCellEdit := TPanel.Create(ACard);
  with pnlCellEdit do begin
    Parent := ACard;
    Height := 50;
    Align := alTop;
    Top := APos * Height;
  end;

end;

constructor TCellEdit.Create(ATableRef: TDBTable; AField: TDBField; APos: integer; ACard: TForm);
begin

end;

end.
















