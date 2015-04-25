unit record_cards;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, metadata,
  StdCtrls, db, ExtCtrls, DBGrids, Buttons, edit_database, types;

type

  TActionType = (atInsert, atUpdate);

  TCustomEditClass = class of TCustomEdit;

  TCellEdit = class
  private
    pnlCellEdit: TPanel;
    lbTitle: TLabel;
    FValue: Variant;
    FTag: integer;
    FReferringField: TDBField;
    FDisplayedField: TDBField;
    procedure SetValue(Value: Variant); virtual; abstract;
  public
    property Value: Variant read FValue write SetValue;
    property Tag: integer read FTag write FTag;
	end;

  TEditCellEdit = class (TCellEdit)
  private
    CellEditor: TCustomEdit;
    procedure SetValue(AValue: Variant); override;
  public
    procedure CellEditorChange(Sender: TObject);
    constructor Create(ADisplayedField: TDBField; APos: integer; ACard: TForm);
	end;

  TComboCellEdit = class (TCellEdit)
  private
    cbbValues: TComboBox;
    ComboIDs: TIntegerDynArray;
    procedure SetValue(AValue: Variant); override;
  public
    procedure cbbValuesChange(Sender: TObject);
    constructor Create(AReferringField, ADisplayedField: TDBField; APos: integer; ACard: TForm);
	end;

	{ TRecordCard }

  TRecordCard = class(TForm)
  published
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    FCellEdits: array of TCellEdit;
    FActionType: TActionType;
    FTable: TDBTable;
    FPrimaryKey: integer;
    FOkClick: TNotifyEvent;
    FVisibleFields: TStringList;
  public
    NewValues: TVariantDynArray;
    OldValues: TVariantDynArray;
    property OnOkClick: TNotifyEvent read FOkClick write FOkClick;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    constructor Create(ATable: TDBTable; AActionType: TActionType);
  end;

  TEditRecordCard = class(TRecordCard)
  public
    constructor Create(ATable: TDBTable; AActionType: TActionType);
  end;

  TRecordCardDynArray = array of TRecordCard;

var
  TypeOfEditor: array [Low(TFieldType)..High(TFieldType)] of TCustomEditClass;

implementation

{$R *.lfm}

procedure TRecordCard.btnOkClick(Sender: TObject);
var
  i, k: integer;
begin
  k := 0;
  for i := 0 to High(NewValues) do
    if Assigned(FCellEdits[k]) and (FCellEdits[k].Tag = i) then begin
      NewValues[i] := FCellEdits[k].Value;
      inc(k);
    end;
  case FActionType of
    atUpdate: UpdateRecord(FTable, OldValues, NewValues);
    atInsert: InsertRecord(FTable, NewValues);
  end;
  FOkClick(Sender);
  Close;
end;

procedure TRecordCard.btnCancelClick(Sender: TObject);
begin
  Close;
end;

constructor TRecordCard.Create(ATable: TDBTable; AActionType: TActionType);
var
  i, k: integer;
begin
  inherited Create(Application);
  Caption := 'Добавить запись';
  Position := poDefault;
  FActionType := AActionType;
  FTable := ATable;
  OnClose := @FormClose;
  VisibleColumnsToList(ATable, FVisibleFields);

  k := 0;
  for i := 0 to High(ATable.Fields) do begin
    SetLength(NewValues, Length(NewValues) + 1);

    if (not ATable.Fields[i].Visible) and (ATable.Fields[i].FieldRef = nil) then begin
      NewValues[i] := MaxValue(ATable.Fields[i]) + 1;
      continue;
		end;

    SetLength(FCellEdits, Length(FCellEdits) + 1);
    if ATable.Fields[i].FieldRef = nil then
      FCellEdits[High(FCellEdits)] := TEditCellEdit.Create(AFields.Objects[k] as TDBField, i, Self)
    else
      FCellEdits[High(FCellEdits)] := TComboCellEdit.Create(ATable.Fields[i].FieldRef, AFields.Objects[k] as TDBField, i, Self);

    FCellEdits[High(FCellEdits)].Tag := i;
    NewValues[i] := FCellEdits[High(FCellEdits)].Value;
    inc(k);
  end;

  Height := FCellEdits[0].pnlCellEdit.Height * (1 + Length(FCellEdits)) + 30;
  BorderStyle := bsSingle;
end;

constructor TEditRecordCard.Create(ATable: TDBTable; AActionType: TActionType);
var
  i, k: integer;
begin
  inherited Create(ATable, AActionType);
  Caption := 'Изменить запись';

  for i := 0 to High(FCellEdits) do
    FCellEdits[i].Value :=
    AGrid.DataSource.DataSet.FieldByName
    ((AFields.Objects[i] as TDBField).TableOwner.Name + (AFields.Objects[i] as TDBField).Name).Value;
end;

procedure TRecordCard.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

constructor TEditCellEdit.Create(ADisplayedField: TDBField; APos: integer; ACard: TForm);
begin
  FDisplayedField := ADisplayedField;
  FReferringField := nil;

  pnlCellEdit := TPanel.Create(ACard);
  with pnlCellEdit do begin
    Parent := ACard;
    Height := 50;
    Align := alTop;
    Top := APos * Height;
  end;

  lbTitle := TLabel.Create(pnlCellEdit);
  with lbTitle do begin
    Parent := pnlCellEdit;
    AutoSize := false;
    Top := 10;
    Left := 8;
    Width := 120;
    Caption := ADisplayedField.Caption;
	end;

  CellEditor := TEdit.Create(pnlCellEdit);
  with CellEditor do begin
    Parent := pnlCellEdit;
    OnChange := @CellEditorChange;
    AutoSize := false;
    Top := 10;
    Left := lbTitle.Left + lbTitle.Width;
    Height := 30;
    Width := pnlCellEdit.Width;
    MaxLength := ADisplayedField.VarCharLimit;
    Anchors := [akLeft, akRight];
	end;

  ACard.ActiveControl := CellEditor;
end;

constructor TComboCellEdit.Create(AReferringField, ADisplayedField: TDBField; APos: integer; ACard: TForm);
begin
  FDisplayedField := ADisplayedField;
  FReferringField := AReferringField;

  pnlCellEdit := TPanel.Create(ACard);
  with pnlCellEdit do begin
    Parent := ACard;
    Height := 50;
    Align := alTop;
    Top := APos * Height;
  end;

  lbTitle := TLabel.Create(pnlCellEdit);
  with lbTitle do begin
    Parent := pnlCellEdit;
    AutoSize := false;
    Top := 10;
    Left := 8;
    Width := 120;
    Caption := ADisplayedField.Caption;
	end;

  cbbValues := TComboBox.Create(pnlCellEdit);
  with cbbValues do begin
    Parent := pnlCellEdit;
    AutoSize := false;
    Top := 10;
    Left := lbTitle.Left + lbTitle.Width;
    Height := 30;
    Width := 320;
    Style := csDropDownList;
    ADisplayedField.RowsTo(cbbValues, ComboIDs);
    OnChange := @cbbValuesChange;
  end;
  cbbValuesChange(cbbValues);
end;

procedure TComboCellEdit.cbbValuesChange(Sender: TObject);
begin
  FValue := ComboIDs[(Sender as TComboBox).ItemIndex];
end;

procedure TEditCellEdit.CellEditorChange(Sender: TObject);
begin
  FValue := (Sender as TEdit).Text;
end;

procedure TEditCellEdit.SetValue(AValue: Variant);
begin
  CellEditor.Text := AValue;
  FValue := AValue;
end;

procedure TComboCellEdit.SetValue(AValue: Variant);
begin
  cbbValues.ItemIndex := cbbValues.Items.IndexOf(AValue);
  cbbValuesChange(cbbValues);
end;

end.
















