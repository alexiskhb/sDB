unit record_cards;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, metadata,
  StdCtrls, db, ExtCtrls, DBGrids, Buttons;

type

  TCustomEditClass = class of TCustomEdit;

  TCellEdit = class
  private
    cbbValues: TComboBox;
    pnlCellEdit: TPanel;
    CellEditor: TCustomEdit;
    lbTitle: TLabel;
    FValue: Variant;
    FTag: integer;
    FReferringField: TDBField;
    FDisplayedField: TDBField;
    procedure cbbValuesChange(Sender: TObject);
    procedure CellEditorChange(Sender: TObject);
    procedure SetValue(Value: Variant);
  public
    property Tag: integer read FTag write FTag;
    property Value: Variant read FValue write SetValue;
    constructor Create(ADisplayedField: TDBField; APos: integer; ACard: TForm); overload;
    constructor Create(AReferringField, ADisplayedField: TDBField; APos: integer; ACard: TForm); overload;
	end;

	{ TRecordCard }

  TRecordCard = class(TForm)
    btnCansel: TBitBtn;
    btnOk: TBitBtn;
		procedure btnOkClick(Sender: TObject);
  private
    FCellEdits: array of TCellEdit;
  public
    NewValues: TVariantDynArray;
    constructor Create(ATable: TDBTable; AFields: TStringList);
  end;

  TEditRecordCard = class(TRecordCard)
  public
    OldValues: TVariantDynArray;
    constructor Create(ATable: TDBTable; AFields: TStringList; AGrid: TDBGrid);
	end;

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
end;

constructor TRecordCard.Create(ATable: TDBTable; AFields: TStringList);
var
  i, k: integer;
begin
  inherited Create(Application);
  Caption := 'Добавить запись';
  Position := poScreenCenter;
  k := 0;
  for i := 0 to High(ATable.Fields) do begin
    SetLength(NewValues, Length(NewValues) + 1);

    if (not ATable.Fields[i].Visible) and (ATable.Fields[i].FieldRef = nil) then begin
      NewValues[i] := MaxValue(ATable.Fields[i]) + 1;
      continue;
		end;

    SetLength(FCellEdits, Length(FCellEdits) + 1);
    if ATable.Fields[i].FieldRef = nil then
      FCellEdits[High(FCellEdits)] := TCellEdit.Create(AFields.Objects[k] as TDBField, i, Self)
    else
      FCellEdits[High(FCellEdits)] := TCellEdit.Create(ATable.Fields[i].FieldRef, AFields.Objects[k] as TDBField, i, Self);

    FCellEdits[High(FCellEdits)].Tag := i;
    NewValues[i] := FCellEdits[High(FCellEdits)].Value;
    inc(k);
  end;

  Height := FCellEdits[0].pnlCellEdit.Height * (1 + Length(FCellEdits)) + 30;
  BorderStyle := bsSingle;
end;

constructor TEditRecordCard.Create(ATable: TDBTable; AFields: TStringList; AGrid: TDBGrid);
var
  i, k: integer;
begin
  inherited Create(ATable, AFields);
  Caption := 'Изменить запись';
  SetLength(OldValues, Length(NewValues));

  for i := 0 to High(ATable.Fields) do begin
      OldValues[i] :=
      AGrid.DataSource.DataSet.FieldByName
      (ATable.Fields[i].TableOwner.Name + ATable.Fields[i].Name).Value;
  end;

  for i := 0 to High(NewValues) do
    NewValues[i] := OldValues[i];

  for i := 0 to High(FCellEdits) do begin
    FCellEdits[i].Value :=
    AGrid.DataSource.DataSet.FieldByName
    ((AFields.Objects[i] as TDBField).TableOwner.Name + (AFields.Objects[i] as TDBField).Name).Value;
	end;
end;

constructor TCellEdit.Create(ADisplayedField: TDBField; APos: integer; ACard: TForm);
begin
  FDisplayedField := ADisplayedField;
  FReferringField := nil;
  cbbValues := nil;

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

constructor TCellEdit.Create(AReferringField, ADisplayedField: TDBField; APos: integer; ACard: TForm);
begin
  FDisplayedField := ADisplayedField;
  FReferringField := AReferringField;
  CellEditor := nil;

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
    OnChange := @cbbValuesChange;
    AutoSize := false;
    Top := 10;
    Left := lbTitle.Left + lbTitle.Width;
    Height := 30;
    Width := pnlCellEdit.Width;
    Style := csDropDownList;
    ADisplayedField.RowsTo(Items);
    ItemIndex := 0;
    Anchors := [akLeft, akRight];
	end;
  cbbValuesChange(cbbValues);
end;

procedure TCellEdit.cbbValuesChange(Sender: TObject);
begin
  FValue :=
    AppropriateValue(FDisplayedField,
                     (Sender as TCombobox).Items[(Sender as TCombobox).ItemIndex],
                     FReferringField);
end;

procedure TCellEdit.CellEditorChange(Sender: TObject);
begin
  FValue := (Sender as TEdit).Text;
end;

procedure TCellEdit.SetValue(Value: Variant);
begin
  if CellEditor <> nil then begin
    CellEditor.Text := Value;
    FValue := Value;
	end
	else if cbbValues <> nil then begin
    cbbValues.ItemIndex := cbbValues.Items.IndexOf(Value);
    cbbValuesChange(cbbValues);
	end;
end;

end.
















