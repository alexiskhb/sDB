unit query_filter;

{$mode objfpc}{$H+}

interface

uses
  Classes, lcltype, SysUtils, Forms, DB,
  ExtCtrls, Dialogs, Controls, StdCtrls, metadata, Spin, Buttons;

type

  TRelationalOperation = (roContaining, roStartsWith, roGreater, roLess, roNotLess, roNotGreater, roEqual,
    roInequal);

  TCustomEditClass = class of TCustomEdit;

  TRelOperation = class
  private
    FCaption: string;
    FCode: string;
  public
    property Caption: string read FCaption;
    property Code: string read FCode;
    constructor Create(aCaption, aCode: string);
	end;

  TQueryFilter = class
  private
    FTag: integer;
    FDestroying: TNotifyEvent;
    FChangingData: TNotifyEvent;
    FPanel: TCustomControl;
    FHeight: integer;
    FTop: integer;
    FAddingFilter: TNotifyEvent;
    cbbFields: TComboBox;
    cbbOperations: TComboBox;
    btnDeleteFilter: TButton;
    btnAddFilter: TButton;
    FTable: TDBTable;
    procedure SetFilterTop(Value: integer);
    procedure SetFilterHeight(Value: integer);
    procedure SetFilterTag(Value: integer);
    function GetField: TDBField;
    function GetValue: Variant;
    function GetOperation: string;
  public
    ConstantEditor: TCustomEdit;
    property ChosenField: TDBField read GetField;
    property Tag: integer read FTag write SetFilterTag;
    property OnDestroy: TNotifyEvent read FDestroying write FDestroying;
    property OnChangeData: TNotifyEvent read FChangingData write FChangingData;
    property OnFilterAdd: TNotifyEvent read FAddingFilter write FAddingFilter;
    property Top: integer read FTop write SetFilterTop;
    property Height: integer read FHeight write SetFilterHeight;
    property Value: variant read GetValue;
    property Operation: string read GetOperation;
    procedure ChosenFieldChange(Sender: TObject);
    procedure DeleteFilterClick(Sender: TObject; MouseButton: TMouseButton; ShiftState: TShiftState; X, Y: longint);
    procedure AddFieldsForChoose(ATable: TDBTable);
    procedure EditChange(Sender: TObject);
    procedure OperationChange(Sender: TObject);
    procedure AddFilterClick(Sender: TObject);
    constructor Create(ATable: TDBTable; AIndex: integer; APanel: TCustomControl);
    destructor Destroy; override;
  end;

const
  FilterHeight = 26;

var
  Operations: array [Low(TRelationalOperation)..High(TRelationalOperation)] of TRelOperation;
  TypeOfEditor: array [Low(TFieldType)..High(TFieldType)] of TCustomEditClass;
  AvailableOperations: array [Low(TFieldType)..High(TFieldType)] of set of TRelationalOperation;

implementation

constructor TQueryFilter.Create(ATable: TDBTable; AIndex: integer; APanel: TCustomControl);
var
  sbxPanel: TScrollBox;
begin
  FPanel := APanel;
  FTag := AIndex;
  sbxPanel := APanel as TScrollBox;
  FTable := ATable;

  btnDeleteFilter := TButton.Create(sbxPanel);
  with btnDeleteFilter do begin
    Parent := sbxPanel;
    Height := FilterHeight;
    Width := Height;
    FHeight := Height;
    Caption := 'X';
    Left := 2;
    Tag := AIndex;
    OnMouseUp := @DeleteFilterClick;
  end;

  cbbFields := TComboBox.Create(sbxPanel);
  with cbbFields do begin
    Parent := sbxPanel;
    Left := btnDeleteFilter.Left + btnDeleteFilter.Width + 1;
    AutoSize := false;
    Height := btnDeleteFilter.Height + 2;
    Style := csDropDownList;
    //-------------
    AddFieldsForChoose(FTable);
    //-------------
    AddItem('ИЛИ', nil);
    OnChange := @ChosenFieldChange;
  end;

  btnAddFilter := TButton.Create(sbxPanel);
  btnAddFilter.Parent := sbxPanel;
  with btnAddFilter do begin
    Height := btnDeleteFilter.Height;
    Width := Height;
    FHeight := Height;
    Caption := '+';
    Tag := btnDeleteFilter.Tag;
    Top := btnDeleteFilter.Top;
    Left := cbbFields.Left + cbbFields.Width + 1;
    OnClick := @AddFilterClick;
  end;
end;

procedure TQueryFilter.AddFieldsForChoose(ATable: TDBTable);
var
  i: integer;
begin
  with cbbFields do
    with ATable do
      for i := 0 to High(Fields) do begin
        if Fields[i].Visible then begin
          AddItem(Fields[i].Caption, Fields[i]);
        end;
        if Assigned(Fields[i].TableRef) then
          AddFieldsForChoose(Fields[i].TableRef);
      end;
end;

procedure TQueryFilter.DeleteFilterClick(Sender: TObject; MouseButton: TMouseButton; ShiftState: TShiftState; X, Y: longint);
begin
  if Assigned(FChangingData) then FChangingData(Self);
  FDestroying(Sender);
end;

procedure TQueryFilter.ChosenFieldChange(Sender: TObject);
var
  VSender: TComboBox;
  tempft: TFieldType;
  ro: TRelationalOperation;
  Panel: TScrollBox;
begin
  VSender := (Sender as TComboBox);
  Panel := FPanel as TScrollBox;

  if Assigned(ConstantEditor) then begin
    FreeAndNil(ConstantEditor);
    FreeAndNil(cbbOperations);
  end;

  if Assigned(VSender.Items.Objects[VSender.ItemIndex]) then
    tempft := ((VSender.Items.Objects[VSender.ItemIndex]) as TDBField).FieldType
  else
    tempft := ftUnknown;

  cbbOperations := TComboBox.Create(Panel);
  cbbOperations.Parent := Panel;
  with cbbOperations do begin
    AutoSize := false;
    Top := btnDeleteFilter.Top;
    Left := cbbFields.Left + cbbFields.Width + 1;
    Height := cbbFields.Height;
    Style := csDropDownList;
    for ro := Low(TRelationalOperation) to High(TRelationalOperation) do
      if ro in AvailableOperations[tempft] then
        AddItem(Operations[ro].Caption, Operations[ro]);
    ItemIndex := 0;
    OnChange := @OperationChange;
  end;

  ConstantEditor := TypeOfEditor[tempft].Create(Panel);
  ConstantEditor.Parent := Panel;
  with ConstantEditor do begin
    AutoSize := false;
    Height := cbbFields.Height;
    Width := cbbFields.Width;;
    Top := btnDeleteFilter.Top;
    Left := cbbOperations.Left + cbbOperations.Width + 1;
    if TypeOfEditor[tempft] = TSpinEdit then
      with (ConstantEditor as TSpinEdit) do begin
        MaxValue := High(Integer);
        MinValue := Low(Integer);
      end;
    if TypeOfEditor[tempft] = TEdit then
      with (ConstantEditor as TEdit) do
        if Assigned(VSender.Items.Objects[VSender.ItemIndex]) then
          MaxLength := ((VSender.Items.Objects[VSender.ItemIndex]) as TDBField).VarCharLimit;
    OnChange := @EditChange;
  end;

  if tempft = ftUnknown then begin
    ConstantEditor.Visible := false;
    cbbOperations.Visible := false;
  end else
    btnAddFilter.Left := ConstantEditor.Left + ConstantEditor.Width + 1;

  if Assigned(FChangingData) then FChangingData(Self);
end;

procedure TQueryFilter.EditChange(Sender: TObject);
begin
  if Assigned(FChangingData) then FChangingData(Self);
end;

procedure TQueryFilter.OperationChange(Sender: TObject);
begin
  if Assigned(FChangingData) then FChangingData(Self);
end;

procedure TQueryFilter.AddFilterClick(Sender: TObject);
begin
  FAddingFilter(Sender);
end;

procedure TQueryFilter.SetFilterTop(Value: integer);
begin
  btnDeleteFilter.Top := Value;
  cbbFields.Top := Value;
  btnAddFilter.Top := Value;
  if Assigned(ConstantEditor) then begin
    ConstantEditor.Top := Value;
    cbbOperations.Top := Value;
  end;
  FTop := Value;
end;

procedure TQueryFilter.SetFilterHeight(Value: integer);
begin
  btnDeleteFilter.Height := Value;
  cbbFields.Height := Value;
  btnAddFilter.Height := Value;
  if Assigned(ConstantEditor) then begin
    ConstantEditor.Height := Value;
    cbbOperations.Height := Value;
  end;
  FHeight := Value;
end;

procedure TQueryFilter.SetFilterTag(Value: integer);
begin
  btnDeleteFilter.Tag := Value;
  btnAddFilter.Tag := Value;
  FTag := Value;
end;

function TQueryFilter.GetField: TDBField;
begin
  if Assigned(cbbFields.Items.Objects[cbbFields.ItemIndex] as TDBField) then
    Result := (cbbFields.Items.Objects[cbbFields.ItemIndex] as TDBField)
  else
    Result := nil;
end;

function TQueryFilter.GetOperation: string;
begin
  Result := (cbbOperations.Items.Objects[cbbOperations.ItemIndex] as TRelOperation).Code;
end;

function TQueryFilter.GetValue: Variant;
begin
  if ((cbbFields.Items.Objects[cbbFields.ItemIndex]) as TDBField).FieldType = ftInteger then
    Result := (ConstantEditor as TSpinEdit).Value
  else
    Result := ConstantEditor.Text;
end;

destructor TQueryFilter.Destroy;
begin
  FreeAndNil(btnDeleteFilter);
  FreeAndNil(cbbFields);
  FreeAndNil(btnAddFilter);
  if Assigned(ConstantEditor) then begin
    FreeAndNil(ConstantEditor);
    FreeAndNil(cbbOperations);
  end;
  inherited;
end;

constructor TRelOperation.Create(ACaption, ACode: string);
begin
  FCaption := ACaption;
  FCode := ACode;
end;

initialization

  TypeOfEditor[ftInteger] := TSpinEdit;
  TypeOfEditor[ftString] := TEdit;
  TypeOfEditor[ftUnknown] := TEdit;
  AvailableOperations[ftInteger] := [roEqual, roInequal, roGreater, roNotGreater, roNotLess, roLess];
  AvailableOperations[ftString] := [roContaining, roStartsWith, roEqual, roInequal, roGreater, roNotGreater, roNotLess, roLess];
  Operations[roGreater] := TRelOperation.Create('>', '  > ');
  Operations[roContaining] := TRelOperation.Create('Содержит', '  containing ');
  Operations[roEqual] := TRelOperation.Create('=', '  = ');
  Operations[roInequal] := TRelOperation.Create('<>', '  <> ');
  Operations[roLess] := TRelOperation.Create('<', '  < ');
  Operations[roNotGreater] := TRelOperation.Create('<=', '  <= ');
  Operations[roNotLess] := TRelOperation.Create('>=', '  >= ');
  Operations[roStartsWith] := TRelOperation.Create('Начинается с', '  starts with ');

end.

