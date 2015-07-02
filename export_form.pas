unit export_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, sf_export, CheckLst, metadata, query_filter, Buttons;

type

  { TExportForm }

  TExportForm = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnBrowse: TButton;
    cgParams: TCheckGroup;
    lbePath: TLabeledEdit;
    rgFormat: TRadioGroup;
    SaveDialog: TSaveDialog;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FStrings: TDblStrinListDynArray;
    FColsCount: integer;
    FRowsCount: integer;
    FCheckList: TCheckListBox;
    FFilters: TQueryFilterDynArray;
    FHorzField: TDBField;
    FVertField: TDBField;
    btnApply: TBitbtn;
    FCheckedCount: ^integer;
    FIsColEmpty: TBoolDynArray;
    FIsRowEmpty: TBoolDynArray;
  public
    procedure HTMLFillStringList(StringList: TStringList);
    procedure HTMLAppendDescription(StringList: TStringList);
    procedure HTMLAppendCells(StringList: TStringList);
    constructor Create(AParent: TForm; AStrings: TDblStrinListDynArray;
      AColsCount, ARowsCount: integer; ACheckList: TCheckListBox;
      AHorzField, AVertField: TDBField; AFilters: TQueryFilterDynArray; AbtnApply: TBitbtn;
      var ACheckedCount: integer; IsColEmpty, IsRowEmpty: TBoolDynArray);
  end;

var
  ExportForm: TExportForm;

implementation

{$R *.lfm}

procedure TExportForm.btnBrowseClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    lbePath.Text := SaveDialog.FileName;
end;

procedure TExportForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TExportForm.btnOkClick(Sender: TObject);
var
  StringList: TStringList;
  Stream: TFileStream;
begin

  case rgFormat.ItemIndex of
    0: begin
         StringList := TStringList.Create;
         Stream := TFileStream.Create(Utf8ToAnsi(lbePath.Text), fmCreate);
         HTMLFillStringList(StringList);
         StringList.SaveToStream(Stream);
         StringList.Free;
         Stream.Free;
       end;
    1: begin
        ShowMessage('dsa');
         //ExportToSpreadsheet(
         //                    FTable,
         //                    sgTable.CellStrings,
         //                    sgTable.ColCount,
         //                    sgTable.RowCount,
         //                    FCheckedCount,
         //                    SaveDialog.FileName,
         //                    1);
       end;
  end;

  Close;
end;

procedure TExportForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

constructor TExportForm.Create(AParent: TForm; AStrings: TDblStrinListDynArray;
  AColsCount, ARowsCount: integer; ACheckList: TCheckListBox;
  AHorzField, AVertField: TDBField; AFilters: TQueryFilterDynArray; AbtnApply: TBitbtn;
  var ACheckedCount: integer; IsColEmpty, IsRowEmpty: TBoolDynArray);
var
  i: integer;
begin
  inherited Create(AParent);
  FColsCount := AColsCount;
  FRowsCount := ARowsCount;
  FCheckList := ACheckList;
  FFilters := AFilters;
  FHorzField := AHorzField;
  FVertField := AVertField;
  FStrings := AStrings;
  btnApply := AbtnApply;
  FIsColEmpty := IsColEmpty;
  FIsRowEmpty := IsRowEmpty;
  FCheckedCount := @ACheckedCount;
  Caption := 'Экспорт: ' + AVertField.Caption + ' \ ' + AHorzField.Caption;
  with cgParams do
    for i := 0 to Items.Count - 1 do
      Checked[i] := true;
end;

procedure TExportForm.HTMLAppendDescription(StringList: TStringList);
var
  i: integer;
  FieldCaption: string;
begin
  with StringList do begin
    if btnApply.Enabled then
      Append('Описание может не соответствовать содержимому таблицы' + '<br><br>');

    if (FCheckedCount^ > 0) and (cgParams.Checked[0]) then begin
      Append('<li>' + 'Отображаемые поля:' + '</li>');
      for i := 0 to FCheckList.Count - 1 do
        if FCheckList.Checked[i] then
          Append(FCheckList.Items.Strings[i] + ' <br>');
    end;

    if (Length(FFilters) > 0) and (cgParams.Checked[1]) then begin
      Append('<br><li>' + 'Фильтры:' + ' </li>');
      for i := 0 to High(FFilters) do begin
        FieldCaption := '';
        if Assigned(FFilters[i].ChosenField) then
          FieldCaption := FFilters[i].ChosenField.Caption
        else if Assigned(FFilters[i].ConstantEditor) then
          FieldCaption := 'ИЛИ'
        else continue;
        Append(
               FieldCaption + ' ' +
               FFilters[i].Operation.Caption + ' ' +
               string(FFilters[i].Value) + '<br>');
      end;
    end;

    if cgParams.Checked[2] then
      Append(
             '<br><br>' +
             FVertField.Caption + ' \ ' +
             FHorzField.Caption);
  end;
end;

procedure TExportForm.HTMLAppendCells(StringList: TStringList);
var
  i, j, k: integer;
begin
  with StringList do begin
    Append('<table>');
    for i := 0 to FRowsCount - 1 do begin
      if cgParams.Checked[4] and FIsRowEmpty[i] then continue;
      Append('<tr>');
      for j := 0 to FColsCount - 1 do begin
        if cgParams.Checked[3] and FIsRowEmpty[j] then continue;
        Append('<td>');
        if (i < Length(FStrings)) and (j < Length(FStrings[i])) and Assigned(FStrings[i, j]) then
          for k := 0 to FStrings[i, j].Count - 1 do
            Append(FStrings[i, j].Strings[k] + ' <br>');
        Append('</td>');
      end;
      Append('</tr>');
    end;
    Append('</table>');
  end;
end;

procedure TExportForm.HTMLFillStringList(StringList: TStringList);
begin
  with StringList do begin
    Append('<html> <head> <meta charset="utf-8">');
    Append('<style>');
    Append('table { width: 1500px; height: 750px; border: 1px solid #000; border-collapse:collapse;}');
    Append('td { border: 1px solid #000; padding: 5px; vertical-align: top; font-size: 16px}');
    Append('</style>');
    Append('</head>');
    Append('<body>');
    HTMLAppendDescription(StringList);
    HTMLAppendCells(StringList);
    Append('</body> </html>');
  end;
end;

end.

