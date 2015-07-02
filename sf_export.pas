unit sf_export;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpspreadsheet, metadata, fpsallformats;

type
  TDblStrinListDynArray =  array of array of TStringList;

procedure ExportToSpreadsheet(ATable: TDBTable; AStrings: TDblStrinListDynArray;
  ColCount, RowCount, StringsCount: integer; Path: string; sfIndex: integer);

implementation

procedure ExportToSpreadsheet(ATable: TDBTable; AStrings: TDblStrinListDynArray;
  ColCount, RowCount, StringsCount: integer; Path: string; sfIndex: integer);
var
  Workbook: TsWorkbook;
  Worksheet: TsWorksheet;
  sfFormat: array [1..2] of TsSpreadsheetFormat;
  DefColWidth: array [1..2] of integer;
  i, j: integer;
begin
  DefColWidth[1] := 35;
  DefColWidth[2] := 20;
  sfFormat[1] := sfOpenDocument;
  sfFormat[2] := sfOpenDocument;
  Workbook := TsWorkbook.Create;
  Worksheet := Workbook.AddWorksheet(ATable.Caption);
  Worksheet.DefaultColWidth := DefColWidth[sfIndex];
  for i := 0 to RowCount - 1 do begin
    for j := 0 to ColCount - 1 do
      if Assigned(AStrings[i, j]) then
        Worksheet.WriteUTF8Text(i, j, AStrings[i, j].Text);
    Worksheet.WriteRowHeight(i, StringsCount + 2);
  end;
  Workbook.WriteToFile(Path, sfFormat[sfIndex]);
  Workbook.Free;
end;

end.

