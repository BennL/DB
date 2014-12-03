unit Query;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, MetaUnit;

procedure ShowItems(SQLQuery: TSQLQuery; DBGrid: TDBGrid; aTable: TTable);
procedure SetCaptions(DBGrid: TDBGrid; uTable: Ttable);
procedure SendQuery(SQLQuery: TSQLQuery; aQuery: string);
procedure ShowFilterResults(sqlquery: TSQLQuery; dbgrid: TDBGrid;
  aTable: TTable; aFilterQuery: string; aParameters: array of string);
procedure ShowOrderByResults(sqlquery: TSQLQuery; dbgrid: TDBGrid;
  aTable: TTable; aOrderByQuery: string);
function CreateQuery(uTable: Ttable): string;

implementation

procedure SetCaptions(dbgrid: tdbgrid; utable: ttable);
var
  i: integer;
begin
  for i := 0 to high(uTable.ColumnInfos) do
  begin
    DBGrid.Columns[i].Title.Caption := uTable.ColumnInfos[i].Caption;
    DBGrid.Columns[i].Width := uTable.ColumnInfos[i].Size;
  end;
end;

function CreateQuery(utable: ttable): string;
var
  i: integer;
begin
  Result := 'Select ';
  for i := 0 to High(uTable.ColumnInfos) do
  begin
    if uTable.ColumnInfos[i].IsReference then
      Result += uTable.ColumnInfos[i].ReferenceTable + '.' +
        uTable.ColumnInfos[i].ReferenceName
    else
      Result += uTable.TName + '.' + uTable.ColumnInfos[i].Name + ' ';
    if i < High(uTable.ColumnInfos) then
      Result += ', ';
  end;
  Result += ' From ' + uTable.TName;
  for i := 0 to High(uTable.ColumnInfos) do
  begin
    if not uTable.ColumnInfos[i].IsReference then
      continue;
    Result += ' inner join ' + uTable.ColumnInfos[i].ReferenceTable +
      ' on ' + uTable.TName + '.' + uTable.ColumnInfos[i].Name +
      ' = ' + uTable.ColumnInfos[i].ReferenceTable + '.ID ';
  end;
end;

procedure SendQuery(sqlquery: tsqlquery; aquery: string);
begin
  SQLQuery.Close;
  SQLQuery.Params.Clear;
  SQLQuery.SQL.Text := aquery;
  SQLQuery.Open;
end;

procedure SendParamQuery(sqlquery: tsqlquery; aquery: string;
  aParameters: array of string);
var
  i: integer;
begin
  SQLQuery.Close;
  SQLQuery.Params.Clear;
  SQLQuery.SQL.Text := aquery;
  for i := 0 to high(aParameters) do
    SQLQuery.ParamByName('p' + IntToStr(i)).AsString := aParameters[i];
  SQLQuery.Open;
end;

procedure ShowItems(sqlquery: tsqlquery; dbgrid: tdbgrid; atable: ttable);
begin
  SendQuery(SQLQuery, createquery(atable));
  SetCaptions(DBGrid, atable);
end;

procedure ShowFilterResults(sqlquery: TSQLQuery; dbgrid: TDBGrid;
  aTable: TTable; aFilterQuery: string; aParameters: array of string);
begin
  SendParamQuery(sqlquery, CreateQuery(aTable) + aFilterQuery, aParameters);
  setcaptions(DBGrid, aTable);
end;

procedure ShowOrderByResults(sqlquery: TSQLQuery; dbgrid: TDBGrid;
  aTable: TTable; aOrderByQuery: string);
begin
  sendquery(sqlquery, CreateQuery(aTable) + aOrderByQuery);
  setcaptions(DBGrid, aTable);
end;

end.
