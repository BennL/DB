unit Query;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, MetaUnit;

procedure ShowItems(SQLQuery: TSQLQuery; DBGrid: TDBGrid; aTable: TTable);
procedure SetCaptions(DBGrid: TDBGrid; uTable: Ttable);
function CreateQuery(uTable: Ttable): string;
procedure SendQuery(SQLQuery: TSQLQuery; aQuery: string);

implementation

procedure setcaptions(dbgrid: tdbgrid; utable: ttable);
var
  i: integer;
begin
  for i := 0 to high(uTable.ColumnInfos) do
  begin
    DBGrid.Columns[i].Title.Caption := uTable.ColumnInfos[i].Caption;
    DBGrid.Columns[i].Width := uTable.ColumnInfos[i].Size;
  end;
end;

function createquery(utable: ttable): string;
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

procedure sendquery(sqlquery: tsqlquery; aquery: string);
begin
  SQLQuery.Close;
  SQLQuery.Params.Clear;
  SQLQuery.SQL.Text := aquery;
  SQLQuery.Open;
end;

procedure showitems(sqlquery: tsqlquery; dbgrid: tdbgrid; atable: ttable);
begin
  SendQuery (SQLQuery, createquery (atable));
  SetCaptions(DBGrid, atable);
end;

end.
