unit querycreate;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, DBGrids, Graphics,
  Dialogs, metadata, ufilter, Grids;

procedure ShowTable(aSQLQuery: TSQLQuery; aDBGrid: TDBGrid; aTable: TTableInfo);
procedure ShowFilterTable(aSQLQuery: TSQLQuery; aDBGrid: TDBGrid;
  aTable: TTableInfo; aFQuery: string; aParam: array of string);
procedure ShowSortTable(aSQLQuery: TSQLQuery; aDBGrid: TDBGrid;
  aTable: TTableInfo; aFQuery: string; aIndex: integer; aOrder: boolean);
procedure ShowUpdateTable(aSQLQuery: TSQLQuery; aUQuery: string;
  aParam: array of string);
procedure SetQuery(aSQLQuery: TSQLQuery; aQuery: string);
procedure SetParamQuery(aSQLQuery: TSQLQuery; aQuery: string; aParam: array of string);
procedure SetScheduleQuery(aSQLQuery: TSQLQuery; aQuery: string);
procedure SetUpdateQuery(aSQLQuery: TSQLQuery; aQuery: string; aParam: array of string);
function CreateQuery(aTable: TTableInfo): string;
procedure SetCaption(aDBGrid: TDBGrid; aTable: TTableInfo);
function GetQueryRowByID(aTable: TTableInfo; aID: integer): string;
function CreateNotReferenceTableQuery(aTable: TTableInfo): string;

implementation

procedure ShowTable(aSQLQuery: TSQLQuery; aDBGrid: TDBGrid; aTable: TTableInfo);
begin
  SetQuery(aSQLQuery, CreateQuery(aTable));
  SetCaption(aDBGrid, aTable);
end;

procedure ShowFilterTable(aSQLQuery: TSQLQuery; aDBGrid: TDBGrid;
  aTable: TTableInfo; aFQuery: string; aParam: array of string);
begin
  SetParamQuery(aSQLQuery, CreateQuery(aTable) + aFQuery, aParam);
  SetCaption(aDBGrid, aTable);
end;

procedure ShowSortTable(aSQLQuery: TSQLQuery; aDBGrid: TDBGrid;
  aTable: TTableInfo; aFQuery: string; aIndex: integer; aOrder: boolean);
var
  sQuery: string;
begin
  sQuery := Format(' Order By %s.%s', [aTable.Name, aTable.ColumnInfos[aIndex].Name]);
  if (aOrder) then
    sQuery += ''
  else
    sQuery += ' Desc ';

  SetQuery(aSQLQuery, CreateQuery(aTable) + aFQuery + sQuery);
  SetCaption(aDBGrid, aTable);
end;

procedure ShowUpdateTable(aSQLQuery: TSQLQuery; aUQuery: string;
  aParam: array of string);
begin
  SetUpdateQuery(aSQLQuery, aUQuery, aParam);
end;

procedure SetQuery(aSQLQuery: TSQLQuery; aQuery: string);
begin
  //ShowMessage (aQuery);
  with aSQLQuery do
  begin
    Close;
    SQL.Text := aQuery;
    Open;
  end;
end;

procedure SetParamQuery(aSQLQuery: TSQLQuery; aQuery: string; aParam: array of string);
var
  i: integer;
begin
  with aSQLQuery do
  begin
    Close;
    SQL.Text := aQuery;
    for i := 0 to high(aParam) do
    begin
      ParamByName('p' + IntToStr(i)).AsString := aParam[i];
    end;
    Open;
  end;
end;

procedure SetScheduleQuery(aSQLQuery: TSQLQuery; aQuery: string);
var
  i: integer;
begin
  with aSQLQuery do
  begin
    Close;
    SQL.Text := aQuery;
    ExecSQL;
  end;
end;

procedure SetUpdateQuery(aSQLQuery: TSQLQuery; aQuery: string; aParam: array of string);
var
  i: integer;
begin
  with aSQLQuery do
  begin
    Close;
    SQL.Text := aQuery;
    for i := 0 to high(aParam) do
    begin
      ParamByName('p' + IntToStr(i)).AsString := aParam[i];
    end;
    ExecSQL;
  end;
end;

function CreateQuery(aTable: TTableInfo): string;
var
  i: integer;
begin
  Result += 'Select ';

  with (aTable) do
  begin
    for i := 0 to high(ColumnInfos) do
    begin
      with (ColumnInfos[i]) do
      begin
        if (Reference) then
          Result += Format('%s.%s', [ReferenceTable, ReferenceColumn])
        else
          Result += Format('%s.%s ', [aTable.Name, Name]);
      end;

      if (i <> high(ColumnInfos)) then
        Result += ', ';
    end;

    Result += Format(' From %s', [Name]);

    for i := 0 to high(ColumnInfos) do
    begin
      with (ColumnInfos[i]) do
      begin
        if (not Reference) then
          Continue;
        Result += Format(' Inner Join %s on %s.%s = %s.ID',
          [ReferenceTable, aTable.Name, Name, ReferenceTable]);
      end;
    end;
  end;
end;

procedure SetCaption(aDBGrid: TDBGrid; aTable: TTableInfo);
var
  i: integer;
begin
  for i := 0 to high(aTable.ColumnInfos) do
  begin
    aDBGrid.Columns[i].Width := aTable.ColumnInfos[i].Size;
    aDBGrid.Columns[i].Title.Caption := aTable.ColumnInfos[i].Caption;
    aDBGrid.Columns[i].ReadOnly := True;
    if (aTable.ColumnInfos[i].VisibleColumn = False) then
      aDBGrid.Columns[i].Visible := False;
  end;
end;

function GetQueryRowByID(aTable: TTableInfo; aID: integer): string;
begin
  Result := Format(' Where %s.ID = %d', [aTable.Name, aID]);
end;

function CreateNotReferenceTableQuery(aTable: TTableInfo): string;
var
  i: integer;
begin
  Result := 'Select ';
  with aTable do
  begin
    for i := 0 to high(ColumnInfos) do
    begin
      Result += Format('%s.%s', [Name, ColumnInfos[i].Name]);
      if i <> high(ColumnInfos) then Result += ', ';
    end;
    Result += ' from ' + Name;
  end;
end;


end.

