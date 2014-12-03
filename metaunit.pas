Unit MetaUnit;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, DB, DBGrids, DBCtrls, sqldb, Menus, Dialogs;

Type

  { TColumn }

  TColumn = Object
    Name: String;
    Caption: String;
    ReferenceTable: String;
    ReferenceName: String;
    Size: Integer;
    Function IsReference: Boolean; Inline;
  End;

  TColumns = Array Of TColumn;

  { TTable }

  TTable = Class
    ColumnInfos: TColumns;
    TCaption: String;
    TName: String;
    TColumn: TColumns;
    Sort: boolean;
    Function AddColumn(aName, aCaption, aReferenceName: String;
      aSize: Integer): TTable;
    Constructor Create(aCaption, aName: String);
  End;

  TTables = Array Of TTable;

  TListOfTables = Class
    TableInfos: TTables;
    Constructor Create();
    Function Add(aCaption, aName: String): TTable;
  End;

Implementation

{ TColumn }

Function TColumn.IsReference: Boolean;
begin
  Result := ReferenceName <> '';
end;

Function TTable.AddColumn(aName, aCaption, aReferenceName: String;
  aSize: Integer): TTable;
begin
  SetLength(ColumnInfos, length(ColumnInfos) + 1);
  With ColumnInfos[High(ColumnInfos)] Do
  Begin
    Name := aName;
    Caption := aCaption;
    If aReferenceName <> '' Then
    Begin
      ReferenceName := aReferenceName;
      ReferenceTable := Copy(aName, 1, Length(aName) - 3) + 's';
    End;
    Size := aSize;
  End;
  Result := self;
end;

Constructor TTable.Create(aCaption, aName: String);
begin
  TCaption := aCaption;
  TName := aName;
end;

Function TListOfTables.Add(aCaption, aName: String): TTable;
begin
  Result := TTable.Create(aCaption, aName);
  SetLength(TableInfos, Length(TableInfos) + 1);
  TableInfos[High(TableInfos)] := Result;
end;


Constructor TListOfTables.Create();
begin
  Add('Предметы', 'Subjects').
    AddColumn('Name', 'Название предмета', '', 400);
  Add('Группы', 'Groups').
    AddColumn('Name', 'Группа', '', 100).
    AddColumn('Group_size', 'Размер группы', '', 100);
  Add('Преподаватели', 'Professors').
    AddColumn('Name', 'Преподаватель', '', 150);
  Add('Кабинеты', 'Rooms').
    AddColumn('Name', 'Номер кабинета', '', 100).
    AddColumn('Room_size', 'Вместимость', '', 80);
  Add('Расписание', 'Schedule_items').
    AddColumn('Subject_id', 'Название предмета', 'Name', 400).
    AddColumn('Subject_type_id', 'Тип', 'Name', 50).
    AddColumn('Professor_id', 'Преподаватель', 'Name', 150).
    AddColumn('Day_id', 'День недели', 'Name', 100).
    AddColumn('Group_id', 'Группа', 'Name', 100).
    AddColumn('Room_id', 'Номер кабинета', 'Name', 100);
end;

end.
