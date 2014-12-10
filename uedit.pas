unit UEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, DBCtrls, metadata, querycreate, DBComponentCreate;

type

  { TDBEditor }

  TDBEditor = class
    EPanel: TPanel;
    ELabel: TLabel;
    EDBEdit: TEdit;
    CurrentCaption: string;
    IDs: array of integer;
    EDBComBo: TComboBox;
    CountComboBox: integer;
    EColumn: TColumnInfo;
    EDBSQLQuery: TSQLQuery;
    ColumnIndex: integer;
    ID: integer;
    CurrCB, CurrCol: TPoint;
    IsReference: boolean;
  private
    procedure CreateInrterface(aOwner: TWinControl; CurrID: integer);
    procedure SetComboBoxItems(CurrID: integer);
  public
    constructor Create(aOwner: TWinControl; aColumn: TColumnInfo; aID: integer;
      aCurrCB, aCurrCol: TPoint; aIndex: integer);
    destructor Destroy; override;
  end;

  { TDBEditors }

  TDBEditors = class
    DBEditors: array of TDBEditor;
    Params: array of string;
    ETable: TTableInfo;
    ID: integer;
    MSQLQuery: TSQLQuery;
    MTransaction: TSQLTransaction;
    destructor Destroy; override;
    procedure AddEditor(aOwner: TWinControl; aID: integer; aTable: TTableInfo;
      aCurrCB, aCurrCol: TPoint);
    function DeleteQuery: string;
    function InsertEdit: string;
    function UpdateEdit: string;
    procedure UpdateQuery();
    procedure InsertQuery;
  end;

var
  ListOfEditors: TDBEditors;

implementation

constructor TDBEditor.Create(aOwner: TWinControl; aColumn: TColumnInfo;
  aID: integer; aCurrCB, aCurrCol: TPoint; aIndex: integer);
var
  TempSQLQuery: TSQLQuery;
  TempTable: TTableInfo;
begin
  EColumn := aColumn;
  ID := aID;
  IsReference := EColumn.Reference;
  CurrCB := aCurrCB;
  CurrCol := aCurrCol;
  CountComboBox := aIndex;

  TempSQLQuery := SQLQueryCreate();
  TempTable := GetTableByColumn(EColumn);
  SetQuery(TempSQLQuery,
    CreateNotReferenceTableQuery(TempTable) + GetQueryRowByID(TempTable, ID));

  if not IsReference then
  begin
    CurrentCaption := TempSQLQuery.FieldByName(EColumn.Name).AsString;
    CreateInrterface(aOwner, 0);
  end
  else
    CreateInrterface(aOwner, TempSQLQuery.FieldByName(EColumn.Name).AsInteger);
end;

procedure TDBEditor.CreateInrterface(aOwner: TWinControl; CurrID: integer);
begin
  EPanel := TPanel.Create(aOwner);
  with EPanel do
  begin
    Parent := aOwner;
    Align := alTop;
    BevelOuter := bvSpace;
  end;

  ELabel := TLabel.Create(EPanel);
  with ELabel do
  begin
    Parent := EPanel;
    Top := 20;
    Left := 10;
    Caption := EColumn.Caption;
  end;

  if (IsReference) then
  begin
    EDBComBo := TComboBox.Create(EPanel);
    with EDBComBo do
    begin
      Parent := EPanel;
      Top := 18;
      Left := 125;
      Width := 105;
      ReadOnly := True;
    end;
    SetComboBoxItems(CurrID);
  end
  else
  begin
    EDBEdit := TEdit.Create(EPanel);
    with EDBEdit do
    begin
      Parent := EPanel;
      Top := 18;
      Left := 100;
      Width := 135;
      Caption := CurrentCaption;
    end;
  end;
  aOwner.Height := aOwner.Height + EPanel.Height;
end;

procedure TDBEditor.SetComboBoxItems(CurrID: integer);
var
  LookUpQuery: TSQLQuery;
  RefTable: TTableInfo;
begin
  RefTable := GetTableByName(EColumn.ReferenceTable);

  if RefTable = nil then
  begin
    ShowMessage('Не найдена таблица: ' + EColumn.ReferenceTable);
    Exit;
  end;

  LookUpQuery := SQLQueryCreate;
  SetQuery(LookUpQuery, CreateQuery(RefTable));

  with EDBComBo do
  begin
    LookUpQuery.First;
    while (not LookUpQuery.EOF) do
    begin
      Items.Add(LookUpQuery.Fields[1].AsString);
      SetLength(IDs, Length(IDs) + 1);
      IDs[High(IDs)] := LookUpQuery.FieldByName('ID').AsInteger;
      if IDs[high(IDs)] = CurrID then
        ItemIndex := high(IDs);
      LookUpQuery.Next;
    end;

    if (CountComboBox = CurrCB.X) or (CountComboBox = CurrCB.Y) then
    begin
      Enabled := false;
      if CountComboBox = CurrCB.X then
        ItemIndex := CurrCol.X;
      if CountComboBox = CurrCB.Y then
        ItemIndex := CurrCol.Y;
    end;

    FreeAndNil(LookUpQuery);
  end;
end;

destructor TDBEditor.Destroy;
begin
  inherited Destroy;
  FreeAndNil(ELabel);
  FreeAndNil(EDBComBo);
  FreeAndNil(EPanel);
end;

procedure TDBEditors.AddEditor(aOwner: TWinControl; aID: integer;
  aTable: TTableInfo; aCurrCB, aCurrCol: TPoint);
begin
  SetLength(DBEditors, length(DBEditors) + 1);
  ETable := aTable;
  MSQLQuery := SQLQueryCreate;
  ID := aID;

  DBEditors[high(DBEditors)] :=
    TDBEditor.Create(aOwner, ETable.ColumnInfos[Length(DBEditors)], ID, aCurrCB,
      aCurrCol, high(DBEditors));
end;

function TDBEditors.DeleteQuery: string;
begin
  Result += Format('Delete from %s Where %s.ID = %s',
    [ETable.Name, ETable.Name, IntToStr(ID)]);
end;

function TDBEditors.InsertEdit: string;
var
  i: integer;
begin
  Result += Format('Insert into %s', [ETable.Name]);
  Result += Format(' Values (next value for %s, ', [ETable.GenerateName]);

  for i := 0 to high(DBEditors) do
  begin
    with DBEditors[i] do
    begin
      if (EColumn.Reference) then
        Result += QuotedStr(IntToStr(IDs[EDBComBo.ItemIndex]))
      else
        Result += ':p' + IntToStr(i);
      if (i <> high(DBEditors)) then
        Result += ', '
      else
        Result += ')';
    end;
  end;

  // ShowMessage (Result);
end;

function TDBEditors.UpdateEdit: string;
var
  i: integer;
begin
  Result := Format('Update %s Set', [ETable.Name]);

  for i := 0 to high(DBEditors) do
  begin
    with (DBEditors[i]) do
    begin
      if (EColumn.Reference) then
        Result += Format(' %s = %s',
          [EColumn.Name, IntToStr(IDs[EDBComBo.ItemIndex])])


      else
        Result += Format(' %s = :p%s', [EColumn.Name, IntToStr(i)]);
    end;

    if (i <> high(DBEditors)) then
      Result += ', '
    else
      Result += ' ';
  end;

  Result += Format(' Where %s.ID = %s', [ETable.Name, IntToStr(ID)]);
  // ShowMessage (Result);
end;

procedure TDBEditors.UpdateQuery();
begin
  ShowUpdateTable(MSQLQuery, UpdateEdit, Params);
end;

procedure TDBEditors.InsertQuery;
begin
  ShowUpdateTable(MSQLQuery, InsertEdit, Params);
end;

destructor TDBEditors.Destroy;
var
  i: integer;
begin
  for i := high(DBEditors) downto 0 do
    FreeAndNil(DBEditors[i]);

  inherited Destroy;
end;

end.
