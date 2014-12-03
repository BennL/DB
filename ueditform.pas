unit ueditform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, DBGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, DBCtrls, metadata, querycreate, Uedit, Buttons, DBComponentCreate;

type

  { TEditForm }

  TEditForm = class(TForm)
    ImageList: TImageList;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    DeleteBtn: TBitBtn;
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    DBEditors: array of TDBEditor;
    ID: integer;
    ETable: TTableInfo;
    MSQLQuery: TSQLQuery;
    IsInsert: boolean;
    MTransaction: TSQLTransaction;
    constructor Create(TheOwner: TComponent; aTable: TTableInfo; aID: integer);
    procedure BtnsCreate;
  end;

var
  EditForm: TEditForm;

implementation

uses
  main;

{$R *.lfm}

{ TEditForm }

constructor TEditForm.Create(TheOwner: TComponent; aTable: TTableInfo;
  aID: integer);
const
  BtnSize = 60;
var
  i: integer;
begin
  inherited Create(TheOwner);
  ETable := aTable;
  ListOfEditors := TDBEditors.Create;
  ID := aID;

  if ID = 0 then
    IsInsert := True;

  MSQLQuery := SQLQueryCreate;
  SetQuery(MSQLQuery, CreateQuery(ETable) + GetQueryRowByID(ETable, ID));

  for i := 0 to high(ETable.ColumnInfos) do
  begin
    if ETable.ColumnInfos[i].VisableColumn then
    begin
      ListOfEditors.AddEditor(Self, ID, ETable);
    end;
  end;
  Self.Height := Self.Height + BtnSize;
  BtnsCreate;
end;

procedure TEditForm.BtnsCreate;
begin
  OKBtn := TBitBtn.Create(Self);
  with OKBtn do
  begin
    Parent := self;
    Left := 32;
    Top := Self.Height - 50;
    Height := 40;
    Kind := bkOK;
    OnClick := @OKBtnClick;
  end;

  CancelBtn := TBitBtn.Create(Self);
  with CancelBtn do
  begin
    Parent := self;
    Left := 152;
    Top := Self.Height - 50;
    Height := 40;
    Kind := bkCancel;
    Caption := 'Отмена';
    OnClick := @CancelBtnClick;
  end;

  if not IsInsert then
  begin
    with OKBtn do
    begin
      Width := 75;
      Left := 8;
    end;
    with CancelBtn do
    begin
      Width := 75;
      Left := 88;
    end;
    DeleteBtn := TBitBtn.Create(Self);
    with DeleteBtn do
    begin
      Parent := self;
      Left := 168;
      Top := Self.Height - 50;
      Height := 40;
      Width := 75;
      Caption := 'Удалить';
      OnClick := @DeleteBtnClick;
      ImageList.GetBitmap(0, Glyph);
    end;
  end;
end;

procedure TEditForm.OKBtnClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(ListOfEditors.DBEditors) do
  begin
    with ListOfEditors do
    begin
      if not DBEditors[i].IsReference then
      begin
        SetLength(Params, length(Params) + 1);
        Params[i] := DBEditors[i].EDBEdit.Caption;
      end;
    end;
  end;

  if (not IsInsert) then
    ListOfEditors.UpdateQuery
  else
    ListOfEditors.InsertQuery;

  MainForm.SQLTransaction.Commit;
  Close;
end;

procedure TEditForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TEditForm.DeleteBtnClick(Sender: TObject);
var
  BtnClick: integer;
begin
  ShowUpdateTable(MSQLQuery, ListOfEditors.DeleteQuery, ListOfEditors.Params);
  BtnClick := MessageDlg('Вы уверенны, что хотите удалить поле?',
    mtConfirmation, mbOKCancel, 0);
  if (BtnClick = mrOk) then
  begin
    MainForm.SQLTransaction.Commit;
    Close;
  end
  else
    MainForm.SQLTransaction.Rollback;
end;

end.
