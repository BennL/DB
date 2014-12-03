unit ueditform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, DBCtrls, metadata, querycreate, Uedit, Buttons, DBComponentCreate;

const
  Size = 60;

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
    constructor Create(TheOwner: TComponent; aTable: TTableInfo; aID: integer;
      aCurrCB, aCurrCol: TPoint);
    procedure BtnsCreate;
  end;

  TList = array of TEditForm;

  { TListOfEditForm }

  TListOfEditForm = class
    EditForms: array of TList;
    constructor Create;
    function HashFunction(element: integer): integer;
    procedure InsertForm(var a: TList; element: TEditForm);
    procedure DeleteForm(var a: TList; element: TEditForm);
  end;

var
  EditForm: TEditForm;

implementation

uses
  main;

{ TListOfEditForm }

constructor TListOfEditForm.Create;
begin
  SetLength(EditForms, Size);
end;

function TListOfEditForm.HashFunction(element: integer): integer;
begin
  Result := element mod Size;
end;

procedure TListOfEditForm.InsertForm(var a: TList; element: TEditForm);
var
  Count, i: integer;
begin
  Count := -1;

  for i := 0 to length(a) - 1 do
    if a[i].ID = element.ID then
      Count := i;

  if Count = -1 then
  begin
    SetLength(a, length(a) + 1);
    a[high(a)] := element;
  end
  else
  begin
    a[count].Free;
    a[count] := element;
  end;
end;

procedure TListOfEditForm.DeleteForm(var a: TList; element: TEditForm);
var
  i, Count, index: integer;
begin
  for i := 0 to high(a) do
    if a[i] = element then
    begin
      Inc(Count);
      index := i;
      break;
    end;

  if Count = 0 then
    exit
  else
    for i := index to high(a) - 1 do
      a[i] := a[i + 1];
  SetLength(a, length(a) - 1);
end;

{$R *.lfm}

{ TEditForm }

constructor TEditForm.Create(TheOwner: TComponent; aTable: TTableInfo;
  aID: integer; aCurrCB, aCurrCol: TPoint);
const
  BtnSize = 60;
var
  i: integer;
begin
  inherited Create(TheOwner);
  ETable := aTable;
  ListOfEditors := TDBEditors.Create;
  ID := aID;

  if ID = 0 then begin
    IsInsert := True;
    Self.Caption:= 'Вставка';
  end;

  MSQLQuery := SQLQueryCreate;
  SetQuery(MSQLQuery, CreateQuery(ETable) + GetQueryRowByID(ETable, ID));

  for i := 0 to high(ETable.ColumnInfos) do
  begin
    if ETable.ColumnInfos[i].VisibleColumn then
    begin
      ListOfEditors.AddEditor(Self, ID, ETable, aCurrCB, aCurrCol);
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
