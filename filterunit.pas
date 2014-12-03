Unit FilterUnit;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, MetaUnit;

Type

  TCondition = Record
    Caption: String;
    QueryF: String;
    ParamF: string;
  End;

  TArrayOfCondition = Class
    Conditions: Array Of TCondition;
    Constructor Create();
    Procedure AddCondition(acaption, aQueryF, aParamF: String);
  End;

  TFilter = Class
    fCondition: TArrayOfCondition;
    DeleteFilter: TSpeedButton;
    FilterCondition: TComboBox;
    FilterColumn: TComboBox;
    FilterPanel: TPanel;
    FilterEdit: TEdit;
    fCaption: String;
    fQuery: String;
    fTable: TTable;
    fTag: Integer;
    Constructor Create(aOwner: TWinControl; uTable: TTable);
    Procedure CreateDeletePanel(aOwner: TWinControl);
    Destructor Destroy; Override;
    Function getquery(aIndex: integer): string;
  End;

  { TArrayOfFilters }

  TArrayOfFilters = Class
    Filters: Array Of TFilter;
    Procedure AddFilter(aOwner: TWinControl; uTable: TTable);
    Procedure DeleteFilter(aIndex: Integer);
    Function createfilterquery: String;
    Procedure DelClick(Sender: TObject);
    Destructor Destroy; Override;
    Constructor Create();
  End;

Implementation

{ TArrayOfFilters }

Constructor tarrayoffilters.Create;
Begin
  Inherited Create;
End;

Destructor tarrayoffilters.Destroy;
Begin
  Inherited Destroy;
End;

Function tarrayoffilters.createfilterquery: String;
Var
  i: Integer;
Begin
  Result += ' where ';
  For i := 0 To high(Filters)-1 Do
  Begin
    Result += Filters[i].GetQuery(i);
    Result += ' and ';
  End;
  Result += Filters[high(Filters)].GetQuery(i+1);
  ShowMessage(Result);
End;

Procedure tarrayoffilters.deletefilter(aindex: Integer);
Var
  i: Integer;
Begin
  FreeAndNil(Filters[aIndex]);

  For i := aIndex To high(Filters) - 1 Do
  Begin
    Filters[i] := Filters[i + 1];
    Filters[i].fTag := i;
    Filters[i].DeleteFilter.Tag := i;
  End;

  SetLength(Filters, Length(Filters) - 1);
End;

Procedure tarrayoffilters.delclick(Sender: TObject);
Begin
  deletefilter(TSpeedButton(Sender).Tag);
End;

Procedure tarrayoffilters.addfilter(aowner: twincontrol; utable: ttable);
Begin
  SetLength(Filters, length(Filters) + 1);
  Filters[high(Filters)] := TFilter.Create(aOwner, uTable);
  Filters[high(Filters)].fTag := high(Filters);
  Filters[high(Filters)].DeleteFilter.OnClick := @delclick;
  Filters[high(Filters)].DeleteFilter.Tag := high(Filters);
End;

{ TArrayOfCondition }

Procedure TArrayOfCondition.AddCondition(acaption, aQueryF, aParamF: String);
Begin
  SetLength(Conditions, length(Conditions) + 1);
  Conditions[high(Conditions)].Caption := aCaption;
  Conditions[high(Conditions)].QueryF := aQueryF;
  Conditions[high(Conditions)].ParamF := aParamF;
End;

Constructor TArrayOfCondition.Create;
Begin
  AddCondition('>', ' %s.%s > :p%s ', '%s');
  AddCondition('<', ' %s.%s < :p%s ', '%s');
  AddCondition('>=', ' %s.%s >= :p%s ', '%s');
  AddCondition('<=', ' %s.%s <= :p%s ', '%s');
  AddCondition('=', ' %s.%s = :p%s ', '%s');
  AddCondition('начинается на', ' %s.%s like :p%s ', '%s%%');
  AddCondition('заканчивается на', ' %s.%s like :p%s ', '%%%s');
  AddCondition('содержит', ' %s.%s like :p%s ', '%%%s%%');
End;

{ TFilter }

Constructor TFilter.Create(aowner: twincontrol; utable: ttable);
Begin
  fCondition := TArrayOfCondition.Create();
  fTable := uTable;
  CreateDeletePanel(aOwner);
End;

Procedure tfilter.CreateDeletePanel(aowner: twincontrol);
Var
  i: Integer;
Begin
  FilterPanel := TPanel.Create(aOwner);
  With FilterPanel Do
  Begin
    Parent := aOwner;
    Align := alTop;
  End;

  FilterColumn := TComboBox.Create(FilterPanel);
  With FilterColumn Do
  Begin
    Parent := FilterPanel;
    ReadOnly := True;
    Top := 10;
    Left := 20;
    Height := 20;
    Width := 135;
    For i := 0 To high(fTable.ColumnInfos) Do
      Items.Add(fTable.ColumnInfos[i].Caption);
  End;

  FilterCondition := TComboBox.Create(FilterPanel);
  With FilterCondition Do
  Begin
    Parent := FilterPanel;
    ReadOnly := True;
    Top := 10;
    Left := 175;
    Height := 20;
    Width := 135;
    For i := 0 To high(FCondition.Conditions) Do
      Items.Add(FCondition.Conditions[i].Caption);
  End;

  FilterEdit := TEdit.Create(FilterPanel);
  With FilterEdit Do
  Begin
    Parent := FilterPanel;
    Top := 10;
    Left := 335;
    Height := 20;
    Width := 135;
  End;

  DeleteFilter := TSpeedButton.Create(FilterPanel);
  With DeleteFilter Do
  Begin
    Parent := FilterPanel;
    Top := 10;
    Left := 490;
    Height := 25;
    Width := 50;
    Caption := 'Delete';
    Flat := True;
  End;
End;

Destructor tfilter.Destroy;
Begin
  FreeAndNil(FilterCondition);
  FreeAndNil(FilterColumn);
  FreeAndNil(DeleteFilter);
  FreeAndNil(FilterEdit);
  FreeAndNil(FilterPanel);
  Inherited Destroy;
End;

Function tfilter.getquery(aIndex: integer): String;
Begin
  If (not FTable.ColumnInfos[FilterColumn.ItemIndex].isReference) Then
    Result += Format(FCondition.Conditions[FilterCondition.ItemIndex].QueryF,
      [fTable.TName, fTable.ColumnInfos[FilterColumn.ItemIndex].Name,
      IntToStr(aIndex)])
  Else
    Result += Format(FCondition.Conditions[FilterCondition.ItemIndex].QueryF,
      [fTable.ColumnInfos[FilterColumn.ItemIndex].ReferenceTable,
      fTable.ColumnInfos[FilterColumn.ItemIndex].ReferenceName,
      IntToStr(aIndex)]);
End;

End.
