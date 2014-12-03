unit UCheckGroupForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  metadata;

type

  { TCheckGroupForm }

  TCheckGroupForm = class(TForm)
    CheckGroup: TCheckGroup;
  private
    { private declarations }
  public
    constructor Create(TheOwner: TComponent;
      aVisible: array of Boolean; aCols: array of TColumnInfo);
  end;

var
  CheckGroupForm: TCheckGroupForm;

implementation

{$R *.lfm}

{ TCheckGroupForm }

constructor TCheckGroupForm.Create(TheOwner: TComponent;
  aVisible: array of Boolean; aCols: array of TColumnInfo);
var
  i: integer;
begin
  inherited Create(TheOwner);
  for i := 0 to High(aCols) do
  begin
    CheckGroup.Items.AddStrings(aCols[i].Caption);
    CheckGroup.Checked[i] := aVisible[i];
  end;
end;

end.

