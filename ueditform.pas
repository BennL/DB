unit ueditform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TEditForm }

  TEditForm = class(TForm)
    EEdit: TEdit;
    EButton: TButton;
  private
    { private declarations }
  public
    constructor Create(TheOwner: TComponent; aReference: Boolean);
    procedure CreateInterface (aReference: Boolean);
  end;

var
  EditForm: TEditForm;

implementation

{$R *.lfm}

{ TEditForm }

constructor TEditForm.Create(TheOwner: TComponent; aReference: Boolean);
begin
  inherited Create(TheOwner);
end;

procedure TEditForm.CreateInterface(aReference: Boolean);
begin

end;

end.

