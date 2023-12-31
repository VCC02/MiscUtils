{
    Copyright (C) 2023 VCC
    creation date: 04 Mar 2016
    initial release date: 31 Dec 2023

    author: VCC
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit MouseHintForm;

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TfrmMouseHint = class(TForm)
    pnlMoment: TPanel;
  private
    { Private declarations }
    FMoment: Integer;
    FMeasurementUnit: string;
    procedure SetMoment(Value: Integer);
    procedure SetMeasurementUnit(Value: string);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property Moment: Integer read FMoment write SetMoment;
    property MeasurementUnit: string read FMeasurementUnit write SetMeasurementUnit;
  end;

var
  frmMouseHint: TfrmMouseHint;


implementation

{$IFDEF FPC}
  {$R *.frm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

procedure TfrmMouseHint.SetMoment(Value: Integer);
begin
  if FMoment <> Value then
  begin
    FMoment := Value;
    pnlMoment.Caption := 'Moment: ' + IntToStr(FMoment) + FMeasurementUnit;
  end;
end;


procedure TfrmMouseHint.SetMeasurementUnit(Value: string);
begin
  if FMeasurementUnit <> Value then
  begin
    FMeasurementUnit := Value;
    pnlMoment.Caption := 'Moment: ' + IntToStr(FMoment) + FMeasurementUnit;
  end;
end;


constructor TfrmMouseHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMeasurementUnit := 'ns';
end;

end.
