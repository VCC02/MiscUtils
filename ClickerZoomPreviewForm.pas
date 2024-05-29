{
    Copyright (C) 2023 VCC
    creation date: Sep 2022
    initial release date: 27 Sep 2022

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


unit ClickerZoomPreviewForm;

{$IFDEF FPC}
  {$mode Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
    LCLIntf, LCLType,
  {$ELSE}
    Windows, //not sure about Delphi for Linux
  {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TfrmClickerZoomPreview }

  TfrmClickerZoomPreview = class(TForm)
    imgZoom: TImage;
    tmrStartup: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
  private
    FlblLeft: TLabel;
    FlblTop: TLabel;
    FlblRight: TLabel;
    FlblBottom: TLabel;
  public

  end;

  
procedure SetZoomContent(ACanvasHandle: THandle; ACanvasWidth, ACanvasHeight: Integer; AXCenter, AYCenter, AWinPosX, AWinPosY: Integer); overload;
procedure SetZoomContent(ABitmap: TBitmap; AXCenter, AYCenter, AWinPosX, AWinPosY: Integer); overload;

procedure ShowZoom(AWinPosX, AWinPosY: Integer);
procedure HideZoom;


var
  frmClickerZoomPreview: TfrmClickerZoomPreview;


implementation

{$IFDEF FPC}
  {$R *.frm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}


const
  clLightYellow = $00A0FFFF;
  clHashPink = $008888FF;
  clDottedLine = $000044FF;


//DrawWipeRect, WipeBitmap and WipeImage were copied from BitmapProcessing, to remove the dependency.
procedure DrawWipeRect(ACanvas: TCanvas; NewWidth, NewHeight: Integer);
begin
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clWhite;
  ACanvas.Pen.Color := clWhite;
  ACanvas.Rectangle(0, 0, NewWidth {- 1}, NewHeight {- 1});
end;


procedure WipeBitmap(ABitmap: TBitmap; NewWidth, NewHeight: Integer);
begin
  {$IFDEF FPC}
    ABitmap.Clear;
  {$ENDIF}

  ABitmap.Width := NewWidth;
  ABitmap.Height := NewHeight;
  DrawWipeRect(ABitmap.Canvas, NewWidth, NewHeight);
end;


procedure WipeImage(AImg: TImage; NewWidth, NewHeight: Integer);
begin
  {$IFDEF FPC}
    AImg.Picture.Clear;
  {$ENDIF}

  AImg.Width := NewWidth;
  AImg.Height := NewHeight;
  AImg.Picture.Bitmap.Width := AImg.Width;
  AImg.Picture.Bitmap.Height := AImg.Height;

  DrawWipeRect(AImg.Canvas, NewWidth, NewHeight);
  AImg.Repaint;
end;


procedure SetZoomContent(ACanvasHandle: THandle; ACanvasWidth, ACanvasHeight: Integer; AXCenter, AYCenter, AWinPosX, AWinPosY: Integer); overload;
var
  DestRect: TRect;
  CroppedBmp: TBitmap;
  Factor: Integer;
  ScaledDownDestWidth, ScaledDownDestHeight: Integer;
begin
  DestRect.Left := 0;
  DestRect.Top := 0;
  DestRect.Right := frmClickerZoomPreview.imgZoom.Width shl 3;
  DestRect.Bottom := frmClickerZoomPreview.imgZoom.Height shl 3;
  Factor := frmClickerZoomPreview.imgZoom.Width shr 4;

  WipeImage(frmClickerZoomPreview.imgZoom, frmClickerZoomPreview.imgZoom.Width, frmClickerZoomPreview.imgZoom.Height);

  CroppedBmp := TBitmap.Create;
  try
    CroppedBmp.Width := frmClickerZoomPreview.imgZoom.Width;
    CroppedBmp.Height := frmClickerZoomPreview.imgZoom.Height;
    WipeBitmap(CroppedBmp, CroppedBmp.Width, CroppedBmp.Height);

    ScaledDownDestWidth := CroppedBmp.Width shr 4;
    ScaledDownDestHeight := CroppedBmp.Height shr 4;

    BitBlt(CroppedBmp.Canvas.Handle,
           0,
           0,
           CroppedBmp.Width,
           CroppedBmp.Height,
           ACanvasHandle,
           AXCenter - ScaledDownDestWidth,
           AYCenter - ScaledDownDestHeight,
           SRCCOPY);

      //BitBlt param definition
      //HDC hdcDest, // handle to destination DC
      //int nXDest,  // x-coord of destination upper-left corner
      //int nYDest,  // y-coord of destination upper-left corner
      //int nWidth,  // width of destination rectangle
      //int nHeight, // height of destination rectangle
      //HDC hdcSrc,  // handle to source DC
      //int nXSrc,   // x-coordinate of source upper-left corner
      //int nYSrc,   // y-coordinate of source upper-left corner
      //DWORD dwRop  // raster operation code

    frmClickerZoomPreview.imgZoom.Canvas.StretchDraw(DestRect, CroppedBmp);
    frmClickerZoomPreview.imgZoom.Canvas.Pen.Color := clLightYellow;

    if AXCenter - ScaledDownDestWidth < 0 then
    begin
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsSolid;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := clLightYellow;
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle(0, 0, (ScaledDownDestWidth - AXCenter) shl 3, frmClickerZoomPreview.imgZoom.Height);

      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsFDiagonal;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := clHashPink;
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle(0, 0, (ScaledDownDestWidth - AXCenter) shl 3, frmClickerZoomPreview.imgZoom.Height);
    end;

    if AYCenter - ScaledDownDestHeight < 0 then
    begin
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsSolid;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := clLightYellow;
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle(0, 0, frmClickerZoomPreview.imgZoom.Height, (ScaledDownDestHeight - AYCenter) shl 3);

      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsFDiagonal;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := clHashPink;
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle(0, 0, frmClickerZoomPreview.imgZoom.Height, (ScaledDownDestHeight - AYCenter) shl 3);
    end;

    if ACanvasWidth - AXCenter < Factor then
    begin
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsSolid;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := clLightYellow;
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle((ACanvasWidth - AXCenter + Factor) shl 3, 0, CroppedBmp.Width, frmClickerZoomPreview.imgZoom.Height);

      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsFDiagonal;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := clHashPink;
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle((ACanvasWidth - AXCenter + Factor) shl 3, 0, CroppedBmp.Width, frmClickerZoomPreview.imgZoom.Height);
    end;

    if ACanvasHeight - AYCenter < Factor then
    begin
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsSolid;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := clLightYellow;
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle(0, (ACanvasHeight - AYCenter + Factor) shl 3, frmClickerZoomPreview.imgZoom.Width, CroppedBmp.Height);

      frmClickerZoomPreview.imgZoom.Canvas.Brush.Style := bsFDiagonal;
      frmClickerZoomPreview.imgZoom.Canvas.Brush.Color := clHashPink;
      frmClickerZoomPreview.imgZoom.Canvas.Rectangle(0, (ACanvasHeight - AYCenter + Factor) shl 3, frmClickerZoomPreview.imgZoom.Width, CroppedBmp.Height);
    end;
  finally
    CroppedBmp.Free;
  end;

  try
    frmClickerZoomPreview.Left := AWinPosX;
    frmClickerZoomPreview.Top := AWinPosY;

    Application.ProcessMessages;
    frmClickerZoomPreview.FlblLeft.Canvas.Pen.Color := clDottedLine;
    frmClickerZoomPreview.FlblLeft.Canvas.Pen.Style := psDot;
    frmClickerZoomPreview.FlblLeft.Canvas.Line(0, 0, 0, frmClickerZoomPreview.FlblLeft.Height);

    frmClickerZoomPreview.FlblRight.Canvas.Pen.Color := clDottedLine;
    frmClickerZoomPreview.FlblRight.Canvas.Pen.Style := psDot;
    frmClickerZoomPreview.FlblRight.Canvas.Line(0, 0, 0, frmClickerZoomPreview.FlblRight.Height);

    frmClickerZoomPreview.FlblTop.Canvas.Pen.Color := clDottedLine;
    frmClickerZoomPreview.FlblTop.Canvas.Pen.Style := psDot;
    frmClickerZoomPreview.FlblTop.Canvas.Line(0, 0, frmClickerZoomPreview.FlblTop.Width, 0);

    frmClickerZoomPreview.FlblBottom.Canvas.Pen.Color := clDottedLine;
    frmClickerZoomPreview.FlblBottom.Canvas.Pen.Style := psDot;
    frmClickerZoomPreview.FlblBottom.Canvas.Line(0, 0, frmClickerZoomPreview.FlblBottom.Width, 0);
  except
    on E: Exception do
      frmClickerZoomPreview.Color := clYellow + Random(64) shl 16;
  end;
end;


procedure SetZoomContent(ABitmap: TBitmap; AXCenter, AYCenter, AWinPosX, AWinPosY: Integer); overload;
begin
  SetZoomContent(ABitmap.Canvas.Handle, ABitmap.Width, ABitmap.Height, AXCenter, AYCenter, AWinPosX, AWinPosY);
end;


procedure ShowZoom(AWinPosX, AWinPosY: Integer);
begin
  frmClickerZoomPreview.Show;
end;


procedure HideZoom;
begin
  frmClickerZoomPreview.Hide;
end;

{ TfrmClickerZoomPreview }

procedure TfrmClickerZoomPreview.FormCreate(Sender: TObject);
begin
  tmrStartup.Enabled := True;
end;


procedure TfrmClickerZoomPreview.tmrStartupTimer(Sender: TObject);
const
  CLabelColor: TColor = $00FF8855;
begin
  tmrStartup.Enabled := False;

  FlblLeft := TLabel.Create(Self);
  FlblTop := TLabel.Create(Self);
  FlblRight := TLabel.Create(Self);
  FlblBottom := TLabel.Create(Self);

  FlblLeft := TLabel.Create(Self);
  FlblTop := TLabel.Create(Self);
  FlblRight := TLabel.Create(Self);
  FlblBottom := TLabel.Create(Self);

  FlblLeft.Parent := Self;
  FlblTop.Parent := Self;
  FlblRight.Parent := Self;
  FlblBottom.Parent := Self;

  FlblLeft.AutoSize := False;
  FlblTop.AutoSize := False;
  FlblRight.AutoSize := False;
  FlblBottom.AutoSize := False;

  FlblLeft.Caption := '';
  FlblTop.Caption := '';
  FlblRight.Caption := '';
  FlblBottom.Caption := '';

  FlblLeft.Color := CLabelColor;
  FlblTop.Color := CLabelColor;
  FlblRight.Color := CLabelColor;
  FlblBottom.Color := CLabelColor;

  FlblLeft.Left := imgZoom.Left + imgZoom.Width shr 1 - 1;
  FlblLeft.Top := imgZoom.Top;
  FlblLeft.Width := 1;
  FlblLeft.Height := imgZoom.Height;

  FlblTop.Left := imgZoom.Left;
  FlblTop.Top := imgZoom.Top + imgZoom.Height shr 1 - 1;
  FlblTop.Width := imgZoom.Width;
  FlblTop.Height := 1;

  FlblRight.Left := imgZoom.Left + imgZoom.Width shr 1 + 8 - 1;
  FlblRight.Top := imgZoom.Top;
  FlblRight.Width := 1;
  FlblRight.Height := imgZoom.Height;

  FlblBottom.Left := imgZoom.Left;
  FlblBottom.Top := imgZoom.Top + imgZoom.Height shr 1 + 8 - 1;
  FlblBottom.Width := imgZoom.Width;
  FlblBottom.Height := 1;

  FlblLeft.Transparent := False;
  FlblTop.Transparent := False;
  FlblRight.Transparent := False;
  FlblBottom.Transparent := False;

  FlblLeft.Visible := True;
  FlblTop.Visible := True;
  FlblRight.Visible := True;
  FlblBottom.Visible := True;

  FlblLeft.BringToFront;
  FlblTop.BringToFront;
  FlblRight.BringToFront;
  FlblBottom.BringToFront;
end;

end.
