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


unit WaveformFuncUtils;

interface

uses
  Graphics;


function TwoPowered(x: Cardinal): Cardinal;
function TwoPoweredInt64(x: Int64): Int64;
function Higher(a: Cardinal): Byte;
function Highest(a: Cardinal): Byte;
function InIntervalInteger(x, LeftEndpoint, RightEndpoint: Integer): Boolean;
function InIntervalExtended(x, LeftEndpoint, RightEndpoint: Extended): Boolean;
procedure Line(Cnv: TCanvas; x1, y1, x2, y2: Integer);


implementation


function TwoPowered(x: Cardinal): Cardinal;
begin
  Result := 1 shl x;
end;


function TwoPoweredInt64(x: Int64): Int64;
begin
  Result := 1 shl x;
end;


function Higher(a: Cardinal): Byte;
begin
  Result := (a shr 16) and $FF;
end;


function Highest(a: Cardinal): Byte;
begin
  Result := (a shr 24) and $FF;
end;


function InIntervalInteger(x, LeftEndpoint, RightEndpoint: Integer): Boolean;
begin
  Result := (x <= RightEndpoint) and (x >= LeftEndpoint);
end;


function InIntervalExtended(x, LeftEndpoint, RightEndpoint: Extended): Boolean;
begin
  Result := (x <= RightEndpoint) and (x >= LeftEndpoint);
end;


procedure Line(Cnv: TCanvas; x1, y1, x2, y2: Integer);
begin
  with Cnv do
  begin
    MoveTo(x1, y1);
    LineTo(x2, y2);
  end;
end;


end.
