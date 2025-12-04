{
    Copyright (C) 2025 VCC
    creation date: 03 Dec 2025
    initial release date: 04 Dec 2025

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


unit EmuVHDL_FT232H;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, EmuVHDL_Types;


procedure InitFT232HSignals;

procedure FT232H(
  IntOsc120MHz: STD_LOGIC; //120MHz  --This signal does not exist on a pin. It is for simulation only.
  out CLKOUT: STD_LOGIC; //--60MHz
  var ADBus: Byte; //STD_LOGIC_VECTOR(7 downto 0);    inout
  out RXF: STD_LOGIC; //--When high, do not read data from the FIFO.
  out TXE: STD_LOGIC; //--When high, do not write data into the FIFO.
  RD: STD_LOGIC;
  WR: STD_LOGIC;
  SIWU: STD_LOGIC;
  OE: STD_LOGIC;
  out BusDriving: STD_LOGIC //Debugging signal, which is "1" when the FT chip drives the data bus. This signal does not exist on a pin.
);


implementation


type
  TVFT232H = record
    EventCounter: Word; //STD_LOGIC_VECTOR(9 downto 0) := CONV_STD_LOGIC_VECTOR(0, 10);
    RXF_Temp: STD_LOGIC;
    TXE_Temp: STD_LOGIC;
    CLKOUT_Temp: STD_LOGIC;

    DataForFPGA: Byte; //STD_LOGIC_VECTOR(7 downto 0) := CONV_STD_LOGIC_VECTOR(30, 8);
  end;

var
  FT232HSignals: TVFT232H;
  OldIntOsc120MHz: STD_LOGIC = 1;
  OldCLKOUT_Temp: STD_LOGIC = 0;

procedure InitFT232HSignals;
begin
  FT232HSignals.EventCounter := 0;
  FT232HSignals.RXF_Temp := 1;
  FT232HSignals.TXE_Temp := 1;
  FT232HSignals.CLKOUT_Temp := 0;
  FT232HSignals.DataForFPGA := 0;
end;


procedure FT232H(
  IntOsc120MHz: STD_LOGIC; //120MHz
  out CLKOUT: STD_LOGIC; //--60MHz
  var ADBus: Byte; //STD_LOGIC_VECTOR(7 downto 0);    inout
  out RXF: STD_LOGIC; //--When high, do not read data from the FIFO.
  out TXE: STD_LOGIC; //--When high, do not write data into the FIFO.
  RD: STD_LOGIC;
  WR: STD_LOGIC;
  SIWU: STD_LOGIC;
  OE: STD_LOGIC;
  out BusDriving: STD_LOGIC //Debugging signal, which is "1" when the FT chip drives the data bus. This signal does not exist on a pin.
);
begin
  with FT232HSignals do
  begin
    //process(IntOsc120MHz)
    if (IntOsc120MHz = 0) and (OldIntOsc120MHz = 0) then // if IntOsc120MHz'event and IntOsc120MHz = '1' then
    begin
      CLKOUT_Temp := not CLKOUT_Temp and 1; // CLKOUT_Temp <= not CLKOUT_Temp;
      EventCounter := EventCounter + 1; // EventCounter <= EventCounter + 1;
    end;

    OldIntOsc120MHz := IntOsc120MHz;

    //if IntOsc120MHz = 0 then //  if IntOsc120MHz'event and IntOsc120MHz = '1' then
    if (CLKOUT_Temp = 1) and (OldCLKOUT_Temp = 0) then
    begin
      if EventCounter = 5 then
        RXF_Temp := 0;

      if EventCounter = 21 then
        RXF_Temp := 1;


      if EventCounter = 25 then
        TXE_Temp := 0;

      if EventCounter = 41 then
        TXE_Temp := 1;


      if EventCounter = 55 then
        TXE_Temp := 0;

      if EventCounter = 93 then
        TXE_Temp := 1;


      if EventCounter = 105 then
        RXF_Temp := 0;

      if EventCounter = 121 then
        RXF_Temp := 1;

      if EventCounter = 173 then
        RXF_Temp := 0;

      if EventCounter = 197 then
        RXF_Temp := 1;

      if EventCounter = 205 then
        RXF_Temp := 0;

      if EventCounter = 221 then
        RXF_Temp := 1;


      if EventCounter = 275 then //both signals fall simultaneously
      begin
        RXF_Temp := 0;
        TXE_Temp := 0;
      end;

      if EventCounter = 295 then
      begin
        RXF_Temp := 1;
        TXE_Temp := 1;
      end;


      if EventCounter = 301 then //"USB To FPGA" -> interupts "FPGA to USB"
        TXE_Temp := 0;

      if EventCounter = 321 then
        RXF_Temp := 0;

      if EventCounter = 341 then
        RXF_Temp := 1;

      if EventCounter = 361 then
        TXE_Temp := 1;


      if EventCounter = 401 then //"USB To FPGA" -> is interrupted by "FPGA to USB"
        RXF_Temp := 0;

      if EventCounter = 421 then
        TXE_Temp := 0;

      if EventCounter = 441 then
        TXE_Temp := 1;

      if EventCounter = 461 then
        RXF_Temp := 1;


      if EventCounter = 501 then //"USB To FPGA" -> is interrupted by "FPGA to USB", but RXF ends before TXE
        RXF_Temp := 0;

      if EventCounter = 521 then
        TXE_Temp := 0;

      if EventCounter = 541 then
        RXF_Temp := 1;

      if EventCounter = 561 then
        TXE_Temp := 1;


      if EventCounter = 601 then //"USB To FPGA" -> interrupts "FPGA to USB", but TXE ends before RXF
        TXE_Temp := 0;

      if EventCounter = 621 then
        RXF_Temp := 0;

      if EventCounter = 641 then
        TXE_Temp := 1;

      if EventCounter = 661 then
        RXF_Temp := 1;
    end; //end process

    //process(CLKOUT_Temp)
    if (CLKOUT_Temp = 1) and (OldCLKOUT_Temp = 0) then // if CLKOUT_Temp'event and CLKOUT_Temp = '1' then
    begin
      if RD = 0 then
        DataForFPGA := DataForFPGA + 1;
    end;

    OldCLKOUT_Temp := CLKOUT_Temp;

    CLKOUT := CLKOUT_Temp; // CLKOUT <= CLKOUT_Temp;
    RXF := RXF_Temp; // RXF <= RXF_Temp;
    TXE := TXE_Temp; // TXE <= TXE_Temp;

    //ADBus <= DataForFPGA when OE = '0' else (others => 'Z');
    if OE = 0 then
    begin
      ADBus := DataForFPGA;
      BusDriving := 1;
    end
    else
      BusDriving := 0;
  end; //with
end;

end.

