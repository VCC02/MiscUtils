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


unit EmuVHDL_FT232HDecoder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, EmuVHDL_Types;


type
  TFT232HDecoder_States = (SIdle, SReading, SWriting);


procedure InitFT232HDecoderSignals;

procedure FT232HDecoder(
  FTClk: STD_LOGIC; //--60MHz
  var ADBus: Byte; //STD_LOGIC_VECTOR(7 downto 0);    inout
  RXF: STD_LOGIC; //--When high, do not read data from the FIFO.
  TXE: STD_LOGIC; //--When high, do not write data into the FIFO.
  out RD:  STD_LOGIC; //--Enables the current FIFO data byte to be driven onto D0...D7 when RD# goes low.
  out WR: STD_LOGIC; //--Enables the data byte on the D0...D7 pins to be written into the transmit FIFO buffer when WR# is low.
  out SIWU: STD_LOGIC; //--During normal operation (PWREN# = 0), if this pin is strobed low any data in the device RX buffer will be sent out over USB on the next Bulk - IN request from the drivers regardless of the pending packet size.
  out OE: STD_LOGIC; //--Output enable when low to drive data onto D0-D7

  out FIFO_Data_FTToFPGA: Byte; //STD_LOGIC_VECTOR(7 downto 0);
  FIFO_Data_FPGAToFT: Byte; //STD_LOGIC_VECTOR(7 downto 0);

  out FIFO_WE_FTToFPGA: STD_LOGIC; //connects inside the FPGA to the WriteEnable signal of a FIFO. It is active when there is data in FT.
  FIFO_RE_FPGAToFT: STD_LOGIC; //connects inside the FPGA to the ReadEnable signal of another FIFO. It is active when the FIFO has data for FT.
  out BusDriving: STD_LOGIC //Debugging signal, which is "1" when the decoder drives the data bus.
);


implementation


type
  TVFT232HDecoder = record
    OE_Temp: STD_LOGIC;
    RD_Temp: STD_LOGIC;
    WR_Temp: STD_LOGIC;
    State: TFT232HDecoder_States;

    OldRXF: STD_LOGIC;
    OldTXE: STD_LOGIC;

    FIFO_Data_FTToFPGA_Temp: Byte;
    FIFO_Data_FPGAToFT_Temp: Byte;
  end;


var
  FT232HDecoderSignals: TVFT232HDecoder;
  OldFTClk: STD_LOGIC = 0;


procedure InitFT232HDecoderSignals;
begin
  FT232HDecoderSignals.OE_Temp := 1;
  FT232HDecoderSignals.RD_Temp := 1;
  FT232HDecoderSignals.WR_Temp := 1;
  FT232HDecoderSignals.State := SIdle;
  FT232HDecoderSignals.OldRXF := 1;
  FT232HDecoderSignals.OldTXE := 1;
end;


procedure FT232HDecoder(
  FTClk: STD_LOGIC; //--60MHz
  var ADBus: Byte; //STD_LOGIC_VECTOR(7 downto 0);    inout
  RXF: STD_LOGIC; //--When high, do not read data from the FIFO.
  TXE: STD_LOGIC; //--When high, do not write data into the FIFO.
  out RD:  STD_LOGIC; //--Enables the current FIFO data byte to be driven onto D0...D7 when RD# goes low.
  out WR: STD_LOGIC; //--Enables the data byte on the D0...D7 pins to be written into the transmit FIFO buffer when WR# is low.
  out SIWU: STD_LOGIC; //--During normal operation (PWREN# = 0), if this pin is strobed low any data in the device RX buffer will be sent out over USB on the next Bulk - IN request from the drivers regardless of the pending packet size.
  out OE: STD_LOGIC; //--Output enable when low to drive data onto D0-D7

  out FIFO_Data_FTToFPGA: Byte; //STD_LOGIC_VECTOR(7 downto 0);
  FIFO_Data_FPGAToFT: Byte; //STD_LOGIC_VECTOR(7 downto 0);

  out FIFO_WE_FTToFPGA: STD_LOGIC; //connects inside the FPGA to the WriteEnable signal of a FIFO. It is active when there is data in FT.
  FIFO_RE_FPGAToFT: STD_LOGIC; //connects inside the FPGA to the ReadEnable signal of another FIFO. It is active when the FIFO has data for FT.
  out BusDriving: STD_LOGIC //Debugging signal, which is "1" when the decoder drives the data bus.
);
begin
  with FT232HDecoderSignals do
  begin
    OE := OE_Temp; // OE <= OE_Temp;
    RD := RD_Temp; // RD <= RD_Temp;
    WR := WR_Temp; // WR <= WR_Temp;
    SIWU := 1; //SIWU <= '1';

    //ADBus <= FIFO_Data_FPGAToFT_Temp when OE_Temp = '1' else (others => 'Z');
    if OE_Temp = 1 then               //OE is active low, which sets the databus to be output from the FT and input to decoder.
    begin
      ADBus := FIFO_Data_FPGAToFT;    //When high, the data bus is output from the decoder.
      BusDriving := 1;                //There can be two OE signals, one for setting the OE pin on FT, and the other for actually driving the bus, which may be delayed for safety reasons.
    end
    else
      BusDriving := 0;

    FIFO_Data_FTToFPGA := ADBus; // FIFOData_FTToFPGA <= ADBus;

    //process(FTClk)
    if (FTClk = 1) and (OldFTClk = 0) then //if FTClk'event and FTClk = '1' then
    begin
      OldRXF := RXF; // OldRXF <= RXF;
      OldTXE := TXE; // OldTXE <= TXE;

      if RXF = 0 then // if RXF = '0' then --receiving from USB
      begin
        State := SReading; // State <= SReading;
        OE_Temp := 0;  //OE_Temp <= '0';
        if OE_Temp = 0 then // if OE_Temp = '0' then
          RD_Temp := 0; // RD_Temp <= '0';
      end
      else
      begin
        State := SIdle; // State <= SIdle;
        OE_Temp := 1; // OE_Temp <= '1';
        RD_Temp := 1; // RD_Temp <= '1';
      end;

      if TXE = 0 then // if TXE = '0' then --receiving from USB
      begin
        if State = SIdle then
        begin
          State := SWriting; //State <= SWriting;
          OE_Temp := 1; // OE_Temp <= '1'; --make sure is '1'
          WR_Temp := 0; // WR_Temp <= '0';
        end;
      end
      else
      begin
        State := SIdle; // State <= SIdle;
        WR_Temp := 1; // WR_Temp <= '1';
      end;
    end; //if FTClk = 1

    OldFTClk := FTClk;

    if (FTClk = 1) and (OldFTClk = 0) then //if FTClk'event and FTClk = '1' then
    begin
      FIFO_Data_FTToFPGA := FIFO_Data_FTToFPGA_Temp; // FIFO_Data_FTToFPGA <= FIFO_Data_FTToFPGA_Temp;
      FIFO_Data_FPGAToFT_Temp := FIFO_Data_FPGAToFT; // FIFO_Data_FPGAToFT_Temp <= FIFO_Data_FPGAToFT;

      FIFO_WE_FTToFPGA := (not RXF and 1) and (not RD_Temp and 1); // FIFO_WE_FTToFPGA <= not RXF and not RD_Temp;
      FIFO_RE_FPGAToFT := (not TXE and 1) and (not WR_Temp and 1);
    end;
    //end process;

  end; //with
end;

end.

