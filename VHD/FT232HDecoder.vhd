----------------------------------------------------------------------------------
-- Company: 
-- Engineer: VCC
-- 
-- Create Date:    12:40:59 12/04/2013 (04 Dec 2013)
-- Initial release date: 04 Dec 2025
-- Design Name: 
-- Module Name:    FT232HDecoder - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Revision 0.02 - Dec 2025 - multiple fixes
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

---- Uncomment the following library declaration if instantiating
---- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity FT232HDecoder is
    Port (FTClk: in STD_LOGIC; --60MHz
          ADBus: inout STD_LOGIC_VECTOR(7 downto 0);
          RXF: in STD_LOGIC; --Active low. When low, there is data in FT FIFO.
          TXE: in STD_LOGIC; --Active low. When low, there is room for data in FT FIFO.
          RD: out STD_LOGIC; --
          WR: out STD_LOGIC; --
          SIWU: out STD_LOGIC; --
          OE: out STD_LOGIC; --When low, the FT drives the data bus.     

          FIFO_Data_FTToFPGA: out STD_LOGIC_VECTOR(7 downto 0);
          FIFO_Data_FPGAToFT: in STD_LOGIC_VECTOR(7 downto 0);
          
          FIFO_WE_FTToFPGA: out STD_LOGIC; --connects inside the FPGA to the WriteEnable signal of a FIFO. It is active when there is data in FT.
          FIFO_RE_FPGAToFT: in STD_LOGIC --connects inside the FPGA to the ReadEnable signal of another FIFO. It is active when the FIFO has data for FT.
          );
end FT232HDecoder;

architecture Behavioral of FT232HDecoder is

  type TStates is (SIdle, SReading, SWriting);

  signal OE_Temp: STD_LOGIC := '1';
  signal RD_Temp: STD_LOGIC := '1';
  signal WR_Temp: STD_LOGIC := '1';
  signal State: TStates := SIdle;
  
  signal OldRXF: STD_LOGIC := '1';
  signal OldTXE: STD_LOGIC := '1';
  
  signal FIFO_Data_FTToFPGA_Temp: STD_LOGIC_VECTOR(7 downto 0);
  signal FIFO_Data_FPGAToFT_Temp: STD_LOGIC_VECTOR(7 downto 0);


begin


  OE <= OE_Temp;
  RD <= RD_Temp;
  WR <= WR_Temp;
  SIWU <= '1';


  ADBus <= FIFO_Data_FPGAToFT_Temp when OE_Temp = '1' else (others => 'Z'); 
  FIFO_Data_FTToFPGA_Temp <= ADBus;

  process(FTClk)
  begin
    if FTClk'event and FTClk = '1' then
      OldRXF <= RXF;
      OldTXE <= TXE;
      
      if RXF = '0' then --receiving from USB
        State <= SReading;
        OE_Temp <= '0';
        if OE_Temp = '0' then
          RD_Temp <= '0';
        end if;
      else
        State <= SIdle;
        OE_Temp <= '1';
        RD_Temp <= '1';
      end if;
      
      if TXE = '0' then --receiving from USB
        if State = SIdle then
          State <= SWriting;
          OE_Temp <= '1'; --make sure is '1'
          WR_Temp <= '0';
        end if;
      else
        State <= SIdle;
        WR_Temp <= '1';
      end if;
    end if;
    
    if FTClk'event and FTClk = '1' then
      FIFO_Data_FTToFPGA <= FIFO_Data_FTToFPGA_Temp;
      FIFO_Data_FPGAToFT_Temp <= FIFO_Data_FPGAToFT;
      
      FIFO_WE_FTToFPGA <= not RXF and not RD_Temp;
      FIFO_RE_FPGAToFT <= not TXE and not WR_Temp;
    end if;
  end process;

end Behavioral;
