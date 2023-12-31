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


//generates testbench stimuli
unit TestBenchGenerator;

interface

uses
  WaveformUtils;

function GenerateTestBenchFromSignals(AllTBWSignals: TPSignalArr; UUTName: string): string;


implementation


uses
  SysUtils, Classes;


//ToDo
//signal name is wrong (it should be only the part after dot)
//add UUT component definition and its instance


function GenerateTestBenchFromSignals(AllTBWSignals: TPSignalArr; UUTName: string): string;
const
  CTimeUnit: string = ' ns';
var
  AStringList: TStringList;
  i, j: Integer;
  ClockDuration, ClockLoopNumber: Integer;
  SigName, SigSizeStr: string;
  WaitAmount: Integer;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Add('library IEEE;');
    AStringList.Add('use IEEE.STD_LOGIC_1164.ALL;');
    AStringList.Add('use IEEE.STD_LOGIC_ARITH.ALL;');
    AStringList.Add('use IEEE.STD_LOGIC_UNSIGNED.ALL;');
    AStringList.Add('USE IEEE.STD_LOGIC_TEXTIO.ALL;');
    AStringList.Add('USE STD.TEXTIO.ALL;');
    AStringList.Add('');
    AStringList.Add('ENTITY ' + UUTName + '_TBW IS');
    AStringList.Add('END ' + UUTName + '_TBW;');
    AStringList.Add('');
    AStringList.Add('ARCHITECTURE testbench_arch OF ' + UUTName + '_TBW IS');
    AStringList.Add('  -- COMPONENT ' + UUTName);
    AStringList.Add('');
    AStringList.Add(' --END COMPONENT;');
    AStringList.Add('');

    for i := 0 to Length(AllTBWSignals) - 1 do
    begin
      SigName := StringReplace(AllTBWSignals[i].SignalName, '.', '_', [rfReplaceAll]);

      if AllTBWSignals[i].Size = 1 then
        AStringList.Add('signal ' + SigName + ': STD_LOGIC := ''0'';')
      else
        AStringList.Add('signal ' + SigName + ': STD_LOGIC_VECTOR(' + IntToStr(AllTBWSignals[i].Size - 1) + ' downto 0) := (others => ''0'');');
    end;

    AStringList.Add('');
    AStringList.Add('');
    AStringList.Add('begin');
    AStringList.Add('  --UUT: ' + UUTName);
    AStringList.Add('  --PORT MAP (');
    AStringList.Add('  --);');
    AStringList.Add('');

    for i := 0 to Length(AllTBWSignals) - 1 do
    begin
      if i > 0 then
        AStringList.Add('');
        
      SigName := StringReplace(AllTBWSignals[i].SignalName, '.', '_', [rfReplaceAll]);
      SigSizeStr := IntToStr(AllTBWSignals[i].Size);
      
      AStringList.Add('  process');
      AStringList.Add('    variable IterationCounter: Integer := 0;');
      AStringList.Add('  begin');

      ClockLoopNumber := 0;
      for j := 0 to Length(AllTBWSignals[i].SignalEvents) - 1 do
      begin
        if j > 0 then
          AStringList.Add('');

        WaitAmount := -1;  

        if j = 0 then
          WaitAmount := AllTBWSignals[i].SignalEvents[j].Moment
        else
          if AllTBWSignals[i].SignalEvents[j - 1].PatternTypeAfterEvent <> ptClock then
            WaitAmount := AllTBWSignals[i].SignalEvents[j].Moment - AllTBWSignals[i].SignalEvents[j - 1].Moment;

        if WaitAmount > -1 then
          AStringList.Add('    WAIT for ' + IntToStr(WaitAmount) + CTimeUnit + '; -- Current time: ' + IntToStr(AllTBWSignals[i].SignalEvents[j].Moment) + CTimeUnit);   //moment of start of clock

        try
          if AllTBWSignals[i].Size = 1 then
          begin
            case AllTBWSignals[i].SignalEvents[j].PatternTypeAfterEvent of
              ptClock:
              begin
                Inc(ClockLoopNumber);

                if j = Length(AllTBWSignals[i].SignalEvents) - 1 then
                  ClockDuration := MaxInt
                else
                  ClockDuration := AllTBWSignals[i].SignalEvents[j + 1].Moment - AllTBWSignals[i].SignalEvents[j].Moment;

                AStringList.Add('    IterationCounter := 0;');
                AStringList.Add('    CLOCK_LOOP_' + IntToStr(ClockLoopNumber) + ': LOOP');

                if ClockDuration <> MaxInt then
                  AStringList.Add('      exit when IterationCounter >= ' + IntToStr(ClockDuration) + ';');

                AStringList.Add('      ' + SigName + ' <= ''1'';');
                AStringList.Add('      WAIT for ' + IntToStr(AllTBWSignals[i].SignalEvents[j].ClockPatternAfterEvent.MidTransitionOffset) + CTimeUnit + ';');

                if ClockDuration <> MaxInt then
                  AStringList.Add('      IterationCounter := IterationCounter + ' + IntToStr(AllTBWSignals[i].SignalEvents[j].ClockPatternAfterEvent.MidTransitionOffset) + ';');

                AStringList.Add('      ' + SigName + ' <= ''0'';');
                AStringList.Add('      WAIT for ' + IntToStr(AllTBWSignals[i].SignalEvents[j].ClockPatternAfterEvent.EndTransitionOffset - AllTBWSignals[i].SignalEvents[j].ClockPatternAfterEvent.MidTransitionOffset) + CTimeUnit + ';');

                if ClockDuration <> MaxInt then
                  AStringList.Add('      IterationCounter := IterationCounter + ' + IntToStr(AllTBWSignals[i].SignalEvents[j].ClockPatternAfterEvent.EndTransitionOffset - AllTBWSignals[i].SignalEvents[j].ClockPatternAfterEvent.MidTransitionOffset) + ';');

                AStringList.Add('    end LOOP CLOCK_LOOP_' + IntToStr(ClockLoopNumber) +';');
              end; //ptClock
                                                 
              ptDiscrete:
              begin
                case AllTBWSignals[i].ValueType of
                  sutNumeric: AStringList.Add('    ' + SigName + ' <= ''' + IntToStr(NumericSignalToInteger(AllTBWSignals[i].SignalEvents[j].ValueAfterEvent) and 1) + ''';');
                  sutSTD_LOGIC: AStringList.Add('    ' + SigName + ' <= ''' + ArrayOfTSTD_LOGIC_ToString(AllTBWSignals[i].SignalEvents[j].ValueAfterEvent.STD_LOGIC_Value) + ''';');
                  sutOrdinal: AStringList.Add('    ' + SigName + ' <= ''' + IntToStr(AllTBWSignals[i].SignalEvents[j].ValueAfterEvent.OrdinalValue and 1) + ''';');
                  sutInt32: AStringList.Add('    ' + SigName + ' <= ''' + IntToStr(AllTBWSignals[i].SignalEvents[j].ValueAfterEvent.Int32Value and 1) + ''';');
                  sutInt64: AStringList.Add('    ' + SigName + ' <= ''' + IntToStr(AllTBWSignals[i].SignalEvents[j].ValueAfterEvent.Int64Value and 1) + ''';');
                end;
                //AStringList.Add('    ' + SigName + ' <= ''' + IntToStr(AllTBWSignals[i].SignalEvents[j].ValueAfterEvent.NumericValue[0]) + ''';');
              end;
            end; //case

          end  //Size = 1
          else
          begin
            case AllTBWSignals[i].ValueType of
              sutNumeric: AStringList.Add('      ' + SigName + ' <= CONV_STD_LOGIC_VECTOR(' + IntToStr(NumericSignalToInteger(AllTBWSignals[i].SignalEvents[j].ValueAfterEvent)) + ', ' + SigSizeStr + ');');
              sutSTD_LOGIC: AStringList.Add('      ' + SigName + ' <= "' + ArrayOfTSTD_LOGIC_ToString(AllTBWSignals[i].SignalEvents[j].ValueAfterEvent.STD_LOGIC_Value) + '";');
              sutOrdinal: AStringList.Add('      ' + SigName + ' <= CONV_STD_LOGIC_VECTOR(' + IntToStr(AllTBWSignals[i].SignalEvents[j].ValueAfterEvent.OrdinalValue) + ', ' + SigSizeStr + ');');
              sutInt32: AStringList.Add('      ' + SigName + ' <= CONV_STD_LOGIC_VECTOR(' + IntToStr(AllTBWSignals[i].SignalEvents[j].ValueAfterEvent.Int32Value) + ', ' + SigSizeStr + ');');
              sutInt64: AStringList.Add('      ' + SigName + ' <= CONV_STD_LOGIC_VECTOR(' + IntToStr(AllTBWSignals[i].SignalEvents[j].ValueAfterEvent.Int64Value) + ', ' + SigSizeStr + ');');
            end;  
          end;  //Size > 1
        except
          on E: Exception do
            AStringList.Add('    --' + SigName + ' <= Exception: ' + E.Message + ' ...  SignalIndex = ' + IntToStr(i) + '  EventIndex = ' + IntToStr(j));
        end;
      end; //for j

      AStringList.Add('  end process;');
      AStringList.Add('');
    end;  //for i

    AStringList.Add('end testbench_arch;');
    Result := AStringList.Text;
  finally
    AStringList.Free
  end;
end;

end.
