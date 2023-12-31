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


unit WaveformFileUtils;

interface

uses
  SysUtils, WaveformUtils;


function GetUnivFormatSettings: TFormatSettings;
procedure LoadSignalsFromIni(AIniFileName: TFileName; var Signals: TPSignalArr; var NodeHeights: TNodeHeightsArr);
procedure SaveSignalsToIni(AIniFileName: TFileName; var Signals: TPSignalArr; var NodeHeights: TNodeHeightsArr);


implementation

uses
  Windows, IniFiles, Classes, Forms;

function GetUnivFormatSettings: TFormatSettings;
begin
  Result.CurrencyFormat := 0;
  Result.NegCurrFormat := 0;
  Result.ThousandSeparator := ',';
  Result.DecimalSeparator := '.';
  Result.CurrencyDecimals := 2;
  Result.DateSeparator := '/';
  Result.TimeSeparator := ':';
  Result.ListSeparator := ',';
  Result.CurrencyString := '$';
  Result.ShortDateFormat := 'yyyy-MM-dd'; //'M/d/yyyy';
  Result.LongDateFormat := 'yyyy, MMMM dd, dddd'; //'dddd, MMMM dd, yyyy';
  Result.TimeAMString := 'AM';
  Result.TimePMString := 'PM';
  Result.ShortTimeFormat := 'hh:mm'; //'h:mm AMPM';
  Result.LongTimeFormat := 'hh:mm:ss'; //'h:mm:ss AMPM';
  Result.TwoDigitYearCenturyWindow := 65044;

  Result.ShortMonthNames[1] := 'Jan';
  Result.ShortMonthNames[2] := 'Feb';
  Result.ShortMonthNames[3] := 'Mar';
  Result.ShortMonthNames[4] := 'Apr';
  Result.ShortMonthNames[5] := 'May';
  Result.ShortMonthNames[6] := 'Jun';
  Result.ShortMonthNames[7] := 'Jul';
  Result.ShortMonthNames[8] := 'Aug';
  Result.ShortMonthNames[9] := 'Sep';
  Result.ShortMonthNames[10] := 'Oct';
  Result.ShortMonthNames[11] := 'Nov';
  Result.ShortMonthNames[12] := 'Dec';

  Result.LongMonthNames[1] := 'January';
  Result.LongMonthNames[2] := 'February';
  Result.LongMonthNames[3] := 'March';
  Result.LongMonthNames[4] := 'April';
  Result.LongMonthNames[5] := 'May';
  Result.LongMonthNames[6] := 'June';
  Result.LongMonthNames[7] := 'July';
  Result.LongMonthNames[8] := 'August';
  Result.LongMonthNames[9] := 'September';
  Result.LongMonthNames[10] := 'October';
  Result.LongMonthNames[11] := 'November';
  Result.LongMonthNames[12] := 'December';

  Result.ShortDayNames[1] := 'Sun';
  Result.ShortDayNames[2] := 'Mon';
  Result.ShortDayNames[3] := 'Tue';
  Result.ShortDayNames[4] := 'Wed';
  Result.ShortDayNames[5] := 'Thu';
  Result.ShortDayNames[6] := 'Fri';
  Result.ShortDayNames[7] := 'Sat';

  Result.LongDayNames[1] := 'Sunday';
  Result.LongDayNames[2] := 'Monday';
  Result.LongDayNames[3] := 'Tuesday';
  Result.LongDayNames[4] := 'Wednesday';
  Result.LongDayNames[5] := 'Thursday';
  Result.LongDayNames[6] := 'Friday';
  Result.LongDayNames[7] := 'Saturday';
end;


procedure LoadSignalsFromIni(AIniFileName: TFileName; var Signals: TPSignalArr; var NodeHeights: TNodeHeightsArr);
var
  Ini: TMemIniFile;
  i, j, k, SignalCount, EvLen, SignalBytesLen: Integer;
  SignalIniId, SignalEventIniId, s: string;
  FFormatSettings: TFormatSettings;
begin
  ClearArrayOfSignals(Signals);

  if not FileExists(AIniFileName) then
    Exit;

  FFormatSettings := GetUnivFormatSettings; 

  Ini := TMemIniFile.Create(AIniFileName);
  try
    SignalCount := Ini.ReadInteger('Signals', 'Count', 0); //SignalCount = signals to be displayed
    SetLength(Signals, SignalCount);
    SetLength(NodeHeights, SignalCount);

    for i := 0 to SignalCount - 1 do
    begin
      New(Signals[i]); //alocate signal
      SignalIniId := 'Signal_' + IntToStr(i);
      Signals[i].SignalName := Ini.ReadString('Signals', SignalIniId + '.Name', SignalIniId + '.Name');
      Signals[i].SignalValueSource := Ini.ReadInteger('Signals', SignalIniId + '.ValueSource', 0);
      Signals[i].SignalDirection := Ini.ReadInteger('Signals', SignalIniId + '.Direction', 0);
      Signals[i].OwnerCircuit := Ini.ReadString('Signals', SignalIniId + '.OwnerCircuit', '');
      Signals[i].ValueType := TSignalUsedType(Ini.ReadInteger('Signals', SignalIniId + '.ValueType', Integer(sutInt32)));

      EvLen := Ini.ReadInteger('Signals', SignalIniId + '.EvLen', 0);
      SetLength(Signals[i].SignalEvents, EvLen);

      for j := 0 to EvLen - 1 do
      begin
        SignalEventIniId := SignalIniId + '.Ev_' + IntToStr(j);

        Signals[i].SignalEvents[j].PatternTypeAfterEvent := TPatternType(Ini.ReadInteger('Signals', SignalEventIniId + '.PatternTypeAfterEvent', 0));

        case Signals[i].SignalEvents[j].PatternTypeAfterEvent of
          ptDiscrete:
          begin
            case Signals[i].ValueType of
              sutNumeric:
              begin
                SignalBytesLen := Ini.ReadInteger('Signals', SignalEventIniId + '.BytesLen', 0);
                SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.NumericValue, SignalBytesLen);

                for k := 0 to SignalBytesLen - 1 do
                  Signals[i].SignalEvents[j].ValueAfterEvent.NumericValue[k] := Ini.ReadInteger('Signals', SignalEventIniId + '.Byte' + IntToStr(k), 0);

                SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.STD_LOGIC_Value, 0);

                Signals[i].SignalEvents[j].ValueAfterEvent.OrdinalValue := 0;
                Signals[i].SignalEvents[j].ValueAfterEvent.Int32Value := 0;
                Signals[i].SignalEvents[j].ValueAfterEvent.Int64Value := 0;
              end; //sutNumeric

              sutSTD_LOGIC:
              begin
                SignalBytesLen := Ini.ReadInteger('Signals', SignalEventIniId + '.BytesLen', 0);
                SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.STD_LOGIC_Value, SignalBytesLen);

                for k := 0 to SignalBytesLen - 1 do
                  Signals[i].SignalEvents[j].ValueAfterEvent.STD_LOGIC_Value[k] := TSTD_LOGIC(Ini.ReadInteger('Signals', SignalEventIniId + '.STD_LOGIC_Byte' + IntToStr(k), Integer(TSL0)));

                SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.NumericValue, 0);

                Signals[i].SignalEvents[j].ValueAfterEvent.OrdinalValue := 0;
                Signals[i].SignalEvents[j].ValueAfterEvent.Int32Value := 0;
                Signals[i].SignalEvents[j].ValueAfterEvent.Int64Value := 0;
              end;

              sutOrdinal:
              begin
                SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.NumericValue, 0);
                SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.STD_LOGIC_Value, 0);
                Signals[i].SignalEvents[j].ValueAfterEvent.OrdinalValue := Ini.ReadInteger('Signals', SignalEventIniId + '.OrdinalValue', 0);
                Signals[i].SignalEvents[j].ValueAfterEvent.Int32Value := 0;
                Signals[i].SignalEvents[j].ValueAfterEvent.Int64Value := 0;
              end;

              sutInt32:
              begin
                SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.NumericValue, 0);
                SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.STD_LOGIC_Value, 0);
                Signals[i].SignalEvents[j].ValueAfterEvent.OrdinalValue := 0;
                Signals[i].SignalEvents[j].ValueAfterEvent.Int32Value := Ini.ReadInteger('Signals', SignalEventIniId + '.Int32Value', 0);
                Signals[i].SignalEvents[j].ValueAfterEvent.Int64Value := 0;
              end;

              sutInt64:
              begin
                SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.NumericValue, 0);
                SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.STD_LOGIC_Value, 0);
                s := Ini.ReadString('Signals', SignalEventIniId + '.Int64Value', '0');
                Signals[i].SignalEvents[j].ValueAfterEvent.OrdinalValue := 0;
                Signals[i].SignalEvents[j].ValueAfterEvent.Int32Value := 0;
                Signals[i].SignalEvents[j].ValueAfterEvent.Int64Value := StrToInt64Def(s, 0);
              end

              else
              begin
                MessageBox(0, 'Undefined signal value type', PChar(Application.Title), MB_ICONERROR);
                Exit;
              end;
            end;  //case
          end;// ptDiscrete

          ptClock:
          begin
            SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.NumericValue, 1); //not sure if needed
            Signals[i].SignalEvents[j].ClockPatternAfterEvent.MidTransitionOffset := Ini.ReadInteger('Signals', SignalEventIniId + '.ClockMidTransitionOffset', 1);
            Signals[i].SignalEvents[j].ClockPatternAfterEvent.EndTransitionOffset := Ini.ReadInteger('Signals', SignalEventIniId + '.ClockEndTransitionOffset', 2);
          end;
        end; //case PatternTypeAfterEvent

        Signals[i].SignalEvents[j].Moment := Ini.ReadInteger('Signals', SignalEventIniId + '.Moment', 0);

        s := Ini.ReadString('Signals', SignalEventIniId + '.Jitter', '0.0');
        Signals[i].SignalEvents[j].Jitter := StrToFloat(s, FFormatSettings);
        Signals[i].SignalEvents[j].JitterType := TJitterType(Ini.ReadInteger('Signals', SignalEventIniId + '.JitterType', Integer(jtPositiveOnly)));
        Signals[i].SignalEvents[j].EditingEvent := False;
        Signals[i].SignalEvents[j].CauseOfEvent := Ini.ReadInteger('Signals', SignalEventIniId + '.CauseOfEvent', 0);
        Signals[i].SignalEvents[j].EventConditionIndex := Ini.ReadInteger('Signals', SignalEventIniId + '.EventConditionIndex', 0);
        Signals[i].SignalEvents[j].EventType := Ini.ReadInteger('Signals', SignalEventIniId + '.EventType', 0);
        Signals[i].SignalEvents[j].EventInfo := Ini.ReadString('Signals', SignalEventIniId + '.EventInfo', '');
        Signals[i].SignalEvents[j].Highlighted := Ini.ReadBool('Signals', SignalEventIniId + '.EventInfo', False);
      end; //for j

      Signals[i].SignalOrdinalValueTypeName := Ini.ReadString('Signals', SignalIniId + '.OrdinalValueTypeName', '');
      Signals[i].Size := Ini.ReadInteger('Signals', SignalIniId + '.Size', 0);
      Signals[i].AddedBy := TSignalAddedBy(Ini.ReadInteger('Signals', SignalIniId + '.AddedBy', 0));
      Signals[i].Editing := False;
      Signals[i].SynchronizedBySignal := Ini.ReadString('Signals', SignalIniId + '.SynchronizedBySignal', '');
      Signals[i].SynchronizedByCircuit := Ini.ReadString('Signals', SignalIniId + '.SynchronizedByCircuit', '');

      Signals[i].DrawType := TDrawType(Ini.ReadInteger('Signals', SignalIniId + '.DrawType', Integer(dtNumeric)));

      SignalBytesLen := Ini.ReadInteger('Signals', SignalIniId + '.NumericBytesLen.MinValue', 0);
      SetLength(Signals[i].NumericMinValue, SignalBytesLen);
      for j := 0 to SignalBytesLen - 1 do
        Signals[i].NumericMinValue[j] := Ini.ReadInteger('Signals', SignalIniId + '.NumericBytes.MinValue.Byte_' + IntToStr(j), 0);

      SignalBytesLen := Ini.ReadInteger('Signals', SignalIniId + '.NumericBytesLen.MaxValue', 0);
      SetLength(Signals[i].NumericMaxValue, SignalBytesLen);
      for j := 0 to SignalBytesLen - 1 do
        Signals[i].NumericMaxValue[j] := Ini.ReadInteger('Signals', SignalIniId + '.NumericBytes.MaxValue.Byte_' + IntToStr(j), 0);

      Signals[i].Int32MinValue := Ini.ReadInteger('Signals', SignalIniId + '.Int32MinValue', 0);
      Signals[i].Int32MaxValue := Ini.ReadInteger('Signals', SignalIniId + '.Int32MaxValue', 0);

      Signals[i].Int64MinValue := StrToInt64Def(Ini.ReadString('Signals', SignalIniId + '.Int64MinValue', '0'), 0);
      Signals[i].Int64MaxValue := StrToInt64Def(Ini.ReadString('Signals', SignalIniId + '.Int64MaxValue', '0'), 0);

      Signals[i].MinValueColor := Ini.ReadInteger('Signals', SignalIniId + '.MinValueColor', 0);
      Signals[i].MaxValueColor := Ini.ReadInteger('Signals', SignalIniId + '.MaxValueColor', 0);

      ////////
    end; //for i

    for i := 0 to SignalCount - 1 do
    begin
      SignalIniId := 'Signal_' + IntToStr(i);
      NodeHeights[i] := Ini.ReadInteger('Signals', SignalIniId + '.NodeHeight', CMinNodeHeight);
    end;
  finally
    Ini.Free;
  end;
end;


procedure SaveSignalsToIni(AIniFileName: TFileName; var Signals: TPSignalArr; var NodeHeights: TNodeHeightsArr);
var
  IniStr: string;
  i, j, k, SignalCount, EvLen, SignalBytesLen: Integer;
  SignalIniId, SignalEventIniId, s: string;
  FFormatSettings: TFormatSettings;
  AStream: TMemoryStream;
begin
  IniStr := '';
  FFormatSettings := GetUnivFormatSettings;
  SignalCount := Length(Signals);

  IniStr := IniStr + '[Signals]' + #13#10;
  IniStr := IniStr + 'Count=' + IntToStr(SignalCount) + #13#10;

  for i := 0 to SignalCount - 1 do
  begin
    SignalIniId := 'Signal_' + IntToStr(i);

    IniStr := IniStr + SignalIniId + '.Name=' + Signals[i].SignalName + #13#10;
    IniStr := IniStr + SignalIniId + '.ValueSource=' + IntToStr(Signals[i].SignalValueSource) + #13#10;
    IniStr := IniStr + SignalIniId + '.Direction=' + IntToStr(Signals[i].SignalDirection) + #13#10;
    IniStr := IniStr + SignalIniId + '.OwnerCircuit=' + Signals[i].OwnerCircuit + #13#10;
    IniStr := IniStr + SignalIniId + '.ValueType=' + IntToStr(Integer(Signals[i].ValueType)) + #13#10;

    EvLen := Length(Signals[i].SignalEvents);
    IniStr := IniStr + SignalIniId + '.EvLen=' + IntToStr(EvLen) + #13#10;

    for j := 0 to EvLen - 1 do
    begin
      SignalEventIniId := SignalIniId + '.Ev_' + IntToStr(j);

      IniStr := IniStr + SignalEventIniId + '.PatternTypeAfterEvent=' + IntToStr(Integer(Signals[i].SignalEvents[j].PatternTypeAfterEvent)) + #13#10;

      case Signals[i].SignalEvents[j].PatternTypeAfterEvent of
        ptDiscrete:
        begin
          case Signals[i].ValueType of
            sutNumeric:
            begin
              SignalBytesLen := Length(Signals[i].SignalEvents[j].ValueAfterEvent.NumericValue);

              IniStr := IniStr + SignalEventIniId + '.BytesLen=' + IntToStr(SignalBytesLen) + #13#10;

              for k := 0 to SignalBytesLen - 1 do
                IniStr := IniStr + SignalEventIniId + '.Byte' + IntToStr(k) + '=' + IntToStr(Signals[i].SignalEvents[j].ValueAfterEvent.NumericValue[k]) + #13#10;
            end; //sutNumeric

            sutSTD_LOGIC:
            begin
              SignalBytesLen := Length(Signals[i].SignalEvents[j].ValueAfterEvent.STD_LOGIC_Value);
              IniStr := IniStr + SignalEventIniId + '.BytesLen=' + IntToStr(SignalBytesLen) + #13#10;

              for k := 0 to SignalBytesLen - 1 do
                IniStr := IniStr + SignalEventIniId + '.STD_LOGIC_Byte' + IntToStr(k) + '=' + IntToStr(Integer(Signals[i].SignalEvents[j].ValueAfterEvent.STD_LOGIC_Value[k])) + #13#10;
            end;

            sutOrdinal:
              IniStr := IniStr + SignalEventIniId + '.OrdinalValue=' + IntToStr(Signals[i].SignalEvents[j].ValueAfterEvent.OrdinalValue) + #13#10;

            sutInt32:
              IniStr := IniStr + SignalEventIniId + '.Int32Value=' + IntToStr(Signals[i].SignalEvents[j].ValueAfterEvent.Int32Value) + #13#10;

            sutInt64:
              IniStr := IniStr + SignalEventIniId + '.Int64Value=' + IntToStr(Signals[i].SignalEvents[j].ValueAfterEvent.Int64Value) + #13#10;
           
            else
            begin
              MessageBox(0, 'Undefined signal value type', PChar('VHDL Controller Generator'), MB_ICONERROR);
              Exit;
            end;
          end;  //case
        end; //discrete

        ptClock:
        begin
          IniStr := IniStr + SignalEventIniId + '.ClockMidTransitionOffset=' + IntToStr(Signals[i].SignalEvents[j].ClockPatternAfterEvent.MidTransitionOffset) + #13#10;
          IniStr := IniStr + SignalEventIniId + '.ClockEndTransitionOffset=' + IntToStr(Signals[i].SignalEvents[j].ClockPatternAfterEvent.EndTransitionOffset) + #13#10;
        end;
      end;  //case PatternTypeAfterEvent

      IniStr := IniStr + SignalEventIniId + '.Moment=' + IntToStr(Signals[i].SignalEvents[j].Moment) + #13#10;
      s := FloatToStr(Signals[i].SignalEvents[j].Jitter, FFormatSettings);
      IniStr := IniStr + SignalEventIniId + '.Jitter=' + s + #13#10;
      IniStr := IniStr + SignalEventIniId + '.JitterType=' + IntToStr(Integer(Signals[i].SignalEvents[j].JitterType)) + #13#10;
      
      IniStr := IniStr + SignalEventIniId + '.CauseOfEvent=' + IntToStr(Signals[i].SignalEvents[j].CauseOfEvent) + #13#10;
      IniStr := IniStr + SignalEventIniId + '.EventConditionIndex=' + IntToStr(Signals[i].SignalEvents[j].EventConditionIndex) + #13#10;
      IniStr := IniStr + SignalEventIniId + '.EventType=' + IntToStr(Signals[i].SignalEvents[j].EventType) + #13#10;
      IniStr := IniStr + SignalEventIniId + '.EventInfo=' + Signals[i].SignalEvents[j].EventInfo + #13#10;
      IniStr := IniStr + SignalEventIniId + '.Highlighted=' + IntToStr(Ord(Signals[i].SignalEvents[j].Highlighted)) + #13#10;
    end; //for j

    IniStr := IniStr + SignalIniId + '.OrdinalValueTypeName=' + Signals[i].SignalOrdinalValueTypeName + #13#10;
    IniStr := IniStr + SignalIniId + '.Size=' + IntToStr(Signals[i].Size) + #13#10;
    IniStr := IniStr + SignalIniId + '.AddedBy=' + IntToStr(Integer(Signals[i].AddedBy)) + #13#10;

    IniStr := IniStr + SignalIniId + '.SynchronizedBySignal=' + Signals[i].SynchronizedBySignal + #13#10;
    IniStr := IniStr + SignalIniId + '.SynchronizedByCircuit=' + Signals[i].SynchronizedByCircuit + #13#10;
    IniStr := IniStr + SignalIniId + '.DrawType=' + IntToStr(Integer(Signals[i].DrawType)) + #13#10;

    SignalBytesLen := Length(Signals[i].NumericMinValue);
    IniStr := IniStr + SignalIniId + '.NumericBytesLen.MinValue=' + IntToStr(SignalBytesLen) + #13#10;
    for j := 0 to SignalBytesLen - 1 do
      IniStr := IniStr + SignalIniId + '.NumericBytes.MinValue.Byte_' + IntToStr(j) + '=' + IntToStr(Signals[i].NumericMinValue[j]) + #13#10;

    SignalBytesLen := Length(Signals[i].NumericMaxValue);
    IniStr := IniStr + SignalIniId + '.NumericBytesLen.MaxValue=' + IntToStr(SignalBytesLen) + #13#10;
    for j := 0 to SignalBytesLen - 1 do
      IniStr := IniStr + SignalIniId + '.NumericBytes.MaxValue.Byte_' + IntToStr(j) + '=' + IntToStr(Signals[i].NumericMaxValue[j]) + #13#10;

    IniStr := IniStr + SignalIniId + '.Int32MinValue=' + IntToStr(Signals[i].Int32MinValue) + #13#10;
    IniStr := IniStr + SignalIniId + '.Int32MaxValue=' + IntToStr(Signals[i].Int32MaxValue) + #13#10;

    IniStr := IniStr + SignalIniId + '.Int64MinValue=' + IntToStr(Signals[i].Int64MinValue) + #13#10;
    IniStr := IniStr + SignalIniId + '.Int64MaxValue=' + IntToStr(Signals[i].Int64MaxValue) + #13#10;

    IniStr := IniStr + SignalIniId + '.MinValueColor=' + IntToStr(Signals[i].MinValueColor) + #13#10;
    IniStr := IniStr + SignalIniId + '.MaxValueColor=' + IntToStr(Signals[i].MaxValueColor) + #13#10;
  end; //for i

  for i := 0 to Length(NodeHeights) - 1 do
  begin
    SignalIniId := 'Signal_' + IntToStr(i);
      IniStr := IniStr + SignalIniId + '.NodeHeight=' + IntToStr(NodeHeights[i]) + #13#10;
  end;

  IniStr := IniStr + #13#10; //to make it look like an ini

  AStream := TMemoryStream.Create;
  try
    AStream.Position := 0;
    AStream.Write(IniStr[1], Length(IniStr));
    AStream.Position := 0;
    AStream.SaveToFile(AIniFileName);
  finally
    AStream.Free;
  end;
end;

end.
