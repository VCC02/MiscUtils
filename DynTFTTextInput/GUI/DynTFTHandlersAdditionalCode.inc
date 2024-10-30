//uses
//  frmMainForm;


var
  AVirtualKeyboard: PDynTFTVirtualKeyboard;
  AVirtualKeyboardX2: PDynTFTVirtualKeyboardX2;
  CurrentEditBox: PDynTFTEdit;


procedure MoveCarretLeft(Sender: PPtrRec);
begin
  DynTFTMoveEditCaretToLeft(CurrentEditBox, 1);
  //ATestEdit^.BaseProps.Focused := CFOCUSED;
end;


procedure MoveCarretRight(Sender: PPtrRec);
begin
  DynTFTMoveEditCaretToRight(CurrentEditBox, 1);
  //ATestEdit^.BaseProps.Focused := CFOCUSED;
end;


procedure HandleEnter(ShowMessageBox: Boolean = True);
begin
  if UpperCase(Copy(edtInput^.Text, 1, 7)) = 'Prefix=' then
  begin
    //DoOnPressEnterOnInput(edtInput^.Text);
    edtInput^.Text := '';
    DynTFTMoveEditCaretToHome(edtInput);
    DynTFTDrawEditWithoutCaret(edtInput, True);
  end;
end;


procedure HandleTab;
begin

end;


procedure UpdateDataBoxes;
begin
  //if CurrentEditBox = edtData1 then
  //  DoOnPressEnterOnData1(CurrentEditBox^.Text);
  //
  //if CurrentEditBox = edtData2 then
  //  DoOnPressEnterOnData2(CurrentEditBox^.Text);
end;


procedure VirtualKeyboard_OnCharKey(Sender: PPtrRec; var PressedChar: TVKPressedChar; CurrentShiftState: TPtr);
var
  AText: string {$IFNDEF IsDesktop}[CMaxKeyButtonStringLength] {$ENDIF};
begin
  if PDynTFTVirtualKeyboard(TPtrRec(Sender))^.ShiftState and CDYNTFTSS_CTRL = CDYNTFTSS_CTRL then
    Exit;

  if PDynTFTVirtualKeyboard(TPtrRec(Sender))^.ShiftState and CDYNTFTSS_ALT = CDYNTFTSS_ALT then
  begin
    Exit;
  end;

  AText := PressedChar;
  DynTFTEditInsertTextAtCaret(CurrentEditBox, AText);

  if CurrentEditBox^.BaseProps.Focused and CFOCUSED <> CFOCUSED then
    DynTFTFocusComponent(PDynTFTBaseComponent(TPtrRec(CurrentEditBox))); 

  if not chkPass^.Checked then
    if UpperCase(Copy(edtInput^.Text, 1, 7)) = 'Prefix=' then    //edtInput only  - set your text here
    begin
      chkPass^.Checked := True;
      DynTFTDrawCheckBox(chkPass, True);
      edtInput^.PasswordText := PDynTFTCheckBox(TPtrRec(Sender))^.Checked;
      DynTFTEditAfterTypingText(edtInput);
    end;

  UpdateDataBoxes;
end;


procedure VirtualKeyboard_OnSpecialKey(Sender: PPtrRec; SpecialKey: Integer; CurrentShiftState: TPtr);
begin
  case SpecialKey of
    VK_BACK :
    begin
      DynTFTEditBackspaceAtCaret(CurrentEditBox);
    end;

    VK_DELETE :
    begin
      if CurrentShiftState and CDYNTFTSS_CTRL_ALT = CDYNTFTSS_CTRL_ALT then
        {$IFNDEF IsDesktop}
          Reset;
        {$ELSE}
          Application.MainForm.Close;
        {$ENDIF}
      DynTFTEditDeleteAtCaret(CurrentEditBox);
    end;

    VK_LEFT: DynTFTMoveEditCaretToLeft(CurrentEditBox, 1);

    VK_RIGHT: DynTFTMoveEditCaretToRight(CurrentEditBox, 1);

    VK_HOME: DynTFTMoveEditCaretToHome(CurrentEditBox);

    VK_END: DynTFTMoveEditCaretToEnd(CurrentEditBox);

    VK_RETURN:
    begin
      HandleEnter;
      chkVKX2^.Checked := True;
      DynTFTDrawCheckBox(chkVKX2, True);
    end;

    VK_APPS:
    begin
      CurrentEditBox^.Text := '';
      DynTFTDrawEdit(CurrentEditBox, False);
    end;
                    
    VK_TAB: HandleTab;
  end;

  if CurrentEditBox^.BaseProps.Focused and CFOCUSED <> CFOCUSED then
    DynTFTFocusComponent(PDynTFTBaseComponent(TPtrRec(CurrentEditBox)));

  UpdateDataBoxes;
end;


procedure VirtualKeyboardX2_OnCharKey(Sender: PPtrRec; var PressedChar: TVKX2PressedChar; CurrentShiftState: TPtr);
var
  AText: string {$IFNDEF IsDesktop}[CMaxKeyButtonStringLength] {$ENDIF};
begin
  if PDynTFTVirtualKeyboardX2(TPtrRec(Sender))^.ShiftState and CDYNTFTSS_CTRL = CDYNTFTSS_CTRL then
    Exit;

  if PDynTFTVirtualKeyboardX2(TPtrRec(Sender))^.ShiftState and CDYNTFTSS_ALT = CDYNTFTSS_ALT then
  begin
    Exit;
  end;

  AText := PressedChar;
  DynTFTEditInsertTextAtCaret(CurrentEditBox, AText);

  if CurrentEditBox^.BaseProps.Focused and CFOCUSED <> CFOCUSED then
    DynTFTFocusComponent(PDynTFTBaseComponent(TPtrRec(CurrentEditBox)));

  UpdateDataBoxes;  
end; //////////////////////////////////////////////////////////////////// maybe a refactoring can clean this handler


procedure btnDestroyKeyButtons_OnClickUser(Sender: PPtrRec);
{$IFNDEF IsDesktop}
  var
    ABtnTemp: PDynTFTButton;
{$ENDIF}
begin
  if not chkVKX2^.Checked then
    DynTFTVirtualKeyboard_DestroyAndPaint(AVirtualKeyboard)
  else
    DynTFTVirtualKeyboardX2_DestroyAndPaint(AVirtualKeyboardX2);

  {$IFDEF IsDesktop}
    DynTFTButton_DestroyAndPaint(PDynTFTButton(TPtrRec(Sender)));
  {$ELSE}
    //without temp var, mikroPascal gives an error:  Operator "@" not applicable to these operands "?T230"
    ABtnTemp := PDynTFTButton(TPtrRec(Sender));
    DynTFTButton_DestroyAndPaint(ABtnTemp);
    Sender := PPtrRec(TPtrRec(ABtnTemp));
  {$ENDIF}
  
  DynTFTEnableComponent(PDynTFTBaseComponent(TPtrRec(btnCreateKeyboard)));
  DynTFTEnableComponent(PDynTFTBaseComponent(TPtrRec(chkVKX2)));
end;


