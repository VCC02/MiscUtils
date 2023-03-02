{
    Copyright (C) 2022 VCC
    creation date: Jan 2023
    initial release date: 02 Feb 2023

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


unit ObjectInspectorFrame;

//{$mode ObjFPC}
{$H+}
{$mode Delphi}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, ExtCtrls, Graphics, ImgList,
  ColorBox, StdCtrls, Dialogs, ComCtrls, Menus, Buttons, VirtualTrees;


const
  CCategoryLevel = 0;
  CPropertyLevel = 1;
  CPropertyItemLevel = 2; //used for properties of list/array types

  CCustomColorName = 'Custom...';

type
  //For single value properties only. For list properties, this is ignored.
  //etNone might be used for list items which should not be directly editable (only through a list editor).
  TOIEditorType = (
    etNone,               //No editor button, editbox or combobox is displayed
    etText,               //The built-in VirtualTreeView editbox
    etTextWithArrow,      //The built-in VirtualTreeView editbox. Next to it, there is down arrow button, which can open a menu.
    etSpinText,           //The built-in VirtualTreeView editbox with an updown button
    etFilePath,           //A "..." browse button with local file open dialog
    etDirPath,            //A "..." browse button with local directory open dialog
    etFilePathWithArrow,  //A "..." browse button with local file open dialog. Next to it, there is down arrow button, which can open a menu.
    etBooleanCombo,       //A combobox with only two items, "False" and "True"
    etColorCombo,         //A standard colorbox (combobox with colors) and an overlayed editbox. This allows custom user colors.
    etEnumCombo,          //A combobox with user-defined list of items.
    etEnumComboWithBtn,   //A combobox with user-defined list of items. Next to the combobox, a "..." browse button is displayed.
    etUserEditor          //A "..." browse button which opens user-defined editor.
  );

  TOIColorFormat = (cfHexa, cfDecimal, cfMixed, cfText);

  TNodeDataPropertyRec = record
    PropertyIndex: Integer;
    Level: Integer; //like node level
    EditorType: TOIEditorType;        //Not used for categories, because they are not editable.
    ItemVisible: Boolean;
  end;
  PNodeDataPropertyRec = ^TNodeDataPropertyRec;

  TOnOIGetCategoryCount = function: Integer of object;
  TOnOIGetCategory = function(AIndex: Integer): string of object;

  TOnOIGetPropertyCount = function(ACategoryIndex: Integer): Integer of object;
  TOnOIGetPropertyName = function(ACategoryIndex, APropertyIndex: Integer): string of object;
  TOnOIGetPropertyValue = function(ACategoryIndex, APropertyIndex: Integer; var AEditorType: TOIEditorType): string of object;

  TOnOIGetListPropertyItemCount = function(ACategoryIndex, APropertyIndex: Integer): Integer of object;
  TOnOIGetListPropertyItemName = function(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string of object;
  TOnOIGetListPropertyItemValue = function(ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AEditorType: TOIEditorType): string of object;

  TOnUIGetDataTypeName = function(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string of object;
  TOnUIGetExtraInfo = function(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string of object;

  TOnOIGetImageIndexEx = procedure(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; Kind: TVTImageKind;
    Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList) of object;

  TOnOIEditedText = procedure(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string) of object;
  TOnOIEditItems = function(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean of object;

  TOnOIGetColorConstsCount = function(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer of object;
  TOnOIGetColorConst = procedure(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AColorItemIndex: Integer; var AColorName: string; var AColorValue: Int64) of object; //AColorValue is In64, instead of TColor, because on 64-bit, TObject is 64-bit

  TOnOIGetEnumConstsCount = function(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer of object;
  TOnOIGetEnumConst = procedure(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string) of object;

  TOnOIPaintText = procedure(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
    const TargetCanvas: TCanvas; Column: TColumnIndex; var TextType: TVSTTextType) of object;

  TOnOIBeforeCellPaint = procedure(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
    TargetCanvas: TCanvas; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect) of object;

  TOnOITextEditorMouseDown = procedure(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
    Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

  TOnOITextEditorMouseMove = function(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
    Sender: TObject; Shift: TShiftState; X, Y: Integer): Boolean of object;

  TOnOITextEditorKeyUp = procedure(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
    Sender: TObject; var Key: Word; Shift: TShiftState) of object;

  TOnOITextEditorKeyDown = TOnOITextEditorKeyUp;

  TOnOIEditorAssignMenuAndTooltip = procedure(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
    Sender: TObject; var APopupMenu: TPopupMenu; var AHint: string; var AShowHint: Boolean) of object;

  TOnOIGetFileDialogSettings = procedure(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AFilter, AInitDir: string) of object;

  TOnOIArrowEditorClick = procedure(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer) of object;
  TOnOIUserEditorClick = procedure(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ARepaintValue: Boolean) of object;

  TOnOIBrowseFile = function(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
    AFilter, ADialogInitDir: string; var Handled: Boolean; AReturnMultipleFiles: Boolean = False): string of object;

  TOnAfterSpinTextEditorChanging = procedure(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewValue: string) of object;

  { TfrObjectInspector }

  TfrObjectInspector = class(TFrame)
    edtSearch: TEdit;
    imgDownArrow: TImage;
    imglstOIColorIcons: TImageList;
    MenuItem_ShowHideSearchBox: TMenuItem;
    pnlvstOI: TPanel;
    pmOI: TPopupMenu;
    tmrSearch: TTimer;
    tmrSetEditBox: TTimer;
    tmrColCmbDropped: TTimer;
    tmrEditingProperty: TTimer;
    procedure edtSearchChange(Sender: TObject);
    procedure MenuItem_ShowHideSearchBoxClick(Sender: TObject);
    procedure tmrColCmbDroppedTimer(Sender: TObject);
    procedure tmrEditingPropertyTimer(Sender: TObject);
    procedure tmrSearchTimer(Sender: TObject);
    procedure tmrSetEditBoxTimer(Sender: TObject);
  private
    vstOI: TVirtualStringTree;

    FTextEditorEditBox: TEdit;  //pointer to the built-in editor
    FEdtColorProperty: TEdit;
    FColcmbProperty: TColorBox;
    FCmbBooleanProperty: TComboBox;
    FCmbMiscEnumProperty: TComboBox;
    FBtnItemsProperty: TButton;
    FupdownTextEditor: TUpDown;
    FBtnArrowProperty: TBitBtn;
    FSpdBtnArrowProperty: TSpeedButton;
    FEdtPath: TEdit; //used for etFilePath and etDirPath

    FEditingText: string;
    FPropHitInfo: THitInfo;
    FEditingNode: PVirtualNode;
    FEditingColumn: Integer; //used at least on the built-in editor
    FcolcmbPropertyIsDropped: Boolean;

    FListItemsVisible: Boolean;
    FDataTypeVisible: Boolean;
    FExtraInfoVisible: Boolean;
    FColorFormat: TOIColorFormat;
    FPropertyItemHeight: Integer;
    FVstVertScrollBarVisible: Boolean; //flag set by vst event

    FOnOIGetCategoryCount: TOnOIGetCategoryCount;
    FOnOIGetCategory: TOnOIGetCategory;
    FOnOIGetPropertyCount: TOnOIGetPropertyCount;
    FOnOIGetPropertyName: TOnOIGetPropertyName;
    FOnOIGetPropertyValue: TOnOIGetPropertyValue;
    FOnOIGetListPropertyItemCount: TOnOIGetListPropertyItemCount;
    FOnOIGetListPropertyItemName: TOnOIGetListPropertyItemName;
    FOnOIGetListPropertyItemValue: TOnOIGetListPropertyItemValue;
    FOnUIGetDataTypeName: TOnUIGetDataTypeName;
    FOnUIGetExtraInfo: TOnUIGetExtraInfo;

    FOnOIGetImageIndexEx: TOnOIGetImageIndexEx;
    FOnOIEditedText: TOnOIEditedText;
    FOnOIEditItems: TOnOIEditItems;

    FOnOIGetColorConstsCount: TOnOIGetColorConstsCount;
    FOnOIGetColorConst: TOnOIGetColorConst;

    FOnOIGetEnumConstsCount: TOnOIGetEnumConstsCount;
    FOnOIGetEnumConst: TOnOIGetEnumConst;

    FOnOIPaintText: TOnOIPaintText;
    FOnOIBeforeCellPaint: TOnOIBeforeCellPaint;

    FOnOITextEditorMouseDown: TOnOITextEditorMouseDown;
    FOnOITextEditorMouseMove: TOnOITextEditorMouseMove;
    FOnOITextEditorKeyUp: TOnOITextEditorKeyUp;
    FOnOITextEditorKeyDown: TOnOITextEditorKeyDown;

    FOnOIEditorAssignMenuAndTooltip: TOnOIEditorAssignMenuAndTooltip;
    FOnOIGetFileDialogSettings: TOnOIGetFileDialogSettings;
    FOnOIArrowEditorClick: TOnOIArrowEditorClick;
    FOnOIUserEditorClick: TOnOIUserEditorClick;

    FOnOIBrowseFile: TOnOIBrowseFile;
    FOnAfterSpinTextEditorChanging: TOnAfterSpinTextEditorChanging;

    procedure SetDataTypeVisible(Value: Boolean);
    procedure SetExtraInfoVisible(Value: Boolean);
    procedure SetPropertyItemHeight(Value: Integer);

    function DoOnOIGetCategoryCount: Integer;
    function DoOnOIGetCategory(AIndex: Integer): string;

    function DoOnOIGetPropertyCount(ACategoryIndex: Integer): Integer;
    function DoOnOIGetPropertyName(ACategoryIndex, APropertyIndex: Integer): string;
    function DoOnOIGetPropertyValue(ACategoryIndex, APropertyIndex: Integer; var AEditorType: TOIEditorType): string;

    function DoOnOIGetListPropertyItemCount(ACategoryIndex, APropertyIndex: Integer): Integer;
    function DoOnOIGetListPropertyItemName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
    function DoOnOIGetListPropertyItemValue(ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AEditorType: TOIEditorType): string;
    function DoOnUIGetDataTypeName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
    function DoOnUIGetExtraInfo(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;

    procedure DoOnOIGetImageIndexEx(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);

    procedure DoOnOIEditedText(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
    function DoOnOIEditItems(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;

    function DoOnOIGetColorConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
    procedure DoOnOIGetColorConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AColorItemIndex: Integer; var AColorName: string; var AColorValue: Int64);

    function DoOnOIGetEnumConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
    procedure DoOnOIGetEnumConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string);

    procedure DoOnOIPaintText(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
      const TargetCanvas: TCanvas; Column: TColumnIndex; var TextType: TVSTTextType);

    procedure DoOnOIBeforeCellPaint(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
      TargetCanvas: TCanvas; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

    procedure DoOnOITextEditorMouseDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function DoOnOITextEditorMouseMove(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; Shift: TShiftState; X, Y: Integer): Boolean;

    procedure DoOnOITextEditorKeyUp(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure DoOnOITextEditorKeyDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure DoOnOIEditorAssignMenuAndTooltip(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      Sender: TObject; var APopupMenu: TPopupMenu; var AHint: string; var AShowHint: Boolean);

    procedure DoOnOIGetFileDialogSettings(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AFilter, AInitDir: string);
    procedure DoOnOIArrowEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);
    procedure DoOnOIUserEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ARepaintValue: Boolean);

    function DoOnOIBrowseFile(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
      AFilter, ADialogInitDir: string; var Handled: Boolean; AReturnMultipleFiles: Boolean = False): string;

    procedure DoOnAfterSpinTextEditorChanging(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewValue: string);

    procedure edtColorPropertyExit(Sender: TObject);
    procedure edtColorPropertyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure colcmbPropertyExit(Sender: TObject);
    procedure colcmbPropertySelect(Sender: TObject);
    procedure colcmbPropertyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure colcmbPropertyKeyPress(Sender: TObject; var Key: Char);
    procedure colcmbPropertyCloseUp(Sender: TObject);
    procedure colcmbPropertyDropDown(Sender: TObject);

    procedure cmbBooleanPropertyExit(Sender: TObject);
    procedure cmbBooleanPropertySelect(Sender: TObject);

    procedure cmbMiscEnumPropertyExit(Sender: TObject);
    procedure cmbMiscEnumPropertySelect(Sender: TObject);

    procedure btnItemsPropertyExit(Sender: TObject);
    procedure btnItemsPropertyClick(Sender: TObject);

    procedure btnArrowPropertyExit(Sender: TObject);
    procedure btnArrowPropertyClick(Sender: TObject);

    procedure spdbtnArrowPropertyExit(Sender: TObject);
    procedure spdbtnArrowPropertyClick(Sender: TObject);

    procedure EdtPathExit(Sender: TObject);

    procedure TextEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TextEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TextEditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TextEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure updownTextEditorChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);

    procedure vstOICompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstOICreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure vstOIDblClick(Sender: TObject);
    procedure vstOIEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
    procedure vstOIEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vstOIEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

    procedure vstOIGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: {$IFDEF FPC} string {$ELSE} WideString {$ENDIF});

    procedure vstOIHeaderMouseDown(Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure vstOIPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure vstOIBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

    procedure vstOIGetImageIndex(
      Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

    procedure vstOIGetImageIndexEx(
      Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);

    procedure vstOIMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure vstOIMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure vstOINewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
    procedure vstOIScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure vstOIShowScrollbar(Sender: TBaseVirtualTree; Bar: Integer; AShow: Boolean);

    procedure CreateRemainingUIComponents;
    procedure FreeEditorComponents;
    function GetNodeIndexInfo(ANode: PVirtualNode; out ANodeLevel, ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer): Boolean;
    function GetColcmbPropertyAsText: string;
    function GetPropertyValueForEditor(ANodeLevel, ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer; ACurrentEditorType: TOIEditorType): string;
    procedure AssignPopupMenuAndTooltipToEditor(AEditor: TControl);

    procedure ResetItemVisibleFlagOnAllFiles;
    procedure MarkAllParentNodesAsVisible(ACurrentNode: PVirtualNode);

    function GetCategoryNodeByIndex(ACategoryIndex: Integer): PVirtualNode;
    function GetPropertyNodeByIndex(ACategoryIndex, APropertyIndex: Integer): PVirtualNode;
    function GetPropertyItemNodeByIndex(ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer): PVirtualNode;
    function GetNodeByLevel(ANodeLevel, ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer): PVirtualNode;

    function GetLocalEditorLeft(AEditorWidth, AVertScrollBarWidth: Integer): Integer;
    function GetLocalComboEditorLeft: Integer;
    function GetLocalComboEditorWidth(AVertScrollBarWidth: Integer): Integer;
    function IsVstVertScrollBarVisible(AVst: TVirtualStringTree): Boolean;
    function GetVertScrollBarWidth: Integer;
    function GetTextEditorWidth: Integer;
    procedure SetTextEditorEditPosAndSize;

    procedure CreateBooleanComboBox(Node: PVirtualNode; VertScrollBarWidth: Integer);
    procedure CreateBrowseEditorButton(Node: PVirtualNode; VertScrollBarWidth: Integer; AUsedForComboBox, AUsedForPath: Boolean);
    procedure CreateArrowButton(Node: PVirtualNode; VertScrollBarWidth: Integer);
    procedure CreateColorComboBox(Node: PVirtualNode; VertScrollBarWidth: Integer);
    procedure CreateEnumComboBox(Node: PVirtualNode; VertScrollBarWidth: Integer; ACreateBrowseButton: Boolean = False);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ReloadContent;
    procedure ReloadPropertyItems(ACategoryIndex, APropertyIndex: Integer; ACloseItemEditor: Boolean = False);
    procedure RepaintNodeByLevel(ANodeLevel, ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer; AScrollIntoView: Boolean = True);
    procedure CancelCurrentEditing; //usually the text editor   - this is called by ReloadContent and ReloadPropertyItems

    property ListItemsVisible: Boolean read FListItemsVisible write FListItemsVisible; //to be set before calling ReloadContent
    property DataTypeVisible: Boolean read FDataTypeVisible write SetDataTypeVisible;    //to be set before calling ReloadContent
    property ExtraInfoVisible: Boolean read FExtraInfoVisible write SetExtraInfoVisible; //to be set before calling ReloadContent

    property EditingText: string read FEditingText write FEditingText; //do not move this to published, it's a runtime thing
  published
    property ColorFormat: TOIColorFormat read FColorFormat write FColorFormat;  //affects ColorBox editors when getting selected color
    property PropertyItemHeight: Integer read FPropertyItemHeight write SetPropertyItemHeight;

    property OnOIGetCategoryCount: TOnOIGetCategoryCount write FOnOIGetCategoryCount;
    property OnOIGetCategory: TOnOIGetCategory write FOnOIGetCategory;
    property OnOIGetPropertyCount: TOnOIGetPropertyCount write FOnOIGetPropertyCount;
    property OnOIGetPropertyName: TOnOIGetPropertyName write FOnOIGetPropertyName;
    property OnOIGetPropertyValue: TOnOIGetPropertyValue write FOnOIGetPropertyValue;
    property OnOIGetListPropertyItemCount: TOnOIGetListPropertyItemCount write FOnOIGetListPropertyItemCount;
    property OnOIGetListPropertyItemName: TOnOIGetListPropertyItemName write FOnOIGetListPropertyItemName;
    property OnOIGetListPropertyItemValue: TOnOIGetListPropertyItemValue write FOnOIGetListPropertyItemValue;
    property OnUIGetDataTypeName: TOnUIGetDataTypeName write FOnUIGetDataTypeName;
    property OnUIGetExtraInfo: TOnUIGetExtraInfo write FOnUIGetExtraInfo;

    property OnOIGetImageIndexEx: TOnOIGetImageIndexEx write FOnOIGetImageIndexEx;
    property OnOIEditedText: TOnOIEditedText write FOnOIEditedText;
    property OnOIEditItems: TOnOIEditItems write FOnOIEditItems;

    property OnOIGetColorConstsCount: TOnOIGetColorConstsCount write FOnOIGetColorConstsCount;
    property OnOIGetColorConst: TOnOIGetColorConst write FOnOIGetColorConst;

    property OnOIGetEnumConstsCount: TOnOIGetEnumConstsCount write FOnOIGetEnumConstsCount;
    property OnOIGetEnumConst: TOnOIGetEnumConst write FOnOIGetEnumConst;

    property OnOIPaintText: TOnOIPaintText write FOnOIPaintText;
    property OnOIBeforeCellPaint: TOnOIBeforeCellPaint write FOnOIBeforeCellPaint;

    property OnOITextEditorMouseDown: TOnOITextEditorMouseDown write FOnOITextEditorMouseDown;
    property OnOITextEditorMouseMove: TOnOITextEditorMouseMove write FOnOITextEditorMouseMove;
    property OnOITextEditorKeyUp: TOnOITextEditorKeyUp write FOnOITextEditorKeyUp;
    property OnOITextEditorKeyDown: TOnOITextEditorKeyDown write FOnOITextEditorKeyDown;

    property OnOIEditorAssignMenuAndTooltip: TOnOIEditorAssignMenuAndTooltip write FOnOIEditorAssignMenuAndTooltip;
    property OnOIGetFileDialogSettings: TOnOIGetFileDialogSettings write FOnOIGetFileDialogSettings;
    property OnOIArrowEditorClick: TOnOIArrowEditorClick write FOnOIArrowEditorClick;
    property OnOIUserEditorClick: TOnOIUserEditorClick write FOnOIUserEditorClick;

    property OnOIBrowseFile: TOnOIBrowseFile write FOnOIBrowseFile;
    property OnAfterSpinTextEditorChanging: TOnAfterSpinTextEditorChanging write FOnAfterSpinTextEditorChanging;
  end;


  TOIEditorTypeStr = array[TOIEditorType] of string;

const
  COIEditorTypeStr: TOIEditorTypeStr = (   //editor names for debugging
    'etNone',
    'etText',
    'etTextWithArrow',
    'etSpinText',
    'etFilePath',
    'etDirPath',
    'etFilePathWithArrow',
    'etBooleanCombo',
    'etColorCombo',
    'etEnumCombo',
    'etEnumComboWithBtn',
    'etUserEditor'
  );


implementation

{$R *.frm}


uses
  Math, Clipbrd;


const
  CTextEditorSpacing = 5;
  CEmptySpaceForIcon = '       ';
  CMinComboWidth = 30;  //minimum combobox width, to be displayed
  CDefaultArrowButonWidth = 21;


function ValidHexColor(ColorStr: string): Boolean;
var
  i: Integer;
begin
  Result := True;

  if (ColorStr = '') or (Length(ColorStr) < 2) or (ColorStr[1] <> '$') then
  begin
    Result := False;
    Exit;
  end;

  for i := 2 to Length(ColorStr) do
    if not (ColorStr[i] in ['0'..'9', 'a'..'f', 'A'..'F']) then
    begin
      Result := False;
      Exit;
    end;
end;


function Pow16(x: Byte): Cardinal;
var
  i: Byte;
begin
  Result := 1;
  for i := 1 to x do
    Result := Result shl 4;
end;


function HexaDigitToByte(ch: Char): Byte;
begin
  Result := 0;
  Ch := UpCase(Ch);
  if Ch in ['0'..'9'] then
  begin
    Result := Ord(Ch) - 48;
    Exit;
  end;

  if Ch in ['A'..'F'] then
    Result := Ord(Ch) - 65 + 10;
end;


function HexToInt(s: string): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
    if s[i] in ['0'..'9', 'a'..'f', 'A'..'F'] then
      Result := Result + HexaDigitToByte(s[i]) * Pow16(Length(s) - i);
end;


function BrowseFile(AFilter, ADialogInitDir: string; AReturnMultipleFiles: Boolean = False): string;
var
  OpenDialog: TOpenDialog;
begin
  Result := '';
  OpenDialog := TOpenDialog.Create(nil);
  try
    if AReturnMultipleFiles then
      OpenDialog.Options := OpenDialog.Options + [ofAllowMultiSelect];

    OpenDialog.Filter := AFilter;
    OpenDialog.InitialDir := ADialogInitDir;

    if not OpenDialog.Execute then
      Exit;

    if not AReturnMultipleFiles then
      Result := OpenDialog.FileName
    else
      Result := OpenDialog.Files.Text;
  finally
    OpenDialog.Free;
  end;
end;


function BrowseDir(AReturnMultipleDirs: Boolean = False): string;
var
  SelDirDialog: TSelectDirectoryDialog;
begin
  Result := '';
  SelDirDialog := TSelectDirectoryDialog.Create(nil);
  try
    if AReturnMultipleDirs then
      SelDirDialog.Options := SelDirDialog.Options + [ofAllowMultiSelect];  //probably won't select multiple

    if not SelDirDialog.Execute then
      Exit;

    if not AReturnMultipleDirs then
      Result := SelDirDialog.FileName
    else
      Result := SelDirDialog.Files.Text;
  finally
    SelDirDialog.Free;
  end;
end;


procedure TfrObjectInspector.CreateRemainingUIComponents;
var
  NewColum: TVirtualTreeColumn;
begin
  vstOI := TVirtualStringTree.Create(Self);

  vstOI.Parent := Self;

  vstOI.ParentFont := False;
  vstOI.Font.Style := [];
  vstOI.Left := 0;
  vstOI.Top := 0;
  vstOI.Width := pnlvstOI.Width;
  vstOI.Height := Height;
  vstOI.Constraints.MinWidth := vstOI.Width;
  vstOI.Constraints.MinHeight := vstOI.Height;
  vstOI.DefaultNodeHeight := 22; //the default value, 18, should be enough, but the TEdit has a bigger default height
  vstOI.Anchors := [akBottom, akLeft, akRight, akTop];
  vstOI.Hint := '';
  vstOI.Header.AutoSizeIndex := 0;
  vstOI.Header.DefaultHeight := 17;
  vstOI.Header.Font.Charset := DEFAULT_CHARSET;
  vstOI.Header.Font.Color := clWindowText;
  vstOI.Header.Font.Height := -11;
  vstOI.Header.Font.Name := 'Tahoma';
  vstOI.Header.Font.Style := [];
  vstOI.Header.Height := 25;
  vstOI.Header.MinHeight := 25;
  vstOI.Header.MainColumn := -1;
  vstOI.Header.Options := [hoColumnResize, hoDblClickResize, hoShowSortGlyphs, hoVisible];
  vstOI.Header.PopupMenu := nil;
  vstOI.Header.Style := hsFlatButtons;
  vstOI.Indent := 12;
  vstOI.ParentShowHint := False;
  vstOI.PopupMenu := pmOI;
  vstOI.ShowHint := True;
  vstOI.StateImages := imglstOIColorIcons;
  vstOI.TabOrder := 0;
  vstOI.TextMargin := 2;
  vstOI.TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toDisableAutoscrollOnEdit];
  vstOI.TreeOptions.MiscOptions := [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning];
  vstOI.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines];
  vstOI.TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect];
  vstOI.OnCompareNodes := vstOICompareNodes;
  vstOI.OnCreateEditor := vstOICreateEditor;
  vstOI.OnDblClick := vstOIDblClick;
  vstOI.OnEditCancelled := vstOIEditCancelled;
  vstOI.OnEdited := vstOIEdited;
  vstOI.OnEditing := vstOIEditing;
  vstOI.OnGetText := vstOIGetText;
  vstOI.OnHeaderMouseDown := vstOIHeaderMouseDown;
  vstOI.OnPaintText := vstOIPaintText;
  vstOI.OnBeforeCellPaint := vstOIBeforeCellPaint;
  vstOI.OnGetImageIndex := vstOIGetImageIndex;
  vstOI.OnGetImageIndexEx := vstOIGetImageIndexEx;
  vstOI.OnMouseDown := vstOIMouseDown;
  vstOI.OnNewText := vstOINewText;
  vstOI.OnScroll := vstOIScroll;

  //vstOI.PopupMenu;
  vstOI.Colors.GridLineColor := clGray;
  vstOI.Colors.UnfocusedSelectionColor := clGradientInactiveCaption;

  NewColum := vstOI.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, {coFixed,} coAllowFocus];
  NewColum.Position := 0;
  NewColum.Width := 240;
  NewColum.Text := 'Property';

  NewColum := vstOI.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Position := 1;
  NewColum.Width := 150;
  NewColum.Text := 'Value';

  NewColum := vstOI.Header.Columns.Add;
  NewColum.MinWidth := 200;
  NewColum.Position := 2;
  NewColum.Width := 200;
  NewColum.Text := 'DataType';

  NewColum := vstOI.Header.Columns.Add;
  NewColum.MinWidth := 300;
  NewColum.Position := 3;
  NewColum.Width := 300;
  NewColum.Text := 'Extra info';

  vstOI.Visible := True;
  pnlvstOI.Visible := True;
end;


constructor TfrObjectInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateRemainingUIComponents;

  vstOI.NodeDataSize := SizeOf(TNodeDataPropertyRec);

  FEdtColorProperty := nil;
  FColcmbProperty := nil;
  FCmbBooleanProperty := nil;
  FCmbMiscEnumProperty := nil;
  FBtnItemsProperty := nil;
  FupdownTextEditor := nil;
  FTextEditorEditBox := nil;

  FOnOIGetCategoryCount := nil;
  FOnOIGetCategory := nil;
  FOnOIGetPropertyCount := nil;
  FOnOIGetPropertyName := nil;
  FOnOIGetPropertyValue := nil;
  FOnOIGetListPropertyItemCount := nil;
  FOnOIGetListPropertyItemName := nil;
  FOnOIGetListPropertyItemValue := nil;
  FOnUIGetDataTypeName := nil;
  FOnUIGetExtraInfo := nil;

  FOnOIGetImageIndexEx := nil;
  FOnOIEditedText := nil;
  FOnOIEditItems := nil;

  FOnOIGetColorConstsCount := nil;
  FOnOIGetColorConst := nil;

  FOnOIGetEnumConstsCount := nil;
  FOnOIGetEnumConst := nil;

  FOnOIPaintText := nil;
  FOnOIBeforeCellPaint := nil;

  FOnOITextEditorMouseDown := nil;
  FOnOITextEditorMouseMove := nil;
  FOnOITextEditorKeyUp := nil;
  FOnOITextEditorKeyDown := nil;

  FOnOIEditorAssignMenuAndTooltip := nil;
  FOnOIGetFileDialogSettings := nil;
  FOnOIArrowEditorClick := nil;
  FOnOIUserEditorClick := nil;

  FOnOIBrowseFile := nil;
  FOnAfterSpinTextEditorChanging := nil;

  FListItemsVisible := True;
  FDataTypeVisible := True;
  FExtraInfoVisible := True;
  FColorFormat := cfHexa;

  FEditingText := '';
  FEditingNode := nil;
  FEditingColumn := -1;
  FcolcmbPropertyIsDropped := False;
  FPropertyItemHeight := 24;
  FVstVertScrollBarVisible := True;
end;


destructor TfrObjectInspector.Destroy;
begin
  inherited Destroy;
end;


procedure TfrObjectInspector.tmrEditingPropertyTimer(Sender: TObject);
begin
  tmrEditingProperty.Enabled := False;

  if FPropHitInfo.HitNode = nil then
    Exit;

  if FPropHitInfo.HitColumn = 1 then
    vstOI.EditNode(FPropHitInfo.HitNode, FPropHitInfo.HitColumn);
end;


procedure TfrObjectInspector.ResetItemVisibleFlagOnAllFiles;
var
  Node: PVirtualNode;
  NodeData: PNodeDataPropertyRec;
begin
  Node := vstOI.GetFirst;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstOI.GetNodeData(Node);
    NodeData^.ItemVisible := False;

    Node := vstOI.GetNext(Node);
  until Node = nil;
end;



procedure TfrObjectInspector.MarkAllParentNodesAsVisible(ACurrentNode: PVirtualNode);
var
  NodeData: PNodeDataPropertyRec;
  Level: Integer;
begin
  if ACurrentNode = nil then
    Exit;

  while ACurrentNode^.Parent <> nil do  //unfortunately (for this algorithm), nodes at level 0 do not have a nil parent
  begin
    Level := vstOI.GetNodeLevel(ACurrentNode);
    if Level > 0 then
      ACurrentNode := ACurrentNode^.Parent;

    NodeData := vstOI.GetNodeData(ACurrentNode);
    NodeData^.ItemVisible := True;

    if Level = 0 then
      Break;
  end;
end;


procedure TfrObjectInspector.tmrSearchTimer(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PNodeDataPropertyRec;
  PropName, SearchString: string;
  Visibility: Boolean;

  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  tmrSearch.Enabled := False;

  //This is not an efficient algorithm, but it should do the job for most projects.
  vstOI.BeginUpdate;
  try
    Node := vstOI.GetLast;
    if Node = nil then
      Exit;

    ResetItemVisibleFlagOnAllFiles;

    SearchString := UpperCase(edtSearch.Text);

    repeat
      NodeData := vstOI.GetNodeData(Node);

      if not GetNodeIndexInfo(Node, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
        PropName := 'NoInfo'
      else
      begin
        case NodeLevel of
          CCategoryLevel:
            PropName := DoOnOIGetCategory(CategoryIndex);

          CPropertyLevel:
            PropName := DoOnOIGetPropertyName(CategoryIndex, PropertyIndex);

          CPropertyItemLevel:
            PropName := DoOnOIGetListPropertyItemName(CategoryIndex, PropertyIndex, PropertyItemIndex);

          else
            PropName := 'PropNameNotFound';
        end;
      end;

      PropName := UpperCase(PropName);

      Visibility := (SearchString = '') or (Pos(SearchString, PropName) > 0);
      vstOI.IsVisible[Node] := Visibility or NodeData^.ItemVisible;

      if Visibility then //mark all parent nodes as visible, so they won't be hidden later
        MarkAllParentNodesAsVisible(Node);

      Node := vstOI.GetPrevious(Node);
    until Node = nil;
  finally
    vstOI.EndUpdate;
    vstOI.UpdateRanges;
    vstOI.UpdateScrollBars(True);
    vstOI.Repaint;
  end;
end;


function TfrObjectInspector.IsVstVertScrollBarVisible(AVst: TVirtualStringTree): Boolean;
begin                                                                    //This check is not reliable.
  //if (AVst.Canvas.ClipRect.Right = 2000) and (Screen.Width < 2000) then  //ToDo:   find out what this 2000 means.
  //begin                                                                  //This value came out from debugging on a 1920x1080 screen.
  //  Result := True;
  //  Exit;
  //end;
  //
  //Result := AVst.Width - (AVst.Canvas.ClipRect.Right - AVst.Canvas.ClipRect.Left) > 10;

  Result := FVstVertScrollBarVisible;
end;


function TfrObjectInspector.GetVertScrollBarWidth: Integer;
begin
  Result := 20 * Ord(IsVstVertScrollBarVisible(vstOI));
end;


function TfrObjectInspector.GetTextEditorWidth: Integer;
begin
  Result := GetLocalComboEditorWidth(GetVertScrollBarWidth) - CTextEditorSpacing;  ///////////////////// - icon width - icon spacing
end;


procedure TfrObjectInspector.SetTextEditorEditPosAndSize;
begin
  try
    FTextEditorEditBox.Left := GetLocalComboEditorLeft;
  except
  end;

  try
    FTextEditorEditBox.Width := GetTextEditorWidth;
  except
  end;
end;


procedure TfrObjectInspector.tmrSetEditBoxTimer(Sender: TObject);
var
  NodeData: PNodeDataPropertyRec;
begin
  tmrSetEditBox.Enabled := False;

  NodeData := vstOI.GetNodeData(FEditingNode);
  if NodeData = nil then
    Exit;

  SetTextEditorEditPosAndSize;

  case NodeData^.EditorType of
    etSpinText:
    begin
      FupdownTextEditor := TUpDown.Create(Self);
      FupdownTextEditor.Parent := FTextEditorEditBox.Parent;//Self;
      FupdownTextEditor.Width := 17;
      FupdownTextEditor.Height := FTextEditorEditBox.Height - 4;
      FupdownTextEditor.Left := GetLocalEditorLeft(FupdownTextEditor.Width, GetVertScrollBarWidth) - CTextEditorSpacing; //vstOI.Left + vstOI.Header.Columns.Items[2].Left - FupdownTextEditor.Width - CTextEditorSpacing; ///////////////////// - icon width - icon spacing
      FupdownTextEditor.Top := FTextEditorEditBox.Top + 2;
      FupdownTextEditor.Flat := True;
      FupdownTextEditor.ParentColor := False;
      FupdownTextEditor.OnChangingEx := updownTextEditorChangingEx;

      FupdownTextEditor.Visible := True;
      FupdownTextEditor.BringToFront;
    end;

    etTextWithArrow:
    begin
      //CreateArrowButton(FEditingNode, GetVertScrollBarWidth, etTextWithArrow);
      FSpdBtnArrowProperty := TSpeedButton.Create(Self);
      FSpdBtnArrowProperty.Parent := FTextEditorEditBox; //using the edtibox as parent, to automatically move the arrow button on scrolling
      FSpdBtnArrowProperty.Width := 17;
      FSpdBtnArrowProperty.Height := FTextEditorEditBox.Height - 4;
      FSpdBtnArrowProperty.Left := FTextEditorEditBox.Width - FSpdBtnArrowProperty.Width - 4; ///////////////////// - icon width - icon spacing
      FSpdBtnArrowProperty.Top := 0;
      FSpdBtnArrowProperty.Flat := True;
      FSpdBtnArrowProperty.Glyph.Assign(imgDownArrow.Picture.Bitmap);
      FSpdBtnArrowProperty.OnClick := spdbtnArrowPropertyClick;
      FSpdBtnArrowProperty.Anchors := [akRight, akTop];

      FSpdBtnArrowProperty.Visible := True;
      FSpdBtnArrowProperty.BringToFront;
    end

    else
      ;
  end; //case
end;


procedure TfrObjectInspector.tmrColCmbDroppedTimer(Sender: TObject);
begin
  tmrColCmbDropped.Enabled := False;

  if FEdtColorProperty = nil then
    Exit;

  if FcolcmbPropertyIsDropped then
    Exit; //do not destroy the editor if it has focus

  FEditingText := FEdtColorProperty.Text;
  vstOIEdited(vstOI, FEditingNode, 1);

  FreeAndNil(FColcmbProperty);
  FreeAndNil(FEdtColorProperty);
  vstOI.Header.Options := vstOI.Header.Options + [hoColumnResize];
end;


procedure TfrObjectInspector.edtSearchChange(Sender: TObject);
begin
  tmrSearch.Enabled := True;
end;


procedure TfrObjectInspector.MenuItem_ShowHideSearchBoxClick(Sender: TObject);
begin
  edtSearch.Visible := not edtSearch.Visible;

  if edtSearch.Visible then
    vstOI.Height := pnlvstOI.Height
  else
    vstOI.Height := Height;
end;


procedure TfrObjectInspector.SetDataTypeVisible(Value: Boolean);
begin
  if FDataTypeVisible <> Value then
  begin
    FDataTypeVisible := Value;

    if Value then
      vstOI.Header.Columns.Items[2].Options := vstOI.Header.Columns.Items[2].Options + [coVisible]
    else
      vstOI.Header.Columns.Items[2].Options := vstOI.Header.Columns.Items[2].Options - [coVisible];
  end;
end;


procedure TfrObjectInspector.SetExtraInfoVisible(Value: Boolean);
begin
  if FExtraInfoVisible <> Value then
  begin
    FExtraInfoVisible := Value;

    if Value then
      vstOI.Header.Columns.Items[3].Options := vstOI.Header.Columns.Items[3].Options + [coVisible]
    else
      vstOI.Header.Columns.Items[3].Options := vstOI.Header.Columns.Items[3].Options - [coVisible];
  end;
end;


procedure TfrObjectInspector.SetPropertyItemHeight(Value: Integer);
begin
  if FPropertyItemHeight <> Value then
  begin
    FPropertyItemHeight := Value;

    if FPropertyItemHeight < Integer(vstOI.DefaultNodeHeight) then
      FPropertyItemHeight := Integer(vstOI.DefaultNodeHeight);

    if FPropertyItemHeight > 300 then
      FPropertyItemHeight := 300;
  end;
end;


function TfrObjectInspector.DoOnOIGetCategoryCount: Integer;
begin
  if not Assigned(FOnOIGetCategoryCount) then
    raise Exception.Create('OnOIGetCategoryCount not assigned.')
  else
    Result := FOnOIGetCategoryCount;
end;


function TfrObjectInspector.DoOnOIGetCategory(AIndex: Integer): string;
begin
  if not Assigned(FOnOIGetCategory) then
    raise Exception.Create('OnOIGetCategory not assigned.')
  else
    Result := FOnOIGetCategory(AIndex);
end;


function TfrObjectInspector.DoOnOIGetPropertyCount(ACategoryIndex: Integer): Integer;
begin
  if not Assigned(FOnOIGetPropertyCount) then
    raise Exception.Create('OnOIGetPropertyCount not assigned.')
  else
    Result := FOnOIGetPropertyCount(ACategoryIndex);
end;


function TfrObjectInspector.DoOnOIGetPropertyName(ACategoryIndex, APropertyIndex: Integer): string;
begin
  if not Assigned(FOnOIGetPropertyName) then
    raise Exception.Create('OnOIGetPropertyName not assigned.')
  else
    Result := FOnOIGetPropertyName(ACategoryIndex, APropertyIndex);
end;


function TfrObjectInspector.DoOnOIGetPropertyValue(ACategoryIndex, APropertyIndex: Integer; var AEditorType: TOIEditorType): string;
begin
  if not Assigned(FOnOIGetPropertyValue) then
    raise Exception.Create('OnOIGetPropertyValue not assigned.')
  else
    Result := FOnOIGetPropertyValue(ACategoryIndex, APropertyIndex, AEditorType);
end;


function TfrObjectInspector.DoOnOIGetListPropertyItemCount(ACategoryIndex, APropertyIndex: Integer): Integer;
begin
  if not Assigned(FOnOIGetListPropertyItemCount) then
    raise Exception.Create('OnOIGetListPropertyItemCount not assigned.')
  else
    Result := FOnOIGetListPropertyItemCount(ACategoryIndex, APropertyIndex);
end;


function TfrObjectInspector.DoOnOIGetListPropertyItemName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  if not Assigned(FOnOIGetListPropertyItemName) then
    raise Exception.Create('OnOIGetListPropertyItemName not assigned.')
  else
    Result := FOnOIGetListPropertyItemName(ACategoryIndex, APropertyIndex, AItemIndex);
end;


function TfrObjectInspector.DoOnOIGetListPropertyItemValue(ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AEditorType: TOIEditorType): string;
begin
  if not Assigned(FOnOIGetListPropertyItemValue) then
    raise Exception.Create('OnOIGetListPropertyItemValue not assigned.')
  else
    Result := FOnOIGetListPropertyItemValue(ACategoryIndex, APropertyIndex, AItemIndex, AEditorType);
end;


function TfrObjectInspector.DoOnUIGetDataTypeName(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  if not Assigned(FOnUIGetDataTypeName) then
    raise Exception.Create('OnUIGetDataTypeName not assigned.')
  else
    Result := FOnUIGetDataTypeName(ACategoryIndex, APropertyIndex, AItemIndex);
end;


function TfrObjectInspector.DoOnUIGetExtraInfo(ACategoryIndex, APropertyIndex, AItemIndex: Integer): string;
begin
  if not Assigned(FOnUIGetExtraInfo) then
    raise Exception.Create('OnUIGetExtraInfo not assigned.')
  else
    Result := FOnUIGetExtraInfo(ACategoryIndex, APropertyIndex, AItemIndex);
end;


procedure TfrObjectInspector.DoOnOIGetImageIndexEx(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
begin
  if not Assigned(FOnOIGetImageIndexEx) then
    raise Exception.Create('OnOIGetImageIndexEx not assigned.')
  else
    FOnOIGetImageIndexEx(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, Kind, Column, Ghosted, ImageIndex, ImageList);
end;


procedure TfrObjectInspector.DoOnOIEditedText(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; ANewText: string);
begin
  if not Assigned(FOnOIEditedText) then
    raise Exception.Create('OnOIEditedText not assigned.')
  else
  begin
    try
      FOnOIEditedText(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, ANewText);
    except
      on E: Exception do
        MessageBox(Handle, PChar(E.Message), PChar(Application.Title), MB_ICONERROR);
    end;
  end;
end;


function TfrObjectInspector.DoOnOIEditItems(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewItems: string): Boolean;
begin
  if not Assigned(FOnOIEditedText) then
    raise Exception.Create('OnOIEditItems not assigned.')
  else
    Result := FOnOIEditItems(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, ANewItems);
end;


function TfrObjectInspector.DoOnOIGetColorConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
begin
  if not Assigned(FOnOIGetColorConstsCount) then
    raise Exception.Create('OnOIGetColorConstsCount not assigned.')
  else
    Result := FOnOIGetColorConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
end;


procedure TfrObjectInspector.DoOnOIGetColorConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AColorItemIndex: Integer; var AColorName: string; var AColorValue: Int64);
begin
  if not Assigned(FOnOIGetColorConst) then
    raise Exception.Create('OnOIGetColorConst not assigned.')
  else
    FOnOIGetColorConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AColorItemIndex, AColorName, AColorValue);
end;


function TfrObjectInspector.DoOnOIGetEnumConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer): Integer;
begin
  if not Assigned(FOnOIGetEnumConstsCount) then
    raise Exception.Create('OnOIGetEnumConstsCount not assigned.')
  else
    Result := FOnOIGetEnumConstsCount(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
end;


procedure TfrObjectInspector.DoOnOIGetEnumConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex: Integer; var AEnumItemName: string);
begin
  if not Assigned(FOnOIGetEnumConst) then
    raise Exception.Create('OnOIGetEnumConst not assigned.')
  else
    FOnOIGetEnumConst(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AEnumItemIndex, AEnumItemName);
end;


procedure TfrObjectInspector.DoOnOIPaintText(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
  const TargetCanvas: TCanvas; Column: TColumnIndex; var TextType: TVSTTextType);
begin
  if not Assigned(FOnOIPaintText) then
    raise Exception.Create('OnOIPaintText not assigned.')
  else
    FOnOIPaintText(ANodeData, ACategoryIndex, APropertyIndex, APropertyItemIndex, TargetCanvas, Column, TextType);
end;


procedure TfrObjectInspector.DoOnOIBeforeCellPaint(ANodeData: TNodeDataPropertyRec; ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer;
  TargetCanvas: TCanvas; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if not Assigned(FOnOIBeforeCellPaint) then
    raise Exception.Create('OnOIBeforeCellPaint not assigned.')
  else
    FOnOIBeforeCellPaint(ANodeData, ACategoryIndex, APropertyIndex, APropertyItemIndex, TargetCanvas, Column, CellPaintMode, CellRect, ContentRect);
end;


procedure TfrObjectInspector.DoOnOITextEditorMouseDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FOnOITextEditorMouseDown) then
    raise Exception.Create('OnOITextEditorMouseDown not assigned.')
  else
    FOnOITextEditorMouseDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, Sender, Button, Shift, X, Y);
end;


function TfrObjectInspector.DoOnOITextEditorMouseMove(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  if not Assigned(FOnOITextEditorMouseMove) then
    raise Exception.Create('OnOITextEditorMouseMove not assigned.')
  else
    Result := FOnOITextEditorMouseMove(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, Sender, Shift, X, Y);
end;


procedure TfrObjectInspector.DoOnOITextEditorKeyUp(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not Assigned(FOnOITextEditorKeyUp) then
    Exit;//raise Exception.Create('OnOITextEditorKeyUp not assigned.')

  FOnOITextEditorKeyUp(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, Sender, Key, Shift);
end;


procedure TfrObjectInspector.DoOnOITextEditorKeyDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not Assigned(FOnOITextEditorKeyDown) then
    Exit;//raise Exception.Create('OnOITextEditorKeyDown not assigned.')

  FOnOITextEditorKeyDown(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, Sender, Key, Shift);
end;


procedure TfrObjectInspector.DoOnOIEditorAssignMenuAndTooltip(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  Sender: TObject; var APopupMenu: TPopupMenu; var AHint: string; var AShowHint: Boolean);
begin
  if not Assigned(FOnOIEditorAssignMenuAndTooltip) then
    Exit; //Do not raise exception for this event. It is not mandatory.

  FOnOIEditorAssignMenuAndTooltip(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, Sender, APopupMenu, AHint, AShowHint);
end;


procedure TfrObjectInspector.DoOnOIGetFileDialogSettings(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var AFilter, AInitDir: string);
begin
  if not Assigned(FOnOIGetFileDialogSettings) then
  begin
    AFilter := '';
    AInitDir := '';
    Exit; //Do not raise exception for this event. It is not mandatory.
  end;

  FOnOIGetFileDialogSettings(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AFilter, AInitDir);
end;


procedure TfrObjectInspector.DoOnOIArrowEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer);
begin
  if not Assigned(FOnOIArrowEditorClick) then
    Exit;  //Do not raise exception for this event. It is not mandatory.

  FOnOIArrowEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex);
end;


procedure TfrObjectInspector.DoOnOIUserEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ARepaintValue: Boolean);
begin
  if not Assigned(FOnOIUserEditorClick) then
    Exit;  //Do not raise exception for this event. It is not mandatory.

  FOnOIUserEditorClick(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, ARepaintValue);
end;


function TfrObjectInspector.DoOnOIBrowseFile(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer;
  AFilter, ADialogInitDir: string; var Handled: Boolean; AReturnMultipleFiles: Boolean = False): string;
begin
  if not Assigned(FOnOIBrowseFile) then
    Result := BrowseFile(AFilter, ADialogInitDir, AReturnMultipleFiles)
  else
  begin
    Result := FOnOIBrowseFile(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, AFilter, ADialogInitDir, Handled, AReturnMultipleFiles);

    if not Handled then
      Result := BrowseFile(AFilter, ADialogInitDir, AReturnMultipleFiles);
  end;
end;


procedure TfrObjectInspector.DoOnAfterSpinTextEditorChanging(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex: Integer; var ANewValue: string);
begin
  if not Assigned(FOnAfterSpinTextEditorChanging) then
    Exit;  //Do not raise exception for this event. It is not mandatory.

  FOnAfterSpinTextEditorChanging(ANodeLevel, ACategoryIndex, APropertyIndex, AItemIndex, ANewValue);
end;


procedure TfrObjectInspector.CancelCurrentEditing;
begin
  vstOI.CancelEditNode; //destroy the text editor, to avoid updating to a new value
end;


procedure TfrObjectInspector.ReloadContent;
var
  i, j, k: Integer;
  CategoryCount, PropertyCount, PropertyItemCount: Integer;
  CategoryNode, PropertyNode, PropertyItemNode: PVirtualNode;
  NodeData: PNodeDataPropertyRec;
begin
  CancelCurrentEditing;

  vstOI.BeginUpdate;
  try
    vstOI.Clear;

    CategoryCount := DoOnOIGetCategoryCount;

    for i := 0 to CategoryCount - 1 do
    begin
      CategoryNode := vstOI.AddChild(vstOI.RootNode);
      PropertyCount := DoOnOIGetPropertyCount(i);

      NodeData := vstOI.GetNodeData(CategoryNode);
      NodeData^.Level := CCategoryLevel;
      NodeData^.PropertyIndex := i;

      for j := 0 to PropertyCount - 1 do
      begin
        PropertyNode := vstOI.AddChild(CategoryNode);
        PropertyItemCount := DoOnOIGetListPropertyItemCount(i, j);

        NodeData := vstOI.GetNodeData(PropertyNode);
        NodeData^.Level := CPropertyLevel;
        NodeData^.PropertyIndex := j;

        if FListItemsVisible then
        begin
          for k := 0 to PropertyItemCount - 1 do
          begin
            PropertyItemNode := vstOI.AddChild(PropertyNode);

            NodeData := vstOI.GetNodeData(PropertyItemNode);
            NodeData^.Level := CPropertyItemLevel;
            NodeData^.PropertyIndex := k;

            vstOI.NodeHeight[PropertyItemNode] := FPropertyItemHeight;
          end;
        end;
      end;

      vstOI.Expanded[CategoryNode] := True;
    end;
  finally
    vstOI.EndUpdate;
  end;
end;


function TfrObjectInspector.GetCategoryNodeByIndex(ACategoryIndex: Integer): PVirtualNode;
var
  CategoryNode: PVirtualNode;
  NodeData: PNodeDataPropertyRec;
begin
  Result := nil;
  CategoryNode := vstOI.GetFirst;
  if CategoryNode = nil then
    Exit;

  repeat
    NodeData := vstOI.GetNodeData(CategoryNode);
    if not Assigned(NodeData) then
      Continue;

    if NodeData^.PropertyIndex = ACategoryIndex then
    begin
      Result := CategoryNode;
      Break;
    end;

    CategoryNode := CategoryNode^.NextSibling;
  until CategoryNode = nil;
end;


function TfrObjectInspector.GetPropertyNodeByIndex(ACategoryIndex, APropertyIndex: Integer): PVirtualNode;
var
  CategoryNode, PropertyNode: PVirtualNode;
  NodeData: PNodeDataPropertyRec;
begin
  Result := nil;
  CategoryNode := GetCategoryNodeByIndex(ACategoryIndex);
  if CategoryNode = nil then
    Exit;

  PropertyNode := vstOI.GetFirstChild(CategoryNode);
  if PropertyNode = nil then
    Exit;

  repeat
    NodeData := vstOI.GetNodeData(PropertyNode);
    if not Assigned(NodeData) then
      Continue;

    if NodeData^.PropertyIndex = APropertyIndex then
    begin  //Found the node
      Result := PropertyNode;
      Break;
    end;

    PropertyNode := PropertyNode^.NextSibling;
  until PropertyNode = nil;
end;


function TfrObjectInspector.GetPropertyItemNodeByIndex(ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer): PVirtualNode;
var
  PropertyNode, ItemNode: PVirtualNode;
  NodeData: PNodeDataPropertyRec;
begin
  Result := nil;
  PropertyNode := GetPropertyNodeByIndex(ACategoryIndex, APropertyIndex);
  if PropertyNode = nil then
    Exit;

  ItemNode := vstOI.GetFirstChild(PropertyNode);
  if ItemNode = nil then
    Exit;

  repeat
    NodeData := vstOI.GetNodeData(ItemNode);
    if not Assigned(NodeData) then
      Continue;

    if NodeData^.PropertyIndex = APropertyItemIndex then
    begin  //Found the node
      Result := ItemNode;
      Break;
    end;

    ItemNode := ItemNode^.NextSibling;
  until ItemNode = nil;
end;


function TfrObjectInspector.GetNodeByLevel(ANodeLevel, ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer): PVirtualNode;
begin
  case ANodeLevel of
    CCategoryLevel:
      Result := GetCategoryNodeByIndex(ACategoryIndex);

    CPropertyLevel:
      Result := GetPropertyNodeByIndex(ACategoryIndex, APropertyIndex);

    CPropertyItemLevel:
      Result := GetPropertyItemNodeByIndex(ACategoryIndex, APropertyIndex, APropertyItemIndex);

    else
      Result := nil;
  end;
end;


procedure TfrObjectInspector.RepaintNodeByLevel(ANodeLevel, ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer; AScrollIntoView: Boolean = True);
var
  Node: PVirtualNode;
begin
  Node := GetNodeByLevel(ANodeLevel, ACategoryIndex, APropertyIndex, APropertyItemIndex);
  if Node = nil then
    Exit;

  vstOI.InvalidateNode(Node);
  if AScrollIntoView then
    vstOI.ScrollIntoView(Node, False);
end;

//there can be an optimization by caching all pointers to nodes, into arrays of arrays, which can be indexed by ACategoryIndex, APropertyIndex, APropertyItemIndex
//this cache has to be rebuilt on every call to ReloadPropertyItems and ReloadContent. (and adding / removing various items like files)


procedure TfrObjectInspector.ReloadPropertyItems(ACategoryIndex, APropertyIndex: Integer; ACloseItemEditor: Boolean = False);
var
  PropertyNode, PropertyItemNode: PVirtualNode;
  i: Integer;
  PropertyItemCount: Integer;
  NodeData: PNodeDataPropertyRec;
begin
  if not FListItemsVisible then
    Exit;

  PropertyNode := GetPropertyNodeByIndex(ACategoryIndex, APropertyIndex);
  if PropertyNode = nil then
    Exit;

  CancelCurrentEditing;

  if ACloseItemEditor then
    FreeEditorComponents;

  vstOI.DeleteChildren(PropertyNode);
  PropertyItemCount := DoOnOIGetListPropertyItemCount(ACategoryIndex, APropertyIndex);

  for i := 0 to PropertyItemCount - 1 do
  begin
    PropertyItemNode := vstOI.AddChild(PropertyNode);

    NodeData := vstOI.GetNodeData(PropertyItemNode);
    NodeData^.Level := CPropertyItemLevel;
    NodeData^.PropertyIndex := i;

    vstOI.NodeHeight[PropertyItemNode] := FPropertyItemHeight;
  end;

  vstOI.InvalidateNode(PropertyNode);
  vstOI.Expanded[PropertyNode] := True;
end;


procedure TfrObjectInspector.vstOICompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
begin
  Result := 0; //CompareStr(Prop1Name, Prop2Name);
end;


procedure TfrObjectInspector.AssignPopupMenuAndTooltipToEditor(AEditor: TControl);
var
  EditorPopupMenu: TPopupMenu;
  EditorHint: string;
  EditorShowHint: Boolean;
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit; //prevent AV

  EditorPopupMenu := nil;
  EditorHint := '';
  EditorShowHint := False;
  DoOnOIEditorAssignMenuAndTooltip(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, AEditor, EditorPopupMenu, EditorHint, EditorShowHint);
  AEditor.PopupMenu := EditorPopupMenu;
  AEditor.ShowHint := EditorShowHint;
  AEditor.Hint := EditorHint;
end;


procedure TfrObjectInspector.vstOICreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out
  EditLink: IVTEditLink);
var
  TempStringEditLink: TStringEditLink;
begin
  TempStringEditLink := TStringEditLink.Create;
  EditLink := TempStringEditLink;

  FTextEditorEditBox := TEdit(TCustomEdit(TempStringEditLink.Edit));

  FTextEditorEditBox.OnMouseDown := TextEditorMouseDown;
  FTextEditorEditBox.OnMouseMove := TextEditorMouseMove;
  FTextEditorEditBox.OnKeyUp := TextEditorKeyUp;
  FTextEditorEditBox.OnKeyDown := TextEditorKeyDown;

  FEditingNode := Node;
  FEditingColumn := Column;

  FTextEditorEditBox.Left := GetLocalComboEditorLeft;
  tmrSetEditBox.Enabled := True;
  AssignPopupMenuAndTooltipToEditor(FTextEditorEditBox);
end;


procedure TfrObjectInspector.vstOIDblClick(Sender: TObject);
begin
  //
end;


procedure TfrObjectInspector.vstOIEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
begin
  if Assigned(FColcmbProperty) then
  begin
    vstOI.Header.Options := vstOI.Header.Options + [hoColumnResize];
    FreeAndNil(FColcmbProperty);
  end;

  if Assigned(FupdownTextEditor) then
    FreeAndNil(FupdownTextEditor);

  //if FBtnArrowProperty <> nil then   //not sure if needed
  //  FreeAndNil(FBtnArrowProperty);

  if FSpdBtnArrowProperty <> nil then
    FreeAndNil(FSpdBtnArrowProperty);

  //if Assigned(FTextEditorEditBox) then
  //  FreeAndNil(FTextEditorEditBox);    //this is freed by the VST itself, so it should not be freed again here
  FTextEditorEditBox := nil; //it has to be set to nil, though
end;


procedure TfrObjectInspector.vstOIEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  if not GetNodeIndexInfo(Node, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit;

  DoOnOIEditedText(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, FEditingText);

  if Assigned(FupdownTextEditor) then
    FreeAndNil(FupdownTextEditor);

  //if FBtnArrowProperty <> nil then   //not sure if needed
  //  FreeAndNil(FBtnArrowProperty);

  if FSpdBtnArrowProperty <> nil then
    FreeAndNil(FSpdBtnArrowProperty);

  FTextEditorEditBox := nil;
end;


function TfrObjectInspector.GetNodeIndexInfo(ANode: PVirtualNode; out ANodeLevel, ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer): Boolean;
var
  CategoryNodeData, PropertyNodeData, NodeData: PNodeDataPropertyRec;
begin
  NodeData := vstOI.GetNodeData(ANode);
  Result := False;

  ANodeLevel := -1;
  ACategoryIndex := -1;
  APropertyIndex := -1;
  APropertyItemIndex := -1;

  if NodeData = nil then
    Exit;

  ANodeLevel := NodeData^.Level;

  case NodeData^.Level of
    CCategoryLevel:
    begin
      ACategoryIndex := NodeData^.PropertyIndex;
      APropertyIndex := NodeData^.PropertyIndex;
      APropertyItemIndex := -1;
    end;

    CPropertyLevel:
    begin
      CategoryNodeData := vstOI.GetNodeData(ANode^.Parent);
      PropertyNodeData := NodeData;

      if CategoryNodeData = nil then
        Exit;

      ACategoryIndex := CategoryNodeData^.PropertyIndex;
      APropertyIndex := PropertyNodeData^.PropertyIndex;
      APropertyItemIndex := -1;
    end;

    CPropertyItemLevel:
    begin
      PropertyNodeData := vstOI.GetNodeData(ANode^.Parent);
      if PropertyNodeData = nil then
        Exit;

      CategoryNodeData := vstOI.GetNodeData(ANode^.Parent^.Parent);
      if CategoryNodeData = nil then
        Exit;

      ACategoryIndex := CategoryNodeData^.PropertyIndex;
      APropertyIndex := PropertyNodeData^.PropertyIndex;
      APropertyItemIndex := NodeData^.PropertyIndex;
    end;
  end;

  Result := True;
end;


function TfrObjectInspector.GetPropertyValueForEditor(ANodeLevel, ACategoryIndex, APropertyIndex, APropertyItemIndex: Integer; ACurrentEditorType: TOIEditorType): string;
begin
  Result := '';

  case ANodeLevel of
    CCategoryLevel:
      Result := '';

    CPropertyLevel:
      Result := DoOnOIGetPropertyValue(ACategoryIndex, APropertyIndex, ACurrentEditorType);

    CPropertyItemLevel:
      Result := DoOnOIGetListPropertyItemValue(ACategoryIndex, APropertyIndex, APropertyItemIndex, ACurrentEditorType);
  end;
end;


function TfrObjectInspector.GetLocalEditorLeft(AEditorWidth, AVertScrollBarWidth: Integer): Integer;
begin
  Result := Max(vstOI.Header.Columns.Items[1].Left + AEditorWidth shl 1, Min(Max(2, vstOI.Header.Columns.Items[2].Left - AEditorWidth), vstOI.Width - AEditorWidth - AVertScrollBarWidth));
end;


function TfrObjectInspector.GetLocalComboEditorLeft: Integer;
begin
  Result := Max(2, Min(vstOI.Header.Columns.Items[1].Left + vstOI.Left + 1, vstOI.Width));
end;


function TfrObjectInspector.GetLocalComboEditorWidth(AVertScrollBarWidth: Integer): Integer;
var
  CmbLeft, CmbRight: Integer;
begin
  CmbLeft := Max(3, vstOI.Header.Columns.Items[1].Left);
  CmbRight := Max(CMinComboWidth, Min(vstOI.Width - AVertScrollBarWidth, vstOI.Header.Columns.Items[2].Left));
  Result := CmbRight - CmbLeft;
end;


procedure TfrObjectInspector.CreateBooleanComboBox(Node: PVirtualNode; VertScrollBarWidth: Integer);
var
  PropValue: string;
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  vstOI.Header.Options := vstOI.Header.Options - [hoColumnResize];

  if FCmbBooleanProperty = nil then
    FCmbBooleanProperty := TComboBox.Create(Self);

  try
    FCmbBooleanProperty.Visible := False;  //sometimes, this causes AV
  except
  end;
  FCmbBooleanProperty.Parent := Self;
  FCmbBooleanProperty.Style := csOwnerDrawFixed; //for boolean
  FCmbBooleanProperty.ParentFont := False;
  FCmbBooleanProperty.Font.Style := [];
  FCmbBooleanProperty.Left := GetLocalComboEditorLeft;
  FCmbBooleanProperty.Top := vstOI.GetDisplayRect(Node, 1, False).Top + vstOI.Top {+ vstOI.Header.Height} + 2;
  FCmbBooleanProperty.Width := GetLocalComboEditorWidth(VertScrollBarWidth);

  FCmbBooleanProperty.ItemHeight := vstOI.DefaultNodeHeight - CTextEditorSpacing;

  if FCmbBooleanProperty.Width <= CMinComboWidth then
  begin
    FreeAndNil(FCmbBooleanProperty);
    Exit;
  end;

  FEditingNode := Node;
  FCmbBooleanProperty.OnSelect := cmbBooleanPropertySelect;
  FCmbBooleanProperty.OnExit := cmbBooleanPropertyExit;

  FCmbBooleanProperty.Clear;
  FCmbBooleanProperty.Items.Add('       False');
  FCmbBooleanProperty.Items.Add('       True');

  if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit; //prevent AV

  AssignPopupMenuAndTooltipToEditor(FCmbBooleanProperty);

  PropValue := GetPropertyValueForEditor(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, etBooleanCombo);
  FCmbBooleanProperty.ItemIndex := Ord(UpperCase(Trim(PropValue)) = 'TRUE');

  FCmbBooleanProperty.Visible := True;
  FCmbBooleanProperty.BringToFront;
  FCmbBooleanProperty.SetFocus;
end;


procedure TfrObjectInspector.CreateBrowseEditorButton(Node: PVirtualNode; VertScrollBarWidth: Integer; AUsedForComboBox, AUsedForPath: Boolean);
var
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  vstOI.Header.Options := vstOI.Header.Options - [hoColumnResize];

  if FBtnItemsProperty = nil then
    FBtnItemsProperty := TButton.Create(Self);

  //FBtnItemsProperty.Visible := False;   //sometimes, this causes AV
  FBtnItemsProperty.Parent := Self;
  FBtnItemsProperty.ParentFont := False;
  FBtnItemsProperty.Font.Style := [];
  FBtnItemsProperty.Font.Name := 'Tahoma';
  FBtnItemsProperty.Width := vstOI.DefaultNodeHeight + 3;//+3
  FBtnItemsProperty.Left := GetLocalEditorLeft(FBtnItemsProperty.Width, VertScrollBarWidth);
  FBtnItemsProperty.Top := vstOI.GetDisplayRect(Node, 1, False).Top + vstOI.Top {+ vstOI.Header.Height} + 3;
  FBtnItemsProperty.Height := vstOI.NodeHeight[Node] - 2; //vstOI.DefaultNodeHeight - 2;
  FBtnItemsProperty.Caption := '•••';//'';    #149 vs. #7
  FBtnItemsProperty.Font.Size := 5; //7

  if FBtnItemsProperty.Left > vstOI.Width - FBtnItemsProperty.Width - 14 then   //14, just to look good and not stay over the scrollbar
  begin
    FreeAndNil(FBtnItemsProperty);
    Exit;
  end;

  FBtnItemsProperty.Tag := Ord(AUsedForComboBox);

  FEditingNode := Node;
  FBtnItemsProperty.OnClick := btnItemsPropertyClick;
  FBtnItemsProperty.OnExit := btnItemsPropertyExit;

  AssignPopupMenuAndTooltipToEditor(FBtnItemsProperty);

  if AUsedForPath then
  begin
    FEdtPath := TEdit.Create(Self);
    FEdtPath.Parent := Self;
    FEdtPath.Width := GetLocalComboEditorWidth(VertScrollBarWidth) - FBtnItemsProperty.Width - 1;
    FEdtPath.Left := GetLocalComboEditorLeft;
    FEdtPath.Top := FBtnItemsProperty.Top;
    FEdtPath.Height := FBtnItemsProperty.Height;

    if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
      Exit; //prevent AV

    FEdtPath.Text := GetPropertyValueForEditor(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, etFilePath);  //can also be etDirPath;

    FEdtPath.OnExit := EdtPathExit;

    //AssignPopupMenuAndTooltipToEditor(FEdtPath);
    FEdtPath.Visible := True;
    FEdtPath.BringToFront;
  end;

  FBtnItemsProperty.Visible := True;
  FBtnItemsProperty.BringToFront;
  FBtnItemsProperty.SetFocus;

  if AUsedForPath then
    FEdtPath.SetFocus;
end;


procedure TfrObjectInspector.CreateArrowButton(Node: PVirtualNode; VertScrollBarWidth: Integer);
begin
  vstOI.Header.Options := vstOI.Header.Options - [hoColumnResize];

  if FBtnArrowProperty = nil then
    FBtnArrowProperty := TBitBtn.Create(Self);

  //FBtnArrowProperty.Visible := False; //sometimes, this causes AV
  FBtnArrowProperty.Parent := Self;

  if FBtnItemsProperty = nil then
  begin
    //raise Exception.Create('The arrow button does not support being on its own yet.');
    FreeAndNil(FBtnArrowProperty);
    Exit;
  end;

  FBtnArrowProperty.Width := CDefaultArrowButonWidth;
  FBtnArrowProperty.Left := FBtnItemsProperty.Left - FBtnArrowProperty.Width + 1;
  FBtnArrowProperty.Top := vstOI.GetDisplayRect(Node, 1, False).Top + vstOI.Top {+ vstOI.Header.Height} + 3;
  FBtnArrowProperty.Height := vstOI.NodeHeight[Node] - 2; //vstOI.DefaultNodeHeight - 2;
  FBtnArrowProperty.Caption := '';
  FBtnArrowProperty.Glyph.Assign(imgDownArrow.Picture.Bitmap);
  FBtnArrowProperty.Glyph.Transparent := True;
  FBtnArrowProperty.Spacing := 0;

  if FBtnArrowProperty.Left < 2 then
    Exit; //just exit and leave the button hidden. Do not destroy it.

  //FEditingNode := Node;
  FBtnArrowProperty.OnClick := btnArrowPropertyClick;
  FBtnArrowProperty.OnExit := btnArrowPropertyExit;

  AssignPopupMenuAndTooltipToEditor(FBtnArrowProperty);

  FBtnArrowProperty.Visible := True;
  FBtnArrowProperty.BringToFront;
  FBtnArrowProperty.SetFocus;
end;


procedure TfrObjectInspector.CreateColorComboBox(Node: PVirtualNode; VertScrollBarWidth: Integer);
var
  PropValue: string;
  CategoryIndex, PropertyIndex, PropertyItemIndex, NodeLevel: Integer;
  i: Integer;
  ColorName: string;
  ColorValue: Int64;
begin
  vstOI.Header.Options := vstOI.Header.Options - [hoColumnResize];

  FEditingNode := Node;
  if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit; //prevent AV

  if FColcmbProperty <> nil then
    FreeAndNil(FColcmbProperty);

  if FEdtColorProperty <> nil then
    FreeAndNil(FEdtColorProperty);

  FColcmbProperty := TColorBox.Create(Self);
  try
    FColcmbProperty.Visible := False;    //sometimes, this causes AV
  except
  end;
  FColcmbProperty.ParentFont := False;
  FColcmbProperty.Font.Style := [];
  FColcmbProperty.Parent := Self;
  FColcmbProperty.Left := GetLocalComboEditorLeft;
  FColcmbProperty.Top := vstOI.GetDisplayRect(Node, 1, False).Top + vstOI.Top {+ vstOI.Header.Height} + 2;
  FColcmbProperty.Width := GetLocalComboEditorWidth(VertScrollBarWidth);
  FColcmbProperty.Height := vstOI.NodeHeight[Node] - 2;
  FColcmbProperty.ItemHeight := vstOI.DefaultNodeHeight - 5;

  if FColcmbProperty.Width < CMinComboWidth then
  begin
    FreeAndNil(FColcmbProperty);
    Exit;
  end;

  FColcmbProperty.OnSelect := colcmbPropertySelect;
  FColcmbProperty.OnExit := colcmbPropertyExit;
  FColcmbProperty.OnKeyDown := colcmbPropertyKeyDown;
  FColcmbProperty.OnKeyPress := colcmbPropertyKeyPress;
  FColcmbProperty.OnCloseUp := colcmbPropertyCloseUp;
  FColcmbProperty.OnDropDown := colcmbPropertyDropDown;
  FColcmbProperty.Style := [cbStandardColors, cbSystemColors, cbCustomColor, cbCustomColors, cbExtendedColors];

  for i := 0 to DoOnOIGetColorConstsCount(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) - 1 do
  begin
    DoOnOIGetColorConst(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, i, ColorName, ColorValue);
    FColcmbProperty.AddItem(ColorName, TObject(ColorValue));
  end;

  PropValue := GetPropertyValueForEditor(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, etColorCombo);

  FColcmbProperty.Visible := True;
  FColcmbProperty.ItemIndex := FColcmbProperty.Items.IndexOf(PropValue);

  if FColcmbProperty.ItemIndex = -1 then
  begin
    //FColcmbProperty.ItemIndex := 0; //custom
    case FColorFormat of
      cfHexa:
        FColcmbProperty.Selected := HexToInt(PropValue);

      cfDecimal:
        FColcmbProperty.Selected := StrToIntDef(PropValue, clRed);

      cfMixed:
      begin
        if Length(PropValue) = 0 then
          FColcmbProperty.Selected := clFuchsia
        else
          if ((Pos('$', PropValue) = 1) and (Length(PropValue) > 1) and (PropValue[Length(PropValue)] <> '$')) or
             (Pos('0x', PropValue) = 1) then
            FColcmbProperty.Selected := HexToInt(PropValue)
         else
           FColcmbProperty.Selected := StrToIntDef(PropValue, clRed);
      end;

      cfText:
        FColcmbProperty.Selected := clBlack;
    end;
  end;

  FColcmbProperty.AutoComplete := False;
  //FColcmbProperty.SetFocus;   //this call prevents proper creation of the editbox below

  FEdtColorProperty := TEdit.Create(Self);
  try
    FEdtColorProperty.Visible := False;  //sometimes, this causes AV
  except
  end;
  FEdtColorProperty.Parent := Self;
  FEdtColorProperty.Left := FColcmbProperty.Left;
  FEdtColorProperty.Top := FColcmbProperty.Top;
  FEdtColorProperty.Width := FColcmbProperty.Width - 16;   //16 is the arrow button width
  FEdtColorProperty.Height := FColcmbProperty.Height;
  FEdtColorProperty.Text := PropValue; //FColcmbProperty.Text;
  FEdtColorProperty.Font.Name := FColcmbProperty.Font.Name;
  FEdtColorProperty.Font.Size := FColcmbProperty.Font.Size;
  FEdtColorProperty.Font.CharSet := FColcmbProperty.Font.CharSet;

  FEdtColorProperty.OnExit := edtColorPropertyExit;
  FEdtColorProperty.OnKeyDown := edtColorPropertyKeyDown;

  AssignPopupMenuAndTooltipToEditor(FEdtColorProperty);  //not sure if this is enough to be assign on the text editor, or it should be also on the color box

  FEdtColorProperty.Visible := True;
  FEdtColorProperty.BringToFront;
  FEdtColorProperty.SetFocus;
end;


procedure TfrObjectInspector.CreateEnumComboBox(Node: PVirtualNode; VertScrollBarWidth: Integer; ACreateBrowseButton: Boolean = False);
var
  CategoryIndex, PropertyIndex, PropertyItemIndex, NodeLevel: Integer;
  i: Integer;
  EnumItemName: string;
begin
  vstOI.Header.Options := vstOI.Header.Options - [hoColumnResize];

  FCmbMiscEnumProperty := TComboBox.Create(Self);
  try
    FCmbMiscEnumProperty.Visible := False;   //sometimes, this causes AV
  except
  end;
  FCmbMiscEnumProperty.Parent := Self;
  FCmbMiscEnumProperty.Style := csOwnerDrawFixed; //for misc "enum" properties
  FCmbMiscEnumProperty.ParentFont := False;
  FCmbMiscEnumProperty.Font.Style := [];
  FCmbMiscEnumProperty.Left := GetLocalComboEditorLeft;
  FCmbMiscEnumProperty.Top := vstOI.GetDisplayRect(Node, 1, False).Top + vstOI.Top {+ vstOI.Header.Height} + 2;
  FCmbMiscEnumProperty.Width := GetLocalComboEditorWidth(VertScrollBarWidth);
  FCmbMiscEnumProperty.ItemHeight := vstOI.DefaultNodeHeight - 5;

  FEditingNode := Node;
  FCmbMiscEnumProperty.OnSelect := cmbMiscEnumPropertySelect;
  FCmbMiscEnumProperty.OnExit := cmbMiscEnumPropertyExit;

  if ACreateBrowseButton then
  begin
    CreateBrowseEditorButton(Node, VertScrollBarWidth, True, False);
    FCmbMiscEnumProperty.Width := FCmbMiscEnumProperty.Width - FBtnItemsProperty.Width - 1;
  end;

  FCmbMiscEnumProperty.Clear;

  if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit; //prevent AV

  for i := 0 to DoOnOIGetEnumConstsCount(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) - 1 do
  begin
    DoOnOIGetEnumConst(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, i, EnumItemName);
    FCmbMiscEnumProperty.Items.Add(CEmptySpaceForIcon + EnumItemName);
  end;

  FEditingText := GetPropertyValueForEditor(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, etEnumCombo);
  FCmbMiscEnumProperty.ItemIndex := FCmbMiscEnumProperty.Items.IndexOf(CEmptySpaceForIcon + FEditingText);

  AssignPopupMenuAndTooltipToEditor(FCmbMiscEnumProperty);
  FCmbMiscEnumProperty.Visible := True;
  FCmbMiscEnumProperty.SetFocus;
end;


procedure TfrObjectInspector.vstOIEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  CategoryIndex, PropertyIndex, PropertyItemIndex, NodeLevel: Integer;
  VertScrollBarWidth: Integer;
  NodeData: PNodeDataPropertyRec;
begin
  Allowed := False;

  if not GetNodeIndexInfo(Node, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit; //prevent AV

  Allowed := True;

  if NodeLevel = 0 then
  begin
    Allowed := False;
    Exit;
  end;

  NodeData := vstOI.GetNodeData(Node);
  if NodeData = nil then
  begin
    Allowed := False;
    Exit;
  end;

  VertScrollBarWidth := GetVertScrollBarWidth;

  case NodeData^.EditorType of
    etNone:
    begin
      Allowed := False;
      Exit;
    end;

    etText, etSpinText, etTextWithArrow:
      Allowed := Column = 1;

    etBooleanCombo:
    begin
      CreateBooleanComboBox(Node, VertScrollBarWidth);
      Allowed := False;
      Exit;
    end;

    etFilePath, etDirPath:
    begin
      CreateBrowseEditorButton(Node, VertScrollBarWidth, False, True);
      Allowed := False;
      Exit;
    end;

    etUserEditor:
    begin
      CreateBrowseEditorButton(Node, VertScrollBarWidth, False, False);
      Allowed := False;
      Exit;
    end;

    etFilePathWithArrow:
    begin
      CreateBrowseEditorButton(Node, VertScrollBarWidth, False, False);
      CreateArrowButton(Node, VertScrollBarWidth);
      Allowed := False;
      Exit;
    end;

    etColorCombo:
    begin
      CreateColorComboBox(Node, VertScrollBarWidth);
      Allowed := False;
      Exit;
    end;

    etEnumCombo:
    begin
      CreateEnumComboBox(Node, VertScrollBarWidth);
      Allowed := False;
      Exit;
    end;

    etEnumComboWithBtn:
    begin
      CreateEnumComboBox(Node, VertScrollBarWidth, True);
      Allowed := False;
      Exit;
    end;
  {else
    Exit; }
  end;

  FEditingText := GetPropertyValueForEditor(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, NodeData^.EditorType);
end;


procedure TfrObjectInspector.colcmbPropertyExit(Sender: TObject);
begin
  if FColcmbProperty = nil then
    Exit;

  FreeAndNil(FColcmbProperty);
  FreeAndNil(FEdtColorProperty);
  vstOI.Header.Options := vstOI.Header.Options + [hoColumnResize];

  FcolcmbPropertyIsDropped := False;
end;


procedure TfrObjectInspector.edtColorPropertyExit(Sender: TObject);
begin
  tmrColCmbDropped.Enabled := True;
end;


procedure TfrObjectInspector.edtColorPropertyKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    tmrColCmbDropped.Enabled := True;
end;


function TfrObjectInspector.GetColcmbPropertyAsText: string;
begin
  case FColorFormat of
    cfHexa:
      Result := {'$' +} IntToHex(FColcmbProperty.Selected, 6);  //let the user code add a prefix ('$' or '0x', and maybe '00')

    cfDecimal:
      Result := IntToStr(FColcmbProperty.Selected);

    cfMixed:
    begin
      if Pos('$', FEditingText) = 1 then
      begin
        if (Length(FEditingText) > 1) and (FEditingText[Length(FEditingText)] = '$') then
          Result := FEditingText
        else
          Result := {'$' +} IntToHex(FColcmbProperty.Selected, 6);  //let the user code add a prefix ('$' or '0x', and maybe '00')
      end
      else
        Result := IntToStr(FColcmbProperty.Selected);
    end;

    cfText:
      Result := FColcmbProperty.ColorNames[FColcmbProperty.ItemIndex];
  end; //case cf
end;


procedure TfrObjectInspector.colcmbPropertyCloseUp(Sender: TObject);
begin
  FcolcmbPropertyIsDropped := False;

  {$IFDEF FPC}
    if FColcmbProperty.ItemIndex = 0 then
    begin
      FEditingText := GetColcmbPropertyAsText;
      FEdtColorProperty.Text := FEditingText;
      vstOIEdited(vstOI, FEditingNode, 1);
    end;
  {$ENDIF}
end;


procedure TfrObjectInspector.colcmbPropertyDropDown(Sender: TObject);
begin
  FcolcmbPropertyIsDropped := True;
end;


procedure TfrObjectInspector.colcmbPropertySelect(Sender: TObject);
begin
  if FColcmbProperty = nil then
    Exit;

  FEditingText := GetColcmbPropertyAsText;
  FEdtColorProperty.Text := FEditingText;
  vstOIEdited(vstOI, FEditingNode, 1);
end;


procedure TfrObjectInspector.colcmbPropertyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  PastedText: string;
begin
  if ssCtrl in Shift then
  begin
    if Key = Ord('C') then
    begin
      if FColcmbProperty.ItemIndex > -1 then
        if FColcmbProperty.Items.Strings[FColcmbProperty.ItemIndex] = CCustomColorName then
          Clipboard.AsText := '$' + IntToHex(FColcmbProperty.Colors[FColcmbProperty.ItemIndex], 8)
        else
          Clipboard.AsText := FColcmbProperty.Items.Strings[FColcmbProperty.ItemIndex];

      Key := 0;
    end;

    if Key = Ord('V') then
    begin
      PastedText := Clipboard.AsText;
      if Pos(#10, PastedText) > 0 then
        Exit;

      if ValidHexColor(PastedText) or (FColcmbProperty.Items.IndexOf(PastedText) > -1) then
      begin
        FEditingText := PastedText;
        vstOIEdited(vstOI, FEditingNode, 1);
        colcmbPropertyExit(FColcmbProperty);
      end;

      Key := 0;
    end;
  end;
end;


procedure TfrObjectInspector.colcmbPropertyKeyPress(Sender: TObject; var Key: Char);
begin
  Key := #0;
end;


procedure TfrObjectInspector.cmbBooleanPropertyExit(Sender: TObject);
begin
  if FCmbBooleanProperty = nil then
    Exit;

  FreeAndNil(FCmbBooleanProperty);
  vstOI.Header.Options := vstOI.Header.Options + [hoColumnResize];
end;


procedure TfrObjectInspector.cmbBooleanPropertySelect(Sender: TObject);
begin
  if FCmbBooleanProperty = nil then
    Exit;

  FEditingText := Trim(FCmbBooleanProperty.Items[FCmbBooleanProperty.ItemIndex]);
  vstOIEdited(vstOI, FEditingNode, 1);
end;


procedure TfrObjectInspector.cmbMiscEnumPropertyExit(Sender: TObject);
begin
  if FCmbMiscEnumProperty = nil then
    Exit;

  if Assigned(FBtnItemsProperty) and FBtnItemsProperty.Focused and (FBtnItemsProperty.Tag = 1) then
    Exit;  //do not destroy the enum if the focus is on button

  FreeAndNil(FCmbMiscEnumProperty);
  vstOI.Header.Options := vstOI.Header.Options + [hoColumnResize];

  if FBtnItemsProperty = nil then
    Exit;

  if FBtnItemsProperty.Tag = 1 then //destroy the button here, if it has been created by the enum it is used for
    FreeAndNil(FBtnItemsProperty);
end;


procedure TfrObjectInspector.cmbMiscEnumPropertySelect(Sender: TObject);
begin
  if FCmbMiscEnumProperty = nil then
    Exit;

  FEditingText := Trim(FCmbMiscEnumProperty.Items[FCmbMiscEnumProperty.ItemIndex]);
  vstOIEdited(vstOI, FEditingNode, 1);
end;


procedure TfrObjectInspector.btnItemsPropertyExit(Sender: TObject);
begin
  if FBtnItemsProperty = nil then
    Exit;

  if (FBtnItemsProperty.Tag = 1) and Assigned(FCmbMiscEnumProperty) and FCmbMiscEnumProperty.Focused then
    Exit;  //do not destroy the button, if it has no focus, caused by the enum it is used for, it is destroyed by cmbMiscEnumPropertyExit

  if Assigned(FCmbMiscEnumProperty) and (FBtnItemsProperty.Tag = 1) then //destroy the button here, if it has been created by the enum it is used for
    FreeAndNil(FCmbMiscEnumProperty);

  if Assigned(FBtnArrowProperty) then
  begin
    if FBtnArrowProperty.Focused then
      Exit;

    FreeAndNil(FBtnArrowProperty);
  end;

  if Assigned(FEdtPath) then
  begin
    if FEdtPath.Focused then
      Exit;

    FreeAndNil(FEdtPath);
  end;

  FreeAndNil(FBtnItemsProperty);
  vstOI.Header.Options := vstOI.Header.Options + [hoColumnResize];
end;


procedure TfrObjectInspector.btnItemsPropertyClick(Sender: TObject);
var
  PropertyNodeData: PNodeDataPropertyRec;
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
  NewFileName, NewItems: string;
  DialogFilter, DialogInitDir: string;
  RepaintValue: Boolean;
  Handled: Boolean;

  procedure EditAsFileName(AReturnMultipleFiles: Boolean = False);
  begin
    DoOnOIGetFileDialogSettings(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, DialogFilter, DialogInitDir);
    Handled := False;
    NewFileName := DoOnOIBrowseFile(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, DialogFilter, DialogInitDir, Handled, AReturnMultipleFiles);

    if NewFileName <> '' then
    begin
      FEditingText := NewFileName;
      vstOIEdited(vstOI, FEditingNode, 1);
    end;
  end;

  procedure EditAsDirName(AReturnMultipleDirs: Boolean = False);
  begin
    NewFileName := BrowseDir(AReturnMultipleDirs);
    if NewFileName <> '' then
    begin
      FEditingText := NewFileName;
      vstOIEdited(vstOI, FEditingNode, 1);
    end;
  end;

begin
  try
    if FBtnItemsProperty = nil then
      Exit;

    if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
      Exit;

    case NodeLevel of
      CPropertyLevel:
      begin
        PropertyNodeData := vstOI.GetNodeData(FEditingNode);
        case PropertyNodeData^.EditorType of
          etFilePath, etFilePathWithArrow:
          begin
            EditAsFileName(vstOI.ChildCount[FEditingNode] > 0);  ////////////////////////////// this will change this part of the tree

            if Assigned(FEdtPath) and (NewFileName <> '') then
              FEdtPath.Text := NewFileName;

            Exit;
          end;

          etDirPath:
          begin
            EditAsDirName(vstOI.ChildCount[FEditingNode] > 0);   ////////////////////////////// this will change this part of the tree

            if Assigned(FEdtPath) and (NewFileName <> '') then
              FEdtPath.Text := NewFileName;

            Exit;
          end;

          etUserEditor:
          begin
            RepaintValue := True;
            DoOnOIUserEditorClick(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, RepaintValue);
            if RepaintValue then
            begin
              try
                vstOI.InvalidateNode(FEditingNode);
              except  // FEditingNode will be invalid, if the tree (or a part of it) is rebuilt
              end;
            end;

            Exit;
          end;

          else
            ;
        end;
      end;

      CPropertyItemLevel:
      begin
        PropertyNodeData := vstOI.GetNodeData(FEditingNode);
        case PropertyNodeData^.EditorType of
          etFilePath, etFilePathWithArrow:
          begin
            EditAsFileName;
            Exit;
          end;

          etDirPath:
          begin
            EditAsDirName;
            Exit;
          end;

          etUserEditor:
          begin
            RepaintValue := True;
            DoOnOIUserEditorClick(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, RepaintValue);
            if RepaintValue then
            begin
              try
                vstOI.InvalidateNode(FEditingNode);
              except  // FEditingNode will be invalid, if the tree (or a part of it) is rebuilt
              end;
            end;

            Exit;
          end;

          else
            ;
        end;
      end;
    end;

    NewItems := FEditingText;
    if DoOnOIEditItems(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, NewItems) then
    begin
      FEditingText := NewItems;
      vstOIEdited(vstOI, FEditingNode, 1);

      if FBtnItemsProperty.Tag = 1 then
        if Assigned(FCmbMiscEnumProperty) then
          FCmbMiscEnumProperty.ItemIndex := FCmbMiscEnumProperty.Items.IndexOf(CEmptySpaceForIcon + NewItems);
    end;

  finally
    vstOI.Header.Options := vstOI.Header.Options + [hoColumnResize];
  end;
end;


procedure TfrObjectInspector.btnArrowPropertyExit(Sender: TObject);
begin
  if FBtnArrowProperty = nil then
    Exit;

  if Assigned(FBtnItemsProperty) then
  begin
    if FBtnItemsProperty.Focused then
      Exit;

    FreeAndNil(FBtnItemsProperty);    //destroy the button here, if it has no focus
  end;

  FreeAndNil(FBtnArrowProperty);
  vstOI.Header.Options := vstOI.Header.Options + [hoColumnResize];
end;


procedure TfrObjectInspector.btnArrowPropertyClick(Sender: TObject);
var
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit;

  DoOnOIArrowEditorClick(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex);
end;


procedure TfrObjectInspector.spdbtnArrowPropertyExit(Sender: TObject);
begin
  if FSpdBtnArrowProperty = nil then
    Exit;

  if Assigned(FTextEditorEditBox) then
  begin
    if FTextEditorEditBox.Focused then
      Exit;

    //FreeAndNil(FTextEditorEditBox);    //do not destroy the VST text editor here
  end;

  FreeAndNil(FSpdBtnArrowProperty);
  vstOI.Header.Options := vstOI.Header.Options + [hoColumnResize];
end;


procedure TfrObjectInspector.spdbtnArrowPropertyClick(Sender: TObject);
var
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit;

  DoOnOIArrowEditorClick(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex);
end;


procedure TfrObjectInspector.EdtPathExit(Sender: TObject);
begin
  if Assigned(FBtnItemsProperty) then
  begin
    if FBtnItemsProperty.Focused then
      Exit;

    FreeAndNil(FBtnItemsProperty);
  end;

  if not Assigned(FEdtPath) then
    Exit;

  FEditingText := FEdtPath.Text;
  vstOIEdited(vstOI, FEditingNode, 1);

  FreeAndNil(FEdtPath);
  vstOI.Header.Options := vstOI.Header.Options + [hoColumnResize];
end;


procedure TfrObjectInspector.vstOIGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: {$IFDEF FPC} string {$ELSE} WideString {$ENDIF});
var
  vst: TVirtualStringTree;
  NodeData: PNodeDataPropertyRec;
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  vst := Sender as TVirtualStringTree;

  if not GetNodeIndexInfo(Node, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
  begin
    CellText := '=bug';
    Exit;
  end;

  NodeData := vst.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  try
    case NodeLevel of
      CCategoryLevel:
      begin
        case Column of
          0: CellText := DoOnOIGetCategory(CategoryIndex);
          1, 2: CellText := '';
          3: CellText := DoOnUIGetExtraInfo(CategoryIndex, PropertyIndex, PropertyItemIndex);
        end;
      end;

      CPropertyLevel:
      begin
        case Column of
          0: CellText := DoOnOIGetPropertyName(CategoryIndex, PropertyIndex);
          1: CellText := DoOnOIGetPropertyValue(CategoryIndex, PropertyIndex, NodeData^.EditorType);
          2: CellText := DoOnUIGetDataTypeName(CategoryIndex, PropertyIndex, PropertyItemIndex);
          3: CellText := DoOnUIGetExtraInfo(CategoryIndex, PropertyIndex, PropertyItemIndex);
        end;
      end;

      CPropertyItemLevel:
      begin
        case Column of
          0: CellText := DoOnOIGetListPropertyItemName(CategoryIndex, PropertyIndex, PropertyItemIndex);
          1: CellText := DoOnOIGetListPropertyItemValue(CategoryIndex, PropertyIndex, PropertyItemIndex, NodeData^.EditorType);
          2: CellText := DoOnUIGetDataTypeName(CategoryIndex, PropertyIndex, PropertyItemIndex);
          3: CellText := DoOnUIGetExtraInfo(CategoryIndex, PropertyIndex, PropertyItemIndex);
        end;
      end;
    end; //case
  except
    CellText := 'bug';
  end;
end;


procedure TfrObjectInspector.vstOIHeaderMouseDown(Sender: TVTHeader; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FreeEditorComponents;
end;


procedure TfrObjectInspector.vstOIPaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
var
  NodeData: PNodeDataPropertyRec;
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  if not GetNodeIndexInfo(Node, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit;

  NodeData := Sender.GetNodeData(Node);
  DoOnOIPaintText(NodeData^, CategoryIndex, PropertyIndex, PropertyItemIndex, TargetCanvas, Column, TextType);
end;


procedure TfrObjectInspector.vstOIBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  NodeData: PNodeDataPropertyRec;
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  if not GetNodeIndexInfo(Node, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit;

  NodeData := Sender.GetNodeData(Node);
  DoOnOIBeforeCellPaint(NodeData^, CategoryIndex, PropertyIndex, PropertyItemIndex, TargetCanvas, Column, CellPaintMode, CellRect, ContentRect);
end;


procedure TfrObjectInspector.vstOIGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  //imglstOIIcons
end;


procedure TfrObjectInspector.vstOIGetImageIndexEx(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  if not GetNodeIndexInfo(Node, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit;

  DoOnOIGetImageIndexEx(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Kind, Column, Ghosted, ImageIndex, ImageList);
end;


procedure TfrObjectInspector.vstOIMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  vstOI.GetHitTestInfoAt(X, Y, True, FPropHitInfo);

  if (Button = mbLeft) and (FPropHitInfo.HitColumn = 1) then
    tmrEditingProperty.Enabled := True;

  //tmrUpdateOIDescription.Enabled := True;
end;


procedure TfrObjectInspector.vstOIMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  //
end;


procedure TfrObjectInspector.vstOINewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
begin
  FEditingText := NewText;
end;


procedure TfrObjectInspector.FreeEditorComponents;
//var
//  NodeData: PNodeDataPropertyRec;
begin
  if FColcmbProperty <> nil then
    FreeAndNil(FColcmbProperty);

  if FCmbBooleanProperty <> nil then
    FreeAndNil(FCmbBooleanProperty);

  if FCmbMiscEnumProperty <> nil then
    FreeAndNil(FCmbMiscEnumProperty);

  if FBtnItemsProperty <> nil then
    FreeAndNil(FBtnItemsProperty);

  //if FTextEditorEditBox <> nil then
  //  FreeAndNil(FTextEditorEditBox);   //this is freed by the VST itself, so it should not be freed again here

  if FEdtColorProperty <> nil then
    FreeAndNil(FEdtColorProperty);

  //if FupdownTextEditor <> nil then
  //  FreeAndNil(FupdownTextEditor);    //this has to stay commented, because the editor should not be freed on tree scrolling, while its built-in editbox still exists


  if FBtnArrowProperty <> nil then
    FreeAndNil(FBtnArrowProperty);

  //NodeData := vstOI.GetNodeData(FEditingNode);
  if not Assigned(FTextEditorEditBox) then
    if FSpdBtnArrowProperty <> nil then       // and Assigned(NodeData) and (NodeData^.EditorType <> etTextWithArrow) then
      FreeAndNil(FSpdBtnArrowProperty);

  if FEdtPath <> nil then
    FreeAndNil(FEdtPath);
end;


procedure TfrObjectInspector.vstOIScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
begin
  FreeEditorComponents;

  try
    if Assigned(FupdownTextEditor) then
      FupdownTextEditor.Left := GetLocalEditorLeft(FupdownTextEditor.Width, GetVertScrollBarWidth) - CTextEditorSpacing;

    if Assigned(FTextEditorEditBox) then
      SetTextEditorEditPosAndSize;
  except
  end;
end;


procedure TfrObjectInspector.vstOIShowScrollbar(Sender: TBaseVirtualTree;
  Bar: Integer; AShow: Boolean);
begin
  if Bar = SB_Vert then
    FVstVertScrollBarVisible := AShow;
end;


procedure TfrObjectInspector.TextEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit;

  DoOnOITextEditorMouseDown(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Sender, Button, Shift, X, Y);
end;


procedure TfrObjectInspector.TextEditorMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit;

  if DoOnOITextEditorMouseMove(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Sender, Shift, X, Y) then
    FEditingText := FTextEditorEditBox.Text;
end;


procedure TfrObjectInspector.TextEditorKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit;

  DoOnOITextEditorKeyUp(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Sender, Key, Shift);
  SetTextEditorEditPosAndSize;
end;


procedure TfrObjectInspector.TextEditorKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
begin
  if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit;

  DoOnOITextEditorKeyDown(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, Sender, Key, Shift);
  SetTextEditorEditPosAndSize;
end;


const
  CDirIncrement: array[TUpDownDirection] of Integer = (0, 1, -1);  //TUpDownDirection = (updNone, updUp, updDown);

procedure TfrObjectInspector.updownTextEditorChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
var
  NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex: Integer;
  NewEditingText: string;
begin
  FTextEditorEditBox.Text := IntToStr(StrToIntDef(FTextEditorEditBox.Text, 0) + CDirIncrement[Direction]);
  FEditingText := FTextEditorEditBox.Text;
  SetTextEditorEditPosAndSize;  //restore, although EditBox.OnChange may set the width back to a smaller value

  if not GetNodeIndexInfo(FEditingNode, NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex) then
    Exit;

  NewEditingText := FEditingText;
  DoOnAfterSpinTextEditorChanging(NodeLevel, CategoryIndex, PropertyIndex, PropertyItemIndex, NewEditingText);
  if NewEditingText <> FEditingText then
  begin
    FTextEditorEditBox.Text := NewEditingText;
    FEditingText := NewEditingText;
    SetTextEditorEditPosAndSize;
  end;
end;


end.

