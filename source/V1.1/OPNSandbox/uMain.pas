(*
   OPN Sandbox v1.1
   License LGPLv3

   Author: Matthew Thomas
   ORCID: [0000-0001-6511-3223]
   Email: mattfwd@mattzart.com
   Websites: http://www.github.com/mathprojects
             http://www.mattzart.com

   Copyright (c) 2020 Matthew Thomas

   Additional Authors: Won't you contribute?
*)

unit uMain;

interface

uses
  System.SysUtils, System.Classes, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Edit, FMX.Layouts, FMX.Objects, FMX.Grid,
  FMX.Types, FMX.Controls, System.Rtti, FMX.Grid.Style, FMX.ScrollBox,
  FMX.Controls.Presentation, StrUtils, System.Types, FMX.Menus,
  OpnPropProc, FMX.Memo;

type

  TOPNSandboxForm = class(TForm)
    MainPanel: TPanel;
    FormOfNGraphic: TImage;
    InstructionsLabel1: TLabel;
    InputGrid: TStringGrid;
    IntegerColumn1: TIntegerColumn;
    IntegerColumn2: TIntegerColumn;
    KEditBox: TEdit;
    InstructionsLabel2: TLabel;
    SetKButton: TButton;
    RunButton: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    OpenDialog1: TOpenDialog;
    ImportButton: TButton;
    Layout1: TLayout;
    OutputGrid: TStringGrid;
    StringColumn1: TStringColumn;
    SaveOuputAsButton: TButton;
    SaveDialog1: TSaveDialog;
    ProgressBarPanel: TPanel;
    ProgressBar1: TProgressBar;
    ProgressLabel: TLabel;
    CancelButton: TButton;
    PopupMenu1: TPopupMenu;
    CopyCellToClipboard: TMenuItem;
    ClipboardMemo: TMemo;
    CopyRowToClipboard: TMenuItem;
    procedure SetKButtonClick(Sender: TObject);
    procedure ImportButtonClick(Sender: TObject);
    procedure SaveAsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RunButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CopyCellToClipboardClick(Sender: TObject);
    procedure CopyRowToClipboardClick(Sender: TObject);
  private
    { Private declarations }
  public
    MaxInputColumns: integer;
    CancelRequested: boolean;

    procedure AutoSizeGrid(Grid: TStringGrid);
    procedure FillInOutputHeaders;
  end;

var
  OPNSandboxForm: TOPNSandboxForm;

implementation

{$R *.fmx}

procedure TOPNSandboxForm.SetKButtonClick(Sender: TObject);
var
  integercol: TIntegerColumn;
  i: integer;
begin
  (* Check if the user entered a number *)
  if StrToIntDef(KEditBox.Text, -1) = -1 then
  begin
    ShowMessage('Enter a positive integer without any stray characters.');
    exit;
  end;

  MaxInputColumns := StrToInt(KEditBox.Text);

  (* Make sure the number is positive *)
  if MaxInputColumns < 1 then
  begin
    ShowMessage('Enter a positive number.');
    exit;
  end;

  (* If the desired columns is greater than what we current have in the input grid,
  create enough new columns *)
  while InputGrid.ColumnCount < MaxInputColumns*2+2 do
  begin
    integercol :=  TIntegerColumn.Create(InputGrid);
    InputGrid.AddObject(integercol);
  end;

  (* If the desired columns is less than what we have, destroy the extra columns *)
  while InputGrid.ColumnCount > MaxInputColumns*2+2 do
  begin
    integercol := TIntegerColumn(InputGrid.Columns[InputGrid.ColumnCount-1]);
    InputGrid.RemoveObject(integercol);
    integercol.Free;
  end;

  (* Label the headers *)
  InputGrid.Columns[0].Header := 'p';
  InputGrid.Columns[1].Header := 'α';
  for i := 1 to MaxInputColumns do
  begin
    InputGrid.Columns[i*2].Header := 'q' + inttostr(i);
    InputGrid.Columns[i*2+1].Header := 'β' + inttostr(i);
  end;
end;

procedure TOPNSandboxForm.CancelButtonClick(Sender: TObject);
begin
  (* Set a variable that will be checked periodically by the main loop
  in whatever is running to let it know to escape *)
  CancelRequested := true;
end;

procedure TOPNSandboxForm.FillInOutputHeaders;
var
  StringArray: TStringDynArray;
  s: string;
  i: integer;
  NumOutputColumns: integer;
  StrColumn: TStringColumn;
begin
  (* We will use a string list and extractstrings to separate the single string
  of comma-separated header labels into individual strings *)
  s := OpnProp.OutputHeaderString;
  s := StringReplace(s, '"', '', [rfReplaceAll]);
  StringArray := SplitString(s, ',');

  NumOutputColumns := length(StringArray);

  (* Make sure we have enough columns, creating new columns if necessary *)
  while OutputGrid.ColumnCount < NumOutputColumns do
  begin
    StrColumn :=  TStringColumn.Create(OutputGrid);
    OutputGrid.AddObject(StrColumn);
  end;

  (* If we already had more columns than needed, destroy the extra columns *)
  while OutputGrid.ColumnCount > NumOutputColumns do
  begin
    StrColumn := TStringColumn(OutputGrid.Columns[OutputGrid.ColumnCount-1]);
    OutputGrid.RemoveObject(StrColumn);
    StrColumn.Free;
  end;

  (* Label the headers in the output grid *)
  for i := 0 to NumOutputColumns-1 do
  begin
    OutputGrid.Columns[i].Header := StringArray[i];
  end;

  (* Free memory used by string array *)
  setlength(StringArray, 0);
end;

procedure TOPNSandboxForm.FormCreate(Sender: TObject);
begin
  Application.Title := 'OPN Sandbox';

  (* Create an instance of TOpnPropProc, which is the object, in common with the
  command line app opnsprop, which takes a row of comma-separated input and
  computes the opn-relevants properties *)
  OpnProp := TOpnPropProc.Create;

  (* For the floating-point property, we might want a large number of decimal
  digits, and we set the default to 200 *)
  OpnProp.SetFPPrecChars(200);

  (* Get the header string for the properties computed by OpnProp and put them
  in the OutputGrid headers. *)
  FillInOutputHeaders;

  ProgressBarPanel.Visible := false;

  {$IFDEF MACOS}
  (* It seems that the MacOS version's scrollbars don't work right in the
  string grids unless we make them visible all the time, so we'll do that until
  further notice *)
  Inputgrid.AniCalculations.AutoShowing:=false;
  Outputgrid.AniCalculations.AutoShowing:=false;
  {$ENDIF}

end;

procedure TOPNSandboxForm.ImportButtonClick(Sender: TObject);

  procedure LoadCSV(Filename: string; sg: TStringGrid);
  var
     StringArray: TStringDynArray;
     InputStr: string;
     InputFile: TextFile;
     row: integer;
     col: integer;
     MaxRows: integer;
  begin
    (* Open the file *)
    AssignFile(InputFile, Filename);

    try
      (* Get ready to read file *)
      Reset(InputFile);

      (* Check if the file is empty *)
      if EOF(InputFile) then
      begin
        CloseFile(InputFile);        Showmessage('File is empty.');
        exit;
      end;

      (* Clear any old contents of the input grid *)
      InputGrid.ClearColumns;

      (* Count number of rows there are going to be *)
      MaxRows := 0;
      while not EOF(InputFile) do
      begin
        ReadLn(InputFile, InputStr);

        (* Count only if not blank *)
        if InputStr <> '' then
          inc(MaxRows);
      end;

      (* Go back to beginning of file *)
      Reset(InputFile);

      (* Size the input grid for the number of rows we expect *)
      InputGrid.RowCount := MaxRows; //+1 if for the header

      (* Init row to -1 so that when it is increment at the first row read, it
      will be 0 *)
      row := -1;

      (* If we expect an extremely large number of rows, show a progress bar *)
      if MaxRows > 500 then
      begin
        ProgressLabel.Text := 'Reading...';
        ProgressBar1.Min := 0;
        ProgressBar1.Max := MaxRows;
        ProgressBar1.Value := 0;
        ProgressBarPanel.Visible := true;
        Application.ProcessMessages;
      end;

      (* Read the rows *)
      while not EOF(InputFile) do
      begin
        ReadLn(InputFile, InputStr);

        (* There should not be blank rows, but we'll check anyway *)
        if InputStr = '' then
          continue;

        (* Increment row counter and increase rows in the input grid *)
        row := row + 1;

        (* For the first row, check if the first few characters are non-numeric
        becaue if the file is UTF8, it may have a file signature in the first few
        bytes that can cause problems, so we get rid of them *)
        if row = 0 then
          while StrToIntDef(copy(InputStr,1,1), -1) = -1 do
            delete(InputStr, 1, 1);

        (* Separate the comma-separated row of items *)
        StringArray := SplitString(InputStr, ',');

        (* Increase the columns in the input grid if necessary *)
        if InputGrid.ColumnCount < length(StringArray) then
        begin
          KEditBox.Text := IntToStr(length(StringArray));
          SetKButtonClick(nil);
        end;

        (* Copy items into input grid *)
        for col := 0 to length(StringArray)-1 do
          InputGrid.Cells[col, row] := StringArray[col];

        (* If we are showing progress bar, update it occasionally *)
        if (row mod 100)=0 then
        begin
          ProgressBar1.Value := row;
          Application.ProcessMessages;
          if CancelRequested then
            break;
        end;
      end;

    finally
      CloseFile(InputFile);
      SetLength(StringArray, 0);
      ProgressBarPanel.Visible := false;
      CancelRequested := false;
    end;
  end;

begin
  if OpenDialog1.Execute then
  begin
    LoadCSV(OpenDialog1.FileName, InputGrid);
    RadioButton2.IsChecked := true;
  end;
end;

procedure TOPNSandboxForm.CopyCellToClipboardClick(Sender: TObject);
begin
  ClipboardMemo.Text := Outputgrid.Cells[Outputgrid.Col, Outputgrid.Row];
  ClipboardMemo.SelectAll;
  ClipboardMemo.CopyToClipboard;
end;

procedure TOPNSandboxForm.CopyRowToClipboardClick(Sender: TObject);
var
  s: string;
  row: integer;
  col: integer;
begin
  (* Get selected row *)
  row := OutputGrid.Row;

  (* Put together a comma-separated string of the cell contents *)
  s := '';
  for col := 0 to OutputGrid.ColumnCount-1 do
  begin
    if col <> 0 then
      s := s + ',';
    s := s + Outputgrid.Cells[col,row];
  end;

  (* Put the string in a memo which has a built-in way to copy it to the clipboard *)
  ClipboardMemo.Text := s;
  ClipboardMemo.SelectAll;
  ClipboardMemo.CopyToClipboard;
end;

procedure TOPNSandboxForm.AutoSizeGrid(Grid: TStringGrid);
var
  col, row: integer;
  w, maxwidth: single;
begin
  (* This procedure tries to size the columns of the output grid to fit
  the contents so you can see it without having to resize columns with the mouse *)
  for col := 0 to Grid.ColumnCount - 1 do
  begin
    (* The width of the header might be the widest, and we start with that *)
    maxwidth := Grid.Canvas.TextWidth(Grid.Columns[col].Header);

    (* Check all the other rows for the column, and make sure we know what the
    widest column needs to be to accomdate the widest content *)
    for row := 0 to (Grid.RowCount - 1) do
    begin
      w := Grid.Canvas.TextWidth(Grid.Cells[col, row]);
      if w > maxwidth then
        maxwidth := w;
    end;

    (* We make sure the column won't be extremely wide *)
    if maxwidth > 1000 then
      maxwidth := 1000;

    Grid.Columns[col].Width := maxwidth + 10;
  end;
end;

procedure TOPNSandboxForm.RunButtonClick(Sender: TObject);
var
  row: integer;
  col: integer;
  InputStr: string;
  OutputStr: string;
  OutputStringArray: TStringDynArray;
begin
  try
    ProgressLabel.Text := 'Running...';
    ProgressBarPanel.Visible := true;
    ProgressBar1.Min := 0;
    ProgressBar1.Max := InputGrid.RowCount-2;
    Application.ProcessMessages;

    (* Size the output grid once so we don't have to keep resizing it *)
    OutputGrid.RowCount := InputGrid.RowCount-1;

    (* Set rows processed count to 0 *)
    OpnProp.RowsProcessed := 0;

    (* Process rows *)
    for row := 0 to InputGrid.RowCount-2 do
    begin
      (* Put the input numbers into a comma-separate string *)
      InputStr := '';
      for col := 0 to InputGrid.ColumnCount-1 do
         if InputGrid.Cells[col,row] = '' then
           break
         else
         begin
           if col <> 0 then
             InputStr := InputStr + ',';
             InputStr := InputStr + InputGrid.Cells[col,row];
         end;

      (* Process the row of input, get a row of comma-separate output *)
      if OpnProp.ProcessInputString(InputStr, OutputStr) = false then
      begin
        Showmessage(OpnProp.ErrorMessage);
        exit;
      end;

      (* Split the output string *)
      OutputStringArray := SplitString(OutputStr, ',');

      (* Put the output items in the output grid *)
      for col := 0 to length(OutputStringArray)-1 do
        OutputGrid.Cells[col,row] := OutputStringArray[col];

      (* Periodically, update progress bar and check to see if a cancel
      was requested *)
      if (row mod 100) = 0 then
      begin
        ProgressBar1.Value := row;
        Application.ProcessMessages;
        if CancelRequested then
          exit;
      end;
    end;

    (* Do one more update of progress bar, because it might still be showing
    while autosizing the output grid *)
    ProgressBar1.Value := row;
    Application.ProcessMessages;
  finally
    (* Resize the columns so that their contents can be seen without the user
    having to resize columns *)
    AutosizeGrid(OutputGrid);

    ProgressBarPanel.Visible := false;
    CancelRequested := false;
    setlength(OutputStringArray, 0);
  end;
end;

procedure TOPNSandboxForm.SaveAsButtonClick(Sender: TObject);
var
  row: integer;
  s: string;
  col: integer;
  colcount: integer;
  OutputFile: TextFile;
  FileStream: TFileStream;
  UTF8FileSignature: array[0..2] of byte;
begin

  if SaveDialog1.Execute then
  begin
    colcount := OutputGrid.ColumnCount;
    if OutputGrid.Columns[colcount-1].Header='' then
      colcount := colcount - 1;

    (* Open the file for writing. How we do it depends on whether we need
    unicode characters in the header *)
    if OpnProp.NoUnicode then
    begin
      (* Open file for writing and make sure it is empty *)
      AssignFile(OutputFile, SaveDialog1.FileName);
      Rewrite(OutputFile);
    end
    else
    begin
      (* Although TStreamWriter is an option to write a UTF8 file (which we
      need since we have math characters in unicode in the header), the following
      method works well in opnsprop which was designed to be compatible with FPC
      which does not have TStreamWriter.
      We will write the signautre of the UTF8 file manually (see https://en.wikipedia.org/wiki/List_of_file_signatures).
      Since the Write() procedures does not seem to work for this, we will use
      TFileStream instead to open the file, make sure it is empty, then
      write the three-byte UTF8 file signature. Then we close it and open
      it again using AssignFile for writing but not clearing the 3-bytes we
      first write. *)
      FileStream := TFileStream.Create(SaveDialog1.FileName, fmOpenWrite+fmCreate);
      FileStream.Size := 0;
      UTF8FileSignature[0] := $EF;
      UTF8FileSignature[1] := $BB;
      UTF8FileSignature[2] := $BF;
      FileStream.Write(UTF8FileSignature, 3);
      FileStream.Free;

      AssignFile(OutputFile, SaveDialog1.FileName);
      SetTextCodePage(OutputFile,  CP_UTF8);
      Append(OutputFile);
    end;

    (* Write the header *)
    Writeln(OutputFile, OpnProp.OutputHeaderString);

    (* Go through the rows, put together the output strings and write them *)
    for row := 0 to OutputGrid.RowCount-1 do
    begin
      (* There shouldn't be any blank rows, but if so, skip them *)
      if OutputGrid.Cells[0,row]='' then
        continue;

      (* Put together a row of comma-separated items into one string to be
      written to the output file *)
      s := '';
      for col := 0 to colcount-1 do
      begin
        s := s + OutputGrid.cells[col,row];
        if col < colcount-1 then
          s := s + ',';
      end;

      (* Write to the output file *)
      Writeln(OutputFile,s);
    end;

    CloseFile(OutputFile);
  end;
end;

end.
