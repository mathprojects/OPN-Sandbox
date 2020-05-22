(*
   OPN Sandbox Property Processor v1.1 (Command line app)
   License LGPLv3

   Author: Matthew Thomas
   ORCID: [0000-0001-6511-3223]
   Email: mattfwd@mattzart.com
   Websites: http://www.github.com/mathprojects
             http://www.mattzart.com

   Copyright (c) 2020 Matthew Thomas

   Additional Authors: Won't you contribute?
*)
program opnsprop;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  {$IFnDEF FPC}
  System.SysUtils,
  System.Classes,
  {$ELSE}
  SysUtils,
  Classes,
  CustApp,
  {$ENDIF }
  OpnPropProc;

var
  i: integer;
  s: string;
  InputFile: TextFile;
  OutputFile: TextFile;
  FileStream: TFileStream;
  UTF8FileSignature: array[0..2] of byte;
  MaxRows: integer;
  StartTime, EndTime: cardinal;
  InputStr: string;
  OutputStr: UnicodeString;

  {$IFDEF FPC}
  function GetCmdLineSwitchValue(const Name: String; var Value: string): boolean;
  var
    Temp: TCustomApplication;
  begin
    Temp := TCustomApplication.Create(nil);
    result := Temp.HasOption(Name);
    if result then
      Value := Temp.GetOptionValue(Name);
    Temp.Free;
  end;
  {$ELSE}
    function GetCmdLineSwitchValue(const Name: String; var Value: string): boolean;
    begin
      result := FindCmdLineSwitch(Name, Value);
    end;
  {$ENDIF}


  procedure PrintHelp;
  begin
    Writeln('OPN Sandbox Property Processor version 1.1');
    Writeln;
    Writeln('opnsprop source destination [/ansi] [/fpprec=precision] [/help] [/maxrows=max] [/showdigits]');
    Writeln;
    Writeln('  source' + #9 + 'Specifies the input file.');
    Writeln('  destination' + #9 + 'Specifies the output file.');
    Writeln('  /ansi' + #9 + 'Use non-unicode characters.');
    Writeln('  /fpprec=precision' + #9 + 'Sets floating-point precision, in decimal digits, to the specified integer value.');
    Writeln('  /help' + #9#9 + 'Shows this help.');
    Writeln('  /maxrows=max' + #9 + 'Specifies the maximum input rows to process.');
    Writeln('  /showdigits' + #9 + 'Includes more columns to show full expansion of digits.');
    Writeln(#9#9 + 'This can result in output rows with thousands of characters each.');

    exit;
  end;

  function GetNonswitchParamStr(ParamNum: integer): string;
  var
    i: integer;
    c: integer;
  begin
    (* Search for the ParamNumth param string that is not a switch *)
    result := '';
    c := 0;
    if ParamCount > 0 then
      for i := 1 to ParamCount do
        if (copy(ParamStr(i), 1, 1) <> '/') and (copy(ParamStr(i), 1, 1) <> '-') then
        begin
          c := c + 1;
          if c = ParamNum then
          begin
            result := ParamStr(i);
            exit;
          end;
        end;
  end;



begin
  try
    try
      OpnProp := TOpnPropProc.Create;

      (* Check for /help switch *)
      if FindCmdLineSwitch('help') then
      begin
        PrintHelp;
        exit;
      end;

      {$IFDEF FPC}
        StartTime := TThread.GetTickCount;
      {$ELSE}
        StartTime := System.Classes.TThread.GetTickCount;
      {$ENDIF}

      (* If /ansi is set, set NoUnicode to true, which will make output not
         include unicode characters such as alpha, sigma, congruent symbol, etc *)
      OpnProp.NoUnicode := FindCmdLineSwitch('ansi');

      (* Set custom floating precision or set default of 200 *)
      if GetCmdLineSwitchValue('fpprec', s) then
      begin
        if copy(s,1,1)='=' then
          delete(s,1,1);
        i := StrTointDef(s, 0);
        if i=0 then
        begin
          Writeln('Specified fpprec not understood');
          exit;
        end;

        OpnProp.SetFPPrecChars(i);

        writeln('FP precision set to ' + s);
        writeln;
      end
      else
      begin
        OpnProp.SetFPPrecChars(200);
      end;

      (* Check for showdigits options *)
      OpnProp.ShowDigits := FindCmdLineSwitch('showdigits');
      if OpnProp.ShowDigits then
        Writeln('Warning: /showdigits specified; each output row could be extremely long.');

      (* Check if input file is specified and exists *)
      s := GetNonswitchParamStr(1);
      if s = '' then
      begin
        Writeln('No input file specified');
        exit;
      end;

      if not FileExists(s) then
      begin
        Writeln('Input file not found at ' + s);
        exit;
      end;

      (* Check if output file is specified *)
      s := GetNonswitchParamStr(2);
      if s = '' then
      begin
        Writeln('No output file specified');
        exit;
      end;

      (* Check for /maxrows *)
      if GetCmdLineSwitchValue('maxrows', s) then
      begin
        if copy(s,1,1)='=' then
          delete(s,1,1);
        i := StrToIntDef(s, 0);
        if i=0 then
        begin
          Writeln('Specified maxrows not understood');
          exit;
        end;
        MaxRows := i;
      end
      else
        MaxRows := 0;

      (* Get input filename again and open input for
         reading and make sure it is not empty *)
      s := GetNonswitchParamStr(1);
      AssignFile(InputFile, s);
      Reset(InputFile);
      if EOF(InputFile) then
      begin
        Writeln('Input file is empty');
        exit;
      end;

      (* Get output filename again and open for writing *)
      s := GetNonswitchParamStr(2);
      if OpnProp.NoUnicode then
      begin
        (* Open file for writing and make sure it is empty *)
        AssignFile(OutputFile, s);
        Rewrite(OutputFile);
      end
      else
      begin
        (* TStreamWriter, which might be able to write a good UTF8 file, is not
        available in FPC. Until there is a better way to write a UTF8 text file,
        we will write the signautre of the UTF8 file manually (see https://en.wikipedia.org/wiki/List_of_file_signatures).
        Since the Write() procedures does not seem to work for this, we will use
        TFileStream instead to open the file, make sure it is empty, then
        write the three-byte UTF8 file signature. Then we close it and open
        it again using AssignFile for writing but not clearing the 3-bytes we
        first write. *)
        FileStream := TFileStream.Create(s, fmOpenWrite+fmCreate);
        FileStream.Size := 0;
        UTF8FileSignature[0] := $EF;
        UTF8FileSignature[1] := $BB;
        UTF8FileSignature[2] := $BF;
        FileStream.Write(UTF8FileSignature, 3);
        FileStream.Free;

        AssignFile(OutputFile, s);
        SetTextCodePage(OutputFile,  CP_UTF8);
        Append(OutputFile);
      end;


      Writeln(OutputFile, OpnProp.OutputHeaderString);


      Write('0 rows processed...');

      (* Begin reading input *)
      while not EOF(InputFile) do
      begin
        if (MaxRows > 0) and (OpnProp.RowsProcessed = MaxRows) then
        begin
          Write(#13);
          Writeln('Specified max rows has been reached.');
          break;
        end;

        ReadLn(InputFile, InputStr);
        if not OpnProp.ProcessInputString(InputStr, OutputStr) then
        begin
          WriteLn(OpnProp.ErrorMessage);
          exit;
        end;

        WriteLn(OutputFile, OutputStr);

        if (OpnProp.RowsProcessed mod 100) = 0 then
        begin
          {$IFDEF FPC}
            EndTime := TThread.GetTickCount;
          {$ELSE}
            EndTime := System.Classes.TThread.GetTickCount;
          {$ENDIF}
          Write(#13 + 'Elapsed time ' + IntToStr((EndTime-StartTime) div 1000) + ' sec ' + #9 + IntToStr(OpnProp.RowsProcessed) + ' rows processed...');
        end;
      end;

      WriteLn(#13 + IntToStr(OpnProp.RowsProcessed) + ' rows processed.                                           ');

      {$IFDEF FPC}
        EndTime := TThread.GetTickCount;
      {$ELSE}
        EndTime := System.Classes.TThread.GetTickCount;
      {$ENDIF}
      if (EndTime - StartTime < 1000) then
        EndTime := StartTime + 1000;

      Writeln('Elapsed time ' + IntToStr((EndTime-StartTime) div 1000) + ' sec');

    finally
      try
        CloseFile(InputFile);
        CloseFile(OutputFile);
      except
        //Need to find out how to detect if the file was ever opened,
        //or use a var
      end;

      if Assigned(OpnProp) then
        OpnProp.Free;

    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
