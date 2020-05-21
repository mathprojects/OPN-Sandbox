(*
   OPN Sandbox Property Processor Object Library v1.1
   License LGPLv3

   Author: Matthew Thomas
   ORCID: [0000-0001-6511-3223]
   Email: mattfwd@mattzart.com
   Websites: http://www.github.com/mathprojects
             http://www.mattzart.com

   Copyright (c) 2020 Matthew Thomas

   Additional Authors: Won't you contribute?
*)

unit OpnPropProc;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFnDEF FPC}
  System.SysUtils,
  System.Classes,
  {$ELSE}
  SysUtils,
  {$ENDIF }
  gmp_obj, Math;

type
  BigInteger = gmpInteger;
  BigFloat = gmpFloat;

  TCacheBinRecord = record
    Base: uint64;
    Power: uint64;
    Result: BigInteger;
    LastPowerDiff: uint64;
    LastPartialFactor: BigInteger;
  end;

  TOpnPropProc = class(TObject)
  private
    i: integer;
    s: string;
    RowInputNumbers: TStringList;
    Alpha: cardinal;
    BetasArray: array of uint64;
    CacheBins: array of TCacheBinRecord;
    (* gmp variables *)
    P: BigInteger;
    N: BigInteger;
    QPower: BigInteger;
    PPower: BigInteger;
    indpower: BigInteger;
    largestindpower: BigInteger;
    smallestindpower: BigInteger;
    sigma: BigInteger;
    divq, divr: BigInteger;
    FloatSigma: BigFloat;
    NumeratorPower: BigInteger;
    QsArray: array of BigInteger;

    function ProcessParsedInput(var InputStr: string; var OutputStr: string): boolean;
    procedure FreeGmpVars;
    procedure CreateGmpVars;

  public
    ErrorMessage: string;
    NoUnicode: boolean;
    ShowDigits: boolean;
    NumQs: integer;
    FPPrecChars: integer;
    RowsProcessed: integer;
    function OutputHeaderString: UnicodeString;
    procedure SetFPPrecChars(NumChars: integer);

    function ProcessInputString(var InputStr: string; var OutputStr: string): boolean;
    constructor Create;
    destructor Destroy;
  end;

var
  OpnProp: TOpnPropProc;

implementation

  {$IFDEF FPC}
  function StrToUInt64Def(s: String; Def: UInt64): Uint64;
  begin
    result := StrToQWordDef(s, Def);
  end;
  {$ENDIF}
  function Log10(var x: BigInteger): uint64;
  var
    r: BigFloat;
  begin
    r := gmpLog10(x);
    result := r.ToCardinal;
  end;

  function GreaterThanPower(var x: BigInteger): uint64;
  begin
    result := Log10(x);
  end;

  function SmallerThanPower(var x: BigInteger): uint64;
  begin
    result := GreaterThanPower(x);
    result := result + 1;
  end;

  {
  procedure FastBigIntegerPower(CacheBin: integer; var Output: BigInteger; Base: uint64; Power: uint64);
  var
    PowerDiff: uint64;
  begin

    if length(CacheBins) < (CacheBin+1) then
      setlength(CacheBins, CacheBin+1);
    if (CacheBins[CacheBin].Base <> Base) or (CacheBins[CacheBin].Power > Power) then
    begin
      CacheBins[CacheBin].Base := Base;
      CacheBins[CacheBin].Power := 1;
      CacheBins[CacheBin].Result := Base;
    end;

    PowerDiff := Power - CacheBins[CacheBin].Power;

    if PowerDiff = 0 then
    begin
      Output := CacheBins[CacheBin].Result;
      exit;
    end;

    if CacheBins[CacheBin].LastPowerDiff = PowerDiff then
      Output := BigInteger.Multiply(CacheBins[CacheBin].Result, CacheBins[CacheBin].LastPartialFactor)
    else
    begin
      CacheBins[CacheBin].LastPartialFactor := BigInteger.Pow(Base,PowerDiff);
      CacheBins[CacheBin].LastPowerDiff := PowerDiff;
      Output := BigInteger.Multiply(CacheBins[CacheBin].Result, CacheBins[CacheBin].LastPartialFactor);
    end;

    CacheBins[CacheBin].Power := Power;
    CacheBins[CacheBin].Result := Output;
  end;
  }

  function TOpnPropProc.ProcessParsedInput(var InputStr: string; var OutputStr: string): boolean;
  var
    i: integer;
    j: integer;
    k: integer;
    s: string;
    largestindindex: integer;
    smallestindindex: integer;
  begin
    (* Set default result *)
    result := false;

    (* NumQs has already been set and the QsArray and BetasArray have already been sized
    and assigned the q's and beta's *)

    (* Build a string that has all the factors (p^a)(q1^2b1)... , and add it to the output string *)
    OutputStr := '(' + IntToStr(P.ToCardinal)+'^'+IntToStr(Alpha)+')';
    for j := 0 to NumQs-1 do
      OutputStr := OutputStr + '(' + IntToStr(QsArray[j].ToCardinal) + '^' + IntToStr(2*BetasArray[j]) + ')';

    (* Determine and write ω*)
    i := 1;
    for j := 0 to NumQs-1 do
      if QsArray[j] = P then
        continue
      else
      begin
        for k := 0 to NumQs-1 do
          if (j<>k) and (QsArray[j] = QsArray[k]) then
            continue;
        i := i + 1;
      end;
    OutputStr := OutputStr + ',' + inttostr(i);
    if i<>(NumQs+1) then
      Writeln('Warning: Row ' + inttostr(RowsProcessed+1) + ' has a non-distinct prime fator');

    (* Determine and write Ω *)
    i := alpha;
    for j := 0 to NumQs-1 do
      i := i + 2*BetasArray[j];
    OutputStr := OutputStr + ',' + inttostr(i);

    (* Compute p^alpha *)
    //FastBigIntegerPower(0, PPower, P, Alpha);
    PPower.Assign(gmpIntPower(P, Alpha));

    (* Optionlly, add the full number of PPower to the output string *)
    if ShowDigits then
      OutputStr := OutputStr + ',' + PPower.ToString;

    (* Compute and add PPower > 10^i to output string *)
    i := GreaterThanPower(PPower);
    OutputStr := OutputStr + ',' + IntToStr(i);

    (* The next loop does two things at once.
    1. It iteratively builds the products of all Q powers, QPower
    2. It determines which q power is the smallest and the largest *)

    (* We initialize QPower, largest and smallest q power to the
    first q power *)
    //FastBigIntegerPower(1, QPower, QsArray[0], 2*BetasArray[0]);
    QPower.Assign(gmpIntPower(QsArray[0], 2*BetasArray[0]));
    smallestindpower.Assign(QPower);
    largestindpower.Assign(QPower);
    smallestindindex := 0;
    largestindindex := 0;

    (* We go through all the other q powers and find the smallest,
    the largest, and work toward building QPower *)
    for j := 1 to NumQs-1 do
    begin
      //FastBigIntegerPower(j+1, indpower, QsArray[j],2*BetasArray[j]);
      indpower.Assign(gmpIntPower(QsArray[j],2*BetasArray[j]));
      if indpower < smallestindpower then
      begin
        smallestindpower.Assign(indpower);
        smallestindindex := j;
      end;
      if indpower > largestindpower then
      begin
        largestindpower.Assign(indpower);
        largestindindex := j;
      end;

      QPower.MultiplyBy(indpower);
    end;

{    if NoUnicode then
      OutputStr := OutputStr + ',q' + inttostr(smallestindindex+1) + '^2b' + inttostr(smallestindindex+1)
    else
      OutputStr := OutputStr + ',q' + inttostr(smallestindindex+1) + '^2β' + inttostr(smallestindindex+1);
    OutputStr := OutputStr + ',10^' + IntToStr(SmallerThanPower(smallestindpower));
}
    (* Write the index of the smallest q power *)
    OutputStr := OutputStr + ',' + IntToStr(smallestindindex+1);

    (* Optinally write all the digits of the smallest q power *)
    if ShowDigits then
      OutputStr := OutputStr + ',' + smallestindpower.ToString;

    (* Compute and write what the smallest q power is less than *)
    OutputStr := OutputStr + ',' + IntToStr(SmallerThanPower(smallestindpower));

{    if NoUnicode then
      OutputStr := OutputStr + ',q' +inttostr(largestindindex+1) + '^2b' + inttostr(largestindindex+1)
    else
      OutputStr := OutputStr + ',q' +inttostr(largestindindex+1) + '^2β' + inttostr(largestindindex+1);
}
    (* Write the index of the largest q power *)
    OutputStr := OutputStr + ',' + inttostr(largestindindex+1);

    (* Optinally write all the digits of the smallest q power *)
    if ShowDigits then
      OutputStr := OutputStr + ',' + largestindpower.ToString;

    (* Compute and write what the largest q power is greater than *)
    OutputStr := OutputStr + ',' + IntToStr(GreaterThanPower(largestindpower));

    (* Output variuos props for QPower*)

    if QPower.IsCongruentToCModD(1, 16) then
      OutputStr := OutputStr + ',Y'
    else
      OutputStr := OutputStr + ',n';

    if QPower.IsCongruentToCModD(1, 32) then
      OutputStr := OutputStr + ',Y'
    else
      OutputStr := OutputStr + ',n';

   (* Calculate N *)
   N.Assign(QPower * PPower);

   (* Optionally write the full expansion of digits for N...yikes! *)
   if ShowDigits then
     OutputStr := OutputStr + ',' + N.ToString;

   (* Compute and write what N is greater than *)
   OutputStr := OutputStr + ',' + inttostr(GreaterThanPower(N));

   (* Output various props for N*)
   if N.IsCongruentToCModD(1,12) then
      OutputStr := OutputStr + ',Y'
    else
      OutputStr := OutputStr + ',n';
   if N.IsCongruentToCModD(117,468) then
      OutputStr := OutputStr + ',Y'
    else
      OutputStr := OutputStr + ',n';

   if N.IsCongruentToCModD(81,324) then
      OutputStr := OutputStr + ',Y'
    else
      OutputStr := OutputStr + ',n';

   if N.IsCongruentToCModD(0,105) then
      OutputStr := OutputStr + ',Y'
    else
      OutputStr := OutputStr + ',n';


   (* Compute sigma, which is done a little at a time *)

   //FastBigIntegerPower(NumQs+1, NumeratorPower, P, Alpha+1);
   //Sigma := BigInteger.Divide(NumeratorPower-1,P-1);
   Sigma.Assign(gmpIntPower(P, Alpha+1) - 1);
   Sigma.DivideBy(P-1);
   //Sigma.DivideBy(P-1);
   for j := 0 to NumQs-1 do
   begin
     //FastBigIntegerPower(NumQs+1+j+1, NumeratorPower, QsArray[j], 2*BetasArray[j]+1);
     //Sigma := BigInteger.Multiply(Sigma,BigInteger.Divide(NumeratorPower-1,QsArray[j]-1));
     Sigma.MultiplyBy(gmpIntPower(QsArray[j], 2*BetasArray[j]+1) - 1);
     Sigma.DivideBy(QsArray[j]-1);
   end;

   (* Optionally show sigma in full *)
   if ShowDigits then
     OutputStr := OutputStr + ',' + Sigma.ToString;

   (* Compute sigma(N)/N and write output *)
   FloatSigma.Assign(Sigma);
   FloatSigma.DivideBy(N);
   s := FloatSigma.ToString;
   s := copy(s, 1, FPPrecChars+2); //+2 is so we get the desired number of digits to right of point
   OutputStr := OutputStr + ',' + s;

   result := true;
  end;

  function TOpnPropProc.ProcessInputString(var InputStr: string; var OutputStr: string): boolean;
  var
    i: integer;
    s: string;
  begin
    result := false;
    OutputStr := '';

    (* Parse comma separated values *)
    RowInputNumbers.Clear;
    ExtractStrings([','], [], pchar(InputStr), RowInputNumbers);

    (* if this is the first row, the first few characters might be a file signature,
    such as the UTF8 signature if the data was exported from Excel *)
    if RowsProcessed = 0 then
    begin
      s := RowInputNumbers.Strings[0];
      while StrToIntDef(copy(s,1,1), -1) = -1 do
        delete(s, 1, 1);
      RowInputNumbers.Strings[0] := s;
    end;

    (* If less than 4 items, there is a problem *)
    if RowInputNumbers.Count < 20 then
    begin
      Writeln('Error: Row ' + inttostr(RowsProcessed + 1) + ' has less than 10 prime powers.');
      exit;
    end;

    (* If not an even number of items, problem *)
    if (RowInputNumbers.Count mod 2) <> 0 then
    begin
      Writeln('Error: Row ' + inttostr(RowsProcessed + 1) + ' has an odd numbers of entries.');
      exit;
    end;

    (* Get p and alpha *)
    P.Assign(StrToUint64Def(RowInputNumbers.Strings[0], 0));

    if P = 0 then
    begin
      ErrorMessage :='Error: Row ' + inttostr(RowsProcessed + 1) + ' has an unrecognizable entry for P, "' + RowInputNumbers.Strings[0] + '"';
      exit;
    end;

    Alpha := StrToUint64Def(RowInputNumbers.Strings[1], 0);

    if Alpha = 0 then
    begin
      Writeln('Error: Row ' + inttostr(RowsProcessed + 1) + ' has an unrecognizable entry for alpha.');
      exit;
    end;

    (* Make sure the q's and beta's array are big enough. They can be too big, but if they
    are too small, we need to grow the arrays and create gmp vars for each new var.
    To determine how many q's we have, we decrement by 2 to take into account p and alpha,
    and then remaining rowinputnumbers are 2 for each q: base and power. *)
    NumQs := (RowInputNumbers.Count - 2) div 2;
    while length(QsArray) < NumQs do
    begin
      (* Grow QsArray by one and create the gmp var *)
      setlength(QsArray, length(QsArray) + 1);
      QsArray[length(QsArray)-1].Create(0);
      (* Grow BetasArray by one and initialize it; it is not a gmp var *)
      setlength(BetasArray, length(BetasArray) + 1);
      BetasArray[length(BetasArray)-1] := 0;
    end;

    (* Read q's and beta's *)
    for i := 1 to NumQs do
    begin
      QsArray[i-1].Assign(StrToUint64Def(RowInputNumbers.Strings[(i*2)], 0));
      BetasArray[i-1] := StrToUint64Def(RowInputNumbers.Strings[(i*2)+1], 0);

      if (QsArray[i-1] = 0) then
      begin
        Writeln('Error: Row ' + inttostr(RowsProcessed + 1) + ' has an unrecognizable entry for q' + inttostr(i) + '.');
        exit;
      end;

      if (BetasArray[i-1] = 0) then
      begin
        Writeln('Error: Row ' + inttostr(RowsProcessed + 1) + ' has an unrecognizable entry for b' + inttostr(i) + '.');
        exit;
      end;

    end;

    result := ProcessParsedInput(InputStr, OutputStr);

    if result = true then
      inc(RowsProcessed);

  end;

  procedure TOpnPropProc.CreateGmpVars;
  begin
    N.Create(0);
    P.Create(0);
    QPower.Create(0);
    PPower.Create(0);
    indpower.Create(0);
    largestindpower.Create(0);
    smallestindpower.Create(0);
    sigma.Create(0);
    divq.Create(0);
    divr.Create(0);
    FloatSigma.Create(0);
    NumeratorPower.Create(0);
    (* QsArray will be sized and vars created as needed *)
  end;

  procedure TOpnPropProc.FreeGmpVars;
  var
    i: integer;
  begin
    N.Free;
    P.Free;
    QPower.Free;
    PPower.Free;
    indpower.Free;
    largestindpower.Free;
    smallestindpower.Free;
    sigma.Free;
    divq.Free;
    divr.Free;
    FloatSigma.Free;
    NumeratorPower.Free;
    if length(QsArray) > 0 then
      for i := 0 to length(QsArray)-1 do
        QsArray[i].Free;
    SetLength(QsArray, 0);
    SetLength(BetasArray, 0);
  end;

  procedure TOpnPropProc.SetFPPrecChars(NumChars: integer);
  var
    BitPrec: integer;
  begin
    FPPrecChars := NumChars;


    (* NumChars is the number of decimal digits we want to right of decimal point,
    but gmp wants total precision in bits. For our purpose, we only need one
    digit to left of decimal point, and we need to determine how many bits
    of precision we need. *)
    BitPrec := trunc((NumChars+1)/Math.log10(2))+1;

    if BitPrec < GMP_MIN_PRECISION then
      BitPrec := GMP_MIN_PRECISION;

    SetDefaultFloatPrecision(BitPrec);

    (* Since we created gmp variables in the constructor, we need to change the
    precision of those vars to the new precision. So we just clear them and recreate
    them *)
    FreeGmpVars;
    CreateGmpVars;
  end;

  function TOpnPropProc.OutputHeaderString: string;
  begin
      (* Output column headers *)
      if not ShowDigits then
      begin
        if NoUnicode then
          result := '"Factors","m(N)","M(N)","p^a>10^_","Smallest q_^2b_","Smallest q^2b<10^_","Largest q_^2b_","Largest q^2b>10^_","Q^2≡1 mod 16","Q^2≡1 mod 32","N>10^_","N≡1 mod 12","N≡117 mod 468","N≡81 mod 324","105|N","s(N)/N~="'
        else
          result := '"Factors","ω(N)","Ω(N)","p^α>10^_","Smallest q_^2ß_","Smallest q^2ß<10^_","Largest q_^2ß_","Largest q^2ß>10^_","Q^2≡1 mod 16","Q^2≡1 mod 32","N>10^_","N≡1 mod 12","N≡117 mod 468","N≡81 mod 324","105|N","σ(N)/N≈"';
      end
      else
      begin
        if NoUnicode then
          result := '"Factors","m(N)","M(N)","p^a=", "p^a>10^_","Smallest q_^2b_","Smallest q^2b=","Smallest q^2b<10^_","Largest q_^2b_","Largest q^2b="'
              + ',"Largest q^2b>10^_","Q^2≡1 mod 16","Q^2≡1 mod 32","N=","N>10^_","N≡1 mod 12","N≡117 mod 468","N≡81 mod 324","105|N","s(N)=","s(N)/N~="'
        else
          result := '"Factors","ω(N)","Ω(N)","p^α=","p^α>10^_","Smallest q_^2ß_","Smallest q^2ß=","Smallest q^2ß<10^_","Largest q_^2ß_","Largest q^2ß="'
              + ',"Largest q^2ß>10^_","Q^2≡1 mod 16","Q^2≡1 mod 32","N=","N>10^_","N≡1 mod 12","N≡117 mod 468","N≡81 mod 324","105|N","σ(N)=","σ(N)/N≈"';
      end;

  end;

  constructor TOpnPropProc.Create;
  begin
    inherited;

    (* Create RowInputNumbers *)
    RowInputNumbers := TStringList.Create;

    (* After we set default precision, we can create float gmp var, along
    with other gmp vars that need to be created too *)
    CreateGmpVars;

    RowsProcessed := 0;
  end;

  destructor TOpnPropProc.Destroy;
  begin
    if Assigned(RowInputNumbers) then
      RowInputNumbers.Free;

    FreeGmpVars;

    inherited;
  end;

end.
