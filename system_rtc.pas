unit System_Rtc;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ExtCtrls;

type

    { TSystemRtc }

    TSystemRtc = class

    private   // Attribute
        type
        TBitReg8 = bitpacked record
            case byte of
                0: (Value: byte); // 8Bit Register Value
                2: (bit: bitpacked array[0..7] of boolean); // Bit Data
        end;

    var
        clkUpdateTimer: TTimer;
        clkDateTime, almDateTime: TDateTime;
        rtcRam: array[0..127] of byte;
        dataAddr, year, century: byte;
        ctrlA, ctrlB, ctrlC, ctrlD: TBitReg8;
        corrSeconds: word;

    const
        // Bitkonstanten für Control Register A
        UIP = $07; // Update in Progress
        DV = $70;  // Oszillator Control
        RS = $0F;  // Rate Selector

        // Bitkonstanten für Control Register B
        SB = $07;   // Set Bit
        PIE = $06;  // Periodic Interrupt Enable
        AIE = $05;  // Alarm Interrupt Enable
        UIE = $04;  // Update-Ended Interrupt Enable
        SQWE = $03; // Square-Wave Enable
        DM = $02;   // Data Mode: 0 = BCD-Mode , 1 = Binary-Mode
        CM24 = $01; // Clock Mode: 0 = 12-Hour Mode , 1 = 24-Hour Mode
        DSE = $00;  // Daylight Saving Enable

        // Bitkonstanten für Control Register C
        IRQF = $07;  // Interrupt Request Flag
        PF = $06;    // Periodic Interrupt Flag
        AF = $05;    // Alarm Interrupt Flag
        UF = $04;    // Update-Ended Interrupt Flag

        // Bitkonstanten für Control Register D
        VRT = $07; // Valid RAM and Time

    protected // Attribute

    public    // Attribute

    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    private   // Methoden
        procedure checkUpdateTimer;
        function BcdToInteger(Value: byte): byte;
        function IntegerToBcd(Value: byte): byte;
        procedure saveRtcData;
        procedure readRtcData;

    protected // Methoden
        procedure doClock(Sender: TObject);

    public    // Methoden
        procedure setAddress(addr: byte);
        procedure Write(Data: byte);
        function Read: byte;
        procedure reset;

    end;

var
    SystemRtc: TSystemRtc;

implementation

{ TSystemRtc }

uses DateUtils, StrUtils, System_Settings;

// --------------------------------------------------------------------------------
constructor TSystemRtc.Create;
begin
    inherited Create;
    clkUpdateTimer := TTimer.Create(nil);
    clkUpdateTimer.Enabled := False;
    clkUpdateTimer.Interval := 100;
    clkUpdateTimer.OnTimer := @doClock;
    clkDateTime := now;
    year := (FormatDateTime('yy', clkDateTime).ToInteger and $7F);
    century := ((((FormatDateTime('yyyy', clkDateTime).ToInteger) - year) div 100) and $7F);
    readRtcData;
    ctrlD.bit[VRT] := True;
    corrSeconds := 0;
    checkUpdateTimer;
end;

// --------------------------------------------------------------------------------
destructor TSystemRtc.Destroy;
begin
    if Assigned(clkUpdateTimer) then begin
        clkUpdateTimer.Enabled := False;
        clkUpdateTimer.OnTimer := nil;
        clkUpdateTimer.Destroy;
    end;
    saveRtcData;
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
procedure TSystemRtc.checkUpdateTimer;
begin
    if ((ctrlA.Value and DV) = %00100000) then begin
        clkUpdateTimer.Enabled := True;
    end
    else begin
        clkUpdateTimer.Enabled := False;
    end;
end;

// --------------------------------------------------------------------------------
function TSystemRtc.BcdToInteger(Value: byte): byte;
begin
    Result := (Value and $0F);
    Result := Result + (((Value shr 4) and $0F) * 10);
end;

// --------------------------------------------------------------------------------
function TSystemRtc.IntegerToBcd(Value: byte): byte;
begin
    Result := Value div 10 mod 10;
    Result := (Result shl 4) or Value mod 10;
end;

// --------------------------------------------------------------------------------
procedure TSystemRtc.saveRtcData;
var
    Data: string;
    addr: byte;
begin
    Data := IntToHex(ctrlA.Value, 2);
    Data := Data + IntToHex(ctrlB.Value, 2);
    Data := Data + IntToHex(ctrlC.Value, 2);
    Data := Data + IntToHex(ctrlD.Value, 2);
    for addr := $0E to $31 do begin
        Data := Data + IntToHex(rtcRam[addr], 2);
    end;
    for addr := $33 to $7F do begin
        Data := Data + IntToHex(rtcRam[addr], 2);
    end;
    SystemSettings.WriteString('RTC', 'Data', Data);
end;

// --------------------------------------------------------------------------------
procedure TSystemRtc.readRtcData;
var
    Data: string;
    addr, index: byte;
begin
    Data := SystemSettings.ReadString('RTC', 'Data', '');
    if (Data.IsEmpty) then
        exit;
    ctrlA.Value := Hex2Dec(Data.Substring(0, 2));
    ctrlB.Value := Hex2Dec(Data.Substring(2, 2));
    ctrlC.Value := Hex2Dec(Data.Substring(4, 2));
    ctrlD.Value := Hex2Dec(Data.Substring(6, 2));
    index := 8;
    for addr := $0E to $31 do begin
        rtcRam[addr] := Hex2Dec(Data.Substring(index, 2));
        Inc(index, 2);
    end;
    for addr := $33 to $7F do begin
        rtcRam[addr] := Hex2Dec(Data.Substring(index, 2));
        Inc(index, 2);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemRtc.doClock(Sender: TObject);
begin
    if (not ctrlB.bit[SB]) then begin
        ctrlA.bit[UIP] := True;
        clkDateTime := IncMilliSecond(clkDateTime, 100);
        ctrlA.bit[UIP] := False;
    end
    else begin
        Inc(corrSeconds);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemRtc.setAddress(addr: byte);
begin
    dataAddr := (addr and $7F);
end;

// --------------------------------------------------------------------------------
procedure TSystemRtc.Write(Data: byte);
begin
    case (dataAddr) of
        $00: begin // write Clock Seconds
            if (ctrlB.bit[DM]) then begin  // binär
                Data := (Data and $3F);
            end
            else begin  // bcd
                Data := BcdToInteger(Data and $7F);
            end;
            clkDateTime := RecodeSecond(clkDateTime, Data);
        end;
        $01: begin // write Alarm Seconds
            if (ctrlB.bit[DM]) then begin  // binär
                Data := (Data and $3F);
            end
            else begin  // bcd
                Data := BcdToInteger(Data and $7F);
            end;
            almDateTime := RecodeSecond(almDateTime, Data);
        end;
        $02: begin // write Clock Minutes
            if (ctrlB.bit[DM]) then begin  // binär
                Data := (Data and $3F);
            end
            else begin  // bcd
                Data := BcdToInteger(Data and $7F);
            end;
            clkDateTime := RecodeMinute(clkDateTime, Data);
        end;
        $03: begin // write Alarm Minutes
            if (ctrlB.bit[DM]) then begin  // binär
                Data := (Data and $3F);
            end
            else begin  // bcd
                Data := BcdToInteger(Data and $7F);
            end;
            almDateTime := RecodeMinute(almDateTime, Data);
        end;
        $04: begin // write Clock Hours
            if (ctrlB.bit[DM]) then begin  // binär
                if (ctrlB.bit[CM24]) then begin  // binär 24Std
                    Data := (Data and $1F);
                end
                else begin  // binär 12Std
                    Data := (Data and $8F);
                    if ((Data and $80) <> 0) then begin
                        //Data := ((Data + 12) - 128);
                        Data := ((Data and $1F) + 12);
                    end;
                end;
            end
            else begin  // bcd
                if (ctrlB.bit[CM24]) then begin  // bcd 24Std
                    Data := BcdToInteger(Data and $3F);
                end
                else begin  // bcd 12Std
                    Data := (Data and $9F);
                    if ((Data and $80) <> 0) then begin
                        Data := (BcdToInteger((Data and $3F)) + 12);
                    end;
                end;
            end;
            clkDateTime := RecodeHour(clkDateTime, Data);
        end;
        $05: begin // write Alarm Hours
            if (ctrlB.bit[DM]) then begin  // binär
                if (ctrlB.bit[CM24]) then begin  // binär 24Std
                    Data := (Data and $1F);
                end
                else begin  // binär 12Std
                    Data := (Data and $8F);
                    if ((Data and $80) <> 0) then begin
                        //Data := ((Data + 12) - 128);
                        Data := ((Data and $1F) + 12);
                    end;
                end;
            end
            else begin  // bcd
                if (ctrlB.bit[CM24]) then begin  // bcd 24Std
                    Data := BcdToInteger(Data and $3F);
                end
                else begin  // bcd 12Std
                    Data := (Data and $9F);
                    if ((Data and $80) <> 0) then begin
                        Data := (BcdToInteger((Data and $3F)) + 12);
                    end;
                end;
            end;
            almDateTime := RecodeHour(almDateTime, Data);
        end;
        $06: begin // write Clock Day
            //clkDay := ((clkDay and not $07) or (Data and $07));
        end;
        $07: begin // write Clock Date
            if (ctrlB.bit[DM]) then begin  // binär
                Data := (Data and $1F);
            end
            else begin  // bcd
                Data := BcdToInteger(Data and $3F);
            end;
            clkDateTime := RecodeDay(clkDateTime, Data);
        end;
        $08: begin // write Clock Month
            if (ctrlB.bit[DM]) then begin  // binär
                Data := (Data and $0F);
            end
            else begin  // bcd
                Data := BcdToInteger(Data and $1F);
            end;
            clkDateTime := RecodeMonth(clkDateTime, Data);
        end;
        $09: begin // write Clock Year
            if (ctrlB.bit[DM]) then begin  // binär
                year := (Data and $7F);
            end
            else begin  // bcd
                year := BcdToInteger(Data and $FF);
            end;
            clkDateTime := RecodeYear(clkDateTime, ((century * 100) + year));
        end;
        $0A: begin // write Control Register A
            ctrlA.Value := ((ctrlA.Value and not $7F) or (Data and $7F));
            checkUpdateTimer;
        end;
        $0B: begin // write Control Register B
            ctrlB.Value := ((ctrlB.Value and not $FF) or (Data and $FF));
            if ((not ctrlB.bit[SB]) and (corrSeconds > 0)) then begin
                clkDateTime := IncMilliSecond(clkDateTime, (100 * corrSeconds));
                corrSeconds := 0;
            end;
        end;
        $0C: begin // write Control Register C
            ctrlC.Value := ((ctrlC.Value and not $00) or (Data and $00));
        end;
        $0D: begin // write Control Register D
            ctrlD.Value := ((ctrlD.Value and not $00) or (Data and $00));
        end;
        $0E..$31: begin
            rtcRam[dataAddr] := Data;
        end;
        $32: begin // write Clock Century
            if (not ctrlB.bit[DM]) then begin  // bcd only
                century := BcdToInteger(Data and $FF);
                clkDateTime := RecodeYear(clkDateTime, ((century * 100) + year));
            end;
        end;
        $33..$7F: begin
            rtcRam[dataAddr] := Data;
        end;
    end;
end;

// --------------------------------------------------------------------------------
function TSystemRtc.Read: byte;
begin
    case (dataAddr) of
        $00: begin // read Clock Seconds
            Result := (FormatDateTime('ss', clkDateTime).ToInteger);
            if (ctrlB.bit[DM]) then begin  // binär
                Result := (Result and $3F);
            end
            else begin // bcd
                Result := (IntegerToBcd(Result) and $7F);
            end;
        end;
        $01: begin // read Alarm Seconds
            Result := (FormatDateTime('ss', almDateTime).ToInteger);
            if (ctrlB.bit[DM]) then begin  // binär
                Result := (Result and $3F);
            end
            else begin // bcd
                Result := (IntegerToBcd(Result) and $7F);
            end;
        end;
        $02: begin // read Clock Minutes
            Result := (FormatDateTime('nn', clkDateTime).ToInteger);
            if (ctrlB.bit[DM]) then begin  // binär
                Result := (Result and $3F);
            end
            else begin // bcd
                Result := (IntegerToBcd(Result) and $7F);
            end;
        end;
        $03: begin // read Alarm Minutes
            Result := (FormatDateTime('nn', almDateTime).ToInteger);
            if (ctrlB.bit[DM]) then begin  // binär
                Result := (Result and $3F);
            end
            else begin // bcd
                Result := (IntegerToBcd(Result) and $7F);
            end;
        end;
        $04: begin // read Clock Hours
            Result := (FormatDateTime('hh', clkDateTime).ToInteger and $1F);
            if (ctrlB.bit[DM]) then begin  // binär
                if (not ctrlB.bit[CM24]) then begin  // binär 12Std
                    if (IsPM(clkDateTime)) then begin
                        Result := (((Result - 12) + 128) and $8F);
                    end;
                end;
            end
            else begin  // bcd
                if (ctrlB.bit[CM24]) then begin  // bcd 24Std
                    Result := (IntegerToBcd(Result) and $3F);
                end
                else begin  // bcd 12Std
                    if (IsPM(clkDateTime)) then begin
                        Result := (IntegerToBcd(Result - 12) and $1F);
                        Result := ((Result + 128) and $9F);
                    end;
                end;
            end;
        end;
        $05: begin // read Alarm Hours
            Result := (FormatDateTime('hh', almDateTime).ToInteger and $1F);
            if (ctrlB.bit[DM]) then begin  // binär
                if (not ctrlB.bit[CM24]) then begin  // binär 12Std
                    if (IsPM(clkDateTime)) then begin
                        Result := (((Result - 12) + 128) and $8F);
                    end;
                end;
            end
            else begin  // bcd
                if (ctrlB.bit[CM24]) then begin  // bcd 24Std
                    Result := (IntegerToBcd(Result) and $3F);
                end
                else begin  // bcd 12Std
                    if (IsPM(almDateTime)) then begin
                        Result := (IntegerToBcd(Result - 12) and $1F);
                        Result := ((Result + 128) and $9F);
                    end;
                end;
            end;
        end;
        $06: begin // read Clock Day
            Result := (DayOfTheWeek(clkDateTime) and $07);
        end;
        $07: begin // read Clock Date
            Result := (FormatDateTime('dd', clkDateTime).ToInteger);
            if (ctrlB.bit[DM]) then begin  // binär
                Result := (Result and $1F);
            end
            else begin  // bcd
                Result := (IntegerToBcd(Result) and $3F);
            end;
        end;
        $08: begin // read Clock Month
            Result := (FormatDateTime('mm', clkDateTime).ToInteger);
            if (ctrlB.bit[DM]) then begin  // binär
                Result := (Result and $0F);
            end
            else begin // bcd
                Result := (IntegerToBcd(Result) and $1F);
            end;
        end;
        $09: begin // read Clock Year
            if (ctrlB.bit[DM]) then begin  // binär
                Result := (year and $7F);
            end
            else begin  // bcd
                Result := (IntegerToBcd(year) and $FF);
            end;
        end;
        $0A: begin // read Control Register A
            Result := (ctrlA.Value and $FF);
        end;
        $0B: begin // read Control Register B
            Result := (ctrlB.Value and $FF);
        end;
        $0C: begin // read Control Register C
            Result := (ctrlC.Value and $F0);
            ctrlC.Value := 0;
        end;
        $0D: begin // read Control Register D
            Result := (ctrlD.Value and $80);
        end;
        $0E..$31: begin
            Result := rtcRam[dataAddr];
        end;
        $32: begin // read Clock Century
            if (not ctrlB.bit[DM]) then begin  // bcd only
                Result := (IntegerToBcd(century) and $FF);
            end;
        end;
        $33..$7F: begin
            Result := rtcRam[dataAddr];
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemRtc.reset;
begin
    ctrlB.bit[PIE] := False;
    ctrlB.bit[AIE] := False;
    ctrlB.bit[UIE] := False;
    ctrlB.bit[SQWE] := False;
    ctrlC.bit[IRQF] := False;
    ctrlC.bit[PF] := False;
    ctrlC.bit[AF] := False;
    ctrlC.bit[UF] := False;
end;

// --------------------------------------------------------------------------------
end.

