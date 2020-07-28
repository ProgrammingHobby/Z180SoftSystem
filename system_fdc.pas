unit System_Fdc;

{$mode objfpc}{$H+}

{$ifdef _DEBUG_}
{$define FASTFDC}
{$endif}

interface

uses
    Classes, SysUtils, ExtCtrls, Graphics;

type

    { TSystemFdc }

    TSystemFdc = class

    private   // Attribute
        type
        TBitReg8 = bitpacked record
            case byte of
                0: (Value: byte); // 8Bit Register Value
                2: (bit: bitpacked array[0..7] of boolean); // Bit Data
        end;

        TFloppyDriveData = record
            Sides: integer;
            Tracks: integer;
            Sectors: integer;
            SectorBytes: integer;
            ImageFileName: string;
            Size: integer;
            ImageChanged: boolean;
            MotorOn: boolean;
            FddStatus: TPanel;
        end;

    var
        timerFdcBusy: TTimer;
        timerFdcMotorOn: TTimer;
        timerFddStatus: TTimer;
        fdcStatus: TBitReg8;
        fdcTrack, tmpTrack: byte;
        fdcSector: byte;
        fdcData: byte;
        fdcSide: byte;
        byteCount: word;
        extStatus: TBitReg8;
        extControl: TBitReg8;
        stepForward: boolean;
        isMultiSectorCommand: boolean;
        isReadCommand: boolean;
        isWriteCommand: boolean;
        canClearIntrq: boolean;
        floppyDrive0, floppyDrive1, actualFloppyDrive: TFloppyDriveData;
        fddData: file of byte;

    const

        // Konstanten fuer die 'fdcStatus' Bit-Adressen
        BUSY = 0; // When set, command is under execution.
        // When reset, no command is under execution.
        DRI = 1; // This bit is a copy of the DRQ output. When set, it indicates the DR is full
        // on a Read Operation or the DR is empty on a Write operation. This bit is
        // reset to zero when updated. On Type 1 commands, this bit indicates the
        // status of the IP signal.
        LDB = 2; // When set, it indicates the computer did not respond to DRQ in one byte time.
        // This bit is reset to zero when updated. On Type 1 commands, this bit
        // reflects the status of the TR00 signal.
        CRCERROR = 3; // If bit 4 (RNF) is set, an error is found in one or more ID fields,
        // otherwise it indicates error data field. This bit is reset when updated.
        RNF = 4; // Record not found. When set, it indicates that the desired track, sector or side
        // were not found. This bit is reset when updated.
        RTSU = 5; // When set, this bit indicates that the Motor Spin-Up sequence has completed
        // on Type 1 commands. On Type 2 & 3 commands, this bit indicates record Type:
        // 0 = Data Mark , 1 = Deleted Data Mark.
        WP = 6; // On Read: not used. On any Write: it indicates a Write Protect. This bit is reset when updated.
        MON = 7; // This bit reflects the status of the Motor On output.

        // Konstanten fuer die 'extStatus' Bit-Adressen
        INTQ = 0; // INTRQ-Signal des FDC, ist es gesetzt wurde das zuletzt an den
        // FDC gesendete Kommando abgearbeitet.
        DRQ = 1; // DRQ-Signal des FDC, ist es gesetzt enthält das Datenregister
        // einen neuen zu lesenden Wert, oder es kann ein neuer Wert
        // geschrieben werden.
        WPS = 2; // Write-Protect Signal vom FDD-Bus, ist dieses Bit gesetzt,
        // ist die im selektierten Laufwerk eingelegte Diskette schreibgeschützt.
        DC = 3; // Disk-Change Signal vom FDD-Bus, ist dieses Bit gesetzt, wurde
        // vom selektierten Laufwerk ein Diskettenwechsel erkannt.

        // Konstanten fuer die 'extControl' Bit-Adressen
        MRES = 0; // MRESET-Pin des FDC, ist das Bit gesetzt wird der FDC
        // zurückgesetzt. Dieses Bit muss für den Betrieb gelöscht sein.
        DDEN = 1; // DDEN-PIN des FDC, wenn dieses Bit gesetzt ist wird der
        // Double-Density Modus des FDC eingeschaltet. Bei gelöschtem
        // Bit arbeitet der FDC in Single-Density Modus.
        D1S = 2; // Wenn dieses Bit gesetzt ist, wird Drive-1 selektiert.
        D0S = 3; // Wenn dieses Bit gesetzt ist, wird Drive-0 selektiert.
        SS = 4; // Side selekt der Laufwerke. Gesetzt Seite 1, gelöscht Seite 0

    protected // Attribute

    public    // Attribute

    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    private   // Methoden
        procedure doReset;
        procedure setMotorOn;
        procedure recallMotorOn;
        function calcSteppingRate(command: byte): DWord;
        function calcFilePosition: DWord;
        procedure setFddOffState(Sender: TObject);
        procedure setFddReadState;
        procedure setFddWriteState;

    protected // Methoden
        procedure resetMotorOnSignal(Sender: TObject);
        procedure clearBusySetIntrqUpdateTrack(Sender: TObject);
        procedure clearBusySetRecordNotFoundSetIntrq(Sender: TObject);
        procedure performSectorRead(Sender: TObject);
        procedure performSectorWrite(Sender: TObject);

    public    // Methoden
        function getStatus: byte;
        procedure setCommand(command: byte);
        function getTrack: byte;
        procedure setTrack(track: byte);
        function getSector: byte;
        procedure setSector(sector: byte);
        function readData: byte;
        procedure writeData(Data: byte);
        function getExtStatus: byte;
        procedure setExtControl(control: byte);
        procedure setFdd0Sides(sides: integer);
        procedure setFdd0Tracks(tracks: integer);
        procedure setFdd0Sectors(sectors: integer);
        procedure setFdd0SectorBytes(sectorbytes: integer);
        function setFdd0Image(FileName: string): boolean;
        procedure setFdd0StatusPanel(var panel: TPanel);
        procedure setFdd1Sides(sides: integer);
        procedure setFdd1Tracks(tracks: integer);
        procedure setFdd1Sectors(sectors: integer);
        procedure setFdd1SectorBytes(sectorbytes: integer);
        function setFdd1Image(FileName: string): boolean;
        procedure setFdd1StatusPanel(var panel: TPanel);

    end;

var
    SystemFdc: TSystemFdc;

implementation

{ TSystemFdc }

// --------------------------------------------------------------------------------
constructor TSystemFdc.Create;
begin
    inherited Create;
    timerFdcBusy := TTimer.Create(nil);
    timerFdcBusy.Enabled := False;
    timerFdcBusy.Interval := 1000;
    timerFdcBusy.OnTimer := nil;
    timerFdcMotorOn := TTimer.Create(nil);
    timerFdcMotorOn.Enabled := False;
    timerFdcMotorOn.Interval := 1800;   // entspricht 9 Umdrehungen
    timerFdcMotorOn.OnTimer := @resetMotorOnSignal;
    timerFddStatus := TTimer.Create(nil);
    timerFddStatus.Enabled := False;
    timerFddStatus.Interval := 50;
    timerFddStatus.OnTimer := @setFddOffState;
    floppyDrive0.ImageFileName := '';
    floppyDrive0.Size := 0;
    floppyDrive0.ImageChanged := False;
    floppyDrive0.MotorOn := False;
    floppyDrive1.ImageFileName := '';
    floppyDrive1.Size := 0;
    floppyDrive1.ImageChanged := False;
    floppyDrive1.MotorOn := False;
    doReset;
end;

// --------------------------------------------------------------------------------
destructor TSystemFdc.Destroy;
begin
    if Assigned(timerFdcMotorOn) then begin
        timerFdcMotorOn.Enabled := False;
        timerFdcMotorOn.OnTimer := nil;
        timerFdcMotorOn.Destroy;
    end;
    if Assigned(timerFdcBusy) then begin
        timerFdcBusy.Enabled := False;
        timerFdcBusy.OnTimer := nil;
        timerFdcBusy.Destroy;
    end;
    if Assigned(timerFddStatus) then begin
        timerFddStatus.Enabled := False;
        timerFddStatus.OnTimer := nil;
        timerFddStatus.Destroy;
    end;
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.doReset;
begin
    fdcStatus.Value := 0;
    tmpTrack := 0;
    fdcTrack := tmpTrack;
    fdcSector := 1;
    fdcData := 0;
    extStatus.Value := 0;
    stepForward := True;
    isMultiSectorCommand := False;
    isReadCommand := False;
    isWriteCommand := False;
    canClearIntrq := True;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setMotorOn;
begin
    actualFloppyDrive.MotorOn := True;
    timerFdcMotorOn.Enabled := False;
    timerFdcMotorOn.Enabled := True;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.recallMotorOn;
begin
    if not timerFdcMotorOn.Enabled then
        exit;
    timerFdcMotorOn.Enabled := False;
    timerFdcMotorOn.Enabled := True;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.calcSteppingRate(command: byte): DWord;
begin
    case (command and %00000011) of
        0: Result := 6;    // Stepping Rate 6ms
        1: Result := 12;   // Stepping Rate 12ms
        2: Result := 20;   // Stepping Rate 20ms
        3: Result := 30;   // Stepping Rate 30ms
    end;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.calcFilePosition: DWord;
begin
    Result := (((((fdcTrack * actualFloppyDrive.Sides) + fdcSide) * actualFloppyDrive.Sectors) + (fdcSector - 1)) * actualFloppyDrive.SectorBytes);
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFddOffState(Sender: TObject);
begin
    timerFddStatus.Enabled := False;
    with actualFloppyDrive.FddStatus do begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := clDefault;
        Canvas.Pen.Style := psClear;
        Canvas.Pen.Color := clDefault;
        Canvas.Pen.Width := 2;
        Canvas.Ellipse(20, 0, 31, 10);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFddReadState;
begin
    timerFddStatus.Enabled := False;
    with actualFloppyDrive.FddStatus do begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := clLime;
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Color := clGreen;
        Canvas.Pen.Width := 1;
        Canvas.Ellipse(21, 1, 29, 9);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFddWriteState;
begin
    timerFddStatus.Enabled := False;
    with actualFloppyDrive.FddStatus do begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := clRed;
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Color := clMaroon;
        Canvas.Pen.Width := 1;
        Canvas.Ellipse(21, 1, 29, 9);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.resetMotorOnSignal(Sender: TObject);
begin
    timerFdcMotorOn.Enabled := False;
    actualFloppyDrive.MotorOn := False;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.clearBusySetIntrqUpdateTrack(Sender: TObject);
begin
    timerFdcBusy.Enabled := False;
    fdcTrack := tmpTrack;
    extStatus.bit[INTQ] := True;
    fdcStatus.bit[BUSY] := False;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.clearBusySetRecordNotFoundSetIntrq(Sender: TObject);
begin
    timerFdcBusy.Enabled := False;
    extStatus.bit[INTQ] := True;
    fdcStatus.bit[RNF] := True;
    fdcStatus.bit[BUSY] := False;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.performSectorRead(Sender: TObject);
var
    filePos: DWord;
begin
    timerFdcBusy.Enabled := False;
    filePos := calcFilePosition;
    if (fdcSector > actualFloppyDrive.Sectors) then begin    // ist der angeforderte Sector nicht vorhanden
        timerFdcBusy.Interval := 600; // Zeitspanne fuer 4 Umdrehungen
        timerFdcBusy.OnTimer := @clearBusySetRecordNotFoundSetIntrq;  // und Command beenden
        timerFdcBusy.Enabled := True;
    end
    else if ((filePos > (actualFloppyDrive.Size - actualFloppyDrive.SectorBytes)) or (actualFloppyDrive.Size = 0) or
        (fdcSector > actualFloppyDrive.Sectors) or (fdcTrack >= actualFloppyDrive.Tracks)) then begin  // Sector und/oder Track ausserhalb der Disk
        clearBusySetRecordNotFoundSetIntrq(nil);
        exit;
    end
    else begin
        try
            Reset(fddData);
            Seek(fddData, filePos);
        except
            extStatus.bit[INTQ] := True;
            fdcStatus.bit[BUSY] := False;
            fdcStatus.bit[CRCERROR] := True;
            exit;
        end;
        byteCount := 0;
        fdcStatus.bit[DRQ] := True;
        extStatus.bit[DRQ] := True;
        setFddReadState;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.performSectorWrite(Sender: TObject);
var
    filePos: DWord;
begin
    timerFdcBusy.Enabled := False;
    filePos := calcFilePosition;
    if (fdcSector > actualFloppyDrive.Sectors) then begin    // ist der angeforderte Sector nicht vorhanden
        timerFdcBusy.Interval := 600; // Zeitspanne fuer 4 Umdrehungen
        timerFdcBusy.OnTimer := @clearBusySetRecordNotFoundSetIntrq;  // und Command beenden
        timerFdcBusy.Enabled := True;
    end
    else if ((filePos > (actualFloppyDrive.Size - actualFloppyDrive.SectorBytes)) or (actualFloppyDrive.Size = 0) or
        (fdcSector > actualFloppyDrive.Sectors) or (fdcTrack >= actualFloppyDrive.Tracks)) then begin  // Sector und/oder Track ausserhalb der Disk
        clearBusySetRecordNotFoundSetIntrq(nil);
    end
    else begin
        try
            Reset(fddData);
            Seek(fddData, filePos);
        except
            extStatus.bit[INTQ] := True;
            fdcStatus.bit[BUSY] := False;
            fdcStatus.bit[CRCERROR] := True;
            exit;
        end;
        byteCount := 0;
        fdcStatus.bit[DRQ] := True;
        extStatus.bit[DRQ] := True;
        setFddWriteState;
    end;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.getStatus: byte;
begin
    // Verhalten nicht eindeutig geklaert. Auf der Original Hardware scheint das Flag verzoegert gejoescht zu werden.
    //if (canClearIntrq) then begin
    //extStatus.bit[INTQ] := False; // beim Lesen des Status-Registers wird das Interrupt-Flag geloescht
    //end;
    fdcStatus.bit[MON] := actualFloppyDrive.MotorOn;
    Result := fdcStatus.Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setCommand(command: byte);
var
    cmdDuration: DWord;
    cmd: TbitReg8;
begin
    cmd.Value := command;
    fdcStatus.bit[BUSY] := True;
    actualFloppyDrive.ImageChanged := False;
    recallMotorOn;
    cmdDuration := 1;
    case (cmd.Value and %11100000) of
        %00000000: begin    // Restore / Seek
            fdcStatus.bit[CRCERROR] := False;
            fdcStatus.bit[RNF] := False;
            fdcStatus.bit[DRQ] := False;
            fdcStatus.bit[RTSU] := False;
            extStatus.bit[DRQ] := False;
            if (canClearIntrq) then begin
                extStatus.bit[INTQ] := False;
            end;
            if (not cmd.bit[4]) then begin  // Restore auf Track 0
                if (fdcTrack > 0) then begin  //  wenn aktueller Track groesser 0
                    stepForward := False;   // Step-Direction auf backward setzen
                end;
                cmdDuration := cmdDuration + (fdcTrack * calcSteppingRate(cmd.Value));    // Zeit fuer die Track moves
                tmpTrack := 0;
            end
            else begin  // Seek auf angeforderten Track
                if (fdcTrack > fdcData) then begin  // wenn aktueller Track groesser angeforderter Track
                    stepForward := False;   // Step-Direction auf backward setzen
                end
                else begin
                    stepForward := True;    // ansonsten Step-Direction forward
                end;
                if (fdcTrack < (actualFloppyDrive.Tracks - 1)) then begin
                    cmdDuration := cmdDuration + (abs(fdcTrack - fdcData) * calcSteppingRate(cmd.Value));    // Zeit fuer die Track moves
                end
                else begin  // falls der angeforderte Track nicht erreichbar ist
                    cmdDuration := cmdDuration + (255 * calcSteppingRate(cmd.Value)); // werden 255 Step simuliert
                    fdcStatus.bit[RNF] := True;   // und 'Seek Error / Record not Found' Flag setzen
                end;
                tmpTrack := fdcData;
            end;
            if (not cmd.bit[3]) and (not actualFloppyDrive.MotorOn) then begin   // evtl. zusaetzliche Zeit fuer Spin-Up
                setMotorOn;
                cmdDuration := cmdDuration + 1000;    // 1200ms entsprechen 6 Umdrehungen des FDD-Laufwerkes
                fdcStatus.bit[RTSU] := True;
            end;
            if (cmd.bit[2]) then begin    // falls ein Verify erfolgen soll
                cmdDuration := cmdDuration + 30;   // nochmals 30ms Dauer dazu
            end;
            {$ifndef FASTFDC}
            timerFdcBusy.Interval := cmdDuration;
            timerFdcBusy.OnTimer := @clearBusySetIntrqUpdateTrack;
            timerFdcBusy.Enabled := True;
            {$else}
            clearBusySetIntrqUpdateTrack(nil);
            {$endif}
        end;    // Restore / Seek
        %00100000: begin    // Step
            fdcStatus.bit[CRCERROR] := False;
            fdcStatus.bit[RNF] := False;
            fdcStatus.bit[DRQ] := False;
            fdcStatus.bit[RTSU] := False;
            extStatus.bit[DRQ] := False;
            if (canClearIntrq) then begin
                extStatus.bit[INTQ] := False;
            end;
            cmdDuration := cmdDuration + calcSteppingRate(cmd.Value);
            tmpTrack := fdcTrack;
            if (cmd.bit[4]) then begin    // Track-Register Update
                if (stepForward) then begin
                    tmpTrack := tmpTrack + 1;
                end
                else begin
                    tmpTrack := tmpTrack - 1;
                end;
            end;
            if (not cmd.bit[3]) and (not actualFloppyDrive.MotorOn) then begin   // evtl. zusaetzliche Zeit fuer Spin-Up
                setMotorOn;
                cmdDuration := cmdDuration + 1000;    // 1200ms entsprechen 6 Umdrehungen des FDD-Laufwerkes
                fdcStatus.bit[RTSU] := True;
            end;
            if (cmd.bit[2]) then begin    // falls ein Verify erfolgen soll
                cmdDuration := cmdDuration + 30;   // nochmals 30ms Dauer dazu
            end;
            {$ifndef FASTFDC}
            timerFdcBusy.Interval := cmdDuration;
            timerFdcBusy.OnTimer := @clearBusySetIntrqUpdateTrack;
            timerFdcBusy.Enabled := True;
            {$else}
            clearBusySetIntrqUpdateTrack(nil);
            {$endif}
        end;    // Step
        %01000000: begin    // Step-in
            fdcStatus.bit[CRCERROR] := False;
            fdcStatus.bit[RNF] := False;
            fdcStatus.bit[DRQ] := False;
            fdcStatus.bit[RTSU] := False;
            extStatus.bit[DRQ] := False;
            if (canClearIntrq) then begin
                extStatus.bit[INTQ] := False;
            end;
            cmdDuration := cmdDuration + calcSteppingRate(cmd.Value);
            tmpTrack := fdcTrack;
            stepForward := True;
            if (cmd.bit[4]) then begin    // Track-Register Update
                tmpTrack := tmpTrack + 1;
            end;
            if (not cmd.bit[3]) and (not actualFloppyDrive.MotorOn) then begin   // evtl. zusaetzliche Zeit fuer Spin-Up
                setMotorOn;
                cmdDuration := cmdDuration + 1000;    // 1200ms entsprechen 6 Umdrehungen des FDD-Laufwerkes
                fdcStatus.bit[RTSU] := True;
            end;
            if (cmd.bit[2]) then begin    // falls ein Verify erfolgen soll
                cmdDuration := cmdDuration + 30;   // nochmals 30ms Dauer dazu
            end;
            {$ifndef FASTFDC}
            timerFdcBusy.Interval := cmdDuration;
            timerFdcBusy.OnTimer := @clearBusySetIntrqUpdateTrack;
            timerFdcBusy.Enabled := True;
            {$else}
            clearBusySetIntrqUpdateTrack(nil);
            {$endif}
        end;    // Step-in
        %01100000: begin    // Step-out
            fdcStatus.bit[CRCERROR] := False;
            fdcStatus.bit[RNF] := False;
            fdcStatus.bit[DRQ] := False;
            fdcStatus.bit[RTSU] := False;
            extStatus.bit[DRQ] := False;
            if (canClearIntrq) then begin
                extStatus.bit[INTQ] := False;
            end;
            cmdDuration := cmdDuration + calcSteppingRate(cmd.Value);
            tmpTrack := fdcTrack;
            stepForward := False;
            if (cmd.bit[4]) then begin    // Track-Register Update
                tmpTrack := tmpTrack - 1;
            end;
            if (not cmd.bit[3]) and (not actualFloppyDrive.MotorOn) then begin   // evtl. zusaetzliche Zeit fuer Spin-Up
                setMotorOn;
                cmdDuration := cmdDuration + 1000;    // 1200ms entsprechen 6 Umdrehungen des FDD-Laufwerkes
                fdcStatus.bit[RTSU] := True;
            end;
            if (cmd.bit[2]) then begin    // falls ein Verify erfolgen soll
                cmdDuration := cmdDuration + 30;   // nochmals 30ms Dauer dazu
            end;
            {$ifndef FASTFDC}
            timerFdcBusy.Interval := cmdDuration;
            timerFdcBusy.OnTimer := @clearBusySetIntrqUpdateTrack;
            timerFdcBusy.Enabled := True;
            {$else}
            clearBusySetIntrqUpdateTrack(nil);
            {$endif}
        end;    // Step-out
        %10000000: begin    // Read Sector
            fdcStatus.bit[DRQ] := False;
            fdcStatus.bit[LDB] := False;
            fdcStatus.bit[RNF] := False;
            fdcStatus.bit[RTSU] := False;
            fdcStatus.bit[WP] := False;
            extStatus.bit[DRQ] := False;
            if (canClearIntrq) then begin
                extStatus.bit[INTQ] := False;
            end;
            isMultiSectorCommand := False;
            if ((not cmd.bit[0]) and (not cmd.bit[1])) then begin    // Bit 0 und 1 muessen gelöscht sein
                if (not cmd.bit[3]) and (not actualFloppyDrive.MotorOn) then begin   // evtl. zusaetzliche Zeit fuer Spin-Up
                    setMotorOn;
                    cmdDuration := cmdDuration + 1000;    // 1200ms entsprechen 6 Umdrehungen des FDD-Laufwerkes
                end;
                if (cmd.bit[2]) then begin    // falls ein Settling Delay erfolgen soll
                    cmdDuration := cmdDuration + 30;   // nochmals 30ms Dauer dazu
                end;
                if (cmd.bit[4]) then begin  // Bit 4 gesetzt, dann Multi-Sector Read
                    isMultiSectorCommand := True;
                end;
                isReadCommand := True;
                {$ifndef FASTFDC}
                timerFdcBusy.Interval := cmdDuration;
                timerFdcBusy.OnTimer := @performSectorRead;
                timerFdcBusy.Enabled := True;
                {$else}
                performSectorRead(nil);
                {$endif}
            end;
        end;    // Read Sector
        %10100000: begin    // Write Sector
            fdcStatus.bit[DRQ] := False;
            fdcStatus.bit[LDB] := False;
            fdcStatus.bit[RNF] := False;
            fdcStatus.bit[RTSU] := False;
            fdcStatus.bit[WP] := False;
            extStatus.bit[DRQ] := False;
            if (canClearIntrq) then begin
                extStatus.bit[INTQ] := False;
            end;
            isMultiSectorCommand := False;
            if (not cmd.bit[3]) and (not actualFloppyDrive.MotorOn) then begin   // evtl. zusaetzliche Zeit fuer Spin-Up
                setMotorOn;
                cmdDuration := cmdDuration + 1000;    // 1200ms entsprechen 6 Umdrehungen des FDD-Laufwerkes
            end;
            if (cmd.bit[2]) then begin    // falls ein Settling Delay erfolgen soll
                cmdDuration := cmdDuration + 30;   // nochmals 30ms Dauer dazu
            end;
            if (cmd.bit[4]) then begin  // Bit 4 gesetzt, dann Multi-Sector Write
                isMultiSectorCommand := True;
            end;
            isWriteCommand := True;
            {$ifndef FASTFDC}
            timerFdcBusy.Interval := cmdDuration;
            timerFdcBusy.OnTimer := @performSectorWrite;
            timerFdcBusy.Enabled := True;
            {$else}
            performSectorWrite(nil);
            {$endif}
        end;    // Write Sector
        %11000000: begin    // Force Interrupt
            if (cmd.bit[4]) then begin   // Bit 4 muß gesetzt sein
                if ((cmd.Value and $0f) = $00) then begin   // Beendet alle Befehle ohne INTRQ
                    isReadCommand := False;
                    isWriteCommand := False;
                    isMultiSectorCommand := False;
                    fdcStatus.bit[DRQ] := False;
                    extStatus.bit[DRQ] := False;
                    fdcStatus.bit[BUSY] := False;
                    canClearIntrq := True;
                end;
                if ((cmd.Value and $0f) = $08) then begin   // Beendet alle Befehle mit INTRQ
                    isReadCommand := False;
                    isWriteCommand := False;
                    isMultiSectorCommand := False;
                    fdcStatus.bit[DRQ] := False;
                    extStatus.bit[DRQ] := False;
                    fdcStatus.bit[BUSY] := False;
                    extStatus.bit[INTQ] := True;
                    canClearIntrq := False;
                end;
            end;
        end;
    end;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.getTrack: byte;
begin
    Result := fdcTrack;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setTrack(track: byte);
begin
    fdcTrack := track;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.getSector: byte;
begin
    Result := fdcSector;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setSector(sector: byte);
begin
    fdcSector := sector;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.readData: byte;
begin
    fdcStatus.bit[DRQ] := False;
    extStatus.bit[DRQ] := False;
    recallMotorOn;
    if (isReadCommand) then begin
        try
            Read(fddData, fdcData);
        except
            fdcStatus.bit[LDB] := True;
            timerFddStatus.Enabled := True;
            exit;
        end;
        byteCount := byteCount + 1;
        if (byteCount = actualFloppyDrive.SectorBytes) then begin
            if ((isMultiSectorCommand) and (fdcSector < actualFloppyDrive.Sectors)) then begin
                fdcSector := fdcSector + 1;
                byteCount := 0;
                fdcStatus.bit[DRQ] := True;
                extStatus.bit[DRQ] := True;
            end
            else begin
                extStatus.bit[DRQ] := False;
                extStatus.bit[INTQ] := True;
                fdcStatus.bit[DRQ] := False;
                fdcStatus.bit[BUSY] := False;
                CloseFile(fddData);
                isReadCommand := False;
                isMultiSectorCommand := False;
                timerFddStatus.Enabled := True;
            end;
        end
        else begin
            fdcStatus.bit[DRQ] := True;
            extStatus.bit[DRQ] := True;
        end;
    end;
    Result := fdcData;
end;
// --------------------------------------------------------------------------------
procedure TSystemFdc.writeData(Data: byte);
begin
    fdcStatus.bit[DRQ] := False;
    extStatus.bit[DRQ] := False;
    recallMotorOn;
    if (isWriteCommand) then begin
        try
            Write(fddData, Data);
        except
            fdcStatus.bit[LDB] := True;
            timerFddStatus.Enabled := True;
            exit;
        end;
        byteCount := byteCount + 1;
        if (byteCount = actualFloppyDrive.SectorBytes) then begin
            if ((isMultiSectorCommand) and (fdcSector < actualFloppyDrive.Sectors)) then begin
                fdcSector := fdcSector + 1;
                byteCount := 0;
                fdcStatus.bit[DRQ] := True;
                extStatus.bit[DRQ] := True;
            end
            else begin
                extStatus.bit[DRQ] := False;
                extStatus.bit[INTQ] := True;
                fdcStatus.bit[DRQ] := False;
                fdcStatus.bit[BUSY] := False;
                CloseFile(fddData);
                isWriteCommand := False;
                isMultiSectorCommand := False;
                timerFddStatus.Enabled := True;
            end;
        end
        else begin
            fdcStatus.bit[DRQ] := True;
            extStatus.bit[DRQ] := True;
        end;
    end;
    fdcData := Data;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.getExtStatus: byte;
begin
    extStatus.bit[DC] := actualFloppyDrive.ImageChanged;
    Result := extStatus.Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setExtControl(control: byte);
var
    oldExtControl: TbitReg8;
begin
    if (extControl.Value = control) then begin
        exit;
    end;
    oldExtControl := extControl;
    extControl.Value := control;
    if (extControl.bit[MRES]) then begin
        doReset;
    end;
    if ((extControl.bit[D0S]) and (not oldExtControl.bit[D0S])) then begin
        if Assigned(actualFloppyDrive.FddStatus) then begin
            setFddOffState(nil);
        end;
        actualFloppyDrive := floppyDrive0;
        AssignFile(fddData, floppyDrive0.ImageFileName);
    end;
    if ((extControl.bit[D1S]) and (not oldExtControl.bit[D1S])) then begin
        if Assigned(actualFloppyDrive.FddStatus) then begin
            setFddOffState(nil);
        end;
        actualFloppyDrive := floppyDrive1;
        AssignFile(fddData, floppyDrive1.ImageFileName);
    end;
    if ((extControl.bit[SS]) and (actualFloppyDrive.Sides = 2)) then begin
        fdcSide := 1;
    end
    else begin
        fdcSide := 0;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFdd0Sides(sides: integer);
begin
    floppyDrive0.Sides := sides;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFdd0Tracks(tracks: integer);
begin
    floppyDrive0.Tracks := tracks;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFdd0Sectors(sectors: integer);
begin
    floppyDrive0.Sectors := sectors;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFdd0SectorBytes(sectorbytes: integer);
begin
    floppyDrive0.SectorBytes := sectorbytes;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.setFdd0Image(FileName: string): boolean;
var
    isLoaded: boolean;
    imageFileSize: integer;
    hintString: string;
begin
    isLoaded := False;
    hintString := '';
    if (FileExists(FileName)) then begin
        try
            AssignFile(fddData, FileName);
            Reset(fddData);
            imageFileSize := FileSize(fddData);
            if ((FileName <> floppyDrive0.ImageFileName) or (imageFileSize <> floppyDrive0.Size)) then begin
                floppyDrive0.ImageChanged := True;
                if (extControl.bit[D0S]) then begin
                    actualFloppyDrive.ImageChanged := True;
                end;
                floppyDrive0.ImageFileName := FileName;
                floppyDrive0.Size := imageFileSize;
                hintString := 'Image:  ' + ExtractFileName(FileName) + LineEnding + 'Größe:  ' + IntToStr(floppyDrive0.Size div 1024) +
                    'KB' + LineEnding + 'Seiten:  ' + IntToStr(floppyDrive0.Sides) + LineEnding + 'Spuren:  ' +
                    IntToStr(floppyDrive0.Tracks) + LineEnding + 'Sektoren:  ' + IntToStr(floppyDrive0.Sectors) + LineEnding +
                    'Bytes/Sektor:  ' + IntToStr(floppyDrive0.SectorBytes);
            end;
            isLoaded := True;
            Close(fddData);
        except
        end;
    end
    else begin
        floppyDrive0.ImageFileName := '';
        floppyDrive0.Size := 0;
    end;
    floppyDrive0.FddStatus.Hint := hintString;
    floppyDrive0.FddStatus.Enabled := isLoaded;
    Result := isLoaded;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFdd0StatusPanel(var panel: TPanel);
begin
    floppyDrive0.FddStatus := panel;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFdd1Sides(sides: integer);
begin
    floppyDrive1.Sides := sides;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFdd1Tracks(tracks: integer);
begin
    floppyDrive1.Tracks := tracks;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFdd1Sectors(sectors: integer);
begin
    floppyDrive1.Sectors := sectors;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFdd1SectorBytes(sectorbytes: integer);
begin
    floppyDrive1.SectorBytes := sectorbytes;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.setFdd1Image(FileName: string): boolean;
var
    isLoaded: boolean;
    imageFileSize: integer;
    hintString: string;
begin
    isLoaded := False;
    hintString := '';
    if (FileExists(FileName)) then begin
        try
            AssignFile(fddData, FileName);
            Reset(fddData);
            imageFileSize := FileSize(fddData);
            if ((FileName <> floppyDrive1.ImageFileName) or (imageFileSize <> floppyDrive1.Size)) then begin
                floppyDrive1.ImageChanged := True;
                if (extControl.bit[D1S]) then begin
                    actualFloppyDrive.ImageChanged := True;
                end;
                floppyDrive1.ImageFileName := FileName;
                floppyDrive1.Size := imageFileSize;
                hintString := 'Image:  ' + ExtractFileName(FileName) + LineEnding + 'Größe:  ' + IntToStr(floppyDrive1.Size div 1024) +
                    'KB' + LineEnding + 'Seiten:  ' + IntToStr(floppyDrive1.Sides) + LineEnding + 'Spuren:  ' +
                    IntToStr(floppyDrive1.Tracks) + LineEnding + 'Sektoren:  ' + IntToStr(floppyDrive1.Sectors) + LineEnding +
                    'Bytes/Sektor:  ' + IntToStr(floppyDrive1.SectorBytes);
            end;
            isLoaded := True;
            Close(fddData);
        except;
        end;
    end
    else begin
        floppyDrive1.ImageFileName := '';
        floppyDrive1.Size := 0;
    end;
    floppyDrive1.FddStatus.Hint := hintString;
    floppyDrive1.FddStatus.Enabled := isLoaded;
    Result := isLoaded;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFdd1StatusPanel(var panel: TPanel);
begin
    floppyDrive1.FddStatus := panel;
end;

// --------------------------------------------------------------------------------
end.
