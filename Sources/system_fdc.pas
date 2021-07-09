unit System_Fdc;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ExtCtrls, Graphics;

type

    { TSystemFdc }

    TSystemFdc = class

    private   // Attribute

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
        D0S = 2; // Wenn dieses Bit gesetzt ist, wird Drive-0 selektiert.
        D1S = 3; // Wenn dieses Bit gesetzt ist, wird Drive-1 selektiert.
        SS = 4; // Side selekt der Laufwerke. Gesetzt Seite 1, gelöscht Seite 0

        type
        TByteArray = array of byte;

        TBitReg8 = bitpacked record
            case byte of
                0: (Value: byte); // 8Bit Register Value
                1: (bit: bitpacked array[0..7] of boolean); // Bit Data
        end;

        TDataMode = (SECTOR_READ, SECTOR_WRITE, ADDRESS_READ, NONE);

        TFloppyDriveData = record
            Sides: byte;
            Tracks: byte;
            Sectors: byte;
            SectorBytes: integer;
            Size: dword;
            Changed: boolean;
            MotorOn: boolean;
            Loaded: boolean;
            FddStatus: TPanel;
            ImageData: file;
        end;
        ptrFloppyDriveData = ^TFloppyDriveData;

    var
        dataBuffer: TByteArray;
        dataCount: dword;
        dataMode: TDataMode;
        timerFddStatus: TTimer;
        fdcStatus: TBitReg8;
        fdcTrack, tmpTrack: byte;
        fdcSector: byte;
        fdcData: byte;
        fdcSide: byte;
        extStatus: TBitReg8;
        extControl: TBitReg8;
        stepForward: boolean;
        isMultiSectorCommand: boolean;
        canClearIntrq: boolean;
        resetFloppyDrive, floppyDrive0, floppyDrive1: TFloppyDriveData;
        actualFloppyDrive: ptrFloppyDriveData;
        filePos: DWord;
        resetState: boolean;

    protected // Attribute

    public    // Attribute

    public  // Konstruktor/Destruktor
        constructor Create(var panel0: TPanel; var panel1: TPanel);
        destructor Destroy; override;

    private   // Methoden
        procedure doReset;
        procedure calcFilePosition;
        procedure setFddOffState(Sender: TObject);
        procedure setFddReadState;
        procedure setFddWriteState;
        procedure clearBusyUpdateTrack;
        procedure clearBusySetRecordNotFoundSetIntrq;
        procedure prepareReadSectors;
        procedure finishReadSectors;
        procedure prepareWriteSectors;
        procedure finishWriteSectors;
        procedure prepareAddressRead;
        procedure finishAddressRead;

    protected // Methoden

    public    // Methoden
        procedure setCommand(command: byte);
        procedure setData(Data: byte);
        procedure setTrack(track: byte);
        procedure setSector(sector: byte);
        procedure setExtControl(control: byte);
        procedure setFdd0Sides(sides: integer);
        procedure setFdd0Tracks(tracks: integer);
        procedure setFdd0Sectors(sectors: integer);
        procedure setFdd0SectorBytes(sectorbytes: integer);
        procedure setFdd0Image(FileName: string);
        procedure setFdd1Sides(sides: integer);
        procedure setFdd1Tracks(tracks: integer);
        procedure setFdd1Sectors(sectors: integer);
        procedure setFdd1SectorBytes(sectorbytes: integer);
        procedure setFdd1Image(FileName: string);
        function getData: byte;
        function getStatus: byte;
        function getTrack: byte;
        function getSector: byte;
        function getExtStatus: byte;


    end;

var
    SystemFdc: TSystemFdc;

implementation

{ TSystemFdc }

uses System_Settings;

// --------------------------------------------------------------------------------
constructor TSystemFdc.Create(var panel0: TPanel; var panel1: TPanel);
var
    ImageFile: string;
begin
    inherited Create;
    timerFddStatus := TTimer.Create(nil);
    timerFddStatus.Enabled := False;
    timerFddStatus.Interval := 30;
    timerFddStatus.OnTimer := @setFddOffState;
    resetFloppyDrive.Size := 0;
    resetFloppyDrive.Changed := False;
    resetFloppyDrive.MotorOn := False;
    resetFloppyDrive.Loaded := False;
    floppyDrive0 := resetFloppyDrive;
    floppyDrive1 := resetFloppyDrive;
    actualFloppyDrive := @resetFloppyDrive;
    resetState := False;
    floppyDrive0.FddStatus := panel0;
    floppyDrive1.FddStatus := panel1;

    setFdd0Sides(SystemSettings.ReadInteger('Fdd0', 'Sides', 2));
    setFdd0Tracks(SystemSettings.ReadInteger('Fdd0', 'Tracks', 80));
    setFdd0Sectors(SystemSettings.ReadInteger('Fdd0', 'Sectors', 9));
    setFdd0SectorBytes(SystemSettings.ReadInteger('Fdd0', 'SectorBytes', 512));
    ImageFile := SystemSettings.ReadString('Fdd0', 'ImageFile', '');
    if ((ImageFile <> '') and (not FileExists(ImageFile))) then begin
        SystemSettings.WriteString('Fdd0', 'ImageFile', '');
        ImageFile := '';
    end;
    setFdd0Image(ImageFile);

    setFdd1Sides(SystemSettings.ReadInteger('Fdd1', 'Sides', 2));
    setFdd1Tracks(SystemSettings.ReadInteger('Fdd1', 'Tracks', 80));
    setFdd1Sectors(SystemSettings.ReadInteger('Fdd1', 'Sectors', 9));
    setFdd1SectorBytes(SystemSettings.ReadInteger('Fdd1', 'SectorBytes', 512));
    ImageFile := SystemSettings.ReadString('Fdd1', 'ImageFile', '');
    if ((ImageFile <> '') and (not FileExists(ImageFile))) then begin
        SystemSettings.WriteString('Fdd1', 'ImageFile', '');
        ImageFile := '';
    end;
    setFdd1Image(ImageFile);

end;

// --------------------------------------------------------------------------------
destructor TSystemFdc.Destroy;
begin
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
    fdcStatus.Value := $20;
    tmpTrack := 0;
    fdcTrack := tmpTrack;
    fdcSector := 1;
    fdcData := 0;
    extStatus.Value := $00;
    stepForward := True;
    isMultiSectorCommand := False;
    canClearIntrq := True;
    dataMode := NONE;
    resetState := True;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.calcFilePosition;
begin
    filePos := ((((fdcTrack * actualFloppyDrive^.Sides) + fdcSide) * actualFloppyDrive^.Sectors) + (fdcSector - 1));
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setFddOffState(Sender: TObject);
begin
    timerFddStatus.Enabled := False;
    with actualFloppyDrive^.FddStatus do begin
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
    with actualFloppyDrive^.FddStatus do begin
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
    with actualFloppyDrive^.FddStatus do begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := clRed;
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Color := clMaroon;
        Canvas.Pen.Width := 1;
        Canvas.Ellipse(21, 1, 29, 9);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.clearBusyUpdateTrack;
begin
    if (resetState or (not actualFloppyDrive^.Loaded)) then begin
        exit;
    end;
    fdcTrack := tmpTrack;
    fdcStatus.bit[BUSY] := False;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.clearBusySetRecordNotFoundSetIntrq;
begin
    if (resetState or (not actualFloppyDrive^.Loaded)) then begin
        exit;
    end;
    extStatus.bit[INTQ] := True;
    fdcStatus.bit[RNF] := True;
    fdcStatus.bit[BUSY] := False;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.prepareReadSectors;
begin
    calcFilePosition;
    if ((fdcSector > actualFloppyDrive^.Sectors) or ((filePos > (actualFloppyDrive^.Size - actualFloppyDrive^.SectorBytes)) or
        (actualFloppyDrive^.Size = 0) or (fdcTrack >= actualFloppyDrive^.Tracks))) then begin
        // Sector nicht vorhanden oder Sector und/oder Track ausserhalb der Disk
        clearBusySetRecordNotFoundSetIntrq;
        exit;
    end
    else begin
        try
            Reset(actualFloppyDrive^.ImageData, actualFloppyDrive^.SectorBytes);
            Seek(actualFloppyDrive^.ImageData, filePos);
            BlockRead(actualFloppyDrive^.ImageData, dataBuffer[0], 1);
            CloseFile(actualFloppyDrive^.ImageData);
            fdcStatus.bit[DRQ] := True;
            extStatus.bit[DRQ] := True;
            dataCount := 0;
            dataMode := SECTOR_READ;
            setFddReadState;
            timerFddStatus.Enabled := True;
        except
            extStatus.bit[INTQ] := True;
            fdcStatus.bit[BUSY] := False;
            fdcStatus.bit[CRCERROR] := True;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.finishReadSectors;
begin
    fdcStatus.bit[DRQ] := False;
    extStatus.bit[DRQ] := False;
    actualFloppyDrive^.MotorOn := False;
    if ((isMultiSectorCommand) and (fdcSector < actualFloppyDrive^.Sectors)) then begin
        Inc(fdcSector);
        prepareReadSectors;
    end
    else begin
        extStatus.bit[INTQ] := True;
        fdcStatus.bit[BUSY] := False;
        dataMode := NONE;
        isMultiSectorCommand := False;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.prepareWriteSectors;
begin
    calcFilePosition;
    if ((fdcSector > actualFloppyDrive^.Sectors) or ((filePos > (actualFloppyDrive^.Size - actualFloppyDrive^.SectorBytes)) or
        (actualFloppyDrive^.Size = 0) or (fdcTrack >= actualFloppyDrive^.Tracks))) then begin
        // Sector nicht vorhanden oder Sector und/oder Track ausserhalb der Disk
        clearBusySetRecordNotFoundSetIntrq;
        exit;
    end
    else begin
        dataCount := 0;
        fdcStatus.bit[DRQ] := True;
        extStatus.bit[DRQ] := True;
        dataMode := SECTOR_WRITE;
        setFddWriteState;
        timerFddStatus.Enabled := True;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.finishWriteSectors;
begin
    fdcStatus.bit[DRQ] := False;
    extStatus.bit[DRQ] := False;
    actualFloppyDrive^.MotorOn := False;
    try
        Reset(actualFloppyDrive^.ImageData, actualFloppyDrive^.SectorBytes);
        Seek(actualFloppyDrive^.ImageData, filePos);
        BlockWrite(actualFloppyDrive^.ImageData, dataBuffer[0], 1);
        CloseFile(actualFloppyDrive^.ImageData);
        if ((isMultiSectorCommand) and (fdcSector < actualFloppyDrive^.Sectors)) then begin
            Inc(fdcSector);
            prepareWriteSectors;
        end
        else begin
            extStatus.bit[INTQ] := True;
            fdcStatus.bit[BUSY] := False;
            dataMode := NONE;
            isMultiSectorCommand := False;
        end;
    except
        extStatus.bit[INTQ] := True;
        fdcStatus.bit[BUSY] := False;
        fdcStatus.bit[CRCERROR] := True;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.prepareAddressRead;
begin
    dataCount := 0;
    dataBuffer[0] := actualFloppyDrive^.Tracks;
    dataBuffer[1] := actualFloppyDrive^.Sides;
    dataBuffer[2] := actualFloppyDrive^.Sectors;
    case (actualFloppyDrive^.SectorBytes) of
        128: dataBuffer[3] := 0;
        256: dataBuffer[3] := 1;
        512: dataBuffer[3] := 2;
        1024: dataBuffer[3] := 3;
        else dataBuffer[3] := $ff;
    end;
    if (actualFloppyDrive^.Loaded) then begin
        dataBuffer[4] := $ff;
    end
    else begin
        dataBuffer[4] := 00;
    end;
    dataBuffer[5] := $ff;
    fdcStatus.bit[DRQ] := True;
    extStatus.bit[DRQ] := True;
    dataMode := ADDRESS_READ;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.finishAddressRead;
begin
    fdcStatus.bit[DRQ] := False;
    extStatus.bit[DRQ] := False;
    extStatus.bit[INTQ] := True;
    fdcStatus.bit[BUSY] := False;
    dataMode := NONE;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setCommand(command: byte);
var
    cmd: TbitReg8;
begin
    cmd.Value := command;
    fdcStatus.bit[BUSY] := True;
    resetState := False;
    case (cmd.Value and %11100000) of
        %00000000: begin    // Restore / Seek
            fdcStatus.bit[CRCERROR] := False;
            fdcStatus.bit[RNF] := False;
            fdcStatus.bit[DRQ] := False;
            fdcStatus.bit[RTSU] := True;
            extStatus.bit[DRQ] := False;
            fdcStatus.bit[LDB] := False;
            if (canClearIntrq) then begin
                extStatus.bit[INTQ] := False;
            end;
            if (not cmd.bit[4]) then begin  // Restore auf Track 0
                if (fdcTrack > 0) then begin  //  wenn aktueller Track groesser 0
                    stepForward := False;   // Step-Direction auf backward setzen
                    actualFloppyDrive^.Changed := False;
                end;
                tmpTrack := 0;
                fdcStatus.bit[LDB] := True;
            end
            else begin  // Seek auf angeforderten Track
                if (fdcTrack > fdcData) then begin  // wenn aktueller Track groesser angeforderter Track
                    stepForward := False;   // Step-Direction auf backward setzen
                end
                else begin
                    stepForward := True;    // ansonsten Step-Direction forward
                end;
                if (fdcTrack <> fdcData) then begin
                    actualFloppyDrive^.Changed := False;
                end;
                if (fdcData > (actualFloppyDrive^.Tracks - 1)) then begin // falls der angeforderte Track nicht erreichbar ist
                    fdcStatus.bit[RNF] := True;   // 'Seek Error / Record not Found' Flag setzen
                end;
                tmpTrack := fdcData;
            end;
            actualFloppyDrive^.MotorOn := True;
            clearBusyUpdateTrack;
        end;    // Restore / Seek
        %00100000: begin    // Step
            fdcStatus.bit[CRCERROR] := False;
            fdcStatus.bit[RNF] := False;
            fdcStatus.bit[DRQ] := False;
            fdcStatus.bit[RTSU] := True;
            extStatus.bit[DRQ] := False;
            if (canClearIntrq) then begin
                extStatus.bit[INTQ] := False;
            end;
            tmpTrack := fdcTrack;
            if (stepForward) then begin
                Inc(tmpTrack);
                actualFloppyDrive^.Changed := False;
                if (tmpTrack > (actualFloppyDrive^.Tracks - 1)) then begin // falls der angeforderte Track nicht erreichbar ist
                    fdcStatus.bit[RNF] := True;   // 'Seek Error / Record not Found' Flag setzen
                end;
            end
            else begin
                if (tmpTrack > 0) then begin
                    Dec(tmpTrack);
                    actualFloppyDrive^.Changed := False;
                end;
            end;
            if (not cmd.bit[4]) then begin    // Track-Register Update
                tmpTrack := fdcTrack;
            end;
            actualFloppyDrive^.MotorOn := True;
            clearBusyUpdateTrack;
        end;    // Step
        %01000000: begin    // Step-in
            fdcStatus.bit[CRCERROR] := False;
            fdcStatus.bit[RNF] := False;
            fdcStatus.bit[DRQ] := False;
            fdcStatus.bit[RTSU] := True;
            extStatus.bit[DRQ] := False;
            if (canClearIntrq) then begin
                extStatus.bit[INTQ] := False;
            end;
            tmpTrack := fdcTrack;
            stepForward := True;
            Inc(tmpTrack);
            actualFloppyDrive^.Changed := False;
            if (tmpTrack > (actualFloppyDrive^.Tracks - 1)) then begin // falls der angeforderte Track nicht erreichbar ist
                fdcStatus.bit[RNF] := True;   // 'Seek Error / Record not Found' Flag setzen
            end;
            if (not cmd.bit[4]) then begin
                tmpTrack := fdcTrack;
            end;
            actualFloppyDrive^.MotorOn := True;
            clearBusyUpdateTrack;
        end;    // Step-in
        %01100000: begin    // Step-out
            fdcStatus.bit[CRCERROR] := False;
            fdcStatus.bit[RNF] := False;
            fdcStatus.bit[DRQ] := False;
            fdcStatus.bit[RTSU] := True;
            extStatus.bit[DRQ] := False;
            if (canClearIntrq) then begin
                extStatus.bit[INTQ] := False;
            end;
            tmpTrack := fdcTrack;
            stepForward := False;
            if (tmpTrack > 0) then begin
                Dec(tmpTrack);
                actualFloppyDrive^.Changed := False;
            end;
            if (not cmd.bit[4]) then begin
                tmpTrack := fdcTrack;
            end;
            actualFloppyDrive^.MotorOn := True;
            clearBusyUpdateTrack;
        end;    // Step-out
        %10000000: begin    // Read Sector
            fdcStatus.bit[DRQ] := False;
            fdcStatus.bit[LDB] := False;
            fdcStatus.bit[RNF] := False;
            fdcStatus.bit[RTSU] := False;
            fdcStatus.bit[WP] := False;
            extStatus.bit[DRQ] := False;
            isMultiSectorCommand := False;
            if ((not cmd.bit[0]) and (not cmd.bit[1])) then begin    // Bit 0 und 1 muessen gelöscht sein
                if (canClearIntrq) then begin
                    extStatus.bit[INTQ] := False;
                end;
                actualFloppyDrive^.MotorOn := True;
                fdcStatus.bit[RTSU] := True;
                if (cmd.bit[4]) then begin  // Bit 4 gesetzt, dann Multi-Sector Read
                    isMultiSectorCommand := True;
                end;
                prepareReadSectors;
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
            actualFloppyDrive^.MotorOn := True;
            fdcStatus.bit[RTSU] := True;
            if (cmd.bit[4]) then begin  // Bit 4 gesetzt, dann Multi-Sector Write
                isMultiSectorCommand := True;
            end;
            prepareWriteSectors;
        end;    // Write Sector
        %11000000: begin    // Read Address / Force Interrupt
            if (cmd.bit[4]) then begin   // Bit 4 gesetzt - Force Interrupt
                if ((cmd.Value and $0f) = $00) then begin   // Beendet alle Befehle ohne INTRQ
                    dataMode := NONE;
                    isMultiSectorCommand := False;
                    fdcStatus.bit[DRI] := False;
                    extStatus.bit[DRQ] := False;
                    fdcStatus.bit[BUSY] := False;
                    fdcStatus.bit[RTSU] := False;
                    actualFloppyDrive^.MotorOn := True;
                    canClearIntrq := True;
                end;
                if ((cmd.Value and $0f) = $08) then begin   // Beendet alle Befehle mit INTRQ
                    dataMode := NONE;
                    isMultiSectorCommand := False;
                    fdcStatus.bit[DRI] := False;
                    extStatus.bit[DRQ] := False;
                    fdcStatus.bit[BUSY] := False;
                    fdcStatus.bit[RTSU] := False;
                    extStatus.bit[INTQ] := True;
                    actualFloppyDrive^.MotorOn := True;
                    canClearIntrq := False;
                end;
            end
            else begin  // Bit 4 gelöscht - Read Address
                fdcStatus.bit[DRQ] := False;
                fdcStatus.bit[LDB] := False;
                fdcStatus.bit[RNF] := False;
                fdcStatus.bit[RTSU] := True;
                fdcStatus.bit[WP] := False;
                extStatus.bit[DRQ] := False;
                actualFloppyDrive^.MotorOn := True;
                prepareAddressRead;
            end;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setData(Data: byte);
begin
    case (dataMode) of
        SECTOR_WRITE: begin
            dataBuffer[dataCount] := Data;
            Inc(dataCount);
            if (dataCount >= actualFloppyDrive^.SectorBytes) then begin
                finishWriteSectors;
            end;
        end
        else begin
            fdcData := Data;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setTrack(track: byte);
begin
    fdcTrack := track;
end;

// --------------------------------------------------------------------------------
procedure TSystemFdc.setSector(sector: byte);
begin
    fdcSector := sector;
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
        if Assigned(actualFloppyDrive^.FddStatus) then begin
            setFddOffState(nil);
        end;
        actualFloppyDrive := @floppyDrive0;
        SetLength(dataBuffer, actualFloppyDrive^.SectorBytes);
    end;
    if ((extControl.bit[D1S]) and (not oldExtControl.bit[D1S])) then begin
        if Assigned(actualFloppyDrive^.FddStatus) then begin
            setFddOffState(nil);
        end;
        actualFloppyDrive := @floppyDrive1;
        SetLength(dataBuffer, actualFloppyDrive^.SectorBytes);
    end;
    if ((not resetState) and (not extControl.bit[D0S]) and (not extControl.bit[D1S])) then begin
        actualFloppyDrive := @resetFloppyDrive;
    end;
    if (extControl.bit[D0S] or extControl.bit[D1S]) then begin
        if ((not resetState) and actualFloppyDrive^.Loaded) then begin
            fdcStatus.bit[LDB] := True;
        end;
        actualFloppyDrive^.MotorOn := True;
    end
    else begin
        fdcStatus.bit[LDB] := False;
    end;
    if ((extControl.bit[SS]) and (actualFloppyDrive^.Sides = 2)) then begin
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
procedure TSystemFdc.setFdd0Image(FileName: string);
var
    hintString: string;
begin
    hintString := '';
    floppyDrive0.Loaded := False;
    floppyDrive0.Changed := True;
    floppyDrive0.MotorOn := False;
    if (FileExists(FileName)) then begin
        try
            AssignFile(floppyDrive0.ImageData, FileName);
            Reset(floppyDrive0.ImageData, 1);
            floppyDrive0.Size := FileSize(floppyDrive0.ImageData);
            hintString := 'Image:  ' + ExtractFileName(FileName) + LineEnding + 'Größe:  ' + IntToStr(floppyDrive0.Size div 1024) +
                'KB' + LineEnding + 'Seiten:  ' + IntToStr(floppyDrive0.Sides) + LineEnding + 'Spuren:  ' +
                IntToStr(floppyDrive0.Tracks) + LineEnding + 'Sektoren:  ' + IntToStr(floppyDrive0.Sectors) + LineEnding +
                'Bytes/Sektor:  ' + IntToStr(floppyDrive0.SectorBytes);
            floppyDrive0.Loaded := True;
            Close(floppyDrive0.ImageData);
        except
        end;
    end
    else begin
        floppyDrive0.Size := 0;
    end;
    floppyDrive0.FddStatus.Hint := hintString;
    floppyDrive0.FddStatus.Enabled := floppyDrive0.Loaded;
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
procedure TSystemFdc.setFdd1Image(FileName: string);
var
    hintString: string;
begin
    hintString := '';
    floppyDrive1.Loaded := False;
    floppyDrive1.Changed := True;
    floppyDrive1.MotorOn := False;
    if (FileExists(FileName)) then begin
        try
            AssignFile(floppyDrive1.ImageData, FileName);
            Reset(floppyDrive1.ImageData, 1);
            floppyDrive1.Size := FileSize(floppyDrive1.ImageData);
            hintString := 'Image:  ' + ExtractFileName(FileName) + LineEnding + 'Größe:  ' + IntToStr(floppyDrive1.Size div 1024) +
                'KB' + LineEnding + 'Seiten:  ' + IntToStr(floppyDrive1.Sides) + LineEnding + 'Spuren:  ' +
                IntToStr(floppyDrive1.Tracks) + LineEnding + 'Sektoren:  ' + IntToStr(floppyDrive1.Sectors) + LineEnding +
                'Bytes/Sektor:  ' + IntToStr(floppyDrive1.SectorBytes);
            floppyDrive1.Loaded := True;
            Close(floppyDrive1.ImageData);
        except;
        end;
    end
    else begin
        floppyDrive1.Size := 0;
    end;
    floppyDrive1.FddStatus.Hint := hintString;
    floppyDrive1.FddStatus.Enabled := floppyDrive1.Loaded;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.getData: byte;
begin
    case (dataMode) of
        SECTOR_READ: begin
            Result := dataBuffer[dataCount];
            Inc(dataCount);
            if (dataCount >= actualFloppyDrive^.SectorBytes) then begin
                finishReadSectors;
            end;
        end;
        ADDRESS_READ: begin
            Result := dataBuffer[dataCount];
            Inc(dataCount);
            if (dataCount >= 6) then begin
                finishAddressRead;
            end;
        end
        else begin
            Result := fdcData;
        end;
    end;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.getStatus: byte;
begin
    // Verhalten nicht eindeutig geklaert. Auf der Original Hardware scheint das Flag verzoegert geloescht zu werden.
    //if (canClearIntrq) then begin
    //extStatus.bit[INTQ] := False; // beim Lesen des Status-Registers wird das Interrupt-Flag geloescht
    //end;
    //if (resetState and (not actualFloppyDrive^.Loaded)) then begin
    if (resetState) then begin
        actualFloppyDrive^.MotorOn := False;
    end;
    fdcStatus.bit[MON] := actualFloppyDrive^.MotorOn;
    Result := fdcStatus.Value;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.getTrack: byte;
begin
    Result := fdcTrack;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.getSector: byte;
begin
    Result := fdcSector;
end;

// --------------------------------------------------------------------------------
function TSystemFdc.getExtStatus: byte;
begin
    extStatus.bit[DC] := actualFloppyDrive^.Changed;
    Result := extStatus.Value;
end;

end.
