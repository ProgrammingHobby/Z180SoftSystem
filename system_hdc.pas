unit System_Hdc;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ExtCtrls, Graphics;

type

    { TSystemHdc }

    TSystemHdc = class

    private   // Attribute
        type
        TByteArray = array of byte;
        TBitReg8 = bitpacked record
            case byte of
                0: (Value: byte); // 8Bit Register Value
                2: (bit: bitpacked array[0..7] of boolean); // Bit Data
        end;

        Treg16 = packed record
            case byte of
                0: (Value: word); // 16Bit Register
                1: (low: byte;    // lower 8Bit
                    high: byte);  // upper 8Bit
        end;

        Treg32 = packed record
            case byte of
                0: (Value: dword); // 32Bit Register
                1: (low: Treg16;   // lower 16Bit
                    high: Treg16); // upper 16Bit
        end;

        TDataMode = (SECTOR_READ, SECTOR_WRITE, BUFFER_READ, BUFFER_WRITE, ID_READ);

        THardDriveData = record
            Heads: byte;
            Tracks: word;
            Sectors: byte;
            ImageFileName: string;
            Size: dword;
            ImageChanged: boolean;
            HddStatus: TPanel;
        end;

    var
        dataBuffer: TByteArray;
        dataCount: dword;
        dataMode: TDataMode;
        timerHddStatus: TTimer;
        hardDrive: THardDriveData;
        hddData: file;
        hdcDataHigh: byte;
        hdcTrack: Treg16;
        hdcSector: byte;
        hdcSectorCount: word;
        hdcDriveHead: TBitReg8;
        hdcFeatures: byte;
        hdcStatus: TBitReg8;
        hdcError: TBitReg8;
        hdcLba: Treg32;
        enable8BitDataTransfer: boolean;

    const
        // Konstanten der Bit-Adressen im HDC-Error Register
        AMNF = 0;  // indicates the data address mark has not been found after finding the correct ID field.
        TK0NF = 1; // indicates track 0 has not been found during the command.
        ABRT = 2;  // indicates the requested command has been aborted due to a
        //            drive status error (such as not ready or write fault) or because
        //            the command is invalid.
        MCR = 3;   // these bit is reserved for removable-media drives.
        IDNF = 4;  // indicates the requested sector’s ID field could not be found.
        MC = 5;    // these bit is reserved for removable-media drives.
        UNC = 6;   // indicates an uncorrectable data error has been encountered.
        BBK = 7;   // indicates a bad block mark was detected in the ID field of the requested sector.


        // Konstanten der Bit-Adressen im HDC-Status Register
        ERR = 0;   // is the error bit. It is set to 1 when the previous command ended
        //            in some type of error. The other bits in the Status register, and
        //            the bits in the Error register, have more information as to the
        //            cause of the error. This bit is set to 0 when a new command is
        //            serviced.
        IDX = 1;   // is the index bit. This bit usually contains a 0, except once per
        //            disc revolution when it is toggled from 0 to 1 and back to 0.
        CORR = 2;  // is the corrected data bit. It is set to 1 when a correctable data
        //            error has been encountered and the data has been corrected.
        //            This condition does not end a multisector read operation. This
        //            bit is set to 0 when a new command is serviced.
        DRQ = 3;   // is the data request bit. It is set to 1 when the drive is ready to
        //            transfer a word or byte of data between the host and the data
        //            port. The drive is busy whenever DRQ or BSY bits are set to 1.
        //            When the DRQ bit is set to 1, the host may read or write any of
        //            the registers including the Command register.
        DSC = 4;   // is the drive seek complete bit. It is set to 1 when the disc drive is not seeking.
        DWF = 5;   // is the drive write fault bit. When there is a write fault error, this
        //            bit is set to 1 and is not changed until the Status register is read
        //            by the host, at which time the bit again indicates the current write
        //            fault status.
        DRDY = 6;  // is the drive ready indicator bit. This bit is set to 0 at power up and
        //            remains set at 0 until the drive is ready to accept a command.
        BSY = 7;   // is the busy bit. It is set to 1 whenever the drive has access to the
        //            command block. When it is set to 1:
        //                •No other bits are valid.
        //                •The host is locked out of reading shared registers; the Status register is read instead.
        //            The BSY bit is set to 1 under the following circumstances:
        //                • At the assertion of the RESET– signal on the interface
        //                • At the assertion of the SRST bit in the Device Control register
        //                • Immediately upon host write to the Command register

        // Konstanten der Bit-Adressen im HDC-Drive/Head Register
        DRV = 4;   // is the bit used to select the drive. Master is 0. Slave is 1.
        LBA = 6;   // signifies the addressing scheme currently being used. If this bit
        //            is set to 1, the drive is using the logical block addressing scheme.
        //            If the bit is set to 0, the CHS scheme is used.
        HEAD = 15; // is the 4-bit address used to select the head. When the drive is
        //            executing the Initialize Drive Parameters command, HEAD
        //            specifies the maximum head address.

        // Konstante für die Anzahl an Bytes / Sektor
        SECBYTES = 512;

    protected // Attribute

    public    // Attribute

    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    private   // Methode
        procedure calcLbaValue;
        procedure calcChsValues;
        function calcHddSize: string;
        procedure setFeatures;
        procedure prepareReadSectors;
        procedure prepareReadBuffer;
        procedure prepareIdentifyDrive;
        procedure finishReadData;
        procedure prepareWriteSectors;
        procedure prepareWriteBuffer;
        procedure finishWriteData;
        procedure setHddOffState(Sender: TObject);
        procedure setHddReadState;
        procedure setHddWriteState;

    protected // Methoden

    public    // Methoden
        procedure doReset;
        procedure setHddHeads(heads: byte);
        procedure setHddTracks(tracks: word);
        procedure setHddSectors(sectors: byte);
        function setHddImage(fileName: string): boolean;
        procedure setHddStatusPanel(var panel: TPanel);
        procedure setDataLow(Value: byte);
        procedure setDataHigh(Value: byte);
        procedure setTrackLow(Value: byte);
        procedure setTrackHigh(Value: byte);
        procedure setSector(Value: byte);
        procedure setSectorCount(Value: byte);
        procedure setDriveHead(Value: byte);
        procedure setCommand(Value: byte);
        procedure setFeatures(Value: byte);
        function getDataLow: byte;
        function getDataHigh: byte;
        function getTrackLow: byte;
        function getTrackHigh: byte;
        function getSector: byte;
        function getSectorCount: byte;
        function getDriveHead: byte;
        function getStatus: byte;
        function getError: byte;

    end;

var
    SystemHdc: TSystemHdc;

implementation

{ TSystemHdc }

// --------------------------------------------------------------------------------
constructor TSystemHdc.Create;
begin
    inherited Create;
    timerHddStatus := TTimer.Create(nil);
    timerHddStatus.Enabled := False;
    timerHddStatus.Interval := 50;
    timerHddStatus.OnTimer := @setHddOffState;
    SetLength(dataBuffer, SECBYTES);
    doReset;
end;

// --------------------------------------------------------------------------------
destructor TSystemHdc.Destroy;
begin
    if Assigned(timerHddStatus) then begin
        timerHddStatus.Enabled := False;
        timerHddStatus.OnTimer := nil;
        timerHddStatus.Destroy;
    end;
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.calcLbaValue;
begin
    if (hdcDriveHead.bit[LBA]) then begin
        hdcLba.low.low := hdcSector; // 0..7
        hdcLba.low.high := hdcTrack.low; // 8..15
        hdcLba.high.low := hdcTrack.high; // 16..23
        hdcLba.high.high := (hdcDriveHead.Value and HEAD); // 24..27
    end
    else begin
        hdcLba.Value := ((((hdcTrack.Value * hardDrive.Heads) + (hdcDriveHead.Value and HEAD)) * hardDrive.Sectors) + (hdcSector - 1));
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.calcChsValues;
var
    tmpValue: dword;
begin
    if (hdcDriveHead.bit[LBA]) then begin
        hdcSector := hdcLba.low.low; // 0..7
        hdcTrack.low := hdcLba.low.high; // 8..15
        hdcTrack.high := hdcLba.high.low; // 16..23
        hdcDriveHead.Value := ((hdcDriveHead.Value and not $0F) or (hdcLba.high.high and $0F)); // 24..27
    end
    else begin
        hdcTrack.Value := (hdcLba.Value div (hardDrive.Heads * hardDrive.Sectors));
        tmpValue := (hdcLba.Value mod (hardDrive.Heads * hardDrive.Sectors));
        hdcDriveHead.Value := ((hdcDriveHead.Value and not $0F) or ((tmpValue div hardDrive.Sectors) and $0F));
        hdcSector := (((tmpValue mod hardDrive.Sectors) + 1) and $FF);
    end;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.calcHddSize: string;
var
    sizeString: string;
begin
    if ((hardDrive.Size div 1048576) > 0) then begin
        sizeString := FloatToStrF((hardDrive.Size / 1048576), ffNumber, 15, 2) + 'MB';
    end
    else if ((hardDrive.Size div 1024) > 0) then begin
        sizeString := FloatToStrF((hardDrive.Size / 1024), ffNumber, 15, 2) + 'KB';
    end
    else begin
        sizeString := IntToStr(hardDrive.Size) + 'Byte';
    end;
    Result := sizeString;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setFeatures;
var
    abort: boolean;
begin
    hdcStatus.bit[BSY] := True;
    hdcStatus.bit[ERR] := False;
    abort := False;
    if (not hdcDriveHead.bit[DRV]) then begin
        case (hdcFeatures) of
            $01: enable8BitDataTransfer := True;
            $81: enable8BitDataTransfer := False;
            else abort := True;
        end;
    end
    else begin
        abort := True;
    end;
    hdcError.bit[ABRT] := abort;
    hdcStatus.bit[ERR] := abort;
    hdcStatus.bit[BSY] := False;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.prepareReadSectors;
begin
    hdcStatus.bit[BSY] := True;
    hdcStatus.bit[DSC] := False;
    calcLbaValue;
    if (hdcSectorCount = 0) then begin
        hdcSectorCount := 256;
    end;
    if (((hdcLba.Value * SECBYTES) > (hardDrive.Size - SECBYTES)) or (hardDrive.Size = 0) or (hdcSector > hardDrive.Sectors) or
        (hdcTrack.Value >= hardDrive.Tracks) or (hdcDriveHead.bit[DRV])) then begin
        hdcStatus.bit[ERR] := True;
        hdcError.bit[IDNF] := True;
        hdcStatus.bit[BSY] := False;
        exit;
    end;
    try
        Reset(hddData, SECBYTES);
        Seek(hddData, hdcLba.Value);
        hdcStatus.bit[DSC] := True;
        BlockRead(hddData, dataBuffer[0], 1);
        CloseFile(hddData);
    except
        hdcStatus.bit[ERR] := True;
        hdcError.bit[AMNF] := True;
        hdcStatus.bit[BSY] := False;
        exit;
    end;
    hdcStatus.bit[BSY] := False;
    hdcStatus.bit[DRQ] := True;
    dataCount := 0;
    dataMode := SECTOR_READ;
    setHddReadState;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.prepareReadBuffer;
begin
    hdcStatus.bit[BSY] := False;
    hdcStatus.bit[DRQ] := True;
    dataCount := 0;
    dataMode := BUFFER_READ;
    setHddReadState;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.prepareIdentifyDrive;
var
    index: word;
    idword: Treg16;
    iddoubleword: Treg32;
begin
    hdcStatus.bit[BSY] := False;
    hdcStatus.bit[DRQ] := True;
    dataCount := 0;
    for index := 0 to SECBYTES - 1 do begin
        dataBuffer[index] := 0;
    end;
    dataBuffer[0] := %01000000;  // fixed drive
    dataBuffer[1] := %00000100;  // disk transfer rate > 10 Mbs
    idword.Value := hardDrive.Tracks; // Number of cylinders
    dataBuffer[2] := idword.low;
    dataBuffer[3] := idword.high;
    idword.Value := hardDrive.Heads; // Number of heads
    dataBuffer[6] := idword.low;
    dataBuffer[7] := idword.high;
    idword.Value := (SECBYTES * hardDrive.Sectors); // Number of unformatted bytes per track
    dataBuffer[8] := idword.low;
    dataBuffer[9] := idword.high;
    idword.Value := SECBYTES; // Number of unformatted bytes per sector
    dataBuffer[10] := idword.low;
    dataBuffer[11] := idword.high;
    idword.Value := hardDrive.Sectors; // Number of sectors per track
    dataBuffer[12] := idword.low;
    dataBuffer[13] := idword.high;
    idword.Value := 1; //  Buffer size in 512 byte increments
    dataBuffer[42] := idword.low;
    dataBuffer[43] := idword.high;
    dataBuffer[49]:=%00000000; // Vendor unique
    dataBuffer[50]:=%00000010; // LBA supported
    idword.Value := hardDrive.Tracks; // Number of current cylinders
    dataBuffer[108] := idword.low;
    dataBuffer[109] := idword.high;
    idword.Value := hardDrive.Heads; // Number of current heads
    dataBuffer[110] := idword.low;
    dataBuffer[111] := idword.high;
    idword.Value := hardDrive.Sectors; // Number of current sectors per track
    dataBuffer[112] := idword.low;
    dataBuffer[113] := idword.high;
    iddoubleword.Value := (hardDrive.Tracks * hardDrive.Sectors * hardDrive.Heads); // Current capacity in sectors
    dataBuffer[114] := iddoubleword.low.low;
    dataBuffer[115] := iddoubleword.low.high;
    dataBuffer[116] := iddoubleword.high.low;
    dataBuffer[117] := iddoubleword.high.high;
    iddoubleword.Value := (hardDrive.Tracks * hardDrive.Sectors * hardDrive.Heads); //  Total number of user addressable sectors (LBA mode only)
    dataBuffer[120] := iddoubleword.low.low;
    dataBuffer[121] := iddoubleword.low.high;
    dataBuffer[122] := iddoubleword.high.low;
    dataBuffer[123] := iddoubleword.high.high;
    dataMode := ID_READ;
    setHddReadState;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.finishReadData;
begin
    hdcStatus.bit[DRQ] := False;
    case (dataMode) of
        SECTOR_READ: begin
            Dec(hdcSectorCount);
            if (hdcSectorCount > 0) then begin
                Inc(hdcLba.Value);
                calcChsValues;
                prepareReadSectors;
            end;
        end;
        BUFFER_READ: begin
            hdcStatus.bit[DRQ] := False;
        end;
        ID_READ: begin
            hdcStatus.bit[DRQ] := False;
        end;
    end;
    timerHddStatus.Enabled := True;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.prepareWriteSectors;
begin
    hdcStatus.bit[BSY] := True;
    hdcStatus.bit[DSC] := False;
    calcLbaValue;
    if (hdcSectorCount = 0) then begin
        hdcSectorCount := 256;
    end;
    if (((hdcLba.Value * SECBYTES) > (hardDrive.Size - SECBYTES)) or (hardDrive.Size = 0) or (hdcSector > hardDrive.Sectors) or
        (hdcTrack.Value >= hardDrive.Tracks) or (hdcDriveHead.bit[DRV])) then begin
        hdcStatus.bit[ERR] := True;
        hdcError.bit[IDNF] := True;
        hdcStatus.bit[BSY] := False;
        exit;
    end;
    hdcStatus.bit[DSC] := True;
    hdcStatus.bit[BSY] := False;
    hdcStatus.bit[DRQ] := True;
    dataCount := 0;
    dataMode := SECTOR_WRITE;
    setHddWriteState;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.prepareWriteBuffer;
begin
    hdcStatus.bit[BSY] := False;
    hdcStatus.bit[DRQ] := True;
    dataCount := 0;
    dataMode := BUFFER_WRITE;
    setHddWriteState;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.finishWriteData;
begin
    case (dataMode) of
        SECTOR_WRITE: begin
            hdcStatus.bit[DRQ] := False;
            try
                Reset(hddData, SECBYTES);
                Seek(hddData, hdcLba.Value);
                BlockWrite(hddData, dataBuffer[0], 1);
                CloseFile(hddData);
            except
                hdcStatus.bit[ERR] := True;
                hdcError.bit[AMNF] := True;
                hdcStatus.bit[BSY] := False;
                exit;
            end;
            Dec(hdcSectorCount);
            if (hdcSectorCount > 0) then begin
                Inc(hdcLba.Value);
                calcChsValues;
                prepareWriteSectors;
            end;
        end;
        BUFFER_WRITE: begin
            hdcStatus.bit[DRQ] := False;
        end;
    end;
    timerHddStatus.Enabled := True;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddOffState(Sender: TObject);
begin
    timerHddStatus.Enabled := False;
    with hardDrive.HddStatus do begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := clDefault;
        Canvas.Pen.Style := psClear;
        Canvas.Pen.Color := clDefault;
        Canvas.Pen.Width := 2;
        Canvas.Ellipse(19, 0, 30, 10);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddReadState;
begin
    timerHddStatus.Enabled := False;
    with hardDrive.HddStatus do begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := clLime;
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Color := clGreen;
        Canvas.Pen.Width := 1;
        Canvas.Ellipse(20, 1, 28, 9);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddWriteState;
begin
    timerHddStatus.Enabled := False;
    with hardDrive.HddStatus do begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := clRed;
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Color := clMaroon;
        Canvas.Pen.Width := 1;
        Canvas.Ellipse(20, 1, 28, 9);
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.doReset;
begin
    hdcError.Value := $01;
    hdcSector := $01;
    hdcSectorCount := $01;
    hdcTrack.low := $00;
    hdcTrack.high := $00;
    hdcDriveHead.Value := $00;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddHeads(heads: byte);
begin
    hardDrive.Heads := heads;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddTracks(tracks: word);
begin
    hardDrive.Tracks := tracks;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddSectors(sectors: byte);
begin
    hardDrive.Sectors := sectors;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.setHddImage(fileName: string): boolean;
var
    isLoaded: boolean;
    imageFileSize: dword;
    hintString: string;
begin
    isLoaded := False;
    hintString := '';
    if (FileExists(FileName)) then begin
        try
            AssignFile(hddData, FileName);
            Reset(hddData, 1);
            imageFileSize := FileSize(hddData);
            if ((FileName <> hardDrive.ImageFileName) or (imageFileSize <> hardDrive.Size)) then begin
                hardDrive.ImageChanged := True;
                hardDrive.ImageFileName := FileName;
                hardDrive.Size := imageFileSize;
                hintString := 'Image:  ' + ExtractFileName(fileName) + LineEnding + 'Größe:  ' + calcHddSize + LineEnding +
                    'Köpfe:  ' + IntToStr(hardDrive.Heads) + LineEnding + 'Spuren:  ' + IntToStr(hardDrive.Tracks) +
                    LineEnding + 'Sektoren:  ' + IntToStr(hardDrive.Sectors) + LineEnding + 'Bytes/Sektor:  ' + IntToStr(SECBYTES);
            end;
            isLoaded := True;
            Close(hddData);
        except;
        end;
    end
    else begin
        hardDrive.ImageFileName := '';
        hardDrive.Size := 0;
    end;
    hardDrive.HddStatus.Hint := hintString;
    hdcStatus.bit[DRDY] := isLoaded;
    hdcStatus.bit[DSC] := isLoaded;
    hardDrive.HddStatus.Enabled := isLoaded;
    Result := isLoaded;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddStatusPanel(var panel: TPanel);
begin
    hardDrive.HddStatus := panel;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setDataLow(Value: byte);
begin
    timerHddStatus.Enabled := True;
    dataBuffer[dataCount] := Value;
    Inc(dataCount);
    if (not enable8BitDataTransfer) then begin
        dataBuffer[dataCount] := hdcDataHigh;
        Inc(dataCount);
    end;
    if (dataCount >= SECBYTES) then begin
        finishWriteData;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setDataHigh(Value: byte);
begin
    hdcDataHigh := Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setTrackLow(Value: byte);
begin
    hdcTrack.low := Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setTrackHigh(Value: byte);
begin
    hdcTrack.high := Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setSector(Value: byte);
begin
    hdcSector := Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setSectorCount(Value: byte);
begin
    hdcSectorCount := Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setDriveHead(Value: byte);
begin
    hdcDriveHead.Value := Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setCommand(Value: byte);
begin
    hdcStatus.Value := 0;
    hdcError.Value := 0;
    hdcStatus.bit[DRDY] := hardDrive.HddStatus.Enabled;
    hardDrive.ImageChanged := False;
    case (Value) of
        $10..$1F: begin    // Recalibrate
            hdcTrack.Value := 0;
            hdcError.Value := 0;
        end;
        $20..$21: begin    // Read Sectors
            prepareReadSectors;
        end;
        $30..$31: begin    // Write Sectors
            prepareWriteSectors;
        end;
        $E4: begin    // Read Buffer
            prepareReadBuffer;
        end;
        $E8: begin    // Write Buffer
            prepareWriteBuffer;
        end;
        $EC: begin    // Identify Drive
            prepareIdentifyDrive;
        end;
        $EF: begin    // Set Feature
            setFeatures;
        end;
        else begin
            hdcStatus.bit[ERR] := True;
            hdcError.bit[ABRT] := True;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setFeatures(Value: byte);
begin
    hdcFeatures := Value;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getDataLow: byte;
var
    dataLow: byte;
begin
    timerHddStatus.Enabled := True;
    dataLow := dataBuffer[dataCount];
    Inc(dataCount);
    if (not enable8BitDataTransfer) then begin
        hdcDataHigh := dataBuffer[dataCount];
        Inc(dataCount);
    end;
    Result := dataLow;
    if (dataCount >= SECBYTES) then begin
        finishReadData;
    end;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getDataHigh: byte;
begin
    Result := hdcDataHigh;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getTrackLow: byte;
begin
    Result := hdcTrack.low;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getTrackHigh: byte;
begin
    Result := hdcTrack.high;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getSector: byte;
begin
    Result := hdcSector;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getSectorCount: byte;
begin
    Result := hdcSectorCount;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getDriveHead: byte;
begin
    Result := hdcDriveHead.Value;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getStatus: byte;
begin
    Result := hdcStatus.Value;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getError: byte;
begin
    Result := hdcError.Value;
end;

// --------------------------------------------------------------------------------
end.
