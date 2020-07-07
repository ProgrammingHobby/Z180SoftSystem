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
        TBitReg8 = bitpacked record
            case byte of
                0: (Value: byte); // 8Bit Register Value
                2: (bit: bitpacked array[0..7] of boolean); // Bit Data
        end;

        THardDriveData = record
            Heads: integer;
            Tracks: integer;
            Sectors: integer;
            SectorBytes: integer;
            ImageFileName: string;
            Size: integer;
            ImageChanged: boolean;
            HddStatus: TPanel;
        end;

    var
        timerHddStatus: TTimer;
        isMultiSectorCommand: boolean;
        isReadCommand: boolean;
        isWriteCommand: boolean;
        hardDrive: THardDriveData;
        hddData: file of byte;
        hdcDataLow: byte;
        hdcDataHigh: byte;
        hdcTrackLow: byte;
        hdcTrackHigh: byte;
        hdcSector: byte;
        hdcSectorCount: byte;
        hdcDriveHead: byte;
        hdcCommand: byte;
        hdcPrecomp: byte;
        hdcStatus: TBitReg8;
        hdcError: TBitReg8;

    protected // Attribute

    public    // Attribute

    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    private   // Methoden

    protected // Methoden

    public    // Methoden
        procedure setHddHeads(heads: integer);
        procedure setHddTracks(tracks: integer);
        procedure setHddSectors(sectors: integer);
        procedure setHddSectorBytes(sectorbytes: integer);
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
        procedure setPrecomp(Value: byte);
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
end;

// --------------------------------------------------------------------------------
destructor TSystemHdc.Destroy;
begin
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddHeads(heads: integer);
begin
    hardDrive.Heads := heads;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddTracks(tracks: integer);
begin
    hardDrive.Tracks := tracks;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddSectors(sectors: integer);
begin
    hardDrive.Sectors := sectors;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddSectorBytes(sectorbytes: integer);
begin
    hardDrive.SectorBytes := sectorbytes;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.setHddImage(fileName: string): boolean;
begin
    hardDrive.ImageFileName := fileName;
    Result := False;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddStatusPanel(var panel: TPanel);
begin
    hardDrive.HddStatus := panel;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setDataLow(Value: byte);
begin
    hdcDataLow := Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setDataHigh(Value: byte);
begin
    hdcDataHigh := Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setTrackLow(Value: byte);
begin
    hdcTrackLow := Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setTrackHigh(Value: byte);
begin
    hdcTrackHigh := Value;
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
    hdcDriveHead := Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setCommand(Value: byte);
begin
    hdcCommand := Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setPrecomp(Value: byte);
begin
    hdcPrecomp := Value;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getDataLow: byte;
begin
    Result := hdcDataLow;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getDataHigh: byte;
begin
    Result := hdcDataHigh;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getTrackLow: byte;
begin
    Result := hdcTrackLow;
end;

// --------------------------------------------------------------------------------
function TSystemHdc.getTrackHigh: byte;
begin
    Result := hdcTrackHigh;
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
    Result := hdcDriveHead;
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

