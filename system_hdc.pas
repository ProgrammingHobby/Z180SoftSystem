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
        TbitReg8 = bitpacked record
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
end;

// --------------------------------------------------------------------------------
procedure TSystemHdc.setHddStatusPanel(var panel: TPanel);
begin
    hardDrive.HddStatus := panel;
end;

// --------------------------------------------------------------------------------
end.

