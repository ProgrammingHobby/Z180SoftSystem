unit System_InOut;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    { TMyClass }

    { TSystemInOut }

    TSystemInOut = class

    private   // Attribute
        read0Data: byte;
        newRead0Data: boolean;
        read1Data: byte;
        newRead1Data: boolean;

    protected // Attribute

    public    // Attribute

    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    private   // Methoden

    protected // Methoden

    public    // Methoden
        function cpuIoRead(port: word): byte;
        procedure cpuIoWrite(port: word; Data: byte);
        procedure cpuTXA0(Data: byte);
        function cpuCanReadRXA0: boolean;
        function cpuRXA0: byte;
        procedure cpuTXA1(Data: byte);
        function cpuCanReadRXA1: boolean;
        function cpuRXA1: byte;

    end;

var
    SystemInOut: TSystemInOut;

implementation

uses System_Terminal, System_Memory, System_Fdc, System_Hdc, System_Rtc;

// --------------------------------------------------------------------------------
constructor TSystemInOut.Create;
begin
    inherited Create;
    read0Data := $00;
    newRead0Data := False;
    read1Data := $00;
    newRead1Data := False;
end;

// --------------------------------------------------------------------------------
destructor TSystemInOut.Destroy;
begin
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
function TSystemInOut.cpuIoRead(port: word): byte;
begin
    case (port and $FF) of
        $70: begin
            Result := SystemFdc.getStatus;
        end;
        $71: begin
            Result := SystemFdc.getTrack;
        end;
        $72: begin
            Result := SystemFdc.getSector;
        end;
        $73: begin
            Result := SystemFdc.getData;
        end;
        $74: begin
            Result := SystemFdc.getExtStatus;
        end;
        $7D: begin
            Result := SystemRtc.Read;
        end;
        $A0: begin
            Result := SystemHdc.getDataLow;
        end;
        $A1: begin
            Result := SystemHdc.getError;
        end;
        $A2: begin
            Result := SystemHdc.getSectorCount;
        end;
        $A3: begin
            Result := SystemHdc.getSector;
        end;
        $A4: begin
            Result := SystemHdc.getTrackLow;
        end;
        $A5: begin
            Result := SystemHdc.getTrackHigh;
        end;
        $A6: begin
            Result := SystemHdc.getDriveHead;
        end;
        $A7: begin
            Result := SystemHdc.getStatus;
        end;
        $A8: begin
            Result := SystemHdc.getDataHigh;
        end;
        else begin
            Result := $FF;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemInOut.cpuIoWrite(port: word; Data: byte);
begin
    case (port and $FF) of
        $70: begin
            SystemFdc.setCommand(Data);
        end;
        $71: begin
            SystemFdc.setTrack(Data);
        end;
        $72: begin
            SystemFdc.setSector(Data);
        end;
        $73: begin
            SystemFdc.setData(Data);
        end;
        $78: begin
            SystemFdc.setExtControl(Data);
        end;
        $7C: begin
            SystemRtc.setAddress(Data);
        end;
        $7D: begin
            SystemRtc.Write(Data);
        end;
        $A0: begin
            SystemHdc.setDataLow(Data);
        end;
        $A1: begin
            SystemHdc.setFeatures(Data);
        end;
        $A2: begin
            SystemHdc.setSectorCount(Data);
        end;
        $A3: begin
            SystemHdc.setSector(Data);
        end;
        $A4: begin
            SystemHdc.setTrackLow(Data);
        end;
        $A5: begin
            SystemHdc.setTrackHigh(Data);
        end;
        $A6: begin
            SystemHdc.setDriveHead(Data);
        end;
        $A7: begin
            SystemHdc.setCommand(Data);
        end;
        $A8: begin
            SystemHdc.setDataHigh(Data);
        end;
        $FF: begin
            SystemMemory.EnableBootRom(False);
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemInOut.cpuTXA0(Data: byte);
begin
    SystemTerminal.writeCharacter(Data);
end;

// --------------------------------------------------------------------------------
function TSystemInOut.cpuCanReadRXA0: boolean;
begin
    Result := SystemTerminal.terminalReadable;
end;

// --------------------------------------------------------------------------------
function TSystemInOut.cpuRXA0: byte;
begin
    Result := SystemTerminal.readCharacter;
end;

// --------------------------------------------------------------------------------
procedure TSystemInOut.cpuTXA1(Data: byte);
begin

end;

// --------------------------------------------------------------------------------
function TSystemInOut.cpuCanReadRXA1: boolean;
begin
    Result := newRead1Data;
end;

// --------------------------------------------------------------------------------
function TSystemInOut.cpuRXA1: byte;
begin
    newRead1Data := False;
    Result := read1Data;
end;

// --------------------------------------------------------------------------------
end.

