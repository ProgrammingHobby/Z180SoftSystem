unit System_Prt;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

type

    { TSystemPrt }

    TSystemPrt = class

    private   // Attribute
        type
        TBitReg8 = bitpacked record
            case byte of
                0: (Value: byte); // 8Bit Register Value
                2: (bit: bitpacked array[0..7] of boolean); // Bit Data
        end;

        const
        // Konstanten für die Status-Bits der Centronics-Schnittstelle
        _ERROR = 02;  // Error
        SEL = 03;     // Select, zeigt Druckerstatus (on- oder offline) an
        PE = 04;      // Paper End, Papierende
        BUSY = 05;    // Busy, zeigt Bereitschaft des Druckers zur Datenübernahme an
        _ACK = 06;    // Acknowledge, Anzeige des Druckers über Empfang der Daten

        var
        prtStatus: TBitReg8;

    protected // Attribute

    public    // Attribute

    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    private   // Methoden

    protected // Methoden

    public    // Methoden
        function getStatus: byte;
        procedure setData(Data: byte);

    end;

var
    SystemPrt: TSystemPrt;

implementation

uses Printers;

{ TSystemPrt }

// --------------------------------------------------------------------------------
constructor TSystemPrt.Create;
begin
    inherited Create;
    prtStatus.bit[BUSY] := True;
    prtStatus.bit[_ERROR] := True;
end;

// --------------------------------------------------------------------------------
destructor TSystemPrt.Destroy;
begin
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
function TSystemPrt.getStatus: byte;
begin
    Result := prtStatus.Value;
end;

// --------------------------------------------------------------------------------
procedure TSystemPrt.setData(Data: byte);
begin
//
end;

// --------------------------------------------------------------------------------
end.

