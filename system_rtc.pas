unit System_Rtc;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    { TSystemRtc }

    TSystemRtc = class

    private   // Attribute

    protected // Attribute

    public    // Attribute

    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    private   // Methoden

    protected // Methoden

    public    // Methoden
        procedure setAddress(addr: byte);
        procedure writeData(Data: byte);
        function readData: byte;

    end;

var
    SystemRtc: TSystemRtc;

implementation

{ TSystemRtc }

// --------------------------------------------------------------------------------
constructor TSystemRtc.Create;
begin
    inherited Create;
end;

// --------------------------------------------------------------------------------
destructor TSystemRtc.Destroy;
begin
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
procedure TSystemRtc.setAddress(addr: byte);
begin

end;

// --------------------------------------------------------------------------------
procedure TSystemRtc.writeData(Data: byte);
begin

end;

// --------------------------------------------------------------------------------
function TSystemRtc.readData: byte;
begin
    Result := 0;
end;

// --------------------------------------------------------------------------------
end.

