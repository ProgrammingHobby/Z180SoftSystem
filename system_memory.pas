unit System_Memory;

{$mode objfpc} {$H+}

interface

uses
    Classes, SysUtils;

type

    { TSystemMemory }

    TSystemMemory = class(TObject)
    private   // Attribute
        type
        TByteArray = array of byte;

    var
        sysMem: TByteArray;
        bootRom: TByteArray;
        bootRomEnabled: boolean;
        reloadOnEnable: boolean;
        fullAdressDecode: boolean;
        sysMemSize: DWord;
        bootRomSize: DWord;
        bootRomFileName: string;
        isLoaded: boolean;

    protected // Attribute

    public    // Attribute

    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    private   // Methoden

    protected // Methoden

    public    // Methoden
        function Read(addr: DWord): byte;
        procedure Write(addr: DWord; Data: byte);
        procedure EnableBootRom(enable: boolean);
        procedure EnableReloadImageOnEnable(reload: boolean);
        function GetSystemMemorySize: DWord;
        function GetBootRomSize: DWord;
        function IsRomEnabled: boolean;
        procedure SetRomImageFile(FileName: string);
        function LoadRamFile(FileName: string): boolean;
        function LoadRomFile: boolean;
        procedure setBootRomSize(size: string);
        procedure setSystemRamSize(size: string);
        procedure EnableFullAdressDecode(enable: boolean);
        function isRomFileValid: boolean;
    end;

var
    SystemMemory: TSystemMemory;

implementation

// --------------------------------------------------------------------------------
constructor TSystemMemory.Create;
begin
    inherited Create;
    sysMemSize := 0;
    bootRomSize := 0;
    //setSystemRamSize(0);
    //setBootRomSize(0);
    bootRomEnabled := False;
    reloadOnEnable := False;
    fullAdressDecode := True;
end;

// --------------------------------------------------------------------------------
destructor TSystemMemory.Destroy;
begin
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
function TSystemMemory.Read(addr: DWord): byte;
begin
    if ((bootRomEnabled = True) and (addr < bootRomSize)) then begin
        Result := bootRom[addr];
    end
    else if (fullAdressDecode) then begin
        if (addr < sysMemSize) then begin
            Result := sysMem[addr];
        end
        else begin
            Result := $FF;
        end;
    end
    else begin
        Result := sysMem[(addr and sysMemSize)];
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemMemory.Write(addr: DWord; Data: byte);
begin
    if (fullAdressDecode) then begin
        if (addr < sysMemSize) then begin
            sysMem[addr] := Data;
        end;
    end
    else begin
        sysMem[(addr and sysMemSize)] := Data;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemMemory.EnableBootRom(enable: boolean);
begin
    bootRomEnabled := enable;
    if (bootRomEnabled and reloadOnEnable) then begin
        LoadRomFile;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemMemory.EnableReloadImageOnEnable(reload: boolean);
begin
    reloadOnEnable := reload;
end;

// --------------------------------------------------------------------------------
function TSystemMemory.GetSystemMemorySize: DWord;
begin
    Result := sysMemSize;
end;

// --------------------------------------------------------------------------------
function TSystemMemory.GetBootRomSize: DWord;
begin
    Result := bootRomSize;
end;

// --------------------------------------------------------------------------------
function TSystemMemory.IsRomEnabled: boolean;
begin
    Result := bootRomEnabled;
end;

// --------------------------------------------------------------------------------
procedure TSystemMemory.SetRomImageFile(FileName: string);
begin
    bootRomFileName := FileName;
    LoadRomFile;
end;

// --------------------------------------------------------------------------------
function TSystemMemory.LoadRomFile: boolean;
var
    inFile: TFileStream;
    addr: integer;
begin
    isLoaded := False;
    if (bootRomFileName = '') then begin
        for addr := 0 to bootRomSize - 1 do begin
            bootRom[addr] := 0;
        end;
    end
    else begin
        inFile := TFileStream.Create(bootRomFileName, fmOpenRead);
        inFile.Position := 0;
        try
            if (inFile.Size < bootRomSize) then begin
                inFile.Read(bootRom[0], inFile.Size);
            end
            else begin
                inFile.Read(bootRom[0], bootRomSize);
            end;
            bootRomEnabled := True;
            isLoaded := True;
        finally
            inFile.Free;
        end;
    end;
    Result := isLoaded;
end;

// --------------------------------------------------------------------------------
function TSystemMemory.LoadRamFile(FileName: string): boolean;
var
    inFile: TFileStream;
    addr: integer;
begin
    isLoaded := False;
    if (FileName = '') then begin
        for addr := 0 to sysMemSize - 1 do begin
            sysMem[addr] := 0;
        end;
    end
    else begin
        inFile := TFileStream.Create(FileName, fmOpenRead);
        inFile.Position := 0;
        try
            if (inFile.Size < sysMemSize) then begin
                inFile.Read(sysMem[0], inFile.Size);
            end
            else begin
                inFile.Read(sysMem[0], sysMemSize);
            end;
            bootRomEnabled := False;
            isLoaded := True;
        finally
            inFile.Free;
        end;
    end;
    Result := isLoaded;
end;

// --------------------------------------------------------------------------------
procedure TSystemMemory.setBootRomSize(size: string);
var
    romSize: DWord;
begin
    case size of
        '4KB': romSize := $1000; //  4KB
        '8KB': romSize := $2000; //  8KB
        '16KB': romSize := $4000; // 16KB
        '32KB': romSize := $8000; // 32KB
        else romSize := $2000;
    end;
    if (bootRomSize <> romSize) then begin
        SetLength(bootRom, romSize);
        bootRomSize := romSize;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemMemory.setSystemRamSize(size: string);
var
    ramSize: DWord;
begin
    case size of
        '64KB': ramSize := $10000;  //  64KB
        '128KB': ramSize := $20000;  // 128KB
        '256KB': ramSize := $40000;  // 256KB
        '512KB': ramSize := $80000;  // 512KB
        '1024KB': ramSize := $100000; //   1MB
        else ramSize := $10000;
    end;
    if (sysMemSize <> ramSize) then begin
        SetLength(sysMem, ramSize);
        sysMemSize := ramSize;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemMemory.EnableFullAdressDecode(enable: boolean);
begin
    fullAdressDecode := enable;
end;

// --------------------------------------------------------------------------------
function TSystemMemory.isRomFileValid: boolean;
begin
    Result := isLoaded;
end;

// --------------------------------------------------------------------------------
end.

