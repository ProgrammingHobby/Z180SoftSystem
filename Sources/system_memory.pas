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
        sysMemSize: DWord;
        bootRomSize: DWord;
        bootRomFileName: string;
        bootRomLoaded: boolean;

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
        procedure setReloadImageOnEnable(reload: boolean);
        function GetSystemMemorySize: DWord;
        function GetBootRomSize: DWord;
        function IsRomEnabled: boolean;
        procedure SetRomImageFile(FileName: string);
        procedure LoadRamFile(FileName: string);
        procedure LoadRomFile;
        procedure setBootRomSize(size: integer);
        procedure setSystemRamSize(size: integer);
        function isRomFileValid: boolean;
    end;

var
    SystemMemory: TSystemMemory;

implementation

uses System_Settings;

// --------------------------------------------------------------------------------
constructor TSystemMemory.Create;
begin
    inherited Create;
    setBootRomSize(SystemSettings.ReadInteger('Memory', 'RomSize', 0));
    setSystemRamSize(SystemSettings.ReadInteger('Memory', 'RamSize', 0));
    setReloadImageOnEnable(SystemSettings.ReadBoolean('Memory', 'ReloadOnEnable', False));
    bootRomFileName := SystemSettings.ReadString('Memory', 'RomImageFile', '');
    if ((bootRomFileName <> '') and (not FileExists(bootRomFileName))) then begin
        SystemSettings.WriteString('Memory', 'RomImageFile', '');
        bootRomFileName := '';
    end;
    LoadRomFile;
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
    else if (addr < sysMemSize) then begin
        Result := sysMem[addr];
    end
    else begin
        Result := $FF;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemMemory.Write(addr: DWord; Data: byte);
begin
    if (addr < sysMemSize) then begin
        sysMem[addr] := Data;
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
procedure TSystemMemory.setReloadImageOnEnable(reload: boolean);
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
procedure TSystemMemory.LoadRomFile;
var
    inFile: TFileStream;
    addr: integer;
begin
    bootRomEnabled := False;
    bootRomLoaded := False;

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
            bootRomLoaded := True;
        finally
            inFile.Free;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemMemory.LoadRamFile(FileName: string);
var
    inFile: TFileStream;
    addr: integer;
begin

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
        finally
            inFile.Free;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemMemory.setBootRomSize(size: integer);
var
    newSize: DWord;
begin
    case (size) of
        0: newSize := $2000; //  8KB
        1: newSize := $4000; // 16KB
        2: newSize := $8000; // 32KB
        3: newSize := $10000; // 64KB
        else newSize := $2000;
    end;
    if (bootRomSize <> newSize) then begin
        SetLength(bootRom, newSize);
        bootRomSize := newSize;
    end;
end;

// --------------------------------------------------------------------------------
procedure TSystemMemory.setSystemRamSize(size: integer);
var
    newSize: DWord;
begin
    case size of
        0: newSize := $10000;  //  64KB
        1: newSize := $20000;  // 128KB
        2: newSize := $40000;  // 256KB
        3: newSize := $80000;  // 512KB
        4: newSize := $100000; //   1MB
        else newSize := $10000;
    end;
    if (sysMemSize <> newSize) then begin
        SetLength(sysMem, newSize);
        sysMemSize := newSize;
    end;
end;

// --------------------------------------------------------------------------------
function TSystemMemory.isRomFileValid: boolean;
begin
    Result := bootRomLoaded;
end;

// --------------------------------------------------------------------------------
end.

