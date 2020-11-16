unit Z180_CPU;

{$mode objfpc}{$H+}
{$inline on}
{$coperators off}
{$rangechecks off}

//{$define NOTRAP}

interface

uses
    Classes, SysUtils;

type

    { TZ180Cpu }

    TZ180Cpu = class

    private   // Attribute
        type
        TregAF = bitpacked record
            case byte of
                0: (Value: word); // 16Bit Register-Paar AF
                1: (F: byte;      // 8Bit Register F (lower 8Bit)
                    A: byte);     // 8Bit Register A (upper 8Bit)
                2: (Flag: bitpacked array[0..15] of boolean); // Flag bits
        end;

        TregBC = packed record
            case byte of
                0: (Value: word); // 16Bit Register-Paar BC
                1: (C: byte;      // 8Bit Register C (lower 8Bit)
                    B: byte);     // 8Bit Register B (upper 8Bit)
        end;

        TregDE = packed record
            case byte of
                0: (Value: word); // 16Bit Register-Paar DE
                1: (E: byte;      // 8Bit Register E (lower 8Bit)
                    D: byte);     // 8Bit Register D (upper 8Bit)
        end;

        TregHL = packed record
            case byte of
                0: (Value: word); // 16Bit Register-Paar HL
                1: (L: byte;      // 8Bit Register L (lower 8Bit)
                    H: byte);     // 8Bit Register H (upper 8Bit)
        end;

        TbitReg8 = bitpacked record
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
                1: (low: byte;     // lower 8Bit
                    high: byte;    // upper 8Bit
                    bank: byte;    // bank 4bit
                    nv: byte);     // not used
        end;

        TIntMode = (IM0, IM1, IM2);
        TAcsiDmaMode = (OFF, ASCI0SEND, ASCI1SEND, ASCI0RECEIVE, ASCI1RECEIVE);

    var
        // Variablen fuer die Takt- und Machinen-Zyklen Verarbeitung
        machineCycles, clockCycles: DWord;
        extraWaitCycles, ioWaitCycles, memWaitCycles: DWord;

        // 1. Z180Cpu Standard-Registersatz
        regAF: TregAF;  // 16Bit Register-Paar AF
        regBC: TregBC;  // 16Bit Register-Paar BC
        regDE: TRegDE;  // 16Bit Register-Paar DE
        regHL: TregHL;  // 16Bit Register-Paar HL

        // 2. Z180Cpu Standard-Registersatz
        // diese werden nur als 16Bit Registerpaar abgebildet,
        // da diese nur ueber die 'EX' bzw. 'EXX' Befehle zu erreichen sind
        regAF_: word;
        regBC_: word;
        regDE_: word;
        regHL_: word;

        // Abbildung des Z180Cpu Spezial-Registersatzes
        regI: byte;      // 8Bit Register I
        regR: byte;      // 8Bit Register R
        regPC: Treg16;   // 16Bit Register PC
        regSP: Treg16;   // 16Bit Register SP
        regIX: Treg16;   // 16Bit Register IX
        regIY: Treg16;   // 16Bit Register IY

        // Abbildung der internen Z180Cpu I/O-Register
        ioCNTLA0: TbitReg8; // ASCI-Channel 0 Control Register A
        ioCNTLA1: TbitReg8; // ASCI-Channel 1 Control Register A
        ioCNTLB0: TbitReg8; // ASCI-Channel 0 Control Register B
        ioCNTLB1: TbitReg8; // ASCI-Channel 1 Control Register B
        ioSTAT0: TbitReg8;  // ASCI-Channel 0 Status
        ioSTAT1: TbitReg8;  // ASCI-Channel 1 Status
        ioTDR0: byte;       // ASCI-Channel 0 Transmit Data Register
        ioTDR1: byte;       // ASCI-Channel 1 Transmit Data Register
        ioRDR0: byte;       // ASCI-Channel 0 Receive Data Register
        ioRDR1: byte;       // ASCI-Channel 1 Receive Data Register
        ioCNTR: TbitReg8;   // CSI/O Control Register
        ioTRD: byte;        // CSI/O Transmit/Receive Data Register
        ioTMDR0: Treg16;    // Timer-Channel 0 Data Register
        ioRLDR0: Treg16;    // Timer-Channel 0 Reload Register
        ioTCR: TbitReg8;    // Timer Control Register
        ioTMDR1: Treg16;    // Timer-Channel 1 Data Register
        ioRLDR1: Treg16;    // Timer-Channel 1 Reload Register
        ioFRC: byte;        // Free Running Counter
        ioSAR0: Treg32;     // DMA-Channel 0 Source Address Register
        ioDAR0: Treg32;     // DMA-Channel 0 Destination Address Register
        ioBCR0: Treg32;     // DMA-Channel 0 Byte Count Register (32Bit um vollen 64K Transfer zu handeln)
        ioMAR1: Treg32;     // DMA-Channel 1 Memory Address Register
        ioIAR1: Treg16;     // DMA-Channel 1 I/O Address Register
        ioBCR1: Treg32;     // DMA-Channel 1 Byte Count Register (32Bit um vollen 64K Transfer zu handeln)
        ioDSTAT: TbitReg8;  // DMA Status Register
        ioDMODE: TbitReg8;  // DMA Mode Register
        ioDCNTL: TbitReg8;  // DMA/WAIT Control Register
        ioIL: byte;         // Interrupt Vector Low Register
        ioITC: TbitReg8;    // INT/TRAP Control Register
        ioRCR: TbitReg8;    // Refresh Control Register
        ioCBR: byte;        // MMU Common Base Register
        ioBBR: byte;        // MMU Bank Base Register
        ioCBAR: byte;       // MMU Common/Base Register
        ioOMCR: TbitReg8;   // Operation Mode Control Register
        ioICR: TbitReg8;    // I/O Control Register

        // Interrupt- und Betriebsmodus Flags
        IFF1: boolean;          // Interrupt-Enable Flag 1
        IFF2: boolean;          // Interrupt-Enable Flag 2
        SLP, tmpSLP: boolean;   // Sleep-Mode enabled
        HALT, tmpHALT: boolean; // Halt-Mode enabled
        intMode: TIntMode;      // Interrupt mode (IM0, IM1, IM2)

        // Abbildung der moeglichen internen Interrupts. Externe Interrupts werden nicht unterstuetzt
        intTRAP: boolean;  // OP-Code Trap Prio 1
        intPRT0: boolean;  // Programmable Reload Timer0 Interrupt Prio 2
        intPRT1: boolean;  // Programmable ReloadTimer1 Interrupt Prio 3
        intDMA0: boolean;  // DMA-Channel0 Interrupt Prio 4
        intDMA1: boolean;  // DMA-Channel1 Interrupt Prio 5
        intCSIO: boolean;  // Clocked Serial-IO Interrupt Prio 6
        intASCI0: boolean; // ASCI-Channel0 Interrupt Prio 7
        intASCI1: boolean; // ASCI-Channel1 Interrupt Prio 8

        // Hilfsvariablen fuer die ASCI Funktion
        asciPhiDevideRatio0, asciPhiDevideRatio1: dword; // Teilungsfaktor fuer den Baudraten-Takt
        asciClockCount0, asciClockCount1: dword;         // Takt-Zaehler fuer den Baudraten-Takt
        shiftModeRatio0, shiftModeRatio1: byte;          // Gesamtzahl der zu Übertragenden Bits pro Datenbyte
        transmitShiftCount0, transmitShiftCount1: byte;  // Bit-Zaehler fuer das Senden von Daten
        receiveShiftCount0, receiveShiftCount1: byte;    // Bit-Zaehler fuer das Empfangen von Daten
        TSR0, TSR1: byte;                                // Sende Schiebe-Register (nicht per I/O Ansprechbar)
        RSR0, RSR1: byte;                                // Empfangs Schiebe-Register (nicht per I/O Ansprechbar)
        asciTSR0E, asciTSR1E: boolean;                   // Hilfsflag 'Transmit Shift Register Empty'
        asciRSR0F, asciRSR1F: boolean;                   // Hilfsflag 'Receive Shift Register Full'

        // Hilfsvariablen fuer die PRT Funktion
        prtClockCount: byte;               // Takt-Zaehler fuer die Programmable Reload Timer
        bufTMDR0H, bufTMDR1H: byte;        // Puffer-Variable fuer Timer Data Register High
        isBufTMDR0H, isBufTMDR1H: boolean; // Flags fuer die Daten-Pufferung

        // Hilfsvariablen für den DMA Controller
        dmaBurstMode: boolean;  // zeigt an ob DMA-Channel 0 im Cycles-Steal oder im Burst-Mode arbeitet
        dreq0: boolean;         // DMA Request Channel 0
        dreq1: boolean;         // DMA Request Channel 1
        tend0: boolean;         // Transfer End Channel 0
        tend1: boolean;         // Transfer End Channel 1
        asciDmaMode: TAcsiDmaMode;

        // statische Arrays fuer ASCI- und Wait-Cycles
    const
        asciDevide: array[0..7] of byte = (1, 2, 4, 8, 16, 32, 64, 0);
        asciTransLength: array[0..7] of byte = (9, 10, 10, 11, 10, 11, 11, 12);
        memCycles: array[0..3] of byte = (0, 1, 2, 3);
        ioCycles: array[0..3] of byte = (0, 2, 3, 4);

        // Konstanten fuer die Status-Flags der CPU
    const
        C = $00;  // Carry Flag
        N = $01;  // Negative Flag
        PV = $02; // Parity/Overflow Flag
        H = $04;  // Half Carry Flag
        Z = $06;  // Zero Flag
        S = $07;  // Sign Flag

        // Konstanten fuer die internen I/O Bit-Adressen
    const
        MOD0 = $00;     // Mode Selection 0
        MOD1 = $01;     // Mode Selection 1
        MOD2 = $02;     // Mode Selection 2
        MODsel = $07;   // Mode Selection
        MPBR_EFR = $03; // Multi Processor Bit Receive/Error Flag Reset
        RTS0 = $04;     // Request to Send
        CKA1D = $04;    // CKA1 Disable
        TE = $05;       // ASCI Transmit Enable
        RE = $06;       // ASCI Receive Enable
        MPE = $07;      // Multi Processor Enable
        SS0 = $00;      // Clock Source and Speed Select 0
        SS1 = $01;      // Clock Source and Speed Select 1
        SS2 = $02;      // Clock Source and Speed Select 0
        SSsel = $07;    // Clock Source and Speed Select
        DR = $03;       // Divide Ratio
        PEO = $04;      // Parity Even or Odd
        CTS_PS = $05;   // Clear to send/Prescale
        MP = $06;       // Multi Processor
        MPBT = $07;     // Multi Processor Bit Transmit
        TIE = $00;      // Transmit Interrupt Enable
        TDRE = $01;     // Transmit Data Register Empty
        DCD0 = $02;     // Data Carrier Detect
        CTS1e = $02;    // CTS1 Enable
        RIE = $03;      // Receive Interrupt Enable
        FE = $04;       // Framing Error
        PE = $05;       // Parity Error
        OVRN = $06;     // Overrun Error
        RDRF = $07;     // Receive Data Register Full
        TEcsio = $04;   // CSI/O Transmit Enable
        REcsio = $05;   // CSI/O Receive Enable
        EIE = $06;      // End Interrupt Enable
        EF = $07;       // End Flag
        TDE0 = $00;     // Timer Down Count Enable 0
        TDE1 = $01;     // Timer Down Count Enable 1
        TOC0 = $02;     // Timer Output Control 0
        TOC1 = $03;     // Timer Output Control 1
        TIE0 = $04;     // Timer Interrupt Enable 0
        TIE1 = $05;     // Timer Interrupt Enable 1
        TIF0 = $06;     // Timer Interrupt Flag 0
        TIF1 = $07;     // Timer Interrupt Flag 1
        DME = $00;      // DMA Master enable
        DIE0 = $02;     // DMA Interrupt Enable 0
        DIE1 = $03;     // DMA Interrupt Enable 1
        DWE0 = $04;     // DMA Enable Bit Write Enable 0
        DWE1 = $05;     // DMA Enable Bit Write Enable 1
        DE0 = $06;      // DMA Enable Channel 0
        DE1 = $07;      // DMA Enable Channel 0
        MMOD = $01;     // Memory MODE select
        SM0 = $02;      // Channel 0 Source Mode 0
        SM1 = $03;      // Channel 0 Source Mode 1
        SMsel = $0C;    // Channel 0 Source
        DM0 = $04;      // Channel 0 Destination Mode 0
        DM1 = $05;      // Channel 0 Destination Mode 1
        DMsel = $30;    // Channel 0 Destination
        DIM0 = $00;     // DMA Channel 1 I/O Memory Mode Select 0
        DIM1 = $01;     // DMA Channel 1 I/O Memory Mode Select 1
        DIMsel = $03;   // DMA Channel 1 I/O Memory Mode Select
        DMS0 = $02;     // DREQ0 Select
        DMS1 = $03;     // DREQ1 Select
        IWI0 = $04;     // I/O Wait Insertion 0
        IWI1 = $05;     // I/O Wait Insertion 1
        IWIsel = $30;   // I/O Wait Insertion
        MWI0 = $06;     // Memory Wait Insertion 0
        MWI1 = $07;     // Memory Wait Insertion 1
        MWIsel = $C0;   // Memory Wait Insertion
        ITE0 = $00;     // INT 0 Enable
        ITE1 = $01;     // INT 1 Enable
        ITE2 = $02;     // INT 2 Enable
        UFO = $06;      // Unidentified Fetch Object
        TRAP = $07;     // TRAP
        CYC0 = $00;     // Refresh Cycle select 0
        CYC1 = $01;     // Refresh Cycle select 1
        REFW = $06;     // Refresh Wait State
        REFE = $07;     // Refresh Enable
        IOC = $05;      // I/O Compatibility
        M1TE = $06;     // M1 Temporary Enable
        M1E = $07;      // M1 Enable
        IOSTP = $05;    // I/O Stop
        IOA6 = $06;     // internal I/O Address 0
        IOA7 = $07;     // internal I/O Address 1
        IOAsel = $C0;   // internal I/O Address

    protected // Attribute

    public    // Attribute
        // Datenstruktur für den export der CPU Core-Register Daten
        type
        TCoreData = record
            A1: byte;
            F1: byte;
            B1: byte;
            C1: byte;
            BC1: word;
            BCi1: byte;
            D1: byte;
            E1: byte;
            DE1: word;
            DEi1: byte;
            H1: byte;
            L1: byte;
            HL1: word;
            HLi1: byte;
            A2: byte;
            F2: byte;
            B2: byte;
            C2: byte;
            BC2: word;
            BCi2: byte;
            D2: byte;
            E2: byte;
            DE2: word;
            DEi2: byte;
            H2: byte;
            L2: byte;
            HL2: word;
            HLi2: byte;
            I: byte;
            R: byte;
            IX: word;
            IY: word;
            SP: word;
            SPi: byte;
            PC: word;
            PCi: byte;
            PCmmu: longword;
            IFF1: boolean;
            IFF2: boolean;
            intMode: byte;
        end;

        // Datenstruktur für den export der CPU IO-Register Daten
        TIoData = record
            CNTLA0: byte; // ASCI-Channel 0 Control Register A
            CNTLA1: byte; // ASCI-Channel 1 Control Register A
            CNTLB0: byte; // ASCI-Channel 0 Control Register B
            CNTLB1: byte; // ASCI-Channel 1 Control Register B
            STAT0: byte;  // ASCI-Channel 0 Status
            STAT1: byte;  // ASCI-Channel 1 Status
            TDR0: byte;   // ASCI-Channel 0 Transmit Data Register
            TDR1: byte;   // ASCI-Channel 1 Transmit Data Register
            RDR0: byte;   // ASCI-Channel 0 Receive Data Register
            RDR1: byte;   // ASCI-Channel 1 Receive Data Register
            CNTR: byte;   // CSI/O Control Register
            TRD: byte;    // CSI/O Transmit/Receive Data Register
            TMDR0L: byte; // Timer-Channel 0 Data Register low
            TMDR0H: byte; // Timer-Channel 0 Data Register high
            RLDR0L: byte; // Timer-Channel 0 Reload Register low
            RLDR0H: byte; // Timer-Channel 0 Reload Register high
            TCR: byte;    // Timer Control Register
            TMDR1L: byte; // Timer-Channel 1 Data Register low
            TMDR1H: byte; // Timer-Channel 1 Data Register high
            RLDR1L: byte; // Timer-Channel 1 Reload Register low
            RLDR1H: byte; // Timer-Channel 1 Reload Register high
            FRC: byte;    // Free Running Counter
            SAR0L: byte;  // DMA-Channel 0 Source Address Register low
            SAR0H: byte;  // DMA-Channel 0 Source Address Register high
            SAR0B: byte;  // DMA-Channel 0 Source Address Register bank
            DAR0L: byte;  // DMA-Channel 0 Destination Address Register low
            DAR0H: byte;  // DMA-Channel 0 Destination Address Register high
            DAR0B: byte;  // DMA-Channel 0 Destination Address Register bank
            BCR0L: byte;  // DMA-Channel 0 Byte Count Register low
            BCR0H: byte;  // DMA-Channel 0 Byte Count Register high
            MAR1L: byte;  // DMA-Channel 1 Memory Address Register low
            MAR1H: byte;  // DMA-Channel 1 Memory Address Register high
            MAR1B: byte;  // DMA-Channel 1 Memory Address Register bank
            IAR1L: byte;  // DMA-Channel 1 I/O Address Register low
            IAR1H: byte;  // DMA-Channel 1 I/O Address Register high
            BCR1L: byte;  // DMA-Channel 1 Byte Count Register low
            BCR1H: byte;  // DMA-Channel 1 Byte Count Register high
            DSTAT: byte;  // DMA Status Register
            DMODE: byte;  // DMA Mode Register
            DCNTL: byte;  // DMA/WAIT Control Register
            IL: byte;     // Interrupt Vector Low Register
            ITC: byte;    // INT/TRAP Control Register
            RCR: byte;    // Refresh Control Register
            CBR: byte;    // MMU Common Base Register
            BBR: byte;    // MMU Bank Base Register
            CBAR: byte;   // MMU Common/Base Register
            OMCR: byte;   // Operation Mode Control Register
            ICR: byte;    // I/O Control Register
        end;

    public  // Konstruktor/Destruktor
        constructor Create; overload;
        destructor Destroy; override;

    private   // Methoden
        function memRead(const addr: word): byte;
        procedure memWrite(const addr: word; const Value: byte);
        function ioRead(const portHI: byte; const portLO: byte): byte;
        procedure ioWrite(const portHI: byte; const portLO: byte; const Data: byte);
        function calcPhysicalMemAddr(logAddr: word): DWord;
        function calcAsciPhiDevide(asciControlRegister: byte): dword;
        function readOpCode(const addr: DWord): byte;
        procedure doAsci0;
        procedure doAsci1;
        procedure doPrt0;
        procedure doPrt1;
        procedure doDma0;
        procedure doDma1;


        function calcParity(Value: byte): boolean;
        procedure inc8Bit(var Value: byte);
        procedure dec8Bit(var Value: byte);
        procedure addA8Bit(const Value: byte);
        procedure adcA8Bit(const Value: byte);
        procedure subA8Bit(const Value: byte);
        procedure sbcA8Bit(const Value: byte);
        procedure andA8Bit(const Value: byte);
        procedure xorA8Bit(const Value: byte);
        procedure orA8Bit(const Value: byte);
        procedure cpA8Bit(const Value: byte);
        procedure rlc8Bit(var Value: byte);
        procedure rrc8Bit(var Value: byte);
        procedure rl8Bit(var Value: byte);
        procedure rr8Bit(var Value: byte);
        procedure sla8Bit(var Value: byte);
        procedure sra8Bit(var Value: byte);
        procedure srl8Bit(var Value: byte);
        procedure tstBit(const Bit: byte; const Value: byte);
        procedure resBit(const Bit: byte; var Value: byte);
        procedure setBit(const Bit: byte; var Value: byte);
        procedure tstA8Bit(const Value: byte);
        function inreg8Bit(const portHi: byte; const portLo: byte): byte;
        procedure addHL16Bit(const Value: word);
        procedure adcHL16Bit(const Value: word);
        procedure sbcHL16Bit(const Value: word);
        procedure addIX16Bit(const Value: word);
        procedure addIY16Bit(const Value: word);
        procedure push(const Value: word);
        procedure pop(var Value: word);
        procedure execOp00Codes;
        procedure execOpCbCodes;
        procedure execOpDdCodes;
        procedure execOpDdCbCodes;
        procedure execOpFdCodes;
        procedure execOpFdCbCodes;
        procedure execOpEdCodes;

    protected // Methoden

    public    // Methoden
        procedure reset;
        procedure exec(opCount: DWord = 1);
        function getCoreData: TCoreData;
        function getIoRegData: TIoData;
        procedure setDREQ0;
        procedure setDREQ1;
        function getTEND0: boolean;
        function getTEND1: boolean;

    end;

var
    Z180Cpu: TZ180Cpu;

implementation

uses
    System_Memory, System_InOut;

// --------------------------------------------------------------------------------
constructor TZ180Cpu.Create;
begin
    inherited Create;
    asciClockCount0 := 0;
    transmitShiftCount0 := 0;
    receiveShiftCount0 := 0;
    asciTSR0E := True;
    asciRSR0F := False;
    asciClockCount1 := 0;
    transmitShiftCount1 := 0;
    receiveShiftCount1 := 0;
    asciTSR1E := True;
    asciRSR1F := False;
    prtClockCount := 0;
    isBufTMDR0H := False;
    isBufTMDR1H := False;

    machineCycles := 0;
    clockCycles := 0;
    extraWaitCycles := 0;
    memWaitCycles := 0;
    ioWaitCycles := 0;
end;

// --------------------------------------------------------------------------------
destructor TZ180Cpu.Destroy;
begin
    inherited Destroy;
end;

// --------------------------------------------------------------------------------
function TZ180Cpu.memRead(const addr: word): byte; inline;
begin
    extraWaitCycles := extraWaitCycles + memWaitCycles;
    Result := SystemMemory.Read(calcPhysicalMemAddr(addr));
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.memWrite(const addr: word; const Value: byte); inline;
begin
    extraWaitCycles := extraWaitCycles + memWaitCycles;
    SystemMemory.Write(calcPhysicalMemAddr(addr), Value);
end;

// --------------------------------------------------------------------------------
function TZ180Cpu.ioRead(const portHI: byte; const portLO: byte): byte;
begin
    extraWaitCycles := extraWaitCycles + ioWaitCycles;
    result := SystemInOut.cpuIoRead(((portHI shl 8) + portLO));
    if ((portLo <= (ioICR.Value and IOAsel) + $3F) and (portLo >= (ioICR.Value and IOAsel)) and (portHi = $00)) then begin
        case (portLO and $3F) of
            $00: begin   //portCNTLA0
                result := (ioCNTLA0.Value and $FF);
            end;
            $01: begin   //portCNTLA1
                result := (ioCNTLA1.Value and $FF);
            end;
            $02: begin   //portCNTLB0
                result := (ioCNTLB0.Value and $FF);
            end;
            $03: begin   //portCNTLB1
                result := (ioCNTLB1.Value and $FF);
            end;
            $04: begin   //portSTAT0
                result := (ioSTAT0.Value and $FF);
            end;
            $05: begin   //portSTAT1
                result := (ioSTAT1.Value and $FF);
            end;
            $06: begin   //portTDR0
                result := (ioTDR0 and $FF);
            end;
            $07: begin   //portTDR1
                result := (ioTDR1 and $FF);
            end;
            $08: begin   //portRDR0
                result := (ioRDR0 and $FF);
                ioSTAT0.bit[RDRF] := False;   // Bit 'Receive Data Register Full' loeschen
            end;
            $09: begin   //portRDR1
                result := (ioRDR1 and $FF);
                ioSTAT1.bit[RDRF] := False;   // Bit 'Receive Data Register Full' loeschen
            end;
            $0A: begin   //portCNTR
                result := (ioCNTR.Value and $F7);
            end;
            $0B: begin   //portTRD
                result := (ioTRD and $FF);
            end;
            $0C: begin   //portTMDR0L
                bufTMDR0H := (ioTMDR0.high and $FF);   // Lesereihenfolge Low-Byte , High-Byte !
                isBufTMDR0H := True; // High-Byte puffern. Z8018x Family MPU User Manual Seite 158
                result := (ioTMDR0.low and $FF);
                ioTCR.bit[TIF0] := False; // Timer 0 Interrupt Flag beim lesen von TMDR0L loeschen
            end;
            $0D: begin   //portTMDR0H
                if (isBufTMDR0H = True) then begin   // wenn gepuffertes High-Byte vorhanden
                    result := (bufTMDR0H and $FF);   // dann dieses auslesen
                    isBufTMDR0H := False;    // und Puffer-Marker loeschen.
                end
                else begin
                    result := (ioTMDR0.high and $FF00);
                end;
                ioTCR.bit[TIF0] := False; // Timer 0 Interrupt Flag beim lesen von TMDR0H loeschen
            end;
            $0E: begin   //portRLDR0L
                result := (ioRLDR0.low and $FF);
            end;
            $0F: begin   //portRLDR0H
                result := (ioRLDR0.high and $FF);
            end;
            $10: begin   //portTCR
                result := (ioTCR.Value and $FF);
                ioTCR.bit[TIF0] := False; // Timer 0 Interrupt Flag und
                ioTCR.bit[TIF1] := False; // Timer 1 Interrupt Flag und beim lesen von TCR loeschen
            end;
            $14: begin   //portTMDR1L
                bufTMDR1H := (ioTMDR1.high and $FF);   // Lesereihenfolge Low-Byte , High-Byte !
                isBufTMDR1H := True; // High-Byte puffern. Z8018x Family MPU User Manual Seite 158
                result := (ioTMDR1.low and $FF);
                ioTCR.bit[TIF1] := False; // Timer 1 Interrupt Flag beim lesen von TMDR1L loeschen
            end;
            $15: begin   //portTMDR1H
                if (isBufTMDR1H = True) then begin  // wenn gepuffertes High-Byte vorhanden
                    result := (bufTMDR1H and $FF);   // dann dieses auslesen
                    isBufTMDR1H := False;    // und Puffer-Marker loeschen.
                end
                else begin
                    result := (ioTMDR1.high and $FF);
                end;
                ioTCR.bit[TIF1] := False; // Timer 1 Interrupt Flag beim lesen von TMDR1H loeschen
            end;
            $16: begin   //portRLDR1L
                result := (ioRLDR1.low and $FF);
            end;
            $17: begin   //portRLDR1H
                result := (ioRLDR1.high and $FF);
            end;
            $18: begin   //portFRC
                result := (ioFRC and $FF);
            end;
            $20: begin   //portSAR0L
                result := (ioSAR0.low and $FF);
            end;
            $21: begin   //portSAR0H
                result := (ioSAR0.high and $FF);
            end;
            $22: begin   //portSAR0B
                result := (ioSAR0.bank and $0F);
            end;
            $23: begin   //portDAR0L
                result := (ioDAR0.low and $FF);
            end;
            $24: begin   //portDAR0H
                result := (ioDAR0.high and $FF);
            end;
            $25: begin   //portDAR0B
                result := (ioDAR0.bank and $0F);
            end;
            $26: begin   //portBCR0L
                result := (ioBCR0.low and $FF);
            end;
            $27: begin   //portBCR0H
                result := (ioBCR0.high and $FF);
            end;
            $28: begin   //portMAR1L
                result := (ioMAR1.low and $FF);
            end;
            $29: begin   //portMAR1H
                result := (ioMAR1.high and $FF);
            end;
            $2A: begin   //portMAR1B
                result := (ioMAR1.bank and $0F);
            end;
            $2B: begin   //portIAR1L
                result := (ioIAR1.low and $FF);
            end;
            $2C: begin   //portIAR1H
                result := (ioIAR1.high and $FF);
            end;
            $2E: begin   //portBCR1L
                result := (ioBCR1.low and $FF);
            end;
            $2F: begin   //portBCR1H
                result := (ioBCR1.high and $FF);
            end;
            $30: begin   //portDSTAT
                result := (ioDSTAT.Value and $CD);
            end;
            $31: begin   //portDMODE
                result := (ioDMODE.Value and $3E);
            end;
            $32: begin   //portDCNTL
                result := (ioDCNTL.Value and $FF);
            end;
            $33: begin   //portIL
                result := (ioIL and $E0);
            end;
            $34: begin   //portITC
                result := (ioITC.Value and $C7);
            end;
            $36: begin   //portRCR
                result := (ioRCR.Value and $C3);
            end;
            $38: begin   //portCBR
                result := (ioCBR and $FF);
            end;
            $39: begin   //portBBR
                result := (ioBBR and $FF);
            end;
            $3A: begin   //portCBAR
                result := (ioCBAR and $FF);
            end;
            $3E: begin   //portOMCR
                result := (ioOMCR.Value and $A0);
            end;
            $3F: begin   //portICR
                result := (ioICR.Value and $E0);
            end;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.ioWrite(const portHI: byte; const portLO: byte; const Data: byte);
begin
    extraWaitCycles := extraWaitCycles + ioWaitCycles;
    SystemInOut.cpuIoWrite(((portHI shl 8) + portLO), Data);
    if ((portLo <= (ioICR.Value and IOAsel) + $3F) and (portLo >= (ioICR.Value and IOAsel)) and (portHi = $00)) then begin
        case (portLO and $3F) of
            $00: begin   //portCNTLA0
                ioCNTLA0.Value := ((ioCNTLA0.Value and not $FF) or (Data and $FF));
                shiftModeRatio0 := asciTransLength[ioCNTLA0.Value and MODSEL]; // bei Aenderungen 'shiftModeRatio' neu bestimmen
                if (not ioCNTLA0.bit[MPBR_EFR]) then begin    // wenn Bit MPBR_EFR mit 0 beschrieben wird
                    ioSTAT0.bit[OVRN] := False;   // in STAT0 Bits OVRN,PE und FE loeschen
                    ioSTAT0.bit[PE] := False;
                    ioSTAT0.bit[FE] := False;
                    ioCNTLA0.bit[MPBR_EFR] := True;   // und Bit MPBR_EFR wieder auf 1 setzen
                end;
            end;
            $01: begin   //portCNTLA1
                ioCNTLA1.Value := ((ioCNTLA1.Value and not $FF) or (Data and $FF));
                shiftModeRatio1 := asciTransLength[ioCNTLA1.Value and MODSEL]; // bei Aenderungen 'shiftModeRatio' neu bestimmen
                if (not ioCNTLA1.bit[MPBR_EFR]) then begin    // wenn Bit MPBR_EFR mit 0 beschrieben wird
                    ioSTAT1.bit[OVRN] := False;   // in STAT0 Bits OVRN,PE und FE loeschen
                    ioSTAT1.bit[PE] := False;
                    ioSTAT1.bit[FE] := False;
                    ioCNTLA1.bit[MPBR_EFR] := True;   // und Bit MPBR_EFR wieder auf 1 setzen
                end;
            end;
            $02: begin   //portCNTLB0
                ioCNTLB0.Value := ((ioCNTLB0.Value and not $FF) or (Data and $FF));
                asciPhiDevideRatio0 := calcAsciPhiDevide(ioCNTLB0.Value);   // bei Aenderungen die 'Baud-Rate' neu berechnen
            end;
            $03: begin   //portCNTLB1
                ioCNTLB1.Value := ((ioCNTLB1.Value and not $FF) or (Data and $FF));
                asciPhiDevideRatio1 := calcAsciPhiDevide(ioCNTLB1.Value);   // bei Aenderungen die 'Baud-Rate' neu berechnen
            end;
            $04: begin   //portSTAT0
                ioSTAT0.Value := ((ioSTAT0.Value and not $09) or (Data and $09));
            end;
            $05: begin   //portSTAT1
                ioSTAT1.Value := ((ioSTAT1.Value and not $0D) or (Data and $0D));
            end;
            $06: begin   //portTDR0
                ioTDR0 := ((ioTDR0 and not $FF) or (Data and $FF));
                ioSTAT0.bit[TDRE] := False;  // und Transmit Data Register Empty loeschen
            end;
            $07: begin   //portTDR1
                ioTDR1 := ((ioTDR1 and not $FF) or (Data and $FF));
                ioSTAT1.bit[TDRE] := False;  // und Transmit Data Register Empty loeschen
            end;
            $08: begin   //portRDR0
                ioRDR0 := ((ioRDR0 and not $FF) or (Data and $FF));
            end;
            $09: begin   //portRDR1
                ioRDR1 := ((ioRDR1 and not $FF) or (Data and $FF));
            end;
            $0A: begin   //portCNTR
                ioCNTR.Value := ((ioCNTR.Value and not $77) or (Data and $77));
            end;
            $0B: begin   //portTRD
                ioTRD := ((ioTRD and not $FF) or (Data and $FF));
            end;
            $0C: begin   //portTMDR0L
                ioTMDR0.low := ((ioTMDR0.low and not $FF) or (Data and $FF));
            end;
            $0D: begin   //portTMDR0H
                ioTMDR0.high := ((ioTMDR0.high and not $FF) or (Data and $FF));
            end;
            $0E: begin   //portRLDR0L
                ioRLDR0.low := ((ioRLDR0.low and not $FF) or (Data and $FF));
            end;
            $0F: begin   //portRLDR0H
                ioRLDR0.high := ((ioRLDR0.high and not $FF) or (Data and $FF));
            end;
            $10: begin   //portTCR
                ioTCR.Value := ((ioTCR.Value and not $3F) or (Data and $3F));
            end;
            $14: begin   //portTMDR1L
                ioTMDR1.low := ((ioTMDR1.low and not $FF) or (Data and $FF));
            end;
            $15: begin   //portTMDR1H
                ioTMDR1.high := ((ioTMDR1.high and not $FF) or (Data and $FF));
            end;
            $16: begin   //portRLDR1L
                ioRLDR1.low := ((ioRLDR1.low and not $FF) or (Data and $FF));
            end;
            $17: begin   //portRLDR1H
                ioRLDR1.high := ((ioRLDR1.high and not $FF) or (Data and $FF));
            end;
            $18: begin   //portFRC
                ioFRC := ((ioFRC and not $00) or (Data and $00));
            end;
            $20: begin   //portSAR0L
                ioSAR0.low := ((ioSAR0.low and not $FF) or (Data and $FF));
            end;
            $21: begin   //portSAR0H
                ioSAR0.high := ((ioSAR0.high and not $FF) or (Data and $FF));
            end;
            $22: begin   //portSAR0B
                ioSAR0.bank := ((ioSAR0.bank and not $0F) or (Data and $0F));
            end;
            $23: begin   //portDAR0L
                ioDAR0.low := ((ioDAR0.low and not $FF) or (Data and $FF));
            end;
            $24: begin   //portDAR0H
                ioDAR0.high := ((ioDAR0.high and not $FF) or (Data and $FF));
            end;
            $25: begin   //portDAR0B
                ioDAR0.bank := ((ioDAR0.bank and not $0F) or (Data and $0F));
            end;
            $26: begin   //portBCR0L
                ioBCR0.low := ((ioBCR0.low and not $FF) or (Data and $FF));
            end;
            $27: begin   //portBCR0H
                ioBCR0.high := ((ioBCR0.high and not $FF) or (Data and $FF));
            end;
            $28: begin   //portMAR1L
                ioMAR1.low := ((ioMAR1.low and not $FF) or (Data and $FF));
            end;
            $29: begin   //portMAR1H
                ioMAR1.high := ((ioMAR1.high and not $FF) or (Data and $FF));
            end;
            $2A: begin   //portMAR1B
                ioMAR1.bank := ((ioMAR1.bank and not $0F) or (Data and $0F));
            end;
            $2B: begin   //portIAR1L
                ioIAR1.low := ((ioIAR1.low and not $FF) or (Data and $FF));
            end;
            $2C: begin   //portIAR1H
                ioIAR1.high := ((ioIAR1.high and not $FF) or (Data and $FF));
            end;
            $2E: begin   //portBCR1L
                ioBCR1.low := ((ioBCR1.low and not $FF) or (Data and $FF));
            end;
            $2F: begin   //portBCR1H
                ioBCR1.high := ((ioBCR1.high and not $FF) or (Data and $FF));
            end;
            $30: begin   //portDSTAT
                ioDSTAT.Value := ((ioDSTAT.Value and not $FC) or (Data and $FC));
                if ((ioDSTAT.bit[DE0]) and (not ioDSTAT.bit[DWE0])) then begin
                    case (ioDMODE.Value and (DMsel or SMsel)) of
                        $0C, $1C: begin // 64k-I/O SAR0 to Memory DAR0 transfer
                            if (ioDCNTL.bit[DMS0] and (ioSAR0.low = $08) and ((ioSAR0.bank and $03) = $01)) then begin
                                asciDmaMode := ASCI0RECEIVE;
                            end;
                            if (ioDCNTL.bit[DMS0] and (ioSAR0.low = $09) and ((ioSAR0.bank and $03) = $02)) then begin
                                asciDmaMode := ASCI1RECEIVE;
                            end;
                        end;
                        $30, $34: begin // Memory SAR0 to 64k-I/O DAR0 transfer
                            if (ioDCNTL.bit[DMS0] and (ioDAR0.low = $06) and ((ioDAR0.bank and $03) = $01)) then begin
                                asciDmaMode := ASCI0SEND;
                            end;
                            if (ioDCNTL.bit[DMS0] and (ioDAR0.low = $07) and ((ioDAR0.bank and $03) = $02)) then begin
                                asciDmaMode := ASCI1SEND;
                            end;
                        end;
                    end;
                    ioDSTAT.bit[DME] := True;
                    ioDSTAT.bit[DWE0] := True;
                end;
                if ((ioDSTAT.bit[DE1]) and (not ioDSTAT.bit[DWE1])) then begin
                    ioDSTAT.bit[DME] := True;
                    ioDSTAT.bit[DWE1] := True;
                end;
            end;
            $31: begin   //portDMODE
                ioDMODE.Value := ((ioDMODE.Value and not $3E) or (Data and $3E));
                // DMA Burst-Mode kann nur aktiv werden, wenn Memory to Memory Transfer gesetzt.
                // um0050.pdf Seite 97
                if (ioDMODE.bit[MMOD] and not ((ioDMODE.Value and (DMsel or SMsel)) = (DMsel or SMsel))) then begin
                    dmaBurstMode := True;
                end
                else begin
                    dmaBurstMode := False;
                end;
            end;
            $32: begin   //portDCNTL
                ioDCNTL.Value := ((ioDCNTL.Value and not $FF) or (Data and $FF));
                memWaitCycles := memCycles[((ioDCNTL.Value and MWIsel) shr $06)];  // bei Aenderungen die Wait-Cycles neu setzen
                ioWaitCycles := ioCycles[((ioDCNTL.Value and IWIsel) shr $04)];    // bei Aenderungen die Wait-Cycles neu setzen
            end;
            $33: begin   //portIL
                ioIL := ((ioIL and not $E0) or (Data and $E0));
            end;
            $34: begin   //portITC
                ioITC.Value := ((ioITC.Value and not $87) or (Data and $87));
            end;
            $36: begin   //portRCR
                ioRCR.Value := ((ioRCR.Value and not $C3) or (Data and $C3));
            end;
            $38: begin   //portCBR
                ioCBR := ((ioCBR and not $FF) or (Data and $FF));
            end;
            $39: begin   //portBBR
                ioBBR := ((ioBBR and not $FF) or (Data and $FF));
            end;
            $3A: begin   //portCBAR
                ioCBAR := ((ioCBAR and not $FF) or (Data and $FF));
            end;
            $3E: begin   //portOMCR
                ioOMCR.Value := ((ioOMCR.Value and not $E0) or (Data and $E0));
            end;
            $3F: begin   //portICR
                ioICR.Value := ((ioICR.Value and not $E0) or (Data and $E0));
            end;
        end;
    end;
end;

// --------------------------------------------------------------------------------
function TZ180Cpu.calcPhysicalMemAddr(logAddr: word): DWord; inline;
var
    physAddr: DWord;
    page, baseArea, commonArea: byte;
begin
    physAddr := logAddr;
    page := ((logAddr and $F000) shr 12) and $FF;
    baseArea := (ioCBAR and $0F);
    commonArea := ((ioCBAR and $F0) shr 4);
    if (page >= baseArea) then begin
        if (page >= commonArea) then begin
            physAddr := physAddr + (ioCBR shl 12);
        end
        else begin
            physAddr := physAddr + (ioBBR shl 12);
        end;
    end;
    Result := (physAddr and $FFFFF);
end;

// --------------------------------------------------------------------------------
function TZ180Cpu.calcAsciPhiDevide(asciControlRegister: byte): dword;
var
    valuePS, valueDR: byte;
begin
    valuePS := 10;
    valueDR := 16;
    if ((asciControlRegister and $20) <> $00) then begin
        valuePS := 30;
    end;
    if ((asciControlRegister and $08) <> $00) then begin
        valueDR := 64;
    end;
    Result := ((valuePS * valueDR * asciDevide[asciControlRegister and SSsel]) and $FFFFFFFF);
end;

// --------------------------------------------------------------------------------
function TZ180Cpu.readOpCode(const addr: DWord): byte; inline;
begin
    regR := (regR + 1) and $7F; // Die letzten 7 Bit von Register R werden bei jedem OpCode Read um 1 hochgezaehlt
    extraWaitCycles := extraWaitCycles + memWaitCycles;
    Result := SystemMemory.Read(calcPhysicalMemAddr(addr));
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.doAsci0;
begin
    if (ioCNTLA0.bit[TE] and (not asciTSR0E)) then begin // ist Senden enabled und liegen Daten im TSR
        transmitShiftCount0 := transmitShiftCount0 + 1; // den Bitshift-Takt
        if (transmitShiftCount0 >= shiftModeRatio0) then begin // entsprechend des Datenformats triggern
            SystemInOut.cpuTXA0(TSR0); // und nach den entsprechenden Shift-Takten die Daten ausgeben
            asciTSR0E := True; // Flags und
            transmitShiftCount0 := 0; // Bitshift-Takt Zaehler korrigieren
        end;
    end;
    if (ioCNTLA0.bit[RE] and (not asciRSR0F) and SystemInOut.cpuCanReadRXA0()) then begin // ist Empfangen enabled, RSR leer und neue Daten verfuegbar
        receiveShiftCount0 := receiveShiftCount0 + 1; // den Bitshift-Takt
        if (receiveShiftCount0 >= shiftModeRatio0) then begin // entsprechend des Datenformats triggern
            RSR0 := SystemInOut.cpuRXA0(); // und nach den entsprechenden Shift-Takten die Daten einlesen
            asciRSR0F := True; // Flags und
            receiveShiftCount0 := 0; // Bitshift-Takt Zaehler korrigieren
        end;
    end;
    if (ioSTAT0.bit[TIE] and ioSTAT0.bit[TDRE]) then begin // wenn Transmit-Interrupts enabled und TDR leer
        intASCI0 := True; // ASCI0-Interrupt ausloesen
    end;
    if (ioSTAT0.bit[RDRF] and asciRSR0F) then begin // wenn RDR voll und RSR voll
        ioSTAT0.bit[OVRN] := True; // OVERRUN Flag setzen
    end;
    if (ioSTAT0.bit[RIE] and (ioSTAT0.bit[RDRF] or ioSTAT0.bit[OVRN])) then begin // wenn Receive-Interrupts enabled und RDRF oder OVRN gesetzt
        intASCI0 := True; // ASCI0-Interrupt ausloesen
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.doAsci1;
begin
    if (ioCNTLA1.bit[TE] and (not asciTSR1E)) then begin // ist Senden enabled und liegen Daten im TSR
        transmitShiftCount1 := transmitShiftCount1 + 1; // den Bitshift-Takt
        if (transmitShiftCount1 >= shiftModeRatio1) then begin // entsprechend des Datenformats triggern
            SystemInOut.cpuTXA1(TSR1); // und nach den entsprechenden Shift-Takten die Daten ausgeben
            asciTSR1E := True; // Flags und
            transmitShiftCount1 := 0; // Bitshift-Takt Zaehler korrigieren
        end;
    end;
    if (ioCNTLA1.bit[RE] and (not asciRSR1F) and SystemInOut.cpuCanReadRXA1()) then begin // ist Empfangen enabled, RSR leer und neue Daten verfuegbar
        receiveShiftCount1 := receiveShiftCount1 + 1; // den Bitshift-Takt
        if (receiveShiftCount1 >= shiftModeRatio1) then begin // entsprechend des Datenformats triggern
            RSR1 := SystemInOut.cpuRXA1(); // und nach den entsprechenden Shift-Takten die Daten einlesen
            asciRSR1F := True; // Flags und
            receiveShiftCount1 := 0; // Bitshift-Takt Zaehler korrigieren
        end;
    end;
    if (ioSTAT1.bit[TIE] and ioSTAT1.bit[TDRE]) then begin // wenn Transmit-Interrupts enabled und TDR leer
        intASCI1 := True; // ASCI1-Interrupt ausloesen
    end;
    if (ioSTAT1.bit[RDRF] and asciRSR1F) then begin // wenn RDR voll und RSR voll
        ioSTAT1.bit[OVRN] := True; // OVERRUN Flag setzen
    end;
    if (ioSTAT1.bit[RIE] and (ioSTAT1.bit[RDRF] or ioSTAT1.bit[OVRN])) then begin // wenn Receive-Interrupts enabled und RDRF oder OVRN gesetzt
        intASCI1 := True; // ASCI1-Interrupt ausloesen
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.doPrt0;
begin
    Dec(ioTMDR0.Value);
    if (ioTMDR0.Value = 0) then begin // Counter auf 0 ?
        ioTMDR0.Value := ioRLDR0.Value; // aus dem Reload-Register neu laden
        if (ioTCR.bit[TIE0]) then begin // und falls Interrupts eingeschaltet
            ioTCR.bit[TIF0] := True; // Channel 0 Interrupt Flag setzen
            intPRT0 := True; // und PRT-Interrupt generieren
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.doPrt1;
begin
    Dec(ioTMDR1.Value);
    if (ioTMDR1.Value = 0) then begin // Counter auf 0 ?
        ioTMDR1.Value := ioRLDR1.Value; // aus dem Reload-Register neu laden
        if (ioTCR.bit[TIE1]) then begin // und falls Interrupts eingeschaltet
            ioTCR.bit[TIF1] := True; // Channel 1 Interrupt Flag setzen
            intPRT1 := True; // und PRT-Interrupt generieren
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.doDma0;
var
    dmaTransferCount: DWord;
begin
    if (ioBCR0.Value = 0) then begin
        ioBCR0.Value := $10000; // Vorbelegung fuer vollen 64K Transfer
    end;
    if (dmaBurstMode) then begin
        dmaTransferCount := ioBCR0.Value;
    end
    else begin
        dmaTransferCount := 1;
    end;
    while (dmaTransferCount > 0) do begin
        if (ioBCR0.Value = 1) then begin
            tend0 := True; // beim letzten anstehenden DMA-Transfer TEND0 setzen
        end;
        case (ioDMODE.Value and (DMsel or SMsel)) of
            $00: begin // Memory SAR0++ to Memory DAR0++ transfer
                systemmemory.Write(ioDAR0.Value, systemmemory.Read(ioSAR0.Value));
                ioDAR0.Value := ((ioDAR0.Value + 1) and $FFFFF);
                ioSAR0.Value := ((ioSAR0.Value + 1) and $FFFFF);
                clockCycles := clockCycles + (2 * memWaitCycles);
                ioBCR0.Value := ((ioBCR0.Value - 1) and $FFFFF);
            end;
            $04: begin // Memory SAR0-- to Memory DAR0++ transfer
                systemmemory.Write(ioDAR0.Value, systemmemory.Read(ioSAR0.Value));
                ioDAR0.Value := ((ioDAR0.Value + 1) and $FFFFF);
                ioSAR0.Value := ((ioSAR0.Value - 1) and $FFFFF);
                clockCycles := clockCycles + (2 * memWaitCycles);
                ioBCR0.Value := ((ioBCR0.Value - 1) and $FFFFF);
            end;
            $08: begin // Memory SAR0 to Memory DAR0++ transfer
                if (dreq0) then begin
                    systemmemory.Write(ioDAR0.Value, systemmemory.Read(ioSAR0.Value));
                    ioDAR0.Value := ((ioDAR0.Value + 1) and $FFFFF);
                    clockCycles := clockCycles + (2 * memWaitCycles);
                    ioBCR0.Value := ((ioBCR0.Value - 1) and $FFFFF);
                    dreq0 := False;
                end;
            end;
            $0C: begin // 64k-I/O SAR0 to Memory DAR0++ transfer
                if (dreq0) then begin
                    systemmemory.Write(ioDAR0.Value, ioRead(ioSAR0.high, ioSAR0.low));
                    ioDAR0.Value := ((ioDAR0.Value + 1) and $FFFFF);
                    clockCycles := clockCycles + memWaitCycles + ioWaitCycles;
                    ioBCR0.Value := ((ioBCR0.Value - 1) and $FFFFF);
                    dreq0 := False;
                end;
            end;
            $10: begin // Memory SAR0++ to Memory DAR0-- transfer
                systemmemory.Write(ioDAR0.Value, systemmemory.Read(ioSAR0.Value));
                ioDAR0.Value := ((ioDAR0.Value - 1) and $FFFFF);
                ioSAR0.Value := ((ioSAR0.Value + 1) and $FFFFF);
                clockCycles := clockCycles + (2 * memWaitCycles);
                ioBCR0.Value := ((ioBCR0.Value - 1) and $FFFFF);
            end;
            $14: begin // Memory SAR0-- to Memory DAR0-- transfer
                systemmemory.Write(ioDAR0.Value, systemmemory.Read(ioSAR0.Value));
                ioDAR0.Value := ((ioDAR0.Value - 1) and $FFFFF);
                ioSAR0.Value := ((ioSAR0.Value - 1) and $FFFFF);
                clockCycles := clockCycles + (2 * memWaitCycles);
                ioBCR0.Value := ((ioBCR0.Value - 1) and $FFFFF);
            end;
            $18: begin // Memory SAR0 to Memory DAR0-- transfer
                if (dreq0) then begin
                    systemmemory.Write(ioDAR0.Value, systemmemory.Read(ioSAR0.Value));
                    ioDAR0.Value := ((ioDAR0.Value - 1) and $FFFFF);
                    clockCycles := clockCycles + (2 * memWaitCycles);
                    ioBCR0.Value := ((ioBCR0.Value - 1) and $FFFFF);
                    dreq0 := False;
                end;
            end;
            $1C: begin // 64k-I/O SAR0 to Memory DAR0-- transfer
                if (dreq0) then begin
                    systemmemory.Write(ioDAR0.Value, ioRead(ioSAR0.high, ioSAR0.low));
                    ioDAR0.Value := ((ioDAR0.Value - 1) and $FFFFF);
                    clockCycles := clockCycles + memWaitCycles + ioWaitCycles;
                    ioBCR0.Value := ((ioBCR0.Value - 1) and $FFFFF);
                    dreq0 := False;
                end;
            end;
            $20: begin // Memory SAR0++ to Memory DAR0 transfer
                if (dreq0) then begin
                    systemmemory.Write(ioDAR0.Value, systemmemory.Read(ioSAR0.Value));
                    ioSAR0.Value := ((ioSAR0.Value + 1) and $FFFFF);
                    clockCycles := clockCycles + (2 * memWaitCycles);
                    ioBCR0.Value := ((ioBCR0.Value - 1) and $FFFFF);
                    dreq0 := False;
                end;
            end;
            $24: begin // Memory SAR0-- to Memory DAR0 transfer
                if (dreq0) then begin
                    systemmemory.Write(ioDAR0.Value, systemmemory.Read(ioSAR0.Value));
                    ioSAR0.Value := ((ioSAR0.Value - 1) and $FFFFF);
                    clockCycles := clockCycles + (2 * memWaitCycles);
                    ioBCR0.Value := ((ioBCR0.Value - 1) and $FFFFF);
                    dreq0 := False;
                end;
            end;
            $30: begin // Memory SAR0++ to 64k-I/O DAR0 transfer
                if (dreq0) then begin
                    ioWrite(ioDAR0.high, ioDAR0.low, systemmemory.Read(ioSAR0.Value));
                    ioSAR0.Value := ((ioSAR0.Value + 1) and $FFFFF);
                    clockCycles := clockCycles + memWaitCycles + ioWaitCycles;
                    ioBCR0.Value := ((ioBCR0.Value - 1) and $FFFFF);
                    dreq0 := False;
                end;
            end;
            $34: begin // Memory SAR0-- to 64k-I/O DAR0 transfer
                if (dreq0) then begin
                    ioWrite(ioDAR0.high, ioDAR0.low, systemmemory.Read(ioSAR0.Value));
                    ioSAR0.Value := ((ioSAR0.Value - 1) and $FFFFF);
                    clockCycles := clockCycles + memWaitCycles + ioWaitCycles;
                    ioBCR0.Value := ((ioBCR0.Value - 1) and $FFFFF);
                    dreq0 := False;
                end;
            end;
        end;
        dmaTransferCount := ((dmaTransferCount - 1) and $FFFFFFFF);
    end;
    if (ioBCR0.Value = 0) then begin // aktueller DMA-Transfer beendet
        ioDSTAT.bit[DE0] := False; // DMA0-Enable loeschen
        if (not ioDSTAT.bit[DE1]) then begin // nur wenn Channel 1 nicht (noch) aktiv
            ioDSTAT.bit[DME] := False; // DMA Main Enable loeschen
        end;
        if (ioDSTAT.bit[DIE0]) then begin // und falls Interrupts eingeschaltet
            intDMA0 := True; // DMA0-Interrupt generieren
        end;
        if (not (asciDmaMode = OFF)) then begin // wenn der Transfer ein Memory <-> ASCI war
            asciDmaMode := OFF; // und den ASCI-DMA-MODE ausschalten
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.doDma1;
begin
    if (ioBCR1.Value = 0) then begin
        ioBCR1.Value := $10000; // Vorbelegung fuer vollen 64K Transfer
    end;
    if (ioBCR1.Value = 1) then begin
        tend1 := True; // beim letzten anstehenden DMA-Transfehr TEND1 setzen
    end;
    case (ioDCNTL.Value and DMsel) of
        $00: begin // Memory MAR1++ to I/O IAR1 transfer
            if (dreq1) then begin
                ioWrite(ioIAR1.high, ioIAR1.low, systemmemory.Read(ioMAR1.Value));
                ioMAR1.Value := ((ioMAR1.Value + 1) and $FFFFF);
                clockCycles := clockCycles + memWaitCycles + ioWaitCycles;
                ioBCR1.Value := ((ioBCR1.Value - 1) and $FFFFF);
                dreq1 := False;
            end;
        end;
        $01: begin // Memory MAR1-- to I/O IAR1 transfer
            if (dreq1) then begin
                ioWrite(ioIAR1.high, ioIAR1.low, systemmemory.Read(ioMAR1.Value));
                ioMAR1.Value := ((ioMAR1.Value - 1) and $FFFFF);
                clockCycles := clockCycles + memWaitCycles + ioWaitCycles;
                ioBCR1.Value := ((ioBCR1.Value - 1) and $FFFFF);
                dreq1 := False;
            end;
        end;
        $02: begin // I/O IAR1 to Memory MAR1++ transfer
            if (dreq1) then begin
                systemmemory.Write(ioMAR1.Value, ioRead(ioIAR1.high, ioIAR1.low));
                ioMAR1.Value := ((ioMAR1.Value + 1) and $FFFFF);
                clockCycles := clockCycles + memWaitCycles + ioWaitCycles;
                ioBCR1.Value := ((ioBCR1.Value - 1) and $FFFFF);
                dreq1 := False;
            end;
        end;
        $03: begin // I/O IAR1 to Memory MAR1-- transfer
            if (dreq1) then begin
                systemmemory.Write(ioMAR1.Value, ioRead(ioIAR1.high, ioIAR1.low));
                ioMAR1.Value := ((ioMAR1.Value - 1) and $FFFFF);
                clockCycles := clockCycles + memWaitCycles + ioWaitCycles;
                ioBCR1.Value := ((ioBCR1.Value - 1) and $FFFFF);
                dreq1 := False;
            end;
        end;
    end;
    if (ioBCR1.Value = 0) then begin // aktueller DMA-Transfer beendet
        ioDSTAT.bit[DE1] := False; // DMA0-Enable loeschen
        if (not ioDSTAT.bit[DE0]) then begin // nur wenn Channel 0 nicht (noch) aktiv
            ioDSTAT.bit[DME] := False; // DMA Main Enable loeschen
        end;
        if (ioDSTAT.bit[DIE1]) then begin // und falls Interrupts eingeschaltet
            intDMA1 := True; // DMA0-Interrupt generieren
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.reset;
begin
    regI := $00;
    regR := $00;
    regPC.Value := $0000;
    regSP.Value := $0000;
    IFF1 := False;
    IFF2 := False;
    HALT := False;
    SLP := False;
    tmpHALT := False;
    tmpSLP := False;
    dmaBurstMode := False;
    dreq0 := False;
    dreq1 := False;
    tend0 := False;
    tend1 := False;
    intMode := IM0;
    asciDmaMode := OFF;

    ioCNTLA0.Value := $08;
    ioCNTLA1.Value := $08;
    ioCNTLB0.Value := $07;
    ioCNTLB1.Value := $07;
    ioSTAT0.Value := $02;
    ioSTAT1.Value := $02;
    ioTDR0 := $00;
    ioTDR1 := $00;
    ioRDR0 := $00;
    ioRDR1 := $00;
    ioCNTR.Value := $0F;
    ioTRD := $00;
    ioTMDR0.Value := $FFFF;
    ioRLDR0.Value := $FFFF;
    ioTCR.Value := $00;
    ioTMDR1.Value := $FFFF;
    ioRLDR1.Value := $FFFF;
    ioFRC := $FF;
    ioSAR0.Value := $000000;
    ioDAR0.Value := $000000;
    ioBCR0.Value := $0000;
    ioMAR1.Value := $000000;
    ioIAR1.Value := $0000;
    ioBCR1.Value := $0000;
    ioDSTAT.Value := $32;
    ioDMODE.Value := $C1;
    ioDCNTL.Value := $F0;
    ioIL := $00;
    ioITC.Value := $01;
    ioRCR.Value := $FC;
    ioCBR := $00;
    ioBBR := $00;
    ioCBAR := $F0;
    ioOMCR.Value := $FF;
    ioICR.Value := $1F;

    intTRAP := False;
    intPRT0 := False;
    intPRT1 := False;
    intDMA0 := False;
    intDMA1 := False;
    intCSIO := False;
    intASCI0 := False;
    intASCI1 := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.exec(opCount: DWord);
var
    vectorAddress: word;
begin
    while (opCount >= 1) do begin
        Dec(opCount);
        if ((not HALT) and not (ioDSTAT.bit[DME] and ioDSTAT.bit[DE0] and dmaBurstMode)) then begin
            extraWaitCycles := 0;
            execOp00Codes;
            clockCycles := clockCycles + extraWaitCycles;
        end
        else begin
            machineCycles := 1;
            clockCycles := 1;
        end;
        while (machineCycles >= 1) do begin // Zaehlschleife fuer Maschinen-Zyklen
            machineCycles := machineCycles - 1;
            if ((not SLP) and ioDSTAT.bit[DME]) then begin
                if (ioDSTAT.bit[DE0]) then begin
                    doDma0;
                end;
                if (ioDSTAT.bit[DE1]) then begin
                    doDma1;
                end;
            end;
        end;
        while (clockCycles >= 1) do begin // Zaehlschleife um den 'Systemtakt Phi' abbilden zu koennen
            clockCycles := clockCycles - 1;
            Dec(ioFRC);    // FRC (Free running counter) wird bei jedem t_state um 1 heruntergezaehlt. Z8018x Family MPU User Manual Seite 172
            if (ioCNTLA0.bit[TE] or ioCNTLA0.bit[RE]) then begin // nur wenn ASCI0 Senden oder Empfangen soll
                asciClockCount0 := asciClockCount0 + 1;    // wird der 'Takt' fuer ASCI0 gestartet
                if ((not ioSTAT0.bit[TDRE]) and asciTSR0E) then begin // ist TSR leer und liegen neue Daten im TDR
                    TSR0 := ioTDR0; // werden diese ins TSR kopiert
                    asciTSR0E := False; // und die Status-Flags entsprechend setzen
                    ioSTAT0.bit[TDRE] := True;
                    if (asciDmaMode = ASCI0SEND) then begin // falls ASCI DMA-Mode aktiv
                        dreq0 := True; // dann einen DMA-Request auslösen
                    end;
                end;
                if ((not ioSTAT0.bit[RDRF]) and asciRSR0F) then begin // ist RDR leer und liegen neue Daten im RSR
                    ioRDR0 := RSR0; // werden diese ins RDR kopier
                    asciRSR0F := False; // und die Status-Flags entsprechend setzen
                    ioSTAT0.bit[RDRF] := True;
                    if (asciDmaMode = ASCI0RECEIVE) then begin // falls ASCI DMA-Mode aktiv
                        dreq0 := True; // dann einen DMA-Request auslösen
                    end;
                end;
                if (asciClockCount0 >= asciPhiDevideRatio0) then begin // nach Ablauf der entsprechenden System-Takte
                    asciClockCount0 := 0; // wird der Baudraten-Takt getriggert
                    doAsci0;
                end;
            end;
            if (ioCNTLA1.bit[TE] or ioCNTLA1.bit[RE]) then begin// nur wenn ASCI1 Senden oder Empfangen soll
                asciClockCount1 := asciClockCount1 + 1;    // wird der 'Takt' fuer ASCI1 gestartet
                if ((not ioSTAT1.bit[TDRE]) and asciTSR1E) then begin// ist TSR leer und liegen neue Daten im TDR
                    TSR1 := ioTDR1; // werden diese ins TSR kopiert
                    asciTSR1E := False; // und die Status-Flags entsprechend setzen
                    ioSTAT1.bit[TDRE] := True;
                    if (asciDmaMode = ASCI1SEND) then begin // falls ASCI DMA-Mode aktiv
                        dreq0 := True; // dann einen DMA-Request auslösen
                    end;
                end;
                if ((not ioSTAT1.bit[RDRF]) and asciRSR1F) then begin // ist RDR leer und liegen neue Daten im RSR
                    ioRDR1 := RSR1; // werden diese ins RDR kopier
                    asciRSR1F := False; // und die Status-Flags entsprechend setzen
                    ioSTAT1.bit[RDRF] := True;
                    if (asciDmaMode = ASCI1RECEIVE) then begin // falls ASCI DMA-Mode aktiv
                        dreq0 := True; // dann einen DMA-Request auslösen
                    end;
                end;
                if (asciClockCount1 >= asciPhiDevideRatio1) then begin // nach Ablauf der entsprechenden System-Takte
                    asciClockCount1 := 0; // wird der Baudraten-Takt getriggert
                    doAsci1;
                end;
            end;
            if (ioTCR.bit[TDE0] or ioTCR.bit[TDE1]) then begin // sobald einer der beiden PRT-Kanaele aktiv ist
                Inc(prtClockCount); // wird der 'Takt' gestartet
                if (prtClockCount >= 20) then begin // nach 20 Systemtakten
                    prtClockCount := 0; // wird der PRT-Takt getriggert
                    if (ioTCR.bit[TDE0]) then begin // PRT-Channel 0 aktiv
                        doPrt0;
                    end;
                    if (ioTCR.bit[TDE1]) then begin // PRT-Channel 1 aktiv
                        doPrt1;
                    end;
                end;
            end;
        end;
        // nach jeder OpCode Ausfuehrung werden moegliche Interrupts geprueft
        // da nur interne Interrupts implementiert sind, wird auch nur diese
        // Interrupt-Behandlung abgebildet
        // Seite 81 , Abbildung 41 und Seite 82 , Tabelle 9
        if (IFF1) then begin
            if (intPRT0) then begin
                tmpHALT := False;
                tmpSLP := False;
                push(regPC.Value);
                vectorAddress := ((regI shl 8) or ((ioIL and $E0) or $04)) and $FFFF;
                regPC.low := memRead(vectorAddress);
                regPC.high := memRead(vectorAddress + 1);
                intPRT0 := False;
            end
            else if (intPRT1) then begin
                tmpHALT := False;
                tmpSLP := False;
                push(regPC.Value);
                vectorAddress := ((regI shl 8) or ((ioIL and $E0) or $06)) and $FFFF;
                regPC.low := memRead(vectorAddress);
                regPC.high := memRead(vectorAddress + 1);
                intPRT1 := False;
            end
            else if (intDMA0) then begin
                tmpHALT := False;
                push(regPC.Value);
                vectorAddress := ((regI shl 8) or ((ioIL and $E0) or $08)) and $FFFF;
                regPC.low := memRead(vectorAddress);
                regPC.high := memRead(vectorAddress + 1);
                intDMA0 := False;
            end
            else if (intDMA1) then begin
                tmpHALT := False;
                push(regPC.Value);
                vectorAddress := ((regI shl 8) or ((ioIL and $E0) or $0A)) and $FFFF;
                regPC.low := memRead(vectorAddress);
                regPC.high := memRead(vectorAddress + 1);
                intDMA1 := False;
            end
            else if (intCSIO) then begin  // CSIO-Funktion derzeit noch nicht implementiert
                tmpHALT := False;
                tmpSLP := False;
                push(regPC.Value);
                vectorAddress := ((regI shl 8) or ((ioIL and $E0) or $0C)) and $FFFF;
                regPC.low := memRead(vectorAddress);
                regPC.high := memRead(vectorAddress + 1);
                intCSIO := False;
            end
            else if (intASCI0) then begin
                tmpHALT := False;
                tmpSLP := False;
                push(regPC.Value);
                vectorAddress := ((regI shl 8) or ((ioIL and $E0) or $0E)) and $FFFF;
                regPC.low := memRead(vectorAddress);
                regPC.high := memRead(vectorAddress + 1);
                intASCI0 := False;
            end
            else if (intASCI1) then begin
                tmpHALT := False;
                tmpSLP := False;
                push(regPC.Value);
                vectorAddress := ((regI shl 8) or ((ioIL and $E0) or $10)) and $FFFF;
                regPC.low := memRead(vectorAddress);
                regPC.high := memRead(vectorAddress + 1);
                intASCI1 := False;
            end;
        end;
        HALT := tmpHALT;
        SLP := tmpSLP;
    end;
end;

// --------------------------------------------------------------------------------
function TZ180Cpu.getCoreData: TCoreData;
var
    coreData: TCoreData;
begin

    with coreData do begin
        A1 := regAF.A;
        F1 := regAF.F;
        B1 := regBC.B;
        C1 := regBC.C;
        BC1 := regBC.Value;
        BCi1 := SystemMemory.Read(calcPhysicalMemAddr(regBC.Value));
        D1 := regDE.D;
        E1 := regDE.E;
        DE1 := regDE.Value;
        DEi1 := SystemMemory.Read(calcPhysicalMemAddr(regDE.Value));
        H1 := regHL.H;
        L1 := regHL.L;
        HL1 := regHL.Value;
        HLi1 := SystemMemory.Read(calcPhysicalMemAddr(regHL.Value));
        A2 := (regAF_ and $FF00) shr 8;
        F2 := (regAF_ and $00FF);
        B2 := (regBC_ and $FF00) shr 8;
        C2 := (regBC_ and $00FF);
        BC2 := regBC_;
        BCi2 := SystemMemory.Read(calcPhysicalMemAddr(regBC_));
        D2 := (regDE_ and $FF00) shr 8;
        E2 := (regDE_ and $00FF);
        DE2 := regDE_;
        DEi2 := SystemMemory.Read(calcPhysicalMemAddr(regDE_));
        H2 := (regHL_ and $FF00) shr 8;
        L2 := (regHL_ and $00FF);
        HL2 := regHL_;
        HLi2 := SystemMemory.Read(calcPhysicalMemAddr(regHL_));
        I := regI;
        R := regR;
        IX := regIX.Value;
        IY := regIY.Value;
        SP := regSP.Value;
        SPi := systemmemory.Read(calcPhysicalMemAddr(regSP.Value));
        PC := regPC.Value;
        PCi := systemmemory.Read(calcPhysicalMemAddr(regPC.Value));
        PCmmu := calcPhysicalMemAddr(regPC.Value);
        IFF1 := IFF1;
        IFF2 := IFF2;
        intMode := intMode;
    end;

    Result := coreData;
end;

// --------------------------------------------------------------------------------
function TZ180Cpu.getIoRegData: TIoData;
var
    ioRegData: TIoData;
begin

    with ioRegData do begin
        CNTLA0 := ioCNTLA0.Value;
        CNTLA1 := ioCNTLA1.Value;
        CNTLB0 := ioCNTLB0.Value;
        CNTLB1 := ioCNTLB1.Value;
        STAT0 := ioSTAT0.Value;
        STAT1 := ioSTAT1.Value;
        TDR0 := ioTDR0;
        TDR1 := ioTDR1;
        RDR0 := ioRDR0;
        RDR1 := ioRDR1;
        CNTR := ioCNTR.Value;
        TRD := ioTRD;
        TMDR0L := ioTMDR0.low;
        TMDR0H := ioTMDR0.high;
        RLDR0L := ioRLDR0.low;
        RLDR0H := ioRLDR0.high;
        TCR := ioTCR.Value;
        TMDR1L := ioTMDR1.low;
        TMDR1H := ioTMDR1.high;
        RLDR1L := ioRLDR1.low;
        RLDR1H := ioRLDR1.high;
        FRC := ioFRC;
        SAR0L := ioSAR0.low;
        SAR0H := ioSAR0.high;
        SAR0B := ioSAR0.bank;
        DAR0L := ioDAR0.low;
        DAR0H := ioDAR0.high;
        DAR0B := ioDAR0.bank;
        BCR0L := ioBCR0.low;
        BCR0H := ioBCR0.high;
        MAR1L := ioMAR1.low;
        MAR1H := ioMAR1.high;
        MAR1B := ioMAR1.bank;
        IAR1L := ioIAR1.low;
        IAR1H := ioIAR1.high;
        BCR1L := ioBCR1.low;
        BCR1H := ioBCR1.high;
        DSTAT := ioDSTAT.Value;
        DMODE := ioDMODE.Value;
        DCNTL := ioDCNTL.Value;
        IL := ioIL;
        ITC := ioITC.Value;
        RCR := ioRCR.Value;
        CBR := ioCBR;
        BBR := ioBBR;
        CBAR := ioCBAR;
        OMCR := ioOMCR.Value;
        ICR := ioICR.Value;
    end;

    Result := ioRegData;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.setDREQ0;
begin
    dreq0 := True;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.setDREQ1;
begin
    dreq1 := True;
end;

// --------------------------------------------------------------------------------
function TZ180Cpu.getTEND0: boolean;
var
    tmpTend: boolean;
begin
    tmpTend := tend0;
    tend0 := False;
    Result := tmpTend;
end;

// --------------------------------------------------------------------------------
function TZ180Cpu.getTEND1: boolean;
var
    tmpTend: boolean;
begin
    tmpTend := tend1;
    tend1 := False;
    Result := tmpTend;
end;

// --------------------------------------------------------------------------------
function TZ180Cpu.calcParity(Value: byte): boolean; // inline;
begin
    Value := Value xor Value shr 4;
    Value := Value xor Value shr 2;
    Value := Value xor Value shr 1;
    Result := ((Value and $01) = $00);
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.inc8Bit(var Value: byte); // inline;
begin
    Value := (Value + 1);
    regAF.Flag[H] := ((Value and $0F) = $00);   // H is set if carry from bit 3; reset otherwise
    regAF.Flag[PV] := (Value = $80);            // P/V is set if r was 7FH before operation; reset otherwise
    regAF.Flag[S] := ((Value and $80) <> $00);  // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (Value = $00);             // Z is set if result is zero; reset otherwise
    regAF.Flag[N] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.dec8Bit(var Value: byte); // inline;
begin
    Value := (Value - 1);
    regAF.Flag[H] := ((Value and $0F) = $0F);   // H is set if borrow from bit 4, reset otherwise
    regAF.Flag[PV] := (Value = $7F);            // P/V is set if r was 80H before operation; reset otherwise
    regAF.Flag[S] := ((Value and $80) <> $00);  // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (Value = $00);             // Z is set if result is zero; reset otherwise
    regAF.Flag[N] := True;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.addA8Bit(const Value: byte); // inline;
var
    tmpA: byte;
begin
    tmpA := (regAF.A + Value);
    regAF.Flag[PV] := ((((regAF.A xor (not Value)) and $FFFF) and (regAF.A xor tmpA) and $80) <> $00);  // P/V is set if overflow; reset otherwise
    regAF.Flag[H] := ((((regAF.A and $0F) + (Value and $0F)) and $10) <> $00);  // H is set if carry from bit 3; reset otherwise
    regAF.Flag[S] := ((tmpA and $80) <> $00);   // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (tmpA = $00);  // Z is set if result is zero; reset otherwise
    regAF.Flag[C] := (((regAF.A + Value) and $100) <> $00);  // C is set if carry from bit 7; reset otherwise
    regAF.Flag[N] := False;
    regAF.A := tmpA;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.adcA8Bit(const Value: byte); // inline;
var
    carry: byte;
    tmpA: byte;
begin
    carry := byte(regAF.Flag[C]);
    tmpA := (regAF.A + Value + carry);
    regAF.Flag[PV] := ((((regAF.A xor (not Value)) and $FFFF) and ((regAF.A xor tmpA) and $80)) <> $00);  // P/V is set if overflow; reset otherwise
    regAF.Flag[H] := ((((regAF.A and $0F) + (Value and $0F) + carry) and $10) <> $00);  // H is set if carry from bit 3; reset otherwise
    regAF.Flag[S] := ((tmpA and $80) <> $00);  // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (tmpA = $00);  // Z is set if result is zero; reset otherwise
    regAF.Flag[C] := (((regAF.A + Value + carry) and $100) <> $00);  // C is set if carry from bit 7; reset otherwise
    regAF.Flag[N] := False;
    regAF.A := tmpA;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.subA8Bit(const Value: byte); // inline;
var
    tmp: integer;
    tmpA: byte;
begin
    tmp := (regAF.A - Value);
    tmpA := (tmp and $FF);
    regAF.Flag[H] := ((((regAF.A and $0F) - (Value and $0F)) and $10) <> $00);  // H is set if borrow from bit 4; reset otherwise
    regAF.Flag[PV] := ((((regAF.A xor Value) and (regAF.A xor tmpA)) and $80) <> $00);  // P/V is set if overflow; reset otherwise
    regAF.Flag[S] := ((tmpA and $80) <> $00);  // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (tmpA = $00);  // Z is set if result is zero; reset otherwise
    regAF.Flag[C] := ((tmp and $100) <> $00);  // C is set if borrow; reset otherwise
    regAF.Flag[N] := True;
    regAF.A := tmpA;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.sbcA8Bit(const Value: byte); // inline;
var
    carry: byte;
    tmpA: byte;
begin
    carry := byte(regAF.Flag[C]);
    tmpA := (regAF.A - Value - carry);
    regAF.Flag[H] := ((((regAF.A and $0F) - (Value and $0F) - carry) and $10) <> $00);  // H is set if borrow from bit 4; reset otherwise
    regAF.Flag[PV] := ((((regAF.A xor Value) and (regAF.A xor tmpA)) and $80) <> $00);  // P/V is set if overflow; reset otherwise
    regAF.Flag[S] := ((tmpA and $80) <> $00);  // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (tmpA = $00);  // Z is set if result is zero; reset otherwise
    regAF.Flag[C] := (((regAF.A - Value - carry) and $100) <> $00);  // C is set if borrow; reset otherwise
    regAF.Flag[N] := True;
    regAF.A := tmpA;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.andA8Bit(const Value: byte); // inline;
begin
    regAF.A := (regAF.A and Value);
    regAF.Flag[S] := ((regAF.A and $80) <> $00);  // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (regAF.A = $00);             // Z is set if result is zero; reset otherwise
    regAF.Flag[PV] := calcParity(regAF.A);        // P/V is set if parity even; reset otherwise
    regAF.Flag[H] := True;
    regAF.Flag[N] := False;
    regAF.Flag[C] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.xorA8Bit(const Value: byte); // inline;
begin
    regAF.A := (regAF.A xor Value);
    regAF.Flag[S] := ((regAF.A and $80) <> $00);  // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (regAF.A = $00);             // Z is set if result is zero; reset otherwise
    regAF.Flag[PV] := calcParity(regAF.A);        // P/V is set if parity even; reset otherwise
    regAF.Flag[H] := False;
    regAF.Flag[N] := False;
    regAF.Flag[C] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.orA8Bit(const Value: byte); // inline;
begin
    regAF.A := (regAF.A or Value);
    regAF.Flag[S] := ((regAF.A and $80) <> $00);  // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (regAF.A = $00);             // Z is set if result is zero; reset otherwise
    regAF.Flag[PV] := calcParity(regAF.A);        // P/V is set if parity even; reset otherwise
    regAF.Flag[H] := False;
    regAF.Flag[N] := False;
    regAF.Flag[C] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.cpA8Bit(const Value: byte); // inline;
var
    test: byte;
begin
    test := (regAF.A - Value);
    regAF.Flag[S] := ((test and $80) <> $00);  // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (test = $00);  // Z is set if result is zero; reset otherwise
    regAF.Flag[H] := ((((regAF.A and $0F) - (Value and $0F)) and $10) <> $00);  // H is set if borrow from bit 4; reset otherwise
    regAF.Flag[PV] := (((regAF.A xor Value) and (regAF.A xor test) and $80) <> $00);  // P/V is set if overflow; reset otherwise
    regAF.Flag[C] := ((regAF.A - Value) < $00);  // C is set if borrow; reset otherwise
    regAF.Flag[N] := True;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.rlc8Bit(var Value: byte); // inline;
begin
    regAF.Flag[C] := ((Value and $80) <> $00); // C is data from bit 7 of source register
    Value := ((Value shl 1) or byte(regAF.Flag[C]));
    regAF.Flag[S] := ((Value and $80) <> $00); // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (Value = $00);            // Z is set if result is zero; reset otherwise
    regAF.Flag[PV] := calcParity(Value);       // P/V is set if parity even; reset otherwise
    regAF.Flag[H] := False;
    regAF.Flag[N] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.rrc8Bit(var Value: byte); // inline;
begin
    regAF.Flag[C] := ((Value and $01) <> $00); // C is data from bit 0 of source register
    Value := ((Value shr 1) or (byte(regAF.Flag[C]) shl 7));
    regAF.Flag[S] := ((Value and $80) <> $00); // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (Value = $00);            // Z is set if result is zero; reset otherwise
    regAF.Flag[PV] := calcParity(Value);       // P/V is set if parity even; reset otherwise
    regAF.Flag[H] := False;
    regAF.Flag[N] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.rl8Bit(var Value: byte); // inline;
var
    tmpCarry: boolean;
begin
    tmpCarry := regAF.Flag[C];
    regAF.Flag[C] := ((Value and $80) <> $00); // C is data from bit 7 of source register
    Value := ((Value shl 1) or byte(tmpCarry));
    regAF.Flag[S] := ((Value and $80) <> $00); // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (Value = $00);            // Z is set if result is zero; reset otherwise
    regAF.Flag[PV] := calcParity(Value);       // P/V is set if parity even; reset otherwise
    regAF.Flag[H] := False;
    regAF.Flag[N] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.rr8Bit(var Value: byte); // inline;
var
    tmpCarry: boolean;
begin
    tmpCarry := regAF.Flag[C];
    regAF.Flag[C] := ((Value and $01) <> $00); // C is data from bit 0 of source register
    Value := ((Value shr 1) or (byte(tmpCarry) shl 7));
    regAF.Flag[S] := ((Value and $80) <> $00); // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (Value = $00);            // Z is set if result is zero; reset otherwise
    regAF.Flag[PV] := calcParity(Value);       // P/V is set if parity even; reset otherwise
    regAF.Flag[H] := False;
    regAF.Flag[N] := False;

end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.sla8Bit(var Value: byte); // inline;
begin
    regAF.Flag[C] := ((Value and $80) <> $00); // C is data from bit 7 of source register
    Value := (Value shl 1);
    regAF.Flag[S] := ((Value and $80) <> $00); // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (Value = $00);            // Z is set if result is zero; reset otherwise
    regAF.Flag[PV] := calcParity(Value);       // P/V is set if parity even; reset otherwise
    regAF.Flag[H] := False;
    regAF.Flag[N] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.sra8Bit(var Value: byte); // inline;
var
    tmpValue: byte;
begin
    regAF.Flag[C] := ((Value and $01) <> $00);  // C is data from bit 0 of source register
    tmpValue := ((Value shr 1) or (Value and $80));
    regAF.Flag[PV] := calcParity(tmpValue);  // P/V is set if parity even; reset otherwise
    regAF.Flag[S] := ((tmpValue and $80) <> $00);  // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (tmpValue = $00);  // Z is set if result is zero; reset otherwise
    regAF.Flag[H] := False;
    regAF.Flag[N] := False;
    Value := tmpValue;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.srl8Bit(var Value: byte); // inline;
begin
    regAF.Flag[C] := ((Value and $01) <> $00); // C is data from bit 0 of source register
    Value := (Value shr 1);
    regAF.Flag[S] := ((Value and $80) <> $00); // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (Value = $00);            // Z is set if result is zero; reset otherwise
    regAF.Flag[PV] := calcParity(Value);       // P/V is set if parity even; reset otherwise
    regAF.Flag[H] := False;
    regAF.Flag[N] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.tstBit(const Bit: byte; const Value: byte); // inline;
begin
    regAF.Flag[Z] := (Value and (1 shl Bit) = $00);  // Z is set if specified bit is 0; reset otherwise
    regAF.Flag[H] := True;
    regAF.Flag[N] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.resBit(const Bit: byte; var Value: byte); // inline;
begin
    Value := (Value and not (1 shl Bit));
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.setBit(const Bit: byte; var Value: byte); // inline;
begin
    Value := (Value or (1 shl Bit));
end;

// -------------------------------------------------------------------------------
procedure TZ180Cpu.tstA8Bit(const Value: byte); // inline;
var
    tmpByte: byte;
begin
    tmpByte := (regAF.A and Value);
    regAF.Flag[S] := ((tmpByte and $80) <> $00); // S is set if the result is negative; reset otherwise
    regAF.Flag[Z] := (tmpByte = $00);            // Z is set if the result is zero; reset otherwise
    regAF.Flag[PV] := calcParity(tmpByte);       // P/V is set if parity is even; reset otherwise
    regAF.Flag[H] := True;
    regAF.Flag[N] := False;
    regAF.Flag[C] := False;
end;

// --------------------------------------------------------------------------------
function TZ180Cpu.inreg8Bit(const portHi: byte; const portLo: byte): byte; // inline;
var
    tmpByte: byte;
begin
    tmpByte := ioRead(portHi, portLo);
    regAF.Flag[S] := ((tmpByte and $80) <> $00); // S is set if input data is negative; reset otherwise
    regAF.Flag[Z] := (tmpByte = $00);            // Z is set if input data is zero; reset otherwise
    regAF.Flag[PV] := calcParity(tmpByte);       // P/V is set if parity is even; reset otherwise
    regAF.Flag[H] := False;
    regAF.Flag[N] := False;
    Result := tmpByte;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.addHL16Bit(const Value: word); // inline;
begin
    regAF.Flag[H] := ((regHL.Value and $0FFF) + (Value and $0FFF) > $0FFF); // H is set if carry out of bit 11; reset otherwise
    regAF.Flag[C] := ((regHL.Value + Value) > $FFFF);                       // C is set if carry from bit 15; reset otherwise
    regHL.Value := (regHL.Value + Value);
    regAF.Flag[N] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.adcHL16Bit(const Value: word); // inline;
var
    carry: byte;
    tmpHL: dword;
begin
    carry := byte(regAF.Flag[C]);
    tmpHL := (regHL.Value + Value + carry);
    regAF.Flag[S] := ((tmpHL and $8000) <> $00);  // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (tmpHL = $0000);  // Z is set if result is zero; reset otherwise
    regAF.Flag[C] := ((tmpHL and $10000) <> $00);  // C is set if carry from bit 15; reset otherwise
    regAF.Flag[PV] := (((regHL.Value xor ((not Value) and $FFFF)) and (regHL.Value xor tmpHL) and $8000) <> $00);  // P/V is set if overflow; reset otherwise
    regAF.Flag[H] := ((((regHL.Value and $0FFF) + (Value and $0FFF) + carry) and $1000) <> $00);  // H is set if carry out of bit 11; reset otherwise
    regAF.Flag[N] := False;
    regHL.Value := tmpHL;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.sbcHL16Bit(const Value: word); // inline;
var
    carry: byte;
    tmpHL: dword;
begin
    carry := byte(regAF.Flag[C]);
    tmpHL := (regHL.Value - Value - carry);
    regAF.Flag[S] := ((tmpHL and $8000) <> $00);  // S is set if result is negative; reset otherwise
    regAF.Flag[Z] := (tmpHL = $0000);  // Z is set if result is zero; reset otherwise
    regAF.Flag[C] := ((tmpHL and $10000) <> $00);  // C is set if borrow; reset otherwise
    regAF.Flag[PV] := (((regHL.Value xor Value) and (regHL.Value xor tmpHL) and $8000) <> $00);  // P/V is set if overflow; reset otherwise
    regAF.Flag[H] := ((((regHL.Value and $0FFF) - (Value and $0FFF) - carry) and $1000) <> $00);  // H is set if borrow from bit 12; reset otherwise
    regAF.Flag[N] := True;
    regHL.Value := tmpHL and $FFFF;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.addIX16Bit(const Value: word); // inline;
begin
    regAF.Flag[H] := ((regIX.Value and $0FFF) + (Value and $0FFF) > $0FFF); // H is set if carry out of bit 11; reset otherwise
    regAF.Flag[C] := ((regIX.Value + Value) > $FFFF);                       // C is set if carry from bit 15; reset otherwise
    regIX.Value := (regIX.Value + Value);
    regAF.Flag[N] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.addIY16Bit(const Value: word); // inline;
begin
    regAF.Flag[H] := ((regIY.Value and $0FFF) + (Value and $0FFF) > $0FFF); // H is set if carry out of bit 11; reset otherwise
    regAF.Flag[C] := ((regIY.Value + Value) > $FFFF);                       // C is set if carry from bit 15; reset otherwise
    regIY.Value := (regIY.Value + Value);
    regAF.Flag[N] := False;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.push(const Value: word); inline;
begin
    Dec(regSP.Value);
    memWrite(regSP.Value, ((Value and $FF00) shr 8));
    Dec(regSP.Value);
    memWrite(regSP.Value, (Value and $00FF));
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.pop(var Value: word); inline;
var
    popLow, popHigh: byte;
begin
    popLow := memRead(regSP.Value);
    Inc(regSP.Value);
    popHigh := memRead(regSP.Value);
    Inc(regSP.Value);
    Value := (popHigh shl 8) + popLow;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.execOp00Codes;
var
    opCode, jmpOffset, tmpByte: byte;
    tmpWord: Treg16;
    tmpFlag: boolean;
begin
    opCode := readOpCode(regPC.Value);
    Inc(regPC.Value);
    case (opCode) of
        $00: begin   // OP-Code 0x00 : NOP
            machineCycles := 1;
            clockCycles := 3;
        end;
        $01: begin   // OP-Code 0x01 : LD BC,nn;
            regBC.C := memRead(regPC.Value);
            Inc(regPC.Value);
            regBC.B := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $02: begin   // OP-Code 0x02 : LD (BC),A
            memWrite(regBC.Value, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $03: begin   // OP-Code 0x03 : INC BC
            Inc(regBC.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $04: begin   // OP-Code 0x04 : INC B
            inc8Bit(regBC.B);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $05: begin   // OP-Code 0x05 : DEC B
            dec8Bit(regBC.B);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $06: begin   // OP-Code 0x06 : LD B,n
            regBC.B := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $07: begin   // OP-Code 0x07 : RLCA
            regAF.Flag[C] := ((regAF.A and $80) <> $00);
            regAF.A := ((regAF.A shl 1) or byte(regAF.Flag[C]));
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $08: begin   // OP-Code 0x08 : EX AF,AF'
            tmpWord.Value := regAF.Value;
            regAF.Value := regAF_;
            regAF_ := tmpWord.Value;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $09: begin   // OP-Code 0x09 : ADD HL,BC
            addHL16Bit(regBC.Value);
            machineCycles := 5;
            clockCycles := 7;
        end;
        $0A: begin   // OP-Code 0x0A : LD A,(BC)
            regAF.A := memRead(regBC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $0B: begin   // OP-Code 0x0B : DEC BC
            Dec(regBC.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $0C: begin   // OP-Code 0x0C : INC C
            inc8Bit(regBC.C);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $0D: begin   // OP-Code 0x0D : DEC C
            dec8Bit(regBC.C);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $0E: begin   // OP-Code 0x0E : LD C,n
            regBC.C := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $0F: begin   // OP-Code 0x0F : RRCA
            regAF.Flag[C] := ((regAF.A and $01) <> $00);
            regAF.A := ((regAF.A shr 1) or (byte(regAF.Flag[C]) shl 7));
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $10: begin   // OP-Code 0x10 : DJNZ d
            jmpOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            Dec(regBC.B);
            if (regBC.B <> $00) then begin
                regPC.Value := regPC.Value + shortint(jmpOffset);
                machineCycles := 5;
                clockCycles := 9;
            end
            else begin
                machineCycles := 3;
                clockCycles := 7;
            end;
        end;
        $11: begin   // OP-Code 0x11 : LD DE,nn
            regDE.E := memRead(regPC.Value);
            Inc(regPC.Value);
            regDE.D := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $12: begin   // OP-Code 0x12 : LD (DE),A
            memWrite(regDE.Value, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $13: begin   // OP-Code 0x13 : INC DE
            Inc(regDE.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $14: begin   // OP-Code 0x14 : INC D
            inc8Bit(regDE.D);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $15: begin   // OP-Code 0x15 : DEC D
            dec8Bit(regDE.D);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $16: begin   // OP-Code 0x16 : LD D,n
            regDE.D := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $17: begin   // OP-Code 0x17 : RLA
            tmpFlag := ((regAF.A and $80) <> $00);
            regAF.A := ((regAF.A shl 1) or byte(regAF.Flag[C]));
            regAF.Flag[C] := tmpFlag;
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $18: begin   // OP-Code 0x18 : JR d
            jmpOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regPC.Value := regPC.Value + shortint(jmpOffset);
            machineCycles := 4;
            clockCycles := 8;
        end;
        $19: begin   // OP-Code 0x19 : ADD HL,DE
            addHL16Bit(regDE.Value);
            machineCycles := 5;
            clockCycles := 7;
        end;
        $1A: begin   // OP-Code 0x1A : LD A,(DE)
            regAF.A := memRead(regDE.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $1B: begin   // OP-Code 0x1B : DEC DE
            Dec(regDE.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $1C: begin   // OP-Code 0x1C : INC E
            inc8Bit(regDE.E);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $1D: begin   // OP-Code 0x1D : DEC E
            dec8Bit(regDE.E);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $1E: begin   // OP-Code 0x1E : LD E,n
            regDE.E := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $1F: begin   // OP-Code 0x1F : RRA
            tmpFlag := ((regAF.A and $01) <> $00);
            regAF.A := ((regAF.A shr 1) or (byte(regAF.Flag[C]) shl 7));
            regAF.Flag[C] := tmpFlag;
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $20: begin   // OP-Code 0x20 : JR NZ,d
            jmpOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[Z] = False) then begin
                regPC.Value := regPC.Value + shortint(jmpOffset);
                machineCycles := 4;
                clockCycles := 8;
            end
            else begin
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $21: begin   // OP-Code 0x21 : LD HL,nn
            regHL.L := memRead(regPC.Value);
            Inc(regPC.Value);
            regHL.H := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $22: begin   // OP-Code 0x22 : LD (nn),HL
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(tmpWord.Value, regHL.L);
            memWrite(tmpWord.Value + 1, regHL.H);
            machineCycles := 6;
            clockCycles := 16;
        end;
        $23: begin   // OP-Code 0x23 : INC HL
            Inc(regHL.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $24: begin   // OP-Code 0x24 : INC H
            inc8Bit(regHL.H);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $25: begin   // OP-Code 0x25 : DEC H
            dec8Bit(regHL.H);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $26: begin   // OP-Code 0x26 : LD H,n
            regHL.H := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $27: begin   // OP-Code 0x27 : DAA
            //NOTE: original Z80 DAA implementation mit Carry Flag.
            //      Die Z180CPU verarbeitet das Carry Flag nicht!!!
            tmpByte := $00;
            if (regAF.Flag[H] or ((regAF.A and $0F) > $09)) then begin
                tmpByte := tmpByte or $06;
            end;
            if (regAF.Flag[C] or (regAF.A > $9F)) then begin
                tmpByte := tmpByte or $60;
            end;
            if ((regAF.A > $8F) and ((regAF.A and $0F) > $09)) then begin
                tmpByte := tmpByte or $60;
            end;
            if (regAF.A > $99) then begin
                regAF.Flag[C] := True;
            end;
            if (regAF.Flag[N]) then begin
                regAF.Flag[H] := ((((regAF.A and $0F) - (tmpByte and $0f)) and $10) <> $00);
                regAF.A := (regAF.A - tmpByte);
            end
            else begin
                regAF.Flag[H] := ((((regAF.A and $0F) + (tmpByte and $0F)) and $10) <> $00);
                regAF.A := (regAF.A + tmpByte);
            end;
            regAF.Flag[PV] := calcParity(regAF.A);
            regAF.Flag[S] := ((regAF.A and $80) <> $00);
            regAF.Flag[Z] := (regAF.A = $00);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $28: begin   // OP-Code 0x28 : JR Z,d
            jmpOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[Z] = True) then begin
                regPC.Value := regPC.Value + shortint(jmpOffset);
                machineCycles := 4;
                clockCycles := 8;
            end
            else begin
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $29: begin   // OP-Code 0x29 : ADD HL,HL
            addHL16Bit(regHL.Value);
            machineCycles := 5;
            clockCycles := 7;
        end;
        $2A: begin   // OP-Code 0x2A : LD HL,(nn)
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            regHL.L := memRead(tmpWord.Value);
            regHL.H := memRead(tmpWord.Value + 1);
            machineCycles := 5;
            clockCycles := 15;
        end;
        $2B: begin   // OP-Code 0x2B : DEC HL
            Dec(regHL.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $2C: begin   // OP-Code 0x2C : INC L
            inc8Bit(regHL.L);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $2D: begin   // OP-Code 0x2D : DEC L
            dec8Bit(regHL.L);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $2E: begin   // OP-Code 0x2E : LD L,n
            regHL.L := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $2F: begin   // OP-Code 0x2F : CPL
            regAF.A := (regAF.A xor $FF);
            regAF.Flag[H] := True;
            regAF.Flag[N] := True;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $30: begin   // OP-Code 0x30 : JR NC,d
            jmpOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[C] = False) then begin
                regPC.Value := regPC.Value + shortint(jmpOffset);
                machineCycles := 4;
                clockCycles := 8;
            end
            else begin
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $31: begin   // OP-Code 0x31 : LD SP,nn
            regSP.low := memRead(regPC.Value);
            Inc(regPC.Value);
            regSP.high := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $32: begin   // OP-Code 0x32 : LD (nn),A
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(tmpWord.Value, regAF.A);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $33: begin   // OP-Code 0x33 : INC SP
            Inc(regSP.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $34: begin   // OP-Code 0x34 : INC (HL)
            tmpByte := memRead(regHL.Value);
            inc8Bit(tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 4;
            clockCycles := 10;
        end;
        $35: begin   // OP-Code 0x35 : DEC (HL)
            tmpByte := memRead(regHL.Value);
            dec8Bit(tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 4;
            clockCycles := 10;
        end;
        $36: begin   // OP-Code 0x36 : LD (HL),n
            memWrite(regHL.Value, memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $37: begin   // OP-Code 0x37 : SCF
            regAF.Flag[C] := True;
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $38: begin   // OP-Code 0x38 : JR C,d
            jmpOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[C] = True) then begin
                regPC.Value := regPC.Value + shortint(jmpOffset);
                machineCycles := 4;
                clockCycles := 8;
            end
            else begin
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $39: begin   // OP-Code 0x39 : ADD HL,SP
            addHL16Bit(regSP.Value);
            machineCycles := 5;
            clockCycles := 7;
        end;
        $3A: begin   // OP-Code 0x3A : LD A,(nn)
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            regAF.A := memRead(tmpWord.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $3B: begin   // OP-Code 0x3B : DEC SP
            Dec(regSP.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $3C: begin   // OP-Code 0x3C : INC A
            inc8Bit(regAF.A);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $3D: begin   // OP-Code 0x3D : DEC A
            dec8Bit(regAF.A);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $3E: begin   // OP-Code 0x3E : LD A,n
            regAF.A := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $3F: begin   // OP-Code 0x3F : CCF
            regAF.Flag[H] := regAF.Flag[C];
            regAF.Flag[C] := not regAF.Flag[C];
            regAF.Flag[N] := False;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $40: begin   // OP-Code 0x40 : LD B,B
            regBC.B := regBC.B;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $41: begin   // OP-Code 0x41 : LD B,C
            regBC.B := regBC.C;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $42: begin   // OP-Code 0x42 : LD B,D
            regBC.B := regDE.D;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $43: begin   // OP-Code 0x43 : LD B,E
            regBC.B := regDE.E;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $44: begin   // OP-Code 0x44 : LD B,H
            regBC.B := regHL.H;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $45: begin   // OP-Code 0x45 : LD B,L
            regBC.B := regHL.L;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $46: begin   // OP-Code 0x46 : LD B,(HL)
            regBC.B := memRead(regHL.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $47: begin   // OP-Code 0x47 : LD B,A
            regBC.B := regAF.A;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $48: begin   // OP-Code 0x48 : LD C,B
            regBC.C := regBC.B;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $49: begin   // OP-Code 0x49 : LD C,C
            regBC.C := regBC.C;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $4A: begin   // OP-Code 0x4A : LD C,D
            regBC.C := regDE.D;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $4B: begin   // OP-Code 0x4B : LD C,E
            regBC.C := regDE.E;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $4C: begin   // OP-Code 0x4C : LD C,H
            regBC.C := regHL.H;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $4D: begin   // OP-Code 0x4D : LD C,L
            regBC.C := regHL.L;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $4E: begin   // OP-Code 0x4E : LD C,(HL)
            regBC.C := memRead(regHL.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $4F: begin   // OP-Code 0x4F : LD C,A
            regBC.C := regAF.A;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $50: begin   // OP-Code 0x50 : LD D,B
            regDE.D := regBC.B;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $51: begin   // OP-Code 0x51 : LD D,C
            regDE.D := regBC.C;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $52: begin   // OP-Code 0x52 : LD D,D
            regDE.D := regDE.D;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $53: begin   // OP-Code 0x53 : LD D,E
            regDE.D := regDE.E;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $54: begin   // OP-Code 0x54 : LD D,H
            regDE.D := regHL.H;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $55: begin   // OP-Code 0x55 : LD D,L
            regDE.D := regHL.L;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $56: begin   // OP-Code 0x56 : LD D,(HL)
            regDE.D := memRead(regHL.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $57: begin   // OP-Code 0x57 : LD D,A
            regDE.D := regAF.A;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $58: begin   // OP-Code 0x58 : LD E,B
            regDE.E := regBC.B;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $59: begin   // OP-Code 0x59 : LD E,C
            regDE.E := regBC.C;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $5A: begin   // OP-Code 0x5A : LD E,D
            regDE.E := regDE.D;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $5B: begin   // OP-Code 0x5B : LD E,E
            regDE.E := regDE.E;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $5C: begin   // OP-Code 0x5C : LD E,H
            regDE.E := regHL.H;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $5D: begin   // OP-Code 0x5D : LD E,L
            regDE.E := regHL.L;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $5E: begin   // OP-Code 0x5E : LD E,(HL)
            regDE.E := memRead(regHL.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $5F: begin   // OP-Code 0x5F : LD E,A
            regDE.E := regAF.A;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $60: begin   // OP-Code 0x60 : LD H,B
            regHL.H := regBC.B;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $61: begin   // OP-Code 0x61 : LD H,C
            regHL.H := regBC.C;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $62: begin   // OP-Code 0x62 : LD H,D
            regHL.H := regDE.D;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $63: begin   // OP-Code 0x63 : LD H,E
            regHL.H := regDE.E;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $64: begin   // OP-Code 0x64 : LD H,H
            regHL.H := regHL.H;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $65: begin   // OP-Code 0x65 : LD H,L
            regHL.H := regHL.L;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $66: begin   // OP-Code 0x66 : LD H,(HL)
            regHL.H := memRead(regHL.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $67: begin   // OP-Code 0x67 : LD H,A
            regHL.H := regAF.A;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $68: begin   // OP-Code 0x68 : LD L,B
            regHL.L := regBC.B;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $69: begin   // OP-Code 0x69 : LD L,C
            regHL.L := regBC.C;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $6A: begin   // OP-Code 0x6A : LD L,D
            regHL.L := regDE.D;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $6B: begin   // OP-Code 0x6B : LD L,E
            regHL.L := regDE.E;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $6C: begin   // OP-Code 0x6C : LD L,H
            regHL.L := regHL.H;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $6D: begin   // OP-Code 0x6D : LD L,L
            regHL.L := regHL.L;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $6E: begin   // OP-Code 0x6E : LD L,(HL)
            regHL.L := memRead(regHL.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $6F: begin   // OP-Code 0x6F : LD L,A
            regHL.L := regAF.A;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $70: begin   // OP-Code 0x70 : LD (HL),B
            memWrite(regHL.Value, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $71: begin   // OP-Code 0x71 : LD (HL),C
            memWrite(regHL.Value, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $72: begin   // OP-Code 0x72 : LD (HL),D
            memWrite(regHL.Value, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $73: begin   // OP-Code 0x73 : LD (HL),E
            memWrite(regHL.Value, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $74: begin   // OP-Code 0x74 : LD (HL),H
            memWrite(regHL.Value, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $75: begin   // OP-Code 0x75 : LD (HL),L
            memWrite(regHL.Value, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $76: begin   // OP-Code 0x76 : HALT
            tmpHALT := True;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $77: begin   // OP-Code 0x77 : LD (HL),A
            memWrite(regHL.Value, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $78: begin   // OP-Code 0x78 : LD A,B
            regAF.A := regBC.B;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $79: begin   // OP-Code 0x79 : LD A,C
            regAF.A := regBC.C;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $7A: begin   // OP-Code 0x7A : LD A,D
            regAF.A := regDE.D;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $7B: begin   // OP-Code 0x7B : LD A,E
            regAF.A := regDE.E;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $7C: begin   // OP-Code 0x7C : LD A,H
            regAF.A := regHL.H;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $7D: begin   // OP-Code 0x7D : LD A,L
            regAF.A := regHL.L;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $7E: begin   // OP-Code 0x7E : LD A,(HL)
            regAF.A := memRead(regHL.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $7F: begin   // OP-Code 0x7F : LD A,A
            regAF.A := regAF.A;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $80: begin   // OP-Code 0x80 : ADD A,B
            addA8Bit(regBC.B);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $81: begin   // OP-Code 0x81 : ADD A,C
            addA8Bit(regBC.C);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $82: begin   // OP-Code 0x82 : ADD A,D
            addA8Bit(regDE.D);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $83: begin   // OP-Code 0x83 : ADD A,E
            addA8Bit(regDE.E);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $84: begin   // OP-Code 0x84 : ADD A,H
            addA8Bit(regHL.H);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $85: begin   // OP-Code 0x85 : ADD A,L
            addA8Bit(regHL.L);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $86: begin   // OP-Code 0x86 : ADD A,(HL)
            addA8Bit(memRead(regHL.Value));
            machineCycles := 2;
            clockCycles := 6;
        end;
        $87: begin   // OP-Code 0x87 : ADD A,A
            addA8Bit(regAF.A);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $88: begin   // OP-Code 0x88 : ADC A,B
            adcA8Bit(regBC.B);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $89: begin   // OP-Code 0x89 : ADC A,C
            adcA8Bit(regBC.C);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $8A: begin   // OP-Code 0x8A : ADC A,D
            adcA8Bit(regDE.D);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $8B: begin   // OP-Code 0x8B : ADC A,E
            adcA8Bit(regDE.E);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $8C: begin   // OP-Code 0x8C : ADC A,H
            adcA8Bit(regHL.H);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $8D: begin   // OP-Code 0x8D : ADC A,L
            adcA8Bit(regHL.L);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $8E: begin   // OP-Code 0x8E : ADC A,(HL)
            adcA8Bit(memRead(regHL.Value));
            machineCycles := 2;
            clockCycles := 6;
        end;
        $8F: begin   // OP-Code 0x8F : ADC A,A
            adcA8Bit(regAF.A);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $90: begin   // OP-Code 0x90 : SUB A,B
            subA8Bit(regBC.B);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $91: begin   // OP-Code 0x91 : SUB A,C
            subA8Bit(regBC.C);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $92: begin   // OP-Code 0x92 : SUB A,D
            subA8Bit(regDE.D);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $93: begin   // OP-Code 0x93 : SUB A,E
            subA8Bit(regDE.E);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $94: begin   // OP-Code 0x94 : SUB A,H
            subA8Bit(regHL.H);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $95: begin   // OP-Code 0x95 : SUB A,L
            subA8Bit(regHL.L);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $96: begin   // OP-Code 0x96 : SUB A,(HL)
            subA8Bit(memRead(regHL.Value));
            machineCycles := 2;
            clockCycles := 6;
        end;
        $97: begin   // OP-Code 0x97 : SUB A,A
            subA8Bit(regAF.A);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $98: begin   // OP-Code 0x98 : SBC A,B
            sbcA8Bit(regBC.B);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $99: begin   // OP-Code 0x99 : SBC A,C
            sbcA8Bit(regBC.C);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $9A: begin   // OP-Code 0x9A : SBC A,D
            sbcA8Bit(regDE.D);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $9B: begin   // OP-Code 0x9B : SBC A,E
            sbcA8Bit(regDE.E);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $9C: begin   // OP-Code 0x9C : SBC A,H
            sbcA8Bit(regHL.H);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $9D: begin   // OP-Code 0x9D : SBC A,L
            sbcA8Bit(regHL.L);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $9E: begin   // OP-Code 0x9E : SBC A,(HL)
            sbcA8Bit(memRead(regHL.Value));
            machineCycles := 2;
            clockCycles := 6;
        end;
        $9F: begin   // OP-Code 0x9F : SBC A,A
            sbcA8Bit(regAF.A);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $A0: begin   // OP-Code 0xA0 : AND B
            andA8Bit(regBC.B);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $A1: begin   // OP-Code 0xA1 : AND C
            andA8Bit(regBC.C);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $A2: begin   // OP-Code 0xA2 : AND D
            andA8Bit(regDE.D);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $A3: begin   // OP-Code 0xA3 : AND E
            andA8Bit(regDE.E);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $A4: begin   // OP-Code 0xA4 : AND H
            andA8Bit(regHL.H);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $A5: begin   // OP-Code 0xA5 : AND L
            andA8Bit(regHL.L);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $A6: begin   // OP-Code 0xA6 : AND (HL)
            andA8Bit(memRead(regHL.Value));
            machineCycles := 2;
            clockCycles := 6;
        end;
        $A7: begin   // OP-Code 0xA7 : AND A
            andA8Bit(regAF.A);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $A8: begin   // OP-Code 0xA8 : XOR B
            xorA8Bit(regBC.B);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $A9: begin   // OP-Code 0xA9 : XOR C
            xorA8Bit(regBC.C);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $AA: begin   // OP-Code 0xAA : XOR D
            xorA8Bit(regDE.D);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $AB: begin   // OP-Code 0xAB : XOR E
            xorA8Bit(regDE.E);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $AC: begin   // OP-Code 0xAC : XOR H
            xorA8Bit(regHL.H);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $AD: begin   // OP-Code 0xAD : XOR L
            xorA8Bit(regHL.L);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $AE: begin   // OP-Code 0xAE : XOR (HL)
            xorA8Bit(memRead(regHL.Value));
            machineCycles := 2;
            clockCycles := 6;
        end;
        $AF: begin   // OP-Code 0xAF : XOR A
            xorA8Bit(regAF.A);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $B0: begin   // OP-Code 0xB0 : OR B
            orA8Bit(regBC.B);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $B1: begin   // OP-Code 0xB1 : OR C
            orA8Bit(regBC.C);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $B2: begin   // OP-Code 0xB2 : OR D
            orA8Bit(regDE.D);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $B3: begin   // OP-Code 0xB3 : OR E
            orA8Bit(regDE.E);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $B4: begin   // OP-Code 0xB4 : OR H
            orA8Bit(regHL.H);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $B5: begin   // OP-Code 0xB5 : OR L
            orA8Bit(regHL.L);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $B6: begin   // OP-Code 0xB6 : OR (HL)
            orA8Bit(memRead(regHL.Value));
            machineCycles := 2;
            clockCycles := 6;
        end;
        $B7: begin   // OP-Code 0xB7 : OR A
            orA8Bit(regAF.A);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $B8: begin   // OP-Code 0xB8 : CP B
            cpA8Bit(regBC.B);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $B9: begin   // OP-Code 0xB9 : CP C
            cpA8Bit(regBC.C);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $BA: begin   // OP-Code 0xBA : CP D
            cpA8Bit(regDE.D);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $BB: begin   // OP-Code 0xBB : CP E
            cpA8Bit(regDE.E);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $BC: begin   // OP-Code 0xBC : CP H
            cpA8Bit(regHL.H);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $BD: begin   // OP-Code 0xBD : CP L
            cpA8Bit(regHL.L);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $BE: begin   // OP-Code 0xBE : CP (HL)
            cpA8Bit(memRead(regHL.Value));
            machineCycles := 2;
            clockCycles := 6;
        end;
        $BF: begin   // OP-Code 0xBF : CP A
            cpA8Bit(regAF.A);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $C0: begin   // OP-Code 0xC0 : RET NZ
            if (regAF.Flag[Z] = False) then begin
                pop(regPC.Value);
                machineCycles := 4;
                clockCycles := 10;
            end
            else begin
                machineCycles := 3;
                clockCycles := 5;
            end;
        end;
        $C1: begin   // OP-Code 0xC1 : POP BC
            pop(regBC.Value);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $C2: begin   // OP-Code 0xC2 : JP NZ,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[Z] = False) then begin
                tmpWord.high := memRead(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 3;
                clockCycles := 9;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $C3: begin   // OP-Code 0xC3 : JP nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            regPC.Value := tmpWord.Value;
            machineCycles := 3;
            clockCycles := 9;
        end;
        $C4: begin   // OP-Code 0xC4 : CALL NZ,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[Z] = False) then begin
                tmpWord.high := memRead(regPC.Value);
                Inc(regPC.Value);
                push(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 6;
                clockCycles := 16;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $C5: begin   // OP-Code 0xC5 : PUSH BC
            push(regBC.Value);
            machineCycles := 5;
            clockCycles := 11;
        end;
        $C6: begin   // OP-Code 0xC6 : ADD A,n
            addA8Bit(memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $C7: begin   // OP-Code 0xC7 : RST 00H
            push(regPC.Value);
            regPC.Value := $0000;
            machineCycles := 5;
            clockCycles := 11;
        end;
        $C8: begin   // OP-Code 0xC8 : RET Z
            if (regAF.Flag[Z] = True) then begin
                pop(regPC.Value);
                machineCycles := 4;
                clockCycles := 10;
            end
            else begin
                machineCycles := 3;
                clockCycles := 5;
            end;
        end;
        $C9: begin   // OP-Code 0xC9 : RET
            pop(regPC.Value);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $CA: begin   // OP-Code 0xCA : JP Z,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[Z] = True) then begin
                tmpWord.high := memRead(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 3;
                clockCycles := 9;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $CB: begin   // OP-Code 0xCB : Prefix 'CB'
            execOpCbCodes;
        end;
        $CC: begin   // OP-Code 0xCC : CALL Z,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[Z] = True) then begin
                tmpWord.high := memRead(regPC.Value);
                Inc(regPC.Value);
                push(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 6;
                clockCycles := 16;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $CD: begin   // OP-Code 0xCD : CALL nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            push(regPC.Value);
            regPC.Value := tmpWord.Value;
            machineCycles := 6;
            clockCycles := 16;
        end;
        $CE: begin   // OP-Code 0xCE : ADC A,n
            adcA8Bit(memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $CF: begin   // OP-Code 0xCF : RST 08H
            push(regPC.Value);
            regPC.Value := $0008;
            machineCycles := 5;
            clockCycles := 11;
        end;
        $D0: begin   // OP-Code 0xD0 : RET NC
            if (regAF.Flag[C] = False) then begin
                pop(regPC.Value);
                machineCycles := 4;
                clockCycles := 10;
            end
            else begin
                machineCycles := 3;
                clockCycles := 5;
            end;
        end;
        $D1: begin   // OP-Code 0xD1 : POP DE
            pop(regDE.Value);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $D2: begin   // OP-Code 0xD2 : JP NC,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[C] = False) then begin
                tmpWord.high := memRead(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 3;
                clockCycles := 9;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $D3: begin   // OP-Code 0xD3 : OUT (n),A
            ioWrite(regAF.A, memRead(regPC.Value), regAF.A);
            Inc(regPC.Value);
            machineCycles := 4;
            clockCycles := 10;
        end;
        $D4: begin   // OP-Code 0xD4 : CALL NC,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[C] = False) then begin
                tmpWord.high := memRead(regPC.Value);
                Inc(regPC.Value);
                push(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 6;
                clockCycles := 16;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $D5: begin   // OP-Code 0xD5 : PUSH DE
            push(regDE.Value);
            machineCycles := 5;
            clockCycles := 11;
        end;
        $D6: begin   // OP-Code 0xD6 : SUB A,n
            subA8Bit(memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $D7: begin   // OP-Code 0xD7 : RST 10H
            push(regPC.Value);
            regPC.Value := $0010;
            machineCycles := 5;
            clockCycles := 11;
        end;
        $D8: begin   // OP-Code 0xD8 : RET C
            if (regAF.Flag[C] = True) then begin
                pop(regPC.Value);
                machineCycles := 4;
                clockCycles := 10;
            end
            else begin
                machineCycles := 3;
                clockCycles := 5;
            end;
        end;
        $D9: begin   // OP-Code 0xD9 : EXX
            tmpWord.Value := regBC.Value;
            regBC.Value := regBC_;
            regBC_ := tmpWord.Value;
            tmpWord.Value := regDE.Value;
            regDE.Value := regDE_;
            regDE_ := tmpWord.Value;
            tmpWord.Value := regHL.Value;
            regHL.Value := regHL_;
            regHL_ := tmpWord.Value;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $DA: begin   // OP-Code 0xDA : JP C,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[C] = True) then begin
                tmpWord.high := memRead(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 3;
                clockCycles := 9;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $DB: begin   // OP-Code 0xDB : IN A,(n)
            regAF.A := ioRead(regAF.A, memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $DC: begin   // OP-Code 0xDC : CALL C,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[C] = True) then begin
                tmpWord.high := memRead(regPC.Value);
                Inc(regPC.Value);
                push(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 6;
                clockCycles := 16;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $DD: begin   // OP-Code 0xDD : Prefix 'DD'
            execOpDdCodes;
        end;
        $DE: begin   // OP-Code 0xDE : SBC A,n
            sbcA8Bit(memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $DF: begin   // OP-Code 0xDF : RST 18H
            push(regPC.Value);
            regPC.Value := $0018;
            machineCycles := 5;
            clockCycles := 11;
        end;
        $E0: begin   // OP-Code 0xE0 : RET PO
            if (regAF.Flag[PV] = False) then begin
                pop(regPC.Value);
                machineCycles := 4;
                clockCycles := 10;
            end
            else begin
                machineCycles := 3;
                clockCycles := 5;
            end;
        end;
        $E1: begin   // OP-Code 0xE1 : POP HL
            pop(regHL.Value);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $E2: begin   // OP-Code 0xE2 : JP PO,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[PV] = False) then begin
                tmpWord.high := memRead(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 3;
                clockCycles := 9;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $E3: begin   // OP-Code 0xE3 : EX (SP),HL
            tmpWord.low := memRead(regSP.Value);
            tmpWord.high := memRead(regSP.Value + 1);
            memWrite(regSP.Value + 1, regHL.H);
            memWrite(regSP.Value, regHL.L);
            regHL.Value := tmpWord.Value;
            machineCycles := 6;
            clockCycles := 16;
        end;
        $E4: begin   // OP-Code 0xE4 : CALL PO,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[PV] = False) then begin
                tmpWord.high := memRead(regPC.Value);
                Inc(regPC.Value);
                push(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 6;
                clockCycles := 16;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $E5: begin   // OP-Code 0xE5 : PUSH HL
            push(regHL.Value);
            machineCycles := 5;
            clockCycles := 11;
        end;
        $E6: begin   // OP-Code 0xE6 : AND n
            andA8Bit(memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $E7: begin   // OP-Code 0xE7 : RST 20H
            push(regPC.Value);
            regPC.Value := $0020;
            machineCycles := 5;
            clockCycles := 11;
        end;
        $E8: begin   // OP-Code 0xE8 : RET PE
            if (regAF.Flag[PV] = True) then begin
                pop(regPC.Value);
                machineCycles := 4;
                clockCycles := 10;
            end
            else begin
                machineCycles := 3;
                clockCycles := 5;
            end;
        end;
        $E9: begin   // OP-Code 0xE9 : JP (HL)
            regPC.Value := regHL.Value;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $EA: begin   // OP-Code 0xEA : JP PE,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[PV] = True) then begin
                tmpWord.high := memRead(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 3;
                clockCycles := 9;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $EB: begin   // OP-Code 0xEB : EX DE,HL
            tmpWord.Value := regDE.Value;
            regDE.Value := regHL.Value;
            regHL.Value := tmpWord.Value;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $EC: begin   // OP-Code 0xEC : CALL PE,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[PV] = True) then begin
                tmpWord.high := memRead(regPC.Value);
                Inc(regPC.Value);
                push(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 6;
                clockCycles := 16;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $ED: begin   // OP-Code 0xED : Prefix 'ED'
            execOpEdCodes;
        end;
        $EE: begin   // OP-Code 0xEE : XOR n
            xorA8Bit(memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $EF: begin   // OP-Code 0xEF : RST 28H
            push(regPC.Value);
            regPC.Value := $0028;
            machineCycles := 5;
            clockCycles := 11;
        end;
        $F0: begin   // OP-Code 0xF0 : RET P
            if (regAF.Flag[S] = False) then begin
                pop(regPC.Value);
                machineCycles := 4;
                clockCycles := 10;
            end
            else begin
                machineCycles := 3;
                clockCycles := 5;
            end;
        end;
        $F1: begin   // OP-Code 0xF1 : POP AF
            pop(regAF.Value);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $F2: begin   // OP-Code 0xF2 : JP P,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[S] = False) then begin
                tmpWord.high := memRead(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 3;
                clockCycles := 9;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $F3: begin   // OP-Code 0xF3 : DI
            IFF1 := False;
            IFF2 := False;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $F4: begin   // OP-Code 0xF4 : CALL P,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[S] = False) then begin
                tmpWord.high := memRead(regPC.Value);
                Inc(regPC.Value);
                push(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 6;
                clockCycles := 16;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $F5: begin   // OP-Code 0xF5 : PUSH AF
            push(regAF.Value);
            machineCycles := 5;
            clockCycles := 11;
        end;
        $F6: begin   // OP-Code 0xF6 : OR n
            orA8Bit(memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $F7: begin   // OP-Code 0xF7 : RST 30H
            push(regPC.Value);
            regPC.Value := $0030;
            machineCycles := 5;
            clockCycles := 11;
        end;
        $F8: begin   // OP-Code 0xF8 : RET M
            if (regAF.Flag[S] = True) then begin
                pop(regPC.Value);
                machineCycles := 4;
                clockCycles := 10;
            end
            else begin
                machineCycles := 3;
                clockCycles := 5;
            end;
        end;
        $F9: begin   // OP-Code 0xF9 : LD SP,HL
            regSP.Value := regHL.Value;
            machineCycles := 2;
            clockCycles := 4;
        end;
        $FA: begin   // OP-Code 0xFA : JP M,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[S] = True) then begin
                tmpWord.high := memRead(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 3;
                clockCycles := 9;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $FB: begin   // OP-Code 0xFB : EI
            IFF1 := True;
            IFF2 := True;
            machineCycles := 1;
            clockCycles := 3;
        end;
        $FC: begin   // OP-Code 0xFC : CALL M,nn
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            if (regAF.Flag[S] = True) then begin
                tmpWord.high := memRead(regPC.Value);
                Inc(regPC.Value);
                push(regPC.Value);
                regPC.Value := tmpWord.Value;
                machineCycles := 6;
                clockCycles := 16;
            end
            else begin
                Inc(regPC.Value);
                machineCycles := 2;
                clockCycles := 6;
            end;
        end;
        $FD: begin   // OP-Code 0xFD : Prefix 'FD'
            execOpFdCodes;
        end;
        $FE: begin   // OP-Code 0xFE : CP n
            cpA8Bit(memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $FF: begin   // OP-Code 0xFF : RST 38H
            push(regPC.Value);
            regPC.Value := $0038;
            machineCycles := 5;
            clockCycles := 11;
        end;
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.execOpCbCodes;
var
    opCode, tmpByte: byte;
begin
    opCode := readOpCode(regPC.Value);
    Inc(regPC.Value);
    case (opCode) of
        $00: begin   // OP-Code 0xCB00 : RLC B
            rlc8Bit(regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $01: begin   // OP-Code 0xCB01 : RLC C
            rlc8Bit(regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $02: begin   // OP-Code 0xCB02 : RLC D
            rlc8Bit(regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $03: begin   // OP-Code 0xCB03 : RLC E
            rlc8Bit(regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $04: begin   // OP-Code 0xCB04 : RLC H
            rlc8Bit(regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $05: begin   // OP-Code 0xCB05 : RLC L
            rlc8Bit(regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $06: begin   // OP-Code 0xCB06 : RLC (HL)
            tmpByte := memRead(regHL.Value);
            rlc8Bit(tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $07: begin   // OP-Code 0xCB07 : RLC A
            rlc8Bit(regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $08: begin   // OP-Code 0xCB08 : RRC B
            rrc8Bit(regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $09: begin   // OP-Code 0xCB09 : RRC C
            rrc8Bit(regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $0A: begin   // OP-Code 0xCB0A : RRC D
            rrc8Bit(regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $0B: begin   // OP-Code 0xCB0B : RRC E
            rrc8Bit(regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $0C: begin   // OP-Code 0xCB0C : RRC H
            rrc8Bit(regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $0D: begin   // OP-Code 0xCB0D : RRC L
            rrc8Bit(regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $0E: begin   // OP-Code 0xCB0E : RRC (HL)
            tmpByte := memRead(regHL.Value);
            rrc8Bit(tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $0F: begin   // OP-Code 0xCB0F : RRC A
            rrc8Bit(regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $10: begin   // OP-Code 0xCB10 : RL B
            rl8Bit(regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $11: begin   // OP-Code 0xCB11 : RL C
            rl8Bit(regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $12: begin   // OP-Code 0xCB12 : RL D
            rl8Bit(regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $13: begin   // OP-Code 0xCB13 : RL E
            rl8Bit(regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $14: begin   // OP-Code 0xCB14 : RL H
            rl8Bit(regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $15: begin   // OP-Code 0xCB15 : RL L
            rl8Bit(regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $16: begin   // OP-Code 0xCB16 : RL (HL)
            tmpByte := memRead(regHL.Value);
            rl8Bit(tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $17: begin   // OP-Code 0xCB17 : RL A
            rl8Bit(regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $18: begin   // OP-Code 0xCB18 : RR B
            rr8Bit(regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $19: begin   // OP-Code 0xCB19 : RR C
            rr8Bit(regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $1A: begin   // OP-Code 0xCB1A : RR D
            rr8Bit(regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $1B: begin   // OP-Code 0xCB1B : RR E
            rr8Bit(regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $1C: begin   // OP-Code 0xCB1C : RR H
            rr8Bit(regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $1D: begin   // OP-Code 0xCB1D : RR L
            rr8Bit(regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $1E: begin   // OP-Code 0xCB1E : RR (HL)
            tmpByte := memRead(regHL.Value);
            rr8Bit(tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $1F: begin   // OP-Code 0xCB1F : RR A
            rr8Bit(regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $20: begin   // OP-Code 0xCB20 : SLA B
            sla8Bit(regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $21: begin   // OP-Code 0xCB21 : SLA C
            sla8Bit(regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $22: begin   // OP-Code 0xCB22 : SLA D
            sla8Bit(regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $23: begin   // OP-Code 0xCB23 : SLA E
            sla8Bit(regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $24: begin   // OP-Code 0xCB24 : SLA H
            sla8Bit(regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $25: begin   // OP-Code 0xCB25 : SLA L
            sla8Bit(regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $26: begin   // OP-Code 0xCB26 : SLA (HL)
            tmpByte := memRead(regHL.Value);
            sla8Bit(tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $27: begin   // OP-Code 0xCB27 : SLA A
            sla8Bit(regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $28: begin   // OP-Code 0xCB28 : SRA B
            sra8Bit(regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $29: begin   // OP-Code 0xCB29 : SRA C
            sra8Bit(regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $2A: begin   // OP-Code 0xCB2A : SRA D
            sra8Bit(regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $2B: begin   // OP-Code 0xCB2B : SRA E
            sra8Bit(regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $2C: begin   // OP-Code 0xCB2C : SRA H
            sra8Bit(regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $2D: begin   // OP-Code 0xCB2D : SRA L
            sra8Bit(regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $2E: begin   // OP-Code 0xCB2E : SRA (HL)
            tmpByte := memRead(regHL.Value);
            sra8Bit(tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $2F: begin   // OP-Code 0xCB2F : SRA A
            sra8Bit(regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $38: begin   // OP-Code 0xCB38 : SRL B
            srl8Bit(regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $39: begin   // OP-Code 0xCB39 : SRL C
            srl8Bit(regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $3A: begin   // OP-Code 0xCB3A : SRL D
            srl8Bit(regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $3B: begin   // OP-Code 0xCB3B : SRL E
            srl8Bit(regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $3C: begin   // OP-Code 0xCB3C : SRL H
            srl8Bit(regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $3D: begin   // OP-Code 0xCB3D : SRL L
            srl8Bit(regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $3E: begin   // OP-Code 0xCB3E : SRL (HL)
            tmpByte := memRead(regHL.Value);
            srl8Bit(tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $3F: begin   // OP-Code 0xCB3F : SRL A
            srl8Bit(regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $40: begin   // OP-Code 0xCB40 : BIT 0,B
            tstBit(0, regBC.B);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $41: begin   // OP-Code 0xCB41 : BIT 0,C
            tstBit(0, regBC.C);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $42: begin   // OP-Code 0xCB42 : BIT 0,D
            tstBit(0, regDE.D);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $43: begin   // OP-Code 0xCB43 : BIT 0,E
            tstBit(0, regDE.E);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $44: begin   // OP-Code 0xCB44 : BIT 0,H
            tstBit(0, regHL.H);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $45: begin   // OP-Code 0xCB45 : BIT 0,L
            tstBit(0, regHL.L);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $46: begin   // OP-Code 0xCB46 : BIT 0,(HL)
            tstBit(0, memRead(regHL.Value));
            machineCycles := 3;
            clockCycles := 9;
        end;
        $47: begin   // OP-Code 0xCB47 : BIT 0,A
            tstBit(0, regAF.A);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $48: begin   // OP-Code 0xCB48 : BIT 1,B
            tstBit(1, regBC.B);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $49: begin   // OP-Code 0xCB49 : BIT 1,C
            tstBit(1, regBC.C);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $4A: begin   // OP-Code 0xCB4A : BIT 1,D
            tstBit(1, regDE.D);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $4B: begin   // OP-Code 0xCB4B : BIT 1,E
            tstBit(1, regDE.E);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $4C: begin   // OP-Code 0xCB4C : BIT 1,H
            tstBit(1, regHL.H);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $4D: begin   // OP-Code 0xCB4D : BIT 1,L
            tstBit(1, regHL.L);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $4E: begin   // OP-Code 0xCB4E : BIT 1,(HL)
            tstBit(1, memRead(regHL.Value));
            machineCycles := 3;
            clockCycles := 9;
        end;
        $4F: begin   // OP-Code 0xCB4F : BIT 1,A
            tstBit(1, regAF.A);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $50: begin   // OP-Code 0xCB50 : BIT 2,B
            tstBit(2, regBC.B);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $51: begin   // OP-Code 0xCB51 : BIT 2,C
            tstBit(2, regBC.C);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $52: begin   // OP-Code 0xCB52 : BIT 2,D
            tstBit(2, regDE.D);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $53: begin   // OP-Code 0xCB53 : BIT 2,E
            tstBit(2, regDE.E);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $54: begin   // OP-Code 0xCB54 : BIT 2,H
            tstBit(2, regHL.H);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $55: begin   // OP-Code 0xCB55 : BIT 2,L
            tstBit(2, regHL.L);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $56: begin   // OP-Code 0xCB56 : BIT 2,(HL)
            tstBit(2, memRead(regHL.Value));
            machineCycles := 3;
            clockCycles := 9;
        end;
        $57: begin   // OP-Code 0xCB57 : BIT 2,A
            tstBit(2, regAF.A);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $58: begin   // OP-Code 0xCB58 : BIT 3,B
            tstBit(3, regBC.B);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $59: begin   // OP-Code 0xCB59 : BIT 3,C
            tstBit(3, regBC.C);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $5A: begin   // OP-Code 0xCB5A : BIT 3,D
            tstBit(3, regDE.D);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $5B: begin   // OP-Code 0xCB5B : BIT 3,E
            tstBit(3, regDE.E);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $5C: begin   // OP-Code 0xCB5C : BIT 3,H
            tstBit(3, regHL.H);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $5D: begin   // OP-Code 0xCB5D : BIT 3,L
            tstBit(3, regHL.L);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $5E: begin   // OP-Code 0xCB5E : BIT 3,(HL)
            tstBit(3, memRead(regHL.Value));
            machineCycles := 3;
            clockCycles := 9;
        end;
        $5F: begin   // OP-Code 0xCB5F : BIT 3,A
            tstBit(3, regAF.A);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $60: begin   // OP-Code 0xCB60 : BIT 4,B
            tstBit(4, regBC.B);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $61: begin   // OP-Code 0xCB61 : BIT 4,C
            tstBit(4, regBC.C);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $62: begin   // OP-Code 0xCB62 : BIT 4,D
            tstBit(4, regDE.D);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $63: begin   // OP-Code 0xCB63 : BIT 4,E
            tstBit(4, regDE.E);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $64: begin   // OP-Code 0xCB64 : BIT 4,H
            tstBit(4, regHL.H);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $65: begin   // OP-Code 0xCB65 : BIT 4,L
            tstBit(4, regHL.L);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $66: begin   // OP-Code 0xCB66 : BIT 4,(HL)
            tstBit(4, memRead(regHL.Value));
            machineCycles := 3;
            clockCycles := 9;
        end;
        $67: begin   // OP-Code 0xCB67 : BIT 4,A
            tstBit(4, regAF.A);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $68: begin   // OP-Code 0xCB68 : BIT 5,B
            tstBit(5, regBC.B);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $69: begin   // OP-Code 0xCB69 : BIT 5,C
            tstBit(5, regBC.C);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $6A: begin   // OP-Code 0xCB6A : BIT 5,D
            tstBit(5, regDE.D);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $6B: begin   // OP-Code 0xCB6B : BIT 5,E
            tstBit(5, regDE.E);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $6C: begin   // OP-Code 0xCB6C : BIT 5,H
            tstBit(5, regHL.H);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $6D: begin   // OP-Code 0xCB6D : BIT 5,L
            tstBit(5, regHL.L);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $6E: begin   // OP-Code 0xCB6E : BIT 5,(HL)
            tstBit(5, memRead(regHL.Value));
            machineCycles := 3;
            clockCycles := 9;
        end;
        $6F: begin   // OP-Code 0xCB6F : BIT 5,A
            tstBit(5, regAF.A);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $70: begin   // OP-Code 0xCB70 : BIT 6,B
            tstBit(6, regBC.B);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $71: begin   // OP-Code 0xCB71 : BIT 6,C
            tstBit(6, regBC.C);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $72: begin   // OP-Code 0xCB72 : BIT 6,D
            tstBit(6, regDE.D);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $73: begin   // OP-Code 0xCB73 : BIT 6,E
            tstBit(6, regDE.E);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $74: begin   // OP-Code 0xCB74 : BIT 6,H
            tstBit(6, regHL.H);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $75: begin   // OP-Code 0xCB75 : BIT 6,L
            tstBit(6, regHL.L);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $76: begin   // OP-Code 0xCB76 : BIT 6,(HL)
            tstBit(6, memRead(regHL.Value));
            machineCycles := 3;
            clockCycles := 9;
        end;
        $77: begin   // OP-Code 0xCB77 : BIT 6,A
            tstBit(6, regAF.A);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $78: begin   // OP-Code 0xCB78 : BIT 7,B
            tstBit(7, regBC.B);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $79: begin   // OP-Code 0xCB79 : BIT 7,C
            tstBit(7, regBC.C);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $7A: begin   // OP-Code 0xCB7A : BIT 7,D
            tstBit(7, regDE.D);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $7B: begin   // OP-Code 0xCB7B : BIT 7,E
            tstBit(7, regDE.E);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $7C: begin   // OP-Code 0xCB7C : BIT 7,H
            tstBit(7, regHL.H);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $7D: begin   // OP-Code 0xCB7D : BIT 7,L
            tstBit(7, regHL.L);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $7E: begin   // OP-Code 0xCB7E : BIT 7,(HL)
            tstBit(7, memRead(regHL.Value));
            machineCycles := 3;
            clockCycles := 9;
        end;
        $7F: begin   // OP-Code 0xCB7F : BIT 7,A
            tstBit(7, regAF.A);
            machineCycles := 2;
            clockCycles := 6;
        end;
        $80: begin   // OP-Code 0xCB80 : RES 0,B
            resBit(0, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $81: begin   // OP-Code 0xCB81 : RES 0,C
            resBit(0, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $82: begin   // OP-Code 0xCB82 : RES 0,D
            resBit(0, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $83: begin   // OP-Code 0xCB83 : RES 0,E
            resBit(0, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $84: begin   // OP-Code 0xCB84 : RES 0,H
            resBit(0, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $85: begin   // OP-Code 0xCB85 : RES 0,L
            resBit(0, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $86: begin   // OP-Code 0xCB86 : RES 0,(HL)
            tmpByte := memRead(regHL.Value);
            resBit(0, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $87: begin   // OP-Code 0xCB87 : RES 0,A
            resBit(0, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $88: begin   // OP-Code 0xCB88 : RES 1,B
            resBit(1, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $89: begin   // OP-Code 0xCB89 : RES 1,C
            resBit(1, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $8A: begin   // OP-Code 0xCB8A : RES 1,D
            resBit(1, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $8B: begin   // OP-Code 0xCB8B : RES 1,E
            resBit(1, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $8C: begin   // OP-Code 0xCB8C : RES 1,H
            resBit(1, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $8D: begin   // OP-Code 0xCB8D : RES 1,L
            resBit(1, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $8E: begin   // OP-Code 0xCB8E : RES 1,(HL)
            tmpByte := memRead(regHL.Value);
            resBit(1, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $8F: begin   // OP-Code 0xCB8F : RES 1,A
            resBit(1, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $90: begin   // OP-Code 0xCB90 : RES 2,B
            resBit(2, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $91: begin   // OP-Code 0xCB91 : RES 2,C
            resBit(2, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $92: begin   // OP-Code 0xCB92 : RES 2,D
            resBit(2, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $93: begin   // OP-Code 0xCB93 : RES 2,E
            resBit(2, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $94: begin   // OP-Code 0xCB94 : RES 2,H
            resBit(2, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $95: begin   // OP-Code 0xCB95 : RES 2,L
            resBit(2, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $96: begin   // OP-Code 0xCB96 : RES 2,(HL)
            tmpByte := memRead(regHL.Value);
            resBit(2, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $97: begin   // OP-Code 0xCB97 : RES 2,A
            resBit(2, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $98: begin   // OP-Code 0xCB98 : RES 3,B
            resBit(3, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $99: begin   // OP-Code 0xCB99 : RES 3,C
            resBit(3, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $9A: begin   // OP-Code 0xCB9A : RES 3,D
            resBit(3, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $9B: begin   // OP-Code 0xCB9B : RES 3,E
            resBit(3, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $9C: begin   // OP-Code 0xCB9C : RES 3,H
            resBit(3, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $9D: begin   // OP-Code 0xCB9D : RES 3,L
            resBit(3, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $9E: begin   // OP-Code 0xCB9E : RES 3,(HL)
            tmpByte := memRead(regHL.Value);
            resBit(3, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $9F: begin   // OP-Code 0xCB9F : RES 3,A
            resBit(3, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $A0: begin   // OP-Code 0xCBA0 : RES 4,B
            resBit(4, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $A1: begin   // OP-Code 0xCBA1 : RES 4,C
            resBit(4, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $A2: begin   // OP-Code 0xCBA2 : RES 4,D
            resBit(4, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $A3: begin   // OP-Code 0xCBA3 : RES 4,E
            resBit(4, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $A4: begin   // OP-Code 0xCBA4 : RES 4,H
            resBit(4, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $A5: begin   // OP-Code 0xCBA5 : RES 4,L
            resBit(4, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $A6: begin   // OP-Code 0xCBA6 : RES 4,(HL)
            tmpByte := memRead(regHL.Value);
            resBit(4, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $A7: begin   // OP-Code 0xCBA7 : RES 4,A
            resBit(4, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $A8: begin   // OP-Code 0xCBA8 : RES 5,B
            resBit(5, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $A9: begin   // OP-Code 0xCBA9 : RES 5,C
            resBit(5, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $AA: begin   // OP-Code 0xCBAA : RES 5,D
            resBit(5, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $AB: begin   // OP-Code 0xCBAB : RES 5,E
            resBit(5, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $AC: begin   // OP-Code 0xCBAC : RES 5,H
            resBit(5, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $AD: begin   // OP-Code 0xCBAD : RES 5,L
            resBit(5, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $AE: begin   // OP-Code 0xCBAE : RES 5,(HL)
            tmpByte := memRead(regHL.Value);
            resBit(5, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $AF: begin   // OP-Code 0xCBAF : RES 5,A
            resBit(5, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $B0: begin   // OP-Code 0xCBB0 : RES 6,B
            resBit(6, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $B1: begin   // OP-Code 0xCBB1 : RES 6,C
            resBit(6, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $B2: begin   // OP-Code 0xCBB2 : RES 6,D
            resBit(6, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $B3: begin   // OP-Code 0xCBB3 : RES 6,E
            resBit(6, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $B4: begin   // OP-Code 0xCBB4 : RES 6,H
            resBit(6, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $B5: begin   // OP-Code 0xCBB5 : RES 6,L
            resBit(6, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $B6: begin   // OP-Code 0xCBB6 : RES 6,(HL)
            tmpByte := memRead(regHL.Value);
            resBit(6, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $B7: begin   // OP-Code 0xCBB7 : RES 6,A
            resBit(6, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $B8: begin   // OP-Code 0xCBB8 : RES 7,B
            resBit(7, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $B9: begin   // OP-Code 0xCBB9 : RES 7,C
            resBit(7, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $BA: begin   // OP-Code 0xCBBA : RES 7,D
            resBit(7, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $BB: begin   // OP-Code 0xCBBB : RES 7,E
            resBit(7, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $BC: begin   // OP-Code 0xCBBC : RES 7,H
            resBit(7, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $BD: begin   // OP-Code 0xCBBD : RES 7,L
            resBit(7, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $BE: begin   // OP-Code 0xCBBE : RES 7,(HL)
            tmpByte := memRead(regHL.Value);
            resBit(7, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $BF: begin   // OP-Code 0xCBBF : RES 7,A
            resBit(7, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $C0: begin   // OP-Code 0xCBC0 : SET 0,B
            setBit(0, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $C1: begin   // OP-Code 0xCBC1 : SET 0,C
            setBit(0, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $C2: begin   // OP-Code 0xCBC2 : SET 0,D
            setBit(0, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $C3: begin   // OP-Code 0xCBC3 : SET 0,E
            setBit(0, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $C4: begin   // OP-Code 0xCBC4 : SET 0,H
            setBit(0, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $C5: begin   // OP-Code 0xCBC5 : SET 0,L
            setBit(0, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $C6: begin   // OP-Code 0xCBC6 : SET 0,(HL)
            tmpByte := memRead(regHL.Value);
            setBit(0, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $C7: begin   // OP-Code 0xCBC7 : SET 0,A
            setBit(0, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $C8: begin   // OP-Code 0xCBC8 : SET 1,B
            setBit(1, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $C9: begin   // OP-Code 0xCBC9 : SET 1,C
            setBit(1, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $CA: begin   // OP-Code 0xCBCA : SET 1,D
            setBit(1, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $CB: begin   // OP-Code 0xCBCB : SET 1,E
            setBit(1, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $CC: begin   // OP-Code 0xCBCC : SET 1,H
            setBit(1, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $CD: begin   // OP-Code 0xCBCD : SET 1,L
            setBit(1, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $CE: begin   // OP-Code 0xCBCE : SET 1,(HL)
            tmpByte := memRead(regHL.Value);
            setBit(1, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $CF: begin   // OP-Code 0xCBCF : SET 1,A
            setBit(1, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $D0: begin   // OP-Code 0xCBD0 : SET 2,B
            setBit(2, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $D1: begin   // OP-Code 0xCBD1 : SET 2,C
            setBit(2, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $D2: begin   // OP-Code 0xCBD2 : SET 2,D
            setBit(2, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $D3: begin   // OP-Code 0xCBD3 : SET 2,E
            setBit(2, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $D4: begin   // OP-Code 0xCBD4 : SET 2,H
            setBit(2, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $D5: begin   // OP-Code 0xCBD5 : SET 2,L
            setBit(2, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $D6: begin   // OP-Code 0xCBD6 : SET 2,(HL)
            tmpByte := memRead(regHL.Value);
            setBit(2, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $D7: begin   // OP-Code 0xCBD7 : SET 2,A
            setBit(2, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $D8: begin   // OP-Code 0xCBD8 : SET 3,B
            setBit(3, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $D9: begin   // OP-Code 0xCBD9 : SET 3,C
            setBit(3, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $DA: begin   // OP-Code 0xCBDA : SET 3,D
            setBit(3, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $DB: begin   // OP-Code 0xCBDB : SET 3,E
            setBit(3, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $DC: begin   // OP-Code 0xCBDC : SET 3,H
            setBit(3, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $DD: begin   // OP-Code 0xCBDD : SET 3,L
            setBit(3, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $DE: begin   // OP-Code 0xCBDE : SET 3,(HL)
            tmpByte := memRead(regHL.Value);
            setBit(3, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $DF: begin   // OP-Code 0xCBDF : SET 3,A
            setBit(3, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $E0: begin   // OP-Code 0xCBE0 : SET 4,B
            setBit(4, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $E1: begin   // OP-Code 0xCBE1 : SET 4,C
            setBit(4, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $E2: begin   // OP-Code 0xCBE2 : SET 4,D
            setBit(4, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $E3: begin   // OP-Code 0xCBE3 : SET 4,E
            setBit(4, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $E4: begin   // OP-Code 0xCBE4 : SET 4,H
            setBit(4, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $E5: begin   // OP-Code 0xCBE5 : SET 4,L
            setBit(4, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $E6: begin   // OP-Code 0xCBE6 : SET 4,(HL)
            tmpByte := memRead(regHL.Value);
            setBit(4, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $E7: begin   // OP-Code 0xCBE7 : SET 4,A
            setBit(4, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $E8: begin   // OP-Code 0xCBE8 : SET 5,B
            setBit(5, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $E9: begin   // OP-Code 0xCBE9 : SET 5,C
            setBit(5, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $EA: begin   // OP-Code 0xCBEA : SET 5,D
            setBit(5, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $EB: begin   // OP-Code 0xCBEB : SET 5,E
            setBit(5, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $EC: begin   // OP-Code 0xCBEC : SET 5,H
            setBit(5, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $ED: begin   // OP-Code 0xCBED : SET 5,L
            setBit(5, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $EE: begin   // OP-Code 0xCBEE : SET 5,(HL)
            tmpByte := memRead(regHL.Value);
            setBit(5, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $EF: begin     // OP-Code 0xCBEF : SET 5,A
            setBit(5, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $F0: begin   // OP-Code 0xCBF0 : SET 6,B
            setBit(6, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $F1: begin   // OP-Code 0xCBF1 : SET 6,C
            setBit(6, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $F2: begin   // OP-Code 0xCBF2 : SET 6,D
            setBit(6, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $F3: begin   // OP-Code 0xCBF3 : SET 6,E
            setBit(6, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $F4: begin   // OP-Code 0xCBF4 : SET 6,H
            setBit(6, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $F5: begin   // OP-Code 0xCBF5 : SET 6,L
            setBit(6, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $F6: begin   // OP-Code 0xCBF6 : SET 6,(HL)
            tmpByte := memRead(regHL.Value);
            setBit(6, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $F7: begin   // OP-Code 0xCBF7 : SET 6,A
            setBit(6, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $F8: begin   // OP-Code 0xCBF8 : SET 7,B
            setBit(7, regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $F9: begin   // OP-Code 0xCBF9 : SET 7,C
            setBit(7, regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $FA: begin   // OP-Code 0xCBFA : SET 7,D
            setBit(7, regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $FB: begin   // OP-Code 0xCBFB : SET 7,E
            setBit(7, regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $FC: begin   // OP-Code 0xCBFC : SET 7,H
            setBit(7, regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $FD: begin   // OP-Code 0xCBFD : SET 7,L
            setBit(7, regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $FE: begin   // OP-Code 0xCBFE : SET 7,(HL)
            tmpByte := memRead(regHL.Value);
            setBit(7, tmpByte);
            memWrite(regHL.Value, tmpByte);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $FF: begin   // OP-Code 0xCBFF : SET 7,A
            setBit(7, regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        {$ifndef NOTRAP}
        else begin
            ioITC.bit[TRAP] := True;  // TRAP Flag in ITC-Register setzen
            ioITC.bit[UFO] := False;  // UFO-Flag loeschen, da TRAP in 2. OP-Code aufgetreten
            push(regPC.Value);
            regPC.Value := $0000;
            machineCycles := 4;
            clockCycles := 12;
        end;
          {$endif}
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.execOpDdCodes;
var
    opCode, memOffset, tmpByte: byte;
    tmpWord: Treg16;
begin
    opCode := readOpCode(regPC.Value);
    Inc(regPC.Value);
    case (opCode) of
        $09: begin   // OP-Code 0xDD09 : ADD IX,BC
            addIX16Bit(regBC.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $19: begin   // OP-Code 0xDD19 : ADD IX,DE
            addIX16Bit(regDE.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $21: begin     // OP-Code 0xDD21 : LD IX,nn
            regIX.low := memRead(regPC.Value);
            Inc(regPC.Value);
            regIX.high := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $22: begin   // OP-Code 0xDD22 : LD (nn),IX
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(tmpWord.Value, regIX.low);
            memWrite(tmpWord.Value + 1, regIX.high);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $23: begin   // OP-Code 0xDD23 : INC IX
            Inc(regIX.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $29: begin   // OP-Code 0xDD29 : ADD IX,IX
            addIX16Bit(regIX.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $2A: begin   // OP-Code 0xDD2A : LD IX,(nn)
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            regIX.low := memRead(tmpWord.Value);
            regIX.high := memRead(tmpWord.Value + 1);
            machineCycles := 6;
            clockCycles := 18;
        end;
        $2B: begin   // OP-Code 0xDD2B : DEC IX
            Dec(regIX.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $34: begin   // OP-Code 0xDD34 : INC (IX+d)
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.Value := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord.Value);
            inc8Bit(tmpByte);
            memWrite(tmpWord.Value, tmpByte);
            machineCycles := 8;
            clockCycles := 18;
        end;
        $35: begin   // OP-Code 0xDD35 : DEC (IX+d)
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.Value := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord.Value);
            dec8Bit(tmpByte);
            memWrite(tmpWord.Value, tmpByte);
            machineCycles := 8;
            clockCycles := 18;
        end;
        $36: begin   // OP-Code 0xDD36 : LD (IX+d),n
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpByte := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIX.Value + shortint(memOffset), tmpByte);
            machineCycles := 5;
            clockCycles := 15;
        end;
        $39: begin   // OP-Code 0xDD39 : ADD IX,SP
            addIX16Bit(regSP.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $46: begin   // OP-Code 0xDD46 : LD B,(IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regBC.B := memRead(regIX.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $4E: begin   // OP-Code 0xDD4E : LD C,(IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regBC.C := memRead(regIX.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $56: begin   // OP-Code 0xDD56 : LD D,(IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regDE.D := memRead(regIX.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $5E: begin   // OP-Code 0xDD4E : LD E,(IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regDE.E := memRead(regIX.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $66: begin   // OP-Code 0xDD66 : LD H,(IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regHL.H := memRead(regIX.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $6E: begin   // OP-Code 0xDD6E : LD L,(IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regHL.L := memRead(regIX.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $70: begin   // OP-Code 0xDD70 : LD (IX+d),B
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIX.Value + shortint(memOffset), regBC.B);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $71: begin   // OP-Code 0xDD71 : LD (IX+d),C
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIX.Value + shortint(memOffset), regBC.C);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $72: begin   // OP-Code 0xDD72 : LD (IX+d),D
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIX.Value + shortint(memOffset), regDE.D);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $73: begin   // OP-Code 0xDD73 : LD (IX+d),E
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIX.Value + shortint(memOffset), regDE.E);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $74: begin   // OP-Code 0xDD74 : LD (IX+d),H
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIX.Value + shortint(memOffset), regHL.H);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $75: begin   // OP-Code 0xDD75 : LD (IX+d),L
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIX.Value + shortint(memOffset), regHL.L);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $77: begin   // OP-Code 0xDD77 : LD (IX+d),A
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIX.Value + shortint(memOffset), regAF.A);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $7E: begin   // OP-Code 0xDD7E : LD A,(IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regAF.A := memRead(regIX.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $86: begin   // OP-Code 0xDD86 : ADD A,(IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            addA8Bit(memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $8E: begin   // OP-Code 0xDD8E : ADC A,(IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            adcA8Bit(memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $96: begin   // OP-Code 0xDD96 : SUB A,(IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            subA8Bit(memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $9E: begin   // OP-Code 0xDD9E : SBC A,(IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            sbcA8Bit(memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $A6: begin   // OP-Code 0xDDA6 : AND (IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            andA8Bit(memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $AE: begin   // OP-Code 0xDDAE : XOR (IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            xorA8Bit(memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $B6: begin   // OP-Code 0xDDB6 : OR (IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            orA8Bit(memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $BE: begin   // OP-Code 0xDDBE : CP (IX+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            cpA8Bit(memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $CB: begin   // OP-Code 0xDDCB : Prefix 'DDCB'
            execOpDdCbCodes;
        end;
        $E1: begin   // OP-Code 0xDDE1 : POP IX
            pop(regIX.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $E3: begin   // OP-Code 0xDDE3 : EX (SP),IX
            tmpWord.low := memRead(regSP.Value);
            tmpWord.high := memRead(regSP.Value + 1);
            memWrite(regSP.Value, regIX.low);
            memWrite(regSP.Value + 1, regIX.high);
            regIX.Value := tmpWord.Value;
            machineCycles := 7;
            clockCycles := 19;
        end;
        $E5: begin   // OP-Code 0xDDE5 : PUSH IX
            push(regIX.Value);
            machineCycles := 6;
            clockCycles := 14;
        end;
        $E9: begin   // OP-Code 0xDDE9 : JP (IX)
            regPC.Value := regIX.Value;
            machineCycles := 2;
            clockCycles := 6;
        end;
        $F9: begin   // OP-Code 0xDDF9 : LD SP,IX
            regSP.Value := regIX.Value;
            machineCycles := 3;
            clockCycles := 7;
        end;
        {$ifndef NOTRAP}
        else begin
            ioITC.bit[TRAP] := True;  // TRAP Flag in ITC-Register setzen
            ioITC.bit[UFO] := False;  // UFO-Flag loeschen, da TRAP in 2. OP-Code aufgetreten
            push(regPC.Value);
            regPC.Value := $0000;
            machineCycles := 4;
            clockCycles := 12;
        end;
          {$endif}
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.execOpDdCbCodes;
var
    opCode, memOffset, tmpByte: byte;
    tmpWord: word;
begin
    memOffset := memRead(regPC.Value);
    Inc(regPC.Value);
    opCode := readOpCode(regPC.Value);
    Inc(regPC.Value);
    case (opCode) of
        $06: begin   // OP-Code 0xDDCB d 06 : RLC (IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            rlc8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $0E: begin   // OP-Code 0xDDCB d 0E : RRC (IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            rrc8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $16: begin   // OP-Code 0xDDCB d 16 : RL (IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            rl8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $1E: begin   // OP-Code 0xDDCB d 1E : RR (IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            rr8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $26: begin   // OP-Code 0xDDCB d 26 : SLA (IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            sla8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $2E: begin   // OP-Code 0xDDCB d 2E : SRA (IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            sra8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $3E: begin   // OP-Code 0xDDCB d 3E : SRL (IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            srl8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $46: begin   // OP-Code 0xDDCB d 46 : BIT 0,(IX+d)
            tstBit(0, memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $4E: begin    // OP-Code 0xDDCB d 4E : BIT 1,(IX+d)
            tstBit(1, memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $56: begin   // OP-Code 0xDDCB d 56 : BIT 2,(IX+d)
            tstBit(2, memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $5E: begin   // OP-Code 0xDDCB d 5E : BIT 3,(IX+d)
            tstBit(3, memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $66: begin   // OP-Code 0xDDCB d 66 : BIT 4,(IX+d)
            tstBit(4, memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $6E: begin   // OP-Code 0xDDCB d 6E : BIT 5,(IX+d)
            tstBit(5, memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $76: begin   // OP-Code 0xDDCB d 76 : BIT 6,(IX+d)
            tstBit(6, memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $7E: begin   // OP-Code 0xDDCB d 7E : BIT 7,(IX+d)
            tstBit(7, memRead(regIX.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $86: begin   // OP-Code 0xDDCB d 86 : RES 0,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(0, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $8E: begin   // OP-Code 0xDDCB d 8E : RES 1,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(1, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $96: begin   // OP-Code 0xDDCB d 96 : RES 2,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(2, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $9E: begin   // OP-Code 0xDDCB d 9E : RES 3,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(3, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $A6: begin   // OP-Code 0xDDCB d A6 : RES 4,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(4, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $AE: begin   // OP-Code 0xDDCB d AE : RES 5,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(5, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $B6: begin   // OP-Code 0xDDCB d B6 : RES 6,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(6, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $BE: begin   // OP-Code 0xDDCB d BE : RES 7,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(7, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $C6: begin   // OP-Code 0xDDCB d C6 : SET 0,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(0, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $CE: begin   // OP-Code 0xDDCB d CE : SET 1,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(1, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $D6: begin   // OP-Code 0xDDCB d D6 : SET 2,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(2, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $DE: begin   // OP-Code 0xDDCB d DE : SET 3,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(3, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $E6: begin   // OP-Code 0xDDCB d E6 : SET 4,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(4, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $EE: begin   // OP-Code 0xDDCB d EE : SET 5,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(5, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $F6: begin   // OP-Code 0xDDCB d F6 : SET 6,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(6, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $FE: begin   // OP-Code 0xDDCB d FE : SET 7,(IX+d)
            tmpWord := regIX.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(7, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        {$ifndef NOTRAP}
        else begin
            ioITC.bit[TRAP] := True;   // TRAP Flag in ITC-Register setzen
            ioITC.bit[UFO] := True;    // UFO-Flag setzen, da TRAP in 3. OP-Code aufgetreten
            push(regPC.Value);
            regPC.Value := $0000;
            machineCycles := 6;
            clockCycles := 18;
        end;
          {$endif}
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.execOpFdCodes;
var
    opCode, memOffset, tmpByte: byte;
    tmpWord: Treg16;
begin
    opCode := readOpCode(regPC.Value);
    Inc(regPC.Value);
    case (opCode) of
        $09: begin   // OP-Code 0xFD09 : ADD IY,BC
            addIY16Bit(regBC.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $19: begin   // OP-Code 0xFD19 : ADD IY,DE
            addIY16Bit(regDE.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $21: begin     // OP-Code 0xFD21 : LD IY,nn
            regIY.low := memRead(regPC.Value);
            Inc(regPC.Value);
            regIY.high := memRead(regPC.Value);
            Inc(regPC.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $22: begin   // OP-Code 0xFD22 : LD (nn),IY
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(tmpWord.Value, regIY.low);
            memWrite(tmpWord.Value + 1, regIY.high);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $23: begin   // OP-Code 0xFD23 : INC IY
            Inc(regIY.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $29: begin   // OP-Code 0xFD29 : ADD IY,IY
            addIY16Bit(regIY.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $2A: begin   // OP-Code 0xFD2A : LD IY,(nn)
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            regIY.low := memRead(tmpWord.Value);
            regIY.high := memRead(tmpWord.Value + 1);
            machineCycles := 6;
            clockCycles := 18;
        end;
        $2B: begin   // OP-Code 0xFD2B : DEC IY
            Dec(regIY.Value);
            machineCycles := 2;
            clockCycles := 4;
        end;
        $34: begin   // OP-Code 0xFD34 : INC (IY+d)
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.Value := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord.Value);
            inc8Bit(tmpByte);
            memWrite(tmpWord.Value, tmpByte);
            machineCycles := 8;
            clockCycles := 18;
        end;
        $35: begin   // OP-Code 0xFD35 : DEC (IY+d)
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.Value := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord.Value);
            dec8Bit(tmpByte);
            memWrite(tmpWord.Value, tmpByte);
            machineCycles := 8;
            clockCycles := 18;
        end;
        $36: begin   // OP-Code 0xFD36 : LD (IY+d),n
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpByte := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIY.Value + shortint(memOffset), tmpByte);
            machineCycles := 5;
            clockCycles := 15;
        end;
        $39: begin   // OP-Code 0xFD39 : ADD IY,SP
            addIY16Bit(regSP.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $46: begin   // OP-Code 0xFD46 : LD B,(IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regBC.B := memRead(regIY.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $4E: begin   // OP-Code 0xFD4E : LD C,(IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regBC.C := memRead(regIY.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $56: begin   // OP-Code 0xFD56 : LD D,(IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regDE.D := memRead(regIY.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $5E: begin   // OP-Code 0xFD4E : LD E,(IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regDE.E := memRead(regIY.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $66: begin   // OP-Code 0xFD66 : LD H,(IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regHL.H := memRead(regIY.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $6E: begin   // OP-Code 0xFD6E : LD L,(IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regHL.L := memRead(regIY.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $70: begin   // OP-Code 0xFD70 : LD (IY+d),B
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIY.Value + shortint(memOffset), regBC.B);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $71: begin   // OP-Code 0xFD71 : LD (IY+d),C
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIY.Value + shortint(memOffset), regBC.C);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $72: begin   // OP-Code 0xFD72 : LD (IY+d),D
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIY.Value + shortint(memOffset), regDE.D);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $73: begin   // OP-Code 0xFD73 : LD (IY+d),E
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIY.Value + shortint(memOffset), regDE.E);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $74: begin   // OP-Code 0xFD74 : LD (IY+d),H
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIY.Value + shortint(memOffset), regHL.H);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $75: begin   // OP-Code 0xFD75 : LD (IY+d),L
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIY.Value + shortint(memOffset), regHL.L);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $77: begin   // OP-Code 0xFD77 : LD (IY+d),A
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(regIY.Value + shortint(memOffset), regAF.A);
            machineCycles := 7;
            clockCycles := 15;
        end;
        $7E: begin   // OP-Code 0xFD7E : LD A,(IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            regAF.A := memRead(regIY.Value + shortint(memOffset));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $86: begin   // OP-Code 0xFD86 : ADD A,(IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            addA8Bit(memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $8E: begin   // OP-Code 0xFD8E : ADC A,(IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            adcA8Bit(memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $96: begin   // OP-Code 0xFD96 : SUB A,(IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            subA8Bit(memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $9E: begin   // OP-Code 0xFD9E : SBC A,(IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            sbcA8Bit(memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $A6: begin   // OP-Code 0xFDA6 : AND (IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            andA8Bit(memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $AE: begin   // OP-Code 0xFDAE : XOR (IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            xorA8Bit(memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $B6: begin   // OP-Code 0xFDB6 : OR (IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            orA8Bit(memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $BE: begin   // OP-Code 0xFDBE : CP (IY+d);
            memOffset := memRead(regPC.Value);
            Inc(regPC.Value);
            cpA8Bit(memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 6;
            clockCycles := 14;
        end;
        $CB: begin   // OP-Code 0xFDCB : Prefix 'DDCB'
            execOpFdCbCodes;
        end;
        $E1: begin   // OP-Code 0xFDE1 : POP IY
            pop(regIY.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $E3: begin   // OP-Code 0xFDE3 : EX (SP),IY
            tmpWord.low := memRead(regSP.Value);
            tmpWord.high := memRead(regSP.Value + 1);
            memWrite(regSP.Value, regIY.low);
            memWrite(regSP.Value + 1, regIY.high);
            regIY.Value := tmpWord.Value;
            machineCycles := 7;
            clockCycles := 19;
        end;
        $E5: begin   // OP-Code 0xFDE5 : PUSH IY
            push(regIY.Value);
            machineCycles := 6;
            clockCycles := 14;
        end;
        $E9: begin   // OP-Code 0xFDE9 : JP (IY)
            regPC.Value := regIY.Value;
            machineCycles := 2;
            clockCycles := 6;
        end;
        $F9: begin   // OP-Code 0xFDF9 : LD SP,IY
            regSP.Value := regIY.Value;
            machineCycles := 3;
            clockCycles := 7;
        end;
        {$ifndef NOTRAP}
        else begin
            ioITC.bit[TRAP] := True;  // TRAP Flag in ITC-Register setzen
            ioITC.bit[UFO] := False;  // UFO-Flag loeschen, da TRAP in 2. OP-Code aufgetreten
            push(regPC.Value);
            regPC.Value := $0000;
            machineCycles := 4;
            clockCycles := 12;
        end;
          {$endif}
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.execOpFdCbCodes;
var
    opCode, memOffset, tmpByte: byte;
    tmpWord: word;
begin
    memOffset := memRead(regPC.Value);
    Inc(regPC.Value);
    opCode := readOpCode(regPC.Value);
    Inc(regPC.Value);
    case (opCode) of
        $06: begin   // OP-Code 0xFDCB d 06 : RLC (IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            rlc8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $0E: begin   // OP-Code 0xFDCB d 0E : RRC (IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            rrc8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $16: begin   // OP-Code 0xFDCB d 16 : RL (IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            rl8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $1E: begin   // OP-Code 0xFDCB d 1E : RR (IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            rr8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $26: begin   // OP-Code 0xFDCB d 26 : SLA (IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            sla8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $2E: begin   // OP-Code 0xFDCB d 2E : SRA (IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            sra8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $3E: begin   // OP-Code 0xFDCB d 3E : SRL (IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            srl8Bit(tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $46: begin   // OP-Code 0xFDCB d 46 : BIT 0,(IY+d)
            tstBit(0, memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $4E: begin    // OP-Code 0xFDCB d 4E : BIT 1,(IY+d)
            tstBit(1, memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $56: begin   // OP-Code 0xFDCB d 56 : BIT 2,(IY+d)
            tstBit(2, memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $5E: begin   // OP-Code 0xFDCB d 5E : BIT 3,(IY+d)
            tstBit(3, memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $66: begin   // OP-Code 0xFDCB d 66 : BIT 4,(IY+d)
            tstBit(4, memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $6E: begin   // OP-Code 0xFDCB d 6E : BIT 5,(IY+d)
            tstBit(5, memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $76: begin   // OP-Code 0xFDCB d 76 : BIT 6,(IY+d)
            tstBit(6, memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $7E: begin   // OP-Code 0xFDCB d 7E : BIT 7,(IY+d)
            tstBit(7, memRead(regIY.Value + shortint(memOffset)));
            machineCycles := 5;
            clockCycles := 15;
        end;
        $86: begin   // OP-Code 0xFDCB d 86 : RES 0,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(0, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $8E: begin   // OP-Code 0xFDCB d 8E : RES 1,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(1, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $96: begin   // OP-Code 0xFDCB d 96 : RES 2,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(2, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $9E: begin   // OP-Code 0xFDCB d 9E : RES 3,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(3, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $A6: begin   // OP-Code 0xFDCB d A6 : RES 4,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(4, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $AE: begin   // OP-Code 0xFDCB d AE : RES 5,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(5, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $B6: begin   // OP-Code 0xFDCB d B6 : RES 6,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(6, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $BE: begin   // OP-Code 0xFDCB d BE : RES 7,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            resBit(7, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $C6: begin   // OP-Code 0xFDCB d C6 : SET 0,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(0, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $CE: begin   // OP-Code 0xFDCB d CE : SET 1,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(1, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $D6: begin   // OP-Code 0xFDCB d D6 : SET 2,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(2, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $DE: begin   // OP-Code 0xFDCB d DE : SET 3,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(3, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $E6: begin   // OP-Code 0xFDCB d E6 : SET 4,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(4, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $EE: begin   // OP-Code 0xFDCB d EE : SET 5,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(5, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $F6: begin   // OP-Code 0xFDCB d F6 : SET 6,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(6, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $FE: begin   // OP-Code 0xFDCB d FE : SET 7,(IY+d)
            tmpWord := regIY.Value + shortint(memOffset);
            tmpByte := memRead(tmpWord);
            setBit(7, tmpByte);
            memWrite(tmpWord, tmpByte);
            machineCycles := 7;
            clockCycles := 19;
        end;
        {$ifndef NOTRAP}
        else begin
            ioITC.bit[TRAP] := True;   // TRAP Flag in ITC-Register setzen
            ioITC.bit[UFO] := True;    // UFO-Flag setzen, da TRAP in 3. OP-Code aufgetreten
            push(regPC.Value);
            regPC.Value := $0000;
            machineCycles := 6;
            clockCycles := 18;
        end;
          {$endif}
    end;
end;

// --------------------------------------------------------------------------------
procedure TZ180Cpu.execOpEdCodes;
var
    opCode, tmpByte, tmpNibble: byte;
    tmpWord: Treg16;
begin
    opCode := readOpCode(regPC.Value);
    Inc(regPC.Value);
    case (opCode) of
        $00: begin   // OP-Code 0xED00 : IN0 B,(n)
            regBC.B := inreg8Bit($00, memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $01: begin   // OP-Code 0xED01 : OUT0 (n),B
            ioWrite($00, memRead(regPC.Value), regBC.B);
            Inc(regPC.Value);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $04: begin   // OP-Code 0xED04 : TST B
            tstA8Bit(regBC.B);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $08: begin   // OP-Code 0xED08 : IN0 C,(n)
            regBC.C := inreg8Bit($00, memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $09: begin   // OP-Code 0xED09 : OUT0 (n),C
            ioWrite($00, memRead(regPC.Value), regBC.C);
            Inc(regPC.Value);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $0C: begin   // OP-Code 0xED0C : TST C
            tstA8Bit(regBC.C);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $10: begin   // OP-Code 0xED10 : IN0 D,(n)
            regDE.D := inreg8Bit($00, memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $11: begin   // OP-Code 0xED11 : OUT0 (n),D
            ioWrite($00, memRead(regPC.Value), regDE.D);
            Inc(regPC.Value);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $14: begin   // OP-Code 0xED14 : TST D
            tstA8Bit(regDE.D);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $18: begin   // OP-Code 0xED18 : IN0 E,(n)
            regDE.E := inreg8Bit($00, memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $19: begin   // OP-Code 0xED19 : OUT0 (n),E
            ioWrite($00, memRead(regPC.Value), regDE.E);
            Inc(regPC.Value);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $1C: begin   // OP-Code 0xED1C : TST E
            tstA8Bit(regDE.E);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $20: begin   // OP-Code 0xED20 : IN0 H,(n)
            regHL.H := inreg8Bit($00, memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $21: begin   // OP-Code 0xED21 : OUT0 (n),H
            ioWrite($00, memRead(regPC.Value), regHL.H);
            Inc(regPC.Value);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $24: begin   // OP-Code 0xED24 : TST H
            tstA8Bit(regHL.H);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $28: begin   // OP-Code 0xED28 : IN0 L,(n)
            regHL.L := inreg8Bit($00, memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $29: begin   // OP-Code 0xED29 : OUT0 (n),L
            ioWrite($00, memRead(regPC.Value), regHL.L);
            Inc(regPC.Value);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $2C: begin   // OP-Code 0xED2C : TST L
            tstA8Bit(regHL.L);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $34: begin   // OP-Code 0xED34 : TST (HL)
            tstA8Bit(memRead(regHL.Value));
            machineCycles := 4;
            clockCycles := 10;
        end;
        $38: begin   // OP-Code 0xED38 : IN0 A,(n)
            regAF.A := inreg8Bit($00, memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $39: begin   // OP-Code 0xED39 : OUT0 (n),A
            ioWrite($00, memRead(regPC.Value), regAF.A);
            Inc(regPC.Value);
            machineCycles := 5;
            clockCycles := 13;
        end;
        $3C: begin   // OP-Code 0xED3C : TST A
            tstA8Bit(regAF.A);
            machineCycles := 3;
            clockCycles := 7;
        end;
        $40: begin   // OP-Code 0xED40 : IN B,(C)
            regBC.B := inreg8Bit(regBC.B, regBC.C);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $41: begin   // OP-Code 0xED41 : OUT (C),B
            ioWrite(regBC.B, regBC.C, regBC.B);
            machineCycles := 4;
            clockCycles := 10;
        end;
        $42: begin   // OP-Code 0xED42 : SBC HL,BC
            sbcHL16Bit(regBC.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $43: begin   // OP-Code 0xED43 : LD (nn),BC
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(tmpWord.Value, regBC.C);
            memWrite(tmpWord.Value + 1, regBC.B);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $44: begin   // OP-Code 0xED44 : NEG
            regAF.Flag[C] := (regAF.A <> $00);  // C is set if Accumulator was not 00H before operation; reset otherwise
            regAF.Flag[PV] := (regAF.A = $80);  // P/V is set if Accumulator was 80H before operation; reset otherwise
            regAF.Flag[H] := ($00 - (regAF.A and $0F) < $00);  // H is set if borrow from bit 4; reset otherwise
            regAF.A := ($00 - regAF.A);
            regAF.Flag[S] := ((regAF.A and $80) <> $00);  // S is set if result is negative; reset otherwise
            regAF.Flag[Z] := (regAF.A = $00);  // Z is set if result is 0; reset otherwise
            regAF.Flag[N] := True;
            machineCycles := 2;
            clockCycles := 6;
        end;
        $45: begin   // OP-Code 0xED45 : RETN
            pop(regPC.Value);
            IFF1 := IFF2;
            machineCycles := 4;
            clockCycles := 12;
        end;
        $46: begin   // OP-Code 0xED46 : IM 0
            intMode := IM0;
            machineCycles := 2;
            clockCycles := 6;
        end;
        $47: begin   // OP-Code 0xED47 : LD I,A
            regI := regAF.A;
            machineCycles := 2;
            clockCycles := 6;
        end;
        $48: begin   // OP-Code 0xED48 : IN C,(C)
            regBC.C := inreg8Bit(regBC.B, regBC.C);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $49: begin   // OP-Code 0xED49 : OUT (C),C
            ioWrite(regBC.B, regBC.C, regBC.C);
            machineCycles := 4;
            clockCycles := 10;
        end;
        $4A: begin   // OP-Code 0xED4A : ADC HL,BC
            adcHL16Bit(regBC.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $4B: begin   // OP-Code 0xED4B : LD BC,(nn)
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            regBC.C := memRead(tmpWord.Value);
            regBC.B := memRead(tmpWord.Value + 1);
            machineCycles := 6;
            clockCycles := 18;
        end;
        $4C: begin   // OP-Code 0xED4C : MLT BC
            regBC.Value := ((regBC.B * regBC.C) and $FFFF);
            machineCycles := 13;
            clockCycles := 17;
        end;
        $4D: begin   // OP-Code 0xED4D : RETI
            //NOTE: OP-Code 0xED4D : RETI interrupt daisy-chain not implemented
            pop(regPC.Value);
            machineCycles := 4;
            clockCycles := 12;
        end;
        $4F: begin   // OP-Code 0xED4F : LD R,A
            regR := regAF.A;
            machineCycles := 2;
            clockCycles := 6;
        end;
        $50: begin   // OP-Code 0xED50 : IN D,(C)
            regDE.D := inreg8Bit(regBC.B, regBC.C);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $51: begin   // OP-Code 0xED51 : OUT (C),D
            ioWrite(regBC.B, regBC.C, regDE.D);
            machineCycles := 4;
            clockCycles := 10;
        end;
        $52: begin   // OP-Code 0xED52 : SBC HL,DE
            sbcHL16Bit(regDE.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $53: begin   // OP-Code 0xED53 : LD (nn),DE
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(tmpWord.Value, regDE.E);
            memWrite(tmpWord.Value + 1, regDE.D);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $56: begin   // OP-Code 0xED56 : IM 1
            intMode := IM1;
            machineCycles := 2;
            clockCycles := 6;
        end;
        $57: begin   // OP-Code 0xED57 : LD A,I
            regAF.A := regI;
            regAF.Flag[S] := ((regI and $80) <> $00); // S is set if I-Register is negative; reset otherwise
            regAF.Flag[Z] := (regI = $00); // Z is set if I-Register is zero; reset otherwise
            regAF.Flag[PV] := IFF2;  // P/V contains contents of IFF2
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            machineCycles := 2;
            clockCycles := 6;
        end;
        $58: begin    // OP-Code 0xED58 : IN E,(C)
            regDE.E := inreg8Bit(regBC.B, regBC.C);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $59: begin   // OP-Code 0xED59 : OUT (C),E
            ioWrite(regBC.B, regBC.C, regDE.E);
            machineCycles := 4;
            clockCycles := 10;
        end;
        $5A: begin   // OP-Code 0xED5A : ADC HL,DE
            adcHL16Bit(regDE.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $5B: begin   // OP-Code 0xED5B : LD DE,(nn)
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            regDE.E := memRead(tmpWord.Value);
            regDE.D := memRead(tmpWord.Value + 1);
            machineCycles := 6;
            clockCycles := 18;
        end;
        $5C: begin   // OP-Code 0xED5C : MLT DE
            regDE.Value := ((regDE.D * regDE.E) and $FFFF);
            machineCycles := 13;
            clockCycles := 17;
        end;
        $5E: begin   // OP-Code 0xED5E : IM 2
            intMode := IM2;
            machineCycles := 2;
            clockCycles := 6;
        end;
        $5F: begin   // OP-Code 0xED5F : LD A,R
            regAF.A := regR;
            regAF.Flag[S] := ((regR and $80) <> $00); // S is set if R-Register is negative; reset otherwise
            regAF.Flag[Z] := (regR = $00); // Z is set if R-Register is zero; reset otherwise
            regAF.Flag[PV] := IFF2;  // P/V contains contents of IFF2
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            machineCycles := 2;
            clockCycles := 6;
        end;
        $60: begin   // OP-Code 0xED60 : IN H,(C)
            regHL.H := inreg8Bit(regBC.B, regBC.C);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $61: begin   // OP-Code 0xED61 : OUT (C),H
            ioWrite(regBC.B, regBC.C, regHL.H);
            machineCycles := 4;
            clockCycles := 10;
        end;
        $62: begin   // OP-Code 0xED62 : SBC HL,HL
            sbcHL16Bit(regHL.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $63: begin   // OP-Code 0xED63 : LD (nn),HL
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(tmpWord.Value, regHL.L);
            memWrite(tmpWord.Value + 1, regHL.H);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $64: begin   // OP-Code 0xED64 : TST n
            tstA8Bit(memRead(regPC.Value));
            Inc(regPC.Value);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $67: begin   // OP-Code 0xED67 : RRD (HL)
            tmpByte := memRead(regHL.Value);
            tmpNibble := (regAF.A and $0F);
            regAF.A := ((regAF.A and $F0) or (tmpByte and $0F));
            tmpByte := (((tmpByte shr 4) or (tmpNibble shl 4)) and $FF);
            memWrite(regHL.Value, tmpByte);
            regAF.Flag[S] := ((tmpByte and $80) <> $00); // S is set if (HL) is negative after operation; reset otherwise
            regAF.Flag[Z] := (tmpByte = $00); // Z is set if (HL) is zero after operation; reset otherwise
            regAF.Flag[PV] := calcParity(tmpByte); // P/V is set if parity of (HL) is even after operation; reset otherwise
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            machineCycles := 8;
            clockCycles := 16;
        end;
        $68: begin   // OP-Code 0xED68 : IN L,(C)
            regHL.L := inreg8Bit(regBC.B, regBC.C);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $69: begin   // OP-Code 0xED69 : OUT (C),L
            ioWrite(regBC.B, regBC.C, regHL.L);
            machineCycles := 4;
            clockCycles := 10;
        end;
        $6A: begin   // OP-Code 0xED6A : ADC HL,HL
            adcHL16Bit(regHL.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $6B: begin   // OP-Code 0xED6B : LD HL,(nn)
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            regHL.L := memRead(tmpWord.Value);
            regHL.H := memRead(tmpWord.Value + 1);
            machineCycles := 6;
            clockCycles := 18;
        end;
        $6C: begin   // OP-Code 0xED6C : MLT HL
            regHL.Value := ((regHL.H * regHL.L) and $FFFF);
            machineCycles := 13;
            clockCycles := 17;
        end;
        $6F: begin   // OP-Code 0xED6F : RLD (HL)
            tmpByte := memRead(regHL.Value);
            tmpNibble := (regAF.A and $0F);
            regAF.A := ((regAF.A and $F0) or (tmpByte shr 4));
            tmpByte := (((tmpByte shl 4) or tmpNibble) and $FF);
            memWrite(regHL.Value, tmpByte);
            regAF.Flag[S] := ((tmpByte and $80) <> $00); // S is set if (HL) is negative after operation; reset otherwise
            regAF.Flag[Z] := (tmpByte = $00); // Z is set if (HL) is zero after operation; reset otherwise
            regAF.Flag[PV] := calcParity(tmpByte); // P/V is set if parity of (HL) is even after operation; reset otherwise
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            machineCycles := 8;
            clockCycles := 16;
        end;
        $72: begin   // OP-Code 0xED72 : SBC HL,SP
            sbcHL16Bit(regSP.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $73: begin   // OP-Code 0xED73 : LD (nn),SP
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            memWrite(tmpWord.Value, regSP.low);
            memWrite(tmpWord.Value + 1, regSP.high);
            machineCycles := 7;
            clockCycles := 19;
        end;
        $74: begin   // OP-Code 0xED74 : TSTIO n
            tmpByte := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpByte := (tmpByte and ioRead($00, regBC.C));
            regAF.Flag[PV] := calcParity(tmpByte); // P/V is set if parity is even; reset otherwise
            regAF.Flag[Z] := (tmpByte = $00); // Z is set if the result is zero; reset otherwise
            regAF.Flag[S] := ((tmpByte and $80) <> $00); // S is set if the result is negative; reset otherwise
            regAF.Flag[C] := False;
            regAF.Flag[N] := False;
            regAF.Flag[H] := True;
            machineCycles := 4;
            clockCycles := 12;
        end;
        $76: begin   // OP-Code 0xED76 : SLP
            tmpSLP := True;
            machineCycles := 2;
            clockCycles := 8;
        end;
        $78: begin   // OP-Code 0xED78 : IN A,(C)
            regAF.A := inreg8Bit(regBC.B, regBC.C);
            machineCycles := 3;
            clockCycles := 9;
        end;
        $79: begin   // OP-Code 0xED79 : OUT (C),A
            ioWrite(regBC.B, regBC.C, regAF.A);
            machineCycles := 4;
            clockCycles := 10;
        end;
        $7A: begin   // OP-Code 0xED7A : ADC HL,SP
            adcHL16Bit(regSP.Value);
            machineCycles := 6;
            clockCycles := 10;
        end;
        $7B: begin   // OP-Code 0xED7B : LD SP,(nn)
            tmpWord.low := memRead(regPC.Value);
            Inc(regPC.Value);
            tmpWord.high := memRead(regPC.Value);
            Inc(regPC.Value);
            regSP.low := memRead(tmpWord.Value);
            regSP.high := memRead(tmpWord.Value + 1);
            machineCycles := 6;
            clockCycles := 18;
        end;
        $7C: begin   // OP-Code 0xED7C : MLT SP
            regSP.Value := ((regSP.high * regSP.low) and $FFFF);
            machineCycles := 13;
            clockCycles := 17;
        end;
        $83: begin   // OP-Code 0xED83 : OTIM
            tmpByte := memRead(regHL.Value);
            ioWrite($00, regBC.C, tmpByte);
            Inc(regHL.Value);
            Inc(regBC.C);
            Dec(regBC.B);
            regAF.Flag[C] := (regBC.B = $FF); // C is set if a borrow occurs after B-l; reset otherwise
            regAF.Flag[N] := ((tmpByte and $80) <> $00); // N is set if MSB of memData=1; reset otherwise
            regAF.Flag[PV] := calcParity(regBC.B); // P/V is set if parity in B is even after B-l; reset otherwise
            regAF.Flag[H] := ((regBC.B and $0F) = $0F); // H is set if a borrow from bit 4 of B occurs after B-l; reset otherwise
            regAF.Flag[Z] := (regBC.B = $00); // Z is set if B=OO after B-l; reset otherwise
            regAF.Flag[S] := ((regBC.B and $80) <> $00); // S is set if B is negative after B-l; reset otherwise
            machineCycles := 6;
            clockCycles := 14;
        end;
        $8B: begin   // OP-Code 0xED8B : OTDM
            tmpByte := memRead(regHL.Value);
            ioWrite($00, regBC.C, tmpByte);
            Dec(regHL.Value);
            Dec(regBC.C);
            Dec(regBC.B);
            regAF.Flag[C] := (regBC.B = $FF); // C is set if a borrow occurs after B-l; reset otherwise
            regAF.Flag[N] := ((tmpByte and $80) <> $00); // N is set if MSB of memData=1; reset otherwise
            regAF.Flag[PV] := calcParity(regBC.B); // P/V is set if parity in B is even after B-l; reset otherwise
            regAF.Flag[H] := ((regBC.B and $0F) = $0F); // H is set if a borrow from bit 4 of B occurs after B-l; reset otherwise
            regAF.Flag[Z] := (regBC.B = $00); // Z is set if B=OO after B-l; reset otherwise
            regAF.Flag[S] := ((regBC.B and $80) <> $00); // S is set if B is negative after B-l; reset otherwise
            machineCycles := 6;
            clockCycles := 14;
        end;
        $93: begin   // OP-Code 0xED93 : OTIMR
            tmpByte := memRead(regHL.Value);
            ioWrite($00, regBC.C, tmpByte);
            Inc(regHL.Value);
            Inc(regBC.C);
            Dec(regBC.B);
            regAF.Flag[C] := (regBC.B = $FF); // C is set if a borrow occurs after B-l; reset otherwise
            regAF.Flag[N] := ((tmpByte and $80) <> $00); // N is set if MSB of memData=1; reset otherwise
            regAF.Flag[PV] := calcParity(regBC.B); // P/V is set if parity in B is even after B-l; reset otherwise
            regAF.Flag[H] := ((regBC.B and $0F) = $0F); // H is set if a borrow from bit 4 of B occurs after B-l; reset otherwise
            regAF.Flag[Z] := (regBC.B = $00); // Z is set if B=OO after B-l; reset otherwise
            regAF.Flag[S] := ((regBC.B and $80) <> $00); // S is set if B is negative after B-l; reset otherwise
            if (not regAF.Flag[Z]) then begin
                regPC.Value := ((regPC.Value - 2) and $FFFF);
                machineCycles := 8;
                clockCycles := 16;
            end
            else begin
                machineCycles := 6;
                clockCycles := 14;
            end;
        end;
        $9B: begin   // OP-Code 0xED9B : OTDMR
            tmpByte := memRead(regHL.Value);
            ioWrite($00, regBC.C, tmpByte);
            Dec(regHL.Value);
            Dec(regBC.C);
            Dec(regBC.B);
            regAF.Flag[C] := (regBC.B = $FF); // C is set if a borrow occurs after B-l; reset otherwise
            regAF.Flag[N] := ((tmpByte and $80) <> $00); // N is set if MSB of memData=1; reset otherwise
            regAF.Flag[PV] := calcParity(regBC.B); // P/V is set if parity in B is even after B-l; reset otherwise
            regAF.Flag[H] := ((regBC.B and $0F) = $0F); // H is set if a borrow from bit 4 of B occurs after B-l; reset otherwise
            regAF.Flag[Z] := (regBC.B = $00); // Z is set if B=OO after B-l; reset otherwise
            regAF.Flag[S] := ((regBC.B and $80) <> $00); // S is set if B is negative after B-l; reset otherwise
            if (not regAF.Flag[Z]) then begin
                regPC.Value := ((regPC.Value - 2) and $FFFF);
                machineCycles := 8;
                clockCycles := 16;
            end
            else begin
                machineCycles := 6;
                clockCycles := 14;
            end;
        end;
        $A0: begin   // OP-Code 0xEDA0 : LDI
            memWrite(regDE.Value, memRead(regHL.Value));
            Inc(regDE.Value);
            Inc(regHL.Value);
            Dec(regBC.Value);
            regAF.Flag[PV] := (regBC.Value <> $0000); // P/V is set if BC-1 ≠ 0; reset otherwise
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            machineCycles := 4;
            clockCycles := 12;
        end;
        $A1: begin   // OP-Code 0xEDA1 : CPI
            tmpByte := memRead(regHL.Value);
            regAF.Flag[H] := ((tmpByte and $0F) > (regAF.A and $0F)); // H is set if borrow from bit 4; reset otherwise
            tmpByte := ((regAF.A - tmpByte) and $FF);
            Inc(regHL.Value);
            Dec(regBC.Value);
            regAF.Flag[S] := ((tmpByte and $80) <> $00); // S is set if result is negative; reset otherwise
            regAF.Flag[Z] := (tmpByte = $00); // Z is set if A is (HL); reset otherwise
            regAF.Flag[PV] := (regBC.Value <> $0000); // P/V is set if BC-1 is not 0; reset otherwise
            regAF.Flag[N] := True;
            machineCycles := 6;
            clockCycles := 12;
        end;
        $A2: begin   // OP-Code 0xEDA2 : INI
            memWrite(regHL.Value, ioRead(regBC.B, regBC.C));
            Inc(regHL.Value);
            Dec(regBC.B);
            regAF.Flag[Z] := (regBC.B = $00); // Z is set if B–1 = 0, reset otherwise
            regAF.Flag[N] := True;
            machineCycles := 4;
            clockCycles := 12;
        end;
        $A3: begin   // OP-Code 0xEDA3 : OUTI
            Dec(regBC.B);
            ioWrite(regBC.B, regBC.C, memRead(regHL.Value));
            Inc(regHL.Value);
            regAF.Flag[Z] := (regBC.B = $00); // Z is set if B–1 = 0, reset otherwise
            regAF.Flag[N] := True;
            machineCycles := 4;
            clockCycles := 12;
        end;
        $A8: begin   // OP-Code 0xEDA8 : LDD
            memWrite(regDE.Value, memRead(regHL.Value));
            Dec(regDE.Value);
            Dec(regHL.Value);
            Dec(regBC.Value);
            regAF.Flag[PV] := (regBC.Value <> $0000); // P/V is set if BC-1 ≠ 0; reset otherwise
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            machineCycles := 4;
            clockCycles := 12;
        end;
        $A9: begin   // OP-Code 0xEDA9 : CPD
            tmpByte := memRead(regHL.Value);
            regAF.Flag[H] := ((tmpByte and $0F) > (regAF.A and $0F)); // H is set if borrow from bit 4; reset otherwise
            tmpByte := ((regAF.A - tmpByte) and $FF);
            Dec(regHL.Value);
            Dec(regBC.Value);
            regAF.Flag[S] := ((tmpByte and $80) <> $00); // S is set if result is negative; reset otherwise
            regAF.Flag[Z] := (tmpByte = $00); // Z is set if A is (HL); reset otherwise
            regAF.Flag[PV] := (regBC.Value <> $0000); // P/V is set if BC-1 is not 0; reset otherwise
            regAF.Flag[N] := True;
            machineCycles := 6;
            clockCycles := 12;
        end;
        $AA: begin   // OP-Code 0xEDAA : IND
            memWrite(regHL.Value, ioRead(regBC.B, regBC.C));
            Dec(regHL.Value);
            Dec(regBC.B);
            regAF.Flag[Z] := (regBC.B = $00); // Z is set if B–1 = 0, reset otherwise
            regAF.Flag[N] := True;
            machineCycles := 4;
            clockCycles := 12;
        end;
        $AB: begin   // OP-Code 0xEDAB : OUTD
            Dec(regBC.B);
            ioWrite(regBC.B, regBC.C, memRead(regHL.Value));
            Dec(regHL.Value);
            regAF.Flag[Z] := (regBC.B = $00); // Z is set if B–1 = 0, reset otherwise
            regAF.Flag[N] := True;
            machineCycles := 4;
            clockCycles := 12;
        end;
        $B0: begin   // OP-Code 0xEDB0 : LDIR
            memWrite(regDE.Value, memRead(regHL.Value));
            Inc(regDE.Value);
            Inc(regHL.Value);
            Dec(regBC.Value);
            regAF.Flag[PV] := False;
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            if (regBC.Value <> $0000) then begin
                regPC.Value := ((regPC.Value - 2) and $FFFF);
                machineCycles := 6;
                clockCycles := 14;
            end
            else begin
                machineCycles := 4;
                clockCycles := 12;
            end;
        end;
        $B1: begin   // OP-Code 0xEDB1 : CPIR
            tmpByte := memRead(regHL.Value);
            regAF.Flag[H] := ((tmpByte and $0F) > (regAF.A and $0F)); // H is set if borrow from bit 4; reset otherwise
            tmpByte := ((regAF.A - tmpByte) and $FF);
            Inc(regHL.Value);
            Dec(regBC.Value);
            regAF.Flag[S] := ((tmpByte and $80) <> $00); // S is set if result is negative; reset otherwise
            regAF.Flag[Z] := (tmpByte = $00); // Z is set if A is (HL); reset otherwise
            regAF.Flag[PV] := (regBC.Value <> $0000); // P/V is set if BC-1 is not 0; reset otherwise
            regAF.Flag[N] := True;
            if (regAF.Flag[PV] and not regAF.Flag[Z]) then begin
                regPC.Value := ((regPC.Value - 2) and $FFFF);
                machineCycles := 8;
                clockCycles := 14;
            end
            else begin
                machineCycles := 6;
                clockCycles := 12;
            end;
        end;
        $B2: begin   // OP-Code 0xEDB2 : INIR
            memWrite(regHL.Value, ioRead(regBC.B, regBC.C));
            Inc(regHL.Value);
            Dec(regBC.B);
            regAF.Flag[Z] := True;
            regAF.Flag[N] := True;
            if (regBC.B <> $00) then begin
                regPC.Value := ((regPC.Value - 2) and $FFFF);
                machineCycles := 6;
                clockCycles := 14;
            end
            else begin
                machineCycles := 4;
                clockCycles := 12;
            end;
        end;
        $B3: begin   // OP-Code 0xEDB3 : OTIR
            Dec(regBC.B);
            ioWrite(regBC.B, regBC.C, memRead(regHL.Value));
            Inc(regHL.Value);
            regAF.Flag[Z] := True;
            regAF.Flag[N] := True;
            if (regBC.B <> $00) then begin
                regPC.Value := ((regPC.Value - 2) and $FFFF);
                machineCycles := 6;
                clockCycles := 14;
            end
            else begin
                machineCycles := 4;
                clockCycles := 12;
            end;
        end;
        $B8: begin   // OP-Code 0xEDB8 : LDDR
            memWrite(regDE.Value, memRead(regHL.Value));
            Dec(regDE.Value);
            Dec(regHL.Value);
            Dec(regBC.Value);
            regAF.Flag[PV] := False;
            regAF.Flag[H] := False;
            regAF.Flag[N] := False;
            if (regBC.Value <> $0000) then begin
                regPC.Value := ((regPC.Value - 2) and $FFFF);
                machineCycles := 6;
                clockCycles := 14;
            end
            else begin
                machineCycles := 4;
                clockCycles := 12;
            end;
        end;
        $B9: begin   // OP-Code 0xEDB9 : CPDR
            tmpByte := memRead(regHL.Value);
            regAF.Flag[H] := ((tmpByte and $0F) > (regAF.A and $0F)); // H is set if borrow from bit 4; reset otherwise
            tmpByte := ((regAF.A - tmpByte) and $FF);
            Dec(regHL.Value);
            Dec(regBC.Value);
            regAF.Flag[S] := ((tmpByte and $80) <> $00); // S is set if result is negative; reset otherwise
            regAF.Flag[Z] := (tmpByte = $00); // Z is set if A is (HL); reset otherwise
            regAF.Flag[PV] := (regBC.Value <> $0000); // P/V is set if BC-1 is not 0; reset otherwise
            regAF.Flag[N] := True;
            if (regAF.Flag[PV] and not regAF.Flag[Z]) then begin
                regPC.Value := ((regPC.Value - 2) and $FFFF);
                machineCycles := 8;
                clockCycles := 14;
            end
            else begin
                machineCycles := 6;
                clockCycles := 12;
            end;
        end;
        $BA: begin   // OP-Code 0xEDBA : INDR
            memWrite(regHL.Value, ioRead(regBC.B, regBC.C));
            Dec(regHL.Value);
            Dec(regBC.B);
            regAF.Flag[Z] := True;
            regAF.Flag[N] := True;
            if (regBC.B <> $00) then begin
                regPC.Value := ((regPC.Value - 2) and $FFFF);
                machineCycles := 6;
                clockCycles := 14;
            end
            else begin
                machineCycles := 4;
                clockCycles := 12;
            end;
        end;
        $BB: begin   // OP-Code 0xEDBB : OTDR
            Dec(regBC.B);
            ioWrite(regBC.B, regBC.C, memRead(regHL.Value));
            Dec(regHL.Value);
            regAF.Flag[Z] := True;
            regAF.Flag[N] := True;
            if (regBC.B <> $00) then begin
                regPC.Value := ((regPC.Value - 2) and $FFFF);
                machineCycles := 6;
                clockCycles := 14;
            end
            else begin
                machineCycles := 4;
                clockCycles := 12;
            end;
        end;
        {$ifndef NOTRAP}
        else begin
            ioITC.bit[TRAP] := True;  // TRAP Flag in ITC-Register setzen
            ioITC.bit[UFO] := False;  // UFO-Flag loeschen, da TRAP in 2. OP-Code aufgetreten
            push(regPC.Value);
            regPC.Value := $0000;
            machineCycles := 4;
            clockCycles := 12;
        end;
          {$endif}
    end;
end;

// --------------------------------------------------------------------------------
end.
