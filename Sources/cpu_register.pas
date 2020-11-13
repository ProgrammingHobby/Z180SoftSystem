unit Cpu_Register;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

    { TCpuRegister }

    TCpuRegister = class(TForm)
        shapeIFF1: TShape;
        shapeIFF2: TShape;
        labelIFF1: TLabel;
        labelIFF2: TLabel;
        labelIntMode: TLabel;
        labelRegIX: TLabel;
        labelRegIY: TLabel;
        labelRegSP: TLabel;
        labelRegSPi: TLabel;
        labelRegI: TLabel;
        labelRegPC: TLabel;
        labelRegPCi: TLabel;
        labelRegR: TLabel;
        labelSpecialRegisters: TLabel;
        labelFlagC1: TLabel;
        labelFlagC2: TLabel;
        labelFlagH1: TLabel;
        labelFlagH2: TLabel;
        labelFlagN1: TLabel;
        labelFlagN2: TLabel;
        labelFlagP1: TLabel;
        labelFlagP2: TLabel;
        labelFlagS1: TLabel;
        labelFlagS2: TLabel;
        labelFlagZ1: TLabel;
        labelFlagZ2: TLabel;
        labelRegA1: TLabel;
        labelRegA2: TLabel;
        labelRegB1: TLabel;
        labelRegB2: TLabel;
        labelRegBC1: TLabel;
        labelRegBC2: TLabel;
        labelRegBCi1: TLabel;
        labelRegBCi2: TLabel;
        labelRegC1: TLabel;
        labelRegC2: TLabel;
        labelRegD1: TLabel;
        labelRegD2: TLabel;
        labelRegDE1: TLabel;
        labelRegDE2: TLabel;
        labelRegDEi1: TLabel;
        labelRegDEi2: TLabel;
        labelRegE1: TLabel;
        labelRegE2: TLabel;
        labelRegF1: TLabel;
        labelRegF2: TLabel;
        labelRegH1: TLabel;
        labelRegH2: TLabel;
        labelRegHL1: TLabel;
        labelRegHL2: TLabel;
        labelRegHLi1: TLabel;
        labelRegHLi2: TLabel;
        labelRegisterSet1: TLabel;
        labelRegisterSet2: TLabel;
        labelRegL1: TLabel;
        labelRegL2: TLabel;
        labelValueA1: TLabel;
        labelValueA2: TLabel;
        labelValueB1: TLabel;
        labelValueB2: TLabel;
        labelValueBC1: TLabel;
        labelValueBC2: TLabel;
        labelValueIntMode: TLabel;
        labelValueIX: TLabel;
        labelValueIY: TLabel;
        labelValueBCi1: TLabel;
        labelValueBCi2: TLabel;
        labelValueC1: TLabel;
        labelValueC2: TLabel;
        labelValueD1: TLabel;
        labelValueD2: TLabel;
        labelValueDE1: TLabel;
        labelValueDE2: TLabel;
        labelValueSP: TLabel;
        labelValueDEi1: TLabel;
        labelValueDEi2: TLabel;
        labelValueSPi: TLabel;
        labelValueE1: TLabel;
        labelValueE2: TLabel;
        labelValueF1: TLabel;
        labelValueF2: TLabel;
        labelValueI: TLabel;
        labelValueH1: TLabel;
        labelValueH2: TLabel;
        labelValueHL1: TLabel;
        labelValueHL2: TLabel;
        labelValuePC: TLabel;
        labelValueHLi1: TLabel;
        labelValueHLi2: TLabel;
        labelValuePCi: TLabel;
        labelValueL1: TLabel;
        labelValueL2: TLabel;
        labelValueR: TLabel;
        panelSpaceHolder2: TPanel;
        panelSpaceIFF1: TPanel;
        panelIFF1: TPanel;
        panelSpaceIFF2: TPanel;
        panelIFF2: TPanel;
        panelIntMode: TPanel;
        panelRegIR: TPanel;
        panelRegIX: TPanel;
        panelRegIY: TPanel;
        panelRegSP: TPanel;
        panelRegSPi: TPanel;
        panelRegI: TPanel;
        panelRegPC: TPanel;
        panelRegPCi: TPanel;
        panelRegR: TPanel;
        panelSpaceRegI: TPanel;
        panelSpaceRegR: TPanel;
        panelSpecialRegistersData: TPanel;
        panelSpecialRegisters: TPanel;
        panelFlagC1: TPanel;
        panelFlagC2: TPanel;
        panelFlagH1: TPanel;
        panelFlagH2: TPanel;
        panelFlagN1: TPanel;
        panelFlagN2: TPanel;
        panelFlagP1: TPanel;
        panelFlagP2: TPanel;
        panelFlagS1: TPanel;
        panelFlagS2: TPanel;
        panelFlagZ1: TPanel;
        panelFlagZ2: TPanel;
        panelFlagSZH: TPanel;
        panelPNC1: TPanel;
        panelPNC2: TPanel;
        panelRegA1: TPanel;
        panelRegA2: TPanel;
        panelRegB1: TPanel;
        panelRegB2: TPanel;
        panelRegBC1: TPanel;
        panelRegBC2: TPanel;
        panelRegBCi1: TPanel;
        panelRegBCi2: TPanel;
        panelRegC1: TPanel;
        panelRegC2: TPanel;
        panelRegD1: TPanel;
        panelRegD2: TPanel;
        panelRegData1: TPanel;
        panelRegData2: TPanel;
        panelRegDE1: TPanel;
        panelRegDE2: TPanel;
        panelRegDEi1: TPanel;
        panelRegDEi2: TPanel;
        panelRegE1: TPanel;
        panelRegE2: TPanel;
        panelRegF1: TPanel;
        panelRegF2: TPanel;
        panelRegH1: TPanel;
        panelRegH2: TPanel;
        panelRegHL1: TPanel;
        panelRegHL2: TPanel;
        panelRegHLi1: TPanel;
        panelRegHLi2: TPanel;
        panelRegisterSet1: TPanel;
        panelRegisterSet2: TPanel;
        panelRegL1: TPanel;
        panelRegL2: TPanel;
        panelSZH1: TPanel;
        panelSZH2: TPanel;
        panelSpaceHolder1: TPanel;
        panelValueA1: TPanel;
        panelValueA2: TPanel;
        panelValueB1: TPanel;
        panelValueB2: TPanel;
        panelValueBC1: TPanel;
        panelValueBC2: TPanel;
        panelValueIntMode: TPanel;
        panelValueIX: TPanel;
        panelValueIY: TPanel;
        panelValueBCi1: TPanel;
        panelValueBCi2: TPanel;
        panelValueC1: TPanel;
        panelValueC2: TPanel;
        panelValueD1: TPanel;
        panelValueD2: TPanel;
        panelValueDE1: TPanel;
        panelValueDE2: TPanel;
        panelValueSP: TPanel;
        panelValueDEi1: TPanel;
        panelValueDEi2: TPanel;
        panelValueSPi: TPanel;
        panelValueE1: TPanel;
        panelValueE2: TPanel;
        panelValueF1: TPanel;
        panelValueF2: TPanel;
        panelValueI: TPanel;
        panelValueH1: TPanel;
        panelValueH2: TPanel;
        panelValueHL1: TPanel;
        panelValueHL2: TPanel;
        panelValuePC: TPanel;
        panelValueHLi1: TPanel;
        panelValueHLi2: TPanel;
        panelValuePCi: TPanel;
        panelValueL1: TPanel;
        panelValueL2: TPanel;
        panelValueR: TPanel;
        shapeFlagC1: TShape;
        shapeFlagC2: TShape;
        shapeFlagH1: TShape;
        shapeFlagH2: TShape;
        shapeFlagN1: TShape;
        shapeFlagN2: TShape;
        shapeFlagP1: TShape;
        shapeFlagP2: TShape;
        shapeFlagS1: TShape;
        shapeFlagS2: TShape;
        shapeFlagZ1: TShape;
        shapeFlagZ2: TShape;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure onPanelValuePaint(Sender: TObject);
        procedure onPanelPaint(Sender: TObject);
    private
        procedure showFlagBit(shape: TShape; flag: boolean);
        // Flag-Bits im Z180 Flag-Register
    const
        Flag_C: byte = $01;
        Flag_N: byte = $02;
        Flag_PV: byte = $04;
        Flag_H: byte = $10;
        Flag_Z: byte = $40;
        Flag_S: byte = $80;

    public
        procedure showRegisterData;
    end;

var
    CpuRegister: TCpuRegister;

implementation

{$R *.lfm}

uses UscaleDPI, System_Settings, Z180_CPU;

{ TCpuRegister }

// --------------------------------------------------------------------------------
procedure TCpuRegister.FormShow(Sender: TObject);
begin
    SystemSettings.restoreFormState(TForm(self));
    self.SetAutoSize(True);
    Constraints.MinWidth := Width;
    Constraints.MaxWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
    ScaleDPI(self, 96);
    DisableAutoSizing;
    showRegisterData;
end;

// --------------------------------------------------------------------------------
procedure TCpuRegister.onPanelValuePaint(Sender: TObject);
begin
    with Sender as TPanel do begin
        Canvas.Brush.Color := clCream;
        Canvas.Pen.Color := clGray;
        Canvas.RoundRect(0, 0, Width, Height, 4, 4);
    end;
end;

// --------------------------------------------------------------------------------
procedure TCpuRegister.onPanelPaint(Sender: TObject);
begin
    with Sender as TPanel do begin
        Canvas.Pen.Color := clGray;
        Canvas.RoundRect(0, 0, Width, Height, 4, 4);
    end;
end;

// --------------------------------------------------------------------------------
procedure TCpuRegister.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    SystemSettings.saveFormState(TForm(self));
    CloseAction := caFree;
    CpuRegister := nil;
end;

// --------------------------------------------------------------------------------
procedure TCpuRegister.showFlagBit(shape: TShape; flag: boolean);
begin
    if (flag = True) then begin
        shape.brush.Color := clBlue;
    end
    else begin
        shape.brush.Color := clWhite;
    end;
end;

// --------------------------------------------------------------------------------
procedure TCpuRegister.showRegisterData;
var
    coreData: Z180Cpu.TCoreData;
begin
    coreData := Z180Cpu.getCoreData;
    labelValueA1.Caption := IntToHex(coreData.A1, 2);
    labelValueF1.Caption := IntToHex(coreData.F1, 2);
    showFlagBit(shapeFlagS1, ((coreData.F1 and Flag_S) <> 0));
    showFlagBit(shapeFlagZ1, ((coreData.F1 and Flag_Z) <> 0));
    showFlagBit(shapeFlagH1, ((coreData.F1 and Flag_H) <> 0));
    showFlagBit(shapeFlagP1, ((coreData.F1 and Flag_PV) <> 0));
    showFlagBit(shapeFlagN1, ((coreData.F1 and Flag_N) <> 0));
    showFlagBit(shapeFlagC1, ((coreData.F1 and Flag_C) <> 0));

    labelValueB1.Caption := IntToHex(coreData.B1, 2);
    labelValueC1.Caption := IntToHex(coreData.C1, 2);
    labelValueBC1.Caption := IntToHex(coreData.BC1, 4);
    labelValueBCi1.Caption := IntToHex(coreData.BCi1, 2);

    labelValueD1.Caption := IntToHex(coreData.D1, 2);
    labelValueE1.Caption := IntToHex(coreData.E1, 2);
    labelValueDE1.Caption := IntToHex(coreData.DE1, 4);
    labelValueDEi1.Caption := IntToHex(coreData.DEi1, 2);

    labelValueH1.Caption := IntToHex(coreData.H1, 2);
    labelValueL1.Caption := IntToHex(coreData.L1, 2);
    labelValueHL1.Caption := IntToHex(coreData.HL1, 4);
    labelValueHLi1.Caption := IntToHex(coreData.HLi1, 2);

    labelValueA2.Caption := IntToHex(coreData.A2, 2);
    labelValueF2.Caption := IntToHex(coreData.F2, 2);
    showFlagBit(shapeFlagS2, ((coreData.F2 and Flag_S) <> 0));
    showFlagBit(shapeFlagZ2, ((coreData.F2 and Flag_Z) <> 0));
    showFlagBit(shapeFlagH2, ((coreData.F2 and Flag_H) <> 0));
    showFlagBit(shapeFlagP2, ((coreData.F2 and Flag_PV) <> 0));
    showFlagBit(shapeFlagN2, ((coreData.F2 and Flag_N) <> 0));
    showFlagBit(shapeFlagC2, ((coreData.F2 and Flag_C) <> 0));

    labelValueB2.Caption := IntToHex(coreData.B2, 2);
    labelValueC2.Caption := IntToHex(coreData.C2, 2);
    labelValueBC2.Caption := IntToHex(coreData.BC2, 4);
    labelValueBCi2.Caption := IntToHex(coreData.BCi2, 2);

    labelValueD2.Caption := IntToHex(coreData.D2, 2);
    labelValueE2.Caption := IntToHex(coreData.E2, 2);
    labelValueDE2.Caption := IntToHex(coreData.DE2, 4);
    labelValueDEi2.Caption := IntToHex(coreData.DEi2, 2);

    labelValueH2.Caption := IntToHex(coreData.H2, 2);
    labelValueL2.Caption := IntToHex(coreData.L2, 2);
    labelValueHL2.Caption := IntToHex(coreData.HL2, 4);
    labelValueHLi2.Caption := IntToHex(coreData.HLi2, 2);

    labelValueI.Caption := IntToHex(coreData.I, 2);
    labelValueR.Caption := IntToHex(coreData.R, 2);
    labelValueIX.Caption := IntToHex(coreData.IX, 4);
    labelValueIY.Caption := IntToHex(coreData.IY, 4);
    labelValueSP.Caption := IntToHex(coreData.SP, 4);
    labelValueSPi.Caption := IntToHex(coreData.SPi, 2);
    labelValuePC.Caption := IntToHex(coreData.PC, 4);
    labelValuePCi.Caption := IntToHex(coreData.PCi, 2);

    showFlagBit(shapeIFF1, coreData.IFF1);
    showFlagBit(shapeIFF2, coreData.IFF2);
end;

// --------------------------------------------------------------------------------
end.
