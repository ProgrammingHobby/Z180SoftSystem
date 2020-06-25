unit Cpu_Io_Register;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
    StdCtrls;

type

    { TCpuIoRegister }

    TCpuIoRegister = class(TForm)
        labelBBR: TLabel;
        labelBCR0H: TLabel;
        labelBCR0L: TLabel;
        labelBCR1H: TLabel;
        labelBCR1L: TLabel;
        labelCBAR: TLabel;
        labelCBR: TLabel;
        labelCNTR: TLabel;
        labelCpuTitle: TLabel;
        labelCsioTitle: TLabel;
        labelDAR0B: TLabel;
        labelDAR0H: TLabel;
        labelDAR0L: TLabel;
        labelDCNTL: TLabel;
        labelDmaTitle: TLabel;
        labelDMODE: TLabel;
        labelDSTAT: TLabel;
        labelFRC: TLabel;
        labelIAR1H: TLabel;
        labelIAR1L: TLabel;
        labelICR: TLabel;
        labelIL: TLabel;
        labelITC: TLabel;
        labelMAR1B: TLabel;
        labelMAR1H: TLabel;
        labelMAR1L: TLabel;
        labelMmuTitle: TLabel;
        labelOMCR: TLabel;
        labelRCR: TLabel;
        labelPrtTitle: TLabel;
        labelAsciTitle: TLabel;
        labelCNTLA0: TLabel;
        labelRDR1: TLabel;
        labelCNTLB0: TLabel;
        labelSAR0B: TLabel;
        labelSAR0H: TLabel;
        labelSAR0L: TLabel;
        labelTCR: TLabel;
        labelTMDR0H: TLabel;
        labelTMDR0L: TLabel;
        labelTMDR1H: TLabel;
        labelTMDR1L: TLabel;
        labelRLDR0H: TLabel;
        labelRLDR0L: TLabel;
        labelRLDR1H: TLabel;
        labelRLDR1L: TLabel;
        labelSTAT0: TLabel;
        labelTDR0: TLabel;
        labelRDR0: TLabel;
        labelCNTLA1: TLabel;
        labelCNTLB1: TLabel;
        labelSTAT1: TLabel;
        labelTDR1: TLabel;
        labelTRD: TLabel;
        labelValueBBR: TLabel;
        labelValueBCR0H: TLabel;
        labelValueBCR0L: TLabel;
        labelValueBCR1H: TLabel;
        labelValueBCR1L: TLabel;
        labelValueCBAR: TLabel;
        labelValueCBR: TLabel;
        labelValueCNTR: TLabel;
        labelValueDAR0B: TLabel;
        labelValueDAR0H: TLabel;
        labelValueDAR0L: TLabel;
        labelValueDCNTL: TLabel;
        labelValueDMODE: TLabel;
        labelValueDSTAT: TLabel;
        labelValueFRC: TLabel;
        labelValueIAR1H: TLabel;
        labelValueIAR1L: TLabel;
        labelValueICR: TLabel;
        labelValueIL: TLabel;
        labelValueITC: TLabel;
        labelValueMAR1B: TLabel;
        labelValueMAR1H: TLabel;
        labelValueMAR1L: TLabel;
        labelValueOMCR: TLabel;
        labelValueRCR: TLabel;
        labelValueSAR0B: TLabel;
        labelValueSAR0H: TLabel;
        labelValueSAR0L: TLabel;
        labelValueTCR: TLabel;
        labelValueTMDR0H: TLabel;
        labelValueTMDR0L: TLabel;
        labelValueTMDR1H: TLabel;
        labelValueTMDR1L: TLabel;
        labelValueRLDR0H: TLabel;
        labelValueRLDR0L: TLabel;
        labelValueRLDR1H: TLabel;
        labelValueRLDR1L: TLabel;
        labelValueCNTLA0: TLabel;
        labelValueRDR1: TLabel;
        labelValueCNTLB0: TLabel;
        labelValueSTAT0: TLabel;
        labelValueTDR0: TLabel;
        labelValueRDR0: TLabel;
        labelValueCNTLA1: TLabel;
        labelValueCNTLB1: TLabel;
        labelValueSTAT1: TLabel;
        labelValueTDR1: TLabel;
        labelValueTRD: TLabel;
        panelDmaCpu: TPanel;
        panelBBR: TPanel;
        panelBCR0H: TPanel;
        panelBCR0L: TPanel;
        panelBCR1H: TPanel;
        panelBCR1L: TPanel;
        panelCBAR: TPanel;
        panelCBR: TPanel;
        panelCNTR: TPanel;
        panelCpu: TPanel;
        panelCpuTitle: TPanel;
        panelCpuValues: TPanel;
        panelCsio: TPanel;
        panelCsioTitle: TPanel;
        panelCsioValues: TPanel;
        panelDAR0B: TPanel;
        panelDAR0H: TPanel;
        panelDAR0L: TPanel;
        panelDCNTL: TPanel;
        panelDMA: TPanel;
        panelDmaData: TPanel;
        panelDmaSpacer1: TPanel;
        panelDmaSpacer2: TPanel;
        panelDmaSpacer3: TPanel;
        panelDmaSpacer4: TPanel;
        panelDmaSpacer5: TPanel;
        panelDmaSpacer6: TPanel;
        panelDmaTitle: TPanel;
        panelDmaValues: TPanel;
        panelDMODE: TPanel;
        panelDSTAT: TPanel;
        panelFRC: TPanel;
        panelIAR1H: TPanel;
        panelIAR1L: TPanel;
        panelICR: TPanel;
        panelIL: TPanel;
        panelITC: TPanel;
        panelMAR1B: TPanel;
        panelMAR1H: TPanel;
        panelMAR1L: TPanel;
        panelMiscellaneous: TPanel;
        panelMmu: TPanel;
        panelMmuCsio: TPanel;
        panelMmuTitle: TPanel;
        panelMmuValues: TPanel;
        panelOMCR: TPanel;
        panelRCR: TPanel;
        panelAsciPrt: TPanel;
        panelPrtSpacer1: TPanel;
        panelPrtSpacer2: TPanel;
        panelPrtSpacer3: TPanel;
        panelPrtValues: TPanel;
        panelPrtTitle: TPanel;
        panelAsciValues: TPanel;
        panelAsciTitle: TPanel;
        panelAsci: TPanel;
        panelPRT: TPanel;
        panelCNTLA0: TPanel;
        panelRDR1: TPanel;
        panelCNTLB0: TPanel;
        panelSAR0B: TPanel;
        panelSAR0H: TPanel;
        panelSAR0L: TPanel;
        panelTCR: TPanel;
        panelTMDR0H: TPanel;
        panelTMDR0L: TPanel;
        panelTMDR1H: TPanel;
        panelTMDR1L: TPanel;
        panelRLDR0H: TPanel;
        panelRLDR0L: TPanel;
        panelRLDR1H: TPanel;
        panelRLDR1L: TPanel;
        panelSTAT0: TPanel;
        panelTDR0: TPanel;
        panelRDR0: TPanel;
        panelCNTLA1: TPanel;
        panelCNTLB1: TPanel;
        panelSTAT1: TPanel;
        panelTDR1: TPanel;
        panelTRD: TPanel;
        panelValueBBR: TPanel;
        panelValueBCR0H: TPanel;
        panelValueBCR0L: TPanel;
        panelValueBCR1H: TPanel;
        panelValueBCR1L: TPanel;
        panelValueCBAR: TPanel;
        panelValueCBR: TPanel;
        panelValueCNTR: TPanel;
        panelValueDAR0B: TPanel;
        panelValueDAR0H: TPanel;
        panelValueDAR0L: TPanel;
        panelValueDCNTL: TPanel;
        panelValueDMODE: TPanel;
        panelValueDSTAT: TPanel;
        panelValueFRC: TPanel;
        panelValueIAR1H: TPanel;
        panelValueIAR1L: TPanel;
        panelValueICR: TPanel;
        panelValueIL: TPanel;
        panelValueITC: TPanel;
        panelValueMAR1B: TPanel;
        panelValueMAR1H: TPanel;
        panelValueMAR1L: TPanel;
        panelValueOMCR: TPanel;
        panelValueRCR: TPanel;
        panelValueSAR0B: TPanel;
        panelValueSAR0H: TPanel;
        panelValueSAR0L: TPanel;
        panelValueTCR: TPanel;
        panelValueTMDR0H: TPanel;
        panelValueTMDR0L: TPanel;
        panelValueTMDR1H: TPanel;
        panelValueTMDR1L: TPanel;
        panelValueRLDR0H: TPanel;
        panelValueRLDR0L: TPanel;
        panelValueRLDR1H: TPanel;
        panelValueRLDR1L: TPanel;
        panelValueCNTLA0: TPanel;
        panelValueRDR1: TPanel;
        panelValueCNTLB0: TPanel;
        panelValueSTAT0: TPanel;
        panelValueTDR0: TPanel;
        panelValueRDR0: TPanel;
        panelValueCNTLA1: TPanel;
        panelValueCNTLB1: TPanel;
        panelValueSTAT1: TPanel;
        panelValueTDR1: TPanel;
        panelValueTRD: TPanel;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure onPanelPaint(Sender: TObject);
        procedure onPanelValuePaint(Sender: TObject);
    private

    public
        procedure showRegisterData;

    end;

var
    CpuIoRegister: TCpuIoRegister;

implementation

{$R *.lfm}

uses UscaleDPI, System_Settings, Z180_CPU;

{ TCpuIoRegister }
// --------------------------------------------------------------------------------
procedure TCpuIoRegister.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    SystemSettings.saveFormState(TForm(self));
    CloseAction := caFree;
    CpuIoRegister := nil;
end;

// --------------------------------------------------------------------------------
procedure TCpuIoRegister.FormShow(Sender: TObject);
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
procedure TCpuIoRegister.onPanelPaint(Sender: TObject);
begin
    with Sender as TPanel do begin
        Canvas.Pen.Color := clGray;
        Canvas.RoundRect(0, 0, Width, Height, 4, 4);
    end;
end;

// --------------------------------------------------------------------------------
procedure TCpuIoRegister.onPanelValuePaint(Sender: TObject);
begin
    with Sender as TPanel do begin
        Canvas.Brush.Color := clCream;
        Canvas.Pen.Color := clGray;
        Canvas.RoundRect(0, 0, Width, Height, 4, 4);
    end;
end;

// --------------------------------------------------------------------------------
procedure TCpuIoRegister.showRegisterData;
var
    ioRegData: Z180Cpu.TIoData;
begin
    ioRegData := Z180Cpu.getIoRegData;
    labelValueCNTLA0.Caption := IntToHex(ioRegData.CNTLA0, 2);
    labelValueCNTLB0.Caption := IntToHex(ioRegData.CNTLB0, 2);
    labelValueSTAT0.Caption := IntToHex(ioRegData.STAT0, 2);
    labelValueTDR0.Caption := IntToHex(ioRegData.TDR0, 2);
    labelValueRDR0.Caption := IntToHex(ioRegData.RDR0, 2);
    labelValueCNTLA1.Caption := IntToHex(ioRegData.CNTLA1, 2);
    labelValueCNTLB1.Caption := IntToHex(ioRegData.CNTLB1, 2);
    labelValueSTAT1.Caption := IntToHex(ioRegData.STAT1, 2);
    labelValueTDR1.Caption := IntToHex(ioRegData.TDR1, 2);
    labelValueRDR1.Caption := IntToHex(ioRegData.RDR1, 2);
    labelValueTCR.Caption := IntToHex(ioRegData.TCR, 2);
    labelValueTMDR0H.Caption := IntToHex(ioRegData.TMDR0H, 2);
    labelValueTMDR0L.Caption := IntToHex(ioRegData.TMDR0L, 2);
    labelValueRLDR0H.Caption := IntToHex(ioRegData.RLDR0H, 2);
    labelValueRLDR0L.Caption := IntToHex(ioRegData.RLDR0L, 2);
    labelValueTMDR1H.Caption := IntToHex(ioRegData.TMDR1H, 2);
    labelValueTMDR1L.Caption := IntToHex(ioRegData.TMDR1L, 2);
    labelValueRLDR1H.Caption := IntToHex(ioRegData.RLDR1H, 2);
    labelValueRLDR1L.Caption := IntToHex(ioRegData.RLDR1L, 2);
    labelValueDMODE.Caption := IntToHex(ioRegData.DMODE, 2);
    labelValueDCNTL.Caption := IntToHex(ioRegData.DCNTL, 2);
    labelValueDSTAT.Caption := IntToHex(ioRegData.DSTAT, 2);
    labelValueSAR0B.Caption := IntToHex(ioRegData.SAR0B, 2);
    labelValueSAR0H.Caption := IntToHex(ioRegData.SAR0H, 2);
    labelValueSAR0L.Caption := IntToHex(ioRegData.SAR0L, 2);
    labelValueDAR0B.Caption := IntToHex(ioRegData.DAR0B, 2);
    labelValueDAR0H.Caption := IntToHex(ioRegData.DAR0H, 2);
    labelValueDAR0L.Caption := IntToHex(ioRegData.DAR0L, 2);
    labelValueBCR0H.Caption := IntToHex(ioRegData.BCR0H, 2);
    labelValueBCR0L.Caption := IntToHex(ioRegData.BCR0L, 2);
    labelValueMAR1B.Caption := IntToHex(ioRegData.MAR1B, 2);
    labelValueMAR1H.Caption := IntToHex(ioRegData.MAR1H, 2);
    labelValueMAR1L.Caption := IntToHex(ioRegData.MAR1L, 2);
    labelValueIAR1H.Caption := IntToHex(ioRegData.IAR1H, 2);
    labelValueIAR1L.Caption := IntToHex(ioRegData.IAR1L, 2);
    labelValueBCR1H.Caption := IntToHex(ioRegData.BCR1H, 2);
    labelValueBCR1L.Caption := IntToHex(ioRegData.BCR1L, 2);
    labelValueOMCR.Caption := IntToHex(ioRegData.OMCR, 2);
    labelValueICR.Caption := IntToHex(ioRegData.ICR, 2);
    labelValueFRC.Caption := IntToHex(ioRegData.FRC, 2);
    labelValueRCR.Caption := IntToHex(ioRegData.RCR, 2);
    labelValueITC.Caption := IntToHex(ioRegData.ITC, 2);
    labelValueIL.Caption := IntToHex(ioRegData.IL, 2);
    labelValueCBAR.Caption := IntToHex(ioRegData.CBAR, 2);
    labelValueBBR.Caption := IntToHex(ioRegData.BBR, 2);
    labelValueCBR.Caption := IntToHex(ioRegData.CBR, 2);
    labelValueCNTR.Caption := IntToHex(ioRegData.CNTR, 2);
    labelValueTRD.Caption := IntToHex(ioRegData.TRD, 2);
end;

// --------------------------------------------------------------------------------
end.

