unit Hardware_Info;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
    ExtCtrls;

type

    { THardwareInfo }

    THardwareInfo = class(TForm)
        memoRtcChip: TMemo;
        memoHddController: TMemo;
        memoFddController: TMemo;
        memoTerminal: TMemo;
        memoZ180Mpu: TMemo;
        pagecontrolHardwareInfo: TPageControl;
        tabRtcChip: TTabSheet;
        tabZ180Mpu: TTabSheet;
        tabTerminal: TTabSheet;
        tabFddController: TTabSheet;
        tabHddController: TTabSheet;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
    private

    public

    end;

var
    HardwareInfo: THardwareInfo;

implementation

{$R *.lfm}

{ THardwareInfo }

uses
    UscaleDPI, System_Settings;

// --------------------------------------------------------------------------------
procedure THardwareInfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    SystemSettings.saveFormState(TForm(self));
    SystemSettings.WriteInteger('HardwareInfo', 'PageIndex', pagecontrolHardwareInfo.PageIndex);
    CloseAction := caFree;
    HardwareInfo := nil;
end;

// --------------------------------------------------------------------------------
procedure THardwareInfo.FormShow(Sender: TObject);
begin
    SystemSettings.restoreFormState(TForm(self));
    pagecontrolHardwareInfo.PageIndex := SystemSettings.ReadInteger('HardwareInfo', 'PageIndex', 0);
    ScaleDPI(self, 96);
end;

// --------------------------------------------------------------------------------
end.

