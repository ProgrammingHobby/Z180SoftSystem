unit Hdd_Settings;

{$mode objfpc}{$H+}

interface

//uses
//    Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, EditBtn, SpinEx, Types;

type

    { THddSettings }

    THddSettings = class(TForm)
        comboboxHddSectorBytes: TComboBox;
        editHddImageFile: TFileNameEdit;
        editHddSize: TEdit;
        groupboxHddGeometrie: TGroupBox;
        groupboxHddImage: TGroupBox;
        labelHddSectorBytes: TLabel;
        labelHddHeads: TLabel;
        labelHddSectors: TLabel;
        labelHddSize: TLabel;
        labelHddTracks: TLabel;
        panelHddSectorBytes: TPanel;
        panelHddHeads: TPanel;
        panelHddSectors: TPanel;
        panelHddSize: TPanel;
        panelHddTracks: TPanel;
        spineditHddHeads: TSpinEditEx;
        spineditHddSectors: TSpinEditEx;
        spineditHddTracks: TSpinEditEx;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure HddGeometryChange(Sender: TObject);
    private

    public

    end;

var
    HddSettings: THddSettings;

implementation

{$R *.lfm}

uses UscaleDPI, System_Settings;

{ THddSettings }

// --------------------------------------------------------------------------------
procedure THddSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    SystemSettings.saveFormState(TForm(self));
    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure THddSettings.FormShow(Sender: TObject);
begin
    SystemSettings.restoreFormState(TForm(self));
    ScaleDPI(self, 96);
    self.SetAutoSize(True);
    Constraints.MinWidth := Width;
    Constraints.MaxWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
end;

procedure THddSettings.HddGeometryChange(Sender: TObject);
var
    tracks, sectors, bytes, heads, size: integer;
    sizeView: string;
begin
    tracks := spineditHddTracks.Value;
    sectors := spineditHddSectors.Value;
    bytes := comboboxHddSectorBytes.Items[comboboxHddSectorBytes.ItemIndex].ToInteger;
    heads := spineditHddHeads.Value;
    size := tracks * sectors * bytes * heads;
    if ((size div 1048576) > 0) then begin
        sizeView := FloatToStrF((size / 1048576), ffNumber, 15, 2) + 'MB';
    end
    else if ((size div 1024) > 0) then begin
        sizeView := FloatToStrF((size / 1024), ffNumber, 15, 2) + 'KB';
    end
    else begin
        sizeView := IntToStr(size) + 'Byte';
    end;
    editHddSize.Text := sizeView;
end;

// --------------------------------------------------------------------------------
end.

