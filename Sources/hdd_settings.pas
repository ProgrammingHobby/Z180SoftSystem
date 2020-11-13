unit Hdd_Settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, EditBtn, SpinEx;

type

    { THddSettings }

    THddSettings = class(TForm)
        editHddImageFile: TFileNameEdit;
        editHddSize: TEdit;
        groupboxHddGeometrie: TGroupBox;
        groupboxHddImage: TGroupBox;
        labelHddHeads: TLabel;
        labelHddSectors: TLabel;
        labelHddSize: TLabel;
        labelHddTracks: TLabel;
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
    var
        oldTracks: integer;
        oldSectors: integer;
        oldHeads: integer;
        oldImageFile: string;

    const
        SECBYTES = 512;

        procedure calcHddSize;

    public

    end;

var
    HddSettings: THddSettings;

implementation

{$R *.lfm}

uses UscaleDPI, System_Settings, System_Hdc;

{ THddSettings }

// --------------------------------------------------------------------------------
procedure THddSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    SystemSettings.saveFormState(TForm(self));

    if (oldHeads <> spineditHddHeads.Value) then begin
        SystemSettings.WriteString('Hdd', 'Heads', IntToStr(spineditHddHeads.Value));
        SystemHdc.setHddHeads(spineditHddHeads.Value);
    end;
    if (oldTracks <> spineditHddTracks.Value) then begin
        SystemSettings.WriteString('Hdd', 'Tracks', IntToStr(spineditHddTracks.Value));
        SystemHdc.setHddTracks(spineditHddTracks.Value);
    end;
    if (oldSectors <> spineditHddSectors.Value) then begin
        SystemSettings.WriteString('Hdd', 'Sectors', IntToStr(spineditHddSectors.Value));
        SystemHdc.setHddSectors(spineditHddSectors.Value);
    end;
    if (oldImageFile <> editHddImageFile.FileName) then begin
        SystemSettings.WriteString('Hdd', 'ImageFile', editHddImageFile.FileName);
        SystemHdc.setHddImage(editHddImageFile.FileName);
    end;

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

    oldHeads := SystemSettings.ReadString('Hdd', 'Heads', '16').ToInteger;
    spineditHddHeads.Value := oldHeads;
    oldTracks := SystemSettings.ReadString('Hdd', 'Tracks', '246').ToInteger;
    spineditHddTracks.Value := oldTracks;
    oldSectors := SystemSettings.ReadString('Hdd', 'Sectors', '63').ToInteger;
    spineditHddSectors.Value := oldSectors;
    oldImageFile := SystemSettings.ReadString('Hdd', 'ImageFile', '');
    editHddImageFile.FileName := oldImageFile;

    calcHddSize;
end;

// --------------------------------------------------------------------------------
procedure THddSettings.HddGeometryChange(Sender: TObject);
begin
    calcHddSize;
end;

// --------------------------------------------------------------------------------
procedure THddSettings.calcHddSize;
var
    tracks, sectors, heads, size: integer;
    sizeView: string;
begin
    tracks := spineditHddTracks.Value;
    sectors := spineditHddSectors.Value;
    heads := spineditHddHeads.Value;
    size := tracks * sectors * SECBYTES * heads;
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
