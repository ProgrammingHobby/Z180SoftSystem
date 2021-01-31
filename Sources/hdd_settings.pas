unit Hdd_Settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
    EditBtn, ComCtrls, SpinEx;

type

    { THddSettings }

    THddSettings = class(TForm)
        labelHddDrive: TLabel;
        editHddImageFile: TFileNameEdit;
        editHddSize: TEdit;
        groupboxHddDrive: TGroupBox;
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
        editHddHeads: TEdit;
        editHddTracks: TEdit;
        editHddSectors: TEdit;
        updownHddSectors: TUpDown;
        updownHddTracks: TUpDown;
        updownHddHeads: TUpDown;
        procedure editHddImageFileChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure OnHddSizeChange(Sender: TObject);
        procedure updownHddHeadsClick(Sender: TObject; Button: TUDBtnType);
    private
    var
        oldTracks: integer;
        oldSectors: integer;
        oldHeads: integer;
        oldImageFile: string;

    const
        SECBYTES = 512;

        procedure calcHddSize;
        function calcBitIndex(Value: integer): integer;

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

    if (oldHeads <> StrToInt(editHddHeads.Text)) then begin
        SystemSettings.WriteString('Hdd', 'Heads', editHddHeads.Text);
        SystemHdc.setHddHeads(StrToInt(editHddHeads.Text));
    end;
    if (oldTracks <> StrToInt(editHddTracks.Text)) then begin
        SystemSettings.WriteString('Hdd', 'Tracks', editHddTracks.Text);
        SystemHdc.setHddTracks(StrToInt(editHddTracks.Text));
    end;
    if (oldSectors <> StrToInt(editHddSectors.Text)) then begin
        SystemSettings.WriteString('Hdd', 'Sectors', editHddSectors.Text);
        SystemHdc.setHddSectors(StrToInt(editHddSectors.Text));
    end;
    if (oldImageFile <> editHddImageFile.FileName) then begin
        SystemSettings.WriteString('Hdd', 'ImageFile', editHddImageFile.FileName);
        SystemHdc.setHddImage(editHddImageFile.FileName);
    end;

    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure THddSettings.editHddImageFileChange(Sender: TObject);
begin
    editHddImageFile.SelStart := editHddImageFile.FileName.Length;
    editHddImageFile.Hint := editHddImageFile.FileName;
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
    updownHddHeads.Position := calcBitIndex(oldHeads);
    editHddHeads.Text := IntToStr(1 shl updownHddHeads.Position);
    oldTracks := SystemSettings.ReadString('Hdd', 'Tracks', '246').ToInteger;
    editHddTracks.Text := IntToStr(oldTracks);
    oldSectors := SystemSettings.ReadString('Hdd', 'Sectors', '63').ToInteger;
    editHddSectors.Text := IntToStr(oldSectors);
    oldImageFile := SystemSettings.ReadString('Hdd', 'ImageFile', '');
    editHddImageFile.FileName := oldImageFile;

    calcHddSize;
    groupboxHddDrive.SetFocus;
end;

// --------------------------------------------------------------------------------
procedure THddSettings.OnHddSizeChange(Sender: TObject);
begin
    calcHddSize;
end;

// --------------------------------------------------------------------------------
procedure THddSettings.updownHddHeadsClick(Sender: TObject; Button: TUDBtnType);
begin
    editHddHeads.Text := IntToStr(1 shl updownHddHeads.Position);
end;

// --------------------------------------------------------------------------------
procedure THddSettings.calcHddSize;
var
    tracks, sectors, heads, size: integer;
    sizeView: string;
begin
    tracks := StrToInt(editHddTracks.Text);
    sectors := StrToInt(editHddSectors.Text);
    TryStrToInt(editHddHeads.Text, heads);
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
function THddSettings.calcBitIndex(Value: integer): integer;
begin
    for Result := 0 to 8 do begin
        if ((1 shl Result) >= Value) then
            break;
    end;
end;

// --------------------------------------------------------------------------------
end.
