unit Fdd_Settings;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, EditBtn;

type

    { TFddSettings }

    TFddSettings = class(TForm)
        labelFdd1: TLabel;
        labelFdd0: TLabel;
        comboboxFdd0Sectors: TComboBox;
        comboboxFdd1Sectors: TComboBox;
        comboboxFdd0Sides: TComboBox;
        comboboxFdd1Sides: TComboBox;
        comboboxFdd0Tracks: TComboBox;
        comboboxFdd1Tracks: TComboBox;
        editFdd0ImageFile: TFileNameEdit;
        editFdd1ImageFile: TFileNameEdit;
        editFdd0Size: TEdit;
        editFdd1Size: TEdit;
        groupboxFdd0: TGroupBox;
        groupboxFdd1Geometrie: TGroupBox;
        groupboxFdd1Image: TGroupBox;
        groupboxFdd1: TGroupBox;
        labelFdd0Sectors: TLabel;
        labelFdd1Sectors: TLabel;
        labelFdd0Sides: TLabel;
        labelFdd1Sides: TLabel;
        labelFdd0Size: TLabel;
        labelFdd1Size: TLabel;
        labelFdd0Tracks: TLabel;
        groupboxFdd0Geometrie: TGroupBox;
        groupboxFdd0Image: TGroupBox;
        labelFdd1Tracks: TLabel;
        panelFdd0Sectors: TPanel;
        panelFdd1Sectors: TPanel;
        panelFdd0Sides: TPanel;
        panelFdd1Sides: TPanel;
        panelFdd0Size: TPanel;
        panelFdd1Size: TPanel;
        panelFdd0Tracks: TPanel;
        panelFdd1Tracks: TPanel;
        procedure editFdd0ImageFileChange(Sender: TObject);
        procedure editFdd1ImageFileChange(Sender: TObject);
        procedure onFdd0GeometryChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure onFdd1GeometryChange(Sender: TObject);
    private
        type
        TFddData = record
            oldSides: string;
            oldTracks: string;
            oldSectors: string;
            oldImageFile: string;
        end;

    var
        Fdd0, Fdd1: TFddData;

    const
        SECBYTES = 512;

        procedure calcFdd0Size;
        procedure calcFdd1Size;

    public

    end;

var
    FddSettings: TFddSettings;

implementation

{$R *.lfm}

uses UscaleDPI, System_Settings, System_Fdc;

{ TFddSettings }

// --------------------------------------------------------------------------------
procedure TFddSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    SystemSettings.saveFormState(TForm(self));

    if (Fdd0.oldSides <> comboboxFdd0Sides.Items[comboboxFdd0Sides.ItemIndex]) then begin
        SystemSettings.WriteString('Fdd0', 'Sides', comboboxFdd0Sides.Items[comboboxFdd0Sides.ItemIndex]);
        SystemFdc.setFdd0Sides(comboboxFdd0Sides.Items[comboboxFdd0Sides.ItemIndex].ToInteger);
    end;
    if (Fdd0.oldTracks <> comboboxFdd0Tracks.Items[comboboxFdd0Tracks.ItemIndex]) then begin
        SystemSettings.WriteString('Fdd0', 'Tracks', comboboxFdd0Tracks.Items[comboboxFdd0Tracks.ItemIndex]);
        SystemFdc.setFdd0Tracks(comboboxFdd0Tracks.Items[comboboxFdd0Tracks.ItemIndex].ToInteger);
    end;
    if (Fdd0.oldSectors <> comboboxFdd0Sectors.Items[comboboxFdd0Sectors.ItemIndex]) then begin
        SystemSettings.WriteString('Fdd0', 'Sectors', comboboxFdd0Sectors.Items[comboboxFdd0Sectors.ItemIndex]);
        SystemFdc.setFdd0Sectors(comboboxFdd0Sectors.Items[comboboxFdd0Sectors.ItemIndex].ToInteger);
    end;
    if (Fdd0.oldImageFile <> editFdd0ImageFile.FileName) then begin
        SystemSettings.WriteString('Fdd0', 'ImageFile', editFdd0ImageFile.FileName);
        SystemFdc.setFdd0Image(editFdd0ImageFile.FileName);
    end;

    if (Fdd1.oldSides <> comboboxFdd1Sides.Items[comboboxFdd1Sides.ItemIndex]) then begin
        SystemSettings.WriteString('Fdd1', 'Sides', comboboxFdd1Sides.Items[comboboxFdd1Sides.ItemIndex]);
        SystemFdc.setFdd1Sides(comboboxFdd1Sides.Items[comboboxFdd1Sides.ItemIndex].ToInteger);
    end;
    if (Fdd1.oldTracks <> comboboxFdd1Tracks.Items[comboboxFdd1Tracks.ItemIndex]) then begin
        SystemSettings.WriteString('Fdd1', 'Tracks', comboboxFdd1Tracks.Items[comboboxFdd1Tracks.ItemIndex]);
        SystemFdc.setFdd1Tracks(comboboxFdd1Tracks.Items[comboboxFdd1Tracks.ItemIndex].ToInteger);
    end;
    if (Fdd1.oldSectors <> comboboxFdd1Sectors.Items[comboboxFdd1Sectors.ItemIndex]) then begin
        SystemSettings.WriteString('Fdd1', 'Sectors', comboboxFdd1Sectors.Items[comboboxFdd1Sectors.ItemIndex]);
        SystemFdc.setFdd1Sectors(comboboxFdd1Sectors.Items[comboboxFdd1Sectors.ItemIndex].ToInteger);
    end;
    if (Fdd1.oldImageFile <> editFdd1ImageFile.FileName) then begin
        SystemSettings.WriteString('Fdd1', 'ImageFile', editFdd1ImageFile.FileName);
        SystemFdc.setFdd1Image(editFdd1ImageFile.FileName);
    end;

    CloseAction := caFree;
end;

// --------------------------------------------------------------------------------
procedure TFddSettings.onFdd0GeometryChange(Sender: TObject);
begin
    calcFdd0Size;
end;

// --------------------------------------------------------------------------------
procedure TFddSettings.editFdd0ImageFileChange(Sender: TObject);
begin
    editFdd0ImageFile.SelStart := editFdd0ImageFile.FileName.Length;
    editFdd0ImageFile.Hint := editFdd0ImageFile.FileName;
end;

// --------------------------------------------------------------------------------
procedure TFddSettings.editFdd1ImageFileChange(Sender: TObject);
begin
    editFdd1ImageFile.SelStart := editFdd1ImageFile.FileName.Length;
    editFdd1ImageFile.Hint := editFdd1ImageFile.FileName;
end;

// --------------------------------------------------------------------------------
procedure TFddSettings.FormShow(Sender: TObject);
var
    ItemIndex: integer;
begin
    SystemSettings.restoreFormState(TForm(self));
    ScaleDPI(self, 96);
    self.SetAutoSize(True);
    Constraints.MinWidth := Width;
    Constraints.MaxWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
    Fdd0.oldSides := SystemSettings.ReadString('Fdd0', 'Sides', '2');
    ItemIndex := comboboxFdd0Sides.Items.IndexOf(Fdd0.oldSides);
    if (ItemIndex = -1) then begin
        ItemIndex := 1;
    end;
    comboboxFdd0Sides.ItemIndex := ItemIndex;
    Fdd0.oldTracks := SystemSettings.ReadString('Fdd0', 'Tracks', '80');
    ItemIndex := comboboxFdd0Tracks.Items.IndexOf(Fdd0.oldTracks);
    if (ItemIndex = -1) then begin
        ItemIndex := 1;
    end;
    comboboxFdd0Tracks.ItemIndex := ItemIndex;
    Fdd0.oldSectors := SystemSettings.ReadString('Fdd0', 'Sectors', '9');
    ItemIndex := comboboxFdd0Sectors.Items.IndexOf(Fdd0.oldSectors);
    if (ItemIndex = -1) then begin
        ItemIndex := 1;
    end;
    comboboxFdd0Sectors.ItemIndex := ItemIndex;
    Fdd0.oldImageFile := SystemSettings.ReadString('Fdd0', 'ImageFile', '');
    editFdd0ImageFile.FileName := Fdd0.oldImageFile;
    calcFdd0Size;

    Fdd1.oldSides := SystemSettings.ReadString('Fdd1', 'Sides', '2');
    ItemIndex := comboboxFdd1Sides.Items.IndexOf(Fdd1.oldSides);
    if (ItemIndex = -1) then begin
        ItemIndex := 1;
    end;
    comboboxFdd1Sides.ItemIndex := ItemIndex;
    Fdd1.oldTracks := SystemSettings.ReadString('Fdd1', 'Tracks', '80');
    ItemIndex := comboboxFdd1Tracks.Items.IndexOf(Fdd1.oldTracks);
    if (ItemIndex = -1) then begin
        ItemIndex := 1;
    end;
    comboboxFdd1Tracks.ItemIndex := ItemIndex;
    Fdd1.oldSectors := SystemSettings.ReadString('Fdd1', 'Sectors', '9');
    ItemIndex := comboboxFdd1Sectors.Items.IndexOf(Fdd1.oldSectors);
    if (ItemIndex = -1) then begin
        ItemIndex := 1;
    end;
    comboboxFdd1Sectors.ItemIndex := ItemIndex;
    Fdd1.oldImageFile := SystemSettings.ReadString('Fdd1', 'ImageFile', '');
    editFdd1ImageFile.FileName := Fdd1.oldImageFile;
    calcFdd1Size;

    groupboxFdd0.SetFocus;
end;

// --------------------------------------------------------------------------------
procedure TFddSettings.onFdd1GeometryChange(Sender: TObject);
begin
    calcFdd1Size;
end;

// --------------------------------------------------------------------------------
procedure TFddSettings.calcFdd0Size;
var
    tracks, sectors, sides, size: integer;
begin
    tracks := comboboxFdd0Tracks.Items[comboboxFdd0Tracks.ItemIndex].ToInteger;
    sectors := comboboxFdd0Sectors.Items[comboboxFdd0Sectors.ItemIndex].ToInteger;
    sides := comboboxFdd0Sides.Items[comboboxFdd0Sides.ItemIndex].ToInteger;
    size := tracks * sectors * SECBYTES * sides;
    editFdd0Size.Text := IntToStr(size div 1024) + 'KB';
end;

// --------------------------------------------------------------------------------
procedure TFddSettings.calcFdd1Size;
var
    tracks, sectors, sides, size: integer;
begin
    tracks := comboboxFdd1Tracks.Items[comboboxFdd1Tracks.ItemIndex].ToInteger;
    sectors := comboboxFdd1Sectors.Items[comboboxFdd1Sectors.ItemIndex].ToInteger;
    sides := comboboxFdd1Sides.Items[comboboxFdd1Sides.ItemIndex].ToInteger;
    size := tracks * sectors * SECBYTES * sides;
    editFdd1Size.Text := IntToStr(size div 1024) + 'KB';

end;

// --------------------------------------------------------------------------------
end.

