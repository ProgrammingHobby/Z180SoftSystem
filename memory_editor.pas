unit Memory_Editor;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
    Dialogs, ComCtrls, StdCtrls, Grids, MaskEdit, Types;

type

    { TMemoryEditor }

    TMemoryEditor = class(TForm)
        editMemoryCell: TEdit;
        gridSystemMemory: TDrawGrid;
        statusBar: TStatusBar;
        procedure editMemoryCellEditingDone(Sender: TObject);
        procedure editMemoryCellKeyPress(Sender: TObject; var Key: char);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
        procedure gridSystemMemoryDblClick(Sender: TObject);
        procedure gridSystemMemoryDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
        procedure gridSystemMemorySelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
        procedure statusBarResize(Sender: TObject);

    private
        CpuProgrammCounter: DWord;

    public
        procedure showMemoryData;
        procedure memoryChanged;

    end;

var
    MemoryEditor: TMemoryEditor;

implementation

{$R *.lfm}

{ TMemoryEditor }

uses
    LCLIntf, LCLType, UscaleDPI, System_Settings, strutils, System_Memory, Z180_CPU;

// --------------------------------------------------------------------------------
procedure TMemoryEditor.FormShow(Sender: TObject);
begin
    with gridSystemMemory do begin
        BeginUpdate;
        ColCount := 17;
        Font.Name := 'Droid Sans Mono';
        Font.Size := 10;
        Canvas.Font.Style := [fsBold];
        DefaultRowHeight := Canvas.TextExtent('X').Height + Canvas.TextExtent('|').Width;
        DefaultColWidth := Canvas.TextExtent('XXX').Width;
        ColWidths[0] := Canvas.TextExtent('XXXXXX').Width;
        EndUpdate();
    end;

    editMemoryCell.Font.Name := 'Droid Sans Mono';
    editMemoryCell.Font.Size := 10;
    Width := gridSystemMemory.ColWidths[0] + (16 * gridSystemMemory.DefaultColWidth) + GetSystemMetrics(SM_CYHSCROLL) + 2;
    Height := (17 * gridSystemMemory.DefaultRowHeight) + statusBar.Height + 2;
    Constraints.MinWidth := Width;
    Constraints.MaxWidth := Width;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
    SystemSettings.restoreFormState(TForm(self));
    ScaleDPI(self, 96);
    memoryChanged;
end;

// --------------------------------------------------------------------------------
procedure TMemoryEditor.gridSystemMemoryDblClick(Sender: TObject);
var
    cellRect: TRect;
begin
    cellRect := gridSystemMemory.CellRect(gridSystemMemory.Col, gridSystemMemory.Row);
    cellRect.Left := cellRect.Left + gridSystemMemory.Left;
    cellRect.Right := cellRect.Right + gridSystemMemory.Left;
    cellRect.Top := cellRect.Top + gridSystemMemory.Top;
    cellRect.Bottom := cellRect.Bottom + gridSystemMemory.Top;
    if ((gridSystemMemory.Col > 0) and (gridSystemMemory.Row > 0)) then begin
        with editMemoryCell do begin
            Left := cellRect.Left + 1;
            Top := cellRect.Top + 1;
            Width := cellRect.Right - cellRect.Left - 1;
            Height := cellRect.Bottom - cellRect.Top - 1;
            Text := IntToHex(SystemMemory.Read(((gridSystemMemory.Row - 1) shl 4) + (gridSystemMemory.Col - 1)), 2);
            SelectAll;
            Visible := True;
            SetFocus;
        end;
    end
    else begin
        editMemoryCell.Visible := False;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMemoryEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    SystemSettings.saveFormState(TForm(self));
    CloseAction := caFree;
    MemoryEditor := nil;
end;

// --------------------------------------------------------------------------------
procedure TMemoryEditor.editMemoryCellKeyPress(Sender: TObject; var Key: char);
begin
    if not (Key in ['0'..'9', 'a'..'f', 'A'..'F', #8]) then begin
        Key := #0;
    end;
end;

// --------------------------------------------------------------------------------
procedure TMemoryEditor.editMemoryCellEditingDone(Sender: TObject);
begin
    SystemMemory.Write((((gridSystemMemory.Row - 1) shl 4) + (gridSystemMemory.Col - 1)),
        Hex2Dec(editMemoryCell.Text));
    editMemoryCell.Visible := False;
end;

// --------------------------------------------------------------------------------
procedure TMemoryEditor.gridSystemMemoryDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
    CellText: string;
    TextWidth, TextHeight: integer;
    TextTop, TextLeft: integer;
    memAddr: DWord;
begin
    if (aCol >= 1) and (aCol <= 16) and (aRow = 0) then begin
        CellText := IntToHex(aCol - 1, 2);
        gridSystemMemory.Canvas.Font.Style := [fsBold];
    end
    else if (aCol = 0) and (aRow >= 1) then begin
        CellText := IntToHex((aRow - 1) * 16, 4);
        gridSystemMemory.Canvas.Font.Style := [fsBold];
    end
    else if (aCol >= 1) and (aRow >= 1) then begin
        memAddr := ((aRow - 1) shl 4) + (aCol - 1);
        CellText := IntToHex(SystemMemory.Read(memAddr), 2);
        gridSystemMemory.Canvas.Font.Style := [];
        if (SystemMemory.IsRomEnabled = True) and (memAddr < SystemMemory.GetBootRomSize) then begin
            gridSystemMemory.Canvas.Brush.Color := $BABAFF;
        end
        else begin
            gridSystemMemory.Canvas.Brush.Color := clDefault;
        end;
        if (memAddr = CpuProgrammCounter) then begin
            gridSystemMemory.Canvas.Brush.Color := $A5FF9D;
        end;
        //if (gdFocused in aState) then begin
        //    gridSystemMemory.Canvas.Brush.Color := clDefault;
        //end;
    end;
    gridSystemMemory.Canvas.FillRect(aRect);
    gridSystemMemory.Canvas.GetTextSize(CellText, TextWidth, TextHeight);
    TextTop := aRect.Top + ((aRect.Height - TextHeight) div 2);
    TextLeft := aRect.Left + ((aRect.Width - TextWidth) div 2);
    gridSystemMemory.Canvas.TextOut(TextLeft, TextTop, CellText);
end;

// --------------------------------------------------------------------------------
procedure TMemoryEditor.gridSystemMemorySelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
begin
    if ((aCol > 0) and (aRow > 0)) then begin
        editMemoryCell.BoundsRect := gridSystemMemory.CellRect(aCol, aRow);
        editMemoryCell.Text := IntToHex(SystemMemory.Read(((aRow - 1) shl 4) + (aCol - 1)), 2);
        Editor := editMemoryCell;
    end;

end;

// --------------------------------------------------------------------------------
procedure TMemoryEditor.statusBarResize(Sender: TObject);
begin
    statusBar.Panels[0].Width := self.Width div 2;
    statusBar.Panels[1].Width := self.Width div 2;
end;

// --------------------------------------------------------------------------------
procedure TMemoryEditor.showMemoryData;
begin
    CpuProgrammCounter := Z180Cpu.getCoreData().PCmmu;
    gridSystemMemory.BeginUpdate;
    gridSystemMemory.Row := 0;
    gridSystemMemory.Row := (CpuProgrammCounter div 16) + 8;
    gridSystemMemory.EndUpdate();
end;

// --------------------------------------------------------------------------------
procedure TMemoryEditor.memoryChanged;
begin
    gridSystemMemory.RowCount := (SystemMemory.GetSystemMemorySize div 16) + 1;
    statusBar.Panels[0].Text := '  System-Memory Size : ' + IntToStr(SystemMemory.GetSystemMemorySize div 1024) + 'KB';
    statusBar.Panels[1].Text := '  Boot-ROM Size : ' + IntToStr(SystemMemory.GetBootRomSize div 1024) + 'KB';
end;

// --------------------------------------------------------------------------------
end.
