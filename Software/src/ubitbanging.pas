unit uBitbanging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons
  ;

type

  { TfBitbanging }

  TfBitbanging = class(TForm)
    cbBit0: TCheckBox;
    cbBit1: TCheckBox;
    cbBit2: TCheckBox;
    cbBit3: TCheckBox;
    cbBit4: TCheckBox;
    cbBit5: TCheckBox;
    cbBit6: TCheckBox;
    sbRefresh: TSpeedButton;
    procedure cbBit0Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbRefreshClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure BitbangingGetStatus(Sender: TObject);
  end; 

var
  fBitbanging: TfBitbanging;

implementation

uses uMain,uLibUSBDevice;

{ TfBitbanging }

procedure TfBitbanging.BitbangingGetStatus(Sender: TObject);
var
  data : byte;
  res: LongInt;
  Device: TLibUSBDevice;
begin
  Device := TLibUSBDevice(fMain.tvProgrammer.Selected.Data);
  if Device.OpenDevice then
    begin
      data := 0;
      res := Device.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,2 {Get Bitbanging},0,0,@Data,1, 5000);
      cbBit0.Checked := data and 1 > 0;
      cbBit1.Checked := data and 2 > 0;
      cbBit2.Checked := data and 4 > 0;
      cbBit3.Checked := data and 8 > 0;
      cbBit4.Checked := data and 16 > 0;
      cbBit5.Checked := data and 32 > 0;
      cbBit6.Checked := data and 64 > 0;
      Device.CloseDevice;
    end;
end;

procedure TfBitbanging.cbBit0Change(Sender: TObject);
var
  data : byte;
  res: LongInt;
  Device: TLibUSBDevice;
begin
  Device := TLibUSBDevice(fMain.tvProgrammer.Selected.Data);
  if Device.OpenDevice then
    begin
      data := 0;
      if cbBit0.Checked then data := data or 1;
      if cbBit1.Checked then data := data or 2;
      if cbBit2.Checked then data := data or 4;
      if cbBit3.Checked then data := data or 8;
      if cbBit4.Checked then data := data or 16;
      if cbBit5.Checked then data := data or 32;
      if cbBit6.Checked then data := data or 64;
      res := Device.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,1 {Set Bitbanging},Data,0,nil,0, 5000);
      Device.CloseDevice;
    end;
end;

procedure TfBitbanging.FormShow(Sender: TObject);
begin
  BitbangingGetStatus(Sender);
end;

procedure TfBitbanging.sbRefreshClick(Sender: TObject);
begin
  BitbangingGetStatus(Sender);
end;

initialization
  {$I ubitbanging.lrs}

end.

