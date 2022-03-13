program demo;

// this modeswitch makes the pointers to
// objects more comfortable to use
{$modeswitch autoderef}

uses
  sysutils, hidapi;

procedure EnumerationDemo;
var
  EnumList, EnumItem: PHidDeviceInfo;
  Path: String;
  Vid, Pid: Word;
  ProductName: UnicodeString;

begin
  WriteLn('will now enumerate all USB HID devices.');
{$ifndef MSWINDOWS}
  WriteLn('Note that it will list more info if you run with sudo,');
  WriteLn('you might want to add an udev rule for your device.');
{$endif}
  WriteLn();

  // request the hidapi to build a linked list of all HID devices
  // it will match all of them because VID and PID are zero
  EnumList := THidDeviceInfo.Enumerate(0, 0);

  // iterate through linked list
  EnumItem := EnumList;
  while Assigned(EnumItem) do begin
    Path := EnumItem.Path;
    Vid := EnumItem.VendorID;
    Pid := EnumItem.ProductID;
    ProductName := PCWCharToUnicodeString(EnumItem.ProductString);
    WriteLn(Format('Found: %s (%0.4x:%0.4x) at: %s', [ProductName, Vid, Pid, Path]));
    EnumItem := EnumItem.Next;
  end;
  WriteLn();

  // dont forget to free the list again, this s done
  // by calling free on the first item of the linked list
  EnumList.Free;
end;

procedure OpenAndReadDemo;
const
  // This is a Logitech Extreme 3D Joystick, a
  // cheap and popular model, probably you own one,
  // if not then try some other VID/PID, try to run
  // as root or make an udev rule for the device.
  DEMO_VID = $046D;
  DEMO_PID = $C215;

  // USBasp
  //DEMO_VID = $16c0;
  //DEMO_PID = $05dc;

  // MPlab Snap
  //DEMO_VID = $03eb;
  //DEMO_PID = $2180;
var
  Device: PHidDevice;
  I, J, Num: Integer;
  Buffer: array[0..63] of Byte;

begin
  WriteLn(Format('Will now try to open device %0.4x:%0.4x', [DEMO_VID, DEMO_PID]));
  WriteLn();
  Device := THidDevice.Open(DEMO_VID, DEMO_PID, '');
  if not Assigned(Device) then begin
    WriteLn('could not open device');
  end
  else begin
    WriteLn('device is open, now going to read data from it');
    WriteLn('Manufacturer: ', Device.GetManufacturerString);
    WriteLn('Product: ', Device.GetProductString);
    for I := 1 to 1000 do begin
      Num := Device.Read(Buffer, SizeOf(Buffer));
      for J := 0 to Num - 1 do begin
          Write(Format('%0.2x ', [Buffer[J]]));
      end;
      Write(#13);
    end;
    WriteLn();
    WriteLn('closing device');
    Device.Close;
  end;
end;

begin
  HidInit('');
  //HidInit('hidapi-0.10.0.dll');
  EnumerationDemo;
  OpenAndReadDemo;
  HidExit();
end.
