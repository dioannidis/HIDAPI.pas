program demo;

// this modeswitch makes the pointers to
// objects more comfortable to use
{$modeswitch autoderef}

uses
  sysutils,
  hidapi; // Dynamic loading of hidapi library. For static loading use hidapi_static.pas .

procedure EnumerationDemo;
var
  EnumList, EnumItem: PHidDeviceInfo;
  Path: String;
  Vid, Pid: Word;
  BusType: String;
  ProductName: UnicodeString;
begin
  WriteLn('will now enumerate all USB HID devices.');
{$ifndef MSWINDOWS}
  WriteLn();
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
    if HidApiVersion.Minor > 12 then
    begin
      WriteStr(BusType, EnumItem.BusType);
      WriteLn(Format('Found: %s in [%s] (%0.4x:%0.4x) at: %s', [ProductName, BusType, Vid, Pid, Path]))
    end
    else
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
  //DEMO_VID = $16C0;
  //DEMO_PID = $05DC;

  // MPlab Snap
  //DEMO_VID = $03EB;
  //DEMO_PID = $2180;
var
  Device: PHidDevice;
  I, J, Num: Integer;
  Buffer: array[0..63] of Byte;
  ReportDescriptor: array[0..HID_API_MAX_REPORT_DESCRIPTOR_SIZE - 1] of Byte;
begin
  WriteLn(Format('Will now try to open device %0.4x:%0.4x', [DEMO_VID, DEMO_PID]));
  WriteLn();
  Device := THidDevice.Open(DEMO_VID, DEMO_PID, '');
  if not Assigned(Device) then begin
    WriteLn('could not open device');
  end
  else begin
    WriteLn('device is open ...');
    WriteLn();

    if HidApiVersion.Minor > 12 then
      WriteLn('Bus Type     : ', Device.GetDeviceInfo.BusType)
    else
      WriteLn('Bus Type     : Not supported ( needs HidApi => 0.13.0 )');

    if HidApiVersion.Minor > 13 then
    begin
      I := Device.GetReportDescriptor(ReportDescriptor, HID_API_MAX_REPORT_DESCRIPTOR_SIZE);
      Write('Report Descr : ');
      for J := 0  to I do
          Write(Format('%0.2x ', [ReportDescriptor[J]]));
      WriteLn();
    end
    else
      WriteLn('Report Descr : Not supported ( needs HidApi => 0.14.0 )');

{$ifdef MSWINDOWS}
    if (HidApiVersion.Minor > 11) or
       ((HidApiVersion.Minor = 11) and (HidApiVersion.Patch > 2)) then
      WriteLn('Container ID : ', Device.GetContainerID.ToString())
    else
      WriteLn('Container ID : Not supported ( needs HidApi => 0.12.0 )');
{$endif}

    WriteLn('Manufacturer : ', Device.GetManufacturerString);
    WriteLn('Product      : ', Device.GetProductString);
    WriteLn();
    WriteLn('... now going to read data from it');
    WriteLn();
    for I := 1 to 1000 do begin
      Num := Device.ReadTimeout(Buffer, SizeOf(Buffer), 250);
      for J := 0 to Num - 1 do begin
          Write(Format('%0.2x ', [Buffer[J]]));
      end;
      Write(#13);
    end;
    WriteLn();
    WriteLn();
    WriteLn('closing device');
    Device.Close;
  end;
end;

begin
  HidInit(''); //HidInit('hidapi-0.10.0.dll');
  WriteLn('HIDAPI.pas demo ( using HidApi ', HidApiVersionStr, ' library ).');
  WriteLn();
  EnumerationDemo;
  OpenAndReadDemo;
  HidExit();
end.
