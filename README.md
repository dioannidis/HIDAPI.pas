# HIDAPI.pas
Free Pascal bindings for libhidapi on Linux

## only Linux (or other *ixes)

This does not (yet) work on Windows, it assumes sizeof(wchar_t) == 4 and this is not true on Windows. Also the library name will be different and I did not have a Windows PC with hidapi.dll at hand when writing this, on Windows I usually just use CreateFile(), ReadFile(), WriteFile() functions and the SetupAPI to enumerate the devices.

## Usage

I have chosen the more Pascal-like SomeObject.FooBar(baz) style of porting this API instead of the naked and rather ugly C-ish some_object_foo_bar(some_object, baz) notation for this API. This results in much nicer code when using it. It also allows to convert some unwieldy things like wchar_t* to more convenient UnicodeString on the fly.

The HIDAPI (like most object oriented C-APIs) will use pointers to structs as handles for objects and all API calls (methods of the struct) will expect this pointer as first argument. This is the typical C-style of object oriented programming. Free Pascal allows us to define these structs as Pascal objects and these can have our own methods attached to them (with more Pascalish naming conventions and more Pascalish parameter types) which will then in turn be implemented as wrappers to the original C api functions.

For best experience you should set the modeswitch

    {$modeswitch autoderef}
    

Use the API like this:

    var
      Paths: TSringList;
      LinkedList, Item: PHidDeviceInfo;
      
    begin
      Paths := TStringList.Create();
      LinkedList := THidDeviceInfo.Enumerate($DEAD, $BEEF);
      Item := LinkedList;
      while Assigned(Item) do begin
        Paths.Append(Item.Path);
        Item := Item.Next;
      end;
      LinkedList.Free;
      
      
      // now you have a list of paths
      
    end;
    
Or when opening a device from a Path you could do it like this:

    
    var
      Device: PHidDevice;
      Buffer: array[0..63] of Byte;
      
    begin
      Device := THidDevice.OpenPath(SomePath);
      if Assigned(Device) then begin
        if Device.Read(Buffer, SizeOf(Buffer)) > 0 then begin
          WriteLn('Success, got some Data');
        end;
        
        // do more stuff with the device
        
        Device.Close;
      end;
    end;
    
Without the autoderef modeswitch you would have to write it like  Device^.Read or LinkedList^.Free, this looks ugly, the autoderef will dereference the pointer to the object automatically.

