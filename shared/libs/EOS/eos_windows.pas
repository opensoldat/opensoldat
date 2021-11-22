const EOS_WINDOWS_RTCOPTIONS_API_LATEST = 1;

type EOS_Windows_RTCOptions = record
  ApiVersion: cint32;
  XAudio29DllPath: PChar;
end;