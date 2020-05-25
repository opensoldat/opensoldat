{*
SDL_net:  An example cross-platform network library for use with SDL
Copyright (C) 1997-2013 Sam Lantinga <slouken@libsdl.org>
Copyright (C) 2012 Simeon Maxein <smaxein@googlemail.com>

This software is provided 'as-is', without any express or implied
warranty.  In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
claim that you wrote the original software. If you use this software
in a product, an acknowledgment in the product documentation would be
appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be
misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.
*}
unit sdl2_net;

{$INCLUDE jedi.inc}

interface

uses SDL2;

const
   {$IFDEF WINDOWS}
   SDLNet_LibName = 'SDL2_net.dll';
   {$ENDIF}
   
{$IFDEF UNIX}
   {$IFDEF DARWIN}
   SDLNet_LibName = 'libSDL2_net.dylib';
   {$ELSE}
   {$IFDEF FPC}
   SDLNet_LibName = 'libSDL2_net.so';
   {$ELSE}
   SDLNet_LibName = 'libSDL2_net-2.0.so.0';
   {$ENDIF}
   {$ENDIF}
   {$ENDIF}
   
{$IFDEF MACOS}
   SDLNet_LibName = 'SDL2_net';
   {$IFDEF FPC}
   {$linklib libSDL2_net}
   {$ENDIF}
   {$ENDIF}
   

type
   TSDLNet_Version = TSDL_Version;
   
const
   {* Printable format: "%d.%d.%d", MAJOR, MINOR, PATCHLEVEL *}
   SDL_NET_MAJOR_VERSION = 2;
   SDL_NET_MINOR_VERSION = 0;
   SDL_NET_PATCHLEVEL    = 0;
   
{* This macro can be used to fill a version structure with the compile-time
* version of the SDL_net library.
*}
procedure SDL_NET_VERSION(Out X: TSDL_Version);

{* This function gets the version of the dynamically linked SDL_net library.
it should NOT be used to fill a version structure, instead you should
use the SDL_NET_VERSION() macro.
*}
   procedure SDLNet_Linked_Version() cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_Linked_Version' {$ENDIF} {$ENDIF};
   
   {* Initialize/Cleanup the network API
   SDL must be initialized before calls to functions in this library,
   because this library uses utility functions from the SDL library.
   *}
      function SDLNet_Init(): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_Init' {$ENDIF} {$ENDIF};
         procedure SDLNet_Quit() cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_Quit' {$ENDIF} {$ENDIF};
         
         type
            {***********************************************************************}
            {* IPv4 hostname resolution API                                        *}
            {***********************************************************************}
            TIPaddress = record
                            host: UInt32;            {* 32-bit IPv4 host address *}
                            port: UInt16;            {* 16-bit protocol port *}  
                         end;
            PIPaddress = ^TIPaddress;
            
         {* Resolve a host name and port to an IP address in network form.
         If the function succeeds, it will return 0.
         If the host couldn't be resolved, the host portion of the returned
         address will be INADDR_NONE, and the function will return -1.
         If 'host' is NULL, the resolved host will be set to INADDR_ANY.
         *}
const
   INADDR_ANY       = $00000000;
   INADDR_NONE      = $FFFFFFFF;
   INADDR_LOOPBACK  = $7f000001;
   INADDR_BROADCAST = $FFFFFFFF;
   
            function SDLNet_ResolveHost(address: PIPaddress; const host: PAnsiChar; port: UInt16): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_ResolveHost' {$ENDIF} {$ENDIF};
            
            {* Resolve an ip address to a host name in canonical form.
            If the ip couldn't be resolved, this function returns NULL,
            otherwise a pointer to a static buffer containing the hostname
            is returned.  Note that this function is not thread-safe.
            *}
               function SDLNet_ResolveIP(const ip: PIPaddress): PAnsiChar cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_ResolveIP' {$ENDIF} {$ENDIF};
               
               {* Get the addresses of network interfaces on this system.
               This returns the number of addresses saved in 'addresses'
               *}
                  function SDLNet_GetLocalAddresses(addresses: PIPaddress; maxcount: Integer): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_GetLocalAddresses' {$ENDIF} {$ENDIF};
                  
                  {***********************************************************************}
                  {* TCP network API                                                     *}
                  {***********************************************************************}
                  type
                     _TCPSocket = record
                                  end;
                     TTCPSocket = ^_TCPSocket;
                     
                  {* Open a TCP network socket
                  If ip.host is INADDR_NONE or INADDR_ANY, this creates a local server
                  socket on the given port, otherwise a TCP connection to the remote
                  host and port is attempted. The address passed in should already be
                  swapped to network byte order (addresses returned from
                  SDLNet_ResolveHost() are already in the correct form).
                  The newly created socket is returned, or NULL if there was an error.
                  *}
                     function SDLNet_TCP_Open(ip: PIPaddress): TTCPSocket cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_TCP_Open' {$ENDIF} {$ENDIF};
                     
                     {* Accept an incoming connection on the given server socket.
                     The newly created socket is returned, or NULL if there was an error.
                     *}
                        function SDLNet_TCP_Accept(server: TTCPSocket): TTCPSocket cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_TCP_Accept' {$ENDIF} {$ENDIF};
                        
                        {* Get the IP address of the remote system associated with the socket.
                        If the socket is a server socket, this function returns NULL.
                        *}
                           function SDLNet_TCP_GetPeerAddress(sock: TTCPSocket): PIPaddress cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_TCP_GetPeerAddress' {$ENDIF} {$ENDIF};
                           
                           {* Send 'len' bytes of 'data' over the non-server socket 'sock'
                           This function returns the actual amount of data sent.  If the return value
                           is less than the amount of data sent, then either the remote connection was
                           closed, or an unknown socket error occurred.
                           *}
                              function SDLNet_TCP_Send(sock: TTCPSocket; const data: Pointer; len: Integer): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_TCP_Send' {$ENDIF} {$ENDIF};
                              
                              {* Receive up to 'maxlen' bytes of data over the non-server socket 'sock',
                              and store them in the buffer pointed to by 'data'.
                              This function returns the actual amount of data received.  If the return
                              value is less than or equal to zero, then either the remote connection was
                              closed, or an unknown socket error occurred.
                              *}
                                 function SDLNet_TCP_Recv(sock: TTCPSocket; data: Pointer; maxlen: Integer): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_TCP_Recv' {$ENDIF} {$ENDIF};
                                 
                                 {* Close a TCP network socket *}
                                    procedure SDLNet_TCP_Close(sock: TTCPSocket) cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_TCP_Close' {$ENDIF} {$ENDIF};
                                    
                                    {***********************************************************************}
                                    {* UDP network API                                                     *}
                                    {***********************************************************************}
                                    
                                    const
                                       {* The maximum channels on a a UDP socket *}
                                       SDLNET_MAX_UDPCHANNELS = 32;
                                       {* The maximum addresses bound to a single UDP socket channel *}
                                       SDLNET_MAX_UDPADDRESSES = 4;
                                       
                                    type
                                       TUDPSocket = record
                                                    end;
                                       PUDPSocket = ^TUDPSocket;
                                       
                                    TUDPPacket = record
                                                    channel: Integer;     {* The src/dst channel of the packet *}
                                                    data: PUInt8;         {* The packet data *}
                                                    len: Integer;         {* The length of the packet data *}
                                                    maxlen: Integer;      {* The size of the data buffer *}
                                                    status: Integer;      {* packet status after sending *}
                                                    address: TIPaddress;  {* The source/dest address of an incoming/outgoing packet *}
                                                 end;
                                       PUDPPacket = ^TUDPPacket;
                                       PPUDPPacket = ^PUDPPacket;
                                       
                                    {* Allocate/resize/free a single UDP packet 'size' bytes long.
                                    The new packet is returned, or NULL if the function ran out of memory.
                                    *}
                                       function SDLNet_AllocPacket(size: Integer): PUDPPacket cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_AllocPacket' {$ENDIF} {$ENDIF};
                                          function SDLNet_ResizePacket(packet: PUDPPacket; newsize: Integer): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_ResizePacket' {$ENDIF} {$ENDIF};
                                             procedure SDLNet_FreePacket(packet: PUDPPacket) cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_FreePacket' {$ENDIF} {$ENDIF};
                                             
                                             {* Allocate/Free a UDP packet vector (array of packets) of 'howmany' packets,
                                             each 'size' bytes long.
                                             A pointer to the first packet in the array is returned, or NULL if the
         function ran out of memory.
                                             *}
                                                function SDLNet_AllocPacketV(howmany: Integer; size: Integer): PPUDPPacket cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_AllocPacketV' {$ENDIF} {$ENDIF};
                                                   procedure SDLNet_FreePacketV(packetV: PPUDPPacket) cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_FreePacketV' {$ENDIF} {$ENDIF};
                                                   
                                                   {* Open a UDP network socket
                                                   If 'port' is non-zero, the UDP socket is bound to a local port.
                                                   The 'port' should be given in native byte order, but is used
                                                   internally in network (big endian) byte order, in addresses, etc.
                                                   This allows other systems to send to this socket via a known port.
                                                   *}
                                                      function SDLNet_UDP_Open(port: UInt16): TUDPSocket cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_UDP_Open' {$ENDIF} {$ENDIF};
                                                      
                                                      {* Set the percentage of simulated packet loss for packets sent on the socket. *}
                                                         procedure SDLNet_UDP_SetPacketLoss(sock: TUDPSocket; percent: Integer) cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_UDP_SetPacketLoss' {$ENDIF} {$ENDIF};
                                                         
                                                         {* Bind the address 'address' to the requested channel on the UDP socket.
                                                         If the channel is -1, then the first unbound channel that has not yet
                                                         been bound to the maximum number of addresses will be bound with
                                                         the given address as it's primary address.
                                                         If the channel is already bound, this new address will be added to the
                                                         list of valid source addresses for packets arriving on the channel.
                                                         If the channel is not already bound, then the address becomes the primary
                                                         address, to which all outbound packets on the channel are sent.
                                                         This function returns the channel which was bound, or -1 on error.
                                                         *}
                                                            function SDLNet_UDP_Bind(sock: TUDPSocket; channel: Integer; const address: PIPaddress): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_UDP_Bind' {$ENDIF} {$ENDIF};
                                                            
                                                            {* Unbind all addresses from the given channel *}
                                                               procedure SDLNet_UDP_Unbind(sock: TUDPSocket; channel: Integer) cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_UDP_Unbind' {$ENDIF} {$ENDIF};
                                                               
                                                               {* Get the primary IP address of the remote system associated with the
                                                               socket and channel.  If the channel is -1, then the primary IP port
                                                               of the UDP socket is returned -- this is only meaningful for sockets
                                                               opened with a specific port.
                                                               If the channel is not bound and not -1, this function returns NULL.
                                                               *}
                                                                  function SDLNet_UDP_GetPeerAddress(sock: TUDPSocket; channel: Integer): PIPaddress cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_UDP_GetPeerAddress' {$ENDIF} {$ENDIF};
                                                                  
                                                                  {* Send a vector of packets to the the channels specified within the packet.
                                                                  If the channel specified in the packet is -1, the packet will be sent to
                                                                  the address in the 'src' member of the packet.
                                                                  Each packet will be updated with the status of the packet after it has
                                                                  been sent, -1 if the packet send failed.
                                                                  This function returns the number of packets sent.
                                                                  *}
                                                                     function SDLNet_UDP_SendV(sock: TUDPSocket; packets: PPUDPPacket; npackets: Integer): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_UDP_SendV' {$ENDIF} {$ENDIF};
                                                                     
                                                                     {* Send a single packet to the specified channel.
                                                                     If the channel specified in the packet is -1, the packet will be sent to
                                                                     the address in the 'src' member of the packet.
                                                                     The packet will be updated with the status of the packet after it has
                                                                     been sent.
                                                                     This function returns 1 if the packet was sent, or 0 on error.
                                                                     
                                                                     NOTE:
                                                                     The maximum size of the packet is limited by the MTU (Maximum Transfer Unit)
                                                                     of the transport medium.  It can be as low as 250 bytes for some PPP links,
                                                                     and as high as 1500 bytes for ethernet.
                                                                     *}
                                                                        function SDLNet_UDP_Send(sock: TUDPSocket; channel: Integer; packet: PUDPPacket): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_UDP_Send' {$ENDIF} {$ENDIF};
                                                                        
                                                                        {* Receive a vector of pending packets from the UDP socket.
                                                                        The returned packets contain the source address and the channel they arrived
                                                                        on.  If they did not arrive on a bound channel, the the channel will be set
                                                                        to -1.
                                                                        The channels are checked in highest to lowest order, so if an address is
                                                                        bound to multiple channels, the highest channel with the source address
                                                                        bound will be returned.
                                                                        This function returns the number of packets read from the network, or -1
                                                                        on error.  This function does not block, so can return 0 packets pending.
                                                                        *}
                                                                           function SDLNet_UDP_RecvV(sock: TUDPSocket; packets: PPUDPPacket): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_UDP_RecvV' {$ENDIF} {$ENDIF};
                                                                           
                                                                           {* Receive a single packet from the UDP socket.
                                                                           The returned packet contains the source address and the channel it arrived
                                                                           on.  If it did not arrive on a bound channel, the the channel will be set
                                                                           to -1.
                                                                           The channels are checked in highest to lowest order, so if an address is
                                                                           bound to multiple channels, the highest channel with the source address
                                                                           bound will be returned.
                                                                           This function returns the number of packets read from the network, or -1
                                                                           on error.  This function does not block, so can return 0 packets pending.
                                                                           *}
                                                                              function SDLNet_UDP_Recv(sock: TUDPSocket; packet: PUDPPacket): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_UDP_Recv' {$ENDIF} {$ENDIF};
                                                                              
                                                                              {* Close a UDP network socket *}
                                                                                 procedure SDLNet_UDP_Close(sock: TUDPSocket) cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_UDP_Close' {$ENDIF} {$ENDIF};
                                                                                 
                                                                                 {***********************************************************************}
                                                                                 {* Hooks for checking sockets for available data                       *}
                                                                                 {***********************************************************************}
                                                                                 
                                                                                 type
                                                                                    TSDLNet_SocketSet = record
                                                                                                        end;
                                                                                    PSDLNet_SocketSet = ^TSDLNet_SocketSet;
                                                                                    
                                                                                 {* Any network socket can be safely cast to this socket type *}
                                                                                    TSDLNet_GenericSocket = record
                                                                                                               ready: Integer;
                                                                                                            end;
                                                                                    PSDLNet_GenericSocket = ^TSDLNet_GenericSocket;
                                                                                    
                                                                                 {* Allocate a socket set for use with SDLNet_CheckSockets()
                                                                                 This returns a socket set for up to 'maxsockets' sockets, or NULL if
                                                                                 the function ran out of memory.
                                                                                 *}
                                                                                    function SDLNet_AllocSocketSet(maxsockets: Integer): TSDLNet_GenericSocket cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_AllocSocketSet' {$ENDIF} {$ENDIF};
                                                                                    
                                                                                    {* Add a socket to a set of sockets to be checked for available data *}
                                                                                       function SDLNet_AddSocket(set_: TSDLNet_SocketSet; sock: TSDLNet_GenericSocket): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_AddSocket' {$ENDIF} {$ENDIF};
                                                                                       //function SDLNet_TCP_AddSocket(set_: TSDLNet_SocketSet; sock: TTCPSocket): Integer; inline;
                                                                                       //function SDLNet_UDP_AddSocket(set_: TSDLNet_SocketSet; sock: TUDPSocket): Integer; inline;
                                                                                       
                                                                                       {* Remove a socket from a set of sockets to be checked for available data *}
                                                                                          function SDLNet_DelSocket(set_: TSDLNet_SocketSet; sock: TSDLNet_GenericSocket): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_DelSocket' {$ENDIF} {$ENDIF};
                                                                                          //function SDLNet_TCP_DelSocket(set_: TSDLNet_SocketSet; sock: TTCPSocket): Integer; inline;
                                                                                          //function SDLNet_UDP_DelSocket(set_: TSDLNet_SocketSet; sock: TUDPSocket): Integer; inline;
                                                                                          
                                                                                          {* This function checks to see if data is available for reading on the
                                                                                          given set of sockets.  If 'timeout' is 0, it performs a quick poll,
                                                                                          otherwise the function returns when either data is available for
                                                                                          reading, or the timeout in milliseconds has elapsed, which ever occurs
                                                                                          first.  This function returns the number of sockets ready for reading,
                                                                                          or -1 if there was an error with the select() system call.
                                                                                          *}
                                                                                             function SDLNet_CheckSockets(set_: TSDLNet_SocketSet; timeout: UInt32): Integer cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_CheckSockets' {$ENDIF} {$ENDIF};
                                                                                             
                                                                                             {* After calling SDLNet_CheckSockets(), you can use this function on a
                                                                                             socket that was in the socket set, to find out if data is available
                                                                                             for reading.
                                                                                             *}
                                                                                                function SDLNet_SocketReady(sock: TSDLNet_GenericSocket): Integer; {$IFNDEF DELPHI} inline; {$ELSE} {$IFDEF DELPHI10UP} inline; {$ENDIF} {$ENDIF}
                                                                                                
                                                                                                {* Free a set of sockets allocated by SDL_NetAllocSocketSet() *}
                                                                                                   procedure SDLNet_FreeSocketSet(set_: TSDLNet_SocketSet) cdecl; external SDLNet_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDLNet_FreeSocketSet' {$ENDIF} {$ENDIF};
                                                                                                   
                                                                                                   {***********************************************************************}
                                                                                                   {* Error reporting functions                                           *}
                                                                                                   {***********************************************************************}
                                                                                                   
                                                                                                      procedure SDLNet_SetError(const fmt: PAnsiChar); cdecl;
                                                                                                         function SDLNet_GetError(): PAnsiChar; cdecl;
                                                                                                         
                                                                                                         {***********************************************************************}
                                                                                                         {* Inline functions to read/write network data                         *}
                                                                                                         {***********************************************************************}
                                                                                                         
                                                                                                         {* Write a 16/32-bit value to network packet buffer *}
                                                                                                         
                                                                                                         //procedure SDLNet_Write16(value: UInt16; areap: Pointer); inline;
                                                                                                         //procedure SDLNet_Write32(value: UInt32; areap: Pointer); inline;
                                                                                                         
                                                                                                         {* Read a 16/32-bit value from network packet buffer *}
                                                                                                         //function SDLNet_Read16(const areap: Pointer): UInt16; inline;
                                                                                                         //function SDLNet_Read32(const areap: Pointer): UInt32; inline;
                                                                                                         
                                                                                                         implementation
                                                                                                         
                                                                                                            procedure SDL_NET_VERSION(Out X: TSDL_Version);
                                                                                                            begin
                                                                                                               X.major := SDL_NET_MAJOR_VERSION;
                                                                                                               X.minor := SDL_NET_MINOR_VERSION;
                                                                                                               X.patch := SDL_NET_PATCHLEVEL;
                                                                                                            end;
                                                                                                         
                                                                                                         (*
    function SDLNet_TCP_AddSocket(set_: TSDLNet_SocketSet; sock: TTCPSocket): Integer;
    begin
                                                                                                          Result := SDLNet_AddSocket(set_, TSDLNet_GenericSocket(sock));
    end;
                                                                                                          
    function SDLNet_UDP_AddSocket(set_: TSDLNet_SocketSet; sock: TUDPSocket): Integer;
    begin
                                                                                                          Result := SDLNet_AddSocket(set_, TSDLNet_GenericSocket(sock));
    end;
                                                                                                          
    function SDLNet_TCP_DelSocket(set_: TSDLNet_SocketSet; sock: TTCPSocket): Integer;
    begin
                                                                                                          Result := SDLNet_DelSocket(set_, TSDLNet_GenericSocket(sock));
    end;
                                                                                                          
    function SDLNet_UDP_DelSocket(set_: TSDLNet_SocketSet; sock: TUDPSocket): Integer;
    begin
                                                                                                          Result := SDLNet_DelSocket(set_, TSDLNet_GenericSocket(sock));
    end;
                                                                                                          *)
                                                                                                         
                                                                                                            function SDLNet_SocketReady(sock: TSDLNet_GenericSocket): Integer;
                                                                                                            begin
                                                                                                               Result := sock.ready;
                                                                                                            end;
                                                                                                         
                                                                                                            procedure SDLNet_SetError(const fmt: PAnsiChar); cdecl;
                                                                                                            begin
                                                                                                               SDL_SetError(fmt);
                                                                                                            end;
                                                                                                         
                                                                                                            function SDLNet_GetError(): PAnsiChar; cdecl;
                                                                                                            begin
                                                                                                               Result := SDL_GetError();
                                                                                                            end;
                                                                                                         
                                                                                                         (*
    procedure SDLNet_Write16(value: UInt16; areap: Pointer);
    begin
                                                                                                          PUInt16(areap) := SDL_SwapBE16(value);
    end;
                                                                                                          
    procedure SDLNet_Write32(value: UInt32; areap: Pointer);
    begin
                                                                                                          PUInt32(areap) := SDL_SwapBE32(value);
    end;
                                                                                                          
                                                                                                          {* Read a 16/32-bit value from network packet buffer *}
                                                                                                            function SDLNet_Read16(const areap: Pointer): UInt16;
                                                                                                            begin
                                                                                                               Result := SDL_SwapBE16(PUInt16(areap));
                                                                                                            end;
                                                                                                         
                                                                                                            function SDLNet_Read32(const areap: Pointer): UInt32;
                                                                                                            begin
                                                                                                               Result := SDL_SwapBE32(PUInt32(areap));
                                                                                                            end;
                                                                                                         *)
                                                                                                         end.

