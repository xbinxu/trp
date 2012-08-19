trp
===

Tencent Cloud reverse http tunnel


1. Why we need reverse tunnel?

   Nowadays I work for a social game project which target on Tencent QZone & Pengyou platform, the amazing thing of the
platform is our server can't visit the Internet inside Tencent cloud, therefore the maintanence cost become unacceptable.
Thus, here comes the solution.


2. What is a "reverse" tunnel?

   There exist some project named "http tunnel" provide something like "http proxy", but it won't work inside tencent cloud.
But, we can connect to our server inside tencent cloud (don't ask me why, :P) by tcp,  we can deliver http request to the
outside world and pass back http response to our server. Since the tcp server side works as http client, I named it as 
"reverse" tunnel.

3. Architect
    
                                    | <== tencent TGW 
                  +------------+    |    +------------+ 
    internet <==> | trp client | <--|--> | trp server | <==> http client (zypper, curl, wget, etc) 
                  +------------+    |    +------------+
                                    |  inside tencent cloud

4. Install Proxy Server

    a). download erlang R15B01 from http://www.erlang.org
    b). upload and install erlang to the destination server
    c). update dev config at rel/vars/client.config & rel/vars/server.config
        
        {mode, "server"}.                                %% keep it unchanged
        {https_port, "9001"}.                            %% this is http server port, as known as proxy listen port
        {web_server_addr, "127.0.0.1"}.                  %% payment server addr if you're setting up payment sandbox
        {web_server_port, "18081"}.                      %% payment server port for payment sandbox
        {domain_name, "domainname"}.      %% proxy server domain name, to setup tcp connection over TGW
        {tcps_port, "80"}.                               %% tcp server listen port (you should make it available outside tencent cloud)
        {node, "trps@127.0.0.1"}.                        %% just keep it unchanged

    d). build project

        make devrel

    e). start proxy server. 
        
        dev/server/bin/trp start

5. Install Proxy Client
   
   a~e). same as server side
   e). start proxy client:
        dev/client/bin/trp start

6. Take a try:

   update zypper:

   zypper service-delete 1
   http_proxy="http://proxy-addr:9001" zypper service-add http://ftp.hosteurope.de/mirror/ftp.opensuse.org/discontinued/SL-10.1/inst-source/

   now:

   http_proxy="http://proxy-addr:9001" zypper install git 

   Have fun with it!

    
