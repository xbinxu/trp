trp
===

Tencent Cloud reverse http tunnel


1. Description

   Nowadays I work for a social game project which target on Tencent QZone & Pengyou platform, the amazing thing of the
platform is our server can't visit the Internet inside Tencent cloud, therefore the maintanence cost become unacceptable.
Thus, here comes the solution.


2. What is a "reverse" tunnel?

   There exist some project named "http tunnel" provide something like "http proxy", but it won't work inside tencent cloud.
But, we can connect to our server inside tencent cloud (don't ask me why, :P) by tcp,  we can deliver http request to the
outside world and pass back http response to our server. Since the tcp server side works as http client, I named it as 
"reverse" tunnel.