/// icq2tcp.cc   -*-C++-*-

/// Copyright (C) 2001 Steve Youngs


/// This program is free software; you can redistribute it and/or modify
/// it under the terms of the GNU General Public License as published by
/// the Free Software Foundation; either version 2 of the License, or
/// (at your option) any later version.

/// This program is distributed in the hope that it will be useful,
/// but WITHOUT ANY WARRANTY; without even the implied warranty of
/// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/// GNU General Public License for more details.

/// You should have received a copy of the GNU General Public License
/// along with this program; if not, write to the Free Software
/// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA


// Based on udp2tcp.cc by Stephen Tse <stephent@sfu.ca>

//     Author: Steve Youngs <youngs@xemacs.org>
// Maintainer: Steve Youngs <youngs@xemacs.org>
//    License: GPL
//    Created: Jul 16, 2001
//    Version: 0.1.0
//      Where: http://eicq.sourceforge.net/
//   Keywords: udp, tcp, network, bridge, icq, emacs
 
 
// Description:
//    A network bridge program that receives TCP packets from one host
//    and send them to second host as UDP packets and vice versa.  It
//    doesn't have much use outside of ICQ without modification.
 
//    Initially developed to solve the deficiency of no UDP support in
//    Emacs, in order to connect ICQ server with UDP packets. However,
//    only the function send_v5() is Eicq (an XEmacs ICQ client) dependent,
//    you can easily modify it for other general use.
 
//    thost: TCP-speaking host
//    uhost: UDP-speaking host



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>

#define dword unsigned long
#define byte unsigned char

// increasing this causes troubles (Bad Address) sometimes
#define MAXDATASIZE 2000
#define BACKLOG 10 // no of socket waiting to be listened


// SOCK_ERROR makes use of these global socket variables
int udp_sock, tcp_sock, tcp_new_sock;

void debug_socket(char* header, char *s, size_t len)
{
    dword i;
    char *p = s;

    printf("%s%d\n", header, len);

    for (i = 0; i < len; i++) {
	if ((i % 2) == 0) printf(" ");
	printf("%02x", (byte)*p);
	p++;
    }
  
    printf("\n\n");
}

void sock_close()
{
    close(udp_sock);
    close(tcp_sock);
    close(tcp_new_sock);
}

void sock_error_exit(bool sock_error, char *msg)
{
    if (sock_error) {
	perror(msg);
	sock_close();
	exit(1);
    }
}

static const char v5_table[] = {
    0x59, 0x60, 0x37, 0x6B, 0x65, 0x62, 0x46, 0x48, 
	0x53, 0x61, 0x4C, 0x59, 0x60, 0x57, 0x5B, 0x3D,
	0x5E, 0x34, 0x6D, 0x36, 0x50, 0x3F, 0x6F, 0x67,
	0x53, 0x61, 0x4C, 0x59, 0x40, 0x47, 0x63, 0x39,
	0x50, 0x5F, 0x5F, 0x3F, 0x6F, 0x47, 0x43, 0x69,
	0x48, 0x33, 0x31, 0x64, 0x35, 0x5A, 0x4A, 0x42,
	0x56, 0x40, 0x67, 0x53, 0x41, 0x07, 0x6C, 0x49,
	0x58, 0x3B, 0x4D, 0x46, 0x68, 0x43, 0x69, 0x48,
	0x33, 0x31, 0x44, 0x65, 0x62, 0x46, 0x48, 0x53,
	0x41, 0x07, 0x6C, 0x69, 0x48, 0x33, 0x51, 0x54,
	0x5D, 0x4E, 0x6C, 0x49, 0x38, 0x4B, 0x55, 0x4A,
	0x62, 0x46, 0x48, 0x33, 0x51, 0x34, 0x6D, 0x36,
	0x50, 0x5F, 0x5F, 0x5F, 0x3F, 0x6F, 0x47, 0x63,
	0x59, 0x40, 0x67, 0x33, 0x31, 0x64, 0x35, 0x5A,
	0x6A, 0x52, 0x6E, 0x3C, 0x51, 0x34, 0x6D, 0x36,
	0x50, 0x5F, 0x5F, 0x3F, 0x4F, 0x37, 0x4B, 0x35,
	0x5A, 0x4A, 0x62, 0x66, 0x58, 0x3B, 0x4D, 0x66,
	0x58, 0x5B, 0x5D, 0x4E, 0x6C, 0x49, 0x58, 0x3B,
	0x4D, 0x66, 0x58, 0x3B, 0x4D, 0x46, 0x48, 0x53,
	0x61, 0x4C, 0x59, 0x40, 0x67, 0x33, 0x31, 0x64,
	0x55, 0x6A, 0x32, 0x3E, 0x44, 0x45, 0x52, 0x6E,
	0x3C, 0x31, 0x64, 0x55, 0x6A, 0x52, 0x4E, 0x6C,
	0x69, 0x48, 0x53, 0x61, 0x4C, 0x39, 0x30, 0x6F,
	0x47, 0x63, 0x59, 0x60, 0x57, 0x5B, 0x3D, 0x3E,
	0x64, 0x35, 0x3A, 0x3A, 0x5A, 0x6A, 0x52, 0x4E,
	0x6C, 0x69, 0x48, 0x53, 0x61, 0x6C, 0x49, 0x58,
	0x3B, 0x4D, 0x46, 0x68, 0x63, 0x39, 0x50, 0x5F,
	0x5F, 0x3F, 0x6F, 0x67, 0x53, 0x41, 0x25, 0x41,
	0x3C, 0x51, 0x54, 0x3D, 0x5E, 0x54, 0x5D, 0x4E,
	0x4C, 0x39, 0x50, 0x5F, 0x5F, 0x5F, 0x3F, 0x6F,
	0x47, 0x43, 0x69, 0x48, 0x33, 0x51, 0x54, 0x5D,
	0x6E, 0x3C, 0x31, 0x64, 0x35, 0x5A, 0x00, 0x00
	};

void dw_2_chars(byte *buf, dword num)
{
    buf[3] = (byte) ((num)>>24) & 0x000000ff;
    buf[2] = (byte) ((num)>>16) & 0x000000ff;
    buf[1] = (byte) ((num)>>8) & 0x000000ff;
    buf[0] = (byte) (num) & 0x000000ff;
}

dword chars_2_dw(byte *buf)
{
    dword i;
   
    i= buf[3];
    i <<= 8;
    i+= buf[2];
    i <<= 8;
    i+= buf[1];
    i <<= 8;
    i+= buf[0];
   
    return i;
}

// stole from micq
void v5_encrypt(byte *buf, size_t len)
{    
    dword checkcode;

    // calculate checkcode
    dword cc, cc2;
    dword r1, r2;
   
    cc = buf[8];
    cc <<= 8;
    cc += buf[4];
    cc <<= 8;
    cc += buf[2];
    cc <<= 8;
    cc += buf[6];

    r1 = rand() % (len - 0x18);
    r1 += 0x18;
    r2 = rand() & 0xff;
   
    cc2 = r1;
    cc2 <<= 8;
    cc2 += buf[r1];
    cc2 <<= 8;
    cc2 += r2;   
    cc2 <<= 8;
    cc2 += v5_table[r2];
    cc2 ^= 0xff00ff;

    checkcode = cc^cc2;

    // insert checkcode
    dw_2_chars(buf+20, checkcode);
    
    // scamble packets
    dword code1 = len * 0x68656c6cL + checkcode;
    
    /*
      Caution! dw_2_chars() at the end of packet can corrupt data (at
      most 3 bytes) after packet. We can save it and restore it.
    */
    char safe[3];
    strncpy(safe, (char *)(buf+len), 3);
    
    for (dword pos = 0xa; pos < (len+3); pos+=4) {
        dword code2 = code1 + v5_table[pos & 0xff];
        dword data = chars_2_dw(buf+pos) ^ code2;
        dw_2_chars(buf+pos, data);
    }

    strncpy((char *)(buf+len), safe, 3);
    
    // scamble checkcode
    dword a[6];
    
    a[1] = checkcode & 0x0000001f;
    a[2] = checkcode & 0x03e003e0;
    a[3] = checkcode & 0xf8000400;
    a[4] = checkcode & 0x0000f800;
    a[5] = checkcode & 0x041f0000;
    
    a[1] <<= 0x0c;
    a[2] <<= 0x01;
    a[3] >>= 0x0a;
    a[4] <<= 0x10;
    a[5] >>= 0x0f;
    
    checkcode =  a[1] + a[2] + a[3] + a[4] + a[5];

    // insert scambled checkcode
    dw_2_chars(buf+20, checkcode);
}

/* 
   ICQ protocol version 5 send()

*/
int send_v5(int s, char *msg, size_t len, int flags)
{
    if (msg[0] == 0x05 && msg[1] == 0x00) {
        send(s, msg, len, flags);
        v5_encrypt((byte *)msg, len);
// 	debug_socket("||||||||| encrypted\t", msg, len);
    }
    
    return send(s, msg, len, flags);
}

int main(int argc, char *argv[])
{
    int retval1, retval2;
    struct sockaddr_in udp_local_addr, udp_remote_addr, 
        tcp_local_addr, tcp_remote_addr;

    printf("ICQ <-> TCP bridge v0.1.0\n\
\n\
http://eicq.sourceforge.net/\n\n");
  
    if (argc != 4) {
        printf("Invalid number of parameters.\n\
\n\
Usuage: %s [udp_host] [udp_remote_port] [tcp_local_port]\n\
\n\
For example, %s icq.mirabilis.com 4000 4001\n\n", argv[0], argv[0]);
        exit(1);
    };
  
    char *udp_remote_host = argv[1];
    int udp_remote_port = atoi(argv[2]);
    int tcp_local_port = atoi(argv[3]);


    printf("creating....\n");
    udp_sock = socket(AF_INET, SOCK_DGRAM, 0);
    tcp_sock = socket(AF_INET, SOCK_STREAM, 0);

    sock_error_exit(udp_sock == -1 || tcp_sock == -1, "socket");



    printf("binding....\n");

    udp_local_addr.sin_family = AF_INET;
    udp_local_addr.sin_port = 0; // any port
    udp_local_addr.sin_addr.s_addr = INADDR_ANY;
    memset(&(udp_local_addr.sin_zero), 0, 8);

    tcp_local_addr.sin_family = AF_INET;
    tcp_local_addr.sin_port = htons(tcp_local_port);
    tcp_local_addr.sin_addr.s_addr = INADDR_ANY;
    memset(&(tcp_local_addr.sin_zero), 0, 8);

    retval1 = bind(udp_sock, (struct sockaddr *)&udp_local_addr,
                   sizeof(struct sockaddr));
    retval2 = bind(tcp_sock, (struct sockaddr *)&tcp_local_addr,
                   sizeof(struct sockaddr));

    sock_error_exit(retval1 == -1 || retval2 == -1, "bind");



    printf("connecting....\n");
    struct hostent *udp_host = gethostbyname(udp_remote_host);

    if (udp_host == NULL) {
        herror("gethostbyname"); // instead of perror
        close(udp_sock);
        close(tcp_sock);
        exit(1);
    }

    udp_remote_addr.sin_family = AF_INET;
    udp_remote_addr.sin_port = htons(udp_remote_port);
    udp_remote_addr.sin_addr = *((struct in_addr *)udp_host->h_addr);
    memset(&(udp_remote_addr.sin_zero), 0, 8);
    
    retval1 = connect(udp_sock, (struct sockaddr *)&udp_remote_addr,
                      sizeof(struct sockaddr));

    sock_error_exit(retval1 == -1, "connect");


  
    printf("listening....\n");
    retval1 = listen(tcp_sock, BACKLOG);
    sock_error_exit(retval1 == -1, "listen");



    printf("accepting....\n");
#ifdef __CYGWIN__
    int sin_size = sizeof(struct sockaddr_in);
#else
    size_t sin_size = sizeof(struct sockaddr_in);
#endif
    tcp_new_sock = accept(tcp_sock, (struct sockaddr *)&tcp_remote_addr,
                          &sin_size);

    sock_error_exit(tcp_new_sock == -1, "accept");

    printf("accepted connection from %s",
	   inet_ntoa(tcp_remote_addr.sin_addr));
    printf(" at port %d\n", tcp_local_port);

    printf("bridging....\n");
    int numchars = 0;
    struct timeval tv;
    int quit = 0;
    fd_set rfds;
    char *buf = new char[MAXDATASIZE];
    int large_sock = 
        tcp_new_sock > udp_sock ? tcp_new_sock : udp_sock;

    while (!quit) {
        // put inside while loop to reset every loop for Linux
        tv.tv_sec = 2;
        tv.tv_usec = 500000;
        FD_ZERO(&rfds);
        FD_SET(udp_sock, &rfds);
        FD_SET(tcp_new_sock, &rfds);

        retval1 = select(large_sock+1, &rfds, NULL, NULL, &tv);

        if (retval1 == 0) {
            // printf("timeout\n");
            continue;
        }
        
        sock_error_exit(retval1 < 0, "select");

        // from UDP_REMOTE_HOST to TCP_HOST
        if (FD_ISSET(udp_sock, &rfds)) {
            numchars = recv(udp_sock, buf+2, MAXDATASIZE-2, 0);

            sock_error_exit(numchars == -1, "recv");

            // length prefix
            buf[0] = (dword)numchars & 0xff;
            buf[1] = ((dword)numchars >> 8) & 0xff;
            debug_socket("---------\t", buf+2, numchars);
            retval1 = send(tcp_new_sock, buf, numchars+2, 0);
            sock_error_exit(retval1 == -1, "send");
        }

        // from TCP_HOST to UDP_REMOTE_HOST
        if (FD_ISSET(tcp_new_sock, &rfds)) {
            numchars = recv(tcp_new_sock, buf, MAXDATASIZE, 0);

            sock_error_exit(retval1 == -1, "recv");
            if (numchars == 0) {
                printf("tcp host signals terminates\n");
                sock_close();
                exit(1);
            }

            // debug raw uncut packets
            // debug_socket("********* raw\t", buf, numchars);

            char *p = buf;
            dword len;
            while (p < buf+numchars) {
                len = (byte)p[0] + ((byte)p[1] << 8);
                p += 2;			// skip length prefix
                if (p+len <= buf+numchars && len > 0) {
                    debug_socket("/////////\t", p, len);	
                    retval1 = send_v5(udp_sock, p, len, 0);
                    sock_error_exit(retval1 == -1, "send");
                    p += len;
                } else {
                    debug_socket("********* error\t", buf, numchars);
                    break;
                }	
            }
        }
    }

 

    sock_close();
    return 0;
}

 


