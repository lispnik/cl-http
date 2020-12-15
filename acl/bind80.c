/* 
cc bind80.c -o bind80 -lsocket -lnsl

This is the SunOS/Solaris version of "bind80" to enable CL-HTTP
users to use port 80 on their local workstation, by Olivier Clarisse
based on a solution originally proposed by David W. Dykstra.

Usage: bind80 must be owned by root at your site, and be
set with permissions 4775 to enable usage by unprivileged
users. When invoked in this mode, bind80 binds port 80 for
TCP listen and then renounces its root privileges
(taking back user privileges) before invoking the program
passed as argument and adding the resulting XX file descriptor
using listen_fd=XX as last parameter to exec.

Disclaimer: Have a local system administrator verify this program
for safety before setting its root privileges. This utility is
provided AS IS, with no explicit or implied warranty whatsoever.
Use at your own risk.

Comments: Forward comments and suggestions to this mailing list:
	Bug-acl-cl-http@ai.mit.edu.

*/

#include <sys/types.h>
#include <sys/socket.h>
#ifndef LINUX
#include <rpcsvc/nis.h>
#endif
#include <netinet/in.h>

/*
struct sockaddr {
	u_short	sa_family;		/* address family *//*
	char	sa_data[14];		/* up to 14 bytes of direct address *//*
};
*/
/* address definition really used
struct sockaddr_in {
	short	sin_family;
	u_short	sin_port;
	struct	in_addr sin_addr;
	char	sin_zero[8];
};
*/

/* Can't seem to link bzero BSD definition safely */
bzero(char* str, int size)
{
  int i = 0;
  for(;i < size; i++)
    *str++ = '\0';
}

#define MAXARGS 124

main (int argc, char* argv[], char* envp[])
{
  /* Hardcoded port to be open by a local root */
  int port = 80;
  struct sockaddr_in sockaddr;
  struct sockaddr *sockaddrp;
  int result;
  int listen_fd;
  int n;
  char listen_arg[12];
  char* nargv[MAXARGS];
  int nuid, ngid;
  
  bzero((char *) &sockaddr, sizeof(sockaddr));
  listen_fd = socket(AF_INET, SOCK_STREAM, 0);
  if ( listen_fd < 0 ) {
    printf("Error: Cannot open socket %d\n", port);
    perror("Socket error");
    exit(1);
  }
  sockaddr.sin_family = AF_INET;
  sockaddr.sin_port = htons(port);
  sockaddrp = (struct sockaddr *) &sockaddr;
  result = bind(listen_fd, sockaddrp, sizeof(sockaddr));
  if ( result != 0 ) {
    printf("Error: Cannot bind socket %d\n", port);
    perror("Bind error");
  }
  else {
    printf("Bind successful, socket=%d, fd=%d\n", port, listen_fd);
    if (0 < argc) {
      if (argc <= MAXARGS) {
	for( n = 1; n < argc; n++ )
	  nargv[n-1] = argv[n];
	sprintf(listen_arg, "listen_fd=%d", listen_fd);
	nargv[argc-1] = listen_arg;
	nargv[argc] = (char *) NULL;
	/* Set proper ownership */
	nuid = getuid();
	ngid = getgid();
	if ( nuid == 0 || ngid == 0 ) {
	  printf ("Error: Access denied for uid=%d or gid=%d.\n", nuid, ngid);
	  exit(1);
	}
	result = setuid(nuid);
	if ( result != 0 ) {
	  printf("Error: Cannot set user id to %d\n", nuid);
	  exit(1);
	}
	result = setgid(ngid);
	if ( result != 0 ) {
	  printf("Error: Cannot set group id to %d\n", ngid);
	  exit(1);
	}
	/* int execve (const char *path, char *const argv[], char *const envp[]); */
	result = execve(nargv[0], nargv, envp);
	if ( result != 0 ) {
	  printf("Error: Cannot exec this command:\n", port);
	  for( n = 0; n < argc; n++ )
	    if (nargv[n] == (char *) NULL)
	      break;
	    else
	      printf("%s ", nargv[n]);
	  printf("\n");
	  perror("Exec error");
	}
      }
      else
	printf("Error: too many arguments passed on command line.");
    }
  }
}
