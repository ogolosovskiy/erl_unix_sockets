#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <sys/stat.h>


#define NAME "/var/run/unison/tcp_socket_test.sock"

main()
{
  int tsock, msgsock, rval;
  struct sockaddr_un server;
  char buf[1024];

  tsock = socket(AF_UNIX, SOCK_STREAM, 0);
  if (tsock < 0) {
    perror("opening stream socket");
    exit(1);
  }
  server.sun_family = AF_UNIX;
  strcpy(server.sun_path, NAME);

  printf("TCP Socket has name %s\n", server.sun_path);

  if (bind(tsock, (struct sockaddr *) &server, sizeof(struct sockaddr_un))) {
    perror("binding stream socket");
    exit(1);
  }
  listen(tsock, 5);
  for (;;) 
    {
      msgsock = accept(tsock, 0, 0);
      if (msgsock == -1)
        perror("accept");
      else do 
             {
               bzero(buf, sizeof(buf));
               if ((rval = ::read(msgsock, buf, 1024)) < 0)
                 perror("reading stream message");
               else if (rval == 0)
                 printf("Ending connection\n");
               else
                 {
                   printf("-->%s\n", buf);
                   int err = ::send(msgsock, buf, rval, 0);
                   if(err>=0)
                    printf("<--%s\n", buf);
                   else
                     printf("error: %s\n", strerror(errno));
                 }
             } while (rval > 0);
      close(msgsock);
    }
  close(tsock);
  unlink(NAME);
}


