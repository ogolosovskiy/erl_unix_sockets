
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include <ei.h>
#include <erl_driver.h>


#define MAX_PATH 1024

int get_string(char* input_buf, int* index, char* out_buf, int out_buf_size)
{
  int type = 0, size = 0;

  memset(out_buf, 0, out_buf_size);
  ei_get_type(input_buf, index, &type, &size); 
  if (type != ERL_STRING_EXT)  
    { 
      fprintf(stderr, "PortDriver: bad arg, unexpected argument type %c, expect ERL_STRING_EXT 'k'", type); 
      return 1;
    } 

  if (size >= out_buf_size)  
    { 
      fprintf(stderr, "PortDriver: Wrong size, expected %d", size); 
      return 1;
    } 

  if (ei_decode_string(input_buf, index,out_buf)) 
    {
      fprintf(stderr, "PortDriver: bad arg, cant read %d argument, string", *index);
      return 1;
    }

  return 0;
}


void open_socket()
{

  int    sd=-1, rc;//, bytesReceived;
  char   buffer[1024];
  struct sockaddr_un serveraddr;

       sd = socket(AF_UNIX, SOCK_STREAM, 0);
       if (sd < 0)
         {
           fprintf(stderr,"/n socket() failed /n");
           return;
         }

       memset(&serveraddr, 0, sizeof(serveraddr));
       serveraddr.sun_family = AF_UNIX;
       strcpy(serveraddr.sun_path, "/var/run/unison/srtpproxy.sock");

       rc = connect(sd, (struct sockaddr *)&serveraddr, SUN_LEN(&serveraddr));
       if (rc < 0)
         {
           fprintf(stderr,"/n connect() failed /n");
           return;
         }


       memset(buffer, 'a', sizeof(buffer));
       rc = send(sd, buffer, sizeof(buffer), 0);
       if (rc < 0)
         {
           fprintf(stderr,"/n send() failed /n");
           return;
         }

       /********************************************************************/
       /* In this example we know that the server is going to respond with */
       /* the same 250 bytes that we just sent.  Since we know that 250    */
       /* bytes are going to be sent back to us, we can use the          */
       /* SO_RCVLOWAT socket option and then issue a single recv() and     */
       /* retrieve all of the data.                                        */
       /*                                                                  */
       /* The use of SO_RCVLOWAT is already illustrated in the server      */
       /* side of this example, so we will do something different here.    */
       /* The 250 bytes of the data may arrive in separate packets,        */
       /* therefore we will issue recv() over and over again until all     */
       /* 250 bytes have arrived.                                          */
       /********************************************************************/
       /*       bytesReceived = 0;
       while (bytesReceived < BUFFER_LENGTH)
         {
           rc = recv(sd, & buffer[bytesReceived],
                     BUFFER_LENGTH - bytesReceived, 0);
           if (rc < 0)
             {
               perror("recv() failed");
               break;
             }
           else if (rc == 0)
             {
               printf("The server closed the connection\n");
               break;
             }

           bytesReceived += rc;
         }

         } while (FALSE); */

   fprintf(stderr,"/nclose/n");
   if (sd != -1)
     close(sd);
}


struct pipedrv {
	ErlDrvPort port;
	char *string_arg;
	long int_arg;
};

typedef struct pipedrv pipedrv_t;

static ErlDrvEntry pipedrv_driver_entry;

static void encode_error_ex(ei_x_buff* buff, char* error_atom, char* error_desc_string) 
{
	ei_x_encode_tuple_header(buff, 3);
	ei_x_encode_atom(buff, "error");
	ei_x_encode_atom(buff, error_atom);
	ei_x_encode_string(buff, error_desc_string);
}

static void encode_error(ei_x_buff* buff, char* error_atom) 
{
	ei_x_encode_tuple_header(buff, 2);
	ei_x_encode_atom(buff, "error");
	ei_x_encode_atom(buff, error_atom);
}

extern "C" ErlDrvData pipedrv_start(ErlDrvPort port, char *buf)
{
	pipedrv_t* d = (pipedrv_t*)driver_alloc(sizeof(pipedrv_t));
	d->port = port;
	return (ErlDrvData)d;
}

extern "C" void pipedrv_stop(ErlDrvData handle)
{
	driver_free((char*)handle);
}

/* messages from erlang */
extern "C" void pipedrv_output(ErlDrvData handle, char *buff, int bufflen)
{
	pipedrv_t* d = (pipedrv_t*)handle;
	int index = 0, version = 0, arity = 0;//, type = 0, size = 0;
	ei_x_buff result;
	char operation[MAXATOMLEN];

        /* Prepare the output buffer that will hold {ok, Result} or {error, Reason} */
        if (ei_x_new_with_version(&result))// || ei_x_encode_tuple_header(&result, 2)) 
          {
            fprintf(stderr, "PortDriver: cant alloc output buffer");
            encode_error(&result, "badalloc");
            goto done;
          }

        /* Ensure that we are receiving the binary term by reading and stripping the version byte */
	if (ei_decode_version(buff, &index, &version)) 
          {
            fprintf(stderr, "PortDriver: wrong data version ");
            encode_error(&result, "badver");
            goto done;
          }

        /* Our marshalling spec is that we are expecting a tuple {Command, Arg1, Arg2} */
	if (ei_decode_tuple_header(buff, &index, &arity) || ei_decode_atom(buff, &index, operation)) 
          {
            fprintf(stderr, "PortDriver: bad arg, expected tuple {Command, Arg1, Arg2}");
            encode_error(&result, "badarg");
            goto done;
          }


	if (!strcmp("open", operation)) 
          {
               char str[256];
               char error[1024];
               long int num = 0;

               /* ei_get_type(buff, &index, &type, &size); */
               /* if (type != ERL_STRING_EXT)  */
               /*   { */
               /*     fprintf(stderr, "PortDriver: bad arg, unexpected argument type %c, expect ERL_STRING_EXT 'k'", type); */
               /*     encode_error(&result, "badarg"); */
               /*     goto done; */
               /*   } */

               if (ei_decode_string(buff, &index,(char*) &str)) 
                 {
                   sprintf(error, "PortDriver: bad arg, cant read %d argument, string", index);
                   encode_error_ex(&result, "badarg",(char*) &error);
                   goto done;
                 }
               
               if (ei_decode_long(buff, &index, &num)) 
                 {
                   sprintf((char*)(&error), "PortDriver: bad arg, cant read %d argument, long", index);
                   encode_error_ex(&result, "badarg", (char*)&error);
                   goto done;
                 }

               // to do
               open_socket();
               
               ei_x_encode_atom(&result, "ok");
          }
        else 
	if (!strcmp("socket", operation)) 
          {
            char path[MAX_PATH];
            int sd = -1, res = 0;
            struct sockaddr_un serveraddr;
            
            if(get_string(buff, &index, (char*)path, MAX_PATH))
              {
                encode_error(&result, "badarg");
                goto done;
              }

            sd = socket(AF_UNIX, SOCK_STREAM, 0);
            if (sd < 0)
              {
                fprintf(stderr,"/n socket() failed /n");
                encode_error(&result, "socket");
                goto done;
              }

            memset(&serveraddr, 0, sizeof(serveraddr));
            serveraddr.sun_family = AF_UNIX;
            strcpy(serveraddr.sun_path,(char*)&path);

            res = connect(sd, (struct sockaddr *)&serveraddr, SUN_LEN(&serveraddr));
            if (res < 0)
              {
                //                GetLastError();
                fprintf(stderr,"/n connect() failed /n");
                encode_error(&result, "connect");
                goto done;
              }

          
          }
        else 
          {
            fprintf(stderr, "PortDriver: bad arg, unexpected operation %s", operation);
            encode_error(&result, "badarg");
            goto done;
          }

done:
	driver_output(d->port, result.buff, result.index);
	ei_x_free(&result);
}

/*
 * Initialize and return a driver entry struct
 */

extern "C" DRIVER_INIT(pipedrv)
{
	pipedrv_driver_entry.init         = NULL;   /* Not used */
	pipedrv_driver_entry.start        = pipedrv_start;
	pipedrv_driver_entry.stop         = pipedrv_stop;
	pipedrv_driver_entry.output       = pipedrv_output;
	pipedrv_driver_entry.ready_input  = NULL;
	pipedrv_driver_entry.ready_output = NULL;
	pipedrv_driver_entry.driver_name  = "pipe_drv";
	pipedrv_driver_entry.finish       = NULL;
	pipedrv_driver_entry.outputv      = NULL;
	return &pipedrv_driver_entry;
}



