
#include <stdarg.h>
#include <string>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sstream>
#include <stdexcept>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <sys/stat.h>


#include <ei.h>
#include <erl_driver.h>


/*
                                \\\|///
                              \\  - -  //
                               (  @ @  )
     O-----------------------oOOo-(_)-oOOo--------------------O
     |	                                                      |
     |      "Unix Socket port driver for Erlang"              |
     |      Author:                                           |
     |      Golosovsky Oleg  ogolosovskiy@gmail.com           |
     |                                                        |
     |                                Oooo                    |
     O------------------------oooO---(   )--------------------O
                             (   )    ) /
                              \ (    (_/
                               \_)
*/


class pdexception : public std::runtime_error
{
public:
  pdexception(std::string const& a_atom, std::string const& a_desc) 
      : atom(a_atom)
      , description(a_desc)
      , std::runtime_error(a_desc) 
  { }
  ~pdexception() throw() {}

  std::string atom;
  std::string description;
};

std::string get_string(char* input_buf, int* index)
{
  int type = 0, size = 0;
  int const out_buf_size = 1024;
  char out_buf[out_buf_size];

  memset(out_buf, 0, out_buf_size);
  ei_get_type(input_buf, index, &type, &size); 
  if (type != ERL_STRING_EXT)  
    { 
      std::ostringstream oss;
      oss << "PortDriver: bad arg, unexpected argument type " << type << " expect ERL_STRING_EXT 'k'"; 
      throw pdexception("badarg",oss.str());
    } 

  if (size >= out_buf_size)  
      throw pdexception("badarg", "too large string argument");

  if (ei_decode_string(input_buf, index, out_buf)) 
      throw pdexception("badarg", "cant read string argument");

  return std::string(out_buf);
}


long get_long(char* input_buf, int* index)
{
  int type = 0, size = 0;
  long num = 0;

  ei_get_type(input_buf, index, &type, &size); 
  if (type != ERL_SMALL_INTEGER_EXT)  
    { 
      std::ostringstream oss;
      oss << "PortDriver: bad arg, unexpected argument type " << (char)type << " expect " << ERL_SMALL_INTEGER_EXT; 
      throw pdexception("badarg",oss.str());
    } 

  if (ei_decode_long(input_buf, index, &num)) 
      throw pdexception("badarg", "cant read int argument");

  //  fprintf(stderr, "Decoded: %d \n", num);
  return num;
}



void close_pipe(int& socket_handle)
{
  //  fprintf(stderr, "Close socket: %d \n", socket_handle);
  ::close(socket_handle);
  socket_handle = 0;
}


int socket()
{

  int sd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (sd <= 0)
    throw pdexception("socket", strerror(errno));

  fprintf(stderr, "Opened socket: %d \n", sd);

  return sd;
}


void connect(std::string const& path, int socket_handle)
{

  struct sockaddr_un serveraddr;
  memset(&serveraddr, 0, sizeof(serveraddr));
  serveraddr.sun_family = AF_UNIX;
  strcpy(serveraddr.sun_path, path.c_str());

  if(connect(socket_handle, (struct sockaddr *)&serveraddr, SUN_LEN(&serveraddr))<0)
      throw pdexception("connect", strerror(errno));

}

void bind(std::string const& local_path, int socket_handle)
{

  struct sockaddr_un clientaddr;
  memset(&clientaddr, 0, sizeof(clientaddr));
  clientaddr.sun_family = AF_UNIX;
  strcpy(clientaddr.sun_path, local_path.c_str());

  unlink(clientaddr.sun_path);

  if(bind(socket_handle, (struct sockaddr *)&clientaddr, SUN_LEN(&clientaddr))<0)
      throw pdexception("bind", strerror(errno));

  if(chmod(clientaddr.sun_path, S_IRWXU)<0)
      throw pdexception("chmod", strerror(errno));
}


void send(std::string const& data, int socket_handle)
{
  fprintf(stderr, "Send: %s to %d\n", data.c_str(), socket_handle );
  int rc = write(socket_handle, data.c_str(), data.size());
  if (rc < 0)
    {
      fprintf(stderr, "Exception: %s ", strerror(errno) );
      throw pdexception("send", strerror(errno));
    }
}

std::string recv(int socket_handle)
{
  fprintf(stderr, "Receiving: from %d\n", socket_handle );

  int rc = 0;
  int const BUFFER_LENGTH = 1024;
  char buffer[BUFFER_LENGTH];
  memset(buffer, 0, BUFFER_LENGTH);

  rc = recv(socket_handle, &buffer[0], BUFFER_LENGTH, 0);

  if (rc < 0)
    throw pdexception("recv", strerror(errno));

  return std::string(buffer, rc);
}


typedef struct pipedrv pipedrv_t;

static ErlDrvEntry pipedrv_driver_entry;

static void encode_error_ex(ei_x_buff* buff, char const* error_atom, char const* error_desc_string) 
{
        fprintf(stderr,"\n Ex Error: \n%s:%s\n", error_atom, error_desc_string);
	ei_x_encode_tuple_header(buff, 3);
	ei_x_encode_atom(buff, "error");
	ei_x_encode_atom(buff, error_atom);
	ei_x_encode_string(buff, error_desc_string);
}

struct pipedrv {
  ErlDrvPort port;
};

extern "C" ErlDrvData pipedrv_start(ErlDrvPort port, char *buf)
{
  //        fprintf(stderr, "Drivers start");
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

        try
          {

            /* Prepare the output buffer that will hold {ok, Result} or {error, Reason} */
            if (ei_x_new_with_version(&result))// || ei_x_encode_tuple_header(&result, 2)) 
              throw pdexception("badalloc", "PortDriver: cant alloc output buffer");

            /* Ensure that we are receiving the binary term by reading and stripping the version byte */
            if (ei_decode_version(buff, &index, &version)) 
              throw pdexception("badver", "PortDriver: wrong data version ");

            // /* Our marshalling spec is that we are expecting a tuple {Command, Arg1 ...} */
            if (ei_decode_tuple_header(buff, &index, &arity) || ei_decode_atom(buff, &index, operation)) 
              throw pdexception("badarg", "PortDriver: bad arg, expected tuple {Command, Arg1 ...}");

            // fprintf(stderr, "version: %d index: %d operation: %s ", version, index, operation);
            if (!strcmp("socket", operation)) 
              {
                int socket_handle = socket();
                ei_x_encode_tuple_header(&result, 2);
                ei_x_encode_atom(&result, "ok");
                ei_x_encode_long(&result, socket_handle); 
              }
            else 
            if (!strcmp("bind", operation)) 
              {
            
                std::string path = get_string(buff, &index);
                int handle = get_long(buff, &index);

                if (handle == 0)
                  throw pdexception("bind", "port is not opened");

                bind(path, handle);
                ei_x_encode_atom(&result, "ok");
              }
            else 
            if (!strcmp("connect", operation)) 
              {
            
                std::string path = get_string(buff, &index);
                int handle = get_long(buff, &index);

                if (handle == 0)
                  throw pdexception("connect", "port is not opened");

                connect(path, handle);
                ei_x_encode_atom(&result, "ok");
              }
            else 
              if (!strcmp("send", operation)) 
                {
                  std::string data = get_string(buff, &index);
                  int handle = get_long(buff, &index);
                  send(data, handle);
                  ei_x_encode_atom(&result, "ok");
                }
              else 
                if (!strcmp("close", operation)) 
                  {
                    int handle = get_long(buff, &index);
                    close_pipe(handle);
                    ei_x_encode_atom(&result, "ok");
                  }
                else
                  if (!strcmp("recv", operation)) 
                    {
                      int handle = get_long(buff, &index);
                      std::string res = recv(handle);
                      ei_x_encode_tuple_header(&result, 2);
                      ei_x_encode_atom(&result, "ok");
                      ei_x_encode_string(&result, res.c_str());
                    }
                  else 
                    throw pdexception("badarg","PortDriver: bad arg, unexpected operation");

          }
        catch(pdexception const& ex)
          {
            encode_error_ex(&result, ex.atom.c_str(), ex.description.c_str());
            fprintf(stderr, "exception :%s\n", ex.description.c_str() );
            //            close_pipe(d->socket_handle);
          }


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
	pipedrv_driver_entry.driver_name  = (char*)"pipe_drv";
	pipedrv_driver_entry.finish       = NULL;
	pipedrv_driver_entry.outputv      = NULL;
	return &pipedrv_driver_entry;
}



