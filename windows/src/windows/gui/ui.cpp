#include "stdafx.h"             

#define UI
#include "camlwin.h"

#include "io.h"    
#include "time.h"    

extern "C" int ui_write(int fd, char *text, int len)
{
  if (fd == 1 || fd == 2) {
    char buffer[1024];
	int i, j;
	for (i = 0, j = 0; i < len; i++) {
	  if (j >= sizeof(buffer) - 2) {
	    buffer[j] = 0;
	    DoWrite(CAMLPrinter, buffer);
		j = 0;
	  }
	  switch (text[i]) {
	  case 10:
	    buffer[j++] = 13;
		buffer[j++] = 10;
		break;
      case 0:
		break;
	  default:
	    buffer[j++] = text[i];
		break;
	  }
	}
	buffer[j] = 0;
    DoWrite (CAMLPrinter, buffer);
    return len;
  }else{
    return write (fd, text, len);
  }
}	


extern "C" int ui_read (int fd, char * p, int len)
{
  if (fd == 0){
    char buffer[1024];
	int n, i, j;
	n = DoRead (buffer, len < 1023 ? len : 1023);
	for (i = 0, j = 0; i < n; i++) {
	  if (buffer[i] != 13) p[j++] = buffer[i];
	}
	return j;
  }else{
    return read (fd, p, len);
  }
}                     

extern "C" int ui_exit(int retcode)
{
	return DoExit(retcode);
}

extern "C" void ui_gc_message(char *fmt, unsigned long arg)
{
}

extern "C" void ui_fatal_error(char *fmt, char *arg)
{		 
	char buf[1024];
	sprintf(buf, fmt, arg);
	AfxMessageBox(buf);
}

#define	MAXCOUNT	3
static 	int 	maxcount=MAXCOUNT;
static	int		count=3;

clock_t	lastdate=0;

// #define ADAPTATIVE
extern "C" void ui_periodic_action(void)
{
	// Bof... 
#ifdef	ADAPTATIVE
	--count;
	if (count <= 0){
		clock_t date;
		LookEvent(0);
		date = clock();
		if(date - lastdate < 15 && maxcount > 0) --maxcount;
		if(date - lastdate > 15 && maxcount < MAXCOUNT) ++maxcount;
		count = maxcount;
		lastdate = date;
	}
#else
		LookEvent(0);
#endif
}

#undef ADAPTATIVE

