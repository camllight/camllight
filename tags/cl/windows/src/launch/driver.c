#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <process.h>
#include <dos.h>
#include <fcntl.h>
#include <sys\stat.h>
#include "driver.h"

void * xmalloc(int size)
{
  char * res;
  res = malloc(size);
  if (res == NULL) {
    fprintf(stderr, "Out of memory\n");
    exit(2);
  }
  return res;
}

int suffix(char * name, char * suff)
{
  int lname = strlen(name);
  int lsuff = strlen(suff);

  return lname >= lsuff && strcmp(name + lname - lsuff, suff) == 0;
}

int prefix(char * name, char * pref)
{
  return strncmp(name, pref, strlen(pref)) == 0;
}

char * change_suffix(char * name, char * suff)
{
  char * res, * dot;
  res = xmalloc(strlen(name) + strlen(suff) + 1);
  strcpy(res, name);
  dot = strrchr(res, '.');
  if (dot == NULL) {
    strcat(res, suff);
  } else {
    strcpy(dot, suff);
  }
  return res;
}

char * modulename(char * filename)
{
  char * p, * q, * res;
  /* Remove extension */
  q = strrchr(filename, '.');
  /* Find beginning of base name */
  p = q;
  while (p > filename) {
    p--;
    if (*p == '/' || *p == '\\' || *p == ':') break;
  }
  *q = 0;
  res = strdup(p);
  *q = '.';
  return res;
}

char * strconc(char * s1, char * s2)
{
  char * s = xmalloc(strlen(s1) + strlen(s2) + 1);
  strcpy(s, s1);
  strcat(s, s2);
  return s;
}

struct stringlist_struct {
  char * car;
  stringlist cdr;
};

stringlist cons(char * s, stringlist l)
{
  stringlist n = xmalloc(sizeof(struct stringlist_struct));
  n->car = s;
  n->cdr = l;
  return n;
}

stringlist add(stringlist l, char * s)
{
  if (l == NULL) {
    return cons(s, NULL);
  } else {
    stringlist m;
    for (m = l; m->cdr != NULL; m = m->cdr) /*nothing*/;
    m->cdr = cons(s, NULL);
    return l;
  }
}

stringlist add2(stringlist l, char * s1, char * s2)
{
  if (l == NULL) {
    return cons(s1, cons(s2, NULL));
  } else {
    stringlist m;
    for (m = l; m->cdr != NULL; m = m->cdr) /*nothing*/;
    m->cdr = cons(s1, cons(s2, NULL));
    return l;
  }
}

void tempname(char * template)
{
  char * p;
  int n, i, j;

  for (p = template; *p != 'X'; p++) {
    if (*p == 0) return;
  }
  n = 0;
  do {
    for (i = n, j = 3; j >= 0; j--) {
      p[j] = '0' + i % 10;
      i = i / 10;
    }
    n++;
  } while (access(template, 0) == 0);
  return;
}

int execute(int terminate, char * cmd, ...)
{
  va_list ap;
  int argc, i, retcode;
  char ** argv;
  stringlist p;

  argc = 0;
  va_start(ap, cmd);
  while (1) {
    switch(va_arg(ap, int)) {
    case STR:
      va_arg(ap, char *);
      argc++;
      break;
    case LST:
      for(p = va_arg(ap, stringlist); p != NULL; p = p->cdr) argc++;
      break;
    case END:
      goto finished;
    }
  }
finished:
  argv = (char **) xmalloc((argc + 1) * sizeof(char *));
  argv[0] = cmd;
  i = 1;
  va_start(ap, cmd);
  while (1) {
    switch(va_arg(ap, int)) {
    case STR:
      argv[i++] = va_arg(ap, char *);
      break;
    case LST:
      for(p = va_arg(ap, stringlist); p != NULL; p = p->cdr)
        argv[i++] = p->car;
      break;
    case END:
      goto finished2;
    }
  }
finished2:
  argv[i] = NULL;
#ifdef DEBUG
  fprintf(stderr, "\t%s", cmd);
  for (i = 1; i <= argc; i++) fprintf(stderr, " %s", argv[i]);
  fprintf(stderr, "\n");
#endif
  retcode = spawnvp(terminate ? P_OVERLAY : P_WAIT, cmd, argv);
  free(argv);
  if (retcode == -1) {
    perror(cmd);
    return 2;
  }
  return retcode;
}

static int saved_stdout;

int redirect_stdout(char * outputname)
{
  int output;
  saved_stdout = dup(1);
  output = open(outputname, O_WRONLY | O_CREAT | O_TRUNC, S_IREAD | S_IWRITE);
  if (output == -1) return -1;
  dup2(output, 1);
  close(output);
}

void end_redirect_stdout()
{
  dup2(saved_stdout, 1);
  close(saved_stdout);
}

int append(char * srcname, char * dstname)
{
  int status, src, dst, n;
  char buffer[1024];
#ifdef DEBUG
  fprintf(stderr, "\tcat %s >> %s\n", srcname, dstname);
#endif
  src = open(srcname, O_RDONLY | O_BINARY);
  if (src == -1) { perror(srcname); return 1; }
  dst = open(dstname, O_WRONLY | O_APPEND | O_BINARY);
  if (dst == -1) { close(src); perror(dstname); return 1; }
  status = 0;
  while(1) {
    n = read(src, buffer, sizeof(buffer));
    if (n == -1) { perror(srcname); status = 1; break; }
    if (n == 0) break;
    if (write(dst, buffer, n) != n) { perror(dstname); status = 1; break; }
  }
  close(src);
  close(dst);
  return status;
}

