#ifdef MSDOS
char * runtime_name = "camlrun.exe";
char * errmsg = "Cannot exec camlrun.exe.\n";
#else
char * runtime_name = "camlrun";
char * errmsg = "Cannot exec camlrun.\n";
#endif

int main(argc, argv)
     int argc;
     char ** argv;
{
#ifdef MSDOS
  char * newargv[64];
  int i;
  newargv[0] = runtime_name;
  for (i = 0; i <= argc; i++) newargv[i+1] = argv[i];
  execvp(runtime_name, newargv);
#else
  execvp(runtime_name, argv);
#endif
  write(2, errmsg, strlen(errmsg));
  return 2;
}
