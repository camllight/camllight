char * runtime_name = "camlrun.exe";
char * errmsg = "Cannot exec camlrun.exe.\n";

int main(argc, argv)
     int argc;
     char ** argv;
{
  char * newargv[64];
  int i;
  newargv[0] = runtime_name;
  for (i = 0; i <= argc; i++) newargv[i+1] = argv[i];
  execvp(runtime_name, newargv);
  write(2, errmsg, sizeof(errmsg)-1);
  return 2;
}
