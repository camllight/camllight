char * runtime_name = "camlrun";
char * errmsg = "Cannot exec camlrun.\n";

int main(argc, argv)
     int argc;
     char ** argv;
{
  execvp(runtime_name, argv);
  write(2, errmsg, strlen(errmsg));
  return 2;
}
