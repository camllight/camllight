
#define eq(s1,s2) (strcmp(s1,s2) == 0)
int suffix(char * name, char * suff);
int prefix(char * name, char * pref);
char * change_suffix(char * name, char * suff);
char * modulename(char * filename);
char * strconc(char * s1, char * s2);

typedef struct stringlist_struct * stringlist;
stringlist add(stringlist l, char * s);
stringlist add2(stringlist l, char * s1, char * s2);

void tempname(char * template);

#define STR 0
#define LST 1
#define END 2
#define NOTERM 0
#define TERMINATE 1
int execute(int terminate, char * cmd, ...);

int redirect_stdout(char * outputname);
void end_redirect_stdout();
int append(char * srcname, char * dstname);
