/* Caml Light preprocessor.
   A very restricted subset of CPP.
*/

#include <stdio.h>

#ifdef __STDC__
#include <stdlib.h>
#else
char *malloc ();
char *realloc ();
#endif

/************************** errors are fatal. */
void fatal (msg)
  char *msg;
{
  fprintf (stderr, "%s\n", msg);
  exit (3);
}

void error (msg, filename)
  char *msg, *filename;
{
  fprintf (stderr, "%s in file %s\n", msg, filename);
  exit (3);
}

/************************** buffers: extensible character strings */
typedef struct {
  char *data;
  unsigned long len;
  unsigned long size;
} buf;

/* operations:
    buf_new (size)        allocate new buffer
	buf_kill (buf*)       deallocate
    buf_add (buf*, char)  append char to buffer
	buf.len, buf.data     get contents
	buf.len = 0;          clear the buffer
	buf_string (buf*)     zero-terminate contents
*/

/* Allocate a new buffer.  [size] is the initial size and must be > 0. */
buf *buf_new (size)
  unsigned long size;
{
  buf *result;
  char *data;
  
  result = (buf *) malloc (sizeof (buf));
  data = malloc (size + 1);
  if (result == NULL || data == NULL) fatal ("out of memory (buf_new)");
  result->data = data;
  result->len = 0;
  result->size = size;
  return result;
}

/* Deallocate a buffer. */
void buf_kill (b)
  buf *b;
{
  free (b->data);
  free (b);
}

void buf_grow (b, c)
  buf *b;
  char c;
{
  b->data = realloc (b->data, b->size * 2 + 1);
  if (b->data == NULL) fatal ("out of memory (buf_grow)");
  b->size *= 2;
  b->data[b->len++] = c;
}

#define buf_add(b, c) ((b)->len < (b)->size \
                        ? (void) ((b)->data[(b)->len++] = (c)) \
                        : buf_grow ((b), (c)))

void buf_string (b)
  buf *b;
{
  b->data [b->len] = '\0';
}

/************************** strings */

/* make a (newly-allocated) copy */
char *str_copy (str)
  char *str;
{
  char *result = malloc (strlen (str) + 1);
  if (result == NULL) fatal ("out of memory (str_copy)");
  strcpy (result, str);
  return result;
}

/************************** character types */
#define ALPHA (1<<0)
#define ALNUM (1<<1)
#define BLANK (1<<2)
#define DIGIT (1<<3)
unsigned char chartype [256];

void init_chartype ()
{
  int i;
  for (i = 0; i < 255; i++) chartype [i] = 0;
  for (i = 'A'; i <= 'Z'; i++) chartype [i] = ALPHA | ALNUM;
  for (i = 'a'; i <= 'z'; i++) chartype [i] = ALPHA | ALNUM;
  chartype ['_'] = ALPHA | ALNUM;
  for (i = '0'; i <= '9'; i++) chartype [i] = DIGIT | ALNUM;
  chartype [' '] = BLANK;
  chartype ['\t'] = BLANK;
}

/************************** symbol table */
/* We don't expect many identifiers.  A sequential list will do */

typedef struct listelem {
  struct listelem *next;
  char *name;
  char *value;
} listelem;

listelem *ident_head = NULL;

/* add a new identifier to the list */
void ident_add (name, value)
  char *name, *value;
{
  listelem *el = (listelem *) malloc (sizeof (listelem));
  if (el == NULL) fatal ("out of memory (ident_add)");
  el->next = ident_head;
  el->name = name;
  el->value = value;
  ident_head = el;
}

/* get the value of an identifier, or NULL if not defined */
char *ident_get (name)
  char *name;
{
  listelem *cur = ident_head;
  while (cur != NULL){
    if (!strcmp (cur->name, name)) return cur->value;
    cur = cur->next;
  }
  return NULL;
}

/************************** automata */
#define begin_automaton int c; {
#define end_automaton }
#define done() buf_kill (b)
#define state(s) } s: c = getc (in); if (0) {
#define class(cl) }else if (c != EOF && (chartype [c] & (cl))){
#define chr(ch) }else if (c == (ch)){
#define chr2(ch1, ch2) }else if (c == (ch1) || c == (ch2)){
#define other }else{

#define ignore()
#define output() putchar (c)
#define save() buf_add (b, c)
#define pushback() (c == EOF ? 0 : ungetc (c, in))
#define flush() (fwrite (b->data, 1, b->len, stdout), b->len = 0)
#define erase() b->len = 0

/* read the tail of an identifier (all but the first char) into buf */
void read_ident (in, b)
  FILE *in;
  buf *b;
{
  int c;
  while (1){
    c = getc (in);
	if (c == EOF) break;
	if (chartype [c] & ALNUM){
	  buf_add (b, c);
	}else{
	  ungetc (c, in);
	  break;
	}
  }
  buf_string (b);
}

enum retcode {pr_else, pr_endif, pr_eof};

/* skip everything except [#ifdef], [#else], and [#endif] */
int process_ignore (in, curfile)
  FILE *in;
  char *curfile;
{
  buf *b = buf_new (1024);
  unsigned long temp;

  begin_automaton
  state (i_startline)
	class (BLANK)   ignore (); goto i_startline;
	chr ('#')       ignore (); goto i_sharp;
	other           pushback (); goto i_normal;
  state (i_sharp)
    class (BLANK)   ignore (); goto i_sharp;
	class (ALPHA)   
	  erase ();
	  save ();
	  read_ident (in, b);
	  if (!strcmp (b->data, "ifdef")){
		temp = process_ignore (in, curfile);
		if (temp == pr_eof) error ("unterminated #ifdef", curfile);
		if (temp == pr_endif) goto i_startline;
		temp = process_ignore (in, curfile);
		if (temp == pr_eof) error ("unterminated #ifdef", curfile);
		if (temp == pr_endif) goto i_startline;
		error ("unexpected #else", curfile);
	  }else if (!strcmp (b->data, "else")){
		done (); return pr_else;
	  }else if (!strcmp (b->data, "endif")){
		done (); return pr_endif;
	  }else{
		erase (); goto i_normal;
	  }
	other           ignore (); goto i_normal;
  state (i_normal)
    chr ('\\')      ignore (); goto i_escape;
    chr ('\n')      ignore (); goto i_startline;
	chr (EOF)       ignore (); done (); return pr_eof;
	other           ignore (); goto i_normal;
  state (i_escape)
    other           ignore (); goto i_normal;
  end_automaton
}

void process_file ();

int process (in, curfile)
  FILE *in;
  char *curfile;
{
  unsigned long temp;
  buf *b = buf_new (1024);
  char *str_temp;

  begin_automaton
  state (p_startline)
    class (BLANK)   save (); goto p_startline;
	chr ('#')       save (); goto p_sharp;
	other           pushback (); flush (); goto p_normal;
  state (p_sharp)
    class (BLANK)   save (); goto p_sharp;
	class (ALPHA)
	  temp = b->len;
	  save ();
	  read_ident (in, b);
	  if (!strcmp (b->data+temp, "ifdef")){
	    erase (); goto p_ifdef;
	  }else if (!strcmp (b->data+temp, "else")){
	    done (); return pr_else;
	  }else if (!strcmp (b->data+temp, "endif")){
	    done (); return pr_endif;
	  }else if (!strcmp (b->data+temp, "include")){
	    erase (); goto p_include;
	  }else if (!strcmp (b->data+temp, "define")){
	    erase (); goto p_define;
	  }else{
	    flush (); goto p_normal;
	  }
	other           flush (); pushback (); goto p_normal;
  state (p_ifdef)
    class (BLANK)   ignore (); goto p_ifdef;
	class (ALPHA)
	  erase ();
	  save ();
	  read_ident (in, b);
	  if (ident_get (b->data) == NULL){
	    erase ();
		temp = process_ignore (in, curfile);
		if (temp == pr_eof) error ("unterminated #ifdef", curfile);
		if (temp == pr_endif) goto p_startline;
		temp = process (in, curfile);
		if (temp == pr_eof) error ("unterminated #ifdef", curfile);
		if (temp == pr_endif) goto p_startline;
		error ("unexpected #else", curfile);
	  }else{
	    erase ();
		temp = process (in, curfile);
		if (temp == pr_eof) error ("unterminated #ifdef", curfile);
		if (temp == pr_endif) goto p_startline;
		temp = process_ignore (in, curfile);
		if (temp == pr_eof) error ("unterminated #ifdef", curfile);
		if (temp == pr_endif) goto p_startline;
		error ("unexpected #else", curfile);
	  }
	other    error ("identifier missing for #ifdef", curfile);
  state (p_include)
    class (BLANK)   ignore (); goto p_include;
	chr ('"')       ignore (); erase (); goto p_filename;
	other           error ("file name missing for #include", curfile);
  state (p_filename)
    chr ('"')
	  ignore ();
	  buf_string (b);
	  {
	    FILE *f = fopen (b->data, "r");
		if (f == NULL) error ("cannot find included file", curfile);
		process_file (f, b->data);
	  }
	  goto p_normal;
	chr2('\n', EOF) error ("ill-formed file name for #include", curfile);
	other           save (); goto p_filename;
  state (p_define)
    class (BLANK)   ignore (); goto p_define;
	class (ALPHA)
	  erase ();
	  save ();
	  read_ident (in, b);
	  str_temp = str_copy (b->data);
	  erase ();
	  goto p_defval;
	other           error ("identifier missing for #define", curfile);
  state (p_defval)
    class (BLANK)   ignore (); goto p_defval;
	other           pushback (); goto p_value;
  state (p_value)
    chr ('\\')      ignore (); goto p_valesc;
	chr2 ('\n', EOF)
	  ignore ();
	  if (b->len == 0){
	    ident_add (str_temp, "1");
	  }else{
		buf_string (b);
		ident_add (str_temp, str_copy (b->data));
	  }
	  erase ();
	  goto p_startline;
	other           save (); goto p_value;
  state (p_valesc)
    chr ('\n')      ignore (); goto p_value;
	chr (EOF)       buf_add (b, '\\'); pushback (); goto p_value;
	other           buf_add (b, '\\'); save (); goto p_value;
  state (p_normal)
    chr ('\n')      output (); goto p_startline;
	chr (EOF)       ignore (); done (); return pr_eof;
	class (DIGIT)   output (); goto p_number;
	chr ('"')       output (); goto p_string;
	chr ('\'')      output (); goto p_char;
	class (ALPHA)
	  save ();
	  read_ident (in, b);
	  str_temp = ident_get (b->data);
	  if (str_temp == NULL){
	    flush ();
		goto p_normal;
	  }else{
	    erase ();
		fputs (str_temp, stdout);
		goto p_normal;
	  }
	other           output (); goto p_normal;
  state (p_number)
    chr2 ('E', 'e') output (); goto p_sign;
    class (ALNUM)   output (); goto p_number;
	chr ('.')       output (); goto p_number;
	other           pushback (); goto p_normal;
  state (p_sign)
    chr2 ('+', '-') output (); goto p_number;
	other           pushback (); goto p_number;
  state (p_string)
    chr ('"')       output (); goto p_normal;
	chr ('\\')      output (); goto p_stringesc;
	chr (EOF)       error ("unterminated string literal", curfile);
	other           output (); goto p_string;
  state (p_stringesc)
    chr (EOF)       error ("unterminated string literal", curfile);
    other           output (); goto p_string;
  state (p_char)
    chr ('\'')      output (); goto p_normal;
	chr ('\\')      output (); goto p_charesc;
	chr (EOF)       error ("unterminated char literal", curfile);
	other           output (); goto p_char;
  state (p_charesc)
    chr (EOF)       error ("unterminated char literal", curfile);
    other           output (); goto p_char;
  end_automaton
}

void process_file (in, name)
  FILE *in;
  char *name;
{
  int result = process (in, name);
  if (result == pr_endif) error ("unexpected #endif", name);
  if (result == pr_else) error ("unexpected #else", name);
}

/************************** start */

/* usage: clprepro [symbol ...] <input >output */

int main (argc, argv)
  int argc;
  char *argv[];
{
  int i;
  int fileargs = 0;
  char *p;
  init_chartype ();
  for (i = 1; i < argc; i++){
    if (argv [i][0] == '-' && argv [i][1] == 'D'){
	  char *arg = str_copy (argv [i]+2);
	  char *val = "1";
	  for (p = arg; *p != '\0'; p++){
	    if (*p == '='){
		  *p = '\0';
		  val = p+1;
		  break;
		}
	  }
      ident_add (arg, val);
	}else{
	  FILE *f = fopen (argv [i], "r");
	  if (f == NULL){
	    fprintf (stderr, "cannot open file %s\n", argv [i]);
		exit (3);
	  }
	  fileargs = 1;
	  process_file (f, argv [i]);
	}
  }
  if (!fileargs) process_file (stdin, "(stdin)");
  fflush (stdout);
  exit (0);
  return 0;
}
