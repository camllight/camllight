let cmdtable = (hashtbl__new 19 : (string, action list) hashtbl__t);;

let def name action =
  hashtbl__add cmdtable name action;;

let find_macro name =
  try
    hashtbl__find cmdtable name
  with Not_found ->
    prerr_string "Unknown macro: "; prerr_endline name; [];;

(* General LaTeX macros *)

def "\\part"
    [Print "<H0>"; Print_arg; Print "</H0>\n"];
def "\\chapter"
    [Print "<H1>"; Print_arg; Print "</H1>\n"];
def "\\chapter*"
    [Print "<H1>"; Print_arg; Print "</H1>\n"];
def "\\section"
    [Print "<H2>"; Print_arg; Print "</H2>\n"];
def "\\section*"
    [Print "<H2>"; Print_arg; Print "</H2>\n"];
def "\\subsection"
    [Print "<H3>"; Print_arg; Print "</H3>\n"];
def "\\subsection*"
    [Print "<H3>"; Print_arg; Print "</H3>\n"];
def "\\subsubsection"
    [Print "<H4>"; Print_arg; Print "</H4>\n"];
def "\\subsubsection*"
    [Print "<H4>"; Print_arg; Print "</H4>\n"];
def "\\paragraph"
    [Print "<H5>"; Print_arg; Print "</H5>\n"];
def "\\begin{alltt}" [Print "<pre>"];
def "\\end{alltt}" [Print "</pre>"];
def "\\begin{itemize}" [Print "<p><ul>"];
def "\\end{itemize}" [Print "</ul>"];
def "\\begin{enumerate}" [Print "<p><ol>"];
def "\\end{enumerate}" [Print "</ol>"];
def "\\begin{description}" [Print "<p><dl>"];
def "\\end{description}" [Print "</dl>"];
def "\\begin{center}" [Print "<blockquote>"];
def "\\end{center}" [Print "</blockquote>"];
def "\\smallskip" [];
def "\\medskip" [];
def "\\bigskip" [];
def "\\markboth" [Skip_arg; Skip_arg];
def "\\ldots" [Print "..."];
def "\\ " [Print " "];
def "\\{" [Print "{"];
def "\\}" [Print "}"];
def "\\/" [];
def "\\newpage" [];
def "\\label" [Print "<A name=\""; Print_arg; Print "\"></A>"];
def "\\ref" [Print "<A href=\"#"; Print_arg; Print "\">X</A>"];
def "\\index" [Skip_arg];
def "\\oe" [Print "oe"];
def "\\&" [Print "&amp;"];
def "\\_" [Print "_"];
def "\\leq" [Print "&lt;="];
def "\\geq" [Print "&gt;="];
def "\\hbox" [Print_arg];
def "\\copyright" [Print "(c)"];
def "\\noindent" [];
def "\\\\" [Print "<br>"];
def "\\begin{flushleft}" [Print "<blockquote>"];
def "\\end{flushleft}" [Print "</blockquote>"];
();;

(* Macros specific to the Caml manual *)

def "\\begin{options}" [Print "<p><dl>"];
def "\\end{options}" [Print "</dl>"];
def "\\var" [Print "<i>"; Print_arg; Print "</i>"];
def "\\nth" [Print "<i>"; Print_arg; Print_arg; Print "</i>"];
def "\\nmth" [Print "<i>"; Print_arg; Print "("; Print_arg;
              Print ","; Print_arg; Print ")</i>"];
def "\\begin{unix}" [Print "<dl><dt><b>Unix:</b><dd>"];
def "\\end{unix}" [Print "</dl>"];
def "\\begin{mac}" [Print "<dl><dt><b>Mac:</b><dd>"];
def "\\end{mac}" [Print "</dl>"];
def "\\begin{pc}" [Print "<dl><dt><b>PC:</b><dd>"];
def "\\end{pc}" [Print "</dl>"];
def "\\begin{requirements}" [Print "<dl><dt><b>Requirements:</b><dd>"];
def "\\end{requirements}" [Print "</dl>"];
def "\\begin{troubleshooting}" [Print "<dl><dt><b>Troubleshooting:</b><dd>"];
def "\\end{troubleshooting}" [Print "</dl>"];
def "\\begin{installation}" [Print "<dl><dt><b>Installation:</b><dd>"];
def "\\end{installation}" [Print "</dl>"];
def "\\index" [Skip_arg];
def "\\ikwd" [Skip_arg];
def "\\th" [Print "-th"];
def "\\begin{library}" [];
def "\\end{library}" [];
def "\\begin{comment}" [Print "<dl><dd>"];
def "\\end{comment}" [Print "</dl>"];
def "\\begin{tableau}"
  [Skip_arg; Print "<tbl border>\n<th>"; Print_arg;
   Print "<th>"; Print_arg; Print "<tr>"];
def "\\entree"
  [Print "<td>"; Print_arg; Print "<td>"; Print_arg; Print "<tr>"];
def "\\end{tableau}" [Print "</tbl>\n"];
def "\\begin{gcrule}" [Print "<dl><dt><b>Rule:</b><dd>"];
def "\\end{gcrule}" [Print "</dl>"];
def "\\begin{tableauoperateurs}"
  [Print "<tbl border>\n<th>Operator<th>Associated ident<th>Behavior in the default environment<tr>"];
def "\\end{tableauoperateurs}" [Print "</tbl>\n"];
def "\\entreeoperateur"
  [Print "<td>"; Print_arg; Print "<td>"; Print_arg;
   Print "<td>"; Print_arg; Print "<tr>"];
def "\\fromoneto"
  [Print "<i>"; Print_arg; Print "</i> = 1, ..., <i>";
   Print_arg; Print "</i>"];
();;
