#ifndef _str_
#define _str_


#ifdef ANSI

mlsize_t string_length(value);
value compare_strings(value, value);

#else

mlsize_t string_length();
value compare_strings();

#endif


#endif /* _str_ */
