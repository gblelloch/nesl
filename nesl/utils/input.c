#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

void begin_paren_vector()
{
  char c1, c2;
  c1 = getchar();
  c2 = getchar();
  if (c2 == EOF) exit_on_mouse();
  if (!((c1 == '(' || c1 == '[') && c2 == ' ')) {
    fprintf(stderr,"Bad input to xneslplot.\nDid you remember to use kill_window or close_window?\n");
    exit_on_mouse();
  }
}

void end_paren_vector()
{
  char c1, c2;
  c1 = getchar();
  c2 = getchar();
  if (!((c1 == ')' || c1 == ']') && c2 == '\n')) {
    fprintf(stderr,"Bad input to xneslplot (end_paren_vect).\n");
    exit_on_mouse();
  }
}

void end_paren_vector_with_space()
{
  char c1, c2, c3;
  c1 = getchar();
  c2 = getchar();
  c3 = getchar();
  if (!(c1 == ' ' && (c2 == ')' || c2 == ']') && c3 == '\n')) {
    fprintf(stderr,"Bad input to xneslplot (end_paren_vect_with_space).\n");
    exit_on_mouse();
  }
}

int get_number()
{
  int i,r;
  begin_paren_vector();
  r = scanf("%d", &i);
  if (r < 1) fprintf(stderr,"Bad input to xneslplot (get_number).\n");
  end_paren_vector_with_space();
  return i;
}

void put_number(int i) 
{
  printf("( %d ) ",i);
  fflush(stdout);
}

char *get_string()
{
  int l,i,c;
  char* str;
  l = get_number();
  l = get_number();
  c = 0; 
  i = 0;
  str = (char *) malloc(sizeof(char)*l + 1);
  while (i < l && c != EOF)  {
    c = getchar();
    str[i++] = c;
  }
  if (c == EOF) 
    fprintf(stderr,"Bad input to xneslplot (get_string).\n");
  str[i] = '\0';
  return str;
}

int sum(int *vals, int l)
{
  int i;
  int sum = 0;
  for (i = 0; i < l; i ++) sum += vals[i];
  return sum;
}

int all_equal(int *vals, int l)
{
  int i,val = 1;
  if (l < 2) return 1;
  for (i = 1; i < l; i++) val = val && (vals[i-1] == vals[i]);
  return val;
}

int read_int()
{
  int c,r;
  r = 0;
  c = getchar();
  while (c == ' ' || c == '\n') 
    c = getchar();
  if (c == '-') {
    c = getchar();
    while (c >= '0' && c <= '9') {
      r = 10*r - (c - '0');
      c = getchar();
    }
  }
  else
    while (c >= '0' && c <= '9') {
      r = 10*r + c - '0';
      c = getchar();
    }
  return r;
}

int *read_int_array(int l)
{
  int i, *int_list;

  int_list = (int *) malloc(sizeof(int)*l);

  begin_paren_vector();
  for (i = 0; i < l ; i++) int_list[i] = read_int();
  end_paren_vector();

  return int_list;
}

char *read_char_array(int l)
{
  int i;
  char *char_list;

  char_list = (char *) malloc(sizeof(int)*l);

  begin_paren_vector();
  for (i = 0; i < l ; i++) char_list[i] = (char) read_int();
  end_paren_vector();

  return char_list;
}

char *old_read_char_array()
{
  int i, l;
  char *char_list;
  l = get_number();
  char_list = (char *) malloc(sizeof(char)*l);

  for (i = 0; i < l ; i++) char_list[i] = getchar();

  return char_list;
}

XPoint *read_points(int l)
{
  int i;
  XPoint *plist = (XPoint *) malloc(sizeof(XPoint)*l);

  begin_paren_vector();
  for (i = 0; i < l ; i++) plist[i].x = read_int();
  end_paren_vector();

  begin_paren_vector();
  for (i = 0; i < l ; i++) plist[i].y = read_int();
  end_paren_vector();

  return plist;
}

XSegment *read_segments(int l)
{
  int i;
  XSegment *slist = (XSegment *) malloc(sizeof(XSegment)*l);

  begin_paren_vector();
  for (i = 0; i < l ; i++) slist[i].x1 = read_int();
  end_paren_vector();

  begin_paren_vector();
  for (i = 0; i < l ; i++) slist[i].y1 = read_int();
  end_paren_vector();

  begin_paren_vector();
  for (i = 0; i < l ; i++) slist[i].x2 = read_int();
  end_paren_vector();

  begin_paren_vector();
  for (i = 0; i < l ; i++) slist[i].y2 = read_int();
  end_paren_vector();

  return slist;
}

XArc *read_arcs(int l)
{
  int i;
  XArc *alist = (XArc *) malloc(sizeof(XArc)*l);

  begin_paren_vector();
  for (i = 0; i < l ; i++)  alist[i].x = read_int();
  end_paren_vector();

  begin_paren_vector();
  for (i = 0; i < l ; i++) alist[i].y = read_int();
  end_paren_vector();

  begin_paren_vector();
  for (i = 0; i < l ; i++) alist[i].width = read_int();
  end_paren_vector();

  begin_paren_vector();
  for (i = 0; i < l ; i++) alist[i].height = read_int();
  end_paren_vector();

  begin_paren_vector();
  for (i = 0; i < l ; i++) alist[i].angle1 = read_int();
  end_paren_vector();

  begin_paren_vector();
  for (i = 0; i < l ; i++) alist[i].angle2 = read_int();
  end_paren_vector();

  return alist;
}

