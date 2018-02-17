#include <stdio.h>
#include "xneslplot.h"

#define MAX_GCS 1000
int gcindex = 0;
GC gcarray[MAX_GCS];
Colormap colormap;
Visual *visual_p;
Font default_font;
long current_event_mask;

Display *getdisplay(char *displayname)
{
  Display *disp;
  if ((disp = XOpenDisplay(displayname)) == NULL){
    fprintf(stderr, "Cannot connect to Xserver %s\n",
	    XDisplayName(NULL));
    exit(-1);
  }
  return disp;
}

int width, height, x0,y0;
FILE *psstream = NULL;

void Create_Ps(int x, int y, int w, int h, char *filename)
{
  psstream = fopen(filename,"w");
  width = w; height = h;
  x0 = x; y0 = y;
  if (psstream == NULL) {
    fprintf(stderr,"Could not open Post Script output file ~a\n",filename);
    exit(1);
  }
  fputs(ps_header1,psstream);
  fprintf(psstream,"%s: %d %d %d %d\n","%%BoundingBox",x,y,x+w,y+h);
  fputs(ps_header2,psstream);
}

void Close_Ps()
{
  fputs(ps_tailer,psstream);
  fclose(psstream);
}

void Create_Window(int x, int y, int width, int height, 
		   char *title, char *displayname, int z)
{
  int screen;
  int param[2];
  unsigned long mask, fore_pix, back_pix;
  XSizeHints	size_hints;
  XWMHints wm_hints;
  XClassHint class_hints;
  XTextProperty wp, ip;
  static char *windowname;
  static char *iconname;
  XSetWindowAttributes winattr;
  XEvent ev;
    
  if (strlen(title) == 0) {
    windowname = (char *)malloc(strlen(DEFAULT_TITLE)+1);
    strcpy(windowname, DEFAULT_TITLE);
  }
  else windowname = title;

  iconname = (char *)malloc(strlen(ICONNAME)+1);
  strcpy(iconname, ICONNAME);

  display = getdisplay(displayname);

  screen = DefaultScreen(display);
  fore_pix = WhitePixel(display, screen);
  back_pix = BlackPixel(display, screen);
  colormap = DefaultColormap(display, screen);
  printf("( %d )\n",DefaultVisual(display, screen)->class);
  fflush(stdout);

  win = XCreateSimpleWindow (display, RootWindow(display,screen),
			     x,y, width, height,
			     3,	/* border width */
			     fore_pix,back_pix);

  /* USPosition is supposed to mean user specified position */
  size_hints.flags = USPosition | USSize;

  /* This says that window will initially start exposed (instead of as
     an icon */
  wm_hints.initial_state = NormalState;
  wm_hints.flags = StateHint;
    
  /* Who knows what these do?? */
  class_hints.res_name = "program name";
  class_hints.res_class = "program class name";

  if (XStringListToTextProperty(&windowname, 1, &wp) == 0 ||
      XStringListToTextProperty(&iconname, 1, &ip) == 0 ){
    fprintf(stderr,"name allocation failed\n");
    exit(-1);
  }
  XSetWMProperties(display, win, &wp, &ip, NULL, 0,
		   &size_hints, &wm_hints, &class_hints);

  winattr.backing_store = Always;
  winattr.bit_gravity = NorthWestGravity;
  mask = CWBackingStore | CWBitGravity;

  XSelectInput(display, win, ExposureMask);
  XMapWindow(display,win);
  do {
    XNextEvent(display,&ev);
    if (ev.type == Expose)
      break;
  } while (1); 

  XSelectInput(display, win, NoEventMask);
  current_event_mask = NoEventMask;
  XChangeWindowAttributes(display, win, mask, &winattr);
  default_font = XLoadFont(display,default_font_name);
}

void clear_window()   
{
     XClearWindow(display,win);
     XFlush(display);
}

void resize_window()
{
    int x,y,width,height;

    x = get_number();
    y = get_number();
    width = get_number();
    height = get_number();
    XMoveResizeWindow(display,win,x,y,width,height);
    XFlush(display);
}

void nextgc(unsigned long mask, XGCValues *values_p) {
  XGCValues v;
  int fl;
  int screen = DefaultScreen(display);
  if (gcindex < MAX_GCS) {
    gcarray[gcindex] = XCreateGC(display, RootWindow(display, screen), 
				 mask, values_p);
    /*fprintf(stderr,"%d\n",gcarray[gcindex]);*/
    /*fl = XGetGCValues(display,  gcarray[gcindex], mask,&v);*/
    /*fprintf(stderr,"%d %d %d %d\n",fl,v.line_style, v.fill_style,v.dashes);*/
    put_number(gcindex++);
  }
  else put_number(-1);
}

void dash_linegc()
{
  unsigned long mask;
  XGCValues values;
  char            dash_list[] = { 100, 100 };
  static GC       new_gc,gcline;
  XGCValues v;
  int fl,screen= DefaultScreen(display);


  values.function = get_number();
  values.foreground = get_number();
  values.background = get_number();
  gcline = DefaultGC(display,DefaultScreen(display));
  new_gc = XCreateGC(display,RootWindow(display, screen),0,(XGCValues *) 0);
  XCopyGC(display,gcline,~0,new_gc);
  gcline = new_gc;
  XSetLineAttributes(display,gcline,0,LineOnOffDash,CapNotLast,JoinMiter);
  fl = XSetDashes(display,gcline,0,dash_list,2);
  mask = GCFunction | GCForeground | GCBackground | GCLineWidth | GCFillStyle | GCLineStyle | GCDashList;

  if (gcindex < MAX_GCS) {
    gcarray[gcindex] = gcline;
/*    fprintf(stderr,"%d\n",gcarray[gcindex]); */
/*     XGetGCValues(display,  gcarray[gcindex], mask,&v);
    fprintf(stderr,"%d %d %d %c\n",fl,v.line_style, v.line_width,v.dashes);*/
    put_number(gcindex++);
  }
  else put_number(-1);
}
/*

  values.function = get_number();
  values.foreground = get_number();
  values.background = get_number();
  values.line_style =  LineOnOffDash;
  values.fill_style = FillSolid;
  values.line_width = 2;
  values.dashes = 10;
  nextgc(mask,&values);
}
*/

void linegc()
{
  unsigned long mask;
  XGCValues values;

  mask = GCFunction | GCForeground | GCBackground | GCLineWidth | GCFillStyle;
  values.function = get_number();
  values.foreground = get_number();
  values.background = get_number();
  values.fill_style = FillSolid;
  values.line_width = 1;
  nextgc(mask,&values);
}

void stipple_gc()
{
  unsigned long mask;
  XGCValues values;
  int i;
  int fl;
  int screen = DefaultScreen(display);
  Pixmap pixi;
 char stupid_bits[] = {
   0x01, 0x02};


  mask = GCFunction | GCForeground | GCBackground | GCFillStyle;
  values.function = get_number();
  values.foreground = get_number();
  values.background = get_number();
  values.fill_style = FillStippled;
  if (gcindex < MAX_GCS) {
    gcarray[gcindex] = XCreateGC(display, RootWindow(display, screen),
                                 mask, &values);
    pixi = XCreateBitmapFromData(display,win,stupid_bits,2,2);
    XSetStipple(display,gcarray[gcindex],pixi);
    put_number(gcindex++);
  }
  else put_number(-1);
}

void colorgc()
{
  unsigned long mask;
  XGCValues values;
  XColor color;
  Status result;

  mask = GCFunction | GCForeground | GCFillStyle  | GCFont;
  values.function = get_number();
  color.red = get_number();
  color.blue = get_number();
  color.green = get_number();
  values.fill_style = FillSolid;
  color.flags = DoRed|DoGreen|DoBlue;
  values.font = default_font;

  result = XAllocColor(display, colormap, &color);
  if( !result ) {
    fprintf(stderr,"Unable to allocate color!");
    exit(1);
  }
  values.foreground = color.pixel;
  nextgc(mask,&values);
}

void textgc()
{
  unsigned long mask;
  XGCValues values;
  char *fontname;
  int screen = DefaultScreen(display);

  mask = GCFunction | GCForeground | GCBackground | GCFont;
  values.function = get_number();
  values.foreground = get_number();
  values.background = get_number();
  fontname = get_string();
  values.font = XLoadFont(display,fontname);
  nextgc(mask,&values);
}

void tilegc()
{
  unsigned long mask;
  XGCValues values;
  int screen = DefaultScreen(display);
  int l, i, *bitmap;
  unsigned char bitmap_bits [8];
  Pixmap map;

  mask = ( GCFunction | GCFillStyle | GCTile );
  values.function = get_number();
  values.fill_style = FillTiled;
  l = get_number();
  if (l != 8) fprintf(stderr,"Wrong number of bitmap's input bytes.\n");
  bitmap = read_int_array(l);
  for (i=0; i<8 ; i++)  { bitmap_bits[i] = bitmap[i]; }
  map = XCreateBitmapFromData(display, RootWindow(display,screen),
			      bitmap_bits, name_width, name_height);
  values.tile = map;

  nextgc(mask,&values);
}

void set_line_width(GC gc, int width)
{
  XSetLineAttributes(display,gc,width,LineSolid,CapRound,JoinRound);
}

void exit_now()
{
  if (psstream != NULL) Close_Ps();
  exit(0); 
}

void exit_on_mouse()
{
  if (psstream != NULL) exit_now();
  else
    {
      XEvent ev;
      XButtonEvent* pev = (XButtonEvent *) &ev;
      XSelectInput(display, win, ButtonPressMask);
      XMapWindow(display,win);
      XNextEvent(display,&ev);
      exit(0);
    }
}

void select_input()
{
  current_event_mask = get_number();
  XSelectInput(display, win, current_event_mask);
}

void keysym() {
  put_number(XKeycodeToKeysym(display,get_number(),get_number()));
}

void next_event()
{
  XEvent ev;
  XButtonEvent* pev = (XButtonEvent *) &ev;
  XNextEvent(display,&ev);
  printf("( %d ) ( %d ) ( %d ) ( %d ) ( %d )\n", 
	 pev->type, pev->x, pev->y,pev->state,pev->button);
  fflush(stdout);
}

void next_event_noblock()
{
  XEvent ev;
  int isevent;
  XButtonEvent* pev = (XButtonEvent *) &ev;
  isevent = XCheckMaskEvent(display,current_event_mask,&ev);
  printf("( %d ) ( %d ) ( %d ) ( %d ) ( %d ) ( %d )\n", 
	 isevent, pev->type, pev->x, pev->y,pev->state,pev->button);
  fflush(stdout);
}

void rect(int gci, int x0, int y0, int x, int y) 
{
  int w,h;
  if (x >= x0) w = x-x0;
  else {w = x0-x; x0 = x;}
  if (y >= y0) h = y-y0;
  else {h = y0-y; y0 = y;}
  XDrawRectangle(display, win, gcarray[gci], x0, y0, w, h);
}

void get_box()
{
  int w, gci, x0, y0, x, y;
  XEvent ev;
  XButtonEvent* pev = (XButtonEvent *) &ev;
  x0 = x = get_number();
  y0 = y = get_number();
  w = get_number();
  gci = get_number();
  set_line_width(gcarray[gci],w);
  rect(gci,x0,y0,x,y);
  do {
    XNextEvent(display,&ev);
    rect(gci,x0,y0,x,y);
    x = pev->x; y = pev->y;
    if (pev->type == ButtonRelease) break;
    rect(gci,x0,y0,x,y);      
  } while(1);
  printf("( %d ) ( %d )\n", x,y);
  fflush(stdout);
}

void set_font()
{
  char *name = get_string();
  XSetFont(display,gcarray[get_number()],XLoadFont(display,name));
}

void white_pixel()
{
  printf("( %d )\n",WhitePixel(display, DefaultScreen(display)));
  fflush(stdout);
}

void black_pixel()
{
  printf("( %d )\n",BlackPixel(display, DefaultScreen(display)));
  fflush(stdout);
}

void psdrawpoints(XPoint *plist, int l) {
  int i;
  fprintf(psstream,"%d setlinewidth\n", 1);
  for (i = 0; i < l; i++)
    fprintf(psstream,"%d %d P\n",x0+plist[i].x,y0+height-plist[i].y);
}

void get_screen_size() {
 int w,h;
 h = XDisplayHeight(display,DefaultScreen(display));
 w = XDisplayWidth(display,DefaultScreen(display));
 /*fprintf(stderr,"%d %d width height\n",w,h);*/
 put_number(w);
 put_number(h);
}


void readdrawpoints()
{
  XPoint *plist;
  int *gclist;
  int w,l,i,p,eq_p;
  l = get_number();
  plist = read_points(l);
  gclist = read_int_array(l);

  if  (psstream != NULL) psdrawpoints(plist,l);
  else
  if (l > 0) {
    if (all_equal(gclist,l))
      XDrawPoints(display, win, gcarray[gclist[0]], plist, l, 0);
    else
      for (i = 0; i < l ; i++)
	XDrawPoint(display, win, gcarray[gclist[i]], plist[i].x, plist[i].y);
    XFlush(display);
  }
}

void readdrawimage()
{
  XImage *image;
  int i;
  int x0 = get_number();
  int y0 = get_number();
  int width = get_number();
  int height = get_number();
  int l = get_number();
  int *gclist = read_int_array(l);
  if (psstream == NULL) {
    for (i = 0 ; i < l ; i++) {
      /* fprintf(stderr," %d", gcarray[gclist[i]]); */
	XDrawPoint(display, win, gcarray[gclist[i]], 
		   x0 + i%width, y0 + i/width);
    }
    XFlush(display);
  }
  /* image = XGetImage(display,win,0,0,width,height,255,ZPixmap);
  for (i = 0; i < 100; i++)
    fprintf(stderr," %d",XGetPixel(image,i,0)); */
}

void readdrawrectangles()
{
  XPoint *plist, *slist;
  int *width_list, *gclist;
  int l,i,gcp,w;

  l = get_number();
  plist = read_points(l);
  slist = read_points(l);
  width_list = read_int_array(l);  
  gclist = read_int_array(l);

  gcp = -2;
  w = -1;

  for(i = 0; i < l; i++)
    if (psstream == NULL) {
      if (gcp != gclist[i] || width_list[i] != w) {
	gcp = gclist[i];
	w = width_list[i];
	set_line_width(gcarray[gcp],w);
      }
      XDrawRectangle(display, win, gcarray[gcp],
		     plist[i].x,plist[i].y-slist[i].y,slist[i].x,slist[i].y);
    }
  if (psstream == NULL) XFlush(display);
}

void psdrawlines(XPoint *plist, int w, int l) {
  int i;
  if (l > 0) {
    fprintf(psstream,"%d setlinewidth\n", w);
    fprintf(psstream,"%d %d LS\n",x0+plist[0].x,y0+height-plist[0].y);
    for (i = 1; i < l; i++)
      fprintf(psstream,"%d %d L\n",x0+plist[i].x,y0+height-plist[i].y);
  }
}

void readdrawlines()
{
  int *seg_list, *width_list, *gclist;
  XPoint *plist;
  int i, l, li, w, s, gcp;

  l = get_number();
  width_list = read_int_array(l);
  gclist = read_int_array(l);
  seg_list = read_int_array(l);
  s = sum(seg_list,l);
  plist = read_points(s);

  w = -1;
  gcp = -2;
  for(i = 0; i < l; i++) { 
    li = seg_list[i];
    if (psstream == NULL) {
      if (gcp != gclist[i] || width_list[i] != w) {
	gcp = gclist[i];
	w = width_list[i];
	set_line_width(gcarray[gcp],w);
      }
      XDrawLines(display, win, gcarray[gcp], plist, li, 0);
    }
    else psdrawlines(plist,width_list[i],li);
    plist += li;
  }
  if (psstream == NULL) XFlush(display);
}

void psdrawsegments(XSegment *slist, int w, int l) {
  int i;
  fprintf(psstream,"%d setlinewidth\n", w);
  for (i = 0; i < l; i++)
    fprintf(psstream,"%d %d %d %d S\n",
            x0+slist[i].x1,y0+height-slist[i].y1,
            x0+slist[i].x2,y0+height-slist[i].y2);
}

void readdrawsegments()
{
  int *seg_list, *width_list, *gclist;
  XSegment *slist;
  int i, l, li, w, s, gcp;

  l = get_number();
  width_list = read_int_array(l);
  gclist = read_int_array(l);
  seg_list = read_int_array(l);
  s = sum(seg_list,l);
  slist = read_segments(s);

  gcp = -2;
  w = -1;
  for(i = 0; i < l; i++) { 
    li = seg_list[i];
    if (psstream == NULL) {
      if (gcp != gclist[i] || width_list[i] != w) {
	gcp = gclist[i];
	w = width_list[i];
	set_line_width(gcarray[gcp],w);
      }
      XDrawSegments(display, win, gcarray[gcp], slist, li);
    }
    else psdrawsegments(slist,width_list[i],li);
    slist += li;
  }
  if (psstream == NULL) XFlush(display);
}

void readdrawarcs()
{
  int  *gclist,*wlist;
  XArc *alist,arc;
  int i, l, gcp,w;

  l = get_number();
  gclist = read_int_array(l);
  wlist = read_int_array(l);
  alist = read_arcs(l);

  gcp = -2;
  w = -1;
  for(i = 0; i < l; i++) {
    if (psstream == NULL) {
      if (gcp != gclist[i] || w != wlist[i]) {
        gcp = gclist[i];
        w = wlist[i];
        set_line_width(gcarray[gcp],w);
      }
      arc = alist[i];
      XDrawArc(display,win,gcarray[gcp],arc.x,arc.y,arc.width,arc.height,arc.angle1,arc.angle2);
    }
    else fprintf(stderr,"psdrawarcs not installed yet\n");
  }
  if (psstream == NULL) XFlush(display);
}

void shade_arcs()
{
  int  *gclist;
  XArc *alist,arc;
  int i, l, gcp;

  l = get_number();
  gclist = read_int_array(l);
  alist = read_arcs(l);

  gcp = -2;
  for(i = 0; i < l; i++) {
    if (psstream == NULL) {
      if (gcp != gclist[i]) {
        gcp = gclist[i];
      }
      arc = alist[i];
      /*fprintf(stderr,"%d\n%d\n%d\n%d\n%d\n%d\n",
	      arc.x,arc.y,arc.width,arc.height,arc.angle1,arc.angle2);*/
      XFillArc(display,win,gcarray[gcp],arc.x,arc.y,
              arc.width,arc.height,arc.angle1,arc.angle2);
    }
    else fprintf(stderr,"psdrawarcs not installed yet\n");
  }
  if (psstream == NULL) XFlush(display);
}




void psdrawstring(int x, int y, char *str, int l) {
  int i, angle = 0;
  fputc('(',psstream);
  for (i = 0; i < l; i++) fputc(str[i],psstream);
  fputc(')',psstream);
  fprintf(psstream," %d %d %d T\n", angle, x0+x, y0+height-y);
}

void readdrawtext()
{
  XPoint point, *plist;
  int *seg_list, *gclist;
  char *clist;
  int l,i,li;
  char c, *pt, *end;

  l = get_number();
  plist = read_points(l);
  gclist = read_int_array(l);
  seg_list = read_int_array(l);
  clist = read_char_array(sum(seg_list,l));

  for(i = 0; i < l; i++) { 
    li = seg_list[i];
    if (psstream == NULL) {
      XDrawString(display,win,gcarray[gclist[i]],
		  plist[i].x, plist[i].y, clist, li);
    }
    else psdrawstring(plist[i].x, plist[i].y, clist, li);
    clist += li;
  }
  if (psstream == NULL) XFlush(display);
}

void shade_polygon()
{
  int *seg_list, *gclist;
  XPoint *plist;
  int i, l, s, li;

  l = get_number();
  gclist = read_int_array(l);
  seg_list = read_int_array(l);
  s = sum(seg_list,l);
  plist = read_points(s);

  for(i = 0; i < l; i++) { 
    li = seg_list[i];
    if (psstream == NULL) {
      XFillPolygon(display, win, gcarray[gclist[i]], plist, li, Complex, 
		   CoordModeOrigin);
    }
    plist += li;
  }
  if (psstream == NULL) XFlush(display);
}

int main() {
  int mode,x0,y0,width,height;
  char *display_name, *title, *par_place;

  display_name = get_string();
  title = get_string();
  mode = get_number();
  x0 = get_number();
  y0 = get_number();
  width = get_number();
  window_height = get_number();
  

  if (mode == 0) Create_Window(x0,y0,width,window_height,title,display_name,0);
  else Create_Ps(x0,y0,width,window_height,display_name);
  do {   
    int command = get_number();
    /* fprintf(stderr, "Start: command %d \n", command); */
    switch (command) {
    case 0: readdrawpoints(); break;
    case 1: readdrawlines();  break;
    case 2: readdrawsegments(); break;
    case 3: readdrawarcs(); break;
    case 4: exit_now(); break;
    case 5: readdrawtext(win); break;
    case 6: clear_window(win); break;
    case 7: shade_arcs(); break;
    case 8: shade_polygon(); break;
    case 9: linegc(); break;
    case 10: tilegc(); break;
    case 11: textgc(); break;
    case 12: keysym(); break;
    case 13: colorgc(); break;
    case 14: stipple_gc(); break;
    case 15: readdrawimage(); break;
    case 18: readdrawrectangles(); break;
    case 19: select_input(); break;
    case 20: next_event(); break;
    case 21: get_box(); break;
    case 22: get_screen_size();break;
    case 23: resize_window();break;
    case 24: next_event_noblock(); break;
    case 25: white_pixel(); break;
    case 26: black_pixel(); break;
    default: exit_on_mouse();
    }
  } while(1);
}
