#include <AL/alut.h>
#include <GL/glut.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <unistd.h>

#include "main.h"

#define MENU_BUFSIZE 100
#define SEPERATOR "/"
#define BUFSIZE 8192

static int initWindow(void);
static void loadTexture(char *filename, int format);
static void initTexture(gDisplay *d);
static void ftxEstablishTexture(fonttex *ftx, unsigned char setupMipmaps);
static void getLine(char *buf, int size, FILE *f);
static sgi_texture* load_sgi_texture(char *filename);
static void unload_sgi_texture(sgi_texture *tex);
static void ftxUnloadFont(fonttex *ftx);
static char* getFullPath(char *filename);
static void initFonts(void);
static fonttex *ftxLoadFont(char *filename);
static int getElapsedTime(void);
static void rasonly(gDisplay *d);
static void restoreCallbacks(void);
static void ftxRenderString(fonttex *ftx, char *string, int len);
static void drawText(int x, int y, int size, char *text);
static void initMenuCaption(Menu *m);
static Menu* loadMenu(FILE* f, char* buf, Menu* parent, int level);
static Menu** loadMenuFile(char *filename);
static void drawMenu(gDisplay *d);
static void guiProjection(int x, int y);
static void displayGui(void);
static void idleGui(void);
static void menuAction(Menu *activated);
static void keyboardGui(unsigned char key, int x, int y);
static void  specialGui(int key, int x, int y);
static void initGui(void);
static void initGLGui(void);

static Menu *pCurrent, **pMenuList;
static fonttex *ftx;

static callbacks *current_callback, *last_callback;

static int lasttime, polycount;

static gDisplay *screen;

static struct config cfg = { NULL, NULL, &cfg, &cfg };
static struct config *cfgp = &cfg;

static struct configfn cfgfn[] = { { &doName, &changeName, "\"cl_player_name\"" },
				   { &doHair, &changeHair, "\"cl_player_hair\"" },
				   { &doSkin, &changeSkin, "\"cl_player_skin\"" },
				   { &doShirt, &changeShirt, "\"cl_player_shirt\"" },
				   { &doPants, &changePants, "\"cl_player_pants\"" },
				   { &doJet, &changeJet, "\"cl_player_jet\"" },
				   { &doHairstyle, &changeHairstyle, "\"cl_player_hairstyle\"" },
				   { &doHeadstyle, &changeHeadstyle, "\"cl_player_headstyle\"" },
				   { &doChainstyle, &changeChainstyle, "\"cl_player_chainstyle\"" },
				   { &doSecWeapon, &changeSecWeapon, "\"cl_player_secwep\"" },
				   { &doFullscreen, &changeFullscreen, "\"r_fullscreen\"" },
				   { &doRenderWidth, &changeRenderWidth, "\"r_renderwidth\"" },
				   { &doRenderHeight, &changeRenderHeight, "\"r_renderheight\"" },
				   { &doScreenWidth, &changeScreenWidth, "\"r_screenwidth\"" },
				   { &doScreenHeight, &changeScreenHeight, "\"r_screenheight\"" },
				   { &doFPSLimit, &changeFPSLimit, "\"r_fpslimit\"" },
				   { &doMaxFPS, &changeMaxFPS, "\"r_maxfps\"" },
				   { &doRenderBackground, &changeRenderBackground, "\"r_renderbackground\"" },
				   { &doForceBackground, &changeForceBackground, "\"r_forcebg\"" },
				   { &doBackgroundColorOne, &changeBackgroundColorOne, "\"r_forcebg_color1\"" },
				   { &doBackgroundColorTwo, &changeBackgroundColorTwo, "\"r_forcebg_color2\"" },
				   { &doWeatherEffects, &changeWeatherEffects, "\"r_weathereffects\"" },
				   { &doSmoothEdges, &changeSmoothEdges, "\"r_smoothedges\"" },
				   { &doScaleInterface, &changeScaleInterface, "\"r_scaleinterface\"" },
				   { &doPlayerIndicator, &changePlayerIndicator, "\"ui_playerindicator\"" },
				   { &doKillConsole, &changeKillConsole, "\"ui_killconsole\"" },
				   { &doSwapEffect, &changeSwapEffect, "\"r_swapeffect\"" },
				   { &doDithering, &changeDithering, "\"r_dithering\"" },
				   { &doSoundVolume, &changeSoundVolume, "\"snd_volume\"" },
				   { &doSoundBattle, &changeSoundBattle, "\"snd_effects_battle\"" },
				   { &doSoundExplosions, &changeSoundExplosions, "\"snd_effects_explosions\"" },
				   { &doEnd, &changeEnd, "" } };
static struct configfn *cfgfnp = cfgfn;

static int initWindow(void) {
  int win_id;
  /* char buf[20]; */

  glutInitWindowSize(screen->vp_w, screen->vp_h);
  glutInitWindowPosition(0, 0);

  glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH);

  /*
  sprintf(buf, "%dx%d:16", game->settings->width, game->settings->height);
  glutGameModeString(buf);

  if(glutGameModeGet(GLUT_GAME_MODE_POSSIBLE) &&
     !game->settings->windowMode) {
     win_id = glutEnterGameMode();
  */
    /* check glutGameMode results */
  /*
    printf("Glut game mode status\n");
    printf("  active: %d\n", glutGameModeGet( GLUT_GAME_MODE_ACTIVE));
    printf("  possible: %d\n", glutGameModeGet( GLUT_GAME_MODE_POSSIBLE));
    printf("  width: %d\n", glutGameModeGet( GLUT_GAME_MODE_WIDTH));
    printf("  height: %d\n", glutGameModeGet( GLUT_GAME_MODE_HEIGHT));
    printf("  depth: %d\n", glutGameModeGet( GLUT_GAME_MODE_PIXEL_DEPTH));
    printf("  refresh: %d\n", glutGameModeGet( GLUT_GAME_MODE_REFRESH_RATE));
    printf("  changed display: %d\n",
	   glutGameModeGet( GLUT_GAME_MODE_DISPLAY_CHANGED));

  } else
  */
    win_id = glutCreateWindow("OpenSoldat");
  if (win_id < 0) {
    printf("could not create window...exiting\n");
    exit(1);
  }
  return win_id;
}

static void loadTexture(char *filename, int format) {
  char *path;
  sgi_texture *tex;

  path = getFullPath(filename);
  if(path != 0)
    tex = load_sgi_texture(path);
  else {
    fprintf(stderr, "fatal: could not load %s, exiting...\n", filename);
    exit(1);
  }
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexImage2D(GL_TEXTURE_2D, 0, format, tex->width, tex->height, 0,
	       GL_RGBA, GL_UNSIGNED_BYTE, tex->data);
  free(tex->data);
  free(tex);
}

static void initTexture(gDisplay *d) {
  /* menu icon */
  glGenTextures(1, &(d->texGui));
  glBindTexture(GL_TEXTURE_2D, d->texGui);
  loadTexture("soldat.sgi", GL_RGBA);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
}

static void ftxEstablishTexture(fonttex *ftx, unsigned char setupMipmaps) {
  /* TODO(1): add support for mipmaps */
  int i;

  ftx->texID = (unsigned int*) malloc(ftx->nTextures * sizeof(unsigned int));
  glGenTextures(ftx->nTextures, ftx->texID);

  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  for(i = 0; i < ftx->nTextures; i++) {
    glBindTexture(GL_TEXTURE_2D, ftx->texID[i]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,
                 (*(ftx->textures + i))->width, (*(ftx->textures + i))->height,
                 0, GL_RGBA, GL_UNSIGNED_BYTE,
                 (*(ftx->textures + i))->data);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    /*glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); */
    /*glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST); */
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  }
}

static void getLine(char *buf, int size, FILE *f) {
  do {
    fgets(buf, size, f);
  } while( buf[0] == '\n' || buf[0] == '#');
}

static sgi_texture* load_sgi_texture(char *filename) {
  FILE *f;
  unsigned char buf[BUFSIZE];
  unsigned int x, y, bpc, zsize;
  long count, bytes;
  unsigned char *tmp;
  int i, j;
  sgi_texture *tex;
  
  f = fopen(filename, "r");
  if(f == 0) {
    perror("loading file");
    return 0;
  }
  
  fread(buf, 512, 1, f);
  if((buf[0] << 8) + (buf[1] << 0) != 474) {
    fprintf(stderr, "wrong magic: %d %d\n",
	    buf[0], buf[1]);
    return 0;
  }
  
  if(buf[2] != 0) {
    fprintf(stderr, "RLE compression not supported\n");
    return 0;
  }

  if(buf[3] != 1) {
    fprintf(stderr, "BPC is %d - not supported\n", buf[3]);
    return 0;
  }

  bpc = buf[3];  

  if((buf[10] << 8) + buf[11] != 4) {
    fprintf(stderr, "number of channels is != 4 - not supported\n");
    return 0;
  }

  zsize = (buf[10] << 8) + buf[11];

  x = (buf[6] << 8) + buf[7];
  y = (buf[8] << 8) + buf[9];

  tex = (sgi_texture*) malloc(sizeof(sgi_texture));
  tex->data = malloc(x * y * zsize * bpc);
  tex->width = x;
  tex->height = y;
  tex->channels = zsize;

  count = x * y * zsize * bpc;
  /* fprintf(stderr, "loading %ld bytes\n", count); */
  tmp = (unsigned char*) malloc(count);
  bytes = fread(tmp, count, 1, f);
  /* now, data is in the wrong order: fix that */
  for(i = 0; i < x * y; i++) {
    for(j = 0; j < zsize; j++) {
      *(tex->data + i * zsize + j) = *(tmp + j * x * y + i);
      /* printf("%d -> %d\n", j * x * y + i, i * zsize + j); */
    }
  }
  free(tmp);
  return tex;
}

static void unload_sgi_texture(sgi_texture *tex) {
  free(tex->data);
  free(tex);
}

static fonttex *ftxLoadFont(char *filename) {
  char *path;
  FILE *file;
  char buf[100];
  char texname[100];
  int i;
  
  fonttex *ftx;

  path = getFullPath(filename);
  if(path == 0) {
    fprintf(stderr, "can't load font file '%s'\n", filename);
    return 0;
  }
  
  file = fopen(path, "r");
  free(path);

  // TODO(5): check for EOF errors in the following code
  
  ftx = (fonttex*) malloc(sizeof(fonttex));
  getLine(buf, sizeof(buf), file);
  sscanf(buf, "%d %d %d ", &(ftx->nTextures), &(ftx->texwidth), &(ftx->width));
  getLine(buf, sizeof(buf), file);
  sscanf(buf, "%d %d ", &(ftx->lower), &(ftx->upper));
  getLine(buf, sizeof(buf), file);
  ftx->fontname = malloc(strlen(buf) + 1);
  memcpy(ftx->fontname, buf, strlen(buf) + 1);
  
  ftx->textures = (sgi_texture**) 
    malloc(ftx->nTextures * sizeof(sgi_texture*));
  for(i = 0; i < ftx->nTextures; i++) {
    getLine(buf, sizeof(buf), file);

    // no spaces in texture filesnames
    sscanf(buf, "%s ", texname);
    path = getFullPath(texname);
    if(path == 0) {
      // clean up allocated memory & spit out error
      int j;
      for(j = 0; j < i; j++)
	unload_sgi_texture(*(ftx->textures + j));
      free(ftx->textures);
      free(ftx->fontname);
      free(ftx);
      fprintf(stderr, "can't load texture file '%s'\n", texname);
      return 0;
    }
    *(ftx->textures + i) = load_sgi_texture(path);
    free(path);
  }
  return ftx;
}

static void ftxUnloadFont(fonttex *ftx) {
  int i;
  for(i = 0; i < ftx->nTextures; i++)
    unload_sgi_texture(*(ftx->textures + i));
  free(ftx->textures);
  free(ftx->texID);
  free(ftx->fontname);
  free(ftx);
}

static char* getFullPath(char *filename) {
  char *path;
  FILE *fp = NULL;
  
  path = malloc(strlen(filename) + 1);
  sprintf(path, "%s", filename);

  printf("checking '%s'...", path);
  fp = fopen(path, "r");
  if(fp != 0) {
    fclose(fp);
	printf("ok\n");
    return path;
  }
  free(path);
  printf("unsuccessful\n");

  return 0;
}

static void initFonts(void) {
  char *path;

  if(ftx != NULL) ftxUnloadFont(ftx);
  path = getFullPath("xenotron.ftx");
  if(path != 0) {
    ftx = ftxLoadFont(path);

    if(ftx == NULL) {
      // fprintf(stderr, "fatal: no Fonts available - %s, %s\n",
      //       path, txfErrorString());
      exit(1);
    }
    // printf("using font from '%s'\n", path);
    // txfEstablishTexture(txf, 1, GL_TRUE);
    ftxEstablishTexture(ftx, GL_TRUE);
  } else {
    printf("fatal: could not load font\n");
    exit(1);
  }
}

void setupDisplay(gDisplay *d) {
  screen = d;
  printf("trying to create window\n");
  d->win_id = initWindow();
  printf("window created\n");
  /* printf("win_id is %d\n", d->win_id); */
  printf("loading fonts...\n");
  initFonts();
  printf("loading textures...\n");
  initTexture(d);
}

static int getElapsedTime(void)
{
#ifdef WIN32
	return timeGetTime();
#else
	return glutGet(GLUT_ELAPSED_TIME);
#endif
}

static void rasonly(gDisplay *d) {
  /* do rasterising only (in local display d) */
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0.0f, (GLfloat) d->vp_w, 0.0f, (GLfloat) d->vp_h, 0.0f, 1.0f);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glViewport(d->vp_x, d->vp_y, d->vp_w, d->vp_h);
}

void switchCallbacks(callbacks *new) {
  last_callback = current_callback;
  current_callback = new;

  glutIdleFunc(new->idle);
  glutDisplayFunc(new->display);
  glutKeyboardFunc(new->keyboard);
  glutSpecialFunc(new->special);

  lasttime = getElapsedTime();

 /* printf("callbacks registred\n"); */
  (new->init)();
  (new->initGL)();
  /* printf("callback init's completed\n"); */
}

static void restoreCallbacks(void) {
  if(last_callback == 0) {
    fprintf(stderr, "no last callback present, exiting\n");
    exit(1);
  }
  current_callback = last_callback;

  glutIdleFunc(current_callback->idle);
  glutDisplayFunc(current_callback->display);
  glutKeyboardFunc(current_callback->keyboard);
  glutSpecialFunc(current_callback->special);
  
  lasttime = getElapsedTime();

  fprintf(stderr, "restoring callbacks\n");
}

static void ftxRenderString(fonttex *ftx, char *string, int len) {
  int i;
  int bound = -1;
  int index;
  
  int tex;
  int w;
  float cw;
  float cx, cy;

  w = ftx->texwidth / ftx->width;
  cw = (float)ftx->width / (float)ftx->texwidth;

  for(i = 0; i < len; i++) {
    /* find out which texture it's in */
    /* TODO(4): find out why the +1 is necessary */
    index = string[i] - ftx->lower + 1;
    if(index >= ftx->upper) 
      fprintf(stderr, "index out of bounds");
    tex = index / (w * 
w);
    /* bind texture */
    if(tex != bound) {
      glBindTexture(GL_TEXTURE_2D, ftx->texID[tex]);
      bound = tex;
    }
    /* find texture coordinates */
    index = index % (w * w);
    cx = (float)(index % w) / (float)w;
    cy = (float)(index / w) / (float)w;
    /* draw quad */
    /* fprintf(stderr, "coords: tex %d (%.2f, %.2f), %.2f\n", */
    /*     bound, cx, cy, cw); */

    glBegin(GL_QUADS);
    glTexCoord2f(cx, 1 - cy - cw);
    glVertex2f(i, 0);
    glTexCoord2f(cx + cw, 1 - cy - cw);
    glVertex2f(i + 1, 0);
    glTexCoord2f(cx + cw, 1 - cy);
    glVertex2f(i + 1, 1);
    glTexCoord2f(cx, 1 - cy);
    glVertex2f(i, 1);
    glEnd();
  }
  /* checkGLError("fonttex.c ftxRenderString\n"); */
}

static void drawText(int x, int y, int size, char *text) {
  /* int i; */

  glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_TEXTURE_2D);
  /* txfBindFontTexture(txf); */

  glPushMatrix();

  glTranslatef(x, y, 0);
  glScalef(size, size, size);
  ftxRenderString(ftx, text, strlen(text));
  
  glPopMatrix();
  glDisable(GL_TEXTURE_2D);
  /* 
  glRasterPos2f(x, y);
  for(i = 0; *(text + i) != 0; i++)
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_18, *(text + i));
  */
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  polycount += strlen(text);
}

static void initMenuCaption(Menu *m) {
  cfgfnp = cfgfn;
  while(cfgfnp->doCaption != doEnd) {
    if(strstr(cfgfnp->name, m->szName) != NULL) {
      cfgfnp->doCaption(cfgfnp, m);
      return;
    }
    cfgfnp++;
  }
  sprintf(m->display.szCaption, "%s", m->szCapFormat);
}

static Menu* loadMenu(FILE* f, char* buf, Menu* parent, int level) {
  Menu* m;
  int i;


  if(level > 10) {
    printf("recursing level > 10 - aborting\n");
    exit(1);
  }

  m = (Menu*) malloc(sizeof(Menu));
  m->parent = parent;
  getLine(buf, MENU_BUFSIZE, f);
  sscanf(buf, "%d ", &(m->nEntries));

  getLine(buf, MENU_BUFSIZE, f);
  buf[31] = 0; /* enforce menu name limit; */
  sprintf(m->szName, "%s", buf);
  if(*(m->szName + strlen(m->szName) - 1) == '\n')
    *(m->szName + strlen(m->szName) - 1) = 0;
  

  getLine(buf, MENU_BUFSIZE, f);
  buf[31] = 0; /* enforce menu caption limit; */
  sprintf(m->szCapFormat, "%s", buf);
  /* remove newline */
  for(i = 0; *(m->szCapFormat + i) != 0; i++)
    if (*(m->szCapFormat + i) == '\n') {
      *(m->szCapFormat + i) = 0;
      break;
    }

  initMenuCaption(m);
	
  /* printf("menu '%s': %d entries\n", m->szName, m->nEntries); */
  if(m->nEntries > 0) {
    m->pEntries = malloc(sizeof(Menu*) * m->nEntries);
    for(i = 0; i < m->nEntries; i++) {
      /* printf("loading menu number %d\n", i); */
      if(i > 10) {
	printf("item limit reached - aborting\n");
	exit(1);
      }
      *(m->pEntries + i) = loadMenu(f, buf, m, level + 1);
    }
  }

  return m;
}

static Menu** loadMenuFile(char *filename) {
  char buf[MENU_BUFSIZE];
  FILE* f;
  Menu* m;
  Menu** list = NULL;
  int nMenus;
  int i, j;
  node *head;
  node *t;
  node *z;
  int sp = 0;

  if((f = fopen(filename, "r")) == NULL)
    return 0;
  /* read count of Menus */
  getLine(buf, MENU_BUFSIZE, f);
  sscanf(buf, "%d ", &nMenus);
  if(nMenus <= 0) return 0;

  /* allocate space for data structures */
  list = (Menu**) malloc(sizeof(Menu*) * nMenus);

  /* load data */
  for(i = 0; i < nMenus; i++) {
    /* printf("loading menu set %d\n", i); */
    if(i > 10) exit(1);
    *(list + i) = loadMenu(f, buf, NULL, 0);
  }

  /* TODO(3): now since I eliminated the need for cx/cy, why */
  /* do I need to traverse the Menu Tree? Just to set the colors??? */

  /* traverse Menu Tree and set Menu Color to some boring default */
  /* printf("finished parsing file - now traversing menus\n"); */
  /* setup stack */
  z = (node*) malloc(sizeof(node));
  z->next = z;
  head = (node*) malloc(sizeof(node));
  head->next = z;
  
  for(i = 0; i < nMenus; i++) {
    t = (node*) malloc(sizeof(node));
    t->data = *(list + i);
    t->next = head->next;
    head->next = t;
    sp++;
    while(head->next != z) {
      t = head->next;
      head->next = t->next;
      m = (Menu*) t->data;
      free(t);
      /* printf("stack count: %d\n", --sp); */
      /* printf("visiting %s\n", m->szName); */
      /* visit m */

      /* TODO(0): put the color defaults somewhere else */

      m->display.fgColor[0] = 0.0;
      m->display.fgColor[1] = 0.0;
      m->display.fgColor[2] = 0.0;
      m->display.fgColor[3] = 1.0;
      m->display.hlColor[0] = 255.0 / 255.0;
      m->display.hlColor[1] = 20.0 / 255.0;
      m->display.hlColor[2] = 20.0 / 255.0;
      m->display.hlColor[3] = 1.0;

      /* push all of m's submenus */
      for(j = 0; j < m->nEntries; j++) {
	t = (node*) malloc(sizeof(node));
	t->data = *(m->pEntries + j);
	t->next = head->next;
	head->next = t;
	/* printf("pushing %s\n", ((Menu*)t->data)->szName); */
	/* printf("stack count: %d\n", ++sp); */
	
      }
    }
  }
  return list;
}

static void drawMenu(gDisplay *d) {
  /* draw Menu pCurrent */
  int i;
  int x, y, size, lineheight;

  rasonly(d);

  x = d->vp_w / 6;
  size = d->vp_w / 32;
  y = 2 * d->vp_h / 3;
  lineheight = size * 2;

  /* draw the entries */
  for(i = 0; i < pCurrent->nEntries; i++) {
    if(i == pCurrent->iHighlight)
      glColor4fv(pCurrent->display.hlColor);
    else glColor4fv(pCurrent->display.fgColor);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    rasonly(d);
    drawText(x, y, size,
	     ((Menu*)*(pCurrent->pEntries + i))->display.szCaption);
    y -= lineheight;
  }
}

static sgi_texture *tex;

static background_states bgs;

static void guiProjection(int x, int y) {
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  /*glOrtho(0, 0, x, y, -1, 1); */
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glViewport(0, 0, x, y);
}

#define GUI_BLUE 0.3
static void displayGui(void) {
  float x, y, w, h;
  float y1, y2;
  float a, b1, b2, c1, c2;
  float alpha;

#define N 20.0
  glClearColor(0.0, 0.0, 1.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  guiProjection(screen->vp_w, screen->vp_h);

  glBegin(GL_QUADS);
  c1 = 0.25; c2 = 0.25;
  glColor3f(c1, c1, GUI_BLUE);
  glVertex2f(-1, -1);
  glColor3f(c2, c2, GUI_BLUE);
  glVertex2f(1, -1);
  glVertex2f(1, 1);
  glColor3f(c1, c1, GUI_BLUE);
  glVertex2f(-1, 1);
  glEnd();

  x = bgs.posx;
  y = bgs.posy;
  w = 0.40;
  h = 0.50;

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, screen->texGui);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

  alpha = (sin(bgs.d - M_PI / 2) + 1) / 2;
  glColor4f(1.0, 1.0, 0.0, alpha);
  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(x, y);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(x + w, y);
  glTexCoord2f(1.0, 1.0);
  glVertex2f(x + w, y + h);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(x, y + h);
  glEnd();
  glDisable(GL_TEXTURE_2D);

  glColor3f(1.0, 0.0, 1.0);
  drawMenu(screen);

  glutSwapBuffers();
}

static void idleGui(void) {
  float delta;
  long now;

#ifdef SOUND
  soundIdle();
#endif

  now = getElapsedTime();
  delta = now - bgs.lt;
  bgs.lt = now;
  delta /= 1000.0;
  bgs.d += delta;
  /* printf("%.5f\n", delta); */
  
  if(bgs.d > 2 * M_PI) { 
    bgs.d -= 2 * M_PI;
    bgs.posx = 1.0 * (float)rand() / (float)RAND_MAX - 1;
    bgs.posy = 1.5 * (float)rand() / (float)RAND_MAX - 1;
  }

  glutPostRedisplay();
}

static void menuAction(Menu *activated)
{
  if(activated->nEntries > 0) {
    pCurrent = activated;
    pCurrent->iHighlight = 0;
  } else {
    cfgfnp = cfgfn;
    while(cfgfnp->doChange != changeEnd) {
      if(strstr(cfgfnp->name, activated->szName)) {
        cfgfnp->doChange(cfgfnp, activated);
	cfgfnp->doCaption(cfgfnp, activated);
	return;
      }
      cfgfnp++;
    }
  }
  switch(activated->szName[1]) {
  case 'q':
    exit(0);
  case 'c':
    switchCallbacks(&conCallbacks);
  }
}

void loadSettings(char *file)
{
  struct strbuf *entry;
  char *path, c;
  FILE *f;

  path = getFullPath(file);
  entry = strbuf_alloc();
  f = fopen(path, "r");
  do {
    fread(&c, (size_t) 1, (size_t) 1, f);
    if(ferror(f)) {
      fprintf(stderr, "error loading config");
      exit(1);
    }
    strbuf_append1(entry, c);
  } while(!feof(f));
  cfgp->file = path;
  cfgp->entry = entry;
  cfgp->next = malloc(sizeof cfg);
  cfgp->next->prev = cfgp;
  cfgp = cfgp->next;
  cfgp->entry = NULL;
  cfgp->next = &cfg;
  cfg.prev = cfgp;
}

void setupSound(int *argc, char **argv)
{
  alutInit(argc, argv);
  loadSound(getFullPath("soldat.wav"));
  setAttribute(AL_LOOPING);
  playSound();
}

void parseSettings(void)
{
  char *entry;

  cfgfnp = cfgfn;
  cfgp = &cfg;
  while(cfgfnp->doCaption != doEnd) {
    while(cfgp->entry != NULL) {
      if((entry = strstr(cfgp->entry->s, cfgfnp->name)) != NULL) {
        entrytok(entry, cfgfnp);
	cfgp = &cfg;
	break;
      }
      cfgp = cfgp->next;
    }
    if(cfgp->entry == NULL) {
      fprintf(stderr, "error loading entry %s", cfgfnp->name);
      exit(1);
    }
    cfgfnp++;
  }
}

static void keyboardGui(unsigned char key, int x, int y) {
  int i;
  switch(key) {
  case 27:
    if(pCurrent->parent != NULL)
      pCurrent = pCurrent->parent;
    break;
  case 13: case ' ':
    menuAction(*(pCurrent->pEntries + pCurrent->iHighlight));
    break;
  case 'q': exit(0); break;
  case 'l':
    printf("%d entries:\n", pCurrent->nEntries);
    for(i = 0; i < pCurrent->nEntries; i++)
      printf("printing '%s' - %d entries\n",
	     ((Menu*)*(pCurrent->pEntries + i))->szName,
	     ((Menu*)*(pCurrent->pEntries + i))->nEntries);
    break;
  default: printf("got key %d\n", key);
  }
}

static void  specialGui(int key, int x, int y) {
  switch(key) {
  case GLUT_KEY_DOWN:
    pCurrent->iHighlight = (pCurrent->iHighlight + 1) % pCurrent->nEntries;
    break;
  case GLUT_KEY_UP:
    pCurrent->iHighlight = (pCurrent->iHighlight - 1) % pCurrent->nEntries;
    if(pCurrent->iHighlight < 0)
      pCurrent->iHighlight = pCurrent->nEntries - 1;
    break;
  }
}

static void initGui(void) {
  /* init states */
  bgs.d = 0;
  bgs.posx = -1;
  bgs.posy = -1;
  bgs.lt = getElapsedTime();

  pMenuList = loadMenuFile(getFullPath("menu.txt"));
  pCurrent = *pMenuList; /* erstes Menu ist RootMenu - Default pCurrent */
  pCurrent->iHighlight = 0;

  /* rasonly(screen); */
  srand(time(NULL));
}

static void initGLGui(void) {
  glShadeModel(GL_SMOOTH);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);

}

void keyboardName(unsigned char key, int x, int y)
{
  static struct strbuf buf;

  if(key == 13) {
    free(cfgfnp->val);
    cfgfnp->val = strbuf_finish(&buf);
    memset(&buf, 0, sizeof buf);
    printf("%s\n", cfgfnp->val);
    switchCallbacks(&backCallbacks);
    return;
  }
  if(key == '"')
    strbuf_append1(&buf, key);
  strbuf_append1(&buf, key);
}

void keyboardCon(unsigned char key, int x, int y)
{
  static struct strbuf buf;

  if(key == 13) {
    static char *game[4] = { "./opensoldat", "-join", };

    game[2] = strbuf_finish(&buf);
    game[3] = NULL;
    execvp(game[0], game);
    fprintf(stderr, "game not found");
    exit(1);
  }
  strbuf_append1(&buf, key);
}

void specialNull(int key, int x, int y)
{
  ;
}

void initNull(void)
{
  ;
}

void initGLNull(void)
{
  ;
}

callbacks guiCallbacks = {
  displayGui, idleGui, keyboardGui, specialGui, initGui, initGLGui
};

callbacks nameCallbacks = {
  displayGui, idleGui, keyboardName, specialNull, initNull, initGLNull
};

callbacks backCallbacks = {
  displayGui, idleGui, keyboardGui, specialGui, initNull, initGLNull
};

callbacks conCallbacks = {
  displayGui, idleGui, keyboardCon, specialNull, initNull, initGLNull
};
