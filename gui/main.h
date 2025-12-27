#define strbuf_init { NULL, 0, 0 }

typedef struct callbacks {
  void (*display)(void);
  void (*idle)(void);
  void (*keyboard)(unsigned char, int, int);
  void (*special)(int, int, int);
  void (*init)(void);
  void (*initGL)(void);
} callbacks;

typedef struct {
  /* fonttex *font; */
  float fgColor[4]; /* entries */
  float hlColor[4]; /* the highlighted one */

  char szCaption[32];
} mDisplay;

typedef struct Menu {
  int nEntries;
  int iHighlight;
  mDisplay display;
  char szName[32];
  char szCapFormat[32];
  struct Menu** pEntries;
  struct Menu* parent;
  void* param; /* reserved to bind parameters at runtime */
} Menu;

typedef struct {
  void* data;
  void* next;
} node;

typedef struct {
  int width;
  int height;
  int channels;
  unsigned char *data;
} sgi_texture;

typedef struct fonttex {
  sgi_texture **textures;
  int nTextures;
  
  int texwidth; /* texture width */
  int width; /* character width */

  int lower; /* lowest ascii character (normally: 32) */
  int upper; /* highest ascii character (normally: 126) */

  unsigned int *texID;

  char *fontname;
} fonttex;

typedef struct gDisplay {
  int win_id;     /* nur das globale Window hat eine */
  int h, w;       /* window */
  int vp_x, vp_y; /* viewport */
  int vp_h, vp_w;
  int blending;
  int fog;
  int wall;
  int onScreen;

  unsigned int texFloor;
  unsigned int texWall;
  unsigned int texGui;
  unsigned int texCrash;
} gDisplay;

typedef struct {
float d;
float posx;
float posy;
long lt; 
} background_states;

typedef struct settings_int {
  char name[32];
  int *value;
} settings_int;

typedef struct settings_float {
  char name[32];
  float *value;
} settings_float;

struct strbuf {
    char *s;
    size_t len;                 /* Valid characters */
    size_t size;                /* Allocated characters */
};

struct config {
    char *file;
    struct strbuf *entry;
    struct config *next,
		  *prev;
};

struct configfn {
    void (*doCaption)(struct configfn *, Menu *);
    void (*doChange)(struct configfn *, Menu *);
    char *name,
	 *val;
};

extern callbacks guiCallbacks;
extern callbacks nameCallbacks;
extern callbacks backCallbacks;
extern callbacks conCallbacks;

extern void loadSettings(char *);
extern void switchCallbacks(callbacks *new);
extern void setupDisplay(gDisplay *d);
extern struct strbuf	 *strbuf_alloc(void);
extern void strbuf_terminate(struct strbuf *);
extern void strbuf_append1(struct strbuf *, char);
extern void strbuf_appendn(struct strbuf *, const char *, size_t);
extern void strbuf_append (struct strbuf *, const char *);
extern char *strbuf_finish (struct strbuf *);
extern void strbuf_cleanup(void *);
extern void strbuf_free(void *);
extern void doHair(struct configfn *, Menu *);
extern void doSkin(struct configfn *, Menu *);
extern void doEnd(struct configfn *, Menu *);
extern void doHairstyle(struct configfn *, Menu *);
extern void doSecWeapon(struct configfn *, Menu *);
extern void doHeadstyle(struct configfn *, Menu *);
extern void doChainstyle(struct configfn *, Menu *);
extern void doJet(struct configfn *, Menu *);
extern void doPants(struct configfn *, Menu *);
extern void doShirt(struct configfn *, Menu *);
extern void doName(struct configfn *, Menu *);
extern char *quotetok(char *);
extern void entrytok(char *, struct configfn *);
extern void parseSettings(void);
extern void doFullscreen(struct configfn *, Menu *);
extern void doRenderWidth(struct configfn *, Menu *);
extern void doRenderHeight(struct configfn *, Menu *);
extern void doScreenWidth(struct configfn *, Menu *);
extern void doScreenHeight(struct configfn *, Menu *);
extern void doFPSLimit(struct configfn *, Menu *);
extern void doMaxFPS(struct configfn *, Menu *);
extern void doRenderBackground(struct configfn *, Menu *);
extern void doForceBackground(struct configfn *, Menu *);
extern void doBackgroundColorOne(struct configfn *, Menu *);
extern void doBackgroundColorTwo(struct configfn *, Menu *);
extern void doWeatherEffects(struct configfn *, Menu *);
extern void doSmoothEdges(struct configfn *, Menu *);
extern void doScaleInterface(struct configfn *, Menu *);
extern void doPlayerIndicator(struct configfn *, Menu *);
extern void doKillConsole(struct configfn *, Menu *);
extern void doSwapEffect(struct configfn *, Menu *);
extern void doDithering(struct configfn *, Menu *);
extern void doSoundVolume(struct configfn *, Menu *);
extern void doSoundBattle(struct configfn *, Menu *);
extern void doSoundExplosions(struct configfn *, Menu *);
extern void loadSound(char *);
extern void setAttribute(ALenum attr);
extern void playSound(void);
extern void setupSound(int *, char **);
extern void changeSoundVolume(struct configfn *, Menu *);
extern void changeSoundBattle(struct configfn *, Menu *);
extern void changeSoundExplosions(struct configfn *, Menu *);
extern void changeHair(struct configfn *, Menu *);
extern void changeSkin(struct configfn *, Menu *);
extern void changeEnd(struct configfn *, Menu *);
extern void changeHairstyle(struct configfn *, Menu *);
extern void changeSecWeapon(struct configfn *, Menu *);
extern void changeHeadstyle(struct configfn *, Menu *);
extern void changeChainstyle(struct configfn *, Menu *);
extern void changeJet(struct configfn *, Menu *);
extern void changePants(struct configfn *, Menu *);
extern void changeShirt(struct configfn *, Menu *);
extern void changeName(struct configfn *, Menu *);
extern void changeFullscreen(struct configfn *, Menu *);
extern void changeRenderWidth(struct configfn *, Menu *);
extern void changeRenderHeight(struct configfn *, Menu *);
extern void changeScreenWidth(struct configfn *, Menu *);
extern void changeScreenHeight(struct configfn *, Menu *);
extern void changeFPSLimit(struct configfn *, Menu *);
extern void changeMaxFPS(struct configfn *, Menu *);
extern void changeRenderBackground(struct configfn *, Menu *);
extern void changeForceBackground(struct configfn *, Menu *);
extern void changeBackgroundColorOne(struct configfn *, Menu *);
extern void changeBackgroundColorTwo(struct configfn *, Menu *);
extern void changeWeatherEffects(struct configfn *, Menu *);
extern void changeSmoothEdges(struct configfn *, Menu *);
extern void changeScaleInterface(struct configfn *, Menu *);
extern void changePlayerIndicator(struct configfn *, Menu *);
extern void changeKillConsole(struct configfn *, Menu *);
extern void changeSwapEffect(struct configfn *, Menu *);
extern void changeDithering(struct configfn *, Menu *);
