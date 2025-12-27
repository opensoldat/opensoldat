#include <AL/alut.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

#include "main.h"

void changeSoundVolume(struct configfn *cfg, Menu *m)
{
  int val;
  char new[8];
  struct strbuf buf = strbuf_init;

  sscanf(cfg->val, "%d", &val);
  switch(val) {
  case 0:
  case 25:
  case 50:
  case 75:
    val += 25;
    break;
  case 100:
    val = 0;
  }
  free(cfg->val);
  sprintf(new, "%d", val);
  strbuf_append(&buf, new);
  cfg->val = strbuf_finish(&buf);
}

void changeSoundBattle(struct configfn *cfg, Menu *m)
{
  int val;
  char new[8];
  struct strbuf buf = strbuf_init;

  sscanf(cfg->val, "%d", &val);
  val = !val;
  free(cfg->val);
  sprintf(new, "%d", val);
  strbuf_append(&buf, new);
  cfg->val = strbuf_finish(&buf);
}

void changeSoundExplosions(struct configfn *cfg, Menu *m)
{
  int val;
  char new[8];
  struct strbuf buf = strbuf_init;

  sscanf(cfg->val, "%d", &val);
  val = !val;
  free(cfg->val);
  sprintf(new, "%d", val);
  strbuf_append(&buf, new);
  cfg->val = strbuf_finish(&buf);
}

void changeShirt(struct configfn *cfg, Menu *m)
{
}

void changeJet(struct configfn *cfg, Menu *m)
{
}

void changeHair(struct configfn *cfg, Menu *m)
{
}

void changeSkin(struct configfn *cfg, Menu *m)
{
}

void changeEnd(struct configfn *cfg, Menu *m)
{
  ;
}

void changeHairstyle(struct configfn *cfg, Menu *m)
{
  char new[8];
  struct strbuf buf = strbuf_init;

  free(cfg->val);
  sprintf(new, "%d", m->parent->iHighlight);
  strbuf_append(&buf, new);
  cfg->val = strbuf_finish(&buf);
  guiCallbacks.keyboard(27, 0, 0);
}

void changeSecWeapon(struct configfn *cfg, Menu *m)
{
  char new[8];
  struct strbuf buf = strbuf_init;

  free(cfg->val);
  sprintf(new, "%d", m->parent->iHighlight);
  strbuf_append(&buf, new);
  cfg->val = strbuf_finish(&buf);
  guiCallbacks.keyboard(27, 0, 0);
}

void changeHeadstyle(struct configfn *cfg, Menu *m)
{
  char new[8];
  struct strbuf buf = strbuf_init;

  free(cfg->val);
  sprintf(new, "%d", m->parent->iHighlight);
  strbuf_append(&buf, new);
  cfg->val = strbuf_finish(&buf);
  guiCallbacks.keyboard(27, 0, 0);
}

void changeChainstyle(struct configfn *cfg, Menu *m)
{
  char new[8];
  struct strbuf buf = strbuf_init;

  free(cfg->val);
  sprintf(new, "%d", m->parent->iHighlight);
  strbuf_append(&buf, new);
  cfg->val = strbuf_finish(&buf);
  guiCallbacks.keyboard(27, 0, 0);
}

void changeName(struct configfn *cfg, Menu *m)
{
  switchCallbacks(&nameCallbacks);
}

void changePants(struct configfn *cfg, Menu *m)
{
}

void changeFullscreen(struct configfn *cfg, Menu *m)
{
}

void changeRenderWidth(struct configfn *cfg, Menu *m)
{
}

void changeRenderHeight(struct configfn *cfg, Menu *m)
{
}

void changeScreenWidth(struct configfn *cfg, Menu *m)
{
}

void changeScreenHeight(struct configfn *cfg, Menu *m)
{
}

void changeFPSLimit(struct configfn *cfg, Menu *m)
{
}

void changeMaxFPS(struct configfn *cfg, Menu *m)
{
}

void changeRenderBackground(struct configfn *cfg, Menu *m)
{
}

void changeForceBackground(struct configfn *cfg, Menu *m)
{
}

void changeBackgroundColorOne(struct configfn *cfg, Menu *m)
{
}

void changeBackgroundColorTwo(struct configfn *cfg, Menu *m)
{
}

void changeWeatherEffects(struct configfn *cfg, Menu *m)
{
}

void changeSmoothEdges(struct configfn *cfg, Menu *m)
{
}

void changeScaleInterface(struct configfn *cfg, Menu *m)
{
}

void changePlayerIndicator(struct configfn *cfg, Menu *m)
{
}

void changeKillConsole(struct configfn *cfg, Menu *m)
{
}

void changeSwapEffect(struct configfn *cfg, Menu *m)
{
}

void changeDithering(struct configfn *cfg, Menu *m)
{
}


void doSoundVolume(struct configfn *cfg, Menu *m)
{
  sprintf(m->display.szCaption, m->szCapFormat, cfg->val);
}

void doSoundBattle(struct configfn *cfg, Menu *m)
{
  int val;

  sscanf(cfg->val, "%d", &val);
  sprintf(m->display.szCaption, m->szCapFormat, val ? "on" : "off");
}

void doSoundExplosions(struct configfn *cfg, Menu *m)
{
  int val;

  sscanf(cfg->val, "%d", &val);
  sprintf(m->display.szCaption, m->szCapFormat, val ? "on" : "off");
}

void doShirt(struct configfn *cfg, Menu *m)
{
  sprintf(m->display.szCaption, m->szCapFormat, cfg->val);
}

void doJet(struct configfn *cfg, Menu *m)
{
  sprintf(m->display.szCaption, m->szCapFormat, cfg->val);
}

void doHair(struct configfn *cfg, Menu *m)
{
  sprintf(m->display.szCaption, m->szCapFormat, cfg->val);
}

void doSkin(struct configfn *cfg, Menu *m)
{
  sprintf(m->display.szCaption, m->szCapFormat, cfg->val);
}

void doEnd(struct configfn *cfg, Menu *m)
{
  ;
}

void doHairstyle(struct configfn *cfg, Menu *m)
{
  sprintf(m->display.szCaption, m->szCapFormat, cfg->val);
}

void doSecWeapon(struct configfn *cfg, Menu *m)
{
  sprintf(m->display.szCaption, m->szCapFormat, cfg->val);
}

void doHeadstyle(struct configfn *cfg, Menu *m)
{
  sprintf(m->display.szCaption, m->szCapFormat, cfg->val);
}

void doChainstyle(struct configfn *cfg, Menu *m)
{
  sprintf(m->display.szCaption, m->szCapFormat, cfg->val);
}

void doName(struct configfn *cfg, Menu *m)
{
  sprintf(m->display.szCaption, m->szCapFormat, cfg->val);
}

void doPants(struct configfn *cfg, Menu *m)
{
}

void doFullscreen(struct configfn *cfg, Menu *m)
{
  int val;

  sscanf(cfg->val, "%d", &val);
  sprintf(m->display.szCaption, m->szCapFormat, val ? "on" : "off");
}

void doRenderWidth(struct configfn *cfg, Menu *m)
{
}

void doRenderHeight(struct configfn *cfg, Menu *m)
{
}

void doScreenWidth(struct configfn *cfg, Menu *m)
{
  int val;

  sscanf(cfg->val, "%d", &val);
  sprintf(m->display.szCaption, "%s", m->szCapFormat);
}

void doScreenHeight(struct configfn *cfg, Menu *m)
{
}

void doFPSLimit(struct configfn *cfg, Menu *m)
{
}

void doMaxFPS(struct configfn *cfg, Menu *m)
{
}

void doRenderBackground(struct configfn *cfg, Menu *m)
{
}

void doForceBackground(struct configfn *cfg, Menu *m)
{
}

void doBackgroundColorOne(struct configfn *cfg, Menu *m)
{
}

void doBackgroundColorTwo(struct configfn *cfg, Menu *m)
{
}

void doWeatherEffects(struct configfn *cfg, Menu *m)
{
  int val;

  sscanf(cfg->val, "%d", &val);
  sprintf(m->display.szCaption, m->szCapFormat, val ? "on" : "off");
}

void doSmoothEdges(struct configfn *cfg, Menu *m)
{
}

void doScaleInterface(struct configfn *cfg, Menu *m)
{
}

void doPlayerIndicator(struct configfn *cfg, Menu *m)
{
}

void doKillConsole(struct configfn *cfg, Menu *m)
{
}

void doSwapEffect(struct configfn *cfg, Menu *m)
{
}

void doDithering(struct configfn *cfg, Menu *m)
{
}

