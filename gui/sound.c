#include <AL/alut.h>
#include <stddef.h>

#include "main.h"

static ALuint src, buf;

void loadSound(char *file)
{
  buf = alutCreateBufferFromFile(file);
  alGenSources(1, &src);
  alSourcei(src, AL_BUFFER, buf);
}

void setAttribute(ALenum attr)
{
  alSourcei(src, attr, buf);
}

void playSound(void)
{
  alSourcePlay(src);
}
