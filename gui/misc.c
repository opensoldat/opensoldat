#include <AL/alut.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "main.h"

static char strnull[] = "";

void entrytok(char *entry, struct configfn *cfgfnp)
{
  entry += strlen(cfgfnp->name);
  cfgfnp->val = quotetok(entry);
  if(cfgfnp->val == strnull) {
    fprintf(stderr, "%s is missing a value", cfgfnp->name);
    exit(1);
  }
}

char *quotetok(char *buf)
{
  struct strbuf ret = strbuf_init;

  while(*buf != '\0')
    if(*buf++ == '"') {
      while(*buf != '\0') {
        if(buf[0] == '"') {
          if(buf[1] == '"') {
            strbuf_appendn(&ret, buf, (size_t) 2);
	    buf += 2;
	    continue;
	  }
	  break;
	}
	strbuf_append1(&ret, *buf++);
      }
      break;
    }
  if(ret.len == 0)
    return strnull;
  return strbuf_finish(&ret);
}

#define DO_STRBUF(STRBUF, CHAR, STRLEN)				\
								\
struct STRBUF *							\
STRBUF##_alloc(void)						\
{								\
    return calloc(1, sizeof(struct STRBUF));			\
}								\
								\
static void							\
STRBUF##_store1(struct STRBUF *buf, CHAR c)			\
{								\
    if (buf->size == buf->len) {				\
	if (buf->size == 0)					\
	    buf->size = 64; /* Arbitrary */			\
	else							\
	    buf->size *= 2;					\
	buf->s = realloc(buf->s, buf->size * sizeof(*buf->s));	\
    }								\
    assert(buf->s);						\
    buf->s[buf->len] = c;					\
}								\
								\
/* Like strbuf_append1(buf, '\0'), but don't advance len */	\
void								\
STRBUF##_terminate(struct STRBUF *buf)				\
{								\
    STRBUF##_store1(buf, '\0');					\
}								\
								\
void								\
STRBUF##_append1(struct STRBUF *buf, CHAR c)			\
{								\
    STRBUF##_store1(buf, c);					\
    buf->len++;							\
}								\
								\
void								\
STRBUF##_appendn(struct STRBUF *buf, const CHAR *s, size_t len)	\
{								\
    if (buf->size < buf->len + len) {				\
	if (buf->size == 0)					\
	    buf->size = 64; /* Arbitrary */			\
	while (buf->size < buf->len + len)			\
	    buf->size *= 2;					\
	buf->s = realloc(buf->s, buf->size * sizeof(*buf->s));	\
    }								\
    memcpy(buf->s + buf->len, s, len * sizeof(*buf->s));	\
    buf->len += len;						\
}								\
								\
void								\
STRBUF##_append(struct STRBUF *buf, const CHAR *s)		\
{								\
    STRBUF##_appendn(buf, s, STRLEN(s));			\
}								\
								\
CHAR *								\
STRBUF##_finish(struct STRBUF *buf)				\
{								\
    STRBUF##_append1(buf, 0);					\
    return realloc(buf->s, buf->len * sizeof(*buf->s));		\
}								\
								\
void								\
STRBUF##_cleanup(void *xbuf)					\
{								\
    struct STRBUF *buf;						\
								\
    buf = xbuf;							\
    free(buf->s);						\
}								\
								\
void								\
STRBUF##_free(void *xbuf)					\
{								\
    STRBUF##_cleanup(xbuf);					\
    free(xbuf);							\
}

DO_STRBUF(strbuf, char, strlen);
