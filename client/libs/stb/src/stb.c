#define _CRT_SECURE_NO_WARNINGS

#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#define STB_IMAGE_RESIZE_IMPLEMENTATION
#define STBI_ONLY_PNG
#define STBI_ONLY_JPEG
#define STBI_ONLY_BMP
#define STBI_ONLY_GIF

#include "stb_image.h"
#include "stb_image_write.h"
#include "stb_image_resize.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct gif_result_t {
	int delay;
	unsigned char *data;
	struct gif_result_t *next;
} gif_result;

STBIDEF unsigned char *stbi_xload(stbi__context *s, int *x, int *y, int *frames);
STBIDEF unsigned char *stbi_xload_mem(unsigned char *buffer, int len, int *x, int *y, int *frames);
STBIDEF unsigned char *stbi_xload_file(char const *filename, int *x, int *y, int *frames);

STBIDEF unsigned char *stbi_xload_mem(unsigned char *buffer, int len, int *x, int *y, int *frames)
{
	stbi__context s;
	stbi__start_mem(&s, buffer, len);
	return stbi_xload(&s, x, y, frames);
}

STBIDEF unsigned char *stbi_xload_file(char const *filename, int *x, int *y, int *frames)
{
	FILE *f;
	stbi__context s;
	unsigned char *result = 0;

	if (!(f = stbi__fopen(filename, "rb")))
		return stbi__errpuc("can't fopen", "Unable to open file");

	stbi__start_file(&s, f);
	result = stbi_xload(&s, x, y, frames);
	fclose(f);

	return result;
}

STBIDEF unsigned char *stbi_xload(stbi__context *s, int *x, int *y, int *frames)
{
	unsigned char *result = 0;

	if (stbi__gif_test(s))
	{
		int c;
		stbi__gif g;
		gif_result head;
		gif_result *prev = 0, *gr = &head;

		memset(&g, 0, sizeof(g));
		memset(&head, 0, sizeof(head));

		*frames = 0;

		while (gr->data = stbi__gif_load_next(s, &g, &c, 4, 0))
		{
			if (gr->data == (unsigned char*)s)
			{
				gr->data = 0;
				break;
			}

			if (prev) prev->next = gr;
			gr->delay = g.delay;
			prev = gr;
			gr = (gif_result*) stbi__malloc(sizeof(gif_result));
			memset(gr, 0, sizeof(gif_result));
			++(*frames);
		}

		STBI_FREE(g.out);

		if (gr != &head)
			STBI_FREE(gr);

		if (*frames > 0)
		{
			*x = g.w;
			*y = g.h;
		}

		result = head.data;

		if (*frames > 1)
		{
			unsigned int size = 4 * g.w * g.h;
			unsigned char *p = 0;

			result = (unsigned char*)stbi__malloc(*frames * (size + 2));
			gr = &head;
			p = result;

			while (gr)
			{
				prev = gr;
				memcpy(p, gr->data, size);
				p += size;
				*p++ = gr->delay & 0xFF;
				*p++ = (gr->delay & 0xFF00) >> 8;
				gr = gr->next;

				STBI_FREE(prev->data);
				if (prev != &head) STBI_FREE(prev);
			}
		}
	}
	else
	{
		stbi__result_info ri;
		result = stbi__load_main(s, x, y, frames, 4, &ri, 8);
		*frames = !!result;

		if (ri.bits_per_channel != 8) {
			STBI_ASSERT(ri.bits_per_channel == 16);
			result = stbi__convert_16_to_8((stbi__uint16 *)result, *x, *y, 4);
			ri.bits_per_channel = 8;
		}
	}

	return result;
}

#ifdef __cplusplus
}
#endif
