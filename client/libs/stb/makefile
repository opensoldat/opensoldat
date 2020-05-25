out := libstb.so

all: | $(out) copy

$(out): src/stb.c src/stb_image.h src/stb_image_write.h
	$(CC) -shared -O3 -fPIC -fno-strict-aliasing -o $(out) src/stb.c

copy:
	cp $(out) ../../build/

clean:
	rm -f $(out)

.PHONY: copy clean
