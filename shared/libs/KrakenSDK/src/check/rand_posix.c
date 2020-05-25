/**
 * @file OS RNG abstraction implementation for POSIX-compatible systems
 * @author Oliver Kuckertz <oliver.kuckertz@softwific.com>
 */

#include "rand.h"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

void randBytes(void* dst, size_t size) {
    int fd = open("/dev/urandom", O_RDONLY);
    ssize_t nread = read(fd, dst, size);
    if (nread < 0) {
        abort();
    }
    close(fd);
}
