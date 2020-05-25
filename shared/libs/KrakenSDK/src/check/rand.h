/**
 * @file OS RNG abstraction
 * @author Oliver Kuckertz <oliver.kuckertz@softwific.com>
 */

#pragma once

#include <stdlib.h>

void randBytes(void* dst, size_t size);
