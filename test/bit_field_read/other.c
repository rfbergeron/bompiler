#include "defs.h"

struct foo f = {
  2, 216, 9, -4, 65535, 0, 69
};

struct bar b = {
  69, 63, 1024, 55555
};

struct baz z = {
  1, 127, -4, 31
};

struct bing i = {
  1, -2, -1, 0
};

// struct bav v = {
//   1023
// };

struct bav v = {
  16383, 28, -1
};

struct bong o = {
  1, 5, 31, 1 << 21, 1 << 21
};
