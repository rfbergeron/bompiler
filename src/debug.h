#ifndef __DEBUG_H__
#define __DEBUG_H__

void debug_set_flags(const char *optflags);
int debug_get_flag(char flag);
void debug_where(char flag, const char *file, int line, const char *msg, ...);

#ifdef NDEBUG
#define PFDBG0(FLAG, PRINTF_STR) ;
#define PFDBG1(FLAG, PRINTF_FMT, PRINTF_ARG1) ;
#define PFDBG2(FLAG, PRINTF_FMT, PRINTF_ARG1, PRINTF_ARG2) ;
#define PFDBG3(FLAG, PRINTF_FMT, PRINTF_ARG1, PRINTF_ARG2, PRINTF_ARG3) ;
#define PFDBG4(FLAG, PRINTF_FMT, PRINTF_ARG1, PRINTF_ARG2, PRINTF_ARG3, \
               PRINTF_ARG4)                                             \
  ;
#else
#define PFDBG0(FLAG, PRINTF_STR)                         \
  do {                                                   \
    if (debug_get_flag(FLAG)) {                          \
      debug_where(FLAG, __FILE__, __LINE__, PRINTF_STR); \
    }                                                    \
  } while (0)
#define PFDBG1(FLAG, PRINTF_FMT, PRINTF_ARG1)                         \
  do {                                                                \
    if (debug_get_flag(FLAG)) {                                       \
      debug_where(FLAG, __FILE__, __LINE__, PRINTF_FMT, PRINTF_ARG1); \
    }                                                                 \
  } while (0)
#define PFDBG2(FLAG, PRINTF_FMT, PRINTF_ARG1, PRINTF_ARG2)           \
  do {                                                               \
    if (debug_get_flag(FLAG)) {                                      \
      debug_where(FLAG, __FILE__, __LINE__, PRINTF_FMT, PRINTF_ARG1, \
                  PRINTF_ARG2);                                      \
    }                                                                \
  } while (0)
#define PFDBG3(FLAG, PRINTF_FMT, PRINTF_ARG1, PRINTF_ARG2, PRINTF_ARG3) \
  do {                                                                  \
    if (debug_get_flag(FLAG)) {                                         \
      debug_where(FLAG, __FILE__, __LINE__, PRINTF_FMT, PRINTF_ARG1,    \
                  PRINTF_ARG2, PRINTF_ARG3);                            \
    }                                                                   \
  } while (0)
#define PFDBG4(FLAG, PRINTF_FMT, PRINTF_ARG1, PRINTF_ARG2, PRINTF_ARG3, \
               PRINTF_ARG4)                                             \
  do {                                                                  \
    if (debug_get_flag(FLAG)) {                                         \
      debug_where(FLAG, __FILE__, __LINE__, PRINTF_FMT, PRINTF_ARG1,    \
                  PRINTF_ARG2, PRINTF_ARG3, PRINTF_ARG4);               \
    }                                                                   \
  } while (0)
#endif

#endif
