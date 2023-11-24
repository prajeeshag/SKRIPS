#if 0

Do not have C++ or F90 style comments in here because this file is processed
by both C++ and F90 compilers.

These lines prevent this file from being read more than once if it
ends up being included multiple times.
#endif

#ifndef CESMF_MACROS_INC
#define CESMF_MACROS_INC

#if 0

former file contents moved to CESMF_BaseMod
so user code can be compiled without requiring
the preprocessor.

#endif 

#if 0
i left the following macro here in case it is needed for our internal use.
#endif

#define CESMF_SRCLINE __FILE__, __LINE__

#endif 