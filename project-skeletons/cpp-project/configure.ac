dnl Process this file with autoconf to produce a configure script

AC_PREREQ(2.26)

m4_define([__PROJECT-NAME___major_version], [0])
m4_define([__PROJECT-NAME___minor_version], [1])
m4_define([__PROJECT-NAME___version], [__PROJECT-NAME___major_version.__PROJECT-NAME___minor_version])

AC_INIT([__PROJECT-NAME__],[1.0])
AC_CONFIG_MACRO_DIR([config])
AM_INIT_AUTOMAKE([1.11 dist-bzip2])
AC_LANG([C++])
AX_CXX_COMPILE_STDCXX_11
LT_PREREQ([2.2])
LT_INIT([dlopen])

AC_SUBST(__(upcase "__PROJECT-NAME__")___MAJOR_VERSION, [__PROJECT-NAME___major_version])
AC_SUBST(__(upcase "__PROJECT-NAME__")___MINOR_VERSION, [__PROJECT-NAME___minor_version])
AC_SUBST(__(upcase "__PROJECT-NAME__")___VERSION, [__PROJECT-NAME___version])

dnl Check for programs

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([src/Makefile])
AC_CONFIG_FILES([tests/Makefile])

AC_OUTPUT