AC_INIT([guile-python], [0.1.0])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror -Wno-portability foreign])

GUILE_PKG([3.0 2.2])
GUILE_PROGS
GUILE_SITE_DIR

if test "x$GUILD" = "x"; then
  AC_MSG_ERROR(['guild' binary not found.])
fi

AC_CONFIG_FILES([Makefile module/Makefile])
AC_CONFIG_FILES([env], [chmod +x env])
AC_OUTPUT
