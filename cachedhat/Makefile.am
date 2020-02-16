include $(top_srcdir)/Makefile.tool.am

EXTRA_DIST = \
	docs/cd-manual.xml \
	docs/cd_annotate-manpage.xml \
	docs/cd_diff-manpage.xml \
	docs/cd_merge-manpage.xml

#----------------------------------------------------------------------------
# Headers, etc
#----------------------------------------------------------------------------

bin_SCRIPTS = cd_annotate cd_diff

noinst_HEADERS = \
	cd_arch.h \
	cd_branchpred.c \
	cd_sim.c

#----------------------------------------------------------------------------
# cd_merge (built for the primary target only)
#----------------------------------------------------------------------------

bin_PROGRAMS = cd_merge

cd_merge_SOURCES = cd_merge.c
cd_merge_CPPFLAGS  = $(AM_CPPFLAGS_PRI)
cd_merge_CFLAGS    = $(AM_CFLAGS_PRI)
cd_merge_CCASFLAGS = $(AM_CCASFLAGS_PRI)
cd_merge_LDFLAGS   = $(AM_CFLAGS_PRI)
# If there is no secondary platform, and the platforms include x86-darwin,
# then the primary platform must be x86-darwin.  Hence:
if ! VGCONF_HAVE_PLATFORM_SEC
if VGCONF_PLATFORMS_INCLUDE_X86_DARWIN
cd_merge_LDFLAGS   += -Wl,-read_only_relocs -Wl,suppress
endif
endif

#----------------------------------------------------------------------------
# cachedhat-<platform>
#----------------------------------------------------------------------------

noinst_PROGRAMS  = cachedhat-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += cachedhat-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

cachedhat_SOURCES_COMMON = \
	cd_main.c \
	cd_arch.c

cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES      = \
	$(cachedhat_SOURCES_COMMON)
cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS       = $(LTO_CFLAGS) \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_PRI_CAPS@)
cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_PRI_CAPS@)
cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_PRI@ \
	$(LINK) \
	$(cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS) \
	$(cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS)

if VGCONF_HAVE_PLATFORM_SEC
cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_SOURCES      = \
	$(cachedhat_SOURCES_COMMON)
cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS       = $(LTO_CFLAGS)\
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_SEC_CAPS@)
cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_SEC@ \
	$(LINK) \
	$(cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS) \
	$(cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS)
endif



#----------------------------------------------------------------------------
# vgpreload_cachedhat-<platform>.so
#----------------------------------------------------------------------------

noinst_PROGRAMS += vgpreload_cachedhat-@VGCONF_ARCH_PRI@-@VGCONF_OS@.so
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += vgpreload_cachedhat-@VGCONF_ARCH_SEC@-@VGCONF_OS@.so
endif

if VGCONF_OS_IS_DARWIN
noinst_DSYMS = $(noinst_PROGRAMS)
endif

vgpreload_cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_SOURCES      = 
vgpreload_cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
vgpreload_cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CFLAGS       = \
	$(AM_CFLAGS_PSO_@VGCONF_PLATFORM_PRI_CAPS@)
vgpreload_cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_DEPENDENCIES = \
	$(LIBREPLACEMALLOC_@VGCONF_PLATFORM_PRI_CAPS@)
vgpreload_cachedhat_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@) \
	$(LIBREPLACEMALLOC_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)

if VGCONF_HAVE_PLATFORM_SEC
vgpreload_cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_SOURCES      = 
vgpreload_cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
vgpreload_cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CFLAGS       = \
	$(AM_CFLAGS_PSO_@VGCONF_PLATFORM_SEC_CAPS@)
vgpreload_cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_DEPENDENCIES = \
	$(LIBREPLACEMALLOC_@VGCONF_PLATFORM_SEC_CAPS@)
vgpreload_cachedhat_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_LDFLAGS      = \
	$(PRELOAD_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@) \
	$(LIBREPLACEMALLOC_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
endif
