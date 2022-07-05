# cane version (ISO 8601)
VERSION=2022-07-5

PREFIX=/usr/local
MANPREFIX=$(PREFIX)/share/man

INC=-Isrc/ -Imodules/conflict/include
LIB=-ljack

CXX=g++

CANE_CXXSTD=c++17
CANE_CXXWARN=-Werror -Wall -Wextra -Wno-unused -pedantic -Wno-unused-parameter

CANE_DBG_CPPFLAGS=-DCANE_VERSION=\"$(VERSION)\"
CANE_DBG_CXXFLAGS=-std=$(CANE_CXXSTD) $(CANE_CXXWARN) $(CANE_CPPFLAGS) \
	-Og -g -fno-omit-frame-pointer $(CXXFLAGS) $(INC)
CANE_DBG_LDFLAGS=$(LIB) $(LDFLAGS)

CANE_REL_CPPFLAGS=-DCANE_VERSION=\"$(VERSION)\" -DNDEBUG
CANE_REL_CXXFLAGS=-std=$(CANE_CXXSTD) $(CANE_CXXWARN) $(CANE_CPPFLAGS) \
	-march=native -O3 $(CXXFLAGS) $(INC)
CANE_REL_LDFLAGS=-flto -s $(LIB) $(LDFLAGS)

DBG?=no

ifeq ($(DBG),no)
	CANE_CPPFLAGS=$(CANE_REL_CPPFLAGS)
	CANE_CXXFLAGS=$(CANE_REL_CXXFLAGS)
	CANE_LDFLAGS=$(CANE_REL_LDFLAGS)
else ifeq ($(DBG),yes)
	CANE_CPPFLAGS=$(CANE_DBG_CPPFLAGS)
	CANE_CXXFLAGS=$(CANE_DBG_CXXFLAGS)
	CANE_LDFLAGS=$(CANE_DBG_LDFLAGS)
else
$(error DBG should be either yes or no)
endif

