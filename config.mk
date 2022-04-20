CXX ?= clang++
CXXWARN ?= -Werror -Wall -Wextra -Wno-unused -pedantic -Wno-unused-parameter

CXXSTD ?= c++17

BUILD_DIR=build
SRC_DIR=src

SRCS=$(basename $(subst $(SRC_DIR),$(BUILD_DIR),$(wildcard $(SRC_DIR)/*.cpp)))

# Libraries to include and link
INC=-Iinc/ -Isrc/
LIBS=$(LDLIBS)

# Flags
dbg ?= yes
san ?= no

# Debug flags
ifeq ($(dbg),no)
	CXXFLAGS+=-O3 -march=native -flto -DNDEBUG -s
else ifeq ($(dbg),yes)
	CXXFLAGS+=-O0 -g -fno-omit-frame-pointer
else
$(error dbg should be either yes or no)
endif

# Sanitizer flags
ifeq ($(san),yes)
	CXXFLAGS+=-fsanitize=undefined,leak
else ifeq ($(san),no)
else
$(error san should be either yes or no)
endif

