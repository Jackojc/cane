# cane

.POSIX:

include config.mk

all: options build

config:
	@mkdir -p $(BUILD_DIR)

options:
	@printf "cxx \033[32m$(CXX)\033[0m | "
	@printf "dbg \033[32m$(dbg)\033[0m | "
	@printf "san \033[32m$(san)\033[0m | "
	@printf "cxxflags \033[32m-std=$(CXXSTD) $(CXXFLAGS)\033[0m\n"

build: $(SRCS)

$(SRCS): options config
	@printf "tgt \033[32m$@\033[0m\n"
	@$(CXX) -std=$(CXXSTD) $(CXXWARN) $(CXXFLAGS) $(LDFLAGS) $(CPPFLAGS) $(INC) \
		$(LIBS) -o $@ $(SRC_DIR)/$(basename $(notdir $@)).cpp

clean:
	rm -rf $(BUILD_DIR)/ *.gcda

.PHONY: all options clean

