## -*- makefile -*-

######################################################################
## Erlang

KIWI_ROOT := ..

ERL := erl
ERLC := $(ERL)c

INCLUDE_DIRS := $(KIWI_ROOT)/include $(wildcard $(KIWI_ROOT)/deps/*/include)
EBIN_DIRS := $(wildcard $(KIWI_ROOT)/deps/*/ebin)
ERLC_FLAGS := -W $(INCLUDE_DIRS:$(KIWI_ROOT)/%=-I $(KIWI_ROOT)/%) $(EBIN_DIRS:%=-pa %)

ifndef no_debug_info
  ERLC_FLAGS += +debug_info
endif

ifdef debug
  ERLC_FLAGS += -Ddebug
endif

EBIN_DIR := $(KIWI_ROOT)/ebin
DOC_DIR  := $(KIWI_ROOT)/docs
EMULATOR := beam

ERL_SOURCES := $(wildcard *.erl)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard $(KIWI_ROOT)/include/*.hrl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
ERL_OBJECTS_LOCAL := $(ERL_SOURCES:%.erl=./%.$(EMULATOR))
APP_FILES := $(wildcard *.app)
EBIN_FILES = $(ERL_OBJECTS) $(APP_FILES:%.app=$(KIWI_ROOT)/ebin/%.app)
EBIN_FILES_NO_DOCS = $(ERL_OBJECTS) $(APP_FILES:%.app=$(KIWI_ROOT)/ebin/%.app)
MODULES = $(ERL_SOURCES:%.erl=%)

$(KIWI_ROOT)/ebin/%.app: %.app
	cp $< $@

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

./%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o . $<
