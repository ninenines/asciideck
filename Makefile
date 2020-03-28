# See LICENSE for licensing information.

PROJECT = asciideck
PROJECT_DESCRIPTION = Asciidoc for Erlang.
PROJECT_VERSION = 0.2.0

# Dependencies.

TEST_ERLC_OPTS += +'{parse_transform, eunit_autoexport}'
TEST_DEPS = $(if $(CI_ERLANG_MK),ci.erlang.mk) ct_helper
dep_ct_helper = git https://github.com/ninenines/ct_helper master

# CI configuration.

dep_ci.erlang.mk = git https://github.com/ninenines/ci.erlang.mk master
DEP_EARLY_PLUGINS = ci.erlang.mk

AUTO_CI_OTP ?= OTP-20+
AUTO_CI_HIPE ?= OTP-LATEST
# AUTO_CI_ERLLVM ?= OTP-LATEST
# Not sure why but the groff tests fail before OTP-21.
AUTO_CI_WINDOWS ?= OTP-21+

include erlang.mk

# Test building documentation of projects that use Asciideck
# and run Groff checks against the output.
#
# We only run against asciidoc-manual because the guide requires
# the DocBook toolchain at this time.

.PHONY: groff

GROFF_PROJECTS = cowboy gun ranch

tests:: groff

groff: $(addprefix groff-,$(GROFF_PROJECTS))

$(ERLANG_MK_TMP)/groff:
	$(verbose) mkdir -p $@

define groff_targets
$(ERLANG_MK_TMP)/groff/$1: | $(ERLANG_MK_TMP)/groff
	$(verbose) rm -rf $$@
	$(verbose) git clone -q --depth 1 -- $(call dep_repo,$1) $$@
	$(verbose) mkdir $$@/deps
ifeq ($(PLATFORM),msys2)
	$(verbose) cmd //c mklink $(call core_native_path,$(ERLANG_MK_TMP)/groff/$1/deps/asciideck) \
		$(call core_native_path,$(CURDIR))
else
	$(verbose) ln -s $(CURDIR) $$@/deps/asciideck
endif
	$(verbose) touch $$@/deps/ci.erlang.mk
	$(verbose) cp $(CURDIR)/erlang.mk $$@/

groff-$1: $(ERLANG_MK_TMP)/groff/$1 app
	$(gen_verbose) $(MAKE) -C $$< asciidoc-manual MAKEFLAGS= DEPS_DIR=$$</deps ERL_LIBS=$$</deps
	$(verbose) for f in $$</doc/man*/*.gz; do \
		echo " GROFF " `basename "$$$$f"`; \
		zcat "$$$$f" | groff -man -rD1 -z -ww; \
	done
endef

$(foreach p,$(GROFF_PROJECTS),$(eval $(call groff_targets,$p)))
