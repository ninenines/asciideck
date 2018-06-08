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

AUTO_CI_OTP ?= OTP-19+
AUTO_CI_HIPE ?= OTP-LATEST
# AUTO_CI_ERLLVM ?= OTP-LATEST
AUTO_CI_WINDOWS ?= OTP-19+

include erlang.mk
