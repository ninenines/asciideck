# See LICENSE for licensing information.

PROJECT = asciideck
PROJECT_DESCRIPTION = Asciidoc for Erlang.
PROJECT_VERSION = 0.1.0

# Options.

CI_OTP ?= OTP-18.0.3 OTP-18.1.5 OTP-18.2.4.1 OTP-18.3.4.4 OTP-19.0.7 OTP-19.1.5

# Dependencies.

TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/ninenines/ct_helper master

include erlang.mk
