PROJECT = asciideck
PROJECT_DESCRIPTION = Asciidoc for Erlang.
PROJECT_VERSION = 0.1.0

TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/extend/ct_helper master

include erlang.mk
