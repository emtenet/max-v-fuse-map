PROJECT = experiments
PROJECT_DESCRIPTION = Altera MAX V Fuse Map (experiments)
PROJECT_VERSION = 1.0.0
PROJECT_MOD = experiments

DEPS += jsx
dep_jsx = git https://github.com/talentdeficit/jsx v3.0.0

ERLC_OPTS = -Werror \
 +warn_bif_clash \
 +warn_deprecated_function \
 +warn_export_all \
 +warn_export_vars \
 +warn_exported_vars \
 +warn_format \
 +warn_obsolete_guard \
 +warn_shadow_vars \
 +warn_untyped_record \
 +warn_unused_function \
 +warn_unused_record \
 +warn_unused_vars \
 +debug_info

SNAME ?= experiments
SHELL_OPTS = -setcookie michael -sname $(SNAME) -run experiments

TEST_DEPS = solarized
dep_solarized = git https://github.com/emtenet/erlang-solarized.git master
EUNIT_OPTS = no_tty, {report, {solarized_eunit, []}}
CT_OPTS = -ct_hooks solarized_ct and testing_ct

PLT_APPS += crypto
#DIALYZER_OPTS += --src -r test

include erlang.mk
