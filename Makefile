# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#


ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

DEPS_PLT=$(CURDIR)/.deps_plt

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

# =============================================================================
# Handle version discovery
# =============================================================================

# We have a problem that we only have 10 minutes to build on travis
# and those travis boxes are quite small. This is ok for the fast
# dialyzer on R15 and above. However on R14 and below we have the
# problem that travis times out. The code below lets us not run
# dialyzer on R14
OTP_VSN=$(shell erl -noshell -eval 'io:format("~p", [erlang:system_info(otp_release)]), erlang:halt(0).' | perl -lne 'print for /R(\d+).*/g')
TRAVIS_SLOW=$(shell expr $(OTP_VSN) \<= 14 )

ifeq ($(TRAVIS_SLOW), 0)
DIALYZER=$(shell which dialyzer)
else
DIALYZER=: not running dialyzer on R14
endif

.PHONY: all compile doc clean test dialyzer typer shell distclean pdf \
	update-deps escript clean-common-test-data rebuild

all: deps compile escript dialyzer test

# =============================================================================
# Rules to build the system
# =============================================================================

deps:
	$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile

escript: deps
	$(REBAR) skip_deps=true escriptize

doc:
	$(REBAR) skip_deps=true doc

eunit: compile clean-common-test-data
	$(REBAR) skip_deps=true eunit

ct: compile clean-common-test-data
	mkdir -p $(CURDIR) logs
	ct_run -pa $(CURDIR)/ebin \
	-pa $(CURDIR)/deps/*/ebin \
	-logdir $(CURDIR)/logs \
	-dir $(CURDIR)/test/ \
	-suite rclt_command_SUITE rclt_discover_SUITE -suite rclt_release_SUITE

test: compile eunit ct

$(DEPS_PLT):
	@echo Building local erts plt at $(DEPS_PLT)
	@echo
	$(DIALYZER) --output_plt $(DEPS_PLT) --build_plt \
	   --apps erts kernel stdlib -r deps

dialyzer: $(DEPS_PLT)
	$(DIALYZER) --fullpath --plt $(DEPS_PLT) \
		 -I include -Wrace_conditions -r ./ebin

typer:
	typer --plt $(DEPS_PLT) -r ./src

shell: get-deps compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)

pdf:
	pandoc README.md -o README.pdf

clean-common-test-data:
# We have to do this because of the unique way we generate test
# data. Without this rebar eunit gets very confused
	- rm -rf $(CURDIR)/test/*_SUITE_data

clean: clean-common-test-data
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	- rm -rf $(CURDIR)/ebin
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rf $(DEPS_PLT)
	- rm -rvf $(CURDIR)/deps

rebuild: distclean deps compile escript dialyzer test
