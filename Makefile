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
ERL = $(shell which erl)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/**/ebin

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

RELCOOL_PLT=$(CURDIR)/.relcool_plt

.PHONY: all compile doc clean eunit dialyzer typer shell distclean pdf get-deps escript

all: compile eunit dialyzer

get-deps:
	$(REBAR) get-deps
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile

escript:
	$(REBAR) escriptize

doc:
	$(REBAR) skip_deps=true doc

eunit: compile
	$(REBAR) skip_deps=true eunit

$(RELCOOL_PLT):
	@echo Building local plt at $(RELCOOL_PLT)
	@echo
	dialyzer --output_plt $(RELCOOL_PLT) --build_plt \
	   --apps erts kernel stdlib -r deps

dialyzer: $(RELCOOL_PLT)
	dialyzer --plt $(RELCOOL_PLT) -Wrace_conditions --src src

typer:
	typer --plt $(RELCOOL_PLT) -r ./src

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

clean:
	$(REBAR) clean

distclean: clean
	rm -rf $(RELCOOL_PLT)
	rm -rvf $(CURDIR)/deps/*
