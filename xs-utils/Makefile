MOD ?= HAssertTest

init:
	git submodule update --init
	cd rocket-chip/dependencies && git submodule update --init cde hardfloat diplomacy

comp:
	mill -i xsutils.compile
	mill -i xsutils.test.compile

idea:
	mill -i mill.idea.GenIdea/idea

rtl:
	@mkdir -p build/$(MOD)
	mill -i xsutils.test.runMain xs.utils.test.$(MOD)Top --full-stacktrace -td build/$(MOD) --target systemverilog --split-verilog | tee build/make.log

clean:
	@rm -rf build/*

.PHONY:init idea clean rtl