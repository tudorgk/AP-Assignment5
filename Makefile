all: compile read


test: compile read
	erlc +debug_info tester.erl
	erl tester.erl

compile:
	erlc +debug_info mr.erl

read:
	erlc +debug_info read_mxm.erl
