all: compile

compile:
	erlc +debug_info mr.erl
	erl mr.erl

test:
	erlc +debug_info tester.erl
	erl tester.erl
