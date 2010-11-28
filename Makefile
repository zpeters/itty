UNAME	       := $(shell uname -s)
LIBDIR		=  `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
APP		= itty
VERSION		= 0.0.1
CC  		= erlc
ERL     	= erl
EBIN		= ebin
CFLAGS  	= -I include -pa $(EBIN)
COMPILE		= $(CC) $(CFLAGS) -o $(EBIN)
EBIN_DIRS	= $(wildcard deps/*/ebin)

all: ebin compile

compile:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

edoc:		
	@echo Generating $(APP) documentation from srcs
	@erl -noinput -eval 'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}, {todo, true}, {private, true}])' -s erlang halt	

shell:
	make all
	erl -sname console -pa ebin
run:
	make all
	erl -pa ebin -run itty -run init stop -noshell      

ebin:
	@mkdir ebin

clean: 
ifeq ($(UNAME), windows32)
	del /F/Q/S ebin\*.beam
	del /F/Q/S ebin\*.dump
	del /F/Q/S ebin\*.boot
	del /F/Q/S ebin\*.rel
	del /F/Q/S ebin\*.script
	del *.dump
	del /F/Q/S doc\*.html
	del /F/Q/S doc\*.css
	del /F/Q/S doc\erlang.png
	del /F/Q/S doc\edoc-info
else
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump ebin/*.boot ebin/*.rel ebin/*.script doc/*.html doc/*.css doc/erlang.png doc/edoc-info
endif
