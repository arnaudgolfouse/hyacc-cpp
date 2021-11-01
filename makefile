############################################################
# This file is part of Hyacc, a LR(1) parser generator.
# Copyright (C) 2007 Xin Chen. chenx@hawaii.edu
#
# Hyacc is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# Hyacc is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Hyacc; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#
############################################################
#
# makefile for hyacc
#
# These targets are used:
#   y, release, debug, clean install, uninstall, pack
# Here target y is the same as release.
#
# To use this makefile, the only configuration needed is
# to change the value of INSTALL_PATH below to the 
# destination path you want to install hyacc.
#
# @Author: Xin Chen
# @Created on: Feb 10, 2007
# @Last modified: Oct 25, 2007
#############################################################

# NOTE: choose your installation directory here.
INSTALL_PATH = /usr/local
# INSTALL_PATH = `pwd` # current directory

SRC_HEADER = y.hpp stack_config.hpp mrt.hpp lane_tracing.hpp
SRC = y.cpp get_yacc_grammar.cpp gen_compiler.cpp get_options.cpp \
      version.cpp hyacc_path.cpp symbol_table.cpp state_hash_table.cpp \
      queue.cpp gen_graphviz.cpp lr0.cpp lane_tracing.cpp stack_config.cpp \
      mrt.cpp upe.cpp lrk.cpp lrk_util.cpp
OBJS = y.o get_yacc_grammar.o gen_compiler.o get_options.o \
       version.o hyacc_path.o symbol_table.o state_hash_table.o \
       queue.o gen_graphviz.o lr0.o lane_tracing.o \
       stack_config.o mrt.o upe.o lrk.o lrk_util.o
PACK_SRC = $(SRC) $(SRC_HEADER) inst.cpp makefile \
           hyaccpar hyaccmanpage hyaccmanpage.html \
           hyacc.1 GPL_license readme.pdf
TARGET = hyacc
CC = clang++
DATE = `date '+%m-%d-%y'`
PACK_NAME = hyacc_$(DATE).tar
FFLAG = -std=c++20


$(TARGET) : $(OBJS) $(SRC_HEADER) 
	@echo please wait ...
	$(CC) $(FFLAG) -o $(TARGET) $(OBJS) 
	@echo compiled successfully 

#
# all - the old default.
#
all : $(SRC) $(SRC_HEADER)
	@echo please wait ...
	$(CC) $(FFLAG) -o $(TARGET) $(SRC)
	@echo compiled successfully

release : $(SRC) $(SRC_HEADER) 
	@echo please wait ...
	@make create_path_file
	$(CC) $(FFLAG) -o $(TARGET) $(SRC)  
	@echo release version is successfully built

debug : $(SRC) $(SRC_HEADER)
	@echo please wait ...
	@make create_path_file
	$(CC) $(FFLAG) -g -o $(TARGET) $(SRC)
	@echo debug version is successfully built

clean :
	rm -f ./$(TARGET) ./$(OBJS)
	@echo target is cleaned

cscope.out : $(SRC) hyaccpar hyaccpark
	cscope -q -b $(SRC) hyaccpar hyaccpark

install : create_path_file
	@echo compile ...
	@make release
	@echo copy to destination $(INSTALL_PATH) ...
	@mkdir -p $(INSTALL_PATH)/bin
	@mkdir -p $(INSTALL_PATH)/lib/hyacc
	@-cp -f ./$(TARGET) $(INSTALL_PATH)/bin
	@-cp -f ./hyaccpar $(INSTALL_PATH)/lib/hyacc
	@-cp -f ./hyaccpark $(INSTALL_PATH)/lib/hyacc
	@-cp -f ./hyaccmanpage $(INSTALL_PATH)/lib/hyacc
	@echo installation succesfully finishes

create_path_file: inst.cpp
	@echo regenerate file hyacc_path.cpp ...
	@$(CC) $(FFLAG) inst.cpp -o inst
	@./inst $(INSTALL_PATH)
	@rm -f inst

uninstall:
	@echo to uninstall, manually remove the following files:
	@echo $(INSTALL_PATH)/bin/$(TARGET)
	@echo $(INSTALL_PATH)/lib/hyacc/hyaccpar
	@echo $(INSTALL_PATH)/lib/hyacc/hyaccpark
	@echo $(INSTALL_PATH)/lib/hyacc/hyaccmanpage

dist:
	@-rm -f $(PACK_NAME).gz
	@tar cvf $(PACK_NAME) $(PACK_SRC)
	@gzip $(PACK_NAME)
	@ls $(PACK_NAME).gz
	@echo pack successfully finishes

#
# for object files.
#

gen_compiler.o : gen_compiler.cpp y.hpp
	$(CC) $(FFLAG) gen_compiler.cpp

gen_graphviz.o : gen_graphviz.cpp y.hpp
	$(CC) $(FFLAG) gen_graphviz.cpp

get_options.o : get_options.cpp y.hpp
	$(CC) $(FFLAG) get_options.cpp

get_yacc_grammar.o : get_yacc_grammar.cpp y.hpp
	$(CC) $(FFLAG) get_yacc_grammar.cpp

hyacc_path.o : hyacc_path.cpp y.hpp
	$(CC) $(FFLAG) hyacc_path.cpp

lane_tracing.o : lane_tracing.cpp lane_tracing.hpp y.hpp stack_config.hpp
	$(CC) $(FFLAG) lane_tracing.cpp 

lr0.o : lr0.cpp y.hpp
	$(CC) $(FFLAG) lr0.cpp

lrk.o : lrk.cpp y.hpp lane_tracing.hpp
	$(CC) $(FFLAG) lrk.cpp

lrk_util.o : lrk_util.cpp y.hpp lane_tracing.hpp
	$(CC) $(FFLAG) lrk_util.cpp

mrt.o : mrt.cpp mrt.hpp y.hpp
	$(CC) $(FFLAG) mrt.cpp

queue.o : queue.cpp y.hpp
	$(CC) $(FFLAG) queue.cpp

stack_config.o : stack_config.cpp y.hpp
	$(CC) $(FFLAG) stack_config.cpp

state_hash_table.o : state_hash_table.cpp y.hpp
	$(CC) $(FFLAG) state_hash_table.cpp

symbol_table.o : symbol_table.cpp y.hpp
	$(CC) $(FFLAG) symbol_table.cpp

upe.o : upe.cpp y.hpp mrt.hpp
	$(CC) $(FFLAG) upe.cpp

version.o : version.cpp y.hpp
	$(CC) $(FFLAG) version.cpp

y.o : y.cpp y.hpp lane_tracing.hpp
	$(CC) $(FFLAG) y.cpp


