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
BUILD_DIR = build
OBJS = $(SRC:%.cpp=$(BUILD_DIR)/%.o)
PACK_SRC = $(SRC) $(SRC_HEADER) inst.cpp makefile \
           hyaccpar hyaccmanpage hyaccmanpage.html \
           hyacc.1 GPL_license readme.pdf
TARGET = hyacc
CC = clang++
DATE = `date '+%m-%d-%y'`
PACK_NAME = hyacc_$(DATE).tar
FFLAG = -std=c++20


$(TARGET) : $(OBJS)
	@echo please wait ...
	$(CC) $(FFLAG) $(OBJS) -o $(TARGET)
	@echo compiled successfully

release : FFLAG += -O2
debug : $(OBJS)
	@echo please wait ...
	@make create_path_file
	$(CC) $(FFLAG) $(OBJS) -o $(TARGET)
	@echo release version is successfully built

debug : FFLAG += -g -Wall -Wextra
debug : $(OBJS)
	@echo please wait ...
	@make create_path_file
	$(CC) $(FFLAG) $(OBJS) -o $(TARGET)
	@echo debug version is successfully built

clean :
	rm -rf ./$(TARGET) ./$(BUILD_DIR)
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

mkdir_build_dir:
	mkdir -p build

$(BUILD_DIR)/%.o : %.cpp $(SRC_HEADER) mkdir_build_dir
	$(CC) $(FFLAG) -c "$*.cpp" -o "build/$*.o"
