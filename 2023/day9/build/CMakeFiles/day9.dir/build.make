# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.27

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/mathias/Documents/AoC/2023/day9

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/mathias/Documents/AoC/2023/day9/build

# Include any dependencies generated for this target.
include CMakeFiles/day9.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/day9.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/day9.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/day9.dir/flags.make

CMakeFiles/day9.dir/src/day9.cpp.o: CMakeFiles/day9.dir/flags.make
CMakeFiles/day9.dir/src/day9.cpp.o: /home/mathias/Documents/AoC/2023/day9/src/day9.cpp
CMakeFiles/day9.dir/src/day9.cpp.o: CMakeFiles/day9.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --progress-dir=/home/mathias/Documents/AoC/2023/day9/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/day9.dir/src/day9.cpp.o"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT CMakeFiles/day9.dir/src/day9.cpp.o -MF CMakeFiles/day9.dir/src/day9.cpp.o.d -o CMakeFiles/day9.dir/src/day9.cpp.o -c /home/mathias/Documents/AoC/2023/day9/src/day9.cpp

CMakeFiles/day9.dir/src/day9.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Preprocessing CXX source to CMakeFiles/day9.dir/src/day9.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/mathias/Documents/AoC/2023/day9/src/day9.cpp > CMakeFiles/day9.dir/src/day9.cpp.i

CMakeFiles/day9.dir/src/day9.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Compiling CXX source to assembly CMakeFiles/day9.dir/src/day9.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/mathias/Documents/AoC/2023/day9/src/day9.cpp -o CMakeFiles/day9.dir/src/day9.cpp.s

# Object files for target day9
day9_OBJECTS = \
"CMakeFiles/day9.dir/src/day9.cpp.o"

# External object files for target day9
day9_EXTERNAL_OBJECTS =

day9: CMakeFiles/day9.dir/src/day9.cpp.o
day9: CMakeFiles/day9.dir/build.make
day9: /usr/lib/libfmt.so
day9: CMakeFiles/day9.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --bold --progress-dir=/home/mathias/Documents/AoC/2023/day9/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable day9"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/day9.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/day9.dir/build: day9
.PHONY : CMakeFiles/day9.dir/build

CMakeFiles/day9.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/day9.dir/cmake_clean.cmake
.PHONY : CMakeFiles/day9.dir/clean

CMakeFiles/day9.dir/depend:
	cd /home/mathias/Documents/AoC/2023/day9/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/mathias/Documents/AoC/2023/day9 /home/mathias/Documents/AoC/2023/day9 /home/mathias/Documents/AoC/2023/day9/build /home/mathias/Documents/AoC/2023/day9/build /home/mathias/Documents/AoC/2023/day9/build/CMakeFiles/day9.dir/DependInfo.cmake "--color=$(COLOR)"
.PHONY : CMakeFiles/day9.dir/depend

