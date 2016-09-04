Lazarus Installation
--------------------

Lazarus is an open source IDE for the programming language "Object Pascal". The included Free Pascal Compiler (FPC) generates EXE files directly, which are statically compiled/linked without any required libraries, runtimes or other dependencies and run on any version of Windows.

Lazarus has a strong resemblance to "Borland Delphi", which was widely used in German speaking countries around the year 2000. As open source development environment, the language continues to survive in fringe areas and its use may still make sense in some cases, e.g. for smaller projects and stand-alone tools that ship without an installer.

(There is also a commercial successor to Borland Delphi named "Embarcadero Delphi" which costs thousands of dollars in licensing fees.)

#### Download:
- http://www.lazarus-ide.org

#### Installation:
- Language: English
- Target Directory: C:\lazarus
- Full Installation
- Startmenu: Lazarus
- Check both boxes
- Install

#### First Start:
- Start IDE via desktop icon
- Leave all settings in the dialog untouched and start the IDE

#### Change Language (if necessary):
- open "Tools-Options"
- choose "Environment-General"
- choose language "Englisch [en]"
- restart Lazarus

#### IDE configuration (optional):
- open "Tools-Options"
- disable code folding in "Editor-Code Folding"
- disable backup files in "Environment-Backup"

#### configure project options:
- open "Project-Project Options-Compiler Options"
- Paths:
  - change "Unit output directory" to "bin\obj"
  - change "Target file name" to "bin\project1"
- Parsing: choose "Intel" assembler style
- Compilation and Linking: choose "0 (no optimization)"
- Debugging:
  - Type of debug info: choose "Dwarf with sets"
  - "Use external gdb debug symbols file" anhaken
- enable checkbox "Set compiler options as default" in the lower left corner
- click OK to save these options for all future projects

WARNING: Do not mess around with compiler settings. For debugging to work properly,
all optimizations need to be turned off. If you enabled the external gdb symbols file,
your compiled executables should be fairly small in size (~2MB instead of >15MB!).

Now you may open a new or existing project (*.lpi) via menu "Project".
