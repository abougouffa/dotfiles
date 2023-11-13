# GDB init file
# Abdelhak Bougouffa (c) 2022

# Save history
set history save on
set history filename ~/.gdb_history
set history remove-duplicates 2048

# When debugging my apps, debug information of system libraries
# aren't that important
set debuginfod enabled off

# Set pretty print
set print pretty on

# I hate stepping into system libraries when I'm debugging my
# crappy stuff, so lets add system headers to skipped files
skip pending on
python
import os

# Add paths here, they will be explored recursivly
LIB_PATHS = ["/usr/include" "/usr/local/include"]

for lib_path in LIB_PATHS:
  for root, dirs, files in os.walk(lib_path):
    for file in files:
      cmd = f"skip file {os.path.join(root, file)}"
      gdb.execute(cmd, True, to_string=True)
end
skip enable

skip pending on
guile

end
skip enable

# This fixes the annoying ncurses TUI gliches and saves typing C-l each time to refresh the screen
define cc
  continue
  refresh
end

define nn
  next
  refresh
end
