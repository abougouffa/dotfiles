# GDB init file
# Abdelhak Bougouffa (c) 2022

# Save history
set history save on

# Set pretty print
set print pretty on


# This fixes the annoying ncurses TUI gliches and saves typing C-l each time to refresh the screen
define c
  continue
  refresh
end

define n
  next
  refresh
end
