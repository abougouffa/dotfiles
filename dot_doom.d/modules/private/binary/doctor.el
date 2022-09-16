;;; private/binary/doctor.el -*- lexical-binding: t; -*-

(when (modulep! +disasm)
  (unless (executable-find "objdump")
    (warn! "The objdump tool is necessary to disassemble executables.")))
