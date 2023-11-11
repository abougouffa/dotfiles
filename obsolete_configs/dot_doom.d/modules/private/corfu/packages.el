;; -*- no-byte-compile: t; -*-
;;; private/corfu/packages.el

(package! corfu
  :pin "1b159d55adfdec6782441602bcd8352b18256650")
(package! corfu-doc
  :pin "da931367802d01e87e1e496ba5b51aa9126a705d")

(when (modulep! +icons)
  (package! kind-icon
    :pin "de22038e414b46c60b470e06c99ccf680ec173ef"))
