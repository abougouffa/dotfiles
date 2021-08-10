;; -*- no-byte-compile: t; -*-

(package! wttrin :recipe (:local-repo "lisp/wttrin"))

(package! theme-magic :pin "844c4311bd26ebafd4b6a1d72ddcc65d87f074e3")

(package! focus)

(package! ess-view :pin "925cafd876e2cc37bc756bb7fcf3f34534b457e2")

(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c" :disable t)

(package! nov :pin "b3c7cc28e95fe25ce7b443e5f49e2e45360944a3")

(package! doct)
(package! org-ref)
(package! org-super-agenda)
(package! org-fragtog)
(package! academic-phrases
  :recipe (:host github
           :repo "nashamri/academic-phrases"))

(package! repo) ;; TODO: configure me!

(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")

(package! bitbake)

(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets")
  :pin "3076cefea0f6ae9d7757f13c27b5602e007b58ec")
(package! laas :recipe (:local-repo "lisp/LaTeX-auto-activating-snippets"))

(package! auctex :pin "6440ec5964dcbe58155e28f00f84ec0118d8fb7b")

(package! graphviz-dot-mode :pin "3642a0a5f41a80c8ecef7c6143d514200b80e194")
