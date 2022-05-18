;;; lisp/messages-buffer-auto-tail.el -*- lexical-binding: t; -*-

(defvar +messages-buffer-auto-tail--enabled nil)

(defun +messages-buffer-auto-tail--advice (&rest arg)
  "Make *Messages* buffer auto-scroll to the end after each message."
  (let* ((buf-name (buffer-name (messages-buffer)))
         ;; Create *Messages* buffer if it does not exist
         (buf (get-buffer-create buf-name)))
    ;; Activate this advice only if the point is _not_ in the *Messages* buffer
    ;; to begin with. This condition is required; otherwise you will not be
    ;; able to use `isearch' and other stuff within the *Messages* buffer as
    ;; the point will keep moving to the end of buffer :P
    (when (not (string= buf-name (buffer-name)))
      ;; Go to the end of buffer in all *Messages* buffer windows that are
      ;; *live* (`get-buffer-window-list' returns a list of only live windows).
      (dolist (win (get-buffer-window-list buf-name nil :all-frames))
        (with-selected-window win
          (goto-char (point-max))))
      ;; Go to the end of the *Messages* buffer even if it is not in one of
      ;; the live windows.
      (with-current-buffer buf
        (goto-char (point-max))))))

(defun +messages-buffer-toggle-auto-tail ()
  "Auto tail the '*Messages*' buffer."
  (interactive)
  ;; Add/remove an advice from the 'message' function.
  (cond (+messages-buffer-auto-tail--enabled
         (advice-remove 'message '+messages-buffer-auto-tail--advice)
         (setq +messages-buffer-auto-tail--enabled nil)
         (message "+messages-buffer-auto-tail: Disabled."))
        (t
         (advice-add 'message :after '+messages-buffer-auto-tail--advice)
         (setq +messages-buffer-auto-tail--enabled t)
         (message "+messages-buffer-auto-tail: Enabled."))))
