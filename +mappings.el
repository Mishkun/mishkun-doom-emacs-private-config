;;; +mappings.el -*- lexical-binding: t; -*-

;; Remove esc key sequence - I mapped ESC to Caps Lock, so I do not need this pathetic feature
(setq evil-escape-key-sequence nil)

;; Disable overwriting clipboard on yank/delete
(setq select-enable-clipboard nil)
(use-package! simpleclip
  :config
  (simpleclip-mode 1))

(after! simpleclip
  (defun yank-buffer-path ()
    "Copy the current buffer's path to the kill ring."
     (interactive)
     (if-let (filename (or (buffer-file-name (buffer-base-buffer))
                           (bound-and-true-p list-buffers-directory)))
         (message "Copied path to clipboard: %s"
                  (simpleclip-set-contents (abbreviate-file-name filename)))
       (error "Couldn't find filename in current buffer"))
))

(map! :after simpleclip :leader
      (:prefix "f"
       ("y" #'yank-buffer-path)))
;; My keyboard has combined SPC and Super in one key, so I keep closing windows
;; with Super-W instead of moving them with SPC w l
(map! :g "s-w" nil)

;; Add vimish fast scroll to ivy
(map! :after ivy :map ivy-minibuffer-map
      "C-u" #'ivy-scroll-down-command
      "C-d" #'ivy-scroll-up-command)

;; TAB in counsel buffer faster
(map! :after counsel :map counsel-find-file-map
      "RET" #'ivy-alt-done)

;; ugly workaround for alt gr inserting of eng layer symbols from russian typography layer
(map! :i "M-1"   (cmd! (insert "!"))
      :i "M-2"   (cmd! (insert "@"))
      :i "M-3"   (cmd! (insert "#"))
      :i "M-4"   (cmd! (insert "$"))
      :i "M-5"   (cmd! (insert "%"))
      :i "M-6"   (cmd! (insert "^"))
      :i "M-7"   (cmd! (insert "&"))
      :i "M-8"   (cmd! (insert "*"))
      :i "M-9"   (cmd! (insert "("))
      :i "M-0"   (cmd! (insert ")"))
      )

;; workaround to mace workspace bindings work with cfw:calendar
(map! :map org-agenda-mode-map
      :g "s-1"   #'+workspace/switch-to-0
      :g "s-2"   #'+workspace/switch-to-1
      :g "s-3"   #'+workspace/switch-to-2
      :g "s-4"   #'+workspace/switch-to-3
      :g "s-5"   #'+workspace/switch-to-4
      :g "s-6"   #'+workspace/switch-to-5
      :g "s-7"   #'+workspace/switch-to-6
      :g "s-8"   #'+workspace/switch-to-7
      :g "s-9"   #'+workspace/switch-to-8
      )


;; Intelij-style duplicate line
(map!
 :g "s-d"
 (cmd! (let* ((pos-end (line-beginning-position 2))
              (line    (buffer-substring (line-beginning-position) pos-end)))
         (goto-char pos-end)
         (unless (bolp) (newline))
         (save-excursion ;; leave point before the duplicate line
           (insert line)))))

