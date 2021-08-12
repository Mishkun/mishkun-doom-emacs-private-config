;;; +mappings.el -*- lexical-binding: t; -*-


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

;; Intelij-style duplicate line
(map!
 :g "s-d"
 (cmd! (let* ((pos-end (line-beginning-position 2))
              (line    (buffer-substring (line-beginning-position) pos-end)))
         (goto-char pos-end)
         (unless (bolp) (newline))
         (save-excursion ;; leave point before the duplicate line
           (insert line)))))
