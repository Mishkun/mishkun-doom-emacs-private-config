;;; +appearance.el -*- lexical-binding: t; -*-

;; Fonts setup
(setq doom-font (font-spec :family "Iosevka" :size 16)
      doom-unicode-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Helvetica"))

;; Color theme choose
(setq doom-theme 'doom-one-light)

;; Absolute line numbers
(setq display-line-numbers-type t)

;; Startup as fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


(defun roam-dashboard-banner ()
  (let* ((banner
          '(
"                                                                                "
"                                ..................                              "
"                          ...°°°°°°°°°°°°°°°°°°°°°°°°°..                        "
"                      .....°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°..     *              "
"                    .......**OOOo**°°******°°°°°°°°°*oooo°°°°°.                 "
"                  ...°.....°*o#@@#Oo#@#OOoooo*°°**oO##Oo*°°°°°°°.               "
"                 . .**oo*°°*oooOO#@@@@@@@##OOooO#@@#Oo*°°°°°°°°°°°.             "
"                  .°***OOOO#@@@@@@@@@@@@@@@@#@@@@#oo*°°°°°°°°°°°°°°°.           "
"                  .°*oOo*ooOO#@@@@@@@@@@@@@@#@@@#OOoo**°°°°°°°°°°°°°°.          "
"                .°*°°*o##@@@@@@@@@@@@@@@@@@@@@@@@@@@@#OOo*°°°°°°°°°°°°°         "
"                .**#Oo***oOO#@@@@@@@@@@@@@@@@@@@@@@@@@@@@##o*°°°°°°°°°°.        "
"                .°**O##@@@@@@@@@@@@@##@@@@@@@@@@@@@@@@@@@@@Oo*°°°°°°°°°°.       "
"               .**oO****ooO@@@@@@@@@@@@###OOo******ooOOOOOo**°°°°°°°°°°°.       "
"                 °*O@#O###@#@@@@@@@@@@@@@@#o*.°°°°°°°°°°°°°°°°°°°°°°°°°.°       "
"                .°***ooOO#@@@@@@@@@@@@@@@@@#o*°.°°°°°°°°°°°°°°°°°°°°°°..°       "
"                .*o#####@@##@@@@@@@@@@@@@@@@@Oo°°.°°.°********°°°°°°°....   *   "
"                  °*ooO##@@@@@@@@@@@@@@@@@@@@@#Oo°..*o#@@@@@@@@@@####OOo*°    * "
"                 °*oOOOOOO#@@@@@@@@@@@@@@@@@@@@@#O**o#@@@@@@@#####OOOO#@@O*°°° @"
"                 ...°**ooO#@@@@@@@@@@@@@@@@@@@@@@@#O*#@@@#o*°°°°°°...°*o@@@O*** "
"       ..........           °o@@@@@@@@@@@@@@@@@@@@@@#oo@@oo.............**@@@***"
"       .....................   *@@@@@@@@@@@@@@@@@@@@@@o*oo°..........   **O#@O**"
"      ...........   ..........  .@@@@@@@@@@@@@@@@@@@@@@o**..........    °*°°°***"
"      .........  °*. ..........  O@@@@@@@@@@@@@@@@@@@@@#*°..°°°**oOO*°°O        "
"      .........  @@°  .........  #@@@@@@@@@@@@@@@@@@@@##oOO##@@@@@@@@#o*°°@  o°°"
"     .........   °.  .......... °@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@####@@@@O*°°*  *"
"     ............  ..........   .°°°°°°°°°°°°°°°°°°°°***°°°°°°.     °*O@@@#o*°* "
"     .....................    .*o#@@@#O*      oOOOOO     OOOOO°  °OO##O*o#@@@O**"
"    .......... ..........    O@@@@o*o@@@@*   #@@@@@@°   o@@@@@o .@@@@@@. .*o@@**"
"    .........   ..........  O@@@O    O@@@@  #@@O.@@@O   @@@#@@o #@@#@@@  °*#@O**"
"    .........    ..........  #@@.    @@@@O #@@@  @@@@  O@@@o@@O*@@*@@@# °*#@@***"
"   .........      ..........  #@.   o@@@@.#@@@@@@@@@@° @@@@.@@@@@**@@@O °*@@o** "
"   .........       ..........  O@O#@@@@O°@@@@o°°°@@@@*O@@@O #@@@O @@O*ooO#@o** *"
"                                o##O*°  °ooo*    °oo* *ooo.       *o°*@@@@o°* * "
"                                                                   .*oO*°°°o *  "
"                                                                   #**°°@   *   "
            ))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(when (boundp 'org-roam-enabled)
  (setq +doom-dashboard-ascii-banner-fn #'roam-dashboard-banner))


(after! writeroom-mode
  (progn
    ;; disable mixed-pitch and all of the changes to the font altogether
    (setq +zen-mixed-pitch-modes '())
    (setq +zen-text-scale 0)
    (setq writeroom-width 110)
    (message "Zen loaded with %s %s" +zen-mixed-pitch-modes +zen-text-scale)
    ))
