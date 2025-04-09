;; This is written by Harrison DiAmbrosio
;; https://eggbert.xyz

;; PACKAGE ARCHIVES
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)

;; Emacs minibuffer configurations.
(use-package emacs
  :config
  ;; GUI CONFIGURATION
  (menu-bar-mode -1) ;; Remove the menu bar
  (scroll-bar-mode -1) ;; Remove the scroll bar
  (tool-bar-mode -1) ;; Remove the tool bar
  (fringe-mode -1) ;; Remove fringe from window

  ;; MISC SETTINGS
  (setq help-window-select t) ;; Automatically focus help windows
  (setq compile-window-select t)
  (setq make-backup-files nil) ;; Stop emacs from creating backup files
  (setq user-full-name "Harrison DiAmbrosio")
  (setq user-main-address "hdiambrosio@gmail.com")
  (global-auto-revert-mode 1) ;; Automatically refresh chaned files
  (setq use-dialog-box nil)
  (setq tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default compilation-scroll-output t)
  (setq backup-inhibited t)
  (setq frame-resize-pixelwise t)
  (push '(fullscreen . maximized) default-frame-alist)

  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))


;; LINE NUMBERS
(column-number-mode)
(global-display-line-numbers-mode t) ;; Enable line numbers
(setq global-display-line-numbers 'relative) ;; Set line numbering to relative

(dolist (mode '(org-mode-hook ;; Disable line numbers for the following modes
        vterm-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; FONT
(defvar egg/default-font "Iosevka NFM")
(defvar egg/variable-pitch-font "MonaspiceAr Nerd Font")
(set-face-attribute 'default nil :font egg/default-font :height 170) ;; Set the font for this frame
(set-face-attribute 'variable-pitch nil :font egg/variable-pitch-font :height 120)
(add-to-list 'default-frame-alist '(font . "Iosevka NFM-18")) ;; Set the font for subsequent frame

;; GENERAL KEYBINDS
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-<return>") 'compile)

;; MAKE DIRED NOT SHIT
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom
  (setq dired-auto-revert-buffer t)
  (setq dired-listing-switches "-agho --group-directories-first")
  :config

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(add-hook 'dired-mode-hook (lambda ()
			    (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; COMPILATION
(setq compilation-finish-functions '((lambda (buffer string) (lsp-headerline-breadcrumb-mode nil) (other-window 1))))

;; FONT KEYBINDS
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; THEME
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker :no-confirm))

;; VERTICO
(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  (keymap-set vertico-map "C-j" 'vertico-next)
  (keymap-set vertico-map "C-k" 'vertico-previous)
  :init
  (vertico-mode))

(use-package consult
  :bind ("C-x b" . consult-buffer))

(use-package savehist
  :init
  (savehist-mode))

;; WHICH KEY
(use-package which-key
  :diminish
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.5))

;; ALL THE ICONS
(use-package all-the-icons)

(use-package all-the-icons-dired
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode))

;; HELPFUL
(use-package helpful
  :ensure t
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer egg/leader-keys
			 :keymaps '(normal visual emacs)
			 :prefix "SPC"
			 :global-prefix "C-SPC")
  (general-create-definer egg/comment-keys
    :keymaps '(normal)
    :prefix "g")
  (egg/leader-keys
   "p" '(:ignoree t :which-key "Picks")
   "ps" '(consult-ripgrep :which-key "Picks ripgrep")
   "pb" '(consult-buffer :which-key "Switch to buffer")
   "pv" '(dired-jump :which-key "Enter dired"))
  (egg/comment-keys
    "c" '(:ignore t :which-key "I am coping, comment line")
    "cc" '(comment-line :which-key "Comment line")
    "b" '(comment-region :which-key "Comment region")))

(defun egg/evil-hook ()
  (dolist (mode '(custom-mode
	    eshell-mode
	    term-mode
        vterm-mode)))
  (add-to-list 'evil-emacs-state-modes modes))

;; EVIL MODE
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)
  (evil-global-set-key 'motion "j"  'evil-next-visual-line)
  (evil-global-set-key 'motion "k"  'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Evil Collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
;; ('evil-collection-mode-list)
;; remove something from evil-collection-mode-list if the evil binding or evil collection binding aren't working properly in that mode

;; This should really mix the color with white to "lighten it"
(defun egg/lighten-color (color percent)
  "Lighten a color by some percentage (0.0-1.0)"
  (let* ((rgb (color-name-to-rgb color))
         (r (nth 0 rgb))
         (g (nth 1 rgb))
         (b (nth 2 rgb)))
    (color-rgb-to-hex (+ r (* r percent))
                      (+ g (* g percent))
                      (+ b (* b percent))
                      2)))

(defun egg/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (let ((block-color (egg/lighten-color (face-background 'default) 0.25)))
    (set-face-attribute 'org-meta-line nil
                        :foreground block-color
                        :background block-color)
    (set-face-attribute 'org-block nil
                        :slant 'italic
                        :font egg/default-font
                        :background block-color)
    (set-face-attribute 'org-quote nil
                        :background block-color))
  (setq evil-auto-indent nil))

(defun egg/what-face (pos)
  "Print the face at point."
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (message "Face: %s" face)))

(use-package org
  :hook
  (org-mode . egg/org-mode-setup)
  :bind
  ([remap org-insert-heading-respect-content] . org-meta-return)
  :config
  (setq org-ellipsis " ⬎ ")
  (setq org-hide-emphasis-markers t))

(defun egg/load-org-bullet-fonts ()
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font egg/variable-pitch-font :weight 'regular :height (cdr face))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config (egg/load-org-bullet-fonts))

(use-package visual-fill-column
  :config
  (setq visual-fill-column-width 75)
  (setq-default visual-fill-column-center-text t)
  :hook (org-mode . (lambda () (visual-fill-column-mode 1))))

;; GOOD SCROLL
(use-package good-scroll
  :diminish
  :config
  (good-scroll-mode 1))

(use-package smartparens
  :diminish
  :config (require 'smartparens-config)
  :hook (prog-mode markdown-mode))

;; LANGUAGE SERVER
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (global-unset-key (kbd "C-l"))
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-disabled-clients '(semgrep-ls))
  :config
  (lsp-enable-which-key-integration t)
  (add-to-list 'warning-suppress-log-types '(lsp-mode))
  (add-to-list 'warning-suppress-types '(lsp-mode)))

(use-package corfu
  :config
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 0)
  :init
  (global-corfu-mode))
  
(dolist (mode '(java-mode-hook
		c-mode-hook
		c++-mode-hook
		typescript-mode-hook))
  (add-hook mode 'lsp))

;; LANGUAGES
;; TYPESCRIPT
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; JAVA
(add-hook 'java-mode-hook 'lsp-mode)
(use-package lsp-java)

;; SVELTE
(use-package svelte-mode
  :config
  (customize-set-variable 'svelte-basic-offset 4)
  (customize-set-variable 'svelte-display-submode-name t))

(use-package cc-mode
  :config
  (setq-default c-basic-offset 4))

;; TERMINAL/SHELL
(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "-\[.\]- ")
  (setq vterm-shell "zsh")
  (setq bterm-max-scrollback 250000))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  (setq term-prompt-regexp "-\[.\]- "))

;; DASHBOARD
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-startup-banner "~/.config/emacs/emacs.png")
  (setq dashboard-items '((projects   . 3)
                          (recents   . 7)))
  (setq dashboard-items-shortcuts '((projects  . "p")
                                    (recents   . "r")))
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-banner-title
                                    dashboard-insert-navigator
                                    dashboard-insert-items))
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

;; Check out https://github.com/rejeep/drag-stuff.el
;; DRAG STUFF
(use-package drag-stuff
  :ensure t
  :diminish
  :config
  (drag-stuff-global-mode t)
  (with-eval-after-load 'evil
    (define-key evil-visual-state-map (kbd "J") 'drag-stuff-down)
    (define-key evil-visual-state-map (kbd "K") 'drag-stuff-up)))

;; PROJECT MANAGEMENT 
(defvar egg/project-base-path "~/programs/")
(defun egg/create-project-directory-list ()
  (setq projectile-project-search-path '())
  (dolist (lang '("c/" "cpp/" "java/" "zig/" "rust/" "js/" "arduino/" "assembly/"
                  "holyc/" "racket/" "shell/" "unity/" "godot/" "emacs-lisp/"
                  "adventOfCode/" "lua/" "python/" "misc/" "java/CS2223/"))
    (message (concat egg/project-base-path lang))
    (add-to-list 'projectile-project-search-path (concat egg/project-base-path lang))))

(use-package project)

;; NO MORE CUSTOM BULLSHIT IN MY CONFIG THANKS
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerr 'nomessage)
(put 'dired-find-alternate-file 'disabled nil)
