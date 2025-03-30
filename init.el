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

;; GUI CONFIGURATION
(menu-bar-mode -1) ;; Remove the menu bar
(scroll-bar-mode -1) ;; Remove the scroll bar
(tool-bar-mode -1) ;; Remove the tool bar
(fringe-mode -1) ;; Remove fringe from window

;; LINE NUMBERS
(column-number-mode)
(global-display-line-numbers-mode t) ;; Enable line numbers
(setq display-line-numbers 'relative) ;; Set line numbering to relative

(dolist (mode '(org-mode-hook ;; Disable line numbers for the following modes
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; FONT
(set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" :height 170)

;; GENERAL KEYBINDS
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; FONT KEYBINDS
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; MISC SETTINGS
(setq make-backup-files nil) ;; Stop emacs from creating backup files
(setq user-full-name "Harrison DiAmbrosio")
(setq user-main-address "hdiambrosio@gmail.com")

;; COUNSEL
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(global-unset-key (kbd "C-j"))
(global-set-key (kbd "C-j") 'counsel-switch-buffer)

;; THEME
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker :no-confirm))

;; IVY
(use-package ivy
  :diminish ;; Keep ivy minor mode out of the mode line
  :bind ( ;; ("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; WHICH KEY
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.5))

;; ALL THE ICONS
(use-package all-the-icons)

;; HELPFUL
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer egg/leader-keys
			 :keymaps '(normal insert visual emacs)
			 :prefix "SPC"
			 :global-prefix "C-SPC")
  (egg/leader-keys
   "p" '(:ignore t :which-key "Picks")
   "pb" '(counsel-switch-buffer :which-key "Switch to buffer")
   "pv" '(dired-jump :which-key "Enter dired")))

(defun egg/evil-hook ()
  (dolist (mode '(custom-mode
	    eshell-mode
	    term-mode)))
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
  (evil-global-set-key 'motion "j"  'evil-next-visual-line)
  (evil-global-set-key 'motion "k"  'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Evil Collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
;; remove something from evil-collection-mode-list if the evil binding or evil collection binding aren't working properly in that mode

;; IDK WHAT THIS STUFF IS
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil evil-collection gruber-darker-theme ivy ivy-rich nerd-icons
	  shrink-path)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
