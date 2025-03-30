;; This is written by Harrison DiAmbrosio
;; https://eggbert.xyz

(set-face-attribute 'default nil
  :font "Iosevka NFM"
  :height 110
  :weight 'medium)
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)
(setq-default line-spacing 0.12)

(add-to-list 'default-frame-alist '(font . "Iosevka NFM-18"))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(menu-bar-mode -1) ;; Remove the menu bar
(scroll-bar-mode -1) ;; Remove the scroll bar
(tool-bar-mode -1) ;; Remove the tool bar
(fringe-mode -1) ;; Remove fringe from window

(global-display-line-numbers-mode 1) ;; Enable line numbers
(setq display-line-numbers 'relative) ;; Set line numbering to relative

(setq make-backup-files nil) ;; Stop emacs from creating backup files
