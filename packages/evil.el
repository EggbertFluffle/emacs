;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; Custom evil keybinds
(define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
