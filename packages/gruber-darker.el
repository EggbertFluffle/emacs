;; Download gruber-darker
(unless (package-installed-p 'gruber-darker)
  (package-install 'gruber-darker))

;; Enable gruber-darker
(load-theme 'gruber-darker)
