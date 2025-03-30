;; This is written by Harrison DiAmbrosio
;; https://eggbert.xyz

;; Give me that sweet sweet evil mode
;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-refresh-contents)
(package-initialize)

(load-file "~/.config/emacs/packages/evil.el")
(load-file "~/.config/emacs/packages/gruber-darker.el")
