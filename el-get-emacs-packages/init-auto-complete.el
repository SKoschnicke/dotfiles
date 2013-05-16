;;; Require
(require 'auto-complete)
(require 'auto-complete-extension nil t) ;optional
(require 'auto-complete-latex nil t) ;optional
(require 'auto-complete-ruby nil t) ;optional
;(require 'auto-complete-yasnippet nil t) ;optional
;(require 'auto-complete-semantic nil t)  ;optional
;(require 'auto-complete-gtags nil t)     ;optional

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'latex-mode 'enh-ruby-mode)
