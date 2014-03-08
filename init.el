(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-list-file-prefix nil)
 '(blink-cursor-mode nil)
 '(c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "stroustrup"))))
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(default-frame-alist (quote ((height . 32) (width . 110))))
 '(default-input-method "russian-computer")
 '(icomplete-mode t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(package-archives (quote (("melpa" . "http://melpa.milkbox.net/packages/") ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 107 :width normal :foundry "outline" :family "Consolas")))))
(fset 'yes-or-no-p 'y-or-n-p)
(eval-after-load 'color-theme-solarized-autoloads '(load-theme 'solarized-dark t))
(eval-after-load 'yasnippet-autoloads '(yas-global-mode t))
(eval-after-load 'rinari-autoloads '(require 'rinari))
(global-set-key "\C-x\C-b" 'ibuffer-list-buffers)
(global-set-key "\C-cm" 'magit-status)
(global-set-key "\C-cn" 'svn-status)
;; TODO: rake, cap
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("[Gg]emfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("[Rr]akefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("[Pp]rocfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("[Cc]apfile\\'" . ruby-mode))
(fset 'perl-mode 'cperl-mode)
(fset 'javascript-mode 'js2-mode)
(fset 'html-mode 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
