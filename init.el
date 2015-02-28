(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 1)
 '(auto-save-list-file-prefix nil)
 '(blink-cursor-mode nil)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "stroustrup"))))
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(default-frame-alist
    (quote
     ((height . 53)
      (width . 120)
      (top . 50)
      (left . 380))))
 '(default-input-method "russian-computer")
 '(dired-auto-revert-buffer (quote dired-directory-changed-p))
 '(dired-listing-switches "-alh")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(eshell-hist-ignoredups t)
 '(flycheck-completion-system (quote ido))
 '(flyspell-mode-line-string " Spell")
 '(global-rinari-mode t)
 '(icomplete-mode t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inf-ruby-default-implementation "pry")
 '(inhibit-startup-screen t)
 '(jedi:complete-on-dot t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-include-node-externs t)
 '(magit-auto-revert-mode-lighter nil)
 '(make-backup-files nil)
 '(markdown-command "redcarpet")
 '(package-archives
   (quote
    (("melpa" . "http://melpa.milkbox.net/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(server-mode t)
 '(show-paren-mode t)
 '(sql-product (quote postgres))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 150 :family "Menlo"))))
 '(popup-face ((t (:background "#073642" :foreground "#839496"))))
 '(popup-menu-mouse-face ((t (:background "#eee8d5" :foreground "#93a1a1"))))
 '(popup-menu-selection-face ((t (:background "#eee8d5" :foreground "#657b83"))))
 '(popup-tip-face ((t (:background "#073642" :foreground "#93a1a1")))))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg '(color-theme-sanityinc-solarized
               magit psvn ag flycheck
               markdown-mode yaml-mode csv-mode
               go-mode jedi rinari js2-mode php-mode
               web-mode jade-mode stylus-mode
               nginx-mode apache-mode
               puppet-mode dockerfile-mode vagrant))
  (unless (package-installed-p pkg)
    (package-install pkg)))
(load-theme 'sanityinc-solarized-dark t)
(fset 'yes-or-no-p 'y-or-n-p)
(fset 'perl-mode 'cperl-mode)
(global-flycheck-mode t)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'text-mode-hook 'flyspell-mode)
(global-set-key "\C-x\C-b" 'ibuffer-list-buffers)
(global-set-key "\C-ca" 'calculator)
(global-set-key "\C-cd" 'calendar)
(global-set-key "\C-cg" 'ag)
(global-set-key "\C-cm" 'magit-status)
(global-set-key "\C-cn" 'svn-status)
(global-set-key "\C-cr" 'rinari-rake)
(global-set-key "\C-cc" 'rinari-cap)
(global-set-key "\C-cp" 'flycheck-list-errors)
(add-to-list 'auto-mode-alist '("\\.\\(?:service\\|socket\\|target\\)\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.pyw\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.rbw\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(?:Proc\\|Berks\\)file\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(fset 'html-mode 'web-mode)
(fset 'xml-mode 'web-mode)
(add-to-list 'auto-mode-alist '("\\.[jgla]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\(?:r\\|dj\\)html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.e\\(?:rb\\|js\\)\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.go\\(?:tmpl\\|html\\)\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("nginx\\.conf\\'" . nginx-mode))
(add-to-list 'auto-mode-alist '("/nginx/.+\\.conf\\'" . nginx-mode))
(add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))
