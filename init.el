;;; init.el -- Emacs configuration
;;; Commentary:
;;;  https://github.com/chillum/.emacs.d
;;; Code:

;; Set fonts and window geometry. Customize this with:
;;  M-x customize-face default
;;  M-x customize-variable default-frame-alist
;;
(set-face-font 'default (if (eq system-type 'darwin)
                            "Menlo-15"
                          "DejaVu Sans Mono-10.5"))
(setq default-frame-alist
      (if (>= (display-pixel-height) 1050)
          '((height . 53)
            (width . 120)
            (top . 50)
            (left . 380))
        '((height . 44)
          (width . 120)
          (top . 3)
          (left . 220))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 1)
 '(auto-save-default nil)
 '(auto-save-list-file-prefix nil)
 '(blink-cursor-mode nil)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "stroustrup"))))
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(default-input-method "russian-computer")
 '(dired-auto-revert-buffer (quote dired-directory-changed-p))
 '(dired-listing-switches "-alh")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(eshell-hist-ignoredups t)
 '(flycheck-completion-system (quote ido))
 '(flyspell-mode-line-string " Spell")
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
 '(magit-auto-revert-mode nil)
 '(magit-auto-revert-mode-lighter nil)
 '(make-backup-files nil)
 '(markdown-command "redcarpet")
 '(package-archives
   (quote
    (("melpa" . "http://melpa.milkbox.net/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))
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
 '(popup-face ((t (:background "#073642" :foreground "#839496"))))
 '(popup-menu-mouse-face ((t (:background "#eee8d5" :foreground "#93a1a1"))))
 '(popup-menu-selection-face ((t (:background "#eee8d5" :foreground "#657b83"))))
 '(popup-tip-face ((t (:background "#073642" :foreground "#93a1a1")))))
(setq apache-indent-level 2)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg
         '(color-theme-sanityinc-solarized magit psvn
          flycheck projectile ag flx-ido auto-complete
          markdown-mode yaml-mode json-mode csv-mode
          go-mode erlang js2-mode jedi php-mode
          web-mode jade-mode stylus-mode
          nginx-mode apache-mode
          puppet-mode dockerfile-mode vagrant))
  (unless (package-installed-p pkg)
    (package-install pkg)))
(load-theme
 (if window-system
     'sanityinc-solarized-dark
   'sanityinc-solarized-light)
 t)
(fset 'yes-or-no-p 'y-or-n-p)
(fset 'perl-mode 'cperl-mode)
(projectile-global-mode)
(global-flycheck-mode t)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(if (executable-find "python")
    (add-hook 'python-mode-hook 'jedi:setup))
(if (executable-find "aspell")
    (add-hook 'text-mode-hook 'flyspell-mode))
(global-set-key "\C-x\C-b" 'ibuffer-list-buffers)
(global-set-key "\C-ca" 'calculator)
(global-set-key "\C-cd" 'calendar)
(global-set-key "\C-cm" 'magit-status)
(global-set-key "\C-cn" 'svn-status)
(global-set-key "\C-cp" 'flycheck-list-errors)
(add-to-list 'auto-mode-alist '("\\.\\(?:service\\|socket\\|target\\)\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pyw\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.rbw\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(?:Proc\\|Berks\\)file\\'" . ruby-mode))
(fset 'javascript-mode 'js2-mode)
(fset 'html-mode 'web-mode)
(fset 'xml-mode 'web-mode)
(add-to-list 'auto-mode-alist '("\\.[jgla]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\(?:r\\|dj\\)html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.e\\(?:rb\\|js\\)\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.go\\(?:tmpl\\|html\\)\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.plist\\'" . web-mode))
(add-to-list 'auto-mode-alist '("nginx\\.conf\\'" . nginx-mode))
(add-to-list 'auto-mode-alist '("/nginx/.+\\.conf\\'" . nginx-mode))
(add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

(provide 'init)
;;; init.el ends here
