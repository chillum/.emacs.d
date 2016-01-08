;;; init.el -- Emacs configuration
;;; Commentary:
;;;  https://github.com/chillum/.emacs.d
;;; Code:

;; Set fonts and window geometry. Customize this with:
;;  M-x customize-face default
;;  M-x customize-variable default-frame-alist
(set-face-font 'default
               (concat
                (if (eq system-type 'darwin)
                    "Menlo"
                  "DejaVu Sans Mono")
                "-"
                (if (>= (display-pixel-height) 1050)
                    "15"
                  (if (>= (display-pixel-height) 800)
                      "13"
                    "10.5"))))
(setq default-frame-alist
      (if (>= (display-pixel-height) 1050)
          '((height . 53)
            (width . 120)
            (top . 50)
            (left . 380))
      (if (>= (display-pixel-height) 800)
          '((height . 50)
            (width . 120)
            (top . 0)
            (left . 190))
        '((height . 44)
          (width . 120)
          (top . 3)
          (left . 220)))))

(if (fboundp 'tool-bar-mode) ;; Customize does not check if this exists
    (tool-bar-mode 0))

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
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(jedi:complete-on-dot t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-include-node-externs t)
 '(js2-skip-preprocessor-directives t)
 '(magit-push-always-verify nil)
 '(magit-revert-buffers t)
 '(make-backup-files nil)
 '(markdown-command "redcarpet --parse fenced_code_blocks")
 '(ns-alternate-modifier (quote none))
 '(ns-command-modifier (quote meta))
 '(package-archives
   (quote
    (("melpa" . "http://melpa.milkbox.net/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (yaml-mode web-mode vagrant stylus-mode scss-mode puppet-mode projectile php-mode nginx-mode markdown-mode magit less-css-mode json-mode js2-mode jedi jade-mode go-mode flycheck flx-ido fish-mode emmet-mode dockerfile-mode csv-mode color-theme-sanityinc-solarized apache-mode ansible-doc ag)))
 '(projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))
 '(ring-bell-function (quote ignore))
 '(sentence-end-double-space nil)
 '(server-mode t)
 '(sh-shell-file "/bin/bash")
 '(show-paren-mode t)
 '(sql-product (quote postgres))
 '(tab-width 4)
 '(tramp-default-method "ssh")
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-closing nil)
 '(web-mode-enable-auto-quoting nil)
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

;; Install ELPA packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg
         '(color-theme-sanityinc-solarized magit
          flycheck projectile ag flx-ido auto-complete
          markdown-mode yaml-mode json-mode csv-mode
          go-mode js2-mode jedi php-mode web-mode jade-mode
          emmet-mode stylus-mode less-css-mode scss-mode
          nginx-mode apache-mode fish-mode
          ansible-doc puppet-mode dockerfile-mode vagrant))
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
(projectile-register-project-type 'ruby-rake '("Rakefile") "rake" "rake test")
(projectile-register-project-type 'make '("Makefile") "make" "make test")
(global-flycheck-mode t)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(if (executable-find "python")
    (add-hook 'python-mode-hook 'jedi:setup))
(if (executable-find "aspell")
    (add-hook 'text-mode-hook 'flyspell-mode))

(global-set-key "\C-x\C-b" 'ibuffer-list-buffers)
(global-set-key "\C-cm" 'magit-status)
(global-set-key "\C-cp" 'flycheck-list-errors)

(add-to-list 'auto-mode-alist '("\\.\\(?:service\\|socket\\|target\\)\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js[hl]intrc\\'" . json-mode))

(add-hook 'yaml-mode-hook 'ansible-doc-mode)

(fset 'html-mode 'web-mode)
(add-to-list 'auto-mode-alist '("\\.[jgla]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\(?:r\\|dj\\)html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.e\\(?:rb\\|js\\)\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.go\\(?:tmpl\\|html\\)\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.plist\\'" . web-mode))

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

(provide 'init)
;;; init.el ends here
