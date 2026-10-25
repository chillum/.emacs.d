;;; init.el -- Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;;  https://github.com/chillum/.emacs.d
;;; Code:

;; Set fonts and window geometry. Customize this with:
;;  M-x customize-face default
(set-face-font 'default
               (concat
                (if (eq system-type 'darwin)
                    "Menlo"
                  "DejaVu Sans Mono")
                "-"
                "12"))

(if (fboundp 'tool-bar-mode) ;; Customize does not check if this exists
    (tool-bar-mode 0))

(if (eq system-type 'darwin)
    (setq trash-directory "~/.Trash"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansible::vault-password-file "~/Documents/.vault")
 '(auto-save-default nil)
 '(auto-save-list-file-prefix nil)
 '(blink-cursor-mode nil)
 '(c-default-style
   '((java-mode . "java") (awk-mode . "awk") (other . "stroustrup")))
 '(column-number-mode t)
 '(company-idle-delay 0.25)
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("48d34b6afe72407ca494387c8bea495bb2deee96bd88516f302db1f11e1810a1" default))
 '(dash-at-point-mode-alist
   '((css-mode . "css,html") (less-css-mode . "css,html") (web-mode . "html,css,javascript")
     (html-mode . "html,css,javascript") (haml-mode . "html,css,javascript") (js2-mode . "javascript,nodejs")
     (apache-mode . "apache") (nginx-mode . "nginx") (puppet-mode . "puppet") (go-mode . "godoc,go")
     (python-mode . "python,tornado,django") (ruby-mode . "ruby,rubygems") (sh-mode . "bash,manpages")
     (sql-mode . "psql") (terraform-mode . "terraform")))
 '(default-frame-alist '((height . 54) (width . 170) (top . 160) (left . 475)))
 '(default-input-method "russian-computer")
 '(dired-auto-revert-buffer 'dired-directory-changed-p)
 '(dired-free-space nil)
 '(dired-listing-switches "-alh")
 '(dired-omit-extensions '(".swp"))
 '(dired-omit-files
   "^\\.DS_Store$\\|^Thumbs\\.db$\\|^\\.CFUserTextEncoding$\\|^\\.localized$\\|^\\.git$\\|^\\.terraform$\\|^\\.#")
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(eshell-hist-ignoredups t)
 '(exec-path-from-shell-variables '("PATH" "GOPATH"))
 '(fill-column 120)
 '(flycheck-completion-system 'ido)
 '(flyspell-mode-line-string " Spell")
 '(global-auto-revert-mode t)
 '(global-visual-line-mode t)
 '(icomplete-mode t)
 '(ido-everywhere t)
 '(ido-mode 'both nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/ido.last")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(jedi:complete-on-dot t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-include-node-externs t)
 '(js2-skip-preprocessor-directives t)
 '(large-file-warning-threshold nil)
 '(magit-auto-revert-mode nil)
 '(magit-push-always-verify nil)
 '(magit-revert-buffers t t)
 '(make-backup-files nil)
 '(markdown-command "redcarpet --parse fenced_code_blocks")
 '(package-archives
   '(("melpa" . "https://melpa.org/packages/") ("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
 '(package-selected-packages
   '(ansible apache-mode color-theme-sanityinc-solarized company company-ansible company-go company-terraform csv-mode
             dash-at-point dockerfile-mode emmet-mode exec-path-from-shell flx-ido flycheck go-mode haml-mode inf-ruby
             js2-mode json-mode magit markdown-mode move-text nginx-mode php-mode projectile puppet-mode rg
             terraform-mode whitespace-cleanup-mode yaml-mode yasnippet))
 '(projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
 '(require-final-newline 'ask)
 '(ring-bell-function 'ignore)
 '(sentence-end-double-space nil)
 '(server-mode t)
 '(sh-shell-file "/bin/bash")
 '(show-paren-mode t)
 '(sql-product 'postgres)
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
 '(ansible::task-label-face ((t nil))))
;; Base configuration
(fset 'yes-or-no-p 'y-or-n-p)
(fset 'perl-mode 'cperl-mode)

(setq apache-indent-level 2
      dired-omit-mode t)

(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

(if (executable-find "aspell")
    (add-hook 'text-mode-hook 'flyspell-mode))

(global-set-key "\C-x\C-b" 'ibuffer)

(global-set-key (kbd "s-w") 'kill-this-buffer)
(global-set-key (kbd "s-o") 'find-file)

(add-to-list 'auto-mode-alist '("\\.\\(?:service\\|socket\\|target\\|timer\\)\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.plist\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . java-mode))
(add-to-list 'auto-mode-alist '("Jenkinsfile" . java-mode))

;; ELPA packages
(when (fboundp 'package-initialize)
  (package-initialize)
  (unless package-alist
    (package-refresh-contents))
  (package-install-selected-packages)

  (if window-system (load-theme 'sanityinc-solarized-dark) t)
  (move-text-default-bindings)

  (if (eq window-system 'ns)
      (exec-path-from-shell-initialize))

  (projectile-mode t)
  (projectile-register-project-type 'ruby-rake '("Rakefile") :compile "rake" :test "rake test")
  (projectile-register-project-type 'make '("Makefile") :compile "make" :test "make test")
  (global-flycheck-mode t)
  (yas-global-mode t)

  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt)

  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)

  (global-set-key "\C-cm" 'magit-status)
  (global-set-key "\C-cp" 'flycheck-list-errors)
  (global-set-key "\C-cd" 'dash-at-point)

  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.js[hl]intrc\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

  (eval-after-load 'magit-remote
    `(define-key magit-mode-map "f" 'magit-pull-and-fetch-popup))

(provide 'init)
;;; init.el ends here
