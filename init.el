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
                (if (>= (display-pixel-height) 800)
                    "12"
                  "10.5")))
(setq default-frame-alist
      (if (>= (display-pixel-height) 800)
          '((height . 53)
            (width . 150)
            (top . 0)
            (left . 125))
        '((height . 44)
          (width . 120)
          (top . 3)
          (left . 220))))

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
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "stroustrup"))))
 '(column-number-mode t)
 '(company-idle-delay 0.25)
 '(company-minimum-prefix-length 1)
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(dash-at-point-mode-alist
   (quote
    ((css-mode . "css,html")
     (less-css-mode . "less,css,html")
     (go-mode . "godoc,go")
     (web-mode . "html,angularjs,css,javascript")
     (html-mode . "html,angularjs,css,javascript")
     (jade-mode . "html,angularjs,css,javascript")
     (js2-mode . "javascript,nodejs,angularjs")
     (apache-mode . "apache")
     (nginx-mode . "nginx")
     (puppet-mode . "puppet")
     (python-mode . "python,tornado,django")
     (ruby-mode . "ruby,rubygems")
     (sh-mode . "bash,manpages")
     (sql-mode . "psql")
     (yaml-mode . "ansible"))))
 '(default-input-method "russian-computer")
 '(dired-auto-revert-buffer (quote dired-directory-changed-p))
 '(dired-listing-switches "-alh")
 '(dired-omit-extensions (quote (".swp")))
 '(dired-omit-files
   "^\\.DS_Store$\\|^Thumbs\\.db$\\|^\\.CFUserTextEncoding$\\|^\\.localized$\\|^\\.git$\\|^\\.vagrant$\\|^\\.#")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(eshell-hist-ignoredups t)
 '(exec-path-from-shell-variables (quote ("PATH" "GOPATH")))
 '(fill-column 120)
 '(flycheck-completion-system (quote ido))
 '(flyspell-mode-line-string " Spell")
 '(global-auto-revert-mode t)
 '(global-visual-line-mode t)
 '(icomplete-mode t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
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
   (quote
    (("melpa" . "http://melpa.milkbox.net/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (move-text inf-ruby yasnippet ansible company-go company-ansible company exec-path-from-shell vagrant dockerfile-mode puppet-mode apache-mode nginx-mode less-css-mode emmet-mode jade-mode php-mode js2-mode go-mode csv-mode json-mode yaml-mode markdown-mode flx-ido ag projectile flycheck dash-at-point magit color-theme-sanityinc-solarized)))
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
 '(ansible::task-label-face ((t nil))))
;; Base configuration
(fset 'yes-or-no-p 'y-or-n-p)
(fset 'perl-mode 'cperl-mode)

(setq apache-indent-level 2
      dired-omit-mode t)

(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

(if (executable-find "aspell")
    (add-hook 'text-mode-hook 'flyspell-mode))

(global-set-key "\C-x\C-b" 'ibuffer-list-buffers)

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
  (global-company-mode t)
  (yas-global-mode t)

  (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
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
