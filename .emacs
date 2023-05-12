;;;; .Emacs[.el] -- My Emacs configuration
;-*-Emacs-Lisp-*-

;;;; Commentary:
;;
; This is my Emacs confiration file.
; Any non-code config. pieces will be explained in this commentary.
; 
;;; Remapping ctrl to caps lock
; https://www.emacswiki.org/emacs/MovingTheCtrlKey
; https://deskthority.net/wiki/Category:Keyboards_with_Unix_layout


;;;; Code:

;;;; Configure MEPLA:
;;     - https://melpa.org/#/getting-started
;;     - https://emacs.stackexchange.com/a/10501/25429; 
(package-initialize)
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
           '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
           '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives 
           '("gnu" . "http://elpa.gnu.org/packages/") t)
  ;; (package-refresh-contents)
)

(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-j")
                            (quote eval-print-last-sexp))))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

;;; Colours in compilation mode
;; https://emacs.stackexchange.com/a/72580/25429
;; https://emacs.stackexchange.com/a/73552/25429
;; (setq compilation-environment '("TERM=ansi-color-apply"))
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq compilation-max-output-line-length nil)

;;;; set shell to bash
(setenv "SHELL" "/usr/local/bin/bash")
(setq explicit-shell-file-name "/usr/local/bin/bash")

;;; Use Package
;; https://emacs.stackexchange.com/a/50603/25429
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;;;;; Emacs Theme

;;; Dracula Theme
;; (use-package dracula-theme
;;   :init
;;   (load-theme 'dracula t))

;;; Atom One Dark Theme
(use-package atom-one-dark-theme
  :ensure t
  :defer t
  
  :init
  (load-theme 'atom-one-dark t))

;; Power line
(use-package smart-mode-line-atom-one-dark-theme)
(use-package smart-mode-line
  :ensure t
  :defer t
  
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'atom-one-dark)
  
  :config
  (sml/setup)
  (sml/apply-theme 'atom-one-dark))

;;;; Represent whitespace as dots
;; (set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")
(setq whitespace-style '(space-mark))
(setq whitespace-display-mappings '((space-mark 32 [183] [46])))

;;;; suppress startup screen
(setq inhibit-startup-screen t
	inhibit-splash-screen t
	inhibit-startup-echo-area-message t)

;;;; hide tool-bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;;;; make window fullscreen (-fs | --fullscreen)
;; must be after the hide toolbar section
;; https://superuser.com/questions/1076443/
(toggle-frame-maximized)
;; https://stackoverflow.com/questions/2151449/
;; (x-display-pixel-width)
;; (x-display-pixel-height)

;;;; adjust scroll mode
;; scroll one line at a time (less "jumpy" than defaults)    
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Highlight matching parenthesis!
(show-paren-mode 1)
(setq show-paren-delay 0)

;;; Paredit
(use-package paredit)

(defun rc/turn-on-paredit ()
  (interactive)
  (paredit-mode 1))

(add-hook 'emacs-lisp-mode-hook  'rc/turn-on-paredit)
(add-hook 'lisp-mode-hook        'rc/turn-on-paredit)
(add-hook 'common-lisp-mode-hook 'rc/turn-on-paredit)

;;;; Disable arrow keys to force yourself to use default Emacs keybindings for navigation
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

;;;; allow tab key
(global-set-key (kbd "TAB") 'self-insert-command)

;;;; Set tab key to four spaces
(setq-default indent-tabs-mode t)
(setq-default tab-width 4) ;; Assuming you want your tabs to be four spaces wide
(defvaralias 'c-basic-offset 'tab-width)

;;;; Disable automatic indentations
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;;;; Allow commenting/uncommenting code
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "M-/") 'toggle-comment-on-line)

;;;; instead of emacs creating a *~ file in the current directory, create backup file elsewhere
(setq backup-directory-alist `(("." . "~/.saves")))

;;;; do something with #file-being-edited#
(use-package no-littering)
;; (require 'no-littering)

;;; ESS
;; namely for R syntax highlighting
(use-package ess)

;;;; find matching parenthesis
(global-set-key "%" 'match-paren)
          
          (defun match-paren (arg)
            "Go to the matching paren if on a paren; otherwise insert %."
            (interactive "p")
            (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
                  ((looking-at "\\s)") (forward-char 1) (backward-list 1))
                  (t (self-insert-command (or arg 1)))))

;;;; Add line numbers
(global-display-line-numbers-mode)
;; Preset `nlinum-format' for minimum width.
;; (defun my-nlinum-mode-hook ()
  ;; (when nlinum-mode
    ;; (setq-local nlinum-format
                ;; (concat "%" (number-to-string
                             ;; Guesstimate number of buffer lines.
                             ;; (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                        ;; "d"))))
;; (add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook)

;;;; add julia support
;; https://github.com/JuliaEditorSupport/julia-emacs
(use-package julia-mode
  :ensure t
  :interpreter ("julia" . julia-mode))

;;;; Formatting advise for Julia code
;; https://codeberg.org/FelipeLema/julia-formatter.el
(use-package julia-formatter)
(add-hook 'julia-mode-hook #'julia-formatter-mode)
;; Load Julia Formatter server in the background after startup
(add-hook 'after-init-hook #'julia-formatter--ensure-server)

;; Change default compilation for Julia
(add-hook 'julia-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "julia --project %s" 
					(file-name-nondirectory buffer-file-name)))))

;;;; add rust support
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode)
;; Change default compilation for Rust
(require 'compile)
(add-hook 'rust-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "rustc %s && ./%s" 
					(file-name-nondirectory buffer-file-name)
					(file-name-base buffer-file-name)))))

;;;; add python compilation
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "python3 %s" (file-name-nondirectory buffer-file-name)))))

;;;; add zig support
(unless (version< emacs-version "24")
  (add-to-list 'load-path "~/.emacs.d/zig-mode/")
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; (setq-default left-fringe-width nil)
;; (setq-default indicate-empty-lines t)
;; (setq-default indent-tabs-mode nil)

;;;; TODO: highlighting
;; https://github.com/tarsius/hl-todo
;; https://www.reddit.com/r/emacs/comments/f8tox6/
;; (use-package hl-todo)
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))
		;; '(("TODO"		 . "#FF0000")
		   ;; ("FIXME"		 . "#FF0000")
		   ;; ("HACK"		 . "#A020F0")
		   ;; ("REVIEW"	 . "#FF4500")
		   ;; ("NOTE"		 . "#1E90FF")
		   ;; ("DEPRECATED" . "#FF0000")));;)
;; (setq hl-todo-keyword-faces
      ;; '(("TODO"   . "#FF0000")
        ;; ("FIXME"  . "#FF0000")
        ;; ("DEBUG"  . "#A020F0")
        ;; ("GOTCHA" . "#FF4500")
        ;; ("STUB"   . "#1E90FF")))


;; enable continuous scrolling
(setq doc-view-continuous t)

;; coloured dots for whitespace
(setq whitespace-style '(space-mark))
(setq whitespace-display-mappings '((space-mark 32 [183] [46])))

;; Moves lines of text
;; by default uses M-up and M-down
(use-package move-text)
(move-text-default-bindings)

;; nim mode
(use-package move-text)

;; Making regex a little bit easier
(use-package re-builder)
;; (setq reb-re-syntax 'string) ;; switch to `string`; there's little reason tu use `read`

;; Relative line numbers
(display-line-numbers-mode)
;; (setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)

;;;; Multiple cursors
(use-package multiple-cursors)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;;;; BEGIN JULIA LSP MODE
;; 
;; See resources:
;;   - Language server: https://github.com/julia-vscode/LanguageServer.jl
;;   - Emacs package/installation instructions: https://github.com/gdkrmr/lsp-julia
;; 
;; Set Up:
;; $ cd ~/.emacs.d && git clone https://github.com/gdkrmr/lsp-julia && cd -
;; The following set-up is no longer needed as we are now cloning the directory:
;; $ mkdir ~/.julia/languageserver
;; $ julia --project=~/.julia/languageserver -e '
;;     using Pkg;
;;     Pkg.add.(("LanguageServer", "PackageCompiler"));
;;     using PackageCompiler;
;;     create_sysimage(:LanguageServer, sysimage_path="$(homedir())/.julia/languageserver/languageserver.so");
;;  '

(use-package lsp-julia
  ;; Manually clone the lsp-julia package from https://github.com/gdkrmr/lsp-julia
  :load-path "~/.emacs.d/lsp-julia"
  :ensure t
  
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.8")
  (add-hook 'julia-mode-hook #'lsp))

(setenv "JULIA_NUM_THREADS" "auto")
(setq lsp-julia-package-dir nil)
;; (setq lsp-julia-flags `(("-J" . "~/.julia/languageserver/languageserver.so")))

;;;; END JULIA LSP MODE

;;;; BEGIN RUST LSP MODE
;;;; Rust IDE-like development environment
;;
;; Resources:
;;   - https://robert.kra.hn/posts/rust-emacs-setup/
;;   - https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/
;;   - https://github.com/brotzeit/rustic
;;
;; Set Up:
;; $ rustup component add rust-src
;; $ rustup component add rust-analyzer
;; 
;; The remaining packages should be installed via use-package
;; For some reason I also had to install zsh for this to work

;;; Rustic requires `rustic` and `use-package`
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;;; lsp-mode and lsp-ui-mode
(use-package lsp-mode
  :ensure

  :init
  ;; Use flycheck instead of flymake (better lsp-ui integration)
  (setq lsp-prefer-flymake nil)
  
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;; Prevent long documentation showing up in the echo area from messing up the
  ;; window configuration -> only show the first line
  (defun ff/lsp-eldoc-advice (orig-fun &rest args)
    (let ((msg (car args)))
      (if msg
          (funcall orig-fun (->> msg (s-trim-left)
                                     (s-split "\n")
                                     (first))))))
  (advice-add 'lsp--eldoc-message :around #'ff/lsp-eldoc-advice)

  ;; Avoid questions about restarting the LSP server when quitting emacs
  (defun ff/lsp-disable-server-autorestart ()
    (setq lsp-restart nil))
  (add-hook 'kill-emacs-hook #'ff/lsp-disable-server-autorestart))

(use-package lsp-ui
  :ensure t
  
  :init
  (setq lsp-ui-doc-enable nil)
  
  :commands lsp-ui-mode

  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]  #'lsp-ui-peek-find-references)
  
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;;; Code completion (autocomplete)
(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;;; Inline errors
(use-package flycheck :ensure)

;;; Inline type hints
(setq lsp-rust-analyzer-server-display-inlay-hints t)

;;;; END RUST LSP MODE

;;;; BEGIN DOCKER LSP MODE
(use-package lsp-docker)

(defvar lsp-docker-client-packages 
  '(lsp-rust lsp-julia))


;;;; END DOCKER LSP MODE

;;; Properly indent CSV mode
(add-hook 'csv-mode-hook
          (lambda ()
            (define-key csv-mode-map (kbd "C-c C-M-a")
              (defun csv-align-visible (&optional arg)
                "Align visible fields"
                (interactive "P")
                (csv-align-fields nil (window-start) (window-end))
                )
              )
            )
          )

;;; Indent in Python mode
;; TODO

;;; Git Commit Mode
;;;; Emacs Wiki: Git Commit Mode: https://www.emacswiki.org/emacs/GitCommitMode
;;;; git-commit-mode isn’t used when committing from the command-line: https://magit.vc/manual/magit/git_002dcommit_002dmode-isn_0027t-used-when-committing-from-the-command_002dline.html
(use-package git-commit)
;; (server-mode)
(use-package server
  :config (or (server-running-p) (server-mode)))

;;; Whitespace mode
;; https://www.emacswiki.org/emacs/WhiteSpace
;; (rc/require-theme 'gruber-darker)

(use-package whitespace)
(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

;; (set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")

(add-hook 'julia-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'rust-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode 'rc/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'haskell-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'rc/set-up-whitespace-handling)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values '((buffer-file-coding-system . utf-8-unix)))
 '(package-selected-packages
   '(gdscript-mode spell-fu ebib writeroom-mode writeroom olivetti eglot-jl julia-repl impatient-mode lsp-julia yasnippet yaml-mode use-package typescript-mode smart-mode-line-atom-one-dark-theme rustic paredit no-littering nlinum nim-mode multiple-cursors move-text magit lua-mode lsp-ui julia-mode hl-todo haskell-mode go-mode git-commit-insert-issue ess dracula-theme csv-mode company atom-one-dark-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Enable Markdown conversion
;;; https://stackoverflow.com/a/36189456/12069968
(use-package impatient-mode)
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
    (current-buffer)))

;; Writing!
;;; Writing modes
(use-package olivetti)
(use-package writeroom-mode)

;;; Research tools
(use-package ebib)
