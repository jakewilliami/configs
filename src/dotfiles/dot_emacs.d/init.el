;;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: Jake W. Ireland
;; Package-Requires: ((emacs "29.1"))  ; Required by use-package

;;;; Commentary:
;; This is my Emacs confiration file.
;; Any non-code config. pieces will be explained in this commentary.
;;
;; For verbose config file (pre mid-October, 2023), see:
;;   https://github.com/jakewilliami/configs/blob/7f75f095/src/.emacs
;;
;; Remapping Caps Lock to Control:
;;   https://www.emacswiki.org/emacs/MovingTheCtrlKey
;;   https://deskthority.net/wiki/Category:Keyboards_with_Unix_layout

;;;; Code:



;;;; High-Level Configurations:
;;
;; This is the most basic, high-level configuration that needs to be
;; instantiated before all else; e.g. specification of package repositories,
;; encoding, shell, etc.

;;; Configure Elpaca package manager
;;   https://github.com/jakewilliami/configs/issues/7
;;
;; This will also install Use Package
;;   https://emacs.stackexchange.com/a/50603
;;
;; See also:
;;   Quelpa: https://github.com/quelpa/quelpa
;;   Quelpa Use Package: https://github.com/quelpa/quelpa-use-package
;;   Straight.el: https://www.github.com/radian-software/straight.el
;;   MELPA: https://melpa.org/#/getting-started, https://emacs.stackexchange.com/a/10510
;;   Previous: https://github.com/jakewilliami/configs/blob/c106b08b/src/.emacs#L26-L56
;;
;; We also load package versions from lock file:
;;   reddit.com/r/emacs/comments/1ilcodh/
;;   github.com/progfolio/elpaca/issues/151#issuecomment-2629560507
;;   github.com/progfolio/elpaca/issues/447
;;   github.com/progfolio/elpaca/blob/f0bbcec4/doc/manual.md#lock-files
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(setq elpaca-lock-file "~/.emacs.d/elpaca.lock.eld")
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;; Encoding
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;;; Set shell to Bash
(setenv "SHELL" "/usr/local/bin/bash")
(setq explicit-shell-file-name "/usr/local/bin/bash")

;;; Set custom file so that the init.el file does not contain generated code
;; (setq custom-file "~/.emacs.d/custom.el")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;;; Do not use --dired option with ls on macOS
;;   https://stackoverflow.com/a/42038174
;;   https://stackoverflow.com/a/56096775
;;
;; I also need to account for BSD.
;;
;; Requires GNU core utilities
(when (memq system-type '(darwin berkeley-unix))
  (setq dired-use-ls-dired t
        insert-directory-program "gls"
        dired-listing-switches "-aBhl --group-directories-first"))



;;;; Aesthetics:
;;
;; Thematic/aesthetic/stylistic configurations.
;;
;; This section also contains code on how Emacs is opened;
;; i.e., in full-screen, with no startup screen.

;;; Theme
;; (use-package dracula-theme
;;   :init
;;   (load-theme 'dracula t))
(use-package atom-one-dark-theme
  :ensure
  :defer t
  :init
  (load-theme 'atom-one-dark t))

;; Power line
(use-package smart-mode-line
  :ensure
  :defer t

  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'atom-one-dark)

  :config
  (sml/setup)
  (sml/apply-theme 'atom-one-dark))
(use-package smart-mode-line-atom-one-dark-theme
  :ensure
  :defer t
  :after smart-mode-line)

;;; Better dired mode:
;;   https://emacs.stackexchange.com/a/33553
;;   https://emacs.stackexchange.com/a/64160
;;
;; Don't need to do for macOS, as this is handled separately above
(unless (string= system-type "darwin")
  (setq dired-listing-switches "-alFh"))

;;; Pages
;;   https://github.com/purcell/page-break-lines
;;   https://www.emacswiki.org/emacs/PageBreaks
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Pages.html
(use-package page-break-lines
  :ensure
  :defer t
  :config
  (global-page-break-lines-mode))

;;;; Represent whitespace as dots
(setq whitespace-style '(space-mark face tabs trailing))
(setq whitespace-display-mappings '((space-mark 32 [183] [46])))

;;; Whitespace mode
;;   https://www.emacswiki.org/emacs/WhiteSpace

(require 'whitespace)
(defun set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  ;; Remove trailing whitespace where possible
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  ;; Highlight tab characters
  ;;   https://stackoverflow.com/a/22011036/
  ;;   https://stackoverflow.com/a/79727953/
  ;;   https://github.com/jakewilliami/configs/issues/15
  ;; And modified colour to match nice red:
  ;;   https://github.com/jakewilliami/tex-macros/blob/60446649/macros/colours.sty#L7
  (modify-face whitespace-tab nil "#ae4744")
  ;; Do the same for trailing whitespace
  (modify-face whitespace-trailing nil "#ae4744"))

;; Reset face for tabs (used for Go [ref KSMPV])
(defun set-up-whitespace-handling-allow-tabs ()
  (interactive)
  (face-spec-reset-face 'whitespace-tab))

;;; Suppress startup buffers
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t)

;;; Hide tool-bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;;; Line numbers
;; Show line numbers
(global-display-line-numbers-mode)

;; Use relative line numbers
(display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;;; Colours in compilation mode
;;   https://emacs.stackexchange.com/a/72580
;;   https://emacs.stackexchange.com/a/73552
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq compilation-max-output-line-length nil)

;;; Make window fullscreen
;; Alternatively, you can use the -fs/--fullscreen flag
;;
;; NOTE: must be after the hide toolbar section
;;   https://superuser.com/questions/1076443/
;;
;; NOTE: `x-display-pixel-*' does not work for me
;;   https://stackoverflow.com/questions/2151449/
(toggle-frame-maximized)



;;;; User Experience:
;;
;; This section contains configuration related to user experience.
;; For example, scrolling, parentheses matchng, tabs, electric modes, etc.

;;; Adjust scroll settings
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

;; Scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; Keyboard scroll one line at a time
(setq scroll-step 1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; enable continuous scrolling
(setq doc-view-continuous t)

;;; Highlight matching parenthesis!
(show-paren-mode 1)
(setq show-paren-delay 0)

;;; Tabs
;; Allow tab key
;; NOTE: this breaks julia-mode's tab completion for LaTeX substitution
;; (global-set-key (kbd "TAB") 'self-insert-command)

;; Set tab key to four spaces
;;   https://stackoverflow.com/a/9383214
(setq-default indent-tabs-mode nil)

;; Tabs are four spaces wide
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
;; (setq-default indent-tabs-mode t)

;;; Paredit
;;   https://www.emacswiki.org/emacs/ParEdit
(use-package paredit :ensure :defer t)
(defun turn-on-paredit ()
  (interactive)
  (paredit-mode 1))

;;; Allow commenting/uncommenting code
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "M-/") 'toggle-comment-on-line)

;;; No littering
;; Instead of Emacs creatiing a *~ file in the current dir,
;; create backup files elsewhere
(setq backup-directory-alist `(("." . "~/.saves")))

;; Also, do something with #file-being-edited#
(use-package no-littering :ensure :defer t :after compat)

;;; Note highlighting
;;   https://github.com/tarsius/hl-todo
;;   https://www.reddit.com/r/emacs/comments/f8tox6/
(use-package hl-todo
  :ensure
  :defer t
  :after compat
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

;;; Moves lines of text
;; By default uses M-up and M-down
(use-package move-text
  :ensure
  :defer t
  :init
  (move-text-default-bindings))

;;; Multiple cursors
(use-package multiple-cursors
  :ensure
  :defer t
  :bind (("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"         . mc/mark-all-like-this)
         ("C-\""        . mc/skip-to-next-like-this)
         ("C-:"         . mc/skip-to-previous-like-this)))

;;; Show number of matches in search
;;   https://emacs.stackexchange.com/a/978/
(use-package anzu
  :ensure
  :defer t
  :init
  (global-anzu-mode +1)
  (anzu-mode +1))

;;; Prefer horizontal split
;; Ref:
;;  https://emacs.stackexchange.com/a/40517
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Choosing-Window-Options.html
;;  https://emacs.stackexchange.com/a/17877
(defun split-window-sensibly-prefer-horizontal (&optional window)
  "Based on split-window-sensibly, but designed to prefer a horizontal split,
   i.e. windows tiled side-by-side."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically
             (with-selected-window window
               (split-window-below)))
        (and
         ;; If window is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
         (not (window-minibuffer-p window))
         (let ((split-width-threshold 0))
           (when (window-splittable-p window t)
             (with-selected-window window
               (split-window-right))))))))

(defun split-window-really-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (with-selected-window window (split-window-sensibly-prefer-horizontal window))
      (with-selected-window window (split-window-sensibly window)))))

(setq
 split-height-threshold 4
 split-width-threshold 40
 split-window-preferred-function 'split-window-really-sensibly)



;;;; Custom Functions:
;;
;; This section contains functions written by moi!
;;
;; Typically if I have to write them myself, they may be
;; a little niche, and not so useful for others.  Nonetheless,
;; they are convenient for me.

;;; Function to untabify whole file
;;   https://www.emacswiki.org/emacs/UntabifyUponSave
(defun untabify-file ()
  "Convert all tabs to spaces in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\t" nil t)
      (untabify (1- (point)) (point-max)))))

;; In some modes, untabify file before saving the file
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html
;;   https://www.emacswiki.org/emacs/UntabifyUponSave
;;
;; To use this:
;;   (add-hook 'something-mode-hook 'untabify-file-hook)
;;
;; We explicitly avoid doing this for Go files [ref KSMPV]
;;   https://stackoverflow.com/a/41450385
;;
;; For previous version, see:
;;   https://github.com/jakewilliami/configs/blob/4ecf4e9c/src/dotfiles/dot_emacs.d/init.el#L421-L429
(defun untabify-file-hook ()
  "Custom hook for untabifying the whole file."
  (unless (derived-mode-p 'go-mode)
    (add-hook 'before-save-hook 'untabify-file nil 'local)))

;;; Enable accented character input system (i.e., macons and umlauts)
;;   https://emacs.stackexchange.com/a/30697
;;
;; For latin-postfix as default, see:
;;   https://emacs.stackexchange.com/a/419
;;   https://github.com/jakewilliami/configs/commit/f11cd4e
(setq default-input-method "latin-postfix")

;; Also note that C-x 8 " e will insert ë;
;; As I use ë and ā most commonly, I have bound them for convenience
;;   - C-x 8 e   => ë
;;   - C-x 8 a => ā
;;
;; Ref:
;;   https://emacs.stackexchange.com/a/7294
(define-key 'iso-transl-ctl-x-8-map "e" [?ë])
(define-key 'iso-transl-ctl-x-8-map "a" [?ā])

;;; Shorten hash functions
;; Shorten a string (e.g. hash) to eight characters---useful for commit URLs
;; Provides the following key bindings:
;;   C-x M-f: shorten hash forward
;;   C-x M-b: shorten hash backward
;;   C-x M-x: shorten hash at word
;;
;; Ref:
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Point.html
;;   https://www.emacswiki.org/emacs/ThingAtPoint
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Region.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Near-Point.html
;;   https://www.gnu.org/software/emacs/manual/html_node/eintr/Point-and-mark.html
;;   https://emacs.stackexchange.com/a/79119
;;
;; NOTE: my implementation here:
;;   https://emacs.stackexchange.com/a/79121
;; is slightly wrong, as it does not start looking at the start of the next work;
;; rather, it starts at the current point, which may cut out characters that are
;; not in the word (e.g. spaces).
(defun shorten-hash (word-motion)
  "Shorten string (forward or backwards) to eight characters.
Particularly useful for shortening hashes.

Takes a word motion argument: either `forward' or `backward'."

  ;; Ensure the word motion argument is valid
  (unless (member word-motion '(forward backward))
    (error "Unknown word motion: %s" word-motion))

  ;; Get the word motion function from the motion argument
  (defun get-word-motion-func (word-motion)
    (cond
     ((eq word-motion 'forward)
      'forward-word)
     ((eq word-motion 'backward)
      'backward-word)
     (t
      (error "Unknown word motion: %s" word-motion))))

  ;; Get the word motion in the opposite direction
  (defun toggle-word-motion (word-motion-func)
    (cond
     ((eq word-motion-func 'forward-word)
      'backward-word)
     ((eq word-motion-func 'backward-word)
      'forward-word)
     (t
      (error "Unknown word motion function: %s" word-motion-func))))

  ;; Main function logic
  (let* ((word-motion-func (get-word-motion-func word-motion))
         (point-stop (progn (funcall word-motion-func) (point)))
         ;; Considered using forward-to-word:
         ;;    https://emacs.stackexchange.com/a/4274
         ;; But can just get the end of the word first and then
         ;; go backwards (or vice versa)
         (word-motion-rev-func (toggle-word-motion word-motion-func))
         (point-start (progn (funcall word-motion-rev-func) (point)))
         (word (buffer-substring  point-start point-stop)))
    (if (> (length word) 8)
        (progn (delete-region point-start point-stop)
               (insert (substring word 0 8)))
      (message "Cannot shorten word to eight characters"))))

(defun shorten-hash-forward ()
  (interactive)
  (shorten-hash 'forward))

(defun shorten-hash-backward ()
  (interactive)
  (shorten-hash 'backward))

(defun shorten-hash-at-word ()
  "Shorten word to eight characters"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (point-start (car bounds))
         (point-stop (cdr bounds))
         (word (buffer-substring point-start point-stop)))
    (if (> (length word) 8)
        (progn (delete-region point-start point-stop)
               (insert (substring word 0 8)))
      (message "Cannot shorten word to eight characters"))))

(global-set-key (kbd "C-x M-f") 'shorten-hash-forward)
(global-set-key (kbd "C-x M-b") 'shorten-hash-backward)
(global-set-key (kbd "C-x M-x") 'shorten-hash-at-word)

;;; Quick brackets!
;;
;; Provides the following key bindings:
;;   C-c p: insert parentheses
;;   C-c c: insert curly brackets
;;   C-c b: insert square brackets
(defun insert-brackets-and-move-between (open close)
  "Insert brackets and move cursor between them."
  (interactive)
  (insert (concat open close))
  (backward-char (length close)))

(defun insert-parens-and-move-between ()
  (interactive)
  (insert-brackets-and-move-between "(" ")"))

(defun insert-curly-and-move-between ()
  (interactive)
  (insert-brackets-and-move-between "{" "}"))

(defun insert-square-and-move-between ()
  (interactive)
  (insert-brackets-and-move-between "[" "]"))

(global-set-key (kbd "C-c p") 'insert-parens-and-move-between)
(global-set-key (kbd "C-c c") 'insert-curly-and-move-between)
(global-set-key (kbd "C-c b") 'insert-square-and-move-between)

;;; Duplicate current line
;;   C-,: duplicates current line
;;
;; Ref:
;;   - https://github.com/rexim/dotfiles/blob/f90578bc/.emacs.rc/misc-rc.el#L120-L132
(defun duplicate-current-line ()
  "Duplicate current line.  Retains column information."
  (interactive)
  (let ((column (- (point) (line-beginning-position)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'duplicate-current-line)



;;;; Writing/Research:
;;
;; I use Emacs for nearly everything, not just programming.  I have
;; discovered a few neat writing modes and research tools that are
;; useful.

;;; Writing modes
(use-package olivetti :ensure :defer t)
(use-package writeroom-mode :ensure :defer t)

;;; Research tools
(use-package ebib
  :ensure
  :defer t
  :after compat
  :config
  (ebib-set-dialect 'biblatex))

;;; LaTeX
;; Use C-c C-c or C-c C-a to compile LaTeX document
;; Note: do not reset to first page when recompiling document, you may have to change preview settings:
;;   - https://tex.stackexchange.com/q/106887/
;;   - https://superuser.com/q/847467/
(use-package auctex
  :ensure
  :defer t
  :config
  ;; https://www.gnu.org/software/auctex/manual/auctex/Quick-Start.html#Quick-Start
  ;; https://emacs.stackexchange.com/a/13870
  ;; Enable parse on load
  (setq TeX-parse-self t)
  ;; Enable parse on save
  (setq TeX-auto-save t))

;; Preview pane
(use-package latex-preview-pane
  :ensure
  :defer t
  :config
  ;; https://tex.stackexchange.com/a/190901
  ;; https://www.emacswiki.org/emacs/LaTeXPreviewPane
  (latex-preview-pane-enable))

;; https://github.com/tom-tan/auctex-latexmk
(use-package auctex-latexmk
  :ensure
  :defer t
  :config
  (auctex-latexmk-setup)
  (setq TeX-interactive-mode t) ; -interaction=nonstopmode
  (setq TeX-source-correlate-mode t) ; -synctex
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; Use LaTeXMk by default
  ;; https://github.com/tom-tan/auctex-latexmk/issues/33#issuecomment-939427447
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LatexMk"))))

;; Set up biblatex
(require 'bibtex)
;; https://tex.stackexchange.com/a/519366
(setq bibtex-dialect 'biblatex)

(require 'reftex)
;; Turn on RefTeX in AUCTeX
;; https://tex.stackexchange.com/a/295337
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Activate nice interface between RefTeX and AUCTeX
(setq reftex-plug-into-AUCTeX t)



;;;; Programming:
;;
;; Major modes for various languages, compilation configuration,
;; and git things.

;;; Custom configurations
;; For when I implement something myself, or can't be bothered with Quelpa
(add-to-list 'load-path "~/.emacs.local/")

;;; Git Commit Mode
;;   https://www.emacswiki.org/emacs/GitCommitMode
;;
;; NOTE: git-commit-mode isn’t used when committing from the command-line:
;;   https://magit.vc/manual/magit/git_002dcommit_002dmode-isn_0027t-used-when-committing-from-the-command_002dline.html
;;
;; This allows us to do things like C-c C-c when `git` from the command line opens a commit dialogue.
(require 'server)
(or (server-running-p) (server-mode))

;;; Magit
;;   https://reddit.com/r/emacs/comments/1954ay9/comment/khnm1en
;;
;; Requires loading of compat before transient or magit on Emacs ≤ 29:
;;   https://github.com/progfolio/elpaca/issues/421#issuecomment-2677091304
(use-package compat :ensure)
(use-package transient
  :ensure
  :defer t
  :after compat)
(use-package magit
  :ensure
  :demand t
  :after transient
  :config
  (add-to-list 'magit-section-initial-visibility-alist '(untracked . show)))

;;; Reset default compilation command
(use-package just-mode :ensure :defer t)
(setq compile-command "just")

;;;; Licenses
(use-package yasnippet
  :ensure
  :defer t
  :init
  ;; Useful for snippets
  (setq yas/triggers-in-field nil)
  (setq yas-snippet-dirs '("~/.emacs.snippets/"))
  (yas-global-mode 1)

  ;; Required for Rust LSP
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;; Useful for modifying compilation commands
(require 'compile)

;; Use colours in compilation buffer
;;   https://stackoverflow.com/a/63710493
(use-package xterm-color :ensure :defer t)
(setq compilation-environment '("TERM=xterm-256color"))
(defun advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'advice-compilation-filter)

;;; Julia
;;   https://github.com/JuliaEditorSupport/julia-emacs
(use-package julia-mode
  :ensure
  :defer t
  :interpreter ("julia" . julia-mode)
  :config
  (setenv "JULIA_NUM_THREADS" "auto")
  (add-hook 'julia-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (format "julia --project %s"
                           (file-name-nondirectory buffer-file-name))))))

;;; Rust
;;   https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :ensure
  :defer t
  :config
  (add-hook 'rust-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "rustc %s && ./%s"
                         (file-name-nondirectory buffer-file-name)
                         (file-name-base buffer-file-name))))))

;;; Go
(use-package go-mode
  :ensure
  :defer t
  :config
  (add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "go run %s"
                         (file-name-nondirectory buffer-file-name))))))

;;; R
(use-package ess :ensure :defer t)

;;; Splunk
(use-package splunk-mode :ensure :defer t)

;;; LLVM
;;
;; Recipe adapted from:
;;   https://github.com/melpa/melpa/blob/90d680/recipes/llvm-mode
;;
;; Previously using: repo github.com/llvm-mirror/llvm.  We should find
;; a better solution for this as it just takes so long to clone.  I'm
;; not the only one who wants this:
;;   https://lists.llvm.org/pipermail/llvm-dev/2018-May/123171.html
(use-package llvm-mode
  :ensure
  (llvm-mode
   :host github
   :repo "llvm/llvm-project"
   :files ("llvm/utils/emacs/llvm-mode.el"))
  :defer t)

;;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "python3 %s" (file-name-nondirectory buffer-file-name)))

            ;; Set line at 80 characters as per PEP8
            (setq fill-column 80)
            (display-fill-column-indicator-mode 1)))

;;; Indent in Python mode
;;   https://stackoverflow.com/a/3685541
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode t)
            (setq tab-width 4)
            (setq python-indent-offset 4)))

;;; Forth
(use-package forth-mode
  :ensure
  :defer t
  :config
  (add-hook 'forth-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "gforth %s -e bye"
                         (file-name-nondirectory buffer-file-name))))))

;;; Paredit hooks
(add-hook 'emacs-lisp-mode-hook  'turn-on-paredit)
(add-hook 'lisp-mode-hook        'turn-on-paredit)
(add-hook 'common-lisp-mode-hook 'turn-on-paredit)

;;; Handle tabs
;;
;; I mostly dispise tabs, but some programs prefer them.  We have to define some to skip
;; in the coming code blocks.
;;
;; Firstly, we untabify file on save in programming modes
;;
;; For more basic major modes, see:
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Major-Modes.html
;;
;; Previously, we were only doing this for certain modes:
;;   https://github.com/jakewilliami/configs/blob/bce7228a/src/dotfiles/dot_emacs.d/init.el#L788-L791
;;
;; Now we do it for all except Go [ref KSMPV]
(add-hook 'prog-mode-hook 'untabify-file-hook)

;; We also add hook to set up whitespace handling for all programming modes
;;
;; Previously, we did this for individual programming mode hooks:
;;   https://github.com/jakewilliami/configs/blob/bce7228a/src/dotfiles/dot_emacs.d/init.el#L793-L801
(add-hook 'prog-mode-hook 'set-up-whitespace-handling)
(add-hook 'go-mode-hook 'set-up-whitespace-handling-allow-tabs)



;;;; Rust LSP Mode:
;;
;; Rust IDE-like development environment
;;
;; Ref:
;;   - https://robert.kra.hn/posts/rust-emacs-setup/
;;   - https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/
;;   - https://github.com/brotzeit/rustic
;;
;; Set Up:
;;   $ rustup component add rust-src
;;   $ rustup component add rust-analyzer
;;
;; The remaining packages should be installed via use-package
;; For some reason I also had to install zsh for this to work

;;; Inline errors for rustic
(use-package flycheck :ensure :defer t)

;;; Rustic requires `rustic' and `use-package'
(use-package rustic
  :ensure
  :after flycheck
  :defer t
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

  ;; Uncomment for less flashiness:
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; Comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rustic-mode-hook))

(defun rustic-mode-hook ()
  ;; So that runnng C-c C-c C-r works without having to confirm,
  ;; but doesn't try to save Rust buffers that are not file visiting.
  ;; Once https://github.com/brotzeit/rustic/issues/253 has been resolved
  ;; this should no longer be necessary
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;;; Configure LSP mode
(use-package lsp-mode
  :ensure
  :defer t

  :init
  ;; Use flycheck instead of flymake (better lsp-ui integration)
  (setq lsp-prefer-flymake nil)

  :commands lsp
  :custom

  ;; What to use when checking on-save.
  ;; `check' is default, but I prefer `clippy'
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)

  ;; Enable/disable the hints as you prefer:
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
  (defun lsp-eldoc-advice (orig-fun &rest args)
    (let ((msg (car args)))
      (if msg
          (funcall orig-fun (->> msg (s-trim-left)
                                 (s-split "\n")
                                 (first))))))
  (advice-add 'lsp--eldoc-message :around #'lsp-eldoc-advice)

  ;; Avoid questions about restarting the LSP server when quitting emacs
  (defun lsp-disable-server-autorestart ()
    (setq lsp-restart nil))
  (add-hook 'kill-emacs-hook #'lsp-disable-server-autorestart))

;;; Configure LSP
(use-package lsp-ui
  :ensure
  :defer t

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
  :defer t
  :custom

  ;; How long to wait until popup
  (company-idle-delay 0.5)

  ;; Uncomment to disable popup
  ;; (company-begin-commands nil)

  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last)))

;;; Templating system for more cleverness
;; See demo:
;;   https://www.youtube.com/watch?v=ZCGmZK4V7Sg
;; (use yassnippet)

;;; Inline type hints
(setq lsp-rust-analyzer-server-display-inlay-hints t)

;;; Properly indent CSV mode
(add-hook 'csv-mode-hook
          (lambda ()
            (define-key csv-mode-map (kbd "C-c C-M-a")
                        (defun csv-align-visible (&optional arg)
                          "Align visible fields"
                          (interactive "P")
                          (csv-align-fields nil (window-start) (window-end))))))
