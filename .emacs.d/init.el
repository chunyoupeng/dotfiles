;; -*- lexical-binding: t; -*-

;;; Basic Settings

;; Ensure Emacs version compatibility
(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Add 'lisp' directory to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Constants
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;; Garbage Collection Optimization

(defvar normal-gc-cons-threshold (* 20 1024 1024)
  "Normal threshold for garbage collection.")
(defvar init-gc-cons-threshold (* 128 1024 1024)
  "Initial threshold for garbage collection during startup.")

(setq gc-cons-threshold init-gc-cons-threshold
      gc-cons-percentage 0.1)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold normal-gc-cons-threshold)
            (setq gc-cons-percentage 0.1)))

;;; Package Management

(require 'package)

;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Refresh package contents if necessary
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t) ;; Defer loading by default

;;; UI Optimizations

;; Disable unnecessary UI elements to reduce overhead
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar
(set-fringe-mode 10)        ; Give some breathing room

;; Use shift + arrow to move between buffer
(windmove-default-keybindings)

;; Pair auto generation
(electric-pair-mode 1)
;; Frame transparency and fullscreen
(defvar efs/frame-transparency '(95 . 95)
  "Frame transparency setting (active . inactive).")

(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Font settings
(defvar efs/default-font-size 150)
(defvar efs/default-variable-font-size 150)

(set-face-attribute 'default nil
                    :font "Iosevka Nerd Font Mono"
                    :height efs/default-font-size
                    :weight 'regular
                    :width 'condensed)

(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka Nerd Font Mono"
                    :height efs/default-font-size)

(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Nerd Font Mono"
                    :height efs/default-variable-font-size
                    :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; Theme and Modeline

(use-package doom-themes
  :config
  ;; Choose a lighter, more performance-friendly theme
  (load-theme 'doom-bluloco-light t)

  ;; Enable doom-themes specific features
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config))

(use-package all-the-icons
  :defer t) ;; Defer loading until needed

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;;; Helpful Packages

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)) ;; Reduced delay for better responsiveness

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5)) ;; Limit color count for performance

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
        vterm-max-scrollback 5000
        shell-command-switch "-ic"
        explicit-shell-file-name "/usr/bin/zsh"))

(use-package multi-vterm
  :commands (multi-vterm multi-vterm-prev multi-vterm-next)
  :bind (("C-<f9>" . multi-vterm)
         ("C-c <left>" . multi-vterm-prev)
         ("C-c <right>" . multi-vterm-next)))

(use-package good-scroll
  :defer t
  :config
  (good-scroll-mode 1)
  :bind (("M-n" . good-scroll-up)
         ("M-p" . good-scroll-down)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package pyim
  :config
  (setq default-input-method "pyim")
  (require 'pyim-wbdict)
  (pyim-wbdict-v86-enable)
  (setq pyim-default-scheme 'wubi))

;;; Performance Tuning

;; Increase read-process-output-max for better performance with language servers
(setq read-process-output-max (* 4 1024 1024)) ;; 4MB
(setq process-adaptive-read-buffering nil)

;; Disable backup files to reduce disk I/O
(setq make-backup-files nil)

;;; Custom Functions

(defun insert-current-date-ymd ()
  "Insert the current date in YYYY-MM-DD format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun move-end-of-line-and-newline ()
  "Move to the end of the line and insert a newline."
  (interactive)
  (move-end-of-line nil)
  (newline))

;;; Key Bindings

(global-set-key (kbd "<f12>") 'compile)
(global-set-key (kbd "C-<return>") 'move-end-of-line-and-newline)
(global-set-key (kbd "C-c \\") 'insert-current-date-ymd)

;;; Hooks and Mode Configurations

;; Disable line numbers in markdown mode
(use-package markdown-mode
  :config
  ;; Remove specific key bindings if necessary
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "M-n") nil)
    (define-key markdown-mode-map (kbd "M-p") nil)
    (define-key markdown-mode-map (kbd "C-c <left>") nil)
    (define-key markdown-mode-map (kbd "C-c <right>") nil)))

;;; Dired Settings

(setq dired-use-ls-dired nil)

(defvar word-count-rule-chinese "\\cc"
  "A regexp string to match Chinese characters.")

(defvar word-count-rule-nonespace "[^[:space:]]"
  "A regexp string to match none pace characters.")

(defvar word-count-rule-ansci "[A-Za-z0-9][A-Za-z0-9[:punct:]]*"
  "A regexp string to match none pace characters.")

(defun special-words-count (start end regexp)
  "Count the word from START to END with REGEXP."
  (let ((count 0))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (re-search-forward regexp end t))
        (setq count (1+ count))))
    count))

;;;###autoload
(defun Chinese-word-count (&optional beg end)
  "Chinese user preferred word count.
If BEG or END is not specified, count the whole buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let ((min (if (and beg end) beg (point-min)))
        (max (if (and beg end) end (point-max)))
        list)
    (setq list
          (mapcar (lambda (r)
                    (special-words-count min max r))
                  (list
                   word-count-rule-chinese
                   word-count-rule-nonespace
                   word-count-rule-ansci
                   )))
    (message "字数: %d"
             (+ (car list) (car (last list))))))

;;; Custom Set Variables and Faces

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; Ensure there is only one instance of this block
 '(package-selected-packages
   '(pyim-wbdict multi-vterm vterm all-the-icons-dired ivy-rich avy drag-stuff yasnippet-snippets yasnippet all-the-icons good-scroll good-scroll-mode  expand-region which-key rainbow-delimiters no-littering ivy-prescient helpful general forge eshell-git-prompt doom-themes doom-modeline dired-single dired-hide-dotfiles company-box command-log-mode auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; Ensure there is only one instance of this block
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#FFFFFF" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 151 :width condensed :foundry "FBI " :family "Iosevka Nerd Font")))))

;;; Performance Enhancements

;; Optionally enable native compilation if using Emacs 28+
(when (and (>= emacs-major-version 28)
           (fboundp 'native-compile-async))
  (setq native-comp-async-report-warnings-errors 'silent))

;;; Display Startup Time

(defun efs/display-startup-time ()
  "Display the time it took Emacs to initialize."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;; Summary

;; This configuration focuses on optimizing performance by:
;; - Adjusting garbage collection settings
;; - Deferring package loading where possible
;; - Minimizing UI overhead
;; - Optimizing specific packages for better performance
;; - Cleaning up redundant settings and key bindings

;; Further optimizations can be made based on specific usage patterns and performance profiling.
