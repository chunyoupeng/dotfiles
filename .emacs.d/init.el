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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI Optimizations

;; Disable unnecessary UI elements to reduce overhead
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar
(set-fringe-mode 10)        ; Give some breathing room

;; Use shift + arrow to move between buffer
(windmove-default-keybindings)
(delete-selection-mode 1)
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

(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Nerd Font Mono"
                    :height efs/default-variable-font-size
                    :weight 'regular)

;; Make ESC quit prompts
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; Theme and Modeline

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
)

(load-theme 'doom-gruvbox t)
(global-git-gutter-mode t)
(use-package all-the-icons
  :defer t) ;; Defer loading until needed

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
;; Function to get the Chinese character count dynamically for the mode line

(defun count-chinese-characters-dynamic ()
  "Return the Chinese character count for the mode line."
  (let ((count (count-chinese-characters)))
    (format "字数: %d" count))) ;; Display the count with label

;; Define mode-line-format to include buffer percentage and Chinese character count
(setq-default mode-line-format
              '((:eval
                 (list
                  ;; Display the buffer name
                  "%b "
                  ;; Custom message
                  " | "
                  ;; Major mode
                  (format "[%s]" mode-name)
                  ;; Line and column info
                  " | Line %l"
                  ;; Dynamic Chinese character count
                  "% | " (count-chinese-characters-dynamic)))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5)) 
;; Enable Paredit for Lisp modes
(use-package paredit
  :ensure t)
(add-hook 'racket-mode-hook #'paredit-mode)
(add-hook 'racket-repl-mode-hook #'paredit-mode)

;; Enable paredit for Lisp modes
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'sly-repl-mode-hook #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helpful Packages
(setq inferior-lisp-program "sbcl")
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)) ;; Reduced delay for better responsiveness

(use-package magit
  :ensure t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Terminal 
(use-package vterm
  :defer t
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
        vterm-max-scrollback 1000
        shell-command-switch "-ic"
        explicit-shell-file-name "/bin/zsh"))

(use-package multi-vterm
  :defer t
  :commands (multi-vterm multi-vterm-prev multi-vterm-next)
  :bind (("C-<f9>" . multi-vterm)
         ("C-c <left>" . multi-vterm-prev)
         ("C-c <right>" . multi-vterm-next)))

(add-hook 'vterm-kill-buffer-hook #'(lambda () (garbage-collect)))
;; (defun my/cleanup-vterm-buffers ()
;;   "关闭不活动的 vterm 缓冲区。"
;;   (interactive)
;;   (dolist (buffer (buffer-list))
;;     (when (and (eq (buffer-local-value 'major-mode buffer) 'vterm-mode)
;;                (not (get-buffer-window buffer)))
;;       (kill-buffer buffer))))

;; ;; 定期运行清理函数，例如每隔 65 分钟
;; (run-at-time nil (* 65 60) #'my/cleanup-vterm-buffers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun insert-line-above ()
  "Insert the line above at any position"
  (interactive)
  (move-beginning-of-line 1)
  (open-line 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Key Bindings

(global-set-key (kbd "<f12>") 'compile)
(global-set-key (kbd "s-<return>") 'move-end-of-line-and-newline)
(global-set-key (kbd "s-S-<return>") 'insert-line-above)
(global-set-key (kbd "C-c \\") 'insert-current-date-ymd)
(global-set-key (kbd "C-c 、") 'insert-current-date-ymd)
(global-set-key (kbd "C-c *") 'count-chinese-characters)
(global-set-key (kbd "C-c d") 'open-journal-2025)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c C-SPC") 'delete-extra-blank-lines-macro)
(global-set-key (kbd "M-》") 'end-of-buffer)
(global-set-key (kbd "M-《") 'beginning-of-buffer)
(global-set-key (kbd "C-`") 'my-expand-copy-search)
(global-set-key (kbd "M-m") 'point-to-register)      ;; Set mark at current point
(global-set-key (kbd "M-'") 'jump-to-register)      ;; Jump to mark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable line numbers in markdown mode
(use-package markdown-mode
  :ensure t
  :config
  ;; Remove specific key bindings if necessary
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "M-n") nil)
    (define-key markdown-mode-map (kbd "M-p") nil)
    (define-key markdown-mode-map (kbd "C-c <left>") nil)
    (define-key markdown-mode-map (kbd "C-c <right>") nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lisp Mode 
(with-eval-after-load 'sly
  (define-key sly-editing-mode-map (kbd "M-n") nil)
  (define-key sly-editing-mode-map (kbd "M-p") nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 中文字符统计
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
(defun count-chinese-characters (&optional beg end)
  "Chinese user preferred word count.
If BEG or END is not specified, count the whole buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let ((min (if (and beg end) beg (point-min)))
        (max (if (and beg end) end (point-max)))
        list total-count)
    (setq list
          (mapcar (lambda (r)
                    (special-words-count min max r))
                  (list
                   word-count-rule-chinese
                   word-count-rule-nonespace
                   word-count-rule-ansci)))
    (setq total-count (+ (car list) (car (last list))))
    total-count))  ;; 返回计算的字数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-journal-2024 ()
  "Open the 2024 journal file."
  (interactive)
  (find-file "/Users/pengyo/Documents/notes/Journal/2024"))

(defun open-journal-2025 ()
  "Open the 2025 journal file."
  (interactive)
  (find-file "/Users/pengyo/Documents/notes/Journal/2025"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Macro 删除多余的行
(defun delete-extra-blank-lines-macro (beg end)
  "Delete extra blank lines in region if active, otherwise in whole buffer."
  (interactive (if (use-region-p)
                  (list (region-beginning) (region-end))
                (list (point-min) (point-max))))
  (save-excursion
    (goto-char beg)
    (while (and (< (point) end) (not (eobp)))
      (delete-blank-lines)
      (forward-line 1))))
;;; Custom Set Variables and Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-expand-copy-search ()
  "Expand region to the word under the cursor, copy it, and search for it."
  (interactive)
  (require 'expand-region) ; Ensure expand-region is loaded
  (er/expand-region 1)    ; Expand region to select the current word
  (kill-ring-save (region-beginning) (region-end)) ; Copy the selected word
  (deactivate-mark)       ; Deactivate the region
  (let ((search-word (current-kill 0))) ; Get the copied word from the kill-ring
    (isearch-mode t)                    ; Start incremental search
    (isearch-yank-string search-word))) ; Paste the word into the search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-dark+ doom-one doom-challenger-deep))
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" default))
 '(package-selected-packages
   '(git-gutter json-navigator json-mode sly web-mode paredit markdown-mode multiple-cursors magit racket-mode multi-vterm vterm all-the-icons-dired ivy-rich avy drag-stuff yasnippet-snippets yasnippet all-the-icons good-scroll good-scroll-mode expand-region which-key rainbow-delimiters no-littering ivy-prescient helpful general forge eshell-git-prompt doom-themes doom-modeline dired-single dired-hide-dotfiles company-box command-log-mode auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 150 :width normal :foundry "nil" :family "Iosevka Nerd Font")))))

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
