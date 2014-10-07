;;; init.el --- Emacs configuration

;;; Packages, global options
;;; ====================================================================

(require 'package)

;; Package repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Initialize package system
(package-initialize)

;; Install all required packages
(setq required-packages
      '(better-defaults
        cider
        cmake-mode
        company
        company-tern
        exec-path-from-shell
        expand-region
        flx
        flx-ido
        flycheck
        go-mode
        ido-ubiquitous
        js2-mode
        js2-refactor
        magit
        markdown-mode
        math-symbols
        multiple-cursors
        protobuf-mode
        rainbow-delimiters
        rust-mode
        shift-text
        skewer-mode
        smartparens
        smex
        undo-tree
        web-mode
        yasnippet))

(defun package-require (pkg)
  (when (not (package-installed-p pkg))
    (package-install pkg)))
(mapc 'package-require required-packages)

;; Load a slightly better set of defaults to build upon
(require 'better-defaults)

;;; Environment
;;; ====================================================================

;; Setup PATH
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Start in home directory
(setq default-directory "~")

;; Store saved customizations in a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Character encodings default to utf-8
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Move deleted files to trash
(setq delete-by-moving-to-trash t)

;; Don't create lock files for opened buffers
(setq create-lockfiles nil)

;; Store temp files in proper location
(setq var-dir "~/.emacs.d/var")
(setq backup-directory-alist `((".*" . ,(expand-file-name "backup" var-dir)))
      undo-tree-history-directory-alist `((".*" . ,(expand-file-name "undo" var-dir)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "autosave" var-dir) t))
      auto-save-list-file-prefix (expand-file-name "autosave-" var-dir)
      ido-save-directory-list-file (expand-file-name "ido.last" var-dir)
      recentf-save-file (expand-file-name "recentf" var-dir)
      save-place-file (expand-file-name "places" var-dir)
      smex-save-file (expand-file-name "smex-items" var-dir)
      mc/list-file (expand-file-name "mc-lists.el" var-dir))

;; Auto refresh buffers
(global-auto-revert-mode t)

;; Run GC every 20 MB
(setq gc-cons-threshold (* 20 1024 1024))

;;; UI
;;; ====================================================================

;; No startup splash screen
(setq inhibit-startup-message t)

;; No bell
(setq ring-bell-function 'ignore)

;; Enable column number in modeline
(setq column-number-mode t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Match parentheses without delay
(setq show-paren-delay 0)

;; Show visual indicators of line wrap
(setq-default fringe-mode 'left-only)
(setq-default visual-line-fringe-indicators
              '(left-curly-arrow right-curly-arrow))

;; IDO
(add-to-list 'ido-ignore-files "\\.DS_Store")
(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)
(require 'flx-ido)
(flx-ido-mode t)
(setq ido-enable-prefix nil
      ido-use-virtual-buffers t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-default-file-method 'selected-window)
;; Bind `~` to go to homedir when in ido-find-file
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))

;; GUI
(when window-system

  ;; Window size and position
  (add-to-list 'default-frame-alist (cons 'left 350))
  (add-to-list 'default-frame-alist (cons 'width 95))
  (add-to-list 'default-frame-alist (cons 'height 50))

  ;; No window border
  (add-to-list 'default-frame-alist '(internal-border-width . 0))

  ;; Indicate the end of buffer
  (setq-default indicate-empty-lines t)

  ;; Linum format to avoid graphics glitches in fringe
  (setq-default linum-format " %4d ")

  ;; Font
  (set-face-font 'default "Input Mono Narrow-12")
  (set-face-font 'fixed-pitch "Input Mono Narrow-12")
  (set-face-font 'variable-pitch "Input Sans Narrow-12")

  ;; Don't blink
  (blink-cursor-mode 0)

  ;; Color themes
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
  ;; (load-theme 'default-black)

  ;; Granular trackpad scrolling
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4)))

  ;; Enable menu bar
  (menu-bar-mode t))

;;; Editing
;;; ====================================================================

;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Do not use shift for selections
(setq shift-select-mode nil)

;; Indentation
(setq-default tab-width 4)

;; Indent after yank
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (when (not current-prefix-arg)
             (let ((mark-even-if-inactive transient-mark-mode))
               (indent-region (region-beginning) (region-end) nil))))))

;; Undo tree
(global-undo-tree-mode t)
(setq undo-tree-auto-save-history t)

;; Auto-pairing and structured editing
(require 'smartparens)
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit
      sp-autoskip-closing-pair 'always
      sp-autoescape-string-quote nil
      sp-show-pair-delay 0)
(sp-use-paredit-bindings)

(defun pair-on-newline-and-indent (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode))
  (indent-according-to-mode))

(sp-pair "{" nil :post-handlers
         '(:add (pair-on-newline-and-indent "RET")))
(sp-pair "[" nil :post-handlers
         '(:add (pair-on-newline-and-indent "RET")))

;; Auto-complete
(require 'company)
(setq company-idle-delay 0.125
      company-tooltip-limit 12
      company-minimum-prefix-length 1
      company-tooltip-flip-when-above t
      company-require-match nil
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil
      company-transformers '(company-sort-by-occurrence)
      company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                          company-preview-frontend
                          company-echo-metadata-frontend))
(define-key company-active-map (kbd "ESC") 'company-abort)

;; Snippets
(require 'yasnippet)
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt)
      yas-verbosity 1
      yas-wrap-around-region t)

(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)
(define-key yas-keymap (kbd "C-g") 'yas/exit-all-snippets)

;;; Global bindings
;;; ====================================================================

(defun kill-default-buffer ()
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(defun kill-buffer-and-window ()
  (interactive)
  (progn
    (kill-buffer)
    (delete-window)))

(defun join-line-down ()
  (interactive)
  (join-line t))
(global-set-key (kbd "M-j") 'join-line-down)

;; Buffer management
(global-set-key (kbd "C-x k") 'kill-default-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

;; Expand region by semantic units
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(define-key mc/keymap (kbd "<return>") nil)

;; Moving line/region up/down
(require 'shift-text)
(global-set-key (kbd "C-S-p") 'shift-text-up)
(global-set-key (kbd "C-S-n") 'shift-text-down)

;; Quick movement
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;; Development modes
;;; ====================================================================

(defun auto-fill-comments ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun delete-trailing-whitespace-on-save ()
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(defun prog-mode-defaults ()
  (subword-mode t)
  (company-mode t)
  (smartparens-mode t)
  (auto-fill-comments)
  (delete-trailing-whitespace-on-save))
(add-hook 'prog-mode-hook 'prog-mode-defaults)

(defun text-mode-defaults ()
  (auto-fill-mode t))
(add-hook 'text-mode-hook 'text-mode-defaults)

;; LISPs

(defun lisp-mode-defaults ()
  (smartparens-strict-mode t)
  (rainbow-delimiters-mode t))
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-defaults)
(add-hook 'clojure-mode-hook 'lisp-mode-defaults)

;; Clojure

(add-hook 'cider-repl-mode-hook 'lisp-mode-defaults)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)

;; Web dev

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 4
      web-mode-code-indent-offset 4)

;; Javascript

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(custom-set-variables
 '(js2-basic-offset 2))

;; Clojure

(add-hook 'cider-repl-mode-hook 'lisp-mode-defaults)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
