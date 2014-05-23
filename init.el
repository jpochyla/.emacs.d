;; init.el
;; Emacs configuration

;;
;; Packages, global options

(require 'package)

;; Package repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Initialize package system
(package-initialize)

;; Load a slightly better set of defaults to build upon
(require 'better-defaults)

;;
;; Environment

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

;; Store temp files in proper location
(setq undo-tree-history-directory-alist
      `((".*" . "~/.emacs.d/var/undo")))
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/var/backup")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/var/autosave" t)))
(setq auto-save-list-file-prefix "~/emacs.d/var")
(setq ac-comphist-file "~/.emacs.d/var/ac-comphist.dat")
(setq save-place-file "~/.emacs.d/var/places")
(setq mc/list-file "~/.emacs.d/var/mc-lists.el")

;; Auto refresh buffers
(global-auto-revert-mode t)

;; Run GC every 20 MB
(setq gc-cons-threshold (* 20 1024 1024))

;;
;; UI

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

;; GUI
(when window-system

  ;; Window size and position
  (add-to-list 'default-frame-alist (cons 'left 400))
  (add-to-list 'default-frame-alist (cons 'width 90))
  (add-to-list 'default-frame-alist (cons 'height 50))

  ;; No window border
  (add-to-list 'default-frame-alist '(internal-border-width . 0))

  ;; Line spacing
  ;; (add-to-list 'default-frame-alist '(line-spacing . 1))

  ;; Indicate the end of buffer
  (setq indicate-empty-lines t)

  ;; Font
  (set-face-font 'default "Meslo LG S-13")
  (set-face-font 'fixed-pitch "Meslo LG S-13")
  (set-face-font 'variable-pitch "Lucida Grande-13")

  ;; Use line cursor, don't blink
  (setq-default cursor-type 'bar)
  (blink-cursor-mode 0)

  ;; Color theme
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
  (load-theme 'noctilux)
  ;; (load-theme 'sanityinc-solarized-light)
  ;; (load-theme 'default-dark)

  ;; Enable menu bar
  (menu-bar-mode t))

;; Granular trackpad scrolling
(global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
(global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
(global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
(global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
(global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
(global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4)))

;; IDO
(add-to-list 'ido-ignore-files "\\.DS_Store")
(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)
(require 'flx-ido)
(flx-ido-mode t)
;; (require 'ido-vertical-mode)
;; (ido-vertical-mode t)
(setq ido-enable-prefix nil
      ido-use-virtual-buffers t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)

;;
;; Editing & Development

;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Do not use shift for selections
(setq shift-select-mode nil)

;; Indentation
(setq-default tab-width 4)
(setq c-default-style "bsd"
      c-basic-offset 4)
(electric-indent-mode 1)

;; Auto-pairing and structured editing
(require 'smartparens)
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit
      sp-autoskip-closing-pair 'always
      sp-autoescape-string-quote nil
      sp-show-pair-delay 0)
(sp-use-paredit-bindings)

;; Auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-ignore-case nil)

;; Indent after yank
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (when (not current-prefix-arg)
             (let ((mark-even-if-inactive transient-mark-mode))
               (indent-region (region-beginning) (region-end) nil))))))

;; Bindings

(defun join-line-down ()
  (interactive)
  (join-line t))
(global-set-key (kbd "M-j") 'join-line-down)

;; Expand region by semantic units
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

;; Moving line/region up/down
(require 'move-text)
(global-set-key (kbd "C-S-p") 'move-text-up)
(global-set-key (kbd "C-S-n") 'move-text-down)

;; Quick movement
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Global dev hooks

(defun auto-fill-comments ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun delete-trailing-whitespace-on-save ()
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(defun prog-mode-defaults ()
  (subword-mode t)
  (smartparens-mode t)
  (auto-complete-mode t)
  (auto-fill-comments)
  (delete-trailing-whitespace-on-save))
(add-hook 'prog-mode-hook 'prog-mode-defaults)

(defun lisp-mode-defaults ()
  (smartparens-strict-mode t)
  (rainbow-delimiters-mode t))
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-defaults)
(add-hook 'clojure-mode-hook 'lisp-mode-defaults)

(defun text-mode-defaults ()
  (smartparens-strict-mode t)
  (auto-fill-mode t))
(add-hook 'text-mode-hook 'text-mode-defaults)

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
