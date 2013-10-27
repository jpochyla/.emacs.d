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
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Character encodings default to utf-8
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Move deleted files to trash
(setq delete-by-moving-to-trash t)

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

;; Enable indicator of the end of the buffer
(setq indicate-empty-lines t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; GUI
(when window-system

  ;; Window size and position
  (add-to-list 'default-frame-alist (cons 'left 400))
  (add-to-list 'default-frame-alist (cons 'width 100))
  (add-to-list 'default-frame-alist (cons 'height 56))

  ;; No window border
  (add-to-list 'default-frame-alist '(internal-border-width . 0))

  ;; Font
  (set-face-font 'default "Monaco-12")
  (set-face-font 'fixed-pitch "Monaco-12")
  (set-face-font 'variable-pitch "Lucida Grande-12")

  ;; Use line cursor
  (setq-default cursor-type 'bar)

  ;; Enable menu bar
  (menu-bar-mode t)

  ;; Color theme
  ;; (load-theme 'noctilux)
  (load-theme 'sanityinc-solarized-light))

;; Granular trackpad scrolling
(global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
(global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
(global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
(global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
(global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
(global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4)))

;; IDO
(require 'ido-ubiquitous)
(require 'ido-vertical-mode)
(require 'flx-ido)
(ido-ubiquitous-mode t)
(ido-vertical-mode t)
(flx-ido-mode t)

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
      sp-autoescape-string-quote nil)
(sp-use-paredit-bindings)

;; Auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-ignore-case nil)

;; Bindings

(defun join-line-down ()
  (interactive)
  (join-line -1))
(global-set-key (kbd "M-j") 'join-line-down)

;; Expand region by semantic units
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Moving line/region up/down
(require 'move-text)
(global-set-key (kbd "C-S-p") 'move-text-up)
(global-set-key (kbd "C-S-n") 'move-text-down)

;; Global dev hooks

(defun auto-fill-comments ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun prog-mode-defaults ()
  (smartparens-strict-mode t)
  (auto-complete-mode t)
  (auto-fill-comments))
(add-hook 'prog-mode-hook 'prog-mode-defaults)

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

;; Clojure

(setq nrepl-hide-special-buffers t)
