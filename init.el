;;; init.el --- Emacs configuration file
;;; Commentary:
;;; Code:

;;; package
;;; ====================================================================

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar required-packages
  '(exec-path-from-shell

    aggressive-indent
    company
    company-tern
    expand-region
    flycheck
    multiple-cursors
    rainbow-delimiters
    shift-text
    smartparens
    dtrt-indent
    undo-tree
    volatile-highlights

    anzu
    counsel
    diminish
    flx
    git-timemachine
    git-gutter
    magit
    projectile
    swiper

    cmake-mode
    dockerfile-mode
    go-mode
    markdown-mode
    protobuf-mode
    restclient
    rust-mode
    web-mode))

(defun install-if-needed (pkg)
  "Install `PKG' if not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(mapc 'install-if-needed required-packages)

;;; environment
;;; ====================================================================

;; custom locations for generated files

(defvar var-dir "~/.emacs.d/var")

(setq backup-directory-alist `((".*" . ,(expand-file-name "backup" var-dir)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "autosave" var-dir) t))
      auto-save-list-file-prefix (expand-file-name "autosave-" var-dir))

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" var-dir))

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" var-dir))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; no lockfiles for open buffers
(setq create-lockfiles nil)

;; same PATH as elsewhere in the system
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; start in the projects dir
(setq default-directory "~/Projects")

;; use utf-8
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; move to trash
(setq delete-by-moving-to-trash t)

;; auto refresh buffers
(global-auto-revert-mode t)
(diminish 'auto-revert-mode)

;; run GC every 20 MB
(setq gc-cons-threshold (* 20 1024 1024))

;; support sudo+ssh in tramp-mode
(setq tramp-default-method "ssh")
(set-default 'tramp-default-proxies-alist
             (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;;; ui
;;; ====================================================================

;; no splash screen!
(setq inhibit-startup-message t)

;; no bell!
(setq ring-bell-function 'ignore)

;; short answers
(fset 'yes-or-no-p 'y-or-n-p)

;; column number in modeline
(setq column-number-mode t)

;; visual indicators of line wrap
(setq-default fringe-mode '(8 . 0))
(setq-default visual-line-fringe-indicators
              '(left-curly-arrow right-curly-arrow))

;; highlight effects of commands
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; enhance search & replace with total matches and search position
(require 'anzu)
(global-anzu-mode)
(diminish 'anzu-mode)

;; show git info in the gutter
(require 'git-gutter)
(diminish 'git-gutter-mode)

;; make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; use shift + arrow keys to switch between windows
(require 'windmove)
(windmove-default-keybindings)

;; window configurations
(winner-mode t)

;; focus follows mouse
(setq mouse-autoselect-window t)

;; smooth trackpad scrolling
(global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
(global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
(global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
(global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
(global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
(global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4)))

;; turn off gui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when window-system

  ;; window size and position
  (add-to-list 'default-frame-alist (cons 'left 350))
  (add-to-list 'default-frame-alist (cons 'width 95))
  (add-to-list 'default-frame-alist (cons 'height 50))

  ;; no window border
  (add-to-list 'default-frame-alist '(internal-border-width . 0))

  ;; font
  ;; (set-face-font 'default "Input Mono Narrow-12")
  ;; (set-face-font 'fixed-pitch "Input Mono Narrow-12")
  ;; (set-face-font 'variable-pitch "Input Sans Narrow-12")
  (set-face-font 'default "Roboto Mono-12")
  (set-face-font 'fixed-pitch "Roboto Mono-12")
  (set-face-font 'variable-pitch "Input Sans Narrow-12")

  ;; wider fringe
  (fringe-mode '(nil . 0))

  ;; don't blink
  (blink-cursor-mode 0)

  ;; enable menu bar
  (menu-bar-mode t))

;; colors

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
;; (load-them 'zenburn)
;; (load-theme 'default-black)

;; (setq-default solarized-use-less-bold t)
;; (load-theme 'solarized-dark)

;; customize theme
(custom-theme-set-faces
 'user
 `(linum
   ((t (:height 100
                :foreground unspecified
                :inherit 'shadow
                :slant normal))))
 `(visible-mark-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inverse-video unspecified
                    :inherit 'hl-line))))
 `(hl-sexp-face
   ((t (:bold nil
              :background unspecified
              :inherit 'hl-line))))
 `(fringe
   ((t (:background unspecified))))
 `(vertical-border
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit file-name-shadow))))
 `(flymake-warnline
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit font-lock-preprocessor-face))))
 `(web-mode-function-call-face
   ((t (:foreground unspecified
                    :inherit default))))
 ;; `(web-mode-symbol-face
 ;;   ((t (:foreground unspecified
 ;;                    :inherit font-lock-constant-face))))
 ;; `(web-mode-builtin-face
 ;;   ((t (:foreground unspecified
 ;;                    :inherit default))))
 ;; `(web-mode-doctype-face
 ;;   ((t (:foreground unspecified
 ;;                    :inherit font-lock-comment-face))))
 ;; `(web-mode-html-tag-face
 ;;   ((t (:foreground unspecified
 ;;                    :inherit font-lock-function-name-face))))
 ;; `(web-mode-html-attr-name-face
 ;;   ((t (:foreground unspecified
 ;;                    :inherit font-lock-variable-name-face))))
 ;; `(web-mode-html-param-name-face
 ;;   ((t (:foreground unspecified
 ;;                    :inherit font-lock-constant-face))))
 ;; `(web-mode-whitespace-face
 ;;   ((t (:foreground unspecified
 ;;                    :inherit whitespace-space))))
 ;; `(web-mode-block-face
 ;;   ((t (:foreground unspecified
 ;;                    :inherit highlight))))
 ;; `(sp-show-pair-match-face
 ;;   ((t (:foreground unspecified
 ;;                    :background unspecified
 ;;                    :inherit show-paren-match))))
 ;; `(sp-show-pair-mismatch-face
 ;;   ((t (:foreground unspecified
 ;;                    :background unspecified
 ;;                    :inherit show-paren-mismatch))))
 )

;; ivy-mode

(require 'ivy)

(ivy-mode t)
(diminish 'ivy-mode)

(setq ivy-use-virtual-buffers t)
(setq ivy-extra-directories nil)
(setq ivy-initial-inputs-alist nil)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

(defun eh-ivy-open-current-typed-path ()
  "Do not open dired on directories."
  (interactive)
  (when ivy--directory
    (let* ((dir ivy--directory)
           (text-typed ivy-text)
           (path (concat dir text-typed)))
      (delete-minibuffer-contents)
      (ivy--done path))))

(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-f") 'eh-ivy-open-current-typed-path)

(defun eh-ivy-return-recentf-index (dir)
  (when (and (boundp 'recentf-list)
             recentf-list)
    (let ((files-list
           (cl-subseq recentf-list
                      0 (min (- (length recentf-list) 1) 20)))
          (index 0))
      (while files-list
        (if (string-match-p dir (car files-list))
            (setq files-list nil)
          (setq index (+ index 1))
          (setq files-list (cdr files-list))))
      index)))

(defun eh-ivy-sort-file-function (x y)
  "Sort files and directories by `recentf' information."
  (let* ((x (concat ivy--directory x))
         (y (concat ivy--directory y))
         (x-mtime (nth 5 (file-attributes x)))
         (y-mtime (nth 5 (file-attributes y))))
    (if (file-directory-p x)
        (if (file-directory-p y)
            (let ((x-recentf-index (eh-ivy-return-recentf-index x))
                  (y-recentf-index (eh-ivy-return-recentf-index y)))
              (if (and x-recentf-index y-recentf-index)
                  ;; Directories is sorted by `recentf-list' index
                  (< x-recentf-index y-recentf-index)
                (string< x y)))
          t)
      (if (file-directory-p y)
          nil
        ;; Files is sorted by mtime
        (time-less-p y-mtime x-mtime)))))

(add-to-list 'ivy-sort-functions-alist
             '(read-file-name-internal . eh-ivy-sort-file-function))

(defun counsel-ag-symbol ()
  (interactive)
  (counsel-ag (thing-at-point 'symbol)))

(defun counsel-ag-project ()
  (interactive)
  (counsel-ag nil (projectile-project-root)))

(defun counsel-ag-project-symbol ()
  (interactive)
  (counsel-ag (thing-at-point 'symbol) (projectile-project-root)))

(global-set-key (kbd "C-c k") 'counsel-ag-project)
(global-set-key (kbd "C-c l") 'counsel-ag-project-symbol)

;; projectile

(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'ivy)
(setq-default projectile-known-projects-file
              (expand-file-name "projectile-bookmarks.eld" var-dir))

;; magit

(global-set-key (kbd "C-x g") 'magit-status)

;;; editing
;;; ====================================================================

;; indent
(setq-default indent-tabs-mode nil
              tab-width 4)
(dtrt-indent-mode t)
(require 'aggressive-indent)
(diminish 'aggressive-indent-mode)

;; replace marked text on typing
(delete-selection-mode t)

;; wrap at 80 characters
(setq-default fill-column 80)

;; fix empty pasteboard error
(setq save-interprogram-paste-before-kill nil)

;; kill, yank, exchange-point-and-mark

(defadvice kill-region (before slick-cut activate compile)
  "When called with no active region, kill a single line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called with no active region, copy a single line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

;; beginning of line

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; smartparens

(defun pair-on-newline-and-indent ()
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (save-excursion
    (newline)
    (indent-according-to-mode))
  (indent-according-to-mode))

(require 'smartparens)
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit
      sp-autoskip-closing-pair 'always
      sp-show-pair-delay 0)
(sp-use-paredit-bindings)
(diminish 'smartparens-mode)

;; indent for pairs
(sp-pair "{" nil :post-handlers '(:add (pair-on-newline-and-indent "RET")))
(sp-pair "[" nil :post-handlers '(:add (pair-on-newline-and-indent "RET")))

;; * is a pair char in `markdown-mode'
(sp-local-pair '(markdown-mode gfm-mode) "*" "*"
               :unless '(sp-in-string-p)
               :actions '(insert wrap))

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(diminish 'undo-tree-mode)
(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist `((".*" . ,(expand-file-name "undo" var-dir))))

;; company
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
(diminish 'company-mode)

;; flycheck
(require 'flycheck)
(setq flycheck-display-errors-delay 0)

;; expand-region
(require 'expand-region)

;; multiple-cursors
(require 'multiple-cursors)
(setq mc/list-file (expand-file-name "mc-lists.el" var-dir))

;; shift-text
(require 'shift-text)

;;; global keys
;;; ====================================================================

(defun join-line-down ()
  "Join this line to next one."
  (interactive)
  (join-line t))
(global-set-key (kbd "M-j") 'join-line-down)

;; windows
(global-set-key (kbd "M-o") 'other-window)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(define-key mc/keymap (kbd "<return>") nil)

;; shift-text
(global-set-key (kbd "C-S-p") 'shift-text-up)
(global-set-key (kbd "C-S-n") 'shift-text-down)

;; move by paragraphs
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;;; prog-mode
;;; ====================================================================

(defun prelude-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):?\\)"
          1 font-lock-warning-face t))))

(defun auto-fill-comments ()
  "Comments get automatically wrapped."
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun delete-trailing-whitespace-on-save ()
  "Trailing whitespace gets trimmed before saving the buffer."
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(defun prog-mode-defaults ()
  "`prog-mode' settings."
  (setq truncate-lines t)
  (subword-mode t)
  (company-mode t)
  (electric-pair-mode t)
  ;; (smartparens-mode t)
  (auto-fill-comments)
  (git-gutter-mode)
  (delete-trailing-whitespace-on-save)
  (prelude-font-lock-comment-annotations))
(add-hook 'prog-mode-hook 'prog-mode-defaults)
(add-hook 'web-mode-hook 'prog-mode-defaults)

(defun text-mode-defaults ()
  "`text-mode' settings."
  (auto-fill-mode t))
(add-hook 'text-mode-hook 'text-mode-defaults)

;; lisp-mode

(defun lisp-mode-defaults ()
  "`lisp-mode' settings."
  (aggressive-indent-mode t)
  (smartparens-strict-mode t)
  (rainbow-delimiters-mode t))
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-defaults)

;; cmake-mode

(add-hook 'cmake-mode-hook 'prog-mode-defaults)

;; cc-mode

(defun cc-mode-defaults ()
  "`cc-mode' settings."
  (semantic-mode t))
(add-hook 'c-mode-common-hook 'cc-mode-defaults)

(defconst cc-default-style
  '("bsd"
    (c-offsets-alist . ((innamespace . [0])))))
(c-add-style "cc-default-style" cc-default-style)
(setq-default c-default-style "cc-default-style"
              c-basic-offset 4)

;; go-mode

(defun go-mode-defaults ()
  "`go-mode' settings."
  (flycheck-mode t)
  (setq tab-width 8))
(add-hook 'go-mode-hook 'go-mode-defaults)

;; web-mode

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 4
      web-mode-code-indent-offset 4
      web-mode-extra-keywords '(("javascript" . ("declare" "module" "type" "continue"
                                                 "async" "await"))))
(require 'web-mode)

(add-to-list 'web-mode-comment-formats '("javascript" . "// "))

(defun js-doc-comment ()
  "Insert a js-doc comment at point."
  (interactive)
  (indent-according-to-mode)
  (insert "/**")
  (newline-and-indent)
  (insert "*")
  (newline-and-indent)
  (insert "*/")
  (previous-line)
  (end-of-line)
  (insert " "))

(defun buffer-first-line ()
  "Return the first line of current buffer."
  (save-excursion
    (goto-char (point-min))
    (thing-at-point 'line)))

(flycheck-define-checker javascript-flow
  "A JavaScript syntax and style checker using Flow."
  :command ("flow" source-original)
  :error-patterns
  ((error line-start
          (file-name)
          ":"
          line
          ":"
          (minimal-match (one-or-more not-newline))
          ": "
          (message (minimal-match (and (one-or-more anything) "\n")))
          line-end))
  :modes web-mode
  :predicate
  (lambda () (string-match ".*@flow.*" (buffer-first-line))))

(add-to-list 'flycheck-checkers 'javascript-flow)

;;; init.el ends here
