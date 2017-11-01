;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 show-trailing-whitespace t
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; global keybindings
(global-unset-key (kbd "C-z"))

;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)



;;;;;;;;;;;;;;
;; UI stuff ;;
;;;;;;;;;;;;;;

;; Hightlight parenthesis
(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-style 'expression) ; alternatives are 'parenthesis' and 'mixed'

;; Enable flyspell

(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(add-hook 'text-mode-hook 'flyspell-mode)

;; Clean whitespaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Color the status menu
(use-package smart-mode-line
  :ensure t)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'dark)
(add-hook 'after-init-hook #'sml/setup)

;; show the cursor when moving after big movements in the window
(use-package beacon
  :ensure t)
(beacon-mode +1)

;; Hightlight current line
(use-package hlinum
  :ensure t)
(hlinum-activate)
(global-linum-mode t)
(global-hl-line-mode)

(use-package jbeans-theme
  :ensure t)

(load-theme 'jbeans t)

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

;; Ensime stable
(use-package ensime
  :ensure t
  :pin melpa-stable)

;; Ensime - unstable
;;(use-package ensime
;;  :ensure t
;;  :pin melpa)

;;(use-package sbt-mode
;;  :ensure t
;;  :pin melpa)

;;(use-package scala-mode
;;  :ensure t
;;  :pin melpa)

;; Projectile
;; http://batsov.com/projectile/
(use-package projectile
  :ensure t
  :demand
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("s-f" . projectile-find-file)
           ("s-F" . projectile-grep)))
(projectile-global-mode)
(setq projectile-require-project-root nil)

;; Undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode)
  :bind ("s-/" . undo-tree-visualize))

;; IBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Which-key
(use-package which-key
  :ensure t)
(which-key-mode)
(which-key-setup-side-window-right-bottom)


;; helm
(use-package helm
  :ensure t)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)

(global-unset-key (kbd "C-x c"))

;; enable fuzzy matching
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)


(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(use-package helm-descbinds
  :ensure t)
(helm-descbinds-mode)

(helm-mode 1)

;; Key Chords
(use-package key-chord
  :ensure t)

(key-chord-define-global "jj" 'avy-goto-word-1)
(key-chord-define-global "jl" 'avy-goto-line)
(key-chord-define-global "jk" 'avy-goto-char)
(key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "xx" 'execute-extended-command)
(key-chord-define-global "yy" 'browse-kill-ring)

(defvar key-chord-tips '("Press <jj> quickly to jump to the beginning of a visible word."
                         "Press <jl> quickly to jump to a visible line."
                         "Press <jk> quickly to jump to a visible character."
                         "Press <JJ> quickly to switch to previous buffer."
                         "Press <uu> quickly to visualize the undo tree."
                         "Press <xx> quickly to execute extended command."
                         "Press <yy> quickly to browse the kill ring."))
(key-chord-mode +1)

;; Personal
(add-to-list 'exec-path "/usr/bin")

;; Yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))
(yas-global-mode 1)

(use-package company
  :diminish company-mode
  :commands company-mode
  :ensure t
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil)
  :bind ("M-0" . company-yasnippet))

(add-hook 'after-init-hook 'global-company-mode)

;; Hightlight Symbols
(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("M-6" . highlight-symbol)
  :bind ("M-7" . highlight-symbol-next)
  :bind ("M-8" . highlight-symbol-prev)
  :bind ("M-9" . highlight-symbol-query-replace))

;; Goto last change
(use-package goto-chg
  :ensure t
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

;; Popup Summary
(use-package popup-imenu
  :ensure t
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

;; Git
(use-package magit
  :ensure t
  :commands magit-status magit-blame
  :init (setq
         magit-revert-buffers nil)
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

;; Newlines in comments
(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

;; Multiline comments
(setq comment-start "/* "
      comment-end " */"
      comment-style 'multi-line
      comment-empty-lines t)

;; Smart Parentheses
(use-package smartparens
  :diminish smartparens-mode
  :ensure t
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

(require 'smartparens-config)
(smartparens-global-mode)

;; For scala Parenthesis Formatting
(sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
(sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

(defun sp-restrict-c (sym)
  "Smartparens restriction on `SYM' for C-derived parenthesis."
  (sp-restrict-to-pairs-interactive "{([" sym))

(add-hook 'scala-mode-hook
          (lambda ()
            (bind-key "RET" 'scala-mode-newline-comments scala-mode-map)
            ;; Force dabbrev
            ;; Sometimes I just want a quick, simple, buffer-only
            ;; completion of what I’m typing, bypassing company-mode and the
            ;; server. This provides it
            (bind-key "C-<tab>" 'dabbrev-expand scala-mode-map)
            ;; Force dabbrev
            ;; Sometimes I just want a quick, simple, buffer-only
            ;; completion of what I’m typing, bypassing company-mode and the
            ;; server. This provides it
            (bind-key "C-<tab>" 'dabbrev-expand scala-mode-map)

            (bind-key "s-<delete>" (sp-restrict-c 'sp-kill-sexp) scala-mode-map)
            (bind-key "s-<backspace>" (sp-restrict-c 'sp-backward-kill-sexp) scala-mode-map)
            (bind-key "s-<home>" (sp-restrict-c 'sp-beginning-of-sexp) scala-mode-map)
            (bind-key "s-<end>" (sp-restrict-c 'sp-end-of-sexp) scala-mode-map)
            ;; Ever wanted to change a (_.thing) to a { foo => foo.thing } and back? This helps…
            (bind-key "s-{" 'sp-rewrap-sexp smartparens-mode-map)
            ;; Multi-line comments
            (setq comment-start "/* "
                  comment-end " */"
                  comment-style 'multi-line
                  comment-empty-lines t)
            (setq show-trailing-whitespace t)
            ;;            (show-paren-mode)
            (smartparens-mode)
            (yas-minor-mode)
            ;;            (git-gutter-mode)
            (company-mode)
            (ensime-mode)
            ;;            (scala-mode:goto-start-of-code)
            ))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(linum-highlight-in-all-buffersp t)
 '(menu-bar f)
 '(menu-bar-mode nil)
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (camcorder magit popup-imenu goto-chg undo-tree scala-mode which-key helm-descbinds yasnippet smartparens auto-org-md company helm-projectile use-package)))
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
