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
(setenv "TZ" "Europe/Madrid")

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
(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package pyimport
  :ensure t)

;; Font size in 1/10pt, so 100 would be 10pt
(set-face-attribute 'default nil :height 80)
;; Hightlight parenthesis
(setq show-paren-delay 0)           ; how long to wait?
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-style 'parenthesis) ; alternatives are 'parenthesis' and 'mixed'


;; Automatically reload changed files
(global-auto-revert-mode t)

;; Remove scrollbar
(scroll-bar-mode -1)

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

;; Emacs Speaks Statistics
(use-package ess
   :ensure t)
(use-package polymode
   :ensure t)
(use-package poly-markdown
  :ensure t)
(use-package poly-R
  :ensure t)
(setq ess-eval-visibly-p nil)

;; Aadd license to file headers
;(use-package lice
;  :ensure t)

;; Polymode activations
;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
;;; R modes
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))


;; Color the status menu
;; (use-package smart-mode-line
;;   :ensure t)
;; (setq sml/no-confirm-load-theme t)
;; (setq sml/theme 'dark)
;; (add-hook 'after-init-hook #'sml/setup)

;; show the cursor when moving after big movements in the window
(use-package beacon
  :ensure t)
(beacon-mode +1)

;; Hightlight current line
(use-package hlinum
  :ensure t)
(hlinum-activate)
;; Show line numbers in margin
;; (global-linum-mode t)
;; Hightlight current line
;; Old color 2d2e3a
(global-hl-line-mode)

;; Themes
(use-package nord-theme
  :ensure t
  :pin melpa)

(setq nord-uniform-mode-lines t)
(setq nord-comment-brightness 20)
(setq nord-region-highlight "frost")
;(load-theme 'nord t)
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (load-theme 'nord t)))
  (load-theme 'nord t))


;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :commands eldoc-mode)

(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)


(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook 'org-bullets-mode))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; CDLATEX
;(use-package auctex
;  :ensure t)
;;(use-package cdlatex
;;  :ensure t)

;;(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(setq org-latex-create-formula-image-program 'imagemagick)

;; Ensime stable
(use-package ensime
  :ensure t
  :pin melpa-stable)

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
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file  "~/.emacs.d/init.el")))



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
(global-set-key (kbd "<f6>") 'org-capture)

(use-package helm-descbinds
  :ensure t)
(helm-descbinds-mode)

(helm-mode 1)

;; Personal
(add-to-list 'exec-path "/usr/bin")

(add-to-list 'load-path "~/.emacs.d/elpa/ess-18.10.2")
(require 'ess-site)

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (nord)))
 '(custom-safe-themes
   (quote
    ("bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "9240e71034689655a6c05c04063af2c90d0a831aa4e7ca24c8b6e29b5a2da946" "5ed520c86d0f75a51ddce1390db509132870e465f0a9dfe4f0d8fa67ba9024f9" "7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "40da996f3246a3e99a2dff2c6b78e65307382f23db161b8316a5440b037eb72c" default)))
 '(display-time-24hr-format t)
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(ensime-eldoc-hints nil)
 '(ensime-graphical-tooltips t)
 '(ensime-implicit-gutter-icons t)
 '(ensime-server-logback "/home/hkr/.sbt/0.13/plugins/logback.xml")
 '(ensime-startup-notification nil)
 '(ess-indent-with-fancy-comments nil)
 '(ess-tab-complete-in-script t)
 '(fill-column 70)
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag nil)
 '(hl-sexp-background-color "#efebe9")
 '(linum-format " %3i ")
 '(linum-highlight-in-all-buffersp t)
 '(menu-bar f)
 '(menu-bar-mode nil)
 '(org-agenda-custom-commands
   (quote
    (("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("i" "Post to read"
      ((tags "learning"
             ((org-agenda-overriding-header "Learning")))
       (todo "READ"
             ((org-agenda-overriding-header "Pending posts"))))
      nil nil))))
 '(org-agenda-files (quote ("~/nextcloud/ORGS")))
 '(org-babel-load-languages (quote ((R . t))))
 '(org-catch-invisible-edits (quote show-and-error))
 '(org-export-backends (quote (ascii beamer html icalendar latex odt)))
 '(org-export-dispatch-use-expert-ui t)
 '(org-export-headline-levels 6)
 '(org-export-in-background nil)
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 2.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-hugo-auto-set-lastmod t)
 '(org-hugo-default-section-directory "post")
 '(org-latex-listings (quote minted))
 '(org-latex-minted-options (quote (("mathescape" "true"))))
 '(org-latex-packages-alist (quote (("outputdir=metafiles" "minted" t))))
 '(org-latex-pdf-process
   (quote
    ("mkdir %o/metafiles" "%latex -shell-escape -interaction nonstopmode -output-directory %o/metafiles %f" "%bib %b" "%latex -shell-escape -interaction nonstopmode -output-directory %o/metafiles %f" "%latex -shell-escape -interaction nonstopmode -output-directory %o/metafiles %f")))
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-log-reschedule (quote note))
 '(org-pretty-entities t)
 '(org-preview-latex-process-alist
   (quote
    ((dvipng :programs
             ("latex" "dvipng")
             :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
             (1.0 . 1.0)
             :latex-compiler
             ("latex -interaction nonstopmode -output-directory %o %f")
             :image-converter
             ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
     (dvisvgm :programs
              ("latex" "dvisvgm")
              :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :use-xcolor t :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
              (1.7 . 1.5)
              :latex-compiler
              ("latex -interaction nonstopmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f -n -b min -c %S -o %O"))
     (imagemagick :programs
                  ("latex" "convert")
                  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("pdflatex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim -antialias %f -quality 100 %O")))))
 '(org-startup-truncated nil)
 '(org-startup-with-inline-images t)
 '(org-tags-column -100)
 '(package-selected-packages
   (quote
    (lice poly-markdown ox-hugo-auto-export org-annotation-helper ox-hugo auctex ox-latex pyimport rainbow-delimiters nord-theme yatemplate shut-up buttercup ess-rutils polymode leuven-theme leuven org-bullets ess camcorder magit popup-imenu goto-chg undo-tree scala-mode which-key helm-descbinds yasnippet smartparens auto-org-md company helm-projectile use-package)))
 '(safe-local-variable-values
   (quote
    ((org-hugo-footer . "

[//]: # \"Exported with love from a post written in Org mode\"
[//]: # \"- https://github.com/kaushalmodi/ox-hugo\"")
     (eval toggle-truncate-lines 1)
     (eval add-hook
           (quote after-save-hook)
           (function org-hugo-export-wim-to-md-after-save)
           :append :local))))
 '(save-place-mode t)
 '(size-indication-mode t)
 '(sml/no-confirm-load-theme t)
 '(tool-bar-mode nil)
 '(user-full-name "Alejandro Alcalde"))
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "PfEd" :family "Source Code Pro"))))
 ;;'(hl-line ((t (:background "dim gray" :underline nil))))
;; )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
