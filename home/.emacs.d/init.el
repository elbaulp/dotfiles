;; global variables
;;; Code:
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-trailing-whitespace t
 sentence-end-double-space nil)
(setenv "TZ" "Europe/Berlin")


;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; global keybindings
(global-unset-key (kbd "C-z"))


;; Straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;
;; UI stuff ;;
;;;;;;;;;;;;;;

(use-package rainbow-delimiters
  :straight t
  :commands (rainbow-delimiters-mode)
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Font size in 1/10pt, so 100 would be 10pt
(set-face-attribute 'default nil :height 130)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; show the cursor when moving after big movements in the window
(use-package beacon
  :config
  (beacon-mode +1)
  :straight t)


;; Hightlight current line
(use-package hlinum
  :straight (:host github :repo "tom-tan/hlinum-mode")
  :hook (hlinum-activate global-hl-line-mode)
  :config
  '(linum-highlight-in-all-buffersp t)
)

;; Themes
(use-package material-theme)


;;;;;;;;;;;;;;;;;;;;;;
;; USEFULL PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;;


;; Multiple cursors
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-m C-S-m") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-w >") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "C-S-w <") 'mc/mark-previous-like-this-word)
  (global-set-key (kbd "C-S-w *") 'mc/mark-all-words-like-this)
  (global-set-key (kbd "C-S-s >") 'mc/mark-next-like-this-symbol)
  (global-set-key (kbd "C-S-s <") 'mc/mark-previous-like-this-symbol)
  (global-set-key (kbd "C-S-s *") 'mc/mark-all-symbol-like-this)
  (global-set-key (kbd "C-S-c *") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-l <") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-S-l >") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-S-m *") 'mc/mark-all-dwim)
  (global-set-key (kbd "C-S-r s") 'mc/sort-regions)

  :straight t)

;;;;;;;;;;;;
;; KOTLIN ;;
;;;;;;;;;;;;
(use-package kotlin-mode
  :straight t)

;;;;;;;;;;
;; YAML ;;
;;;;;;;;;;
(use-package yaml-mode
  :straight (:host github :repo "yoshiki/yaml-mode")
)

;;;;;;;;;;;;;;;
;; EMACS-LSP ;;
;;;;;;;;;;;;;;;
(use-package lsp-mode
  :hook (
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . yas-minor-mode)
         (lsp-mode . yas-global-mode)
         )
  :commands lsp
  :config
  (setq lsp-keymap-prefix "C-c v")
  (define-key lsp-mode-map (kbd "C-c v") lsp-command-map)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'kotlin-mode-hook #'lsp)
  (add-hook 'dockerfile-mode-hook #'lsp)
  (add-hook 'yaml-mode-hook #'lsp)
  :straight t)

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :straight t)
(use-package company-lsp
  :straight t
  :commands company-lsp)
(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

;; if you are helm user
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  :straight t)

(use-package yasnippet
  :straight t)

(setq company-minimum-prefix-length 1
      company-idle-delay 0.2) ;; default is 0.2

(setq lsp-prefer-capf t)

;;;;;;;;;;;;;
;; HASKELL ;;
;;;;;;;;;;;;;
(use-package haskell-mode
  :init
  (require 'haskell-interactive-mode)
  (require 'haskell-process)

  :config
  (interactive-haskell-mode)
  (define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

  (define-key haskell-mode-map (kbd "M-SPC") 'company-complete)
  (eval-after-load "haskell-mode"
    '(define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc))
  (eval-after-load "haskell-mode"
    '(define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at))
  (eval-after-load "which-function"
    '(add-to-list 'which-func-modes 'haskell-mode))

  (eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

  (eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))
  :straight t)
;;;;;;;;;;;;;;;;;
;; END HASKELL ;;
;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; DOCKER ;;
;;;;;;;;;;;;
(use-package dockerfile-mode
  :straight t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(use-package docker
  :straight t
  :bind (("C-c d" . docker)))


;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;


;;;;;;;;;;;;;;
;; org-roam ;;
;;;;;;;;;;;;;;
(use-package org-roam
  :straight t
  :config
  (add-hook 'after-init-hook 'org-roam-mode)
  :init
  (setq org-roam-capture-templates
        '(
          ("e" "Experiment" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>"
           :head "#+title: ${title}\n#+roam_tags:\n#+roam_alias:\n#+roam_key:\n* Source\n\n* Relevant Notes\n* Summary\n"
           :unnarrowed t)
          ))
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Nextcloud/org-roam")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))
(require 'org-roam-protocol)

(use-package magit
  :bind (("C-c g" . magit-file-dispatch)
         ("C-c M-g" . magit-dispatch)
         ("C-x g" . magit-status))
  :config
                                        ; Disable built in vc integration in emacs
  (setq vc-handled-backends nil)
  :straight t)

(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode)

(use-package org-bullets
  :config (add-hook 'org-mode-hook 'org-bullets-mode)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-preview-latex-default-process 'imagemagick)
)

;; Projectile
;; http://batsov.com/projectile/
(use-package projectile
  :demand
  :init   (setq projectile-use-git-grep t)
  :config
  (setq projectile-require-project-root nil)
  (projectile-mode t)
  :bind   (
           ("s-F" . projectile-grep)
           ("C-c p" . projectile-command-map)
           )
)

;; IBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Which-key
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
)

;; helm
(use-package helm
  :config
  (global-unset-key (kbd "C-c C-j"))
  (global-unset-key (kbd "C-x c"))
  :straight t)
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
(global-set-key (kbd "C-c C-j") 'helm-imenu)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq helm-split-window-inside-p            t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
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

(use-package helm-descbinds)
(helm-descbinds-mode)

(helm-mode 1)

;; Personal
(add-to-list 'exec-path "/usr/bin")
(add-to-list 'exec-path "/usr/local/bin" t)

;; Hightlight Symbols
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("M-6" . highlight-symbol)
  :bind ("M-7" . highlight-symbol-next)
  :bind ("M-8" . highlight-symbol-prev)
  :bind ("M-9" . highlight-symbol-query-replace))

;; Goto last change
(use-package goto-chg
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

;; Popup Summary
(use-package popup-imenu
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

;; Smart Parentheses
(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
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

;;;;;;;;;;;;;;;;;;;
;; PYTHON CONFIG ;;
;;;;;;;;;;;;;;;;;;;
(use-package blacken
  :straight t
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '100))

(use-package lsp-python-ms
  :straight t
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

(add-hook 'python-mode-hook #'lsp)

(use-package pyenv-mode
  :straight t
  :hook (python-mode . pyenv-mode))

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

(use-package py-isort
    :straight (:host github :repo "paetzke/py-isort.el")
    :config
    (add-hook 'before-save-hook 'py-isort-before-save)
    (setq py-isort-options '("--lines=100" "-m=3" "-tc" "-fgw=0" "-ca")))


;;;;;;;;;;;;;;;;;;;;;;;
;; END PYTHON CONFIG ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package htmlize
    :straight (:host github :repo "hniksic/emacs-htmlize"))

;;;;;;;;;;;
;; HOOKS ;;
;;;;;;;;;;;
;; Clean whitespaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENABLE DESIRED MODES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hightlight parenthesis
(show-paren-mode t)
;; Automatically reload changed files
(global-auto-revert-mode t)
;; Remove scrollbar
(scroll-bar-mode -1)
;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically generated    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blacken-executable "/Users/alejandro/.pyenv/shims/black")
 '(blacken-line-length 100)
 '(column-number-mode t)
 '(cursor-type 'hbar)
 '(custom-safe-themes
   '("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" default))
 '(display-time-24hr-format t)
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(doc-view-continuous t)
 '(explicit-shell-file-name "/bin/zsh")
 '(fci-rule-color "#37474f")
 '(fill-column 70)
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag nil)
 '(haskell-mode-hook
   '(flyspell-prog-mode haskell-decl-scan-mode haskell-indentation-mode imenu-add-menubar-index interactive-haskell-mode haskell-auto-insert-module-template))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-stylish-on-save t)
 '(hl-sexp-background-color "#efebe9")
 '(linum-format " %3i ")
 '(linum-highlight-in-all-buffersp t)
 '(lsp-completion-enable t)
 '(lsp-keymap-prefix "C-c v")
 '(lsp-pyls-configuration-sources ["pycodestyle"])
 '(lsp-pyls-plugins-jedi-use-pyenv-environment t)
 '(lsp-pyls-plugins-pycodestyle-max-line-length 100)
 '(lsp-pyls-plugins-pydocstyle-enabled t)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-doc-use-webkit t)
 '(magit-blame-styles
   '((headings
      (heading-format . "%-20a %C %s
"))
     (margin
      (margin-format " %s%f" " %C %a" " %H")
      (margin-width . 42)
      (margin-face . magit-blame-margin)
      (margin-body-face magit-blame-dimmed))
     (highlight
      (highlight-face . magit-blame-highlight))
     (lines
      (show-lines . t)
      (show-message . t))))
 '(magit-diff-arguments
   '("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "--stat"))
 '(magit-section-visibility-indicator '(magit-fringe-bitmap-bold> . magit-fringe-bitmap-boldv))
 '(menu-bar f)
 '(menu-bar-mode nil)
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("i" "Post to read"
      ((tags "learning"
             ((org-agenda-overriding-header "Learning")))
       (todo "READ"
             ((org-agenda-overriding-header "Pending posts"))))
      nil nil)))
 '(org-agenda-files '("~/Nextcloud/ORGS"))
 '(org-babel-load-languages '((R . t)))
 '(org-capture-templates
   '(("e" "Experiment" plain #'org-roam--capture-get-point "Exp %?" :unnarrowed t :org-roam
      (:head "#+TITLE: ${title}
" :file-name "%<%Y%m%d%H%M%S>-${slug}exp"))))
 '(org-catch-invisible-edits 'show-and-error)
 '(org-export-backends '(ascii beamer html icalendar latex odt))
 '(org-export-dispatch-use-expert-ui t)
 '(org-export-headline-levels 6)
 '(org-export-in-background nil)
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 2.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-html-htmlize-output-type 'css)
 '(org-html-preamble "{{{ghlink(elbaulp)}}}")
 '(org-hugo-auto-set-lastmod t)
 '(org-hugo-section "post")
 '(org-latex-listings 'minted)
 '(org-latex-minted-options '(("mathescape" "true")))
 '(org-latex-packages-alist '(("outputdir=metafiles" "minted" t)))
 '(org-latex-pdf-process
   '("mkdir %o/metafiles" "%latex -shell-escape -interaction nonstopmode -output-directory %o/metafiles %f" "%bib %b" "%latex -shell-escape -interaction nonstopmode -output-directory %o/metafiles %f" "%latex -shell-escape -interaction nonstopmode -output-directory %o/metafiles %f"))
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-log-reschedule 'note)
 '(org-pretty-entities t)
 '(org-preview-latex-process-alist
   '((dvipng :programs
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
                  ("convert -density %D -trim -antialias %f -quality 100 %O"))))
 '(org-roam-buffer-prepare-hook
   '(org-roam-buffer--insert-title org-roam-buffer--insert-backlinks org-roam-buffer--insert-citelinks))
 '(org-roam-buffer-width 0.2)
 '(org-roam-completion-system 'helm)
 '(org-roam-directory "~/Nextcloud/org-roam")
 '(org-roam-graph-viewer "firefox-bin")
 '(org-roam-link-title-format "rl:%s")
 '(org-roam-mode t nil (org-roam))
 '(org-roam-tag-sources '(prop all-directories))
 '(org-startup-truncated nil)
 '(org-startup-with-inline-images t)
 '(org-tags-column -100)
 '(package-selected-packages
   '(yaml-mode python-docstring py-docformatter py-autoflake py-isort dockerfile-mode kotlin-mode auto-package-update lsp-treemacs company-lsp ox-hugo-auto-export org-annotation-helper ox-hugo auctex ox-latex pyimport rainbow-delimiters yatemplate shut-up buttercup ess-rutils org-bullets ess camcorder magit popup-imenu goto-chg which-key helm-descbinds yasnippet smartparens auto-org-md helm-projectile use-package))
 '(safe-local-variable-values
   '((dockerfile-image-name . "spark-locak")
     (dockerfile-image-name . "local-image-spark")
     (org-hugo-footer . "

[//]: # \"Exported with love from a post written in Org mode\"
[//]: # \"- https://github.com/kaushalmodi/ox-hugo\"")
     (eval toggle-truncate-lines 1)
     (eval add-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :append :local)))
 '(save-place-mode t)
 '(size-indication-mode t)
 '(sml/no-confirm-load-theme t)
 '(tool-bar-mode nil)
 '(user-full-name "Alejandro Alcalde")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a")))
 '(vc-annotate-very-old-color nil))
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
 '(cursor ((t (:background "chocolate"))))
 '(haskell-interactive-face-prompt ((t (:inherit font-lock-function-name-face :foreground "#ff8700"))))
 '(org-roam-link ((t (:background "deep sky blue" :foreground "white"))))
 '(org-roam-link-current ((t (:background "deep sky blue" :foreground "white" :slant italic :weight ultra-bold))))
 '(term-color-white ((t (:background "#263238" :foreground "light green")))))
(put 'set-goal-column 'disabled nil)
(provide 'init)
;;; init.el ends here
