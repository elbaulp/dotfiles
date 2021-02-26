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
(global-prettify-symbols-mode t)

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


;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO RELATED PACKAGE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(use-package ido-vertical-mode
  :straight t
  :config
  (ido-mode 1)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-show-count t)
)

(use-package ido-completing-read+
  :straight t
  :config
  (ido-ubiquitous-mode 1)
  (setq magit-completing-read-function 'magit-ido-completing-read)
)

(use-package amx
  :straight t
  :config
  (amx-mode t)
)


;; Themes
(use-package material-theme
  :straight t
  :config
  (load-theme 'material-light t))


;;;;;;;;;;;;;;;;;;;;;;
;; USEFULL PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors
(use-package multiple-cursors
  :config
  ;; https://github.com/magnars/multiple-cursors.el
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
  (global-set-key (kbd "C-S-r r") 'mc/reverse-regions)
  (global-set-key (kbd "C-S-r n") 'mc/insert-numbers)
  (global-set-key (kbd "C-S-r a") 'set-rectangular-region-anchor)
  (global-set-key (kbd "C-S-r p") 'mc/mark-sgml-tag-pair)
  (global-set-key (kbd "C-S-u n") 'mc/unmark-next-like-this)
  (global-set-key (kbd "C-S-u p") 'mc/unmark-previous-like-this)
  (global-set-key (kbd "C-S-u s") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-S-u r") 'mc/skip-to-previous-like-this)

  :straight t)

(use-package csv-mode
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
  (add-hook 'scala-mode-hook #'lsp)
  (add-hook 'dockerfile-mode-hook #'lsp)
  (add-hook 'yaml-mode-hook #'lsp)
  :straight t)

(load "~/.emacs.d/scala.el")

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :straight t)
(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package company
  :straight t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; (use-package dap-mode
;;   :straight t
;;   :config
;;   (require 'dap-python)
;;   (dap-register-debug-template "My App"
;;   (list :type "python"
;;         :args "-i"
;;         :cwd nil
;;         :env '(("DEBUG" . "1"))
;;         :target-module (expand-file-name "~/moia-dev/data-cruncher/.venv/bin/crunch")
;;         :request "launch"
;;         :name "My App"))
;;   (add-hook 'dap-stopped-hook
;;           (lambda (arg) (call-interactively #'dap-hydra)))
;; )

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
  (setq org-roam-completion-system 'ido)
  :init
  (setq org-roam-capture-templates
        '(("e" "Experiment" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>"
           :head "#+title: ${title}\n#+roam_tags:\n#+roam_alias:\n#+roam_key:\n* Source\n\n* Relevant Notes\n* Summary\n"
           :unnarrowed t)
          ))
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Development/mein-zettelkasten")
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

(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "<f6>") 'org-capture)
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file  "~/.emacs.d/init.el")))

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
                          (lsp)))
  :init
  (setq lsp-python-ms-executable (executable-find "python-language-server")))

(add-hook 'python-mode-hook #'lsp)

(use-package py-isort
    :straight (:host github :repo "paetzke/py-isort.el")
    :config
    (add-hook 'before-save-hook 'py-isort-before-save)
    (setq py-isort-options '("-m=3" "-tc" "-fgw=0" "-ca")))


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
 '(blacken-executable "/Users/alejandro/.pyenv/shims/black")
 '(column-number-mode t)
 '(cursor-type (quote hbar))
 '(custom-safe-themes
   (quote
    ("afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" default)))
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
   (quote
    (flyspell-prog-mode haskell-decl-scan-mode haskell-indentation-mode imenu-add-menubar-index interactive-haskell-mode haskell-auto-insert-module-template)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-stylish-on-save t)
 '(hl-sexp-background-color "#efebe9")
 '(ido-enable-flex-matching t)
 '(linum-format " %3i ")
 '(linum-highlight-in-all-buffersp t)
 '(lsp-completion-enable t)
 '(lsp-keymap-prefix "C-c v")
 '(lsp-pyls-configuration-sources ["pycodestyle"])
 '(lsp-pyls-plugins-pycodestyle-max-line-length 100)
 '(lsp-pyls-plugins-pydocstyle-enabled t)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-position (quote bottom))
 '(lsp-ui-doc-use-webkit t)
 '(lsp-ui-flycheck-list-position (quote right))
 '(magit-blame-styles
   (quote
    ((headings
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
      (show-message . t)))))
 '(magit-diff-arguments
   (quote
    ("--ignore-space-change" "--ignore-all-space" "--no-ext-diff" "--stat")))
 '(magit-section-visibility-indicator
   (quote
    (magit-fringe-bitmap-bold> . magit-fringe-bitmap-boldv)))
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
 '(org-agenda-files (quote ("~/Nextcloud/ORGS")))
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
 '(org-html-htmlize-output-type (quote css))
 '(org-hugo-auto-set-lastmod t)
 '(org-hugo-section "post")
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
 '(org-roam-buffer-prepare-hook
   (quote
    (org-roam-buffer--insert-title org-roam-buffer--insert-backlinks org-roam-buffer--insert-citelinks)))
 '(org-roam-buffer-width 0.2)
 '(org-roam-directory "~/Development/mein-zettelkasten")
 '(org-roam-graph-viewer "firefox-bin")
 '(org-roam-link-title-format "rl:%s")
 '(org-roam-mode t nil (org-roam))
 '(org-roam-tag-sources (quote (prop all-directories)))
 '(org-startup-truncated nil)
 '(org-startup-with-inline-images t)
 '(org-tags-column -100)
 '(package-selected-packages
   (quote
    (yaml-mode python-docstring py-docformatter py-autoflake py-isort dockerfile-mode kotlin-mode auto-package-update lsp-treemacs ox-hugo-auto-export org-annotation-helper ox-hugo auctex ox-latex pyimport rainbow-delimiters yatemplate shut-up buttercup ess-rutils org-bullets ess camcorder magit popup-imenu goto-chg which-key yasnippet smartparens auto-org-md use-package)))
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((dockerfile-image-name . "spark-locak")
     (dockerfile-image-name . "local-image-spark")
     (org-hugo-footer . "

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
 '(user-full-name "Alejandro Alcalde")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
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
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil)
 '(which-key-frame-max-width 60)
 '(which-key-side-window-max-width 0.5))
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
 '(org-roam-link-current ((t (:background "deep sky blue" :foreground "white" :slant italic :weight ultra-bold)))))
(put 'set-goal-column 'disabled nil)
(provide 'init)
;;; init.el ends here
