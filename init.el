;; init.el -*- lexical-binding: t -*-

;;; Package Initialization
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(setq package-quickstart t
      package-quickstart-async t
      package-native-compile t
      native-comp-async-report-warnings-errors 'silent
      native-comp-deferred-compilation t
      package-install-upgrade-built-in t)

(setq use-package-verbose nil
      use-package-expand-minimally t
      use-package-always-ensure t
      use-package-compute-statistics t
      use-package-minimum-reported-time 0.1)

(use-package diminish :defer t)
(use-package no-littering :demand t)

;;; Undo System
(use-package undo-fu :defer t)
(use-package undo-fu-session :defer t :hook (after-init . global-undo-fu-session-mode))

;;; Evil Mode
(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-undo-system 'undo-fu)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config (evil-collection-init))

(use-package evil-surround
  :after evil
  :hook (after-init . global-evil-surround-mode))

(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :hook (after-init . evil-commentary-mode))

;;; General.el
(use-package general
  :demand t
  :config
  (general-evil-setup)
  (general-def
    "C-=" 'text-scale-increase
    "C--" 'text-scale-decrease
    "C-0" (lambda () (interactive) (text-scale-set 0)))

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    "x" '(execute-extended-command :which-key "M-x")
    ":" '(eval-expression :which-key "M-:")
    "r" '(restart-emacs :which-key "restart")
    "q" '(kill-emacs :which-key "exit")
    "i" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init")

    "b" '(:ignore t :which-key "buffer")
    "b d" 'kill-current-buffer
    "b b" 'consult-buffer
    "b p" 'previous-buffer
    "b n" 'next-buffer

    "w" '(:ignore t :which-key "window")
    "w c" 'delete-window
    "w v" 'evil-window-vsplit
    "w s" 'evil-window-split
    "w n" 'evil-window-next
    "w p" 'evil-window-prev

    "h" '(:ignore t :which-key "help")
    "h f" 'helpful-callable
    "h k" 'helpful-key
    "h o" 'helpful-symbol

    "f" '(:ignore t :which-key "file")
    "f s" 'save-buffer
    "f f" 'find-file
    "f g" 'consult-find
    "f G" 'consult-ripgrep
    "f r" 'consult-recent-file
    "f p" 'project-find-file
    "f P" 'project-switch-project
    "f d" 'dired

    "e" 'eshell
    "g" '(magit-status :which-key "git")
    "c" '(copilot-chat-display :which-key "chat")

    "l" '(:ignore t :which-key "lsp")
    "l f" 'eglot-format-buffer
    "l r" 'eglot-rename
    "l a" 'eglot-code-actions
    "l d" 'eglot-find-declaration
    "l i" 'eglot-find-implementation
    "l D" 'eglot-find-typeDefinition
    "l m" 'consult-flymake
    "l h" 'eldoc-box-help-at-point))

(global-set-key (kbd "<escape>") 'keyboard-quit)

;;; Visual Elements
(use-package which-key
  :defer t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package which-key-posframe
  :defer t
  :hook (which-key-mode . which-key-posframe-mode)
  :custom (which-key-posframe-parameters '((left-fringe . 5) (right-fringe . 5))))

(use-package base16-theme :demand t :config (load-theme 'base16-oxocarbon-dark t))

(use-package spacious-padding
  :defer t
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '(:internal-border-width 8
                            :mode-line-width 1
                            :tab-width 2
                            :left-fringe-width 4
                            :right-divider-width 10)))

(use-package rainbow-delimiters :defer t :hook (prog-mode . rainbow-delimiters-mode))
(use-package highlight-numbers :defer t :hook (prog-mode . highlight-numbers-mode))

(use-package popper
  :defer t
  :hook ((after-init . popper-mode)
         (after-init . popper-echo-mode))
  :bind (("C-`"   . popper-toggle))
  :config
  (setq popper-reference-buffers '("\\*.*\\*")
        popper-mode-line `(:eval (propertize " POP" 'face '(:foregorund ,(face-foreground 'font-lock-warning-face) :weight bold)))))

;;; Completion
(use-package cape
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package corfu
  :hook ((after-init . global-corfu-mode)
         (after-init . corfu-history-mode)
         (after-init . corfu-popupinfo-mode))
  :config
  (setq tab-always-indent 'complete
        corfu-preview-current nil
        corfu-min-width 4
        corfu-auto t
        corfu-cycle t
        corfu-popupinfo-delay '(0.5 . 0.25)))

;;; Minibuffer
(use-package vertico :hook (after-init . vertico-mode))
(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode)
  :custom (vertico-posframe-parameters '((left-fringe . 5) (right-fringe . 5))))

(use-package marginalia :hook (after-init . marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless substring)
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (global-set-key [remap project-switch-to-buffer] 'consult-project-buffer)
  (global-set-key [remap isearch-forward] 'consult-line))

(use-package helpful :defer t)

;;; VC
(use-package magit :defer t)
(use-package transient-posframe :defer t :hook (after-init . transient-posframe-mode))
(use-package diff-hl
  :defer t
  :hook (after-init . global-diff-hl-mode)
  :diminish diff-hl-mode)

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;;; Lang Support
(use-package nix-mode :defer t)
(use-package zig-mode :defer t :custom (zig-format-on-save nil))

;;; LSP Support
(use-package eglot
  :defer t
  :hook (zig-mode . eglot-ensure)
  :config
  (set-face-attribute 'eglot-inlay-hint-face nil :height 1.0)
  (setq eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.1))

(use-package eglot-booster
  :after eglot
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest :branch "main")
  :hook (eglot-booster-mode . eglot-ensure))

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

;;; Diagnostics
(use-package flymake
  :defer t
  :config
  (custom-set-faces
   '(flymake-error   ((t (:underline (:style wave :color "Red")))))
   '(flymake-warning ((t (:underline (:style wave :color "Orange")))))
   '(flymake-note    ((t (:underline (:style wave :color "Blue")))))))

(use-package eldoc
  :defer t
  :hook (prog-mode . eldoc-mode)
  :diminish eldoc-mode)

(use-package eldoc-box
  :defer t
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :diminish eldoc-box-hover-mode
  :custom (eldoc-idle-delay 0.1))

(use-package copilot-chat
  :defer t
  :custom
  (copilot-chat-frontend 'shell-maker))

;;; Dired
(use-package dired
  :ensure nil
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-mouse-drag-files t
        delete-by-moving-to-trash t
        dired-dwim-target t))

(use-package async
  :defer t
  :hook (dired-mode . dired-async-mode))

(use-package dired-subtree
  :defer t :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("TAB" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-remove)
        ("S-TAB" . dired-subtree-remove))
  :custom (dired-subtree-use-backgrounds nil))

;;; Terminal
(use-package eat
  :defer t
  :hook ((eshell-mode . eat-eshell-mode)
         (eshell-mode . eat-eshell-visual-command-mode)))

(use-package eshell-syntax-highlighting
  :defer t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eshell
  :defer t
  :hook (eshell-mode . (lambda () (eshell/alias "c" "clear-scrollback")))
  :config
  (setq eshell-banner-message ""
        eshell-prompt-function
        (lambda nil
          (let ((dir-color (face-attribute 'font-lock-keyword-face :foreground))
                (prompt-color (face-attribute 'font-lock-builtin-face :foreground)))
            (concat
             (propertize (abbreviate-file-name (eshell/pwd)) 'face `(:foreground ,dir-color))
             (propertize " λ" 'face `(:foreground ,prompt-color))
             (propertize " "))))
        ))

;;; Global Modes
(dolist (mode '(global-hl-line-mode
                global-auto-revert-mode
                global-so-long-mode
                global-prettify-symbols-mode
                electric-pair-mode
                recentf-mode
                size-indication-mode
                column-number-mode
                pixel-scroll-precision-mode
                savehist-mode
                save-place-mode
                delete-selection-mode))
  (funcall mode 1))

(add-hook 'prog-mode-hook 
          (lambda () 
            (setq display-line-numbers 'relative)
            (when (> (buffer-size) 100000)
              (display-line-numbers-mode -1))))

;;; Defaults
(setq-default
 ;; Emacs behavior
 confirm-kill-emacs nil
 confirm-kill-processes nil
 use-short-answers t

 ;; Editing behavior
 indent-tabs-mode nil
 tab-width 4
 require-final-newline nil
 backward-delete-char-untabify-method 'hungry
 truncate-lines t
 word-wrap t
 line-move-visual nil

 ;; Display settings
 display-time-default-load-average nil
 fringes-outside-margins nil
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 line-spacing 0.08
 cursor-type 'bar
 cursor-in-non-selected-windows nil

 ;; File handling
 create-lockfiles nil
 delete-by-moving-to-trash t
 make-backup-files nil
 auto-save-default nil
 auto-save-interval 2000
 auto-save-timeout 20
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 vc-make-backup-files nil
 vc-follow-symlinks t
 find-file-visit-truename nil

 ;; Auto-revert settings
 auto-revert-verbose nil
 auto-revert-interval 1
 auto-save-no-message t
 global-auto-revert-non-file-buffers t

 ;; Completion settings
 completion-ignore-case t

 ;; Scrolling behavior
 scroll-margin 3
 scroll-conservatively 100000
 scroll-preserve-screen-position t
 scroll-step 5
 auto-window-vscroll nil

 ;; Miscellaneous
 ad-redefinition-action 'accept)

;;; Personal Info
(setq user-full-name "Mori Zen"
      user-mail-address "71zenith@proton.me"
      default-input-method "japanese"
      display-time-format "%a %d %b %H:%M"
      calendar-week-start-day 1)

(setq-default
 mode-line-right-align-edge 'right-fringe
 mode-line-format
 `(
   (:eval (propertize (format " [%s]" (substring current-input-method 0 2)) 'face '(:foreground ,(face-foreground 'font-lock-preprocessor-face) :weight bold)))
   (:eval (propertize evil-mode-line-tag 'face '(:foreground ,(face-foreground 'font-lock-regexp-grouping-construct))))
   (:eval (propertize " %I " 'face '(:foreground ,(face-foreground 'elisp-shorthand-font-lock-face))))
   (:eval (propertize " %l:%c " 'face '(:foreground ,(face-foreground 'font-lock-type-face))))
   (:eval (propertize " %p " 'face '(:foreground ,(face-foreground 'font-lock-builtin-face))))
   (:eval (propertize (format "%s  " (project-mode-line-format)) 'face '(:foreground ,(face-foreground 'font-lock-string-face) :weight bold)))
   flymake-mode-line-counters
   mode-line-format-right-align
   (:eval (propertize (format " %s " (capitalize (substring vc-mode 5))) 'face '(:foreground ,(face-foreground 'font-lock-string-face) :weight bold)))
   (:eval (propertize " %b  " 'face '(:foreground ,(face-foreground 'font-lock-type-face) :weight bold)))
   (:eval (propertize (capitalize (symbol-name major-mode)) 'face '(:foreground ,(face-foreground 'font-lock-function-name-face) :weight bold)))
   ))
