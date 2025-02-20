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

(use-package diminish :ensure t :defer t)
(use-package no-littering :ensure t :demand t)

;;; Undo System
(use-package undo-fu :ensure t :defer t)
(use-package undo-fu-session :ensure t :defer t :hook (after-init . global-undo-fu-session-mode))

;;; Evil Mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-undo-system 'undo-fu)
  :config (evil-mode 1))

(use-package evil-collection :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config (evil-collection-init))

(use-package evil-surround :ensure t
  :after evil
  :hook (after-init . global-evil-surround-mode))

(use-package evil-commentary :ensure t
  :after evil
  :diminish evil-commentary-mode
  :hook (after-init . evil-commentary-mode))

;;; General.el
(use-package general :ensure t
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
(use-package which-key :ensure t
  :defer t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package which-key-posframe :ensure t
  :defer t
  :hook (which-key-mode . which-key-posframe-mode)
  :custom (which-key-posframe-parameters '((left-fringe . 5) (right-fringe . 5))))

(use-package base16-theme :ensure t :demand t :config (load-theme 'base16-oxocarbon-dark t))

(use-package spacious-padding :ensure t
  :defer t
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '(:internal-border-width 8
                            :mode-line-width 1
                            :tab-width 2
                            :fringe-width 6
                            :right-divider-width 10)))

(use-package rainbow-delimiters :ensure t :defer t :hook (prog-mode . rainbow-delimiters-mode))
(use-package highlight-numbers :ensure t :defer t :hook (prog-mode . highlight-numbers-mode))

(use-package popper :ensure t
  :defer t
  :hook ((after-init . popper-mode)
         (after-init . popper-echo-mode))
  :bind (("C-`"   . popper-toggle))
  :custom
  (popper-reference-buffers '("\\*.*\\*"))
  (popper-mode-line `(:eval (propertize " POP" 'face '(:foregorund ,(plist-get base16-oxocarbon-dark-theme-colors :base0D) :weight bold)))))

;;; Completion
(use-package cape :ensure t
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package corfu :ensure t
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
(use-package vertico :ensure t :hook (after-init . vertico-mode))
(use-package vertico-posframe :ensure t
  :hook (vertico-mode . vertico-posframe-mode)
  :custom (vertico-posframe-parameters '((left-fringe . 5) (right-fringe . 5))))

(use-package marginalia :ensure t :hook (after-init . marginalia-mode))

(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless substring))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult :ensure t
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (global-set-key [remap project-switch-to-buffer] 'consult-project-buffer)
  (global-set-key [remap isearch-forward] 'consult-line))

(use-package helpful :ensure t :defer t)

;;; VC
(use-package magit :ensure t :defer t)
(use-package diff-hl :ensure t
  :defer t
  :hook (after-init . global-diff-hl-mode)
  :diminish diff-hl-mode)

;;; Lang Support
(use-package nix-mode :ensure t :defer t)
(use-package zig-mode :ensure t :defer t :custom (zig-format-on-save nil))

;;; LSP Support
(use-package eglot :ensure t
  :defer t
  :hook (zig-mode . eglot-ensure)
  :config
  (set-face-attribute 'eglot-inlay-hint-face nil :height 1.0)
  (setq eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.1))

(use-package eglot-booster :ensure t
  :after eglot
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest :branch "main")
  :hook (eglot-booster-mode . eglot-ensure))

(use-package yasnippet :ensure t
  :defer t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

(use-package nerd-icons :ensure t)

;;; Diagnostics
(use-package flymake :ensure t
  :defer t
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string `((error "X" compilation-error)
                                      (warning "!" compilation-warning)
                                      (note "i" compilation-info)))
  :config
  (custom-set-faces
   '(flymake-error   ((t (:underline (:style wave :color "Red")))))
   '(flymake-warning ((t (:underline (:style wave :color "Orange")))))
   '(flymake-note    ((t (:underline (:style wave :color "Blue")))))))

(use-package eldoc :ensure t
  :defer t
  :hook (prog-mode . eldoc-mode)
  :diminish eldoc-mode)

(use-package eldoc-box :ensure t
  :defer t
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :diminish eldoc-box-hover-mode
  :custom (eldoc-idle-delay 0.1))

(use-package copilot-chat :ensure t
  :defer t
  :custom
  (copilot-chat-frontend 'shell-maker))

;;; Dired
(use-package dired
  :ensure nil
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-mouse-drag-files t)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t))

(use-package async :ensure t
  :defer t
  :hook (dired-mode . dired-async-mode))

(use-package dired-subtree :ensure t
  :defer t :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("TAB" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-remove)
        ("S-TAB" . dired-subtree-remove))
  :custom (dired-subtree-use-backgrounds nil))

;;; Terminal
(use-package eat :ensure t
  :defer t
  :hook ((eshell-mode . eat-eshell-mode)
         (eshell-mode . eat-eshell-visual-command-mode)))

(use-package eshell-syntax-highlighting :ensure t
  :defer t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eshell
  :defer t
  :hook (eshell-mode . (lambda () (eshell/alias "c" "clear-scrollback")))
  :custom
  (eshell-banner-message "")
  (eshell-prompt-function
   (lambda nil
     (concat
      (propertize (abbreviate-file-name (eshell/pwd)) 'face `(:foreground ,(plist-get base16-oxocarbon-dark-theme-colors :base09)))
      (propertize " λ" 'face `(:foreground ,(plist-get base16-oxocarbon-dark-theme-colors :base0A)))
      (propertize " ")))
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
 fringe-indicator-alist nil
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
   (:eval (propertize (format " [%s]" (substring current-input-method 0 2)) 'face '(:foreground ,(plist-get base16-oxocarbon-dark-theme-colors :base0D) :weight bold)))
   (:eval (propertize evil-mode-line-tag 'face '(:foreground ,(plist-get base16-oxocarbon-dark-theme-colors :base0E))))
   (:eval (propertize " %I " 'face '(:foreground ,(plist-get base16-oxocarbon-dark-theme-colors :base0B))))
   (:eval (propertize " %l:%c " 'face '(:foreground ,(plist-get base16-oxocarbon-dark-theme-colors :base07))))
   (:eval (propertize " %p " 'face '(:foreground ,(plist-get base16-oxocarbon-dark-theme-colors :base0D))))
   (:eval (propertize (format "%s  " (project-mode-line-format)) 'face '(:foreground ,(plist-get base16-oxocarbon-dark-theme-colors :base09) :weight bold)))
   flymake-mode-line-counters
   mode-line-format-right-align
   (:eval (propertize (format " %s " (capitalize (substring vc-mode 5))) 'face '(:foreground ,(plist-get base16-oxocarbon-dark-theme-colors :base0B) :weight bold)))
   (:eval (propertize " %b  " 'face '(:foreground ,(plist-get base16-oxocarbon-dark-theme-colors :base0A) :weight bold)))
   (:eval (propertize (capitalize (symbol-name major-mode)) 'face '(:foreground ,(plist-get base16-oxocarbon-dark-theme-colors :base0D) :weight bold)))))
