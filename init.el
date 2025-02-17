;;; init.el -*- lexical-binding: t -*-

;;; Package Initialization
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(setopt package-install-upgrade-built-in t)
(setq use-package-verbose nil
      use-package-expand-minimally t
      use-package-always-ensure t
      use-package-compute-statistics t
      use-package-minimum-reported-time 0.1
      package-native-compile t)

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

(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :hook (after-init . global-evil-surround-mode))

(use-package evil-commentary
  :ensure t
  :after evil
  :diminish evil-commentary-mode
  :hook (after-init . evil-commentary-mode))


;;; General.el
(use-package general
  :ensure t
  :config
  (general-evil-setup)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    "x" '(execute-extended-command :which-key "M-x")
    ":" '(eval-expression :which-key "M-:")
    "r" 'restart-emacs
    "i" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init file")

    "b" '(:ignore t :which-key "buffer")
    "b d" 'kill-current-buffer
    "b b" 'consult-buffer
    "b p" 'previous-buffer
    "b n" 'next-buffer

    "h" '(:ignore t :which-key "help")
    "h f" 'helpful-callable
    "h k" 'helpful-key
    "h o" 'helpful-symbol

    "f" '(:ignore t :which-key "file")
    "f s" 'save-buffer
    "f f" 'find-file
    "f r" 'consult-recent-file
    "f p" 'project-switch-project
    "f d" 'dired

    "e" 'eshell
    "g" 'magit-status

    "c" '(:ignore t :which-key "lsp")
    "c f" 'eglot-format-buffer
    "c r" 'eglot-rename
    "c a" 'eglot-code-actions
    "c d" 'eglot-find-declaration
    "c D" 'eglot-find-typeDefinition
    "c m" 'consult-flymake
    "c h" 'eldoc-box-help-at-point
    ))

(global-set-key (kbd "<escape>") 'keyboard-quit)


;;; Visual Elements
(use-package which-key
  :ensure t :defer t
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.25))

(use-package which-key-posframe :ensure t :defer t :hook (which-key-mode . which-key-posframe-mode))

(use-package base16-theme :ensure t :init (load-theme 'base16-oxocarbon-dark t))

(use-package spacious-padding
  :ensure t :defer t
  :hook (after-init . spacious-padding-mode)
  :custom
  (spacious-padding-widths
   '(:internal-border-width 8
			                :mode-line-width 1
			                :tab-width 2
			                :left-fringe-width 4
			                :right-divider-width 10)))

(use-package rainbow-delimiters :ensure t :defer t :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers :ensure t :defer t :hook (prog-mode . highlight-numbers-mode))

(use-package popwin
  :ensure t :defer t
  :hook (after-init . popwin-mode)
  :config
  (push '("^\*.*\*$" :regexp t) popwin:special-display-config))

(use-package keycast
  :ensure t :defer t
  :hook (after-init . keycast-mode-line-mode)
  :config
  (setq keycast-mode-line-format "%2s%K%C%R "
        keycast-mode-line-remove-tail-elements nil
        keycast-mode-line-insert-after 'mode-line-end-spaces)
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…"))))


;;; Completion
(use-package cape
  :ensure t
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package corfu
  :ensure t
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
(use-package vertico-posframe :ensure t :hook (vertico-mode . vertico-posframe-mode))
(use-package marginalia :ensure t :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless substring )
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap load-theme] 'consult-theme)
  (global-set-key [remap recentf] 'consult-recent-file)
  (global-set-key [remap project-switch-to-buffer] 'consult-project-buffer)
  (global-set-key [remap isearch-forward] 'consult-line))


;;; Help Enhancement
(use-package helpful
  :ensure t :defer t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-command] . helpful-command)
         ([remap describe-symbol] . helpful-symbol)))


;;; Org Mode
(use-package org
  :ensure t
  :config
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-edit-src-content-indentation 0
        org-return-follows-link t))


;;; VC
(use-package magit :ensure t :defer t)
(use-package transient-posframe :ensure t :defer t :hook (after-init . transient-posframe-mode))
(use-package git-gutter
  :ensure t :defer t
  :hook (after-init . global-git-gutter-mode)
  :diminish git-gutter-mode)


;;; Lang Support
(use-package nix-mode :ensure t :defer t)
(use-package zig-mode :ensure t :defer t :custom (zig-format-on-save nil))


;;; LSP Support
(use-package eglot
  :ensure t :defer t
  :hook (zig-mode . eglot-ensure)
  :config
  (set-face-attribute 'eglot-inlay-hint-face nil :height 1.0)
  (setq eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.1))

(use-package eglot-booster
  :after eglot
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest :branch "main")
  :hook (eglot-booster-mode . eglot-ensure))

(use-package flymake
  :ensure t :defer t
  :config
  (custom-set-faces
   '(flymake-error   ((t (:underline (:style wave :color "Red")))))
   '(flymake-warning ((t (:underline (:style wave :color "Orange")))))
   '(flymake-note    ((t (:underline (:style wave :color "Blue")))))))

(use-package yasnippet
  :ensure t :defer t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

(use-package eldoc
  :ensure t :defer t
  :hook (prog-mode . eldoc-mode)
  :diminish eldoc-mode)

(use-package eldoc-box
  :ensure t :defer t
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :diminish eldoc-box-hover-mode
  :config
  (setq eldoc-idle-delay 0.1))


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
  :ensure t :defer t
  :hook (dired-mode . dired-async-mode))

(use-package dired-subtree
  :ensure t :defer t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil))


;;; Terminal
(use-package eat
  :ensure t :defer t
  :hook ((eshell-mode . eat-eshell-mode)
         (eshell-mode . eat-eshell-visual-command-mode)))

(use-package eshell-syntax-highlighting
  :ensure t :defer t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eshell
  :ensure t :defer t
  :config
  (setq eshell-banner-message ""
        eshell-prompt-function
        (lambda nil
          (let ((dir-color (face-attribute 'font-lock-keyword-face :foreground))
                (prompt-color (face-attribute 'font-lock-builtin-face :foreground)))
            (concat
             (propertize (abbreviate-file-name (eshell/pwd)) 'face `(:foreground ,dir-color))
             (propertize " λ" 'face `(:foreground ,prompt-color))
             (propertize " "))))))


;;; Global Modes
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(global-so-long-mode 1)
(global-prettify-symbols-mode 1)
(electric-pair-mode 1)
(recentf-mode 1)
(size-indication-mode 1)
(pixel-scroll-precision-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(delete-selection-mode 1)


;;; Defaults
(setq-default confirm-kill-emacs nil
              ad-redefinition-action 'accept
              display-time-default-load-average nil
              display-line-numbers 'relative
              confirm-kill-processes nil
              indent-tabs-mode nil
              tab-width 4
              require-final-newline nil
              use-short-answers t
              fringes-outside-margins nil
              indicate-buffer-boundaries nil
              indicate-empty-lines nil
              create-lockfiles nil
              auto-revert-verbose nil
              auto-revert-interval 1
              auto-save-no-message t
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
              line-spacing 0.08
              global-auto-revert-non-file-buffers t
              completion-ignore-case t
              cursor-in-non-selected-windows nil
              find-file-visit-truename nil
              ad-redefinition-action 'accept
              debug-on-error nil
              scroll-margin 3
              scroll-conservatively 100000
              scroll-preserve-screen-position t
              scroll-step 5
              auto-window-vscroll nil
              backward-delete-char-untabify-method 'hungry
              redisplay-skip-fontification-on-input t
              truncate-lines t
              vc-follow-symlinks t
              word-wrap t
              cursor-type 'bar
              cursor-in-non-selected-windows nil
              line-move-visual nil)

;;; Server
(use-package server
  :ensure nil :defer t
  :config (unless (server-running-p) (server-start)))


;;; Personal Info
(setq user-full-name "Mori Zen"
      user-mail-address "71zenith@proton.me"
      default-input-method "japanese"
      display-time-format "%a %d %b %H:%M"
      calendar-week-start-day 1)
