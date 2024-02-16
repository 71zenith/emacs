;; -*- lexical-binding: t; -*-
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil
			      :files (:defaults "elpaca-test.el" (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		 ((zerop (call-process "git" nil buffer t "clone"
				       (plist-get order :repo) repo)))
		 ((zerop (call-process "git" nil buffer t "checkout"
				       (or (plist-get order :ref) "--"))))
		 (emacs (concat invocation-directory invocation-name))
		 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
				       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		 ((require 'elpaca))
		 ((elpaca-generate-autoloads "elpaca" repo)))
	    (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	  (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(elpaca-wait)

(use-package vertico
  :bind (:map vertico-map
              ("C-h" . left-char)
              ("C-l" . right-char)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :config
  (setq vertico-resize nil
        vertico-cycle t)
  :init
  (vertico-mode t))

(use-package pulsar
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)
  (pulsar-global-mode 1))


(use-package marginalia
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode t))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preview-current 'insert)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5)
  (setq corfu-popupinfo-delay nil)
  :bind
  (:map corfu-map
        ("TAB"      . corfu-next)
        ([tab]      . corfu-next)
        ("S-TAB"    . corfu-previous)
        ([backtab]  . corfu-previous))

  :hook ((prog-mode   . corfu-mode)
         (shell-mode  . corfu-mode)
         (eshell-mode . corfu-mode))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package consult
  :ensure t
  :after vertico)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :bind
  (("M-." . embark-dwim)
   ("C-." . embark-act)
   ([remap describe-bindings] . embark-bindings))
  :custom
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator
     embark-minimal-indicator))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-prompter 'embark-completing-read-prompter))

(use-package embark-consult
  :after (:all embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches
	"-AGFhlv --group-directories-first --time-style=long-iso"))
;; (evil-collection-define-key 'normal 'dired-mode-map
;;   "h" 'dired-up-directory
;;   "l" 'dired-find-file))

(use-package async
  :init
  (dired-async-mode t))

(use-package helpful)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))


(use-package dired-single)



(use-package general
  :config
  (defvar my-help-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "h") #'helpful-symbol)
      (define-key map (kbd "k") #'helpful-key)
      (define-key map (kbd "m") #'describe-mode)
      (define-key map (kbd "M") #'man)
      (define-key map (kbd "p") #'helpful-at-point)
      map))

  (defvar my-buffer-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "b") #'consult-buffer)
      (define-key map (kbd "k") #'kill-current-buffer)
      (define-key map (kbd "n") #'next-buffer)
      (define-key map (kbd "p") #'previous-buffer)
      (define-key map (kbd "s") #'scratch-buffer)
      map))

  (defvar my-window-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "s") #'split-window-below)
      (define-key map (kbd "v") #'split-window-right)
      (define-key map (kbd "c") #'delete-window)
      (define-key map (kbd "w") #'evil-window-next)
      map))

  (defvar my-embark-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "RET") #'embark-find-defition)
      (define-key map (kbd "k") #'split-window-right)
      (define-key map (kbd "c") #'delete-window)
      (define-key map (kbd "w") #'evil-window-next)
      map))

  (defvar my-org-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "SPC") #'tempo-complete-tag)
      (define-key map (kbd "b") #'org-insert-structure-template)
      (define-key map (kbd "c") #'org-cite-insert)
      (define-key map (kbd "e") #'org-babel-execute-src-block)
      (define-key map (kbd "h") #'org-html-export-to-html)
      (define-key map (kbd "i") #'org-indent-mode)
      (define-key map (kbd "j") #'consult-org-heading)
      (define-key map (kbd "k") #'org-clock-in)
      (define-key map (kbd "K") #'org-clock-out)
      (define-key map (kbd "n") #'org-narrow-to-subtree)
      (define-key map (kbd "m") #'tempo-forward-mark)
      (define-key map (kbd "M") #'tempo-backward-mark)
      (define-key map (kbd "N") #'widen)
      (define-key map (kbd "p") #'org-latex-export-to-pdf)
      (define-key map (kbd "P") #'org-beamer-export-to-pdf)
      (define-key map (kbd "s") #'org-cut-subtree)
      (define-key map (kbd "t") #'org-time-stamp)
      (define-key map (kbd "v") #'visual-line-mode)
      (define-key map (kbd "x") #'org-export-dispatch)
      (define-key map (kbd "X") #'toggle-org-pdf-export-on-save)
      map))

  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :global-prefix "SPC"
   "a" '(org-preferred-agenda :which-key "agenda")
   "b" `(,my-buffer-map :which-key "Buffer")
   "B" '(magit-blame-addition :which-key "git blame")
   "c" '(org-capture :which-key "capture")
   "C" '(magit-clone :which-key "magit clone")
   "d" '(dired-jump :which-key "dired jump")
   "e" '(embark-act :which-key "embark")
   "E" '(eat :which-key "eat")
   "f" '(find-file :which-key "open file")
   "F" '(consult-find :which-key "consult find")
   "g" '(magit :which-key "magit")
   "G" '(consult-ripgrep :which-key "consult grep")
   "h" `(,my-help-map :which-key "Help")
   "i" '(insert-char :which-key "insert unicode")
   "I" '(toggle-input-method :which-key "change layout")
   "j" '(consult-imenu :which-key "jump via imenu")
   "k" '(comment-or-uncomment-region :which-key "comment region")
   "K" '(evil-indent-line :which-key "indent region")
   "l" '(slock :which-key "lock screen")
   "L" '(consult-git-log-grep :which-key "grep git log")
   "m" '(hl-todo-next :which-key "next Todo")
   "n" '(elfeed :which-key "news (elfeed)")
   "p" '(projectile-find-file :which-key "hop project file")
   "P" '(projectile-switch-project :which-key "hop project")
   "Q" '(save-buffers-kill-emacs :which-key "quit emacs")
   "r" '(consult-recent-file :which-key "open recent")
   "u" '(consult-theme :which-key "change theme")
   "v" '(consult-yank-pop :which-key "clipboard")
   "V" '(eval-region :which-key "eval region")
   "w" `(,my-window-map :which-key "Windows")
   "x" '(consult-flymake :which-key "run (flymake)")
   "SPC" '(execute-extended-command :which-key "M-x")
   ":" '(eval-expression :which-key "M-:")
   "/" `(consult-line :which-key "search")
   "<return>" '(consult-bookmark :which-key "jump to bookmark")
   "S-<return>" '(bookmark-set :which-key "set a bookmark")))

(use-package seq)

(use-package magit)

(use-package nix-mode)

(use-package projectile
  :init (projectile-mode t))

(use-package evil
  :init (setq evil-want-keybinding nil
              evil-want-C-u-scroll t
              evil-vsplit-window-right t
              evil-split-window-below t
              evil-want-integration t)
  (evil-mode t)
  :custom
  (evil-undo-system 'undo-redo))

(use-package evil-collection
  :config (evil-collection-init)
  :diminish evil-collection-unimpaired-mode
  :after evil)

(use-package eat
  :hook ((eshell-mode . eat-eshell-mode)
         (eshell-mode . eat-eshell-visual-command-mode)))

(use-package which-key
  :diminish which-key-mode
  :custom
  (setq which-key-idle-delay 1.5
        which-key-idle-secondary-delay 0.05)
  :init (which-key-mode))

(use-package diminish)

(use-package org-modern
  :config
  (setq org-startup-indented t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-confirm-babel-evaluate nil
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t

	org-hide-emphasis-markers t
	org-pretty-entities t
	org-ellipsis "…"

	org-agenda-tags-column 0
	org-agenda-block-separator ?─
	org-agenda-time-grid
	'((daily today require-timed)
	  (800 1000 1200 1400 1600 1800 2000)
	  " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	org-agenda-current-time-string
	"◀── now ─────────────────────────────────────────────────")
  :hook (org-mode . org-modern-mode))

(use-package eldoc
  :diminish eldoc-mode)

(use-package savehist
  :ensure nil
  :init (savehist-mode t))


(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-emoji))


(use-package emacs
  :ensure nil
  :config
  (setq ring-bell-function #'ignore
        completion-cycle-threshold 3
        scroll-step 1
        scroll-margin 3
        scroll-conservatively 10000
        make-backup-files nil
        next-screen-context-lines 5
        tab-always-indent 'complete
	comment-multi-line nil
        line-move-visual nil
        initial-scratch-message nil
	confirm-kill-emacs nil
        inhibit-startup-screen t
	display-time-format "%H:%M"
	display-time-default-load-average nil
        default-input-method "japanese"
        enable-recursive-minibuffers t
	tab-width 2
        evil-shift-width tab-width)
  (setq electric-pair-pairs
        '(
          (?\" . ?\")
	  (?\{ . ?\})))
  (fset 'yes-or-no-p 'y-or-n-p)
  (set-face-attribute 'default nil :height 200)
  (prettify-symbols-mode t)
  (global-auto-revert-mode t)
  (global-hl-line-mode t)
  (electric-pair-mode t)
  (recentf-mode t)
  (save-place-mode t)
  (indent-tabs-mode nil)
  (setq completion-in-region-function #'consult-completion-in-region)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (defadvice split-window (after split-window-after activate)
    (other-window 1))

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))


(use-package spacious-padding
  :config (spacious-padding-mode))

(use-package keycast)
  ;; :config
  ;; (setq keycast-header-line-format "%2s%k%c%R")
  ;;  (dolist (input '(self-insert-command org-self-insert-command))
  ;;    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))
  ;;  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
  ;;    (add-to-list 'keycast-substitute-alist `(,event nil))))

(use-package mini-echo
  :config
  (setq mini-echo-default-segments
	'(:long ("major-mode" "buffer-name" "vcs" "buffer-position"
		 "flymake" "process" "selection-info"
		 "narrow" "macro" "profiler" "time" "keycast")
		:short ("buffer-name-short" "buffer-position" "process"
			"profiler" "selection-info" "narrow" "macro")))
  ;; set separator to concat information
  (setq mini-echo-separator " ")

  (setq mini-echo-right-padding 1)

  (mini-echo-define-segment "keycast"
    "Display keycast info."
    :update-hook '(post-command-hook)
    :fetch (keycast--format keycast-mode-line-format)
    :update (keycast--update))

  (mini-echo-define-segment "time"
    "Return current time."
    :setup
    (display-time-mode 1)
    :fetch (mini-echo-segment--extract display-time-string))
  (mini-echo-mode))

(use-package popper
  :init
  (setopt popper-reference-buffers
          '("\\*Messages\\*"
            "\\*Warnings\\*"
            "\\*xref\\*"
            "\\*Backtrace\\*"
            "*Flymake diagnostics.*"
            "*helpful.*"
            "\\*eldoc\\*"
            "\\*compilation\\*"
            "^*tex"
            "Output\\*$"
            "\\*Async Shell Command\\*"
            "\\*Dtache Shell Command\\*"
            "\\*GDB.*out\\*"
            help-mode
            compilation-mode))
  (popper-mode t))
