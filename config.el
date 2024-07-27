;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Mori Zen"
      user-mail-address "71zenith@proton.me")

(setq doom-font (font-spec :size 22)
      doom-symbol-font (font-spec :size 22)
      doom-big-font (font-spec :size 30)
      doom-serif-font (font-spec :size 22)
      doom-variable-pitch-font (font-spec :size 22))

(setq doom-theme 'doom-oxocarbon-dark)

(setq display-line-numbers-type 'relative)

(setq org-directory "~/notes/")

(setq default-input-method "japanese"
      scroll-margin 5
      kill-whole-line t
      confirm-kill-emacs nil)

(global-visual-line-mode +1)
(global-subword-mode +1)

(setq fancy-splash-image (concat doom-user-dir "kobeni.jpg"))

(setq +doom-dashboard-functions '(doom-dashboard-widget-banner doom-dashboard-widget-loaded))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(after! evil
  (setq evil-split-window-below t
        evil-vsplit-window-right t
        evil-ex-substitute-global t
        evil-kill-on-visual-paste nil
        evil-mode-line-format nil))

(after! which-key
  (setq which-key-idle-delay 0.2
        which-key-idle-secondary-delay 0.05))

(set-frame-parameter (selected-frame) 'alpha-background 80)
(add-to-list 'default-frame-alist '(alpha-background . 80))

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit window-split)
  (consult-buffer))
