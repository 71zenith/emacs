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

(global-visual-line-mode 1)

(set-rotate-patterns! 'nix-mode
  :words '(("true" "false")))

(setq fancy-splash-image (concat doom-user-dir "kobeni.jpg"))

(setq elfeed-feeds
      '("https://port19.xyz/rss.xml"
        "https://derisis13.github.io/feed.xml"))

(setq +doom-dashboard-menu-sections (cl-subseq +doom-dashboard-menu-sections 5 6))
