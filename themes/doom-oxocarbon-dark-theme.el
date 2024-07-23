;;; doom-oxocarbon-theme.el --- an original light theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: July 23, 2024
;; Author: 71zenith <https://github.com/71zenith>
;; Maintainer:
;; Source: https://github.com/nyoom-engineering/base16-oxocarbon
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-oxocarbon-dark-theme nil
  "Options for the `doom-oxocarbon-dark' theme."
  :group 'doom-themes)

(defcustom doom-oxocarbon-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-oxocarbon-dark-theme
  :type 'boolean)

(defcustom doom-oxocarbon-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-oxocarbon-dark-theme
  :type 'boolean)

(defcustom doom-oxocarbon-dark-comment-bg doom-oxocarbon-dark-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-oxocarbon-dark-theme
  :type 'boolean)

(defcustom doom-oxocarbon-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-oxocarbon-dark-theme
  :type '(choice integer boolean))

(defcustom doom-oxocarbon-dark-region-highlight t
  "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
  :group 'doom-oxocarbon-dark-theme
  :type 'symbol)


;;
;;; Theme definition

(def-doom-theme doom-oxocarbon-dark
                "A dark Opera theme."

                ;; name        default   256       16
                ((bg         '("#161616" ))
                 (bg-alt     '("#161616" ))
                 (base0      '("#161616" ))
                 (base1      '("#262626" ))
                 (base2      '("#393939" ))
                 (base3      '("#525252" ))
                 (base4      '("#dde1e6" ))
                 (base5      '("#f2f4f8" ))
                 (base6      '("#ffffff" ))
                 (base7      '("#08bdba" ))
                 (base8      '("#3ddbd9" ))
                 (fg         '("#ff7eb6" ))
                 (fg-alt     '("#262626" ))

                 (grey       '("#393939" ))
                 (red        '("#ee5396" ))
                 (orange     '("#ff7eb6" ))
                 (green      '("#42be65" ))
                 (teal       '("#3ddbd9" ))
                 (yellow     '("#ECCC87" ))
                 (blue       '("#33b1ff" ))
                 (dark-blue  '("#78a9ff" ))
                 (magenta    '("#be95ff" ))
                 (violet     '("#be95ff" ))
                 (cyan       '("#82cfff" ))
                 (dark-cyan  '("#08bdba" ))

                 ;; face categories -- required for all themes
                 (highlight      base5)
                 (vertical-bar   (doom-darken base1 0.5))
                 (selection      dark-blue)
                 (builtin        blue)
                 (comments       base3)
                 (doc-comments   base3)
                 (constants      dark-blue)
                 (functions      blue)
                 (keywords       blue)
                 (methods        teal)
                 (operators      yellow)
                 (type           yellow)
                 (strings        magenta)
                 (variables      red)
                 (numbers        cyan)
                 (region         (doom-darken grey 0.3))
                 (error          red)
                 (warning        yellow)
                 (success        green)
                 (vc-modified    orange)
                 (vc-added       green)
                 (vc-deleted     red)

                 ;; custom categories
                 (hidden     `(,(car bg) "black" "black"))
                 (-modeline-bright doom-oxocarbon-dark-brighter-modeline)
                 (-modeline-pad
                  (when doom-oxocarbon-dark-padded-modeline
                    (if (integerp doom-oxocarbon-dark-padded-modeline) doom-oxocarbon-dark-padded-modeline 4)))

                 (modeline-fg     base8)
                 (modeline-fg-alt base8)

                 (modeline-bg
                  (if -modeline-bright
                      (doom-darken blue 0.45)
                    `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
                 (modeline-bg-l
                  (if -modeline-bright
                      (doom-darken blue 0.475)
                    `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
                 (modeline-bg-inactive   `(,(car bg-alt) ,@(cdr base1)))
                 (modeline-bg-inactive-l (doom-darken bg-alt 0.1)))

  ;;;; Base theme face overrides
                (((line-number &override) :foreground fg-alt)
                 ((line-number-current-line &override) :foreground fg)
                 ((font-lock-comment-face &override)
                  :slant 'italic
                  :background (if doom-oxocarbon-dark-comment-bg (doom-lighten bg 0.05) 'unspecified))
                 (mode-line
                  :background modeline-bg :foreground modeline-fg
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
                 (mode-line-inactive
                  :background modeline-bg-inactive :foreground modeline-fg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
                 (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; doom-modeline
                 (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; solaire-mode
                 (solaire-mode-line-face
                  :inherit 'mode-line
                  :background modeline-bg-l
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
                 (solaire-mode-line-inactive-face
                  :inherit 'mode-line-inactive
                  :background modeline-bg-inactive-l
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l))))

  ;;;; Base theme variable overrides-
                ())

;;; doom-oxocarbon-dark-theme.el ends here
