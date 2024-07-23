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
                ((bg         '("#161616" "#161616" nil            ))
                 (bg-alt     '("#161616" "#161616" nil            ))
                 (base0      '("#161616" "#161616" "black"        ))
                 (base1      '("#262626" "#262626" "brightblack"  ))
                 (base2      '("#393939" "#393939" "brightblack"  ))
                 (base3      '("#525252" "#525252" "brightblack"  ))
                 (base4      '("#dde1e6" "#dde1e6" "brightblack"  ))
                 (base5      '("#f2f4f8" "#f2f4f8" "brightblack"  ))
                 (base6      '("#ffffff" "#ffffff" "brightblack"  ))
                 (base7      '("#08bdba" "#08bdba" "brightblack"  ))
                 (base8      '("#3ddbd9" "#3ddbd9" "white"        ))
                 (fg         '("#ff7eb6" "#f2f4f8" "white"        ))
                 (fg-alt     '("#262626" "#ffffff" "brightwhite"  ))

                 (grey       '("#393939" "#393939" "white"))
                 (red        '("#ee5396" "#ee5396" "red"          ))
                 (orange     '("#ff7eb6" "#ff7eb6" "brightred"    ))
                 (green      '("#42be65" "#42be65" "green"        ))
                 (teal       '("#3ddbd9" "#3ddbd9" "brightgreen"  ))
                 (yellow     '("#ECCC87" "#ECBE7B" "yellow"       ))
                 (blue       '("#33b1ff" "#33b1ff" "brightblue"   ))
                 (dark-blue  '("#78a9ff" "#78a9ff" "blue"         ))
                 (magenta    '("#be95ff" "#be95ff" "magenta"      ))
                 (violet     '("#be95ff" "#be95ff" "brightmagenta"))
                 (cyan       '("#82cfff" "#82cfff" "brightcyan"   ))
                 (dark-cyan  '("#08bdba" "#08bdba" "cyan"         ))

                 ;; face categories -- required for all themes
                 (highlight      blue)
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

                 (modeline-fg     'unspecified)
                 (modeline-fg-alt base5)

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
