;;; themes/doom-github-classic-dark-theme.el --- inspired by GitHub Theme for VS Code -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added:
;; Author:
;; Maintainer:
;; Source:
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-github-classic-dark-theme nil
  "Options for the `doom-github-classic-dark' theme."
  :group 'doom-themes)

(defcustom doom-github-classic-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-github-classic-dark-theme
  :type 'boolean)

(defcustom doom-github-classic-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-github-classic-dark-theme
  :type 'boolean)

(defcustom doom-github-classic-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-github-classic-dark-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-github-classic-dark
                "A dark theme inspired by Atom One Dark."

                ;; name        default   256           16
                ((bg         '("#24292e" "black"       "black"  ))
                 (fg         '("#e1e4e8" "#bfbfbf"     "brightwhite"  ))

                 ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
                 ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
                 ;; or region), especially when paired with the `doom-darken', `doom-lighten',
                 ;; and `doom-blend' helper functions.
                 (bg-alt     '("#1b1f23" "black"       "black"        ))
                 (fg-alt     '("#e1e4e8" "#2d2d2d"     "white"        ))

                 ;; These should represent a spectrum from bg to fg, where base0 is a starker
                 ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
                 ;; dark grey, base0 should be white and base8 should be black.
                 (base0      '("#1b1f23" "black"       "black"        ))
                 (base1      '("#24292e" "#1e1e1e"     "brightblack"  ))
                 (base2      '("#2b3036" "#2e2e2e"     "brightblack"  ))
                 (base3      '("#444d56" "#262626"     "brightblack"  ))
                 (base4      '("#586069" "#3f3f3f"     "brightblack"  ))
                 (base5      '("#6a737d" "#525252"     "brightblack"  ))
                 (base6      '("#d1d5da" "#6b6b6b"     "brightblack"  ))
                 (base7      '("#e1e4e8" "#979797"     "brightblack"  ))
                 (base8      '("#f6f8fa" "#dfdfdf"     "white"        ))

                 (grey       base4)
                 (red        '("#f97583" "#ff6655" "red"          ))
                 (orange     '("#ffab70" "#dd8844" "brightred"    ))
                 (green      '("#85e89d" "#99bb66" "green"        ))
                 (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
                 (yellow     '("#ffea7f" "#ECBE7B" "yellow"       ))
                 (blue       '("#79B8FF" "#51afef" "brightblue"   ))
                 (dark-blue  '("#2188ff" "#2257A0" "blue"         ))
                 (magenta    '("#f692ce" "#c678dd" "brightmagenta"))
                 (violet     '("#b392f0" "#a9a1e1" "magenta"      ))
                 (cyan       '("#9ECBFF" "#46D9FF" "brightcyan"   ))
                 (dark-cyan  '("#39c5cf" "#5699AF" "cyan"         ))

                 ;; These are the "universal syntax classes" that doom-themes establishes.
                 ;; These *must* be included in every doom themes, or your theme will throw an
                 ;; error, as they are used in the base theme defined in doom-themes-base.
                 (highlight      blue)
                 (vertical-bar   (doom-darken base0 0.1))
                 (selection      dark-blue)
                 (builtin        red)
                 (comments       (if doom-github-classic-dark-brighter-comments dark-cyan base5))
                 (doc-comments   (if doom-github-classic-dark-brighter-comments dark-cyan base5))
                 (constants      blue)
                 (functions      violet)
                 (keywords       red)
                 (methods        violet)
                 (operators      red)
                 (type           violet)
                 (strings        cyan)
                 (variables      fg)
                 (numbers        blue)
                 (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
                 (error          red)
                 (warning        yellow)
                 (success        green)
                 (vc-modified    dark-blue)
                 (vc-added       green)
                 (vc-deleted     red)

                 ;; These are extra color variables used only in this theme; i.e. they aren't
                 ;; mandatory for derived themes.
                 (modeline-fg              fg)
                 (modeline-fg-alt          base5)
                 (modeline-bg              (if doom-github-classic-dark-brighter-modeline
                                               (doom-darken blue 0.45)
                                             (doom-darken bg-alt 0.1)))
                 (modeline-bg-alt          (if doom-github-classic-dark-brighter-modeline
                                               (doom-darken blue 0.475)
                                             `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
                 (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
                 (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))
                 (treemacs-dirname "#acb1bf")
                 (treemacs-dir-icon "#646773")
                 (treemacs-filename "#646773")

                 (-modeline-pad
                  (when doom-github-classic-dark-padded-modeline
                    (if (integerp doom-github-classic-dark-padded-modeline) doom-github-classic-dark-padded-modeline 4))))


  ;;;; Base theme face overrides
                (((line-number &override) :foreground base3 :italic nil)
                 ((line-number-current-line &override) :italic nil)

                 (hl-line :background base2)
                 (cursor :background fg)

    ;;;; rainbow-delimiters
                 (rainbow-delimiters-depth-1-face :foreground blue)
                 (rainbow-delimiters-depth-2-face :foreground orange)
                 (rainbow-delimiters-depth-3-face :foreground violet)
                 (rainbow-delimiters-depth-4-face :foreground blue)
                 (rainbow-delimiters-depth-5-face :foreground orange)
                 (rainbow-delimiters-depth-6-face :foreground violet)
                 (rainbow-delimiters-depth-7-face :foreground blue)
                 (rainbow-delimiters-depth-8-face :foreground orange)
                 (rainbow-delimiters-depth-9-face :foreground violet)

   ;;;; treemacs-mode
                 (treemacs-root-face :foreground treemacs-dirname)
                 (treemacs-directory-face :foreground treemacs-dirname)
                 (treemacs-file-face :foreground treemacs-filename)
                 (my/treemacs-theme-dir-icon-face :foreground treemacs-dir-icon)
   ;;;; modeline
                 (mode-line
                  :background modeline-bg :foreground modeline-fg
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
                 (mode-line-inactive
                  :background modeline-bg-inactive :foreground modeline-fg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
                 (mode-line-emphasis :foreground (if doom-github-classic-dark-brighter-modeline base8 highlight))

   ;;;; solaire-mode
                 (solaire-mode-line-face
                  :inherit 'mode-line
                  :background modeline-bg-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
                 (solaire-mode-line-inactive-face
                  :inherit 'mode-line-inactive
                  :background modeline-bg-inactive-alt
                  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))


  ;;;; Base theme variable overrides-
                ())

;;; doom-github-classic-dark-theme.el ends here
