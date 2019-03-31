;;; tango-dark-theme.el --- Tango-based custom theme for faces

;; Copyright (C) 2010-2015 Free Software Foundation, Inc.

;; Authors: Guoqiang Jin <ustczhan@gmail.com>
;;          Chong Yidong <cyd@stupidchicken>
;;          Jan Moringen <jan.moringen@uni-bielefeld.de>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; The colors in this theme come from the Tango palette, which is in
;; the public domain: http://tango.freedesktop.org/

;;; Changlog

;; 2015-06-15(Guoqiang Jin): add color faces for company and helm
;; 2014-05-19(Guoqiang Jin): add color faces for flycheck and rainbow-delimeters.


;;; Code:

(deftheme tangotango
  "Face colors using the Tango palette (dark background).
Basic, Font Lock, Isearch, Gnus, Message, Ediff, Flyspell,
Semantic, and Ansi-Color faces are included.")

(let ((class '((class color) (min-colors 89)))
      ;; Tango palette colors.
      (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
      (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      (cham-1 "#8ae234") (cham-2 "#73d216") (cham-3 "#4e9a06")
      (blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
      (plum-1 "#ad7fa8") (plum-2 "#75507b") (plum-3 "#5c3566")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      (alum-4 "#888a85") (alum-5 "#555753") (alum-6 "#2e3436")

      ;; Not in Tango palette; used for better contrast.
      (cham-0 "#b4fa70") (blue-0 "#8cc4ff") (plum-0 "#e6a8df")
	  (red-0 "#ff4b4b")  (alum-5.5 "#41423f") (alum-7 "#212526"))


  (custom-theme-set-faces
   'tangotango
   ;; Ensure sufficient contrast on low-color terminals.
   `(default ((((class color) (min-colors 4096))
	  	   (:foreground ,alum-1 :background ,alum-6))
	  	  (((class color) (min-colors 256))
	  	   (:foreground ,alum-1 :background "#222"))
	  	  (,class
	  	   (:foreground ,alum-1 :background "black"))))
   `(cursor ((,class (:background ,butter-1))))
   ;; Highlighting faces
   `(fringe ((,class (:background ,alum-7))))
   ;;`(highlight ((,class (:foreground ,alum-6 :background ,butter-2))))
   `(highlight ((t (:background "#444444"))))
   `(border ((,class (:background, alum-4))))
   `(region ((,class (:background ,alum-5))))
   `(secondary-selection ((,class (:background ,blue-3))))
   `(isearch ((t (:background "#f57900" :foreground "#2e3436"))))
   `(isearch-lazy-highlight-face ((t (:foreground "#2e3436" :background "#e9b96e"))))
										;   `(isearch ((,class (:foreground ,alum-1 :background ,orange-3))))
										;   `(lazy-highlight ((,class (:background ,choc-3))))
   `(trailing-whitespace ((,class (:background ,red-3))))
   ;; Mode line faces
   `(mode-line ((,class
				 (:foreground "#eeeeec" :background "#111111"
							  :box (:line-width 1 :color ,alum-6)))))
   `(mode-line-inactive ((,class (:foreground ,alum-5 :background "#1F2427"
											  :box (:line-width 1 :color "#1F2427"))
								 )))
   `(mode-line-buffer-id ((,class (:bold t :foreground ,orange-2 :background nil))))


   ;;   `(mode-line (:foreground aluminium-1 :background black
   ;;                          :box (:line-width 1 :color aluminium-6)))
   ;; `(mode-line-inactive (:foreground aluminium-5 :background "#1F2427"
   ;;                                   :box (:line-width 1 :color background)))
   ;; `(mode-line-buffer-id (:bold t :foreground orange-2))

   `(region ((t (:background "dark slate blue"))))
   `(link ((t (:underline t :foreground "dodger blue"))))
   `(custom-link ((t (:inherit 'link))))
   `(match ((t (:bold t :background "#e9b96e" :foreground "#2e3436"))))
   `(tool-tips ((t (:inherit 'variable-pitch :foreground "black" :background "lightyellow"))))
   `(tooltip ((t (:inherit 'variable-pitch :foreground "black" :background "lightyellow"))))
   `(bold ((t (:bold t :underline nil :background nil))))
   `(italic ((t (:italic t :underline nil :background nil))))
   ;; `(mode-line ((,class
   ;; 		 (:box (:line-width -1 :style released-button)
   ;; 		  :background ,alum-2 :foreground ,alum-6))))
   ;; `(mode-line-inactive ((,class
   ;; 			  (:box (:line-width -1 :style released-button)
   ;; 			   :background ,alum-5 :foreground ,alum-1))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,cham-0 :weight bold))))
   `(escape-glyph ((,class (:foreground ,butter-3))))
   `(error ((,class (:foreground ,red-0))))
   `(warning ((,class (:foreground ,orange-1))))
   `(success ((,class (:foreground ,cham-1))))

   ;; Font lock faces
   `(font-lock-builtin-face ((t (:foreground "#729fcf"))))
   `(font-lock-comment-face ((t (:foreground "#888a85"))))
   `(font-lock-constant-face ((t (:foreground "#8ae234"))))
   `(font-lock-doc-face ((t (:foreground "#888a85"))))
   `(font-lock-keyword-face ((t (:foreground "#729fcf" :bold t))))
   `(font-lock-string-face ((t (:foreground "#ad7fa8" :italic t))))
   `(font-lock-type-face ((t (:foreground "#8ae234" :bold t))))
   `(font-lock-variable-name-face ((t (:foreground "tomato"))))
   `(font-lock-warning-face ((t (:bold t :foreground "#f57900"))))
   `(font-lock-function-name-face ((t (:foreground "#edd400" :bold t))))
   `(comint-highlight-input ((t (:italic t :bold t))))
   `(comint-highlight-prompt ((t (:foreground "#8ae234"))))

   `(show-paren-match-face ((t (:foreground "#2e3436" :background "#73d216"))))
   `(show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436"))))

   `(info-xref ((t (:foreground "#729fcf"))))
   `(info-xref-visited ((t (:foreground "#ad7fa8"))))
   `(diary-face ((t (:bold t :foreground "IndianRed"))))
   `(eshell-ls-clutter-face ((t (:bold t :foreground "DimGray"))))
   `(eshell-ls-executable-face ((t (:bold t :foreground "Coral"))))
   `(eshell-ls-missing-face ((t (:bold t :foreground "black"))))
   `(eshell-ls-special-face ((t (:bold t :foreground "Gold"))))
   `(eshell-ls-symlink-face ((t (:bold t :foreground "White"))))
   `(widget-button ((t (:bold t))))
   `(widget-mouse-face ((t (:bold t :foreground "white" :background "brown4"))))
   `(widget-field ((t (:foreground "orange" :background "gray30"))))
   `(widget-single-line-field ((t (:foreground "orange" :background "gray30"))))
   `(custom-group-tag ((t (:bold t :foreground "#edd400" :height 1.3))))
   `(custom-variable-tag ((t (:bold t :foreground "#edd400" :height 1.1))))
   `(custom-face-tag ((t (:bold t :foreground "#edd400" :height 1.1))))
   `(custom-state-face ((t (:foreground "#729fcf"))))
   `(custom-button  ((t (:box (:line-width 1 :style released-button) :background "grey50" :foreground "black"))))
   `(custom-variable-button ((t (:inherit 'custom-button))))
   `(custom-button-mouse  ((t (:inherit 'custom-button :background "grey60"))))
   `(custom-button-unraised  ((t (:background "grey50" :foreground "black"))))
   `(custom-button-mouse-unraised  ((t (:inherit 'custom-button-unraised :background "grey60"))))
   `(custom-button-pressed  ((t (:inherit 'custom-button :box (:style pressed-button)))))
   `(custom-button-mouse-pressed-unraised  ((t (:inherit 'custom-button-unraised :background "grey60"))))
   `(custom-documentation ((t (:italic t))))

   ;; `(font-latex-math-face ((t (:foreground "burlywood"))))
   ;; `(font-latex-sedate-face ((t (:foreground "gray60"))))
   ;; `(font-latex-sectioning-5-face ((t (:weight bold :bold t :foreground "red3"))))
   ;; `(font-latex-bold-face ((t (:weight bold :bold t :foreground "green3"))))
   ;; `(font-latex-string-face ((t (:weight bold :bold nil :foreground "darkgreen"))))
   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue-1))))
   `(link-visited ((,class (:underline t :foreground ,blue-2))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:foreground ,plum-1))))
   `(gnus-group-news-1-low ((,class (:foreground ,plum-2))))
   `(gnus-group-news-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-news-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-news-3 ((,class (:foreground ,cham-1))))
   `(gnus-group-news-3-low ((,class (:foreground ,cham-2))))
   `(gnus-group-news-4 ((,class (:foreground ,plum-0))))
   `(gnus-group-news-4-low ((,class (:foreground ,choc-2))))
   `(gnus-group-news-5 ((,class (:foreground ,orange-1))))
   `(gnus-group-news-5-low ((,class (:foreground ,orange-2))))
   `(gnus-group-news-low ((,class (:foreground ,butter-2))))
   `(gnus-group-mail-1 ((,class (:foreground ,plum-1))))
   `(gnus-group-mail-1-low ((,class (:foreground ,plum-2))))
   `(gnus-group-mail-2 ((,class (:foreground ,blue-1))))
   `(gnus-group-mail-2-low ((,class (:foreground ,blue-2))))
   `(gnus-group-mail-3 ((,class (:foreground ,cham-1))))
   `(gnus-group-mail-3-low ((,class (:foreground ,cham-2))))
   `(gnus-group-mail-low ((,class (:foreground ,butter-2))))
   `(gnus-header-content ((,class (:weight normal :foreground ,butter-3))))
   `(gnus-header-from ((,class (:foreground ,butter-2))))
   `(gnus-header-subject ((,class (:foreground ,cham-1))))
   `(gnus-header-name ((,class (:foreground ,blue-1))))
   `(gnus-header-newsgroups ((,class (:foreground ,choc-2))))
   `(gnus-signature((t (:italic t :foreground "dark grey"))))
   `(gnus-summary-cancelled((t (:background "black" :foreground "yellow"))))
   `(gnus-summary-high-ancient((t (:bold t :foreground "rotal blue"))))
   `(gnus-summary-high-read((t (:bold t :foreground "lime green"))))
   `(gnus-summary-high-ticked((t (:bold t :foreground "tomato"))))
   `(gnus-summary-high-unread((t (:bold t :foreground "white"))))
   `(gnus-summary-low-ancient((t (:italic t :foreground "lime green"))))
   `(gnus-summary-low-read((t (:italic t :foreground "royal blue"))))
   `(gnus-summary-low-ticked((t (:italic t :foreground "dark red"))))
   `(gnus-summary-low-unread((t (:italic t :foreground "white"))))
   `(gnus-summary-normal-ancient((t (:foreground "royal blue"))))
   `(gnus-summary-normal-read((t (:foreground "lime green"))))
   `(gnus-summary-normal-ticked((t (:foreground "indian red"))))
   `(gnus-summary-normal-unread((t (:foreground "white"))))
   `(gnus-summary-selected ((t (:background "brown4" :foreground "white"))))
   `(gnus-cite-face-1 ((t (:foreground "#ad7fa8"))))
   `(gnus-cite-face-2 ((t (:foreground "sienna4"))))
   `(gnus-cite-face-3 ((t (:foreground "khaki4"))))
   `(gnus-cite-face-4 ((t (:foreground "PaleTurquoise4"))))
   ;; Message faces
   `(message-header-name ((,class (:foreground ,blue-1))))
   `(message-header-cc ((,class (:foreground ,butter-3))))
   `(message-header-other ((,class (:foreground ,choc-2))))
   `(message-header-subject ((,class (:foreground ,cham-1))))
   `(message-header-to ((,class (:foreground ,butter-2))))
   `(message-cited-text ((,class (:foreground ,cham-1))))
   `(message-separator ((,class (:foreground ,plum-1))))
   ;; Org faces
   `(org-hide ((t (:foreground "#2e3436"))))
   `(org-level-1 ((t (:bold t :foreground "dodger blue" :height 1.25))))
   `(org-level-2 ((t (:bold t :foreground "#edd400" :height 1.1))))
   `(org-level-3 ((t (:bold t :foreground "#6ac214" :height 1.0))))
   `(org-level-4 ((t (:bold nil :foreground "tomato" :height 1.0))))
   `(org-date ((t (:underline t :foreground "cyan"))))
   `(org-footnote  ((t (:underline t :foreground "magenta3"))))
   `(org-link ((t (:foreground "skyblue2" :background "#2e3436"))))
   `(org-special-keyword ((t (:foreground "brown"))))
   `(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
   `(org-block ((t (:foreground "#bbbbbc"))))
   `(org-quote ((t (:inherit org-block :slant italic))))
   `(org-verse ((t (:inherit org-block :slant italic))))
   `(org-todo ((t (:bold t :foreground "Red"))))
   `(org-done ((t (:bold t :foreground "ForestGreen" :strike-through t))))
   `(org-agenda-structure ((t (:weight bold :foreground "tomato"))))
   `(org-agenda-date ((t (:foreground "#6ac214"))))
   `(org-agenda-date-weekend ((t (:weight normal :foreground "dodger blue"))))
   `(org-agenda-date-today ((t (:weight bold :foreground "#edd400"))))
   `(org-block-begin-line ((t ( ;:underline "#a7a6aa"
							   :foreground "#888a85" :background "#333333"))))
   `(org-block-background ((t (:background "#252b2b"))))
   `(org-block-end-line ((t (;:overline "#a7a6aa"
							 :foreground "#888a85" :background "#333333"))))
   `(org-meta-line ((t (:inherit font-lock-comment-face :weight normal))))
   `(org-checkbox ((t (:background "#2b2b2b" :foreground "white" :bold t
								   :box (:line-width 1 :style released-button)))))
   `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   ;; SMerge faces
   `(smerge-refined-change ((,class (:background ,blue-3))))
   ;; Ediff faces
   `(ediff-current-diff-A ((,class (:background ,alum-5))))
   `(ediff-fine-diff-A ((,class (:background ,blue-3))))
   `(ediff-even-diff-A ((,class (:background ,alum-5.5))))
   `(ediff-odd-diff-A ((,class (:background ,alum-5.5))))
   `(ediff-current-diff-B ((,class (:background ,alum-5))))
   `(ediff-fine-diff-B ((,class (:background ,choc-3))))
   `(ediff-even-diff-B ((,class (:background ,alum-5.5))))
   `(ediff-odd-diff-B ((,class (:background ,alum-5.5))))
    ;;;;; flycheck
   `(flycheck-error
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,red-0) :inherit unspecified))
	  (t (:foreground ,red-0 :weight bold :underline t))))
   `(flycheck-warning
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color "#F0DFAF") :inherit unspecified))
	  (t (:foreground "#F0DFAF" :weight bold :underline t))))
   `(flycheck-info
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color "#93E0E3") :inherit unspecified))
	  (t (:foreground "#93E0E3" :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,red-0 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground "#F0DFAF" :weight bold))))
   `(flycheck-fringe-info ((t (:foreground "#93E0E3" :weight bold))))
   ;; Flyspell faces
   `(flyspell-duplicate ((,class (:underline ,orange-1))))
   `(flyspell-incorrect ((,class (:underline ,red-1))))
   ;; ess ecb faces
   `(ess-jb-comment-face ((t (:background "#2e3436" :foreground "#888a85" :slant italic))))
   `(ess-jb-hide-face ((t (:background "#2e3436" :foreground "#243436"))))
   `(ess-jb-h1-face ((t (:height 1.6 :foreground "dodger blue" :slant normal))))
   `(ess-jb-h2-face ((t (:height 1.4 :foreground "#6ac214" :slant normal))))
   `(ess-jb-h3-face ((t (:height 1.2 :foreground "#edd400" :slant normal))))
   `(ecb-default-highlight-face ((t (:background "#729fcf"))))
   `(ecb-tag-header-face ((t (:background "#f57900"))))
   ;; magit
   `(magit-header ((t (:foreground "#edd400"))))
   `(magit-diff-add ((t (:foreground "#729fcf" :background "#222"))))
   ;;`(magit-item-highlight ((t (:weight extra-bold :inverse-video t))))
   `(magit-item-highlight ((,class (:background , alum-5.5))))
   `(magit-diff-del ((,class (:background "#222"))))
   ;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,alum-1))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground "#BFEBBF"))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground "#D0BF8F"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground "#93E0E3"))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground "#9FC59F"))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground "#94BFF3"))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground "#E0CF9F"))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground "#8FB28F"))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground "#6CA0A3"))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground "#DFAF8F"))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground "#7F9F7F"))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground "#366060"))))
   ;;; semantic mode
   `(semantic-decoration-on-includes ((,class (:underline ,alum-4))))
   `(semantic-decoration-on-private-members-face
	 ((,class (:background ,plum-3))))
   `(semantic-decoration-on-protected-members-face
	 ((,class (:background ,choc-3))))
   `(semantic-decoration-on-unknown-includes
	 ((,class (:background ,red-3))))
   `(semantic-decoration-on-unparsed-includes
	 ((,class (:background ,alum-5.5))))
   `(semantic-tag-boundary-face ((,class (:overline ,blue-1))))
   `(semantic-unmatched-syntax-face ((,class (:underline ,red-1))))

   ;;company
   `(company-tooltip ((t (:foreground ,alum-2 :background ,alum-5.5))))
   `(company-tooltip-annotation ((t (:foreground ,plum-0 :background ,alum-5.5))))
   `(company-tooltip-selection ((t (:foreground ,alum-2 :background ,alum-7))))
   `(company-tooltip-mouse ((t (:background ,alum-7))))
   `(company-tooltip-common ((t (:foreground ,cham-0))))
   `(company-tooltip-common-selection ((t (:foreground ,cham-0))))
   `(company-scrollbar-fg ((t (:background ,alum-7))))
   `(company-scrollbar-bg ((t (:background ,alum-5))))
   `(company-preview ((t (:background ,cham-0))))
   `(company-preview-common ((t (:foreground ,cham-0 :background ,alum-7))))


   ;; helm
   `(helm-source-header ((,class (:inherit success))))
   `(helm-visible-mark ((,class (:inherit region :foreground ,alum-3))))
   `(helm-header ((,class (:inherit mode-line))))
   `(helm-candidate-number ((,class (:inherit highlight))))
   `(helm-selection ((,class (:inherit secondary-selection))))
   `(helm-match ((,class (:inherit warning))))
   `(helm-separator ((,class (:inherit message-separator))))
   `(helm-action ((,class (:foreground ,blue-1))))
   `(helm-ff-directory ((,class (:foreground ,blue-1 :background nil :underline nil))))
   `(helm-ff-file ((,class (:inherit link :foreground ,plum-1 :underline nil))))
   `(helm-grep-file ((,class (:inherit link :foreground ,plum-1 :underline t))))

   `(swiper-minibufer-match-face-1 ((, class (:background "#dddddd"))))
   `(swiper-minibuffer-match-face-2 ((,class (:background "#bbbbbb" :weight bold))))
   `(swiper-minibuffer-match-face-3 ((,class (:background "#bbbbff" :weight bold))))
   `(swiper-minibuffer-match-face-4 ((,class (:background "#ffbbff" :weight bold))))
   )

  (custom-theme-set-variables
   'tangotango
   `(ansi-color-names-vector [,alum-7 ,red-0 ,cham-0 ,butter-1
									  ,blue-1 ,plum-1 ,blue-0 ,alum-1])))

(provide-theme 'tangotango)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; tangotango-theme.el ends here
