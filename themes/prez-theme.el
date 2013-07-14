;;; prez-theme.el --- Modified low-contrast Zenburn-inverted theme for presentations

;; Author: Andrey Kotlarski <m00naticus@gmail.com>
;; Modified by: Magnar Sveen (based on Bodil Stokke's presentation colors)
;; URL: https://github.com/m00natic/prez-theme
;; Version: 20130417.2351
;; X-Original-Version: 2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Derived from Bozhidar Batsov's port of the Zenburn theme
;; located here https://github.com/bbatsov/zenburn-emacs

;;; Code:

(deftheme prez "Presentation theme.")

(let ((class '((class color) (min-colors 89)))
      ;; Zenburn palette reversed
      ;; colors with -x are lighter, colors with +x are darker
      (azenburn-fg "#232333")
      (azenburn-fg-1 "#9a9aaa")
      (azenburn-bg-1 "#d4d4d4")
      (azenburn-bg-05 "#c7c7c7")
      (azenburn-bg "#c0c0c0")
      (azenburn-bg+1 "#b0b0b0")
      (azenburn-bg+2 "#a0a0a0")
      (azenburn-bg+3 "#909090")
      (azenburn-blue+1 "#235c5c")
      (azenburn-blue "#336c6c")
      (azenburn-blue-1 "#437c7c")
      (azenburn-blue-2 "#538c8c")
      (azenburn-blue-3 "#639c9c")
      (azenburn-blue-4 "#73acac")
      (azenburn-light-blue "#205070")
      (azenburn-dark-blue "#0f2050")
      (azenburn-dark-blue-1 "#1f3060")
      (azenburn-dark-blue-2 "#2f4070")
      (azenburn-violet-1 "#a080a0")
      (azenburn-violet "#806080")
      (azenburn-violet+1 "#704d70")
      (azenburn-violet+2 "#603a60")
      (azenburn-violet+3 "#502750")
      (azenburn-violet+4 "#401440")
      (azenburn-bordeaux "#6c1f1c")
      (azenburn-beige+1 "#6b400c")
      (azenburn-beige "#732f2c")
      (azenburn-beige-1 "#834744")
      (azenburn-beige-2 "#935f5c")
      (azenburn-beige-3 "#a37774")
      (azenburn-beige-4 "#b38f8c")
      (azenburn-beige-5 "#c99f9f")
      (azenburn-green "#23733c"))
  (custom-theme-set-faces
   'prez
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,azenburn-dark-blue :underline t :weight bold))))
   `(link-visited ((t (:foreground ,azenburn-dark-blue-2 :underline t :weight normal))))

   ;;; basic coloring
   `(default ((t (:foreground "#000000" :background "#ffffff"))))
   `(cursor ((t (:foreground ,azenburn-fg :background "black"))))
   `(escape-glyph ((t (:foreground ,azenburn-dark-blue :bold t))))
   `(fringe ((t (:foreground ,azenburn-fg :background "#f0f0f0"))))
   `(header-line ((t (:foreground ,azenburn-dark-blue
                                  :background ,azenburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,azenburn-bg-05))))
   `(success ((t (:foreground ,azenburn-violet :weight bold))))
   `(warning ((t (:foreground ,azenburn-light-blue :weight bold))))

   ;;; compilation
   `(compilation-column-face ((t (:foreground ,azenburn-dark-blue))))
   `(compilation-enter-directory-face ((t (:foreground ,azenburn-violet))))
   `(compilation-error-face ((t (:foreground ,azenburn-blue-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,azenburn-fg))))
   `(compilation-info-face ((t (:foreground ,azenburn-beige))))
   `(compilation-info ((t (:foreground ,azenburn-violet+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,azenburn-violet))))
   `(compilation-line-face ((t (:foreground ,azenburn-dark-blue))))
   `(compilation-line-number ((t (:foreground ,azenburn-dark-blue))))
   `(compilation-message-face ((t (:foreground ,azenburn-beige))))
   `(compilation-warning-face ((t (:foreground ,azenburn-light-blue :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,azenburn-violet+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,azenburn-blue :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,azenburn-dark-blue :weight bold))))

   ;;; grep
   `(grep-context-face ((t (:foreground ,azenburn-fg))))
   `(grep-error-face ((t (:foreground ,azenburn-blue-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,azenburn-beige))))
   `(grep-match-face ((t (:foreground ,azenburn-light-blue :weight bold))))
   `(match ((t (:background ,azenburn-bg-1 :foreground ,azenburn-light-blue :weight bold))))

   ;; faces used by isearch
   `(isearch ((t (:foreground ,azenburn-dark-blue-2 :weight bold :background ,azenburn-bg-1))))
   `(isearch-fail ((t (:foreground ,azenburn-fg :background ,azenburn-blue-4))))
   `(lazy-highlight ((t (:foreground ,azenburn-dark-blue-2 :weight bold :background ,azenburn-bg-05))))

   `(menu ((t (:foreground ,azenburn-fg :background ,azenburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,azenburn-dark-blue))))
   `(mode-line
     ((,class (:foreground ,azenburn-violet+1
                           :background ,azenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,azenburn-violet-1
                      :background ,azenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background "#d4d4d4"))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,azenburn-bg+2))))
   `(trailing-whitespace ((t (:background ,azenburn-blue))))
   `(vertical-border ((t (:foreground ,azenburn-fg))))

   ;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,azenburn-bordeaux))))
   `(font-lock-comment-face ((t (:foreground ,azenburn-violet))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,azenburn-violet))))
   `(font-lock-constant-face ((t (:foreground ,azenburn-violet+4))))
   `(font-lock-doc-face ((t (:foreground ,azenburn-violet+1))))
   `(font-lock-doc-string-face ((t (:foreground ,azenburn-beige-2))))
   `(font-lock-function-name-face ((t (:foreground ,azenburn-beige))))
   `(font-lock-keyword-face ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,azenburn-fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,azenburn-beige+1))))
   `(font-lock-string-face ((t (:foreground ,azenburn-blue))))
   `(font-lock-type-face ((t (:foreground ,azenburn-beige-1))))
   `(font-lock-variable-name-face ((t (:foreground ,azenburn-light-blue))))
   `(font-lock-warning-face ((t (:foreground ,azenburn-dark-blue-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))

   ;;; newsticker
   `(newsticker-date-face ((t (:foreground ,azenburn-fg))))
   `(newsticker-default-face ((t (:foreground ,azenburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,azenburn-violet+3))))
   `(newsticker-extra-face ((t (:foreground ,azenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,azenburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,azenburn-violet))))
   `(newsticker-new-item-face ((t (:foreground ,azenburn-beige))))
   `(newsticker-obsolete-item-face ((t (:foreground ,azenburn-blue))))
   `(newsticker-old-item-face ((t (:foreground ,azenburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,azenburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,azenburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,azenburn-violet))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,azenburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,azenburn-beige :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,azenburn-blue))))
   `(newsticker-treeview-old-face ((t (:foreground ,azenburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:foreground ,azenburn-dark-blue))))

   ;;; external
   `(ace-jump-face-background
     ((t (:foreground ,azenburn-fg-1 :background ,azenburn-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,azenburn-violet+2 :background ,azenburn-bg :inverse-video nil))))

   ;; full-ack
   `(ack-separator ((t (:foreground ,azenburn-fg))))
   `(ack-file ((t (:foreground ,azenburn-beige))))
   `(ack-line ((t (:foreground ,azenburn-dark-blue))))
   `(ack-match ((t (:foreground ,azenburn-light-blue :background ,azenburn-bg-1 :weight bold))))

   ;; auctex
   `(font-latex-bold ((t (:inherit bold))))
   `(font-latex-warning ((t (:inherit font-lock-warning))))
   `(font-latex-sedate ((t (:foreground ,azenburn-dark-blue :weight bold ))))
   `(font-latex-title-4 ((t (:inherit variable-pitch :weight bold))))

   ;; auto-complete
   `(ac-candidate-face ((t (:background ,azenburn-bg+3 :foreground "white"))))
   `(ac-selection-face ((t (:background ,azenburn-beige-4 :foreground ,azenburn-fg))))
   `(popup-tip-face ((t (:background ,azenburn-dark-blue-2 :foreground "white"))))
   `(popup-scroll-bar-foreground-face ((t (:background ,azenburn-beige-5))))
   `(popup-scroll-bar-background-face ((t (:background ,azenburn-bg-1))))
   `(popup-isearch-match ((t (:background ,azenburn-bg :foreground ,azenburn-fg))))

   ;; android mode
   `(android-mode-debug-face ((t (:foreground ,azenburn-violet+1))))
   `(android-mode-error-face ((t (:foreground ,azenburn-light-blue :weight bold))))
   `(android-mode-info-face ((t (:foreground ,azenburn-fg))))
   `(android-mode-verbose-face ((t (:foreground ,azenburn-violet))))
   `(android-mode-warning-face ((t (:foreground ,azenburn-dark-blue))))

   ;; bm
   `(bm-face ((t (:background ,azenburn-dark-blue-1 :foreground ,azenburn-bg))))
   `(bm-fringe-face ((t (:background ,azenburn-dark-blue-1 :foreground ,azenburn-bg))))
   `(bm-fringe-persistent-face ((t (:background ,azenburn-violet-1 :foreground ,azenburn-bg))))
   `(bm-persistent-face ((t (:background ,azenburn-violet-1 :foreground ,azenburn-bg))))

   ;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,azenburn-light-blue :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,azenburn-blue :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,azenburn-violet+1 :weight bold :underline t))))

   ;; ctable
   `(ctbl:face-cell-select ((t (:background ,azenburn-beige :foreground ,azenburn-bg))))
   `(ctbl:face-continue-bar ((t (:background ,azenburn-bg-05 :foreground ,azenburn-bg))))
   `(ctbl:face-row-select ((t (:background ,azenburn-bordeaux :foreground ,azenburn-bg))))

   ;; diff
   `(diff-added ((,class (:foreground ,azenburn-violet+4 :background nil))
                 (t (:foreground ,azenburn-violet+1 :background nil))))
   `(diff-changed ((t (:foreground ,azenburn-dark-blue))))
   `(diff-removed ((,class (:foreground ,azenburn-blue :background nil))
                   (t (:foreground ,azenburn-blue-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,azenburn-bg+2))
                  (t (:background ,azenburn-fg :foreground ,azenburn-bg))))
   `(diff-file-header
     ((,class (:background ,azenburn-bg+2 :foreground ,azenburn-fg :bold t))
      (t (:background ,azenburn-fg :foreground ,azenburn-bg :bold t))))

   ;; dired+
   `(diredp-display-msg ((t (:foreground ,azenburn-beige))))
   `(diredp-compressed-file-suffix ((t (:foreground ,azenburn-light-blue))))
   `(diredp-date-time ((t (:foreground ,azenburn-green))))
   `(diredp-deletion ((t (:foreground ,azenburn-dark-blue))))
   `(diredp-deletion-file-name ((t (:foreground ,azenburn-blue))))
   `(diredp-dir-heading ((t (:foreground ,azenburn-beige :background ,azenburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,azenburn-bordeaux))))
   `(diredp-exec-priv ((t (:foreground ,azenburn-blue))))
   `(diredp-executable-tag ((t (:foreground ,azenburn-violet+1))))
   `(diredp-file-name ((t (:foreground ,azenburn-beige))))
   `(diredp-file-suffix ((t (:foreground ,azenburn-violet))))
   `(diredp-flag-mark ((t (:foreground ,azenburn-dark-blue))))
   `(diredp-flag-mark-line ((t (:foreground ,azenburn-light-blue))))
   `(diredp-ignored-file-name ((t (:foreground ,azenburn-blue))))
   `(diredp-link-priv ((t (:foreground ,azenburn-dark-blue))))
   `(diredp-mode-line-flagged ((t (:foreground ,azenburn-dark-blue))))
   `(diredp-mode-line-marked ((t (:foreground ,azenburn-light-blue))))
   `(diredp-no-priv ((t (:foreground ,azenburn-fg))))
   `(diredp-number ((t (:foreground ,azenburn-violet+1))))
   `(diredp-other-priv ((t (:foreground ,azenburn-dark-blue-1))))
   `(diredp-rare-priv ((t (:foreground ,azenburn-blue-1))))
   `(diredp-read-priv ((t (:foreground ,azenburn-violet-1))))
   `(diredp-symlink ((t (:foreground ,azenburn-dark-blue))))
   `(diredp-write-priv ((t (:foreground ,azenburn-green))))

   ;; ert
   `(ert-test-result-expected ((t (:foreground ,azenburn-violet+4 :background ,azenburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,azenburn-blue :background ,azenburn-bg))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,azenburn-blue-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,azenburn-beige+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,azenburn-blue+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,azenburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,azenburn-bordeaux :weight bold))))

   ;; flycheck
   `(flycheck-error-face ((t (:foreground ,azenburn-blue-1 :weight bold :underline t))))
   `(flycheck-warning-face ((t (:foreground ,azenburn-light-blue :weight bold :underline t))))

   ;; flymake
   `(flymake-errline ((t (:foreground ,azenburn-blue-1 :weight bold :underline t))))
   `(flymake-warnline ((t (:foreground ,azenburn-light-blue :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((t (:foreground ,azenburn-light-blue :weight bold :underline t))))
   `(flyspell-incorrect ((t (:foreground ,azenburn-blue-1 :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,azenburn-beige :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   `(erc-default-face ((t (:foreground ,azenburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,azenburn-dark-blue))))
   `(erc-keyword-face ((t (:foreground ,azenburn-beige :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,azenburn-blue :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,azenburn-violet))))
   `(erc-pal-face ((t (:foreground ,azenburn-light-blue :weight bold))))
   `(erc-prompt-face ((t (:foreground ,azenburn-light-blue :background ,azenburn-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,azenburn-violet+1))))
   `(erc-underline-face ((t (:underline t))))

   ;; git-gutter
   `(git-gutter:added ((t (:foreground ,azenburn-violet :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,azenburn-blue :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,azenburn-green :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,azenburn-fg :weight bold :inverse-video t))))

   ;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,azenburn-violet  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,azenburn-blue :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,azenburn-green :weight bold))))

   ;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,azenburn-light-blue))))
   `(gnus-summary-high-ancient ((t (:foreground ,azenburn-beige))))
   `(gnus-summary-high-read ((t (:foreground ,azenburn-violet :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,azenburn-light-blue :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,azenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,azenburn-beige))))
   `(gnus-summary-low-read ((t (:foreground ,azenburn-violet))))
   `(gnus-summary-low-ticked ((t (:foreground ,azenburn-light-blue :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,azenburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,azenburn-beige))))
   `(gnus-summary-normal-read ((t (:foreground ,azenburn-violet))))
   `(gnus-summary-normal-ticked ((t (:foreground ,azenburn-light-blue :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,azenburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,azenburn-beige))))
   `(gnus-cite-10 ((t (:foreground ,azenburn-dark-blue-1))))
   `(gnus-cite-11 ((t (:foreground ,azenburn-dark-blue))))
   `(gnus-cite-2 ((t (:foreground ,azenburn-beige-1))))
   `(gnus-cite-3 ((t (:foreground ,azenburn-beige-2))))
   `(gnus-cite-4 ((t (:foreground ,azenburn-violet+2))))
   `(gnus-cite-5 ((t (:foreground ,azenburn-violet+1))))
   `(gnus-cite-6 ((t (:foreground ,azenburn-violet))))
   `(gnus-cite-7 ((t (:foreground ,azenburn-blue))))
   `(gnus-cite-8 ((t (:foreground ,azenburn-blue-1))))
   `(gnus-cite-9 ((t (:foreground ,azenburn-blue-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,azenburn-dark-blue))))
   `(gnus-group-news-2-empty ((t (:foreground ,azenburn-violet+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,azenburn-violet+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,azenburn-beige-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,azenburn-beige-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,azenburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,azenburn-bg+2))))
   `(gnus-signature ((t (:foreground ,azenburn-dark-blue))))
   `(gnus-x ((t (:background ,azenburn-fg :foreground ,azenburn-bg))))

   ;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,azenburn-beige))))
   `(guide-key/key-face ((t (:foreground ,azenburn-violet))))
   `(guide-key/prefix-command-face ((t (:foreground ,azenburn-violet+1))))

   ;; helm
   `(helm-header
     ((t (:foreground ,azenburn-violet
                      :background ,azenburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,azenburn-dark-blue
                      :background ,azenburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,azenburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,azenburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,azenburn-bg :background ,azenburn-dark-blue-2))))
   `(helm-candidate-number ((t (:foreground ,azenburn-violet+4 :background ,azenburn-bg-1))))
   `(helm-ff-directory ((t (:foreground ,azenburn-green))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background "#f4f4f4"))
                   (t :weight bold)))
   `(hl-line ((,class (:background "#f4f4f4")) ; old emacsen
              (t :weight bold)))

   ;; hl-sexp
   `(hl-sexp-face ((,class (:background ,azenburn-bg+1))
                   (t :weight bold)))

   ;; ido-mode
   `(ido-first-match ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(ido-only-match ((t (:foreground ,azenburn-light-blue :weight bold))))
   `(ido-subdir ((t (:foreground ,azenburn-dark-blue))))

   ;; js2-mode
   `(js2-warning-face ((t (:underline ,azenburn-light-blue))))
   `(js2-error-face ((t (:foreground ,azenburn-blue :weight bold))))
   `(js2-jsdoc-tag-face ((t (:foreground ,azenburn-violet-1))))
   `(js2-jsdoc-type-face ((t (:foreground ,azenburn-violet+2))))
   `(js2-jsdoc-value-face ((t (:foreground ,azenburn-violet+3))))
   `(js2-function-param-face ((t (:foreground, azenburn-violet+3))))
   `(js2-external-variable-face ((t (:foreground "#ff0000"))))

   ;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,azenburn-violet+2))))
   `(jabber-roster-user-online ((t (:foreground ,azenburn-beige-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,azenburn-blue+1))))
   `(jabber-rare-time-face ((t (:foreground ,azenburn-violet+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,azenburn-beige-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,azenburn-blue+1))))
   `(jabber-activity-face((t (:foreground ,azenburn-blue+1))))
   `(jabber-activity-personal-face ((t (:foreground ,azenburn-beige+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((t (:foreground ,azenburn-violet+2 :background "#f0f0f0"))))

   ;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,azenburn-violet+2 :background ,azenburn-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,azenburn-blue+1 :background ,azenburn-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,azenburn-beige+1 :background ,azenburn-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,azenburn-green :background ,azenburn-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,azenburn-dark-blue :background ,azenburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))

   ;; magit
   `(magit-section-title ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(magit-branch ((t (:foreground ,azenburn-light-blue :weight bold))))
   `(magit-item-highlight ((t (:background ,azenburn-bg+1))))

   ;; egg
   `(egg-text-base ((t (:foreground ,azenburn-fg))))
   `(egg-help-header-1 ((t (:foreground ,azenburn-dark-blue))))
   `(egg-help-header-2 ((t (:foreground ,azenburn-violet+3))))
   `(egg-branch ((t (:foreground ,azenburn-dark-blue))))
   `(egg-branch-mono ((t (:foreground ,azenburn-dark-blue))))
   `(egg-term ((t (:foreground ,azenburn-dark-blue))))
   `(egg-diff-add ((t (:foreground ,azenburn-violet+4))))
   `(egg-diff-del ((t (:foreground ,azenburn-blue+1))))
   `(egg-diff-file-header ((t (:foreground ,azenburn-dark-blue-2))))
   `(egg-section-title ((t (:foreground ,azenburn-dark-blue))))
   `(egg-stash-mono ((t (:foreground ,azenburn-violet+4))))

   ;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,azenburn-violet+1))))
   `(message-header-other ((t (:foreground ,azenburn-violet))))
   `(message-header-to ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(message-header-from ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(message-header-cc ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(message-header-subject ((t (:foreground ,azenburn-light-blue :weight bold))))
   `(message-header-xheader ((t (:foreground ,azenburn-violet))))
   `(message-mml ((t (:foreground ,azenburn-dark-blue :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((t (:foreground ,azenburn-light-blue))))
   `(mew-face-header-from ((t (:foreground ,azenburn-dark-blue))))
   `(mew-face-header-date ((t (:foreground ,azenburn-violet))))
   `(mew-face-header-to ((t (:foreground ,azenburn-blue))))
   `(mew-face-header-key ((t (:foreground ,azenburn-violet))))
   `(mew-face-header-private ((t (:foreground ,azenburn-violet))))
   `(mew-face-header-important ((t (:foreground ,azenburn-beige))))
   `(mew-face-header-marginal ((t (:foreground ,azenburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,azenburn-blue))))
   `(mew-face-header-xmew ((t (:foreground ,azenburn-violet))))
   `(mew-face-header-xmew-bad ((t (:foreground ,azenburn-blue))))
   `(mew-face-body-url ((t (:foreground ,azenburn-light-blue))))
   `(mew-face-body-comment ((t (:foreground ,azenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,azenburn-violet))))
   `(mew-face-body-cite2 ((t (:foreground ,azenburn-beige))))
   `(mew-face-body-cite3 ((t (:foreground ,azenburn-light-blue))))
   `(mew-face-body-cite4 ((t (:foreground ,azenburn-dark-blue))))
   `(mew-face-body-cite5 ((t (:foreground ,azenburn-blue))))
   `(mew-face-mark-review ((t (:foreground ,azenburn-beige))))
   `(mew-face-mark-escape ((t (:foreground ,azenburn-violet))))
   `(mew-face-mark-delete ((t (:foreground ,azenburn-blue))))
   `(mew-face-mark-unlink ((t (:foreground ,azenburn-dark-blue))))
   `(mew-face-mark-refile ((t (:foreground ,azenburn-violet))))
   `(mew-face-mark-unread ((t (:foreground ,azenburn-blue-2))))
   `(mew-face-eof-message ((t (:foreground ,azenburn-violet))))
   `(mew-face-eof-part ((t (:foreground ,azenburn-dark-blue))))

   ;; mic-paren
   `(paren-face-match ((t (:foreground ,azenburn-bordeaux :background ,azenburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,azenburn-bg :background ,azenburn-green :weight bold))))
   `(paren-face-no-match ((t (:foreground ,azenburn-bg :background ,azenburn-blue :weight bold))))

   ;; mingus
   `(mingus-directory-face ((t (:foreground ,azenburn-beige))))
   `(mingus-pausing-face ((t (:foreground ,azenburn-green))))
   `(mingus-playing-face ((t (:foreground ,azenburn-bordeaux))))
   `(mingus-playlist-face ((t (:foreground ,azenburn-bordeaux ))))
   `(mingus-song-file-face ((t (:foreground ,azenburn-dark-blue))))
   `(mingus-stopped-face ((t (:foreground ,azenburn-blue))))

   ;; nav
   `(nav-face-heading ((t (:foreground ,azenburn-dark-blue))))
   `(nav-face-button-num ((t (:foreground ,azenburn-bordeaux))))
   `(nav-face-dir ((t (:foreground ,azenburn-violet))))
   `(nav-face-hdir ((t (:foreground ,azenburn-blue))))
   `(nav-face-file ((t (:foreground ,azenburn-fg))))
   `(nav-face-hfile ((t (:foreground ,azenburn-blue-4))))

   ;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,azenburn-beige    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,azenburn-violet+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,azenburn-beige-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,azenburn-violet   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,azenburn-beige-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,azenburn-violet-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,azenburn-beige    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,azenburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,azenburn-bg+3 :strike-through t))))

   ;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,azenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,azenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,azenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,azenburn-bg+1))))

   ;; org-mode
   `(org-agenda-date-today
     ((t (:foreground "black" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,azenburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,azenburn-bg+2 :foreground "black"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,azenburn-beige :underline t))))
   `(org-deadline-announce ((t (:foreground ,azenburn-blue-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,azenburn-violet+3))))
   `(org-formula ((t (:foreground ,azenburn-dark-blue-2))))
   `(org-headline-done ((t (:foreground ,azenburn-violet+3))))
   `(org-hide ((t (:foreground ,azenburn-bg-1))))
   `(org-level-1 ((t (:foreground ,azenburn-light-blue))))
   `(org-level-2 ((t (:foreground ,azenburn-violet+4))))
   `(org-level-3 ((t (:foreground ,azenburn-beige-1))))
   `(org-level-4 ((t (:foreground ,azenburn-dark-blue-2))))
   `(org-level-5 ((t (:foreground ,azenburn-bordeaux))))
   `(org-level-6 ((t (:foreground ,azenburn-violet+2))))
   `(org-level-7 ((t (:foreground ,azenburn-blue-4))))
   `(org-level-8 ((t (:foreground ,azenburn-beige-4))))
   `(org-link ((t (:foreground ,azenburn-dark-blue-2 :underline t))))
   `(org-scheduled ((t (:foreground ,azenburn-violet+4))))
   `(org-scheduled-previously ((t (:foreground ,azenburn-blue-4))))
   `(org-scheduled-today ((t (:foreground ,azenburn-beige+1))))
   `(org-special-keyword ((t (:foreground ,azenburn-fg-1 :weight normal))))
   `(org-table ((t (:foreground ,azenburn-violet+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,azenburn-light-blue))))
   `(org-todo ((t (:bold t :foreground ,azenburn-blue :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,azenburn-blue :weight bold :underline nil))))
   `(org-column ((t (:background ,azenburn-bg-1))))
   `(org-column-title ((t (:background ,azenburn-bg-1 :underline t :weight bold))))

   ;; outline
   `(outline-1 ((t (:foreground ,azenburn-light-blue))))
   `(outline-2 ((t (:foreground ,azenburn-violet+4))))
   `(outline-3 ((t (:foreground ,azenburn-beige-1))))
   `(outline-4 ((t (:foreground ,azenburn-dark-blue-2))))
   `(outline-5 ((t (:foreground ,azenburn-bordeaux))))
   `(outline-6 ((t (:foreground ,azenburn-violet+2))))
   `(outline-7 ((t (:foreground ,azenburn-blue-4))))
   `(outline-8 ((t (:foreground ,azenburn-beige-4))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,azenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,azenburn-violet+2))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,azenburn-dark-blue-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,azenburn-bordeaux))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,azenburn-violet-1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,azenburn-beige+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,azenburn-dark-blue-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,azenburn-violet+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,azenburn-beige-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,azenburn-light-blue))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,azenburn-violet))))
   `( rainbow-delimiters-depth-12-face ((t (:foreground ,azenburn-beige-5))))

   ;;rcirc
   `(rcirc-my-nick ((t (:foreground ,azenburn-beige))))
   `(rcirc-other-nick ((t (:foreground ,azenburn-light-blue))))
   `(rcirc-bright-nick ((t (:foreground ,azenburn-beige+1))))
   `(rcirc-dim-nick ((t (:foreground ,azenburn-beige-2))))
   `(rcirc-server ((t (:foreground ,azenburn-violet))))
   `(rcirc-server-prefix ((t (:foreground ,azenburn-violet+1))))
   `(rcirc-timestamp ((t (:foreground ,azenburn-violet+2))))
   `(rcirc-nick-in-message ((t (:foreground ,azenburn-dark-blue))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,azenburn-dark-blue :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,azenburn-dark-blue :bold t))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,azenburn-violet))))
   `(rpm-spec-doc-face ((t (:foreground ,azenburn-violet))))
   `(rpm-spec-ghost-face ((t (:foreground ,azenburn-blue))))
   `(rpm-spec-macro-face ((t (:foreground ,azenburn-dark-blue))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,azenburn-blue))))
   `(rpm-spec-package-face ((t (:foreground ,azenburn-blue))))
   `(rpm-spec-section-face ((t (:foreground ,azenburn-dark-blue))))
   `(rpm-spec-tag-face ((t (:foreground ,azenburn-beige))))
   `(rpm-spec-var-face ((t (:foreground ,azenburn-blue))))

   ;; rst-mode
   `(rst-level-1-face ((t (:foreground ,azenburn-light-blue))))
   `(rst-level-2-face ((t (:foreground ,azenburn-violet+1))))
   `(rst-level-3-face ((t (:foreground ,azenburn-beige-1))))
   `(rst-level-4-face ((t (:foreground ,azenburn-dark-blue-2))))
   `(rst-level-5-face ((t (:foreground ,azenburn-bordeaux))))
   `(rst-level-6-face ((t (:foreground ,azenburn-violet-1))))

   ;; show-paren
   `(show-paren-mismatch ((t (:foreground ,azenburn-blue-3 :background ,azenburn-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,azenburn-beige-1 :background ,azenburn-bg :weight bold))))

   ;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))

   ;; SLIME
   `(slime-repl-inputed-output-face ((t (:foreground ,azenburn-blue))))

   ;; tabbar
   `(tabbar-button ((t (:foreground ,azenburn-fg
                                    :background ,azenburn-bg))))
   `(tabbar-selected ((t (:foreground ,azenburn-fg
                                      :background ,azenburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,azenburn-fg
                                        :background ,azenburn-bg+1
                                        :box (:line-width -1 :style released-button)))))

   ;; term
   `(term-color-black ((t (:foreground ,azenburn-bg
                                       :background ,azenburn-bg-1))))
   `(term-color-red ((t (:foreground ,azenburn-blue-2
                                     :background ,azenburn-blue-4))))
   `(term-color-green ((t (:foreground ,azenburn-violet
                                       :background ,azenburn-violet+2))))
   `(term-color-yellow ((t (:foreground ,azenburn-light-blue
                                        :background ,azenburn-dark-blue))))
   `(term-color-blue ((t (:foreground ,azenburn-beige-1
                                      :background ,azenburn-beige-4))))
   `(term-color-magenta ((t (:foreground ,azenburn-green
                                         :background ,azenburn-blue))))
   `(term-color-cyan ((t (:foreground ,azenburn-bordeaux
                                      :background ,azenburn-beige))))
   `(term-color-white ((t (:foreground ,azenburn-fg
                                       :background ,azenburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))

   ;; volatile-highlights
   `(vhl/default-face ((t (:background ,azenburn-bg-05))))

   ;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,azenburn-dark-blue :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,azenburn-dark-blue-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,azenburn-blue-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,azenburn-dark-blue
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,azenburn-violet+2 :background ,azenburn-bg))))
   `(w3m-lnum-match ((t (:background ,azenburn-bg-1
                                     :foreground ,azenburn-light-blue
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,azenburn-dark-blue))))

   ;; whitespace-mode
   `(whitespace-space ((t (:background ,azenburn-bg+1 :foreground ,azenburn-bg+1))))
   `(whitespace-hspace ((t (:background ,azenburn-bg+1 :foreground ,azenburn-bg+1))))
   `(whitespace-tab ((t (:background ,azenburn-blue-1))))
   `(whitespace-newline ((t (:foreground ,azenburn-bg+1))))
   `(whitespace-trailing ((t (:background ,azenburn-blue))))
   `(whitespace-line ((t (:background ,azenburn-bg :foreground ,azenburn-green))))
   `(whitespace-space-before-tab ((t (:background ,azenburn-light-blue :foreground ,azenburn-light-blue))))
   `(whitespace-indentation ((t (:background ,azenburn-dark-blue :foreground ,azenburn-blue))))
   `(whitespace-empty ((t (:background ,azenburn-dark-blue))))
   `(whitespace-space-after-tab ((t (:background ,azenburn-dark-blue :foreground ,azenburn-blue))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,azenburn-blue-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,azenburn-blue-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,azenburn-light-blue))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,azenburn-beige))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,azenburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,azenburn-beige))))
   `(wl-highlight-message-citation-header ((t (:foreground ,azenburn-blue-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,azenburn-blue))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,azenburn-violet+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,azenburn-beige))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,azenburn-beige+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,azenburn-violet))))
   `(wl-highlight-message-headers-face ((t (:foreground ,azenburn-blue+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,azenburn-violet+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,azenburn-violet+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,azenburn-violet+2))))
   `(wl-highlight-message-signature ((t (:foreground ,azenburn-violet))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,azenburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,azenburn-beige))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,azenburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,azenburn-beige))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,azenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,azenburn-dark-blue))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,azenburn-green))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,azenburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((t (:foreground ,azenburn-violet+4))))

   ;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,azenburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,azenburn-bg-1 :foreground ,azenburn-bg-1))))
   )

  ;;; custom theme variables
  (custom-theme-set-variables
   'prez
   `(ansi-color-names-vector [,azenburn-bg ,azenburn-blue ,azenburn-violet
                                           ,azenburn-dark-blue ,azenburn-beige
                                           ,azenburn-green ,azenburn-bordeaux
                                           ,azenburn-fg])

   ;; fill-column-indicator
   `(fci-rule-color ,azenburn-bg-05)

   ;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,azenburn-blue-1)
       ( 40. . ,azenburn-blue)
       ( 60. . ,azenburn-light-blue)
       ( 80. . ,azenburn-dark-blue-2)
       (100. . ,azenburn-dark-blue-1)
       (120. . ,azenburn-dark-blue)
       (140. . ,azenburn-violet-1)
       (160. . ,azenburn-violet)
       (180. . ,azenburn-violet+1)
       (200. . ,azenburn-violet+2)
       (220. . ,azenburn-violet+3)
       (240. . ,azenburn-violet+4)
       (260. . ,azenburn-bordeaux)
       (280. . ,azenburn-beige-2)
       (300. . ,azenburn-beige-1)
       (320. . ,azenburn-beige)
       (340. . ,azenburn-beige+1)
       (360. . ,azenburn-green)))
   `(vc-annotate-very-old-color ,azenburn-green)
   `(vc-annotate-background ,azenburn-bg-1)))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'prez)

;;; prez-theme.el ends here
