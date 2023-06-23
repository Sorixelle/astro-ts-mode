;;; astro-mode.el --- Major mode for editing Astro templates  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ruby Iris Juric

;; Author: Ruby Iris Juric <ruby@srxl.me>
;; Homepage: https://github.com/Sorixelle/astro-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "29"))
;; Keywords: languages

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a major mode with syntax highlighting for Astro
;; templates. It leverages Emacs' built-in tree-sitter support, as well as
;; virchau13's tree-sitter grammar for Astro.
;;
;; More info:
;; README: https://github.com/Sorixelle/astro-mode
;; tree-sitter-astro: https://github.com/virchau13/tree-sitter-astro
;; Astro: https://astro.build/

;;; Code:

(require 'treesit)
(require 'typescript-ts-mode)
(require 'css-mode)

(defgroup astro ()
  "Major mode for editing Astro templates."
  :group 'languages)

(defcustom astro-mode-indent-offset 2
  "Number of spaces for each indentation step in `astro-mode'."
  :type 'integer
  :group 'astro
  :package-version '(astro-mode . "1.0.0"))

(defvar astro-mode--indent-rules
  `((astro
     ((parent-is "fragment") column-0 0)
     ((node-is "frontmatter") column-0 0)
     ((node-is "/>") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((node-is "end_tag") parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "element") parent-bol astro-mode-indent-offset)
     ((parent-is "script_element") parent-bol astro-mode-indent-offset)
     ((parent-is "style_element") parent-bol astro-mode-indent-offset)
     ((parent-is "start_tag") parent-bol astro-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol astro-mode-indent-offset))
    (css . ,(append (alist-get 'css css--treesit-indent-rules)
                    '(((parent-is "stylesheet") parent-bol 0))))
    (tsx . ,(alist-get 'tsx (typescript-ts-mode--indent-rules 'tsx))))
  "Tree-sitter indentation rules for `astro-mode'.")

(defun astro-mode--prefix-font-lock-features (prefix settings)
  "Prefix with PREFIX the font lock features in SETTINGS."
  (mapcar (lambda (setting)
            (list (nth 0 setting)
                  (nth 1 setting)
                  (intern (format "%s-%s" prefix (nth 2 setting)))
                  (nth 3 setting)))
          settings))

(defvar astro-mode--font-lock-settings
  (append
   (astro-mode--prefix-font-lock-features
    "tsx"
    (typescript-ts-mode--font-lock-settings 'tsx))
   (astro-mode--prefix-font-lock-features "css" css--treesit-settings)
   (treesit-font-lock-rules
    :language 'astro
    :feature 'astro-comment
    '((comment) @font-lock-comment-face
      (frontmatter ("---") @font-lock-comment-face))

    :language 'astro
    :feature 'astro-keyword
    '("doctype" @font-lock-keyword-face)

    :language 'astro
    :feature 'astro-definition
    '((tag_name) @font-lock-function-name-face)

    :language 'astro
    :feature 'astro-string
    '((quoted_attribute_value) @font-lock-string-face
      (attribute_name) @font-lock-constant-face)

    :language 'astro
    :feature 'astro-bracket
    '((["<" ">" "</" "/>" "{" "}"]) @font-lock-bracket-face)

    :language 'tsx
    :feature 'astro-bracket
    '((jsx_opening_element (["<" ">"]) @font-lock-bracket-face)
      (jsx_closing_element (["<" ">" "/"]) @font-lock-bracket-face)
      (jsx_self_closing_element (["<" ">" "/"]) @font-lock-bracket-face))))
  "Tree-sitter font-lock settings for `astro-mode'.")

(defvar astro-mode--range-settings
  (treesit-range-rules
   :embed 'tsx
   :host 'astro
   '((frontmatter (raw_text) @cap)
     ;; TODO: this doesn't really parse correctly, because emacs' tree-sitter
     ;;       integration just shoves everything in the same language into one
     ;;       long chunk to parse, instead of parsing each range individually.
     ;;       syntax highlighting doesn't look awful with it though, so i'm
     ;;       leaving it in for now. better than nothing. need to investigate
     ;;       alternatives though.
     (interpolation (raw_text) @cap)
     (script_element (raw_text) @cap))

   :embed 'css
   :host 'astro
   '((style_element (raw_text) @cap))))

;;;###autoload
(defun astro-mode--advice-for-treesit-buffer-root-node (&optional lang)
  "Return the current ranges for the LANG parser in the current buffer.

If LANG is omitted, return ranges for the first language in the parser list.

If `major-mode' is currently `astro-mode', or if LANG is 'astro, this function
instead always returns t."
  (if (or (eq lang 'astro) (not (eq major-mode 'astro-mode)))
    t
    (treesit-parser-included-ranges
     (treesit-parser-create
      (or lang (treesit-parser-language (car (treesit-parser-list))))))))

;;;###autoload
(defun astro-mode--advice-for-treesit--merge-ranges (_ new-ranges _ _)
  "Returns truthy if `major-mode' is `astro-mode', and if NEW-RANGES is non-nil."
  (and (eq major-mode 'astro-mode) new-ranges))

(defun astro-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (when (equal (treesit-node-type node) "tag_name")
    (treesit-node-text node t)))

(defun astro-mode--treesit-language-at-point (point)
  "Return the language at POINT."
  (let* ((range nil)
         (language-in-range
          (cl-loop
           for parser in (treesit-parser-list)
           do (setq range
                    (cl-loop
                     for range in (treesit-parser-included-ranges parser)
                     if (and (>= point (car range)) (<= point (cdr range)))
                     return parser))
           if range
           return (treesit-parser-language parser))))
    (or language-in-range 'astro)))

;;;###autoload
(define-derived-mode astro-mode html-mode "Astro"
  "Major mode for editing Astro templates, powered by tree-sitter."
  :group 'astro

  (unless (treesit-ready-p 'astro)
    (error "Tree-sitter grammar for Astro isn't available"))

  (unless (treesit-ready-p 'css)
    (error "Tree-sitter grammar for CSS isn't available"))

  (unless (treesit-ready-p 'tsx)
    (error "Tree-sitter grammar for Typescript/TSX isn't available"))

  (treesit-parser-create 'astro)

  ;; Comments and text content
  (setq-local treesit-text-type-regexp
              (regexp-opt '("comment" "text")))

  ;; Indentation rules
  (setq-local treesit-simple-indent-rules astro-mode--indent-rules
              css-indent-offset astro-mode-indent-offset)

  ;; Font locking
  (setq-local treesit-font-lock-settings astro-mode--font-lock-settings
              treesit-font-lock-feature-list
              '((astro-comment astro-keyword astro-definition css-selector
                               css-comment css-query css-keyword tsx-comment
                               tsx-declaration tsx-jsx)
                (astro-string css-property css-constant css-string tsx-keyword
                              tsx-string tsx-escape-sequence)
                (css-error css-variable css-function css-operator tsx-constant
                           tsx-expression tsx-identifier tsx-number tsx-pattern
                           tsx-property)
                (astro-bracket css-bracket tsx-function tsx-bracket
                               tsx-delimiter)))

  ;; Embedded languages
  (setq-local treesit-range-settings astro-mode--range-settings
              treesit-language-at-point-function
              #'astro-mode--treesit-language-at-point)

  (treesit-major-mode-setup))

;;;###autoload
(if (treesit-ready-p 'astro)
    (add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-mode)))

;; HACK: treesit-buffer-root-node seems to be returning a node spanning the
;;       whole file if treesit-parser-included-ranges returns nil for that
;;       language (ie. that language doesn't appear in the file). This screws
;;       with font locking, causing CSS syntax highlighting to be applied over
;;       the whole file if there's no <style> tag. To work around this, we
;;       advise treesit-buffer-root-node to make it return nil if there's no
;;       range for the language, instead of a node covering the file. I haven't
;;       seen any adverse effects come out of this, and I've done my best to
;;       make sure this stays isolated to astro-mode buffers, so hopefully
;;       nothing explodes too hard. I feel like this is a bug in treesit tbh,
;;       I'll have to report it there. But yeah, I'm so sorry about this. This
;;       is awful, I know. I hate it too. I don't know what else to do though.
;;;###autoload
(advice-add
 #'treesit-buffer-root-node
 :before-while
 #'astro-mode--advice-for-treesit-buffer-root-node)

;; HACK: treesit--merge-ranges doesn't properly account for when new-ranges is
;;       nil (ie. the code block covering that range was deleted), and returns
;;       old-ranges when it should probably also be returning nil. As a result,
;;       syntax highlighting from the old language sticks around and tries to
;;       apply itself to whatever takes it's place, which is usually a different
;;       language. This looks weird. We can work around this by advising
;;       treesit--merge-ranges to just short circuit and return nil if
;;       new-ranges is also nil. Another bug in treesit to report.
;;;###autoload
(advice-add
 #'treesit--merge-ranges
 :before-while
 #'astro-mode--advice-for-treesit--merge-ranges)

(provide 'astro-mode)
;;; astro-mode.el ends here
