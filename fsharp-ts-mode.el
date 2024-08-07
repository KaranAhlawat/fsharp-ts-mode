;;; fsharp-ts-mode.el --- F# Tree-Sitter Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karan Ahlawat

;; Author: Karan Ahlawat <ahlawatkaran12@gmail.com>
;; Version: 1.0.0
;; Filename: fsharp.el
;; Package-Requires: ((emacs "29.1"))
;; Keywords: fsharp, languages, tree-sitter
;; URL: https://github.com/KaranAhlawat/fsharp-ts-mode

;; This program is free software; you can redistribute it and/or modify
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

;; This package provides a tree-sitter based major mode for the F#
;; programming language.  Currently, the supported features and their
;; statuses are
;; 1. font-locking (complete, looking for bugs and maintainance)
;; 2. imenu (basic support, needs work)
;; 3. indentation

;;; Code:

(require 'rx)
(require 'treesit)
(require 'thingatpt)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-parent-while "treesit.c")
(declare-function treesit-parent-until "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")

(defcustom fsharp-ts-indent-offset 4
  "Number of spaces for each indentation in `fsharp-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'fsharp-ts)

;; utility functions -- begin

(defun fsharp-ts--node-type= (type node)
  "Compare TYPE and type of NODE for string equality."
  (string= type (treesit-node-type node)))

;; utility functions -- end

(defvar fsharp-ts--syntax-table nil
  "Syntax table in use in fsharp mode buffers.")
(unless fsharp-ts--syntax-table
  (setq fsharp-ts--syntax-table (make-syntax-table))
                                        ; backslash is an escape sequence
  (modify-syntax-entry ?\\ "\\" fsharp-ts--syntax-table)

                                        ; ( is first character of comment start
  (modify-syntax-entry ?\( "()1n" fsharp-ts--syntax-table)
                                        ; * is second character of comment start,
                                        ; and first character of comment end
  (modify-syntax-entry ?*  ". 23n" fsharp-ts--syntax-table)
                                        ; ) is last character of comment end
  (modify-syntax-entry ?\) ")(4n" fsharp-ts--syntax-table)

                                        ; // is the beginning of a comment "b"
  (modify-syntax-entry ?/ ". 12b" fsharp-ts--syntax-table)
                                        ; // \n is the end of a comment "b"
  (modify-syntax-entry ?\n "> b" fsharp-ts--syntax-table)

                                        ; quote and underscore are part of symbols
                                        ; so are # and ! as they can form part of types/preprocessor
                                        ; directives and also keywords
  (modify-syntax-entry ?' "_" fsharp-ts--syntax-table)
  (modify-syntax-entry ?_ "_" fsharp-ts--syntax-table)
  (modify-syntax-entry ?# "_" fsharp-ts--syntax-table)
  (modify-syntax-entry ?! "_" fsharp-ts--syntax-table)

                                        ; ISO-latin accented letters and EUC kanjis are part of words
  (let ((i 160))
    (while (< i 256)
      (modify-syntax-entry i "w" fsharp-ts--syntax-table)
      (setq i (1+ i)))))

;; KEYWORDS AND LITERALS

(defvar fsharp-ts--keywords
  '("as" "assert" "begin" "end" "done" "default"
    "in" "do" "do!" "event" "field" "fun" "function"
    "get" "set" "lazy" "new" "of" "param" "property"
    "struct" "val" "module" "namespace" "with")
  "Keywords for `fsharp-ts-mode'.")

(defvar fsharp-ts--keywords-conditional
  '("if" "then" "else" "elif" "when" "match" "match!"
    "return" "return!" "yield" "yield!" "for" "while"
    "downto" "to")
  "Control flow keywords for `fsharp-ts-mode'.")

(defvar fsharp-ts--keywords-operator
  '("and" "or" "not" "upcast" "downcast")
  "Operator keywords for `fsharp-ts-mode'.")

(defvar fsharp-ts--keywords-import
  '("open" "#r" "#load")
  "Import keywords for `fsharp-ts-mode'.")

(defvar fsharp-ts--keywords-modifier
  '("abstract" "delegate" "static" "inline"
    "mutable" "override" "rec" "global" (access_modifier))
  "Modifier keywords for `fsharp-ts-mode'.")

(defvar fsharp-ts--keywords-function
  '("let" "let!" "use" "use!" "member")
  "Function keywords for `fsharp-ts-mode'.")

(defvar fsharp-ts--keywords-type
  '("enum" "type" "inherit" "interface")
  "Type definition keywords for `fsharp-ts-mode'.")

(defvar fsharp-ts--keywords-try
  '("try" "with" "finally")
  "Try construct keywords for `fsharp-ts-mode'.")

(defvar fsharp-ts--punctuation-bracket
  '("(" ")" "{" "}" "[" "]"
    "[|" "|]" "{|" "|}" "[<" ">]")
  "Brackets for `fsharp-ts-mode'.")

(defvar fsharp-ts--operators
  '("|" "=" ">" "<" "-" "~" "->"
    "<-" "&&" "||" ":>" ":?>" "::"
    (infix_op)
    (prefix_op))
  "Operators for `fsharp-ts-mode'.")

(defun fsharp-ts--check-fails (node)
  "Check if NODE is a keyword for indicating failure."
  (seq-contains-p '("failwith" "failwithf" "raise" "reraise") (treesit-node-text node)))

(defun fsharp-ts--check-builtin (node)
  "Check if NODE is a builtin type."
  (seq-contains-p '("bool" "byte" "sbyte" "int16" "uint16" "int" "uint" "int64" "uint64" "nativeint"
                    "unativeint" "decimal" "float" "double" "float32" "single" "char" "string" "unit")
                  (treesit-node-text node)))

(defun fsharp-ts--check-builtin-module (node)
  "Check if NODE is a builtin module."
  (seq-contains-p '("Array" "Async" "Directory" "File" "List" "Option" "Path" "Map" "Set" "Lazy" "Seq" "Task" "String" "Result")
                  (treesit-node-text node)))

(defvar fsharp-ts--delimiters
  '("," ";")
  "Delimiters for `fsharp-ts-mode'.")

;; FONT LOCK
(defvar fsharp-ts-font-lock-rules
  `( :language fsharp
     :feature comment
     (((line_comment) @doc @font-lock-doc-markup-face
       (:match "^///" @doc))
      [(line_comment) (block_comment)] @font-lock-comment-face)

     :language fsharp
     :feature keyword
     ([,@fsharp-ts--keywords] @font-lock-keyword-face
      ((identifier) @font-lock-keyword-face
       (:equal "this" @font-lock-keyword-face))
      [,@fsharp-ts--keywords-conditional] @font-lock-keyword-face
      [,@fsharp-ts--keywords-import] @font-lock-keyword-face
      [,@fsharp-ts--keywords-modifier] @font-lock-keyword-face
      [,@fsharp-ts--keywords-function] @font-lock-keyword-face
      [,@fsharp-ts--keywords-operator] @font-lock-keyword-face
      [,@fsharp-ts--keywords-type] @font-lock-keyword-face
      (try_expression
       [,@fsharp-ts--keywords-try] @font-lock-keyword-face)
      (match_expression "with" @font-lock-keyword-face)
      ((identifier) @font-lock-keyword-face
       (:pred fsharp-ts--check-fails
              @font-lock-keyword-face))
      (compiler_directive_decl) @font-lock-keyword-face
      (preproc_line
       "#line" @font-lock-preprocessor-face)
      (preproc_if
       [
        "#if" @font-lock-preprocessor-face
        "#endif" @font-lock-preprocessor-face
        ]
       condition: (_):? @font-lock-preprocessor-face)
      (preproc_else
       "#else" @font-lock-preprocessor-face))

     :language fsharp
     :feature literal
     ([(xint)
       (int)
       (int16)
       (uint16)
       (int32)
       (uint32)
       (int64)
       (uint64)
       (nativeint)
       (unativeint)
       (ieee32)
       (ieee64)
       (float)
       (decimal)] @font-lock-number-face
       (bool) @font-lock-keyword-face
       [(string)
        (triple_quoted_string)
        (verbatim_string)
        (char)] @font-lock-string-face)

     :language fsharp
     :feature punctuation
     ([,@fsharp-ts--punctuation-bracket] @font-lock-bracket-face
      (format_string_eval ["{" "}"] @font-lock-punctuation-face)
      [,@fsharp-ts--delimiters] @font-lock-delimiter-face)


     :language fsharp
     :feature type
     ([(type) (atomic_type)] @font-lock-type-face
      (type_name type_name: (_) @font-lock-type-face))

     :language fsharp
     :feature operator
     ([,@fsharp-ts--operators] @font-lock-operator-face
      (op_identifier) @font-lock-operator-face)

     :language fsharp
     :feature constant
     ((const
       [(_) @font-lock-constant-face
        (unit) @font-lock-builtin-face])
      "null" @font-lock-builtin-face
      ((type
        (long_identifier (identifier) @font-lock-builtin-face))
       (:pred fsharp-ts--check-builtin
              @font-lock-builtin-face))
      (union_type_case (identifier) @font-lock-constant-face)
      (rules
       (rule
        :anchor
        pattern: [(const) @font-lock-constant-face
                  (identifier_pattern :anchor (_) @font-lock-constant-face)
                  (_) @font-lock-variable-name-face]
        block: (_)))
      (ce_expression
       :anchor
       (_) @font-lock-constant-face)
      ((value_declaration
        (attributes
         (attribute
          (type
           (long_identifier
            (identifier) @attribute_name))))
        (function_or_value_defn
         (value_declaration_left
          :anchor
          (_) @font-lock-constant-face)))
       (:equal @attribute_name "Literal")))

     :language fsharp
     :feature function
     ((function_declaration_left
       (access_modifier):?
       :anchor
       (_) @font-lock-function-name-face
       :anchor
       (_) @font-lock-variable-name-face)

      (member_defn
       (method_or_prop_defn
        [
         (property_or_ident) @font-lock-function-name-face
         (property_or_ident
          instance: (identifier) @font-lock-variable-name-face
          method: (identifier) @font-lock-function-name-face)
         ]
        args: (_):* @font-lock-variable-name-face))

      (application_expression
       :anchor
       (_) @font-lock-function-call-face
       :anchor
       (_) @font-lock-variable-use-face)

      ((infix_expression
        (_)
        (infix_op) @operator
        :anchor
        (_) @font-lock-function-call-face)
       (:equal @operator "|>"))

      ((infix_expression
        (_) @font-lock-function-call-face
        :anchor
        (infix_op) @operator
        (_))
       (:equal @operator "<|")))

     :language fsharp
     :feature variable
     ((primary_constr_args (_) @font-lock-variable-name-face)
      (identifier_pattern
       :anchor
       (_) @font-lock-constant-face
       :anchor
       (_) @font-lock-variable-name-face)
      (field_initializer
       field: (_) @font-lock-property-name-face)
      (record_pattern
       (field_pattern :anchor (_) @font-lock-constant-face))
      (record_fields
       (record_field
        :anchor
        (identifier) @font-lock-property-name-face))
      (dot_expression
       base: (_):? @font-lock-variable-use-face)
      (value_declaration_left :anchor (_) @font-lock-variable-name-face)
      (declaration_expression (identifier) @font-lock-variable-name-face)
      (identifier) @font-lock-variable-use-face)

     :language fsharp
     :feature module
     :override t
     ((fsi_directive_decl :anchor (string) @font-lock-type-face)
      (import_decl :anchor (_) @font-lock-type-face)
      (named_module
       name: (_) @font-lock-type-face)
      (namespace
       name: (_) @font-lock-type-face)
      (module_defn
       :anchor
       (_) @font-lock-type-face)
      ((identifier) @font-lock-type-face
       (:pred fsharp-ts--check-builtin-module
              @font-lock-type-face)))

     :language fsharp
     :feature extra
     ((wildcard_pattern) @font-lock-regex-face
      (member_signature
       :anchor
       (identifier) @font-lock-function-name-face
       (curried_spec
        (arguments_spec
         "*":* @font-lock-operator-face
         (argument_spec
          (argument_name_spec
           "?":? @font-lock-regex-face
           name: (_) @font-lock-variable-name-face)))))
      (member_signature
       :anchor
       (identifier) @font-lock-function-name-face
       (_)))))

;;; WIP
(defvar fsharp-ts--indent-rules
  `((fsharp
     ((node-is ,(rx ?=)) parent-bol fsharp-ts-indent-offset)
     (no-node parent 0))))

(defun fsharp-ts--defun-name (_)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  "Bazza")

;;;###autoload
(define-derived-mode fsharp-ts-mode prog-mode " F# (TS)"
  "Major mode for F# files using tree-sitter."
  :group 'fsharp-ts
  :syntax-table fsharp-ts--syntax-table

  (when (treesit-ready-p 'fsharp)
    (treesit-parser-create 'fsharp)

    ;; Comments
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "//" (* (syntax whitespace))))

    (setq-local treesit-font-lock-settings (apply #'treesit-font-lock-rules fsharp-ts-font-lock-rules))
    (setq-local treesit-font-lock-feature-list '((comment keyword)
                                                 (type constant module)
                                                 (extra function variable)
                                                 (operator literal punctuation)))


    (setq-local treesit-simple-indent-rules fsharp-ts--indent-rules)

    ;; Navigation.
    (setq-local treesit-defun-name-function #'fsharp-ts--defun-name)

    ;; TODO (could possibly be more complex?)
    ;; (setq-local treesit-simple-imenu-settings
    ;;             `(("Class" "\\`class_definition\\'" nil nil)
    ;;               ("Trait" "\\`trait_definition\\'" nil nil)
    ;;               ("Enum" "\\`enum_definition\\'" nil nil)
    ;;               ("Object" "\\`object_definition\\'" nil nil)
    ;;               ("Function" "\\`function_definition\\'" nil nil)
    ;;               ("Definition" "\\`function_declaration'" nil nil)))

    (treesit-major-mode-setup)))

(provide 'fsharp-ts-mode)
;;; fsharp-ts-mode.el ends here
