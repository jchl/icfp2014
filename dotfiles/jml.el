(setq jml-operators '("fst" "snd" "not" "break" "null" "head" "tail" "and" "or"))
(setq jml-keywords '("fn" "fun" "val" "const" "main" "if" "then" "else" "let" "in" "end" "trace"))
(setq jml-constants '("true" "false"))


(setq jml-operators-regexp (regexp-opt jml-operators 'words))
(setq jml-keywords-regexp (regexp-opt jml-keywords 'words))
(setq jml-constants-regexp (regexp-opt jml-constants 'words))

(setq jml-font-lock-keywords
  `(
    (, jml-operators-regexp . font-lock-builtin-face)
    (, jml-keywords-regexp . font-lock-keyword-face)
    (, jml-constants-regexp . font-lock-constant-face)
))


(define-derived-mode jml-mode fundamental-mode
  (setq font-lock-defaults '(jml-font-lock-keywords))
  (setq mode-name "JML")
)
