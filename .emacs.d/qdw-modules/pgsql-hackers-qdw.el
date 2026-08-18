(when (load-file-if-exists-qdw
        (expand-file-name
          "~/src-3/pg/postgresql/src/tools/editors/emacs.samples"))
  ;;;; Emacs ships with both perl-mode and cperl-mode. I prefer cperl-mode.
  ;;;; So the remaining code in this file, except the (add-to-list ...) call,
  ;;;; I pasted from emacs.samples and replaced "perl" with "cperl".

  ;; Style that matches the formatting used by
  ;; src/tools/pgindent/perltidyrc.
  (defun pgsql-cperl-style ()
    "cperl style adjusted for PostgreSQL project"
    (interactive)
    (setq cperl-brace-imaginary-offset 0)
    (setq cperl-brace-offset 0)
    (setq cperl-continued-statement-offset 2)
    (setq cperl-continued-brace-offset (- cperl-continued-statement-offset))
    (setq cperl-indent-level 4)
    (setq cperl-label-offset -2)
    ;; Next two aren't marked safe-local-variable, so .dir-locals.el omits them.
    (setq cperl-indent-continued-arguments 4)
    (setq cperl-indent-parens-as-block t)
    (setq indent-tabs-mode t)
    (setq tab-width 4))

  (add-hook 'cperl-mode-hook
            (defun postgresql-cperl-mode-hook ()
              (when (string-match "/postgres\\(ql\\)?/" buffer-file-name)
                (pgsql-cperl-style))))

  (add-to-list 'auto-mode-alist
               '("/postgres\\(ql\\)?/.*\\.dat\\'" . cperl-mode)))
