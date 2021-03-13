;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; TODO(Austin): Migrate to emacs 27 when this is fixed?
;; https://github.com/hlissner/doom-emacs/issues/1170

(setq user-full-name "Austin Green"
      user-mail-address "agreen@liftoff.io"
      doom-big-font-increment 2
      doom-theme 'doom-outrun-electric
      org-directory "~/org/"
      display-line-numbers-type nil
      ; Add go commands
      gofmt-command "goimports"
      gofmt-args '("-local" "liftoff/")
      ; Cider config (for my packages)
      cider-clojure-cli-global-options "-A:liftoff:dev:nrepl"
      ;; Increase size of recentf
      recentf-max-menu-items 2000
      recentf-max-saved-items 2000
      projectile-project-search-path "${REPOS}/liftoff/"
      global-whitespace-mode t
      ;; Spaces > Tabs. Use 2 spaces for tabs in JS.
      indent-tabs-mode nil
      js-indent-level 2)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(map! :leader
      :desc "Split window vertically"
      "w /" #'evil-window-vsplit)

(map! :leader
      :desc "Split window horizontally"
      "w -" #'evil-window-split)

(map! :leader
      :desc "Switch window right"
      "w <right>" #'evil-window-right)

(map! :leader
      :desc "Switch window left"
      "w <left>" #'evil-window-left)

(map! :leader
      :desc "Switch window down"
      "w <down>" #'evil-window-down)

(map! :leader
      :desc "Switch window up"
      "w <up>" #'evil-window-up)

; I slip <Shift>+w and q too often.
(evil-ex-define-cmd "W" #'evil-write)
(evil-ex-define-cmd "Q" #'evil-quit)

(defun sync-env ()
  (when (memq window-system '(mac ns x))
    (let ((env-pair-re "^\\([^=[:space:]]+\\)=\\(.*\\)$"))
      (with-temp-buffer
        (shell-command (concat shell-file-name " -i -c e") t)
        (goto-char (point-min))
        (while (re-search-forward env-pair-re nil t)
          (let ((name (match-string 1))
                (val (match-string 2)))
            (setenv name val)
            (when (string-equal "PATH" name)
              (setq eshell-path-env val
                    exec-path (append (parse-colon-path val) (list exec-directory))))))))))

(sync-env)

; Enable formatting on save
(load! "$REPOS/liftoff/exp/emacs/cljfmt.el")
(add-hook 'before-save-hook 'cljfmt-before-save)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'js2-mode-hook 'prettier-js-mode)

; Use diff-hl-mode always
(global-diff-hl-mode)

; Check for spelling mistakes
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

; Use centaur tabs mode
(map! :leader
      :desc "Go to left tab"
      "<left>" #'centaur-tabs-backward)

(map! :leader
      :desc "Go to right tab"
      "<right>" #'centaur-tabs-forward)

; Clojure productivity
(map! :leader "a r" #'paredit-wrap-sexp)
(map! :leader "a t" #'paredit-split-sexp)

; Enable centaur mode by default
; (centaur-tabs-mode)

; TODO(Austin): Make own theme.
