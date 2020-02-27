;;; helm-systemd.el --- helm interface to systemd units -*- lexical-binding:t-*-

;; Copyright (C) 2016

;; Author: <lompik@oriontabArch>
;;         Štěpán Němec <stepnem@gmail.com>
;; Maintainer: Štěpán Němec <stepnem@gmail.com>
;; URL: https://github.com/stepnem/helm-systemd
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (helm "1.9.2") (with-editor "2.5.0"))
;; Keywords: convenience, processes, unix
;; Tested-with: GNU Emacs 27, 28

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

;; Other than the code and doc strings, there is README.org included
;; in the repository.

;; Corrections and constructive feedback appreciated.

;;; Code:

(require 'helm-mode)
(require 'with-editor)
(eval-when-compile (require 'subr-x))                       ; string-join

(defgroup helm-systemd nil "Helm interface to systemd units."
  :group 'helm)

(defcustom helm-systemd-unit-types
  '("service" "timer" "mount" "target" "socket" "scope" "device")
  "List of recognized systemd unit types."
  :type '(set
          (const "service")
          (const "timer")
          (const "mount")
          (const "target")
          (const "socket")
          (const "scope")
          (const "device")
          (const "path")
          (const "slice")
          (const "swap")))

(defcustom helm-systemd-list-all nil
  "If non-nil, list all units/properties, including dead/empty ones.
This just passes the \"--all\" option to systemctl."
  :type 'boolean)

(defcustom helm-systemd-list-not-loaded nil
  "If non-nil, list all unit files returned by \"systemctl list-unit-files\"."
  :type 'boolean)

(defcustom helm-systemd-line-format nil
  "Format string describing the display of helm completion candidates.
If this is nil, try to determine the format dynamically based on the
helm window width.
If non-nil, it should accomodate six string values in order:

- systemd unit name

- UnitFileState unit property value

- LoadState unit property value

- ActiveState unit property value

- SubState unit property value

- Description unit property value"
  :type '(choice (const :tag "Dynamic" nil)
                 (string :tag "Format string")))

(defcustom helm-systemd-buffer-name "*Helm systemd log*"
  "Name of the buffer where systemctl output is displayed."
  :type 'string)

(defcustom helm-systemd-angry-fruit-salad t
  "If non-nil, colorize helm completion candidates."
  :type 'boolean)

(defface helm-systemd-property
    '((t (:inherit font-lock-keyword-face)))
  "Face used for systemd property text.")

(defface helm-systemd-info
    '((t (:inherit font-lock-comment-face)))
  "Face used for systemd informative text.")

(defface helm-systemd-pid
    '((t (:inherit font-lock-constant-face)))
  "Face used for process ID.")

(defface helm-systemd-static
    '((t (:foreground "magenta")))
  "Face used for systemd static unit status.")

(defface helm-systemd-running
    '((t (:foreground "green" :bold t)))
  "Face used for systemd running unit status.")

(defface helm-systemd-active
    '((t (:foreground "green")))
  "Face used for systemd active unit status.")

(defface helm-systemd-failed
    '((t (:foreground "red" :bold t)))
  "Face used for systemd failed unit status.")

(defvar helm-systemd-status-font-lock-keywords
  '(("^ *\\([^:\n]+?\\): " 1 'helm-systemd-property)
    ("active (running)" 0 'helm-systemd-running)
    ("active (mounted)" 0 'helm-systemd-running)
    ("active (exited)" 0 'helm-systemd-active)
    ("inactive (dead)" 0 'helm-systemd-info)

    ("[fF]ailed" 0 'helm-systemd-failed)

    ("─\\([0-9]+\\)" 1 'helm-systemd-pid)
    ("[●🔜] .*" 0 'helm-systemd-info))  ; command lines
  "Default expressions to highlight in `helm systemd log'.")

(define-derived-mode helm-systemd-status-mode fundamental-mode "Systemd-log"
  "Major mode for viewing systemd status logs.
\\{helm-systemd-status-mode-map}"
  (setq-local font-lock-defaults '(helm-systemd-status-font-lock-keywords))
  (font-lock-mode t))

(defun helm-systemd-command-line-option ()
  "Return default systemctl options as a string."
  (concat "--full --no-pager --no-legend -t "
          (car helm-systemd-unit-types)
          (if helm-systemd-list-all " --all")))

(defvar helm-systemd-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<C-return>")    'helm-cr-empty-string)
    (define-key map (kbd "<M-RET>")       'helm-cr-empty-string)
    (define-key map (kbd "C-]")           'helm-systemd-next-type)
    (define-key map (kbd "C-[")           'helm-systemd-prev-type)
    map)
  "Keymap for `helm-systemd'.")

(defvar helm-systemd--unit-regexp
  (mapconcat (lambda (prop) (format "%s=\\(.*\\)" prop))
             '("Id" "Description" "LoadState" "ActiveState"
               "SubState" "UnitFileState")
             "\n")
  "Regexp used for property value extraction from systemctl command output.")

(defun helm-systemd--get-candidates (&optional sysd-options)
  "Return a list of systemd service units.
SYSD-OPTIONS is an options string passed to the systemd subcommand."
  (let (result
        unit-ids
        (window-width (with-helm-window (window-width)))
        (format helm-systemd-line-format)
        (max-unit-width 25)
        (unit-list
         (split-string
          (shell-command-to-string
           (concat "systemctl " sysd-options " "
                   (helm-systemd-command-line-option)
                   " --property=Id,Description,LoadState,"
                   "ActiveState,SubState,UnitFileState show '*'"))
          "\n\n" t)))
    (dolist (unit unit-list)
      (string-match "^Id=\\(.*\\)$" unit)
      (push (match-string 1 unit) unit-ids)
      (let ((len (- (match-end 1) (match-beginning 1))))
        (when (> len max-unit-width)
          (setq max-unit-width len))))
    (unless format
      (let ((width (number-to-string (min max-unit-width
                                          (- window-width 50)))))
        (setq format
              (concat "%-" width "." width "s"
                      " %-10.10s"
                      " %-10.10s"
                      " %-10.10s"
                      " %-10.10s"
                      " %s"))))
    (dolist (unit unit-list)
      (string-match helm-systemd--unit-regexp unit)
      (push (helm-systemd--format-line format
                                       (match-string 1 unit)
                                       (match-string 2 unit)
                                       (match-string 3 unit)
                                       (match-string 4 unit)
                                       (match-string 5 unit)
                                       (match-string 6 unit))
            result))
    (when helm-systemd-list-not-loaded
      (dolist (item (split-string
                     (shell-command-to-string
                      (concat "systemctl " sysd-options " "
                              (helm-systemd-command-line-option)
                              " list-unit-files"))
                     "\n" t))
        (pcase-let ((`(,id ,state) (split-string item)))
          (unless (member id unit-ids)
            (push (format format id "" "" (helm-systemd--propertize state) "" "")
                  result)))))
    (sort result #'string-lessp)))

(defun helm-systemd--propertize (string)
  "Return STRING propertized with colors."
  (dolist (pair '(("active" . helm-systemd-active)
                  ("enabled" . helm-systemd-active)
                  ("failed" . helm-systemd-failed)
                  ("listening" . helm-systemd-active)
                  ("mounted" . helm-systemd-running)
                  ("running" . helm-systemd-running)
                  ("runtime" . helm-systemd-info)
                  ("static" . helm-systemd-static))
                string)
    (setq string
          (replace-regexp-in-string
           (concat "\\(?:\\`\\| \\)\\(" (car pair) "\\)" "\\(?: \\|\n\\|\\'\\)")
           (propertize (car pair) 'face (cdr pair))
           string nil t 1))))

(defun helm-systemd--format-line (format id description load active sub state)
  "Format systemd unit candidate line for helm display.
FORMAT is the format string for the following values:
ID is the unit name.
DESCRIPTION is the Description unit property value.
LOAD is the LoadState unit property value.
ACTIVE is the ActiveState unit property value.
SUB is the SubState unit property value.
STATE is the UnitFileState unit property value.
Cf. `helm-systemd--unit-regexp' and `helm-systemd--get-candidates' for
details."
  (when (string= "enabled-runtime" state)
    (setq state "runtime"))
  (let ((line (format format id state load active sub description))
        (case-fold-search nil))
    (if (not helm-systemd-angry-fruit-salad)
        line
      (helm-systemd--propertize line))))

(defun  helm-systemd--display (unit-command unit &optional userp nodisplay)
  "Display output of systemctl UNIT-COMMAND for UNIT in a buffer.
USERP non-nil means UNIT is a user unit. With NODISPLAY non-nil the
buffer is not displayed, only its contents updated."
  (with-current-buffer (get-buffer-create helm-systemd-buffer-name)
    (helm-systemd-status-mode)
    (goto-char (point-max))
    (let ((command (string-join `("systemctl"
                                  ,(helm-systemd-command-line-option)
                                  ,(when userp "--user")
                                  ,unit-command "--" ,unit)
                                " "))
          output)
      (insert "\n🔜 " command "\n")
      (if (or userp (string= unit-command "status"))
          (insert  (shell-command-to-string command))
        (with-temp-buffer
          (cd "/sudo::/")
          (setq output (shell-command-to-string (concat "sudo " command))))
        (insert output))
      (insert "\n"))
    (unless nodisplay
      (display-buffer (current-buffer)))))

(defun helm-systemd-next-type ()
  "Cycle to the next systemd unit type."
  (interactive)
  (setq helm-systemd-unit-types
        (append (cdr helm-systemd-unit-types)
                (list (car helm-systemd-unit-types))))
  (with-helm-alive-p
    (helm-force-update )))

(defun helm-systemd-prev-type ()
  "Cycle to the previous systemd unit type."
  (interactive)
  (setq helm-systemd-unit-types
        (append (last helm-systemd-unit-types)
                (remove (car (last helm-systemd-unit-types))
                        helm-systemd-unit-types)))
  (with-helm-alive-p
    (helm-force-update )))

(defun helm-systemd-show-status (_line &optional userp)
  "Show unit status. USERP non-nil means this is a user unit."
  (let ((units (helm-marked-candidates)))
    (mapc (lambda (line)
            (let ((unit (car (split-string line))))
              (helm-systemd--display "status" unit userp)))
          units)))

(defconst helm-systemd-actions-alist
  '(("print". "Printed")
    ("restart". "Restarted")
    ("stop" ."Stopped")
    ("start". "Started")))

(defun helm-systemd-make-action (sysd-verb &optional userp)
  "Helper macro for systemd helm sources.
Return a lambda function suitable as a helm action.
SYSD-VERB (string) is the systemd subcommand, USERP non-nil means this
action is for a user unit."
  (lambda (_)
    (mapc (lambda (candidate)
            (let ((unit (car (split-string candidate))))
              (helm-systemd--display sysd-verb unit userp t)
              (message (concat
                        (cdr (assoc sysd-verb helm-systemd-actions-alist))
                        " "
                        unit))))
          (helm-marked-candidates))))

(defun helm-source-systemd ()
  "Helm source for systemd units."
  (helm-build-sync-source "system units"
    :candidates #'helm-systemd--get-candidates
    :action (helm-make-actions
             "Print"   (helm-systemd-make-action "status")
             "Restart" (helm-systemd-make-action "restart")
             "Stop"    (helm-systemd-make-action "stop")
             "Start"   (helm-systemd-make-action "start"))
    :persistent-action #'helm-systemd-show-status
    :persistent-help "Show unit status"
    :keymap helm-systemd-map))

(defun helm-source-systemd-user ()
  "Helm source for systemd user units."
  (helm-build-sync-source "user units"
    :candidates (lambda () (helm-systemd--get-candidates "--user"))
    :action (helm-make-actions
             "Print"   (helm-systemd-make-action "status" t)
             "Restart" (helm-systemd-make-action "restart" t)
             "Stop"    (helm-systemd-make-action "stop" t)
             "Start"   (helm-systemd-make-action "start" t)
             "Edit with Emacs"
             (lambda (candidate)
               (add-to-list 'with-editor-envvars "SYSTEMD_EDITOR" t)
               (with-editor-async-shell-command
                   (concat "systemctl --user --full edit "
                           (car (split-string candidate))))))
    :persistent-action (lambda (line)
                         (funcall #'helm-systemd-show-status line t))
    :persistent-help "Show unit status"
    :keymap helm-systemd-map))

;;;###autoload
(defun helm-systemd ()
  "Use `helm' to inspect and manipulate systemd units.
Use `helm-systemd-next-type' and `helm-systemd-prev-type' to cycle
between available unit types.

\\{helm-systemd-map}"
  (interactive)
  (helm
   :sources `(,(helm-source-systemd) ,(helm-source-systemd-user))
   :truncate-lines t
   :buffer "*helm systemd*"))

(provide 'helm-systemd)
;;; helm-systemd.el ends here
