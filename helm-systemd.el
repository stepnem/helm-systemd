;;; helm-systemd.el --- helm interface to systemd units -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author:  <lompik@oriontabArch>
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (helm "1.9.2") (with-editor "2.5.0"))
;; Keywords: convenience, processes, unix

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

;;

;;; Code:

(require 'cl-lib)
(require 'helm)
(eval-when-compile (require 'rx))
(require 'subr-x)
(require 'with-editor)

(defgroup helm-systemd nil "Helm interface to systemd units."
  :group 'helm)

(defcustom helm-systemd-unit-types
  '("service" "timer" "mount" "target" "socket" "scope" "device")
  "List of recognized systemd unit types."
  :group 'helm-systemd
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
  :group 'helm-systemd
  :type 'boolean)

(defcustom helm-systemd-list-not-loaded nil
  "If non-nil, list all unit files returned by \"systemctl list-unit-files\"."
  :group 'helm-systemd
  :type 'boolean)

(defcustom helm-systemd-buffer-name "*Helm systemd log*"
  "Name of the buffer where systemctl output is displayed."
  :group 'helm-systemd
  :type 'string)

(defconst helm-systemd-actions-list
  '(("print". "Printed")
    ("restart". "Restarted")
    ("stop" ."Stopped")
    ("start". "Started")))

(defcustom helm-systemd-angry-fruit-salad t
  "If non-nil, colorize helm completion candidates."
  :group 'helm-systemd
  :type 'boolean)

(defface helm-systemd-property
    '((t (:inherit font-lock-keyword-face)))
  "Face used for systemd property text."
  :group 'helm-systemd)

(defface helm-systemd-info
    '((t (:inherit font-lock-comment-face)))
  "Face used for systemd informative text."
  :group 'helm-systemd)

(defface helm-systemd-static
    '((t (:foreground "magenta")))
  "Face used for systemd static unit status."
  :group 'helm-systemd)

(defface helm-systemd-running
    '((t (:foreground "green" :bold t)))
  "Face used for systemd running unit status."
  :group 'helm-systemd)

(defface helm-systemd-active
    '((t (:foreground "green")))
  "Face used for systemd active unit status."
  :group 'helm-systemd)

(defface helm-systemd-failed
    '((t (:foreground "red" :bold t)))
  "Face used for systemd failed unit status."
  :group 'helm-systemd)

(defvar helm-systemd-status-font-lock-keywords
  '(("\\(Loaded\\|Active\\|Status\\|Docs\\|Process\\|Main PID\\|Tasks\\|CGroup\\):" 1 'helm-systemd-property)
    ("active (running)" 0 'helm-systemd-running)
    ("inactive (dead)" 0 'helm-systemd-info)
    ("active (exited)" 0 'helm-systemd-active)

    ("[fF]ailed" 0 'helm-systemd-failed)

    ("─\\([0-9]+\\)" 1 'helm-systemd-info) ; PID
    ("[●🔜] .*" 0 'helm-systemd-info)  ; command lines
    "Default expressions to highlight in `helm systemd log'."))

(define-derived-mode helm-systemd-status-mode fundamental-mode "Systemd-log"
  "Major mode for viewing systemd status logs.
\\{helm-systemd-status-mode-map}"
  (setq-local font-lock-defaults '(helm-systemd-status-font-lock-keywords))
  (font-lock-mode t))

(defun helm-systemd-command-line-option ()
  "Return default systemctl options as a string."
  (concat "--no-pager --no-legend -t "
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

(defun helm-systemd-systemctl-command (&rest args)
  "Construct string with: 'systemctl default-args' ARGS."
  (string-join (push (concat "systemctl " (helm-systemd-command-line-option))
                     args)
               " "))

(defun helm-systemd-propertize-description (unit description userp)
  "Propertize systemd UNIT DESCRIPTION for display in helm completions buffer.
USERP non-nil means this is a user unit."
  (let* ((state
          (string-trim
           (shell-command-to-string
            (string-join
             `("systemctl" "is-enabled " ,(when userp "--user") "--" ,unit)
             " "))))
         (state (pcase state
                  ("enabled-runtime" "runtime")
                  ((rx bos "Failed") "XXXXXX")
                  (_ state)))
         (line (concat (format "%8s" state) " " description)))
    (if (not helm-systemd-angry-fruit-salad)
        line
      (setq line (propertize line 'face 'helm-systemd-info))
      (dolist (pair '(("enabled" . helm-systemd-info)
                      ("runtime" . helm-systemd-info)
                      ("static" . helm-systemd-static)
                      ("active" . helm-systemd-active)
                      ("mounted" . helm-systemd-running)
                      ("running" . helm-systemd-running)
                      ("listening" . font-lock-keyword-face)
                      ("failed" . helm-systemd-failed)
                      ("XXXXXX" . helm-systemd-failed))
                    line)
        (setq line
              (replace-regexp-in-string (concat "\\<" (car pair) "\\>")
                                        (propertize (car pair) 'face (cdr pair))
                                        line nil t))))))

(defun helm-systemd-get-candidates (&optional sysd-options)
  "Return a list of systemd service units.
SYSD-OPTIONS is an options string passed to the systemd subcommand."
  (let* ((result ())
         (left-col-width 25)
         (table (make-hash-table :test 'equal)))
    (mapc (lambda (line) (puthash (car (split-string line)) line table))
          (split-string (shell-command-to-string
                         (helm-systemd-systemctl-command
                          "list-units" sysd-options))
                        "\n" t))
    (when helm-systemd-list-not-loaded
      (mapc (lambda (line)
              (let ((unit (car (split-string line))))
                (unless (gethash unit table nil)
                  (puthash unit line table))))
            (split-string (shell-command-to-string
                           (helm-systemd-systemctl-command
                            "list-unit-files" sysd-options))
                          "\n" t)))
    (maphash (lambda (unit _)
               (setq left-col-width (max left-col-width (length unit))))
             table)
    (setq left-col-width (number-to-string left-col-width))
    (maphash (lambda (unit line)
               (let ((desc (string-trim-left (substring line (length unit)))))
                 (push (format (concat "%-" left-col-width "s %s")
                               unit
                               (helm-systemd-propertize-description
                                unit desc sysd-options))
                       result)))
             table)
    (nreverse result)))

(defun  helm-systemd-display (unit-command unit &optional userp nodisplay)
  "Display output of systemctl UNIT-COMMAND for UNIT in a buffer.
USERP non-nil means UNIT is a user unit. With NODISPLAY non-nil the
buffer is not displayed, only its contents updated."
  (with-current-buffer (get-buffer-create helm-systemd-buffer-name)
    (helm-systemd-status-mode)
    (let ((command
           (helm-systemd-systemctl-command (if userp "--user")
                                           unit-command "--" unit))
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
              (helm-systemd-display "status" unit userp)))
          units)))

(defmacro helm-systemd-make-action (sysd-verb userp)
  "Helper macro for systemd helm sources.
Return a lambda function suitable as a helm action.
SYSD-VERB (string) is the systemd subcommand, USERP non-nil means this
action is for a user unit."
  `(lambda (_ignore)
     (mapc (lambda (candidate)
             (helm-systemd-display ,sysd-verb (car (split-string candidate)) ,userp t)
             (message (concat
                       (cdr (assoc ,sysd-verb helm-systemd-actions-list))
                       " "
                       (car (split-string candidate)))))
           (helm-marked-candidates))))

(defun helm-systemd-build-source ()
  "Helm source for systemd units."
  (helm-build-sync-source "systemd"
    :candidates #'helm-systemd-get-candidates
    :action (helm-make-actions
             "Print"   (helm-systemd-make-action "status" nil)
             "Restart" (helm-systemd-make-action "restart" nil)
             "Stop"    (helm-systemd-make-action "stop" nil)
             "Start"   (helm-systemd-make-action "start" nil))
    :persistent-action #'helm-systemd-show-status
    :persistent-help "Show unit status"
    :keymap helm-systemd-map))

(defun helm-systemd-build-source-user ()
  "Helm source for systemd user units."
  (helm-build-sync-source "Systemd User"
    :candidates (lambda () (helm-systemd-get-candidates "--user"))
    :action (helm-make-actions
             "Print"   (helm-systemd-make-action "status" t)
             "Restart" (helm-systemd-make-action "restart" t)
             "Stop"    (helm-systemd-make-action "stop" t)
             "Start"   (helm-systemd-make-action "start" nil)
             "Edit with Emacs"   (lambda (candidate)
                                   (add-to-list 'with-editor-envvars "SYSTEMD_EDITOR" t)
                                   (with-editor-async-shell-command (concat "systemctl --user --full edit " (car (split-string candidate))) )))
    :persistent-action (lambda (line) (funcall #'helm-systemd-show-status line t))
    :persistent-help "Show unit status"
    :keymap helm-systemd-map))

;;;###autoload
(defun helm-systemd ()
  "Use `helm' to inspect and manipulate systemd units."
  (interactive)
  (helm
   :sources (mapcar #'funcall
                    '(helm-systemd-build-source helm-systemd-build-source-user))
   :truncate-lines t
   :buffer "*helm systemd*"))

(provide 'helm-systemd)
;;; helm-systemd.el ends here
