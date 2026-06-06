(defpackage #:barista-plugins/git-reps
  (:use #:cl)
  (:import-from #:barista/menu
                #:defmenu)
  (:import-from #:barista/plugin
                #:defplugin)
  (:import-from #:priority-queue)
  (:import-from #:local-time)
  (:import-from #:f-underscore
                #:f_
                #:_
                #:f_%)
  (:import-from #:cl-ppcre
                #:register-groups-bind))
(in-package barista-plugins/git-reps)

(defparameter *max-interval* (* 24 3600)
  "Maximumum number of seconds between checks.")

(defvar *repositories* nil
  "A list of repositories to check their status.")

(defparameter *queue*
  (priority-queue:make-pqueue #'local-time:timestamp<)
  "This queue will store a closures which will check repositories state from time to time.

   Closures will always be sorted by a timestamp. When closure is done, it will reschedule itself unless
   the directory was removed.

   Also, there will be a closure which searches a new reporitories and adds them to the queue.")

(defparameter *osascript-template*
  "tell application \"iTerm2\"
  tell current window
    create tab with default profile
    tell current session
      write text \"cd '~A';clear;git status\"
    end tell
  end tell
end tell")

(defclass repository ()
  ((path :type string
         :initarg :path
         :reader get-path)
   (last-changed-at :type local-time:timestamp
                    ;; We will set this field to a correct value
                    ;; after the first check
                    :initform (local-time:now)
                    :accessor get-last-changed-at
                    :documentation "This field will be set to the last commits date.
                                    An interval between checks depends on this value
                                    because the old repositories more likely will not
                                    change their state.")
   (num-changes :type (or integer null)
                :initform 0
                :accessor get-num-changes)))


(defmethod print-object ((repo repository) stream)
  (print-unreadable-object (repo stream :type t)
    (format stream "~A ~A"
            (get-path repo)
            (get-last-changed-at repo))))

(defun make-repository (path)
  (check-type path string)
  (make-instance 'repository
                 :path path))

(defun search-git-repositories ()
  "Returns a list of strings with paths to directories
   containing git repositories."
  (log:info "Searching new git-repositories")
  (loop with lines = (cl-strings:split
                      (with-output-to-string (s)
                        (uiop:run-program "find ~ -name .git -type d"
                                          :output s
                                          :ignore-error-status t))
                      #\Newline)
        for line in lines
        unless (string= line "")
        do (let* ((path (subseq line 0 (- (length line)
                                          4))))
             (unless (member path *repositories* :key 'get-path)
               (let ((repo (make-repository path)))
                 (push repo
                       *repositories*)
                 (queue-item
                  (local-time:now)
                  (lambda ()
                    (update-and-reschedule repo)))))))
  (values))


(defun get-git-output (path command)
  (with-output-to-string (s)
    (uiop:with-current-directory (path)
      (uiop:run-program command :output s))))

(defun is-comment (line)
  (cl-strings:starts-with line "#"))

(defun get-status (path)
  "Returns a number of changed files."
  (let* ((output (get-git-output path "git status --porcelain=v2 --branch"))
         (lines (cl-strings:split output #\Newline))
         (changes 0))
    (dolist (line lines)
      (unless (string= line "")
        ;; This counts a number not pushed/pulled commits
        (register-groups-bind (ahead behind)
            ("# branch.ab \\+(\\d+) -(\\d+)" line)
          (incf changes
                (+ (parse-integer ahead)
                   (parse-integer behind))))
        ;; This counts a number of changed files
        (unless (is-comment line)
          (incf changes))))
    (values changes)))

(defun get-last-update (path)
  "Returns a local-time:timestamp of the last commit."
  (let* ((output (get-git-output path "git log --format=%ct -n 1"))
         (unixtime (parse-integer output :junk-allowed t)))
    (when unixtime
      (local-time:unix-to-timestamp unixtime))))

(defun update-repo (repo)
  (let ((path (get-path repo)))
    (when (uiop:directory-exists-p path)
      (let* ((num-changes (get-status path))
             (last-update (get-last-update path)))
        (setf (get-num-changes repo)
              num-changes)
        (when last-update
          (setf (get-last-changed-at repo)
                last-update))
        (values t)))))

(defun get-next-check-time (repo)
  (declare (ignorable repo))
  (local-time:adjust-timestamp (local-time:now)
    (offset :sec (* 60 60))))

(defun queue-item (timestamp callable)
  (priority-queue:pqueue-push
      callable
      timestamp
      *queue*))

(defun open-dir-in-terminal (dir)
  ;; Here we consider the user is using iTerm.
  ;; Probly we also need to look if iTerm is running and if it is availble in Applications.
  (with-output-to-string (s)
    (uiop:run-program (list "osascript"
                            "-e"
                            (format nil *osascript-template* dir))
                      :output s
                      :error-output s
                      :ignore-error-status t)))


(defun update-and-reschedule (repo)
  (log:info "Updating" repo)
  (cond
    ((update-repo repo)
     (log:info "Rescheduling")
     (queue-item
      (get-next-check-time repo)
      (lambda ()
        (update-and-reschedule repo))))
    (t
     (log:info "Removing repo from the list" repo)
     (alexandria:removef *repositories* repo))))


(defun get-first-timestamp ()
  (unless (priority-queue:pqueue-empty-p *queue*)
    (priority-queue:pqueue-front-key *queue*)))


(defun process-queue ()
  "Processes all tasks for which timestamp is less than (now)."
  (loop for key = (get-first-timestamp) then (get-first-timestamp)
        if (or (null key)
               (local-time:timestamp> key
                                      (local-time:now)))
        do (return)
        else
        do (funcall (priority-queue:pqueue-pop *queue*))))

(defun ignore-repository (repository period)
  (log:info "Ignoring repository" repository period))

(defun build-submenu-for (repository)
  (barista/menu:build-menu
    (barista/menu:add-item "Ignore for a day" :callback (f_% (ignore-repository repository :day)))
    (barista/menu:add-item "Ignore for a week" :callback (f_% (ignore-repository repository :week)))))

(defun update ()
  (log:info "Updating git repositories status")
  (when (pqueue:pqueue-empty-p *queue*)
    (log:debug "Queue is empty. Kickstarting!")
    (queue-item (local-time:now)
                'search-git-repositories))
  (process-queue)

  (setf (barista/plugin:get-title barista/vars:*plugin*)
        (format nil "G ~@[~A~]"
                (when *repositories*
                    (length *repositories*))))
  
  (setf (barista/plugin:get-menu barista/vars:*plugin*)
        (barista/menu:build-menu
          (loop for repository in *repositories*
                for num-changes = (get-num-changes repository)
                when (and num-changes
                          (> num-changes 0))
                do (barista/menu:add-item
                    (format nil "~A (~A)"
                            (get-path repository)
                            num-changes)
                    :callback (funcall (f_ (f_% (open-dir-in-terminal _)))
                                       (get-path repository))
                    :submenu (build-submenu-for repository))))))

(defplugin git-reps
    ()
  (:title "G")
  (:every (15 :seconds)
          (update)))

;; TODO:
;; * Ignoring items
;; * Making icon become red
;; * Nice icon with git logo
