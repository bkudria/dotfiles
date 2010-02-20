;; Initialize a few variables
(setq-default project-base-dir nil)

(dir-locals "~/projects/work/younoodle/younoodle.org"
            `((nil . ((project-base-dir . ,(concat base-dir "/"))
                      (tags-file-name   . ,(concat base-dir "/.etags"))))))
