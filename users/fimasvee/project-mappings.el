;; Intelliadv

(defun custom-persp/intelliadv ()
  (interactive)
  (custom-persp "intelliadv"
                (find-file "~/projects/intelliadv/todo.org")))

(define-key persp-mode-map (kbd "C-x p i") 'custom-persp/intelliadv)

;; Emacs Rocks

(defun custom-persp/emacsrocks ()
  (interactive)
  (custom-persp "emacsrocks"
                (find-file "~/projects/emacsrocks/site/lib/episodes.rb")))

(define-key persp-mode-map (kbd "C-x p r") 'custom-persp/emacsrocks)

(project-specifics "projects/emacsrocks"
  (ffip-local-patterns "*.js" "*.scss" "*.org" "*.rb" "*.erb"))

;; zombietdd.com

(defun custom-persp/zombietdd.com ()
  (interactive)
  (custom-persp "zombietdd.com"
                (find-file "~/projects/site-ztdd/lib/episodes.rb")))

(define-key persp-mode-map (kbd "C-x p s") 'custom-persp/zombietdd.com)

(project-specifics "projects/site-ztdd"
  (ffip-local-patterns "*.js" "*.scss" "*.org" "*.rb" "*.erb"))

;; Blockout

(defun custom-persp/blockout ()
  (interactive)
  (custom-persp "blockout"
                (find-file "~/projects/blockout/")))

(define-key persp-mode-map (kbd "C-x p bl") 'custom-persp/blockout)

(project-specifics "projects/blockout"
  (ffip-local-patterns "*.js" "*.css"))

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "projects/blockout" (buffer-file-name))
              (setq js2-additional-externs '("BLOCKS"))
              (set (make-local-variable 'buster-default-global) "BLOCKS")
              (set (make-local-variable 'buster-add-default-global-to-iife) t)
              (set (make-local-variable 'buster-use-strict) t)
              (set (make-local-variable 'buster-test-prefix) "")
              (set (make-local-variable 'js2r-use-strict) t))))

;; kodemaker.no

(defun custom-persp/kodemaker ()
  (interactive)
  (custom-persp "kodemaker" (find-file "~/projects/kodemaker.no/")))

(define-key persp-mode-map (kbd "C-x p k") 'custom-persp/kodemaker)

(project-specifics "projects/kodemaker.no"
  (ffip-local-patterns "*.clj" "*.js" "*.css" "*.edn" "*.html"))

;; mytomatoes

(defun custom-persp/mytomatoes ()
  (interactive)
  (custom-persp "mytomatoes" (find-file "~/projects/mytomatoes/")))

(define-key persp-mode-map (kbd "C-x p y") 'custom-persp/mytomatoes)

(defun js2-mytomatoes-settings ()
  (when (string-match-p "projects/mytomatoes" (buffer-file-name))
    (make-variable-buffer-local 'js2-basic-offset)
    (setq js2-basic-offset 4)))

(add-hook 'js2-mode-hook 'js2-mytomatoes-settings)

(project-specifics "projects/mytomatoes"
  (ffip-local-patterns "*.clj" "*.js" "*.css" "*.edn" "*.sql"))

;; Oiiku

(defun custom-persp/oiiku ()
  (interactive)
  (custom-persp "oiiku" (find-file "~/projects/oiiku/")))

(define-key persp-mode-map (kbd "C-x p o") 'custom-persp/oiiku)

(defun js2-oiiku-settings ()
  (when (string-match-p "projects/oiiku" (buffer-file-name))
    (setq js2-additional-externs '("angular" "cull" "dome" "app" "expect" "it" "inject" "beforeEach" "describe"))
    (make-variable-buffer-local 'js2-basic-offset)
    (setq js2-basic-offset 4)))

(add-hook 'js2-mode-hook 'js2-oiiku-settings)

(project-specifics "projects/oiiku"
  (set (make-local-variable 'sgml-basic-offset) 2))

(project-specifics "projects/oiiku"
  (ffip-local-patterns "*.clj" "*.js" "*.css" "*.edn" "*.html")
  (ffip-local-excludes "target"))

(defface prodigy-dull-face
  '((((class color)) :foreground "#999999"))
  "Gray color indicating waiting."
  :group 'prodigy)

(prodigy-define-status :id 'running :face 'prodigy-dull-face)
(prodigy-define-status :id 'exception :face 'prodigy-red-face)

(prodigy-define-tag
  :name 'ring
  :on-output (lambda (service output)
               (when (s-matches? "Started server on port" output)
                 (prodigy-set-status service 'ready))
               (when (s-matches? "Exception" output)
                 (prodigy-set-status service 'exception))))

(prodigy-define-service
  :name "kodemaker.no"
  :cwd "~/projects/kodemaker.no/"
  :command "lein"
  :args '("trampoline" "ring" "server-headless")
  :port 3333
  :tags '(ring))

(comment
 (prodigy-define-service
   :name "datomic oiiku-central-api"
   :cwd "~/data/datomic-free-0.8.4218"
   :path '("~/data/datomic-free-0.8.4218/bin")
   :command "transactor"
   :args '("../../projects/oiiku/oiiku-central-api/oiiku-central-api-server/config/datomic-transactor-free.properties")
   :tags '(oiiku datomic pillar))

 (prodigy-define-service
   :name "datomic oiiku-badges-app"
   :cwd "~/data/datomic-free-0.8.4218"
   :path '("~/data/datomic-free-0.8.4218/bin")
   :command "transactor"
   :args '("../../projects/oiiku/oiiku-badges-app/config/datomic-transactor-free.properties")
   :tags '(oiiku datomic pillar))

 (prodigy-define-service
   :name "datomic oiiku-screen-admin"
   :cwd "~/data/datomic-free-0.8.4218"
   :path '("~/data/datomic-free-0.8.4218/bin")
   :command "transactor"
   :args '("../../projects/oiiku/oiiku-screen-admin-app/config/datomic-transactor-free.properties")
   :tags '(oiiku datomic pillar))

 (prodigy-define-service
   :name "elasticsearch"
   :cwd "~/data/elasticsearch-1.0.0.Beta1/"
   :path '("~/data/elasticsearch-1.0.0.Beta1/bin/")
   :command "elasticsearch"
   :args '("-f")
   :tags '(oiiku elasticsearch pillar)
   :on-output (lambda (service output)
                (when (s-matches? "] started" output)
                  (prodigy-set-status service 'ready))))

 (prodigy-define-service
   :name "oiiku-central-api"
   :cwd "~/projects/oiiku/oiiku-central-api/oiiku-central-api-server/"
   :command "lein"
   :args '("trampoline" "ring" "server-headless")
   :tags '(oiiku pillar))

 (prodigy-define-service
   :name "oiiku-sso"
   :cwd "~/projects/oiiku/oiiku-sso/"
   :command "lein"
   :args '("trampoline" "ring" "server-headless")
   :tags '(oiiku pillar))

 (prodigy-define-service
   :name "oiiku-event-admin"
   :cwd "~/projects/oiiku/oiiku-event-admin/"
   :command "lein"
   :args '("trampoline" "ring" "server-headless")
   :tags '(oiiku pillar))

 (prodigy-define-service
   :name "oiiku-attendants-app"
   :cwd "~/projects/oiiku/oiiku-attendants-app/"
   :command "lein"
   :args '("trampoline" "ring" "server-headless")
   :tags '(oiiku pillar))

 (prodigy-define-service
   :name "oiiku-messages-app"
   :cwd "~/projects/oiiku/oiiku-messages-app/"
   :command "lein"
   :args '("trampoline" "ring" "server-headless")
   :tags '(oiiku messages))

 (prodigy-define-service
   :name "oiiku-messages-gateway"
   :cwd "~/projects/oiiku/oiiku-messages-gateway/"
   :command "lein"
   :args '("trampoline" "ring" "server-headless")
   :tags '(oiiku messages))

 (prodigy-define-service
   :name "oiiku-messages-dummy"
   :cwd "~/projects/oiiku/oiiku-messages-dummy/"
   :path '("~/projects/oiiku/oiiku-messages-dummy/")
   :command "gradlew"
   :args '("run")
   :tags '(oiiku messages))

 (prodigy-define-service
   :name "oiiku-badges-app"
   :cwd "~/projects/oiiku/oiiku-badges-app/"
   :command "lein"
   :args '("trampoline" "ring" "server-headless")
   :tags '(oiiku))

 (prodigy-define-service
   :name "oiiku-invitations-app"
   :cwd "~/projects/oiiku/oiiku-invitations-app/"
   :command "lein"
   :args '("trampoline" "ring" "server-headless")
   :tags '(oiiku))

 (prodigy-define-service
   :name "oiiku-screen-admin-app"
   :cwd "~/projects/oiiku/oiiku-screen-admin-app/"
   :command "lein"
   :args '("trampoline" "ring" "server-headless")
   :tags '(oiiku)))

;; FINN Reise

(defun custom-persp/travel ()
  (interactive)
  (custom-persp "travel"
                (find-file "~/projects/finn-reise/travel-app/")))

(define-key persp-mode-map (kbd "C-x p t") 'custom-persp/travel)

(require 'travel-mode)

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "travel-app" (buffer-file-name))
              (--each '("cull" "dome" "bane") (add-to-list 'js2-additional-externs it))
              (js2-fetch-autolint-externs "~/projects/finn-reise/travel-app/web/src/autolint.js")
              (setq js2r-path-to-tests "/test/javascript/tests/")
              (setq js2r-path-to-sources "/main/webapp/scripts/")
              (setq js2r-test-suffix "Test")
              (setq buster-default-global "FINN.travel")
              (setq buster-add-default-global-to-iife t)
              (setq buster-test-prefix "")
              (set (make-local-variable 'js2-basic-offset) 4)
              (set (make-local-variable 'buster-use-strict) t)
              (set (make-local-variable 'js2r-use-strict) t))))

(project-specifics "travel-app"
  (ffip-local-patterns "*.js" "*.tag" "*.jsp" "*.css" "*.org" "*.vm" "*jawr.properties")
  (set (make-local-variable 'sgml-basic-offset) 2)
  (travel-mode))

;; kodemaker.no

(project-specifics "projects/kodemaker.no"
  (ffip-local-patterns "*.js" "*.css" "*.edn" "*.adoc" "*.clj" "*.md"))

;; Zombie CLJ

(project-specifics "zombieclj"
  (ffip-local-patterns "*.js" "*.css" "*.edn" "*.clj" "*.cljs" "*.md" "*.html"))

;; Zombie TDD

(defun custom-persp/zombie ()
  (interactive)
  (custom-persp "zombie"
                (find-file "~/projects/zombietdd/todo.org")))

(define-key persp-mode-map (kbd "C-x p z") 'custom-persp/zombie)

(project-specifics "projects/zombietdd"
  (ffip-local-patterns "*.js" "*.jade" "*.css" "*.json" "*.md"))

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "projects/zombietdd" (buffer-file-name))
              (fci-mode 1)
              (setq js2-additional-externs '("ZOMBIE" "Faye" "EventEmitter" "when"))
              (set (make-local-variable 'buster-default-global) "ZOMBIE")
              (set (make-local-variable 'buster-add-default-global-to-iife) t)
              (set (make-local-variable 'buster-use-strict) t)
              (set (make-local-variable 'buster-test-prefix) "")
              (set (make-local-variable 'js2r-use-strict) t))))

;; creator

(defun custom-persp/creator ()
  (interactive)
  (custom-persp "creator"
                (find-file "~/projects/creator/README.md")))

(define-key persp-mode-map (kbd "C-x p cr") 'custom-persp/creator)

(project-specifics "projects/creator"
  (ffip-local-patterns "*.js" "*.md"))

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "projects/creator" (buffer-file-name))
              (fci-mode 1)
              (set (make-local-variable 'buster-default-global) "creator")
              (set (make-local-variable 'buster-add-default-global-to-iife) nil)
              (set (make-local-variable 'buster-use-strict) t)
              (set (make-local-variable 'buster-test-prefix) "")
              (set (make-local-variable 'js2r-use-strict) t))))

;; jztdd

(defun magnars/jztdd-setup ()
  (when (string-match-p "projects/jz-tdd" (buffer-file-name))
    (--each '("cull" "app" "JZTDD" "angular") (add-to-list 'js2-additional-externs it))
    (setq js2r-path-to-tests "/test/")
    (setq js2r-path-to-sources "/public/")
    (set (make-local-variable 'buster-default-global) "")
    (set (make-local-variable 'buster-add-default-global-to-iife) nil)
    (set (make-local-variable 'buster-test-prefix) "")))

(add-hook 'js2-mode-hook 'magnars/jztdd-setup)

;; kodemake

(defun magnars/kodemake-setup ()
  (when (string-match-p "projects/kodemake" (buffer-file-name))
    (setq js2r-path-to-sources "/source/javascripts/")
    (set (make-local-variable 'buster-default-global) "MAKE")
    (set (make-local-variable 'buster-test-prefix) "")))

(add-hook 'js2-mode-hook 'magnars/kodemake-setup)

;; culljs

(defun custom-persp/culljs ()
  (interactive)
  (custom-persp "culljs"
                (find-file "~/projects/culljs/todo.org")))

(define-key persp-mode-map (kbd "C-x p cu") 'custom-persp/culljs)

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "projects/culljs" (buffer-file-name))
              (fci-mode 1)
              (setq js2-additional-externs '("cull"))
              (set (make-local-variable 'buster-default-global) "cull")
              (set (make-local-variable 'buster-add-default-global-to-iife) t)
              (set (make-local-variable 'buster-use-strict) t)
              (set (make-local-variable 'buster-test-prefix) "")
              (set (make-local-variable 'js2-basic-offset) 4)
              (set (make-local-variable 'js2r-use-strict) t))))

;; buster

(defun custom-persp/buster ()
  (interactive)
  (custom-persp "buster"
                (find-file "~/stuff/fs-watch-tree/todo.org")))

(define-key persp-mode-map (kbd "C-x p bu") 'custom-persp/buster)

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "stuff/fs-watch-tree" (buffer-file-name))
              (fci-mode 1)
              (set (make-local-variable 'buster-default-global) "cull")
              (set (make-local-variable 'buster-add-default-global-to-iife) nil)
              (set (make-local-variable 'buster-use-strict) nil)
              (set (make-local-variable 'buster-test-prefix) "")
              (set (make-local-variable 'js2-basic-offset) 4)
              (set (make-local-variable 'js2r-use-strict) nil))))

;; SPiD

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "projects/spid" (buffer-file-name))
              (set (make-local-variable 'js2-basic-offset) 4))))

(add-hook 'js2-mode-hook
          (lambda ()
            (when (string-match-p "projects/spid/reckoning" (buffer-file-name))
              (require 'single-quotes-mode)
              (single-quotes-mode 1))))

(project-specifics "spid/reckoning"
  (ffip-local-patterns "*.scss" "*.html" "*.js")
  (set (make-local-variable 'css-indent-offset) 2))

;; no-adventur

(defun custom-persp/no-adventur ()
  (interactive)
  (custom-persp "no-adventur"
                (find-file "~/projects/no-adventur/")))

(define-key persp-mode-map (kbd "C-x p n") 'custom-persp/no-adventur)

(project-specifics "no-adventur"
  (ffip-local-patterns "*.clj" "*.js" "*.css" "*.edn")
  (ffip-local-excludes "target"))

;; Adventur

(defun custom-persp/adventur ()
  (interactive)
  (custom-persp "adventur"
                (find-file "~/projects/adventur/nettsidene/adventur_no/source/backlog.txt")))

(define-key persp-mode-map (kbd "C-x p a") 'custom-persp/adventur)

(project-specifics "adventur/nettsidene/adventur_no/"
  (ffip-local-patterns "*.js" "*.php" "*.css")
  (ffip-local-excludes "compiled_pages" "compiler_test_files" "simpletest" "compressed"))

(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories "compiled_pages")))

;; Adventur Master

(defun custom-persp/adventur-master ()
  (interactive)
  (custom-persp "adventur-master"
                (require 'adventur-mode)
                (find-file "~/projects/eventyr/master/notat.org")))

(define-key persp-mode-map (kbd "C-x p m") 'custom-persp/adventur-master)

;; What the emacs.d
(project-specifics "projects/what-the-emacsd/posts"
  (buffer-local-set-key (kbd "C-c C-c") 'what-the-emacsd-publish))

;; Emacs

(defun custom-persp/emacs ()
  (interactive)
  (custom-persp "emacs"
                (find-file "~/.emacs.d/init.el")))

(project-specifics ".emacs.d"
  (ffip-local-excludes "swank")
  (ffip-local-patterns "*.el" "*.md" "*.org"))

(define-key persp-mode-map (kbd "C-x p e") 'custom-persp/emacs)

;; Org

(defun custom-persp/org ()
  (interactive)
  (custom-persp "org")
  (find-file "~/Dropbox/org/"))

(define-key persp-mode-map (kbd "C-<f6>") 'custom-persp/org)
