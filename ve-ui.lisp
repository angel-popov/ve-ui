;;;; ve-ui.lisp

(in-package #:ve-ui)

;;; "ve-ui" goes here. Hacks and glory await!

(plan nil)
(defvar *agent*  (make-session))
(defvar *visitor* (make-session))
(defvar *visitor1* (make-session))
(defun login(user pass)
  (element-send-keys (find-element "//input[@name='email']" :by :xpath) user)
  (element-send-keys (find-element "//input[@name='password']" :by :xpath) pass)
  (element-click (find-element "//button[contains(.,'Sign In')]" :by :xpath))
  (loop :repeat 30 :until (find-elements "//span[@id='agent_status' and @style='background:#72aa01;']" :by :xpath) do
     (sleep 1))
  (not (zerop (length (find-elements "//span[@id='agent_status' and @style='background:#72aa01;']" :by :xpath)))))

(defun agent-login(url user pass)
  #'(lambda(session)
      (use-session session)
      (setf (url) url)
      (login user pass)))

(defun agent-logout()
  #'(lambda(session)
      (use-session session)
      (element-click (find-element "//a[@href='#']" :by :xpath))
      t))

(defun visitor-login(shorturl)
  #'(lambda(session)
      (use-session session)
      (setf (url) (concatenate 'string *host* "/" shorturl))
      (loop for items = (reduce #'append (mapcar #'(lambda (txt) (find-elements txt :by :xpath))
                                                 (list "//div[text()='Hang tight. We are trying to locate one of our professionals for you.']"
                                                       "//li[text()='Agent is not available.']")))
         :repeat 30 :until (and items (some #'element-displayed items)) do
         (sleep 1))
      (not (zerop (length (find-elements "//div[@data-placeholder='Type your message here...']" :by :xpath))))
      ))

(defun visitor-logout()
  #'(lambda(session)
      (use-session session)
      (setf (url) *host*)
      (loop :repeat 30 :until (find-element "//button[contains(.,'Sign In')]" :by :xpath) do
         (sleep 1))
      (not (zerop (length (find-elements "//button[contains(.,'Sign In')]" :by :xpath)))) ))

(defun no-agentp()
  #'(lambda(session)
      (use-session session)
      (let((items (find-elements "//li[text()='Agent is not available.']" :by :xpath)))
        (every #'element-displayed items))))

(defun agent-loggedp()
  #'(lambda(session)
      (use-session session)
      (let((items (find-elements "//div[@data-placeholder='Type your message here...']" :by :xpath)))
        (and items (every #'element-displayed items)))))

(defun agent-total(total)
  #'(lambda(session)
      (use-session session)
      (loop :repeat 30
         for items = (find-elements (format nil "//span[@id='total_users_online' and text()='~A']" total) :by :xpath)
         :until (and items (every #'element-displayed items) ) do
           (sleep 1)
           (format t "~%Waiting total visitors of ~A to be ~A" session total))
      (not (zerop (length (find-elements (format nil "//span[@id='total_users_online' and text()='~A']" total) :by :xpath))) )
      ))


(defparameter *sessions* (make-hash-table))
(defparameter *bug* nil)
(defparameter *state* '(((:v1 nil)
                         (:v2 nil))
                        ((:a1 "AngelP" nil)
                         (:a2 "AngelP" nil))
                        ))

(defun session(b)
  (format t "~%with session ~A" b)
  (unless (gethash b *sessions*)
    (format t "~%create session ~A" b)
    (setf (gethash b *sessions*) (make-session)))
  (gethash b *sessions*))

(defun available-visitor(state)
  #'(lambda(visitor)
      (destructuring-bind (visitors agents) state
        (destructuring-bind (visitor-session connected) visitor
          (cond
            ((not connected) (mapcar #'(lambda(agent)
                                         (destructuring-bind (agent-session shorturl connected) agent
                                           (list #'(lambda()(funcall(visitor-login shorturl) (session visitor-session)))
                                                 #'(lambda(state)(subst (list visitor-session shorturl) visitor state :test #'equal))
                                                 (format nil "~A connect to ~A" visitor-session shorturl))))
                                     (remove-duplicates agents :test #'equal :key #'second)))
            ((and connected (not (find connected (mapcar #'second agents))));; visitor connected and has logged agents
              (list (list #'(lambda()(funcall (no-agentp) (session visitor-session)))
                          #'identity
                          (format nil "~A check no agents" visitor-session))))
            ((and connected (print(find connected (mapcar #'third agents))));; visitor connected and has logged agents
              (list (list #'(lambda()(funcall (agent-loggedp) (session visitor-session)))
                          #'identity
                          (format nil "~A check agent exists" visitor-session)))))))))

(defun available-agent(state)
  #'(lambda(agent)
      (destructuring-bind (visitors agents) state
        (destructuring-bind (agent-session shorturl connected) agent
          (cond
            ((not connected) (list (list #'(lambda()(funcall (agent-login *host* *user* *pass*) (session agent-session)))
                                         #'(lambda(state)(subst (list agent-session shorturl shorturl) agent state :test #'equal))
                                         (format nil "~A agent login" agent-session))))
            ((and connected (find connected (mapcar #'second visitors) :test #'equal)) ;; agent connected and has visitors
             (list (list #'(lambda()(funcall (agent-total (length (remove-if-not #'(lambda(vis-connected)(equal vis-connected connected))
                                                                                 (mapcar #'second visitors))))
                                             (session agent-session)))
                         #'identity
                         (format nil "~A check total visitors" agent-session))
                   (list #'(lambda()(funcall (agent-logout)
                                             (session agent-session)))
                         #'(lambda(state)
                             (format t "~%new -> ~S" (list agent-session shorturl ()))
                             (format t "~%old -> ~S"agent)
                             (format t "~%state -> ~S" state)
                             (subst (list agent-session shorturl ()) agent state :test #'equal))
                         (format nil "~A agent logged out" agent-session)))))))))

(defun available(state)
  (destructuring-bind (visitors agents) state
    (append (reduce #'append (mapcar (available-visitor state)  visitors))
            (reduce #'append (mapcar (available-agent state) agents)))))




(defun available-actions(&optional (state *state*))
  (available state))

(defun explore-maze()
  (loop for x from 0 to (1-(length (available-actions))) :collect (node 0 0)))

(defun apply-action(no)
  (let ((selected (nth no (available-actions))))
    (format t "~%old-state:~S" *state*)
    (format t "~%Selected: ~A" (third selected))
    (setf *bug* (not (funcall (first selected))))
    (let ((new-state (funcall (second selected) *state*)))
      
      (setf *state* new-state )
      (format t "~%new-state:~S" *state*)
      )
    
    
))

(defun completed(path)
  (cond
    (*bug* 1)
    ((> (print (length path)) 100) -1)
    (t 0)
    ))

(defun agent-search (node)
  (maphash #'(lambda(k v)(print (delete-session v)) (remhash k *sessions*)) *sessions*)
  (setf *bug* nil)
  (format t "~%Starting ")
  (defparameter *state* '(((:v1 nil)
                           (:v2 nil))
                          ((:a1 "AngelP" nil)
                           (:a2 "AngelP" nil))
                          ))

  (copy-tree (destructuring-bind (path node)
                 (mcst '() node #'completed #'explore-maze #'apply-action)
               (format t "~%Explored:~A ~A = ~A" path (length  path) (completed path))
               (print path)
               (when (>  (completed *state*) 0)
                 (format t "~%----------------------------------------------------------  ~A"(completed *state*)))
               node)))

(setf node (node 0 0))
(loop :until *bug* do (setf node (agent-search node)))

