(defparameter *input* nil)
(defparameter *position* 0)

(defun current-char ()
  (if (< *position* (length *input*))
      (char *input* *position*)
      nil))

(defun match (expected)
  (if (and (current-char) (char= (current-char) expected))
      (progn
        (incf *position*)
        t)
      nil))

;; G function to match specific characters
(defun G ()  
  (let ((ch (current-char)))
    ;; Allow more characters (including digits and other letters)
    (if (member ch '(#\x #\y #\z #\w #\o #\d #\s))  
        (progn
          (incf *position*)
          t)
        nil)))

;; F function calls G to match characters
(defun F ()  
  (or (G) (G)))

;; E function calls F and G
(defun E ()  
  (if (F)
      (G)
      nil))

;; S function handles spaces or digits followed by spaces and a word boundary
(defun S ()  
  (or (match #\s)  ;; Match spaces
      (and (match #\d) (L) (match #\b))))  ;; Match digits followed by spaces and word boundary

;; L function to process spaces (loops through spaces)
(defun L ()  
  (loop while (match #\s) do (incf *position*)))  ;; Loop through spaces

;; I function starts the parsing process
(defun I ()  
  (when (match #\i)
    (if (E)
        (if (S)
            (or (and (match #\e) (S)) t)
            nil)
        nil)))

;; parse function to check if the input matches the expected structure
(defun parse (input-string)
  (setq *input* input-string)
  (setq *position* 0)
  (if (and (I) (= *position* (length *input*)))  ;; Ensure entire input is processed
      (format t "Accepted~%")
      (format t "Rejected~%")))
