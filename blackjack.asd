(in-package :cl-user)

(defpackage :blackjack-asd
  (:use :cl :asdf))

(in-package :blackjack-asd)

(defsystem :blackjack  
  :version "0.1"
  :serial t
  :components
  ((:file "packages")
   (:file "misc")
   (:file "global")
   (:file "cards")
   (:file "players")
   (:file "game-model")
   (:file "display")
   (:file "game-loop")))