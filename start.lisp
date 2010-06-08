(require 'asdf)
(load "blackjack.asd")
(asdf:oos 'asdf:load-op :blackjack)
(blackjack::start-game)