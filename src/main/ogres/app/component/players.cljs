(ns ogres.app.component.players
  (:require [uix.core :refer [defui $]]))

(defui players []
  ($ :.players
    (for [[idx title subtitle color]
          [[1 "Odo" "Human Genie Warlock 6" "purple"]
           [2 "Ronald von Trier" "Tiefling Artificer 6" "orange"]
           [3 "Sabrina" "Astral Elf Twilight Cleric 4 Quick Brown Fox" "yellow"]
           [4 "Strahd von Zarovich" "Undead Vampire Lord" "red"]
           [5 "Karl Franz" "Human Emperor 20" "white"]]]
      ($ :.players-player {:key idx}
        ($ :.players-player-color
          {:style {:background-color color}})
        ($ :.players-player-image
          ($ :.players-player-image-frame)
          ($ :.players-player-image-content))
        ($ :.players-player-tile
          ($ :.players-player-title title)
          ($ :.players-player-subtitle subtitle))))))
