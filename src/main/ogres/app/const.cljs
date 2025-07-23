(ns ogres.app.const)

(goog-define VERSION "latest")
(goog-define PATH "/release")
(goog-define SOCKET-URL "ws://localhost:5000/ws")

(def ^:const grid-size
  "The length, in pixels, of a single square in the scene grid. This
   correlates to 1 square unit in this spatial system."
  70)

(def ^:const half-size
  "Half the length, in pixels, of a single square in the scene grid."
  35)

(def ^:const grid-dist
  "Grid size in arbitrary units. Default 5.0"
  1.5)