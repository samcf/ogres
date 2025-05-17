(ns ogres.app.const)

(goog-define VERSION "latest")
(goog-define PATH "/release")
(goog-define SOCKET-URL "ws://localhost:5000/ws")

(def ^:const grid-size
  "The length, in pixels, of a single square in the scene grid. This
   correlates to 5 feet in this spatial system."
  70)

(def ^:const half-size
  "Half the length, in pixels, of a single square in the scene grid."
  35)

(def ^:const world-line-thickness
  "The thickness of drawn lines in world units (e.g., feet).
   For example, 0.25 represents a 3-inch thick line if 1 world unit = 1 foot."
  0.25)