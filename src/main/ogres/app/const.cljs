(ns ogres.app.const)

(goog-define VERSION "latest")
(goog-define PATH "/release")
(goog-define SOCKET-URL "ws://localhost:5000/ws")

(def ^:const grid-size
  "The length, in pixels, of a single square in the scene grid. This
   correlates to 5 feet in this spatial system."
  70)
