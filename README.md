## ogre.tools

[ogre.tools](https://ogre.tools) is a free and open source virtual table top to help run your **Dungeons & Dragons 5th Edition** games. It is designed to be another tool in your dungeon master toolbox rather than an all-in-one solution. This application runs in your browser and relies exclusively on local storage to persist your uploaded maps, tokens, and the state of your workspaces.

This is a **work in progress** and not intended for serious usage at this time. It is under heavy development and will go through many more changes as it matures.

### Design Goals
* Free and open source &ndash; forever.
* Tailored for Dungeons & Dragons 5th Edition games.
* No account registrations, sign ups, or third party authorizations.
* Runs entirely within your browser &ndash; no server necessary.
* Small number of well designed features.
* No rules engine or game data.

### Roadmap
* Initial Release
  * Drawing shapes such as circles and rectangles
  * New options for token styles
  * Upload images for tokens and manage their default settings
  * Keyboard and mouse shortcuts
* Soon Thereafter
  * Pop-out mode suitable for screen sharing
  * Visibility controls such as a fog tool and hidden tokens
  * Improved intelligence when importing maps
  * Export and import application data
* Further Down The Line
  * Select and manage multiple tokens and shapes
  * Weather and color temperature effects
  * Simple initiative tracker

### Contributing
This project is licensed under the [GNU AGPLv3](https://choosealicense.com/licenses/agpl-3.0/) to ensure that any changes made to the project are freely available to all users.

Since this project is in its infancy I'm not considering or accepting contributions at this time. Should you wish to make changes, please feel free to fork this project.

### Technology
* The application is written in [ClojureScript](https://clojurescript.org/), a functional programming language that compiles to Javascript and runs in the browser.
* An in-memory database called [DataScript](https://github.com/tonsky/datascript) is responsible for storing and querying the application state.
* [uix](https://github.com/roman01la/uix) renders the [React](https://reactjs.org/) user interface.
* [Dexie](https://dexie.org/) acts as an interface to the browser's [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) data store and is used when saving large images and the application state to the browser so that your data lives on even after closing the window.
* The canvas is drawn with [SVG](https://developer.mozilla.org/en-US/docs/Web/SVG), a retained-mode vector graphics language that is popular in browsers. Though not as efficient as HTML Canvas for many use cases, it offers several large advantages such as native event handling and superior text rendering. Additionally, it looks crisp at any resolution and fits perfectly into the React paradigm.

### Credit
This project is heavily inspired by [Owlbear Rodeo](https://owlbear.rodeo/), a fantastic VTT for the dungeon master tired of bulky and intrusive tools.
