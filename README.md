[ogres.app](https://ogres.app) is a free and open-source virtual tabletop that you can run in your browser and share with your friends.

![Screenshot](web/extra/share-town-center.png)

## Features

- Instantly start preparing your dungeons with no sign-ups or ads.
- Start an online lobby for your friends.
- Prepare and manage multiple scenes at once.
- Built-in initiative tracker with helpful controls for quick combat.
- Minimal rules knowledge makes it easy to use for other systems.
- ... and much more planned!

## Support

Some important things to know before setting up your first table.

- **Chrome** or **Firefox** are recommended; other browsers may have major issues at the moment. Other browsers like Opera or Microsoft Edge which are Chromium browsers may also work well, though I have not tested these myself.
- This is an offline-first application and all your work is saved on your local computer. You won't be able to transfer your work from one computer to another.
- Its easy to accidentally delete an entire scene with no way to recover it. I hope to add more safeguards soon.

Find more information on the [project wiki](https://github.com/samcf/ogres.app/wiki), including tips and keyboard shortcuts.

## Install

The following are instructions to start a local development environment (for developers only).

You'll need to have `node` and `clojure` installed.

```sh
# clone the repository
git clone git@github.com:samcf/ogres.git

# install dependencies
npm install

# start the local web server at http://localhost:8080
npm start

# start the application server necessary for hosting online sessions
clojure -X:server/dev
```
