const defaultTheme = require('tailwindcss/defaultTheme')

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["web/index.html"],
  theme: {
    extend: {
      fontFamily: {
        "title": ["Young Serif", ...defaultTheme.fontFamily.serif]
      },
      backgroundImage: {
        "pattern-texture": "url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='4' height='4' viewBox='0 0 4 4'%3E%3Cpath fill='%23902255' fill-opacity='0.4' d='M1 3h1v1H1V3zm2-2h1v1H3V1z'%3E%3C/path%3E%3C/svg%3E\")"
      }
    },
  },
  plugins: [],
}
