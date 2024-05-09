/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.hs", "./app/**/*.hs"],
  theme: {
    extend: {},
  },
  plugins: [require("@tailwindcss/typography"), require('daisyui')],
}

