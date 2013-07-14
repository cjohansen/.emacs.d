module.exports = {
  "paths": [
    "test/**/*.js",
    "lib/**/*.js"
  ],

  "linterOptions": {
    "indent": 2,
    "vars": true,
    "nomen": true,
    "sloppy": true,
    "plusplus": true,
    "predef": [
      "assert",
      "refute",
      "buster",
      "cull",
      "dome",
      "__GLOBAL__"
    ]
  },

  "excludes": [
    "external"
  ]
};
