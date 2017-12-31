require('./css/index.scss')

const Elm = require('./Main.elm')

Elm.Main.embed(document.getElementById('main'))
