// index.js

import { Elm } from '../src/Main.elm'

import * as MarkdownEditor from './assets/MarkdownEditor.js'
import '/assets/custom-element-config.js'
import '/assets/math-text.js'
import '/assets/navigation.js'
import '/assets/outside.js'

MarkdownEditor.defineWebComponent()

// Generate random numbers to see UUID generator
const crypto = window.crypto || window.msCrypto;
const getRandomInts = (n) => {
    const randInts = new Uint32Array(n);
    crypto.getRandomValues(randInts);
    return Array.from(randInts);
};
// For a UUID, we need at least 128 bits of randomness.
// This means we need to seed our generator with at least 4 32-bit ints.
// We get 5 here, since the Pcg.Extended generator performs slightly faster if our extension array
// has a size that is a power of two (4 here).
const randInts = getRandomInts(5);

var root = document.getElementById('main');
var app = Elm.Main.init({node: root, flags:  {
    width:  window.innerWidth,
    height: window.innerHeight,
    seed: randInts[0],
    randInts: randInts.slice(1),
    location: location.href
   }
 })


