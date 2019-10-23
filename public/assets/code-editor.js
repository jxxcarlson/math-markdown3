//////////////////////////////////////////////////////////////////////
//
// code-editor.js
// Define the `code-editor` custom element.
// Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

customElements.define('code-editor', class extends HTMLElement {
  constructor() {
    super();
    this._editorValue = "-- If you see this, the Elm code didn't set the value."
  }

  get editorValue() {
    return this._editorValue
  }

  get lineValue() {
      return this._lineValue
   }

  set editorValue(value) {
    if (this._editorValue === value) return;
    this._editorValue = value;
    if (!this._editor) return;
    this._editor.setValue(value);
  }


  set searchTargetValue(value) {
     console.log("TARGET", value)
//       if (this._targetValue === value) return;
//       this._targetValue = value;
//       if (!this._editor) return;
//       this._editor.setValue(value);
     }

  connectedCallback() {
    this._editor = CodeMirror(this, {
      identUnit: 3,
      mode: 'elm',
      lineNumbers: true,
      lineSeparator: null,
      lineWrapping: true,
      spellcheck: true,
      value: this._editorValue
    })


    this._editor.on('gutterClick', (cm, n) => {
        this._lineValue = this._editor.getLine(n);
        console.log("n: " + n);
        console.log("line: " + this._lineValue)
        this.dispatchEvent(new CustomEvent('gutterClicked'));
    });


    this._editor.on('changes', () => {
      this._editorValue = this._editor.getValue();
      this.dispatchEvent(new CustomEvent('editorChanged'));
    });

  }
})



