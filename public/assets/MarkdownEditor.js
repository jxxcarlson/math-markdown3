import markdownit from 'markdown-it'
import { Schema } from 'prosemirror-model'
import { EditorView } from 'prosemirror-view'
import { AllSelection, EditorState, TextSelection } from 'prosemirror-state'
import { schema, defaultMarkdownParser, MarkdownParser, defaultMarkdownSerializer } from 'prosemirror-markdown'
import { exampleSetup } from 'prosemirror-example-setup'
import { blockTypeItem, MenuItem } from 'prosemirror-menu'
import { lift, joinUp, toggleMark } from 'prosemirror-commands'
import { undo, redo } from 'prosemirror-history'
import { wrapInList } from 'prosemirror-schema-list'
import '@webcomponents/custom-elements/src/native-shim'

// Clone default markdown schema and make lists tight by default.
// See <https://discuss.prosemirror.net/t/lists-default-tight/1483/14>
const markdownSchema = new Schema({
  nodes: schema.spec.nodes
    .update('ordered_list', Object.assign({}, schema.spec.nodes.get('ordered_list'),
      { attrs: { order: { default: 1 }, tight: { default: true } } }))
    .update('bullet_list', Object.assign({}, schema.spec.nodes.get('bullet_list'),
      { attrs: { tight: { default: true } } })),
  marks: schema.spec.marks
})

// This a copy of the default markdownParser implementation, except that is uses
// our custom markdown schema.
const markdownParser =
  new MarkdownParser(markdownSchema, markdownit('commonmark', { html: false }), defaultMarkdownParser.tokens)

class ProseMirrorView {
  constructor (target, content, onInput, onBlur, onFocus) {
    let view = new EditorView(target, {
      state: EditorState.create({
        doc: markdownParser.parse(content),
        plugins: exampleSetup({
          schema: markdownSchema,
          menuContent: buildMenuContent(markdownSchema),
          floatingMenu: false // The menubar is made sticky via CSS
        })
      }),
      dispatchTransaction (transaction) {
        let oldMarkdown = defaultMarkdownSerializer.serialize(view.state.doc)
        view.updateState(view.state.apply(transaction))
        let newMarkdown = defaultMarkdownSerializer.serialize(view.state.doc)
        // Only call onInput when the markdown changed. This fixes an issue when
        // only moving the pointer would mark the form field as dirty on the Elm side.
        if (oldMarkdown !== newMarkdown) {
          onInput(newMarkdown)
        }
      },
      handleDOMEvents: {
        focus: () => {
          onFocus()
        },
        blur: () => {
          onBlur()
        }
      }
    })
    this.view = view
  }

  get content () {
    return defaultMarkdownSerializer.serialize(this.view.state.doc)
  }

  focus () { this.view.focus() }

  destroy () { this.view.destroy() }

  /* Setting a new value needs to be done with some precautions
  to prevent issues when users type quickly.
  See <https://github.com/itravel-de/plaza/issues/106>
  The main idea is to keep as much state as possible to
  - avoid re-rendering
  the main editor element which would cause a blur event to happen
  - and to be able to reset the cursor position.
  The reason we need to do this is because on the Elm side we always set the
  value attribute of the editor element with the current serialized content,
  which can get out of sync as Elm renders asynchronously on the next animation frame.
  See <https://github.com/elm/html/issues/105>
  */
  setValue (value) {
    let newNode = markdownParser.parse(value)
    let currentState = this.view.state
    let currentSelection = currentState.selection
    let allSelection = new AllSelection(currentState.doc)
    let transaction = currentState.tr

    transaction
      .setSelection(allSelection)
      .replaceSelectionWith(newNode)

    // Create a new fitting selection if the upper bound of the current
    // selection exceeds the content size of the new node to prevent crashing
    // the browser.
    if (currentSelection.to > newNode.content.size) {
      let newSelection = TextSelection.create(newNode, newNode.content.size)
      transaction.setSelection(newSelection)
    } else {
      transaction.setSelection(currentSelection)
    }

    this.view.updateState(currentState.apply(transaction))
  }
}

/* globals HTMLElement, CustomEvent */
class MarkdownEditorWebComponent extends HTMLElement {
  constructor () {
    super()
    this._editorValue = ''
    this.onInputCallback = this.onInputCallback.bind(this)
    this.onBlurCallback = this.onBlurCallback.bind(this)
    this.onFocusCallback = this.onFocusCallback.bind(this)
  }

  get editorValue () {
    return this._editorValue
  }

  set editorValue (value) {
    if (this._editorValue === value) {
      return
    }

    this._editorValue = value

    if (!this._editor) {
      return
    }

    this._editor.setValue(value)
  }

  onInputCallback (content) {
    this._editorValue = content
    this.dispatchEvent(new CustomEvent('editorChanged'))
  }

  onBlurCallback () {
    this.dispatchEvent(new CustomEvent('editorBlur'))
  }

  onFocusCallback () {
    this.dispatchEvent(new CustomEvent('editorFocus'))
  }

  connectedCallback () {
    this._editor = new ProseMirrorView(this, this._editorValue, this.onInputCallback, this.onBlurCallback, this.onFocusCallback)
  }
}

export function defineWebComponent () {
  window.customElements.define('markdown-editor', MarkdownEditorWebComponent)
}

// MENU BAR

let fasIcons = {
  strong: fasIcon('bold'),
  em: fasIcon('italic'),
  undo: fasIcon('undo'),
  redo: fasIcon('redo'),
  bulletList: fasIcon('list-ul'),
  orderedList: fasIcon('list-ol'),
  paragraph: fasIcon('paragraph'),
  heading: fasIcon('heading'),
  lift: fasIcon('outdent'),
  join: fasIcon('angle-double-up'),
  link: fasIcon('link')
}

function fasIcon (name) {
  let node = document.createElement('i')
  node.className = `fas fa-${name}`
  return { dom: node }
}

// Most of the following functions are taken from
// https://github.com/ProseMirror/prosemirror-menu/blob/master/src/menu.js
// and adapted to our needs.

function cmdItem (cmd, options) {
  let passedOptions = {
    label: options.title,
    run: cmd
  }
  for (let prop in options) { passedOptions[prop] = options[prop] }
  if ((!options.enable || options.enable === true) && !options.select) {
    passedOptions[options.enable ? 'enable' : 'select'] = state => cmd(state)
  }

  return new MenuItem(passedOptions)
}

function markActive (state, type) {
  let { from, $from, to, empty } = state.selection
  if (empty) return type.isInSet(state.storedMarks || $from.marks())
  else return state.doc.rangeHasMark(from, to, type)
}

function markItem (markType, options) {
  let passedOptions = {
    active (state) { return markActive(state, markType) },
    enable: true
  }
  for (let prop in options) passedOptions[prop] = options[prop]
  return cmdItem(toggleMark(markType), passedOptions)
}

function linkItem (markType) {
  return new MenuItem({
    title: 'Add or remove link',
    icon: fasIcons.link,
    active (state) { return markActive(state, markType) },
    enable (state) { return !state.selection.empty },
    run (state, dispatch, view) {
      if (markActive(state, markType)) {
        toggleMark(markType)(state, dispatch)
        return true
      }

      let href = promptForURL()
      if (!href) return false

      toggleMark(markType, { href })(view.state, view.dispatch)
      view.focus()
    }
  })
}

function promptForURL () {
  let url = window.prompt('Enter the URL', 'https://')
  if (url && !/^https?:\/\//i.test(url)) {
    url = 'http://' + url
  }
  return url
}

function wrapListItem (nodeType, options) {
  return cmdItem(wrapInList(nodeType, options.attrs), options)
}

function buildMenuContent (schema) {
  const inlineMenu = [
    markItem(schema.marks.strong, { title: 'Toggle strong style', icon: fasIcons.strong }),
    markItem(schema.marks.em, { title: 'Toggle emphasis', icon: fasIcons.em }),
    linkItem(schema.marks.link)
  ]

  const typeMenu = [
    blockTypeItem(schema.nodes.paragraph, {
      title: 'Change to paragraph',
      icon: fasIcons.paragraph
    }),
    blockTypeItem(schema.nodes.heading, {
      title: 'Change to heading ',
      icon: fasIcons.heading,
      attrs: { level: 3 }
    })

  ]

  const blockMenu = [
    wrapListItem(schema.nodes.bullet_list, {
      title: 'Wrap in bullet list',
      icon: fasIcons.bulletList
    }),
    wrapListItem(schema.nodes.ordered_list, {
      title: 'Wrap in ordered list',
      icon: fasIcons.orderedList
    }),
    new MenuItem({
      title: 'Join with above block',
      run: joinUp,
      select: state => joinUp(state),
      icon: fasIcons.join
    }),
    new MenuItem({
      title: 'Lift out of enclosing block',
      run: lift,
      select: state => lift(state),
      icon: fasIcons.lift
    })
  ]

  const historyMenu = [
    new MenuItem({
      title: 'Undo last change',
      run: undo,
      enable: state => undo(state),
      icon: fasIcons.undo
    }),
    new MenuItem({
      title: 'Redo last change',
      run: redo,
      enable: state => redo(state),
      icon: fasIcons.redo
    })
  ]

  return [
    inlineMenu,
    typeMenu,
    historyMenu,
    blockMenu
  ]
}
