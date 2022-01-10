import React from "react";
import monaco from "monaco-editor";

import { IconButton } from "./IconButton";
import { EditorThemeContext, ThemeContext } from "./ThemeProvider";
import { Cut, Copy, Paste, Undo, Redo, Open, Save } from "../icons/copy";
import { kLanguageId } from "../monaco/scheme";
import { openFilePicker, saveFilePicker } from "../util";
import Editor from "./Editor";

interface TerminalEditorProps {
  fontSize: number;
  line: string;
  onDoneEditing: (line: string) => void;
}

interface TerminalEditorState {
  editor?: monaco.editor.IStandaloneCodeEditor;
  canPaste: boolean;
  currentVersion: number;
  initialVersion: number;
  highVersion: number;
}

export default class TerminalEditor extends React.Component<
  TerminalEditorProps,
  TerminalEditorState
> {
  constructor(props: TerminalEditorProps) {
    super(props);
    this.state = {
      canPaste: true,
      currentVersion: 0,
      initialVersion: 0,
      highVersion: 0,
    };
  }

  onEditorMount(editor: monaco.editor.IStandaloneCodeEditor) {
    // editor.updateOptions({ minimap: { enabled: false } });
    const KeyMod: typeof monaco.KeyMod = (window as any).monaco.KeyMod;
    const KeyCode: typeof monaco.KeyCode = (window as any).monaco.KeyCode;
    editor.addAction({
      id: "end-editing-scheme",
      label: "End Editing",
      keybindings: [
        KeyMod.CtrlCmd | KeyCode.KeyE,
        KeyMod.chord(
          KeyMod.CtrlCmd | KeyCode.KeyK,
          KeyMod.CtrlCmd | KeyCode.KeyD
        ),
      ],
      contextMenuGroupId: "navigation",
      contextMenuOrder: 1.5,
      run: (ed: monaco.editor.IStandaloneCodeEditor) => {
        this.props.onDoneEditing(ed.getValue());
      },
    });
    editor.updateOptions({ fontSize: this.props.fontSize });
    editor.focus();
    const model = editor.getModel();
    if (model) {
      const ver = model.getAlternativeVersionId();
      this.setState({
        initialVersion: ver,
        currentVersion: ver,
        highVersion: ver,
      });
    }
    editor.onDidChangeModelContent(() => {
      const model = editor.getModel();
      if (!model) {
        return;
      }
      const ver = model.getAlternativeVersionId();
      this.setState({
        currentVersion: ver,
        highVersion: Math.max(ver, this.state.highVersion),
      });
    });
    this.setState({ editor: editor });
  }

  render() {
    return (
      <ThemeContext.Consumer>
        {(theme) => (
          <EditorThemeContext.Consumer>
            {(maybeTheme) => {
              const editorTheme = maybeTheme || theme;
              return (
                <div
                  style={{
                    color: editorTheme.foreground,
                    background: editorTheme.background,
                  }}
                >
                  <div
                    style={{
                      display: "grid",
                      background: editorTheme.boldBackground,
                      padding: "0.25em",
                      gridTemplateColumns: "auto auto 1fr auto",
                    }}
                  >
                    <span
                      style={{
                        margin: "auto 1em",
                        fontWeight: 500,
                      }}
                    >
                      Editing current line
                    </span>
                    <div
                      style={{
                        display: "grid",
                        gridTemplateColumns:
                          "3fr 3fr 3fr 1fr 3fr 3fr 1fr 3fr 3fr",
                        columnGap: "0.25em",
                        alignSelf: "center",
                      }}
                    >
                      <IconButton
                        size={this.props.fontSize * 2}
                        title="Cut"
                        onClick={() => {
                          const editor = this.state.editor;
                          if (!editor) {
                            return;
                          }
                          editor.focus();
                          const selection = editor.getSelection();
                          if (!selection || selection.isEmpty()) {
                            navigator.clipboard.writeText("");
                            return;
                          }
                          const data = editor
                            .getModel()
                            ?.getValueInRange(selection);
                          navigator.clipboard.writeText(data || "");
                          editor.executeEdits("clipboard", [
                            {
                              range: selection,
                              text: "",
                              forceMoveMarkers: true,
                            },
                          ]);
                        }}
                      >
                        <Cut />
                      </IconButton>
                      <IconButton
                        size={this.props.fontSize * 2}
                        title="Copy"
                        onClick={() => {
                          const editor = this.state.editor;
                          if (!editor) {
                            return;
                          }
                          editor.focus();
                          const selection = editor.getSelection();
                          if (!selection || selection.isEmpty()) {
                            navigator.clipboard.writeText("");
                            return;
                          }
                          const data = editor
                            .getModel()
                            ?.getValueInRange(selection);
                          navigator.clipboard.writeText(data || "");
                        }}
                      >
                        <Copy />
                      </IconButton>
                      <IconButton
                        size={this.props.fontSize * 2}
                        title="Paste"
                        disabled={!this.state.canPaste}
                        onClick={() => {
                          const editor = this.state.editor;
                          if (!editor) {
                            return;
                          }
                          editor.focus();
                          navigator.clipboard.readText().then((v) => {
                            const selection = editor.getSelection();
                            if (!selection) {
                              return;
                            }
                            editor.executeEdits("clipboard", [
                              {
                                range: selection,
                                text: v,
                                forceMoveMarkers: true,
                              },
                            ]);
                          });
                        }}
                      >
                        <Paste />
                      </IconButton>
                      <div />
                      <IconButton
                        disabled={
                          this.state.currentVersion <= this.state.initialVersion
                        }
                        size={this.props.fontSize * 2}
                        title="Undo"
                        onClick={() => {
                          const editor = this.state.editor;
                          if (!editor) {
                            return;
                          }
                          editor.trigger("toolbar", "undo", null);
                        }}
                      >
                        <Undo />
                      </IconButton>
                      <IconButton
                        disabled={
                          this.state.currentVersion >= this.state.highVersion
                        }
                        size={this.props.fontSize * 2}
                        title="Redo"
                        onClick={() => {
                          const editor = this.state.editor;
                          if (!editor) {
                            return;
                          }
                          editor.trigger("toolbar", "redo", null);
                        }}
                      >
                        <Redo />
                      </IconButton>
                      <div />
                      <IconButton
                        size={this.props.fontSize * 2}
                        title="Open"
                        onClick={() => {
                          const editor = this.state.editor;
                          if (!editor) {
                            return;
                          }
                          openFilePicker()
                            .then((text) => {
                              editor.focus();
                              const selection = editor.getSelection();
                              if (!selection) {
                                editor.setValue(text);
                                return;
                              } else {
                                editor.executeEdits("file-open", [
                                  {
                                    range: selection,
                                    text: text,
                                    forceMoveMarkers: true,
                                  },
                                ]);
                              }
                            })
                            .catch((err) => {
                              console.error(err);
                              editor.focus();
                            });
                        }}
                      >
                        <Open />
                      </IconButton>
                      <IconButton
                        size={this.props.fontSize * 2}
                        title="Save"
                        onClick={async () => {
                          const editor = this.state.editor;
                          if (!editor) {
                            return;
                          }

                          const content = editor.getValue();
                          saveFilePicker(content)
                            .catch((err) => {
                              console.error(err);
                            })
                            .finally(() => editor.focus());
                        }}
                      >
                        <Save />
                      </IconButton>
                    </div>
                    <div />
                    <button
                      style={{
                        fontSize: "inherit",
                        minWidth: "6em",
                        minHeight: "2em",
                        margin: "0.25em",
                        background: editorTheme.blue,
                        border: `1px solid ${theme.base00}`,
                        borderRadius: "0.25em",
                        color: editorTheme.foreground,
                      }}
                      title={
                        "Finish editing\nShortcuts:\n  · Ctrl+K Ctrl+D\n  · Ctrl+E"
                      }
                      onClick={() =>
                        this.props.onDoneEditing(
                          this.state.editor
                            ? this.state.editor.getValue()
                            : this.props.line
                        )
                      }
                    >
                      Done
                    </button>
                  </div>
                  <Editor
                    height="90vh"
                    maxWidth="calc(95vw - 8rem)"
                    theme={editorTheme.name}
                    defaultLanguage={kLanguageId}
                    defaultValue={this.props.line}
                    onMount={(editor: monaco.editor.IStandaloneCodeEditor) =>
                      this.onEditorMount(editor)
                    }
                  />
                </div>
              );
            }}
          </EditorThemeContext.Consumer>
        )}
      </ThemeContext.Consumer>
    );
  }
}
