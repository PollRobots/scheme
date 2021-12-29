import Editor, { Monaco } from "@monaco-editor/react";
import React from "react";
import { TerminalData } from "./TerminalData";
import { TerminalInput } from "./TerminalInput";
import { editor, KeyMod, KeyCode } from "monaco-editor";
import { kLanguageId, registerLanguage } from "../monaco/scheme";
import { defineThemes } from "../monaco/solarized";
import { EditorThemeContext, ThemeContext } from "./ThemeProvider";
import { Copy, Cut, Open, Paste, Redo, Save, Undo } from "../icons/copy";
import { IconButton } from "./IconButton";

interface TerminalProps {
  welcomeMessage?: React.ReactNode;
  output: string[];
  prompt: string;
  pause: boolean;
  autofocus: boolean;
  fontSize: number;
  onInput: (str: string) => Promise<void>;
}

interface TerminalState {
  history: string[];
  hidx: number;
  input: string;
  cached: string;
  editing: boolean;
  line: string;
  editor: editor.IStandaloneCodeEditor | null;
  canPaste: boolean;
  initialVersion: number;
  currentVersion: number;
  highVersion: number;
}

const kDefaultState: TerminalState = {
  history: [],
  hidx: 0,
  input: "",
  cached: "",
  editing: false,
  line: "",
  editor: null,
  canPaste: false,
  initialVersion: 0,
  currentVersion: 0,
  highVersion: 0,
};

export class Terminal extends React.Component<TerminalProps, TerminalState> {
  constructor(props: TerminalProps) {
    super(props);
    this.state = { ...kDefaultState };

    this.clipboardChange = this.clipboardChange.bind(this);
  }

  async onEnter(prompt: string, text: string) {
    const cmd = text;
    const history = [...this.state.history];

    // update history
    const hidx = history.findIndex((el) => el === cmd);
    if (hidx >= 0) {
      history.splice(hidx, 1);
    }
    history.push(cmd);

    await this.props.onInput(cmd);

    this.setState({
      input: "",
      history: history,
      hidx: history.length,
      cached: "",
    });
  }

  onUp() {
    if (this.state.hidx <= 0) {
      return;
    }
    this.setState({
      hidx: this.state.hidx - 1,
      cached:
        this.state.hidx == this.state.history.length
          ? this.state.input
          : this.state.cached,
      input: this.state.history[this.state.hidx - 1],
    });
  }

  onDown() {
    if (this.state.hidx >= this.state.history.length) {
      return;
    } else if (this.state.hidx == this.state.history.length - 1) {
      this.setState({
        hidx: this.state.hidx + 1,
        input: this.state.cached,
        cached: "",
      });
    } else {
      this.setState({
        hidx: this.state.hidx + 1,
        input: this.state.history[this.state.hidx + 1],
      });
    }
  }

  onEscape(text: string) {
    this.setState({
      editing: true,
      line: text,
    });
  }

  beforeEditorMount(monaco: Monaco) {
    registerLanguage(monaco);
    defineThemes(monaco);
  }

  async clipboardChange() {
    try {
      const content = await navigator.clipboard.readText();
      this.setState({ canPaste: !!content && content.length > 0 });
    } catch (err) {
      this.setState({ canPaste: false });
    }
  }

  onEditorMount(editor: editor.IStandaloneCodeEditor) {
    // editor.updateOptions({ minimap: { enabled: false } });
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
      run: (ed: editor.IStandaloneCodeEditor) => {
        this.setState({
          editing: false,
          line: "",
          input: ed.getValue(),
        });
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

  componentDidUpdate(prevProps: TerminalProps, prevState: TerminalState) {
    if (this.props.fontSize != prevProps.fontSize) {
      const editor = this.state.editor;
      if (editor) {
        editor.updateOptions({ fontSize: this.props.fontSize });
      }
    }

    if (this.state.editing != prevState.editing) {
      if (this.state.editing) {
        navigator.clipboard.addEventListener(
          "clipboardchange",
          this.clipboardChange
        );
        navigator.clipboard
          .readText()
          .then((content) =>
            this.setState({ canPaste: !!content && content.length > 0 })
          );
      } else {
        navigator.clipboard.removeEventListener(
          "clipboardchange",
          this.clipboardChange
        );
      }
    }
  }

  onDoneEditing() {
    this.setState({
      editing: false,
      line: "",
      editor: null,
      input: this.state.editor ? this.state.editor.getValue() : this.state.line,
    });
  }

  render() {
    return (
      <ThemeContext.Consumer>
        {(theme) => (
          <EditorThemeContext.Consumer>
            {(maybeEditorTheme) => {
              const editorTheme = maybeEditorTheme || theme;
              if (this.state.editing && !this.props.pause) {
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
                            this.state.currentVersion <=
                            this.state.initialVersion
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
                            const input = document.createElement("input");
                            input.type = "file";
                            input.multiple = false;
                            input.accept =
                              ".scm,text/plain,text/x-script.scheme,text/x-scheme";
                            input.addEventListener("change", async () => {
                              const files = input.files;
                              if (!files || files.length == 0) {
                                return;
                              }
                              const file = files.item(0);
                              if (!file) {
                                return;
                              }
                              const text = await file.text();
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
                            });
                            input.click();
                          }}
                        >
                          <Open />
                        </IconButton>
                        <IconButton
                          size={this.props.fontSize * 2}
                          title="Save"
                          onClick={() => {
                            const editor = this.state.editor;
                            if (!editor) {
                              return;
                            }

                            const content = editor.getValue();
                            const blob = new Blob([content], {
                              type: "text/plain",
                            });
                            const anchor = document.createElement("a");
                            anchor.href = window.URL.createObjectURL(blob);
                            anchor.download = "untitled.scm";
                            anchor.click();
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
                        onClick={() => this.onDoneEditing()}
                      >
                        Done
                      </button>
                    </div>
                    <Editor
                      height="90vh"
                      theme={editorTheme.name}
                      defaultLanguage={kLanguageId}
                      defaultValue={this.state.line}
                      onMount={(editor: editor.IStandaloneCodeEditor) =>
                        this.onEditorMount(editor)
                      }
                      beforeMount={(instance: Monaco) =>
                        this.beforeEditorMount(instance)
                      }
                    />
                  </div>
                );
              } else {
                return (
                  <div
                    style={{
                      backgroundColor: theme.background,
                      color: theme.foreground,
                      fontFamily: "'Source Code Pro', monospace",
                      padding: "0.5em",
                      minWidth: "40rem",
                    }}
                  >
                    {this.props.welcomeMessage}
                    <TerminalData text={this.props.output} />
                    <TerminalInput
                      prompt={this.props.prompt}
                      readonly={this.props.pause}
                      value={this.state.input}
                      autofocus={this.props.autofocus}
                      onEnter={(text) => this.onEnter(this.props.prompt, text)}
                      onUp={() => this.onUp()}
                      onDown={() => this.onDown()}
                      onEscape={(text) => this.onEscape(text)}
                    />
                  </div>
                );
              }
            }}
          </EditorThemeContext.Consumer>
        )}
      </ThemeContext.Consumer>
    );
  }
}
